module Reducer (reductionSheet) where

import Common.CommonTypes hiding (Dirty (..))
import Evaluation.EvalLayerTypes

import Evaluation.EvalLayerUtils

import DocTypes_Generated
import DocUtils_Generated
import DocumentEdit_Generated


instance ReductionSheet Document EnrichedDoc ClipDoc where
  reductionSheetSimplest (RootEnr root)            = RootDoc (reduceRoot root)
  reductionSheetSimplest HoleEnrichedDoc           = HoleDocument
  reductionSheetSimplest (ParseErrEnrichedDoc prs) = ParseErrDocument prs

reduceRoot (Root graph probtables title sections) =
  let subgraphs = getSubgraphsSections sections
      (graph', subgraphs') = resolveSubgraphs graph subgraphs
      probtables' = updateProbtablesList (mkProbtableMap (getProbtablesSectionList sections)) probtables
  in  Root graph' probtables' title (replaceSubgraphsSectionList subgraphs' sections)
reduceRoot r = r


isCleanDoc Clean = True
isCleanDoc _     = False



getSubgraphsSections :: List_Section -> [Subgraph]
getSubgraphsSections sections = concatMap getSubgraphsSection (fromList_Section sections)
  where getSubgraphsSection (Section _ paras subsections) = 
          getSubgraphsParas (fromList_Paragraph paras) ++ getSubgraphsSubsections subsections
          

getSubgraphsSubsections subsections = concatMap getSubgraphsSubsection (fromList_Subsection subsections)
  where getSubgraphsSubsection (Subsection _ paras subsubsections) = 
          getSubgraphsParas (fromList_Paragraph paras) ++ getSubgraphsSubsubsections subsubsections


getSubgraphsSubsubsections subsubsections = concatMap getSubsubgraphsSubsection (fromList_Subsubsection subsubsections)
  where getSubsubgraphsSubsection (Subsubsection _ paras) = 
          getSubgraphsParas (fromList_Paragraph paras)

getSubgraphsParas [] = []
getSubgraphsParas (SubgraphPara sg:paras) = sg:getSubgraphsParas paras 
getSubgraphsParas (_:paras)               = getSubgraphsParas paras 

replaceSubgraphsSectionList sgs sections = 
  let (sections', sgs') = replaceSubgraphsSections sgs $ fromList_Section sections
  in  (if not (null sgs') then debug Err "Reducer.replaceSubgraphSectionList: too many subgraphs" else id)
        toList_Section $ sections'

replaceSubgraphsSections sgs []                                           = ([], sgs)
replaceSubgraphsSections sgs (Section title paras subsections : sections) = 
  let (paras', sgs') = replaceSubgraphsParas sgs (fromList_Paragraph paras)
      (subsections', sgs'') = replaceSubgraphsSubsectionList sgs' subsections
      (sections', sgs''') = replaceSubgraphsSections sgs'' sections
  in  (Section title (toList_Paragraph paras') subsections': sections', sgs''')

replaceSubgraphsSubsectionList sgs subsections = 
  let (subsections', sgs') = replaceSubgraphsSubsections sgs $ fromList_Subsection subsections
  in  (toList_Subsection $ subsections', sgs')

replaceSubgraphsSubsections sgs []                                           = ([], sgs)
replaceSubgraphsSubsections sgs (Subsection title paras subsubsections : subsections) = 
  let (paras', sgs') = replaceSubgraphsParas sgs (fromList_Paragraph paras)
      (subsubsections', sgs'') = replaceSubgraphsSubsubsectionList sgs' subsubsections
      (subsections', sgs''') = replaceSubgraphsSubsections sgs'' subsections
  in  (Subsection title (toList_Paragraph paras') subsubsections': subsections', sgs''')

replaceSubgraphsSubsubsectionList sgs subsubsections = 
  let (subsubsections', sgs') = replaceSubgraphsSubsubsections sgs $ fromList_Subsubsection subsubsections
  in  (toList_Subsubsection $ subsubsections', sgs')

replaceSubgraphsSubsubsections sgs []                                           = ([], sgs)
replaceSubgraphsSubsubsections sgs (Subsubsection title paras : subsubsections) = 
  let (paras', sgs') = replaceSubgraphsParas sgs (fromList_Paragraph paras)
      (subsubsections', sgs'') = replaceSubgraphsSubsubsections sgs' subsubsections
  in  (Subsubsection title (toList_Paragraph paras'): subsubsections', sgs'')


replaceSubgraphsParas sgs [] = ([], sgs)
replaceSubgraphsParas (sg:sgs) (SubgraphPara _ :paras) = let (paras',sgs')= replaceSubgraphsParas sgs paras
                                                         in  (SubgraphPara sg : paras', sgs') 
replaceSubgraphsParas sgs      (para:paras)            = let (paras',sgs')= replaceSubgraphsParas sgs paras
                                                         in  (para : paras', sgs')
replaceSubgraphsParas [] (para:paras)                  = 
   debug Err "Reducer.replaceSubgraphSectionList: too few subgraphs" $
     (para: paras, []) -- internal error: no more subgraphs, so stop replacing
     
{-
-- replace each subgraph in sections with a subgraph from subgraphs
replaceSubgraphs :: List_Section -> [Subgraph] -> List_Section
replaceSubgraphs sections subgraphs = 
  toList_Section $ zipWith replaceSubgraph (fromList_Section sections) subgraphs
  where replaceSubgraph (Section title idd paras subsections _) subgraph = Section title idd paras subsections subgraph
-}
  
-- if the graph is clean, and one of the subgraphs dirty, resolveSubgraphs
-- adds the edges from the dirty subgraph to the graph
-- if the graph is dirty, any vertices not in the graph are removed from the subgraphs 
-- and we also need to delete edges to or from non-existent nodes
resolveSubgraphs :: Graph -> [Subgraph] -> (Graph, [Subgraph])
resolveSubgraphs graph@(Graph graphDirty vs es) subgraphs =
  if isCleanSupergraph graph
  then 
    case filter (not . isCleanSubgraph) subgraphs of
           []                -> (graph, subgraphs) -- all are clean
           (dirtySubgraph:_) -> -- at least one subgraph dirty, only consider the first
              (addEdgesFromSubgraph dirtySubgraph graph, subgraphs)
  else -- supergraph is dirty:    remove deleted nodes from subgraphs
    let superGraphIDs = map getID_Vertex $ fromList_Vertex vs        
        correctEdges = filter (\e -> getFrom_Edge e `elem` superGraphIDs &&
                                     getTo_Edge e `elem` superGraphIDs ) 
                              (fromList_Edge es) 
    in ( Graph graphDirty vs (toList_Edge correctEdges)
       , map (removeOldVertices superGraphIDs) subgraphs)
resolveSubgraphs graph subgraphs = (graph, subgraphs)

isCleanSupergraph (Graph d _ _) = isCleanDoc d

isCleanSubgraph (Subgraph d _ _) = isCleanDoc d

removeOldVertices vertexIDs (Subgraph d vs es) = 
  let subgraphVertices' = toList_Vertex $ filter (\v -> getID_Vertex v `elem` vertexIDs) $
                                                 fromList_Vertex vs
  in  Subgraph d subgraphVertices' es

addEdgesFromSubgraph (Subgraph _ vs' es') (Graph d vs es) =
  let superGraphIDs = map getID_Vertex $ fromList_Vertex vs
      subgraphIDs = filter (`elem` superGraphIDs) $ map getID_Vertex $ fromList_Vertex vs'
      edgesWithoutSubgraphNodes = filter (\e -> getFrom_Edge e `notElem` subgraphIDs || getTo_Edge e `notElem` subgraphIDs) 
                                      (fromList_Edge es)
      graphEdges = toList_Edge $ edgesWithoutSubgraphNodes ++ fromList_Edge es'
  in  debug Err ("\n\n\n\nsub"++ show (map getID_Vertex $ fromList_Vertex vs') ++
                     "super"++      show (map getID_Vertex $ fromList_Vertex vs) ++ 
                     "diff"++ show ((map getID_Vertex $ fromList_Vertex vs') \\ (map getID_Vertex $ fromList_Vertex vs)) ++
                 if null ((map getID_Vertex $ fromList_Vertex vs') \\ (map getID_Vertex $ fromList_Vertex vs)) then "" else "new nodes in subgraph") $
      Graph d vs graphEdges

getFrom_Edge (Edge fromV _) = fromV

getTo_Edge (Edge _ toV) = toV

getID_Vertex (Vertex _ _ id _ _) = id




--- Probtables

getProbtablesSectionList :: List_Section -> [Probtable]
getProbtablesSectionList sections = 
  getProbtablesSections $ fromList_Section sections

getProbtablesSections []                                           = []
getProbtablesSections (Section title paras subsections : sections) = 
  getProbtablesParas (fromList_Paragraph paras) ++
  getProbtablesSections sections     

getProbtablesParas []                              = []
getProbtablesParas (ProbtablePara probtable:paras) = probtable : getProbtablesParas paras
getProbtablesParas (para:paras)                    = getProbtablesParas paras

-- update probtables in the second list with information from the first
updateProbtablesList :: ProbtableMap -> List_Probtable -> List_Probtable
updateProbtablesList probtableMap = toList_Probtable . map (updateProbtable probtableMap) . fromList_Probtable

updateProbtable :: ProbtableMap -> Probtable -> Probtable
updateProbtable probtableMap probtable@(Probtable id prob) = 
  case lookup id probtableMap of
    Just probtable' -> probtable'
    Nothing        -> probtable

mkProbtableMap :: [Probtable] -> ProbtableMap
mkProbtableMap probtables = map probtableMapEntry probtables
 where probtableMapEntry probtable@(Probtable id _) = (id,probtable)

type ProbtableMap = [(Int, Probtable)]
