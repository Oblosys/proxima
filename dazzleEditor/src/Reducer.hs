module Reducer (reductionSheet) where

import CommonTypes hiding (Dirty (..))
import EvalLayerTypes

import EvalLayerUtils

import PresTypes -- for initDoc 

import DocTypes_Generated
import DocUtils_Generated
import DocumentEdit_Generated

reductionSheet ::
               LayerStateEval -> EnrichedDocLevel EnrichedDoc -> DocumentLevel Document clip ->
               EnrichedDocLevel EnrichedDoc ->
               IO (EditDocument documentLevel Document, LayerStateEval, EnrichedDocLevel EnrichedDoc)
reductionSheet state _ (DocumentLevel (RootDoc idd _) _ _) enrDoc@(EnrichedDocLevel (RootEnr _ root _) _) =
  return (SetDoc (RootDoc idd (reduceRoot root)),state, enrDoc)
reductionSheet state _ _ enrDoc@(EnrichedDocLevel (HoleEnrichedDoc) oldfocus) = return $
  (SetDoc (HoleDocument),state, enrDoc )
reductionSheet state _ _ enrDoc@(EnrichedDocLevel (ParseErrEnrichedDoc prs) oldfocus) = return $
  (SetDoc (ParseErrDocument prs),state, enrDoc )  -- nd is not right


reduceRoot (Root idd graph title sections) =
  let subgraphs = getSubgraphsSections sections
      (graph', subgraphs') = resolveSubgraphs graph subgraphs
  in  Root idd graph' title (replaceSubgraphsSectionList subgraphs' sections)



isCleanDoc (Clean _) = True
isCleanDoc _         = False



getSubgraphsSections :: List_Section -> [Subgraph]
getSubgraphsSections sections = concatMap getSubgraphsSection (fromList_Section sections)
  where getSubgraphsSection (Section _ _ paras subsections) = 
          getSubgraphsParas (fromList_Paragraph paras) ++ getSubgraphsSubsections subsections
          

getSubgraphsSubsections subsections = concatMap getSubgraphsSubsection (fromList_Subsection subsections)
  where getSubgraphsSubsection (Subsection _ _ paras subsubsections) = 
          getSubgraphsParas (fromList_Paragraph paras) ++ getSubgraphsSubsubsections subsubsections


getSubgraphsSubsubsections subsubsections = concatMap getSubsubgraphsSubsection (fromList_Subsubsection subsubsections)
  where getSubsubgraphsSubsection (Subsubsection _ _ paras) = 
          getSubgraphsParas (fromList_Paragraph paras)

getSubgraphsParas [] = []
getSubgraphsParas (SubgraphPara _ sg:paras) = sg:getSubgraphsParas paras 
getSubgraphsParas (Paragraph _ _:paras)     = getSubgraphsParas paras 

replaceSubgraphsSectionList sgs sections = 
  let (sections', sgs') = replaceSubgraphsSections sgs $ fromList_Section sections
  in  (if not (null sgs') then debug Err "Reducer.replaceSubgraphSectionList: too many subgraphs" else id)
        toList_Section $ sections'

replaceSubgraphsSections sgs []                                           = ([], sgs)
replaceSubgraphsSections sgs (Section id title paras subsections : sections) = 
  let (paras', sgs') = replaceSubgraphsParas sgs (fromList_Paragraph paras)
      (subsections', sgs'') = replaceSubgraphsSubsectionList sgs' subsections
      (sections', sgs''') = replaceSubgraphsSections sgs'' sections
  in  (Section id title (toList_Paragraph paras') subsections': sections', sgs''')

replaceSubgraphsSubsectionList sgs subsections = 
  let (subsections', sgs') = replaceSubgraphsSubsections sgs $ fromList_Subsection subsections
  in  (toList_Subsection $ subsections', sgs')

replaceSubgraphsSubsections sgs []                                           = ([], sgs)
replaceSubgraphsSubsections sgs (Subsection id title paras subsubsections : subsections) = 
  let (paras', sgs') = replaceSubgraphsParas sgs (fromList_Paragraph paras)
      (subsubsections', sgs'') = replaceSubgraphsSubsubsectionList sgs' subsubsections
      (subsections', sgs''') = replaceSubgraphsSubsections sgs'' subsections
  in  (Subsection id title (toList_Paragraph paras') subsubsections': subsections', sgs''')

replaceSubgraphsSubsubsectionList sgs subsubsections = 
  let (subsubsections', sgs') = replaceSubgraphsSubsubsections sgs $ fromList_Subsubsection subsubsections
  in  (toList_Subsubsection $ subsubsections', sgs')

replaceSubgraphsSubsubsections sgs []                                           = ([], sgs)
replaceSubgraphsSubsubsections sgs (Subsubsection id title paras : subsubsections) = 
  let (paras', sgs') = replaceSubgraphsParas sgs (fromList_Paragraph paras)
      (subsubsections', sgs'') = replaceSubgraphsSubsubsections sgs' subsubsections
  in  (Subsubsection id title (toList_Paragraph paras'): subsubsections', sgs'')


replaceSubgraphsParas sgs [] = ([], sgs)
replaceSubgraphsParas (sg:sgs) (SubgraphPara idd _ :paras) = let (paras',sgs')= replaceSubgraphsParas sgs paras
                                                             in  (SubgraphPara idd sg : paras', sgs') 
replaceSubgraphsParas sgs (para@(Paragraph _ _):paras)     = let (paras',sgs')= replaceSubgraphsParas sgs paras
                                                             in  (para : paras', sgs')
replaceSubgraphsParas [] (para:paras)                      = 
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
resolveSubgraphs graph@(Graph idd graphDirty vs es) subgraphs =
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
    in ( Graph idd graphDirty vs (toList_Edge correctEdges)
       , map (removeOldVertices superGraphIDs) subgraphs)
resolveSubgraphs graph subgraphs = (graph, subgraphs)

isCleanSupergraph (Graph _ d _ _) = isCleanDoc d

isCleanSubgraph (Subgraph _ d _ _) = isCleanDoc d

removeOldVertices vertexIDs (Subgraph id d vs es) = 
  let subgraphVertices' = toList_Vertex $ filter (\v -> getID_Vertex v `elem` vertexIDs) $
                                                 fromList_Vertex vs
  in  Subgraph id d subgraphVertices' es

addEdgesFromSubgraph (Subgraph _ _ vs' es') (Graph id d vs es) =
  let superGraphIDs = map getID_Vertex $ fromList_Vertex vs
      subgraphIDs = filter (`elem` superGraphIDs) $ map getID_Vertex $ fromList_Vertex vs'
      edgesWithoutSubgraphNodes = filter (\e -> getFrom_Edge e `notElem` subgraphIDs || getTo_Edge e `notElem` subgraphIDs) 
                                      (fromList_Edge es)
      graphEdges = toList_Edge $ edgesWithoutSubgraphNodes ++ fromList_Edge es'
  in  debug Err ("\n\n\n\nsub"++ show (map getID_Vertex $ fromList_Vertex vs') ++
                     "super"++      show (map getID_Vertex $ fromList_Vertex vs) ++ 
                     "diff"++ show ((map getID_Vertex $ fromList_Vertex vs') \\ (map getID_Vertex $ fromList_Vertex vs)) ++
                 if null ((map getID_Vertex $ fromList_Vertex vs') \\ (map getID_Vertex $ fromList_Vertex vs)) then "" else "new nodes in subgraph") $
      Graph id d vs graphEdges

getFrom_Edge (Edge _ (Int_ _ fromV) _) = fromV

getTo_Edge (Edge _ _ (Int_ _ toV)) = toV

getID_Vertex (Vertex _ _ _ (Int_ _ id) _ _) = id

