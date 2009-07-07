module Reducer (reductionSheet, reduceDoc) where

import Common.CommonTypes hiding (Dirty (..))
import Evaluation.EvalLayerTypes

import Evaluation.EvalLayerUtils

import DocTypes_Generated
import DocUtils_Generated
import DocumentEdit_Generated

import Data.Generics
import Data.Maybe

instance ReductionSheet Document EnrichedDoc ClipDoc where
  reductionSheetSimplest (RootEnr root)            = RootDoc (reduceRoot root)
  reductionSheetSimplest HoleEnrichedDoc           = HoleDocument
  reductionSheetSimplest (ParseErrEnrichedDoc prs) = ParseErrDocument prs

reduceRoot (Root graph caption label probtables title sections) =
  let subgraphs = getSubgraphs sections
      (graph', subgraphs') = resolveSubgraphs graph subgraphs
      probtables' = updateProbtables (mkProbtableMap (getProbtables sections)) probtables
      probtables'' = syncProbtables graph probtables'
  in  Root graph' caption label (toList_Probtable probtables'') title 
           (replaceSubgraphs subgraphs' sections)
reduceRoot r = r


isCleanDoc Clean = True
isCleanDoc _     = False

getSubgraphs :: Data x => x -> [Subgraph]
getSubgraphs x = listify (const True) x

everywhereAccum :: Data d => (forall b . Data b => a -> b -> (a,b)) -> a -> d -> (a,d)
everywhereAccum f acc a =
 let (acc'',r) = gfoldl k z a
 in  f acc'' r  
 where k (acc',a2b) a = let (acc'',a') = everywhereAccum f acc' a
                            b = a2b a'
                        in  (acc'', b)
       z e = (acc, e)
       
-- replace an occurrence of a in d with an element from [a]
gReplace :: forall a d . (Typeable a, Data d) => [a] -> d -> ([a],d)
gReplace = mkAccT $ \(i:is) _ -> (is,i)

number :: Data d => Int -> d -> (Int,d)
number  = mkAccT $ \acc _ -> (acc+1, acc)

mkAccT :: forall a b acc . (Typeable acc, Typeable a, Data b) => (acc -> a -> (acc,a)) -> (acc -> b -> (acc,b))
mkAccT f = case cast f  of -- can we do this without requiring Typeable acc?
                  Just g  -> g 
                  Nothing -> \acc b -> (acc,b)


replaceSubgraphs sgs sections = snd $ everywhereAccum gReplace sgs sections

  
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
    let supergraphIDsNames = map (\(Vertex nm _ id _ _) -> (id,nm)) $ fromList_Vertex vs        
        supergraphIDs = map fst supergraphIDsNames        
        correctEdges = filter (\e -> getFrom_Edge e `elem` supergraphIDs &&
                                     getTo_Edge e `elem` supergraphIDs ) -- maybe this should be done by Proxima?
                              (fromList_Edge es) 
    in ( Graph graphDirty vs (toList_Edge correctEdges)
       , map (updateSubgraph supergraphIDsNames) subgraphs)
resolveSubgraphs graph subgraphs = (graph, subgraphs)

isCleanSupergraph (Graph d _ _) = isCleanDoc d

isCleanSubgraph (Subgraph d _ _) = isCleanDoc d

updateSubgraph supergraphIDsNames (Subgraph d vs es) = 
  let subgraphVertices' = toList_Vertex $ catMaybes 
                            [ case lookup i supergraphIDsNames of
                                    Nothing -> Nothing
                                    Just n' -> Just $ Vertex n' s i x y 
                            | Vertex n s i x y <- fromList_Vertex vs
                            ]
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

getProbtables :: Data x => x -> [Probtable]
getProbtables x = listify (const True) x

-- update probtables in the second list with information from the first
updateProbtables probtableMap = map (updateProbtable probtableMap) . fromList_Probtable

updateProbtable :: ProbtableMap -> Probtable -> Probtable
updateProbtable probtableMap probtable@(Probtable id _ _) = 
  case lookup id probtableMap of
    Just probtable' -> probtable'
    Nothing        -> probtable


{-
resize table probs to 
-}
syncProbtables :: Graph -> [Probtable] -> [Probtable]
syncProbtables (Graph _ _ list_Edge) probtables = 
  map syncProbtable probtables
 where edges = [(f,t) | Edge f t <- fromList_Edge list_Edge ]
       getIncomingIDs node = map fst (filter (\(f,t) -> t==node) edges)
       syncProbtable (Probtable id list_Val (Table oldList_Int oldList_Axis oldList_Prob)) = 
         let parents = getIncomingIDs id
             axes = map (getValues probtables) (id:parents)
             list_Axis = toList_Axis $ [ Axis list_Val | list_Val <- axes ]
             nrsOfValues = getAxesNrsOfValues list_Axis
             nrOfProbs = product nrsOfValues
         in  Probtable id list_Val $
               Table parents list_Axis $ -- always refresh, in case values changed (parents does not change, but is refreshed anyway)
                    -- maybe a check on parents and each ones nr of values is prettier
                 if nrsOfValues == getAxesNrsOfValues oldList_Axis
                 then oldList_Prob
                 else toList_Probability $ replicate nrOfProbs (Probability "0")
         --(toList_Probability $ take (arity id) $ 
         --      probs ++ repeat (Probability "dummy"))

getAxesNrsOfValues :: List_Axis -> [Int]
getAxesNrsOfValues list_Axis = [ length $ fromList_Value list_Val
                               | Axis list_Val <- fromList_Axis list_Axis
                               ]

getValues :: [Probtable] -> Int -> List_Value
getValues probtables id = 
  case lookupProbtable id probtables of
    Nothing -> toList_Value [Value "Err"]
    Just (Probtable _ list_Val _) -> list_Val

lookupProbtable _  [] = Nothing
lookupProbtable id (probtable@(Probtable id' list_Val table):probtables) =
  if id==id' then Just probtable else lookupProbtable id probtables

mkProbtableMap :: [Probtable] -> ProbtableMap
mkProbtableMap probtables = map probtableMapEntry probtables
 where probtableMapEntry probtable@(Probtable id _ _) = (id,probtable)

type ProbtableMap = [(Int, Probtable)]




---

reduceDoc :: DocumentLevel Document ClipDoc -> DocumentLevel Document ClipDoc
reduceDoc (DocumentLevel (RootDoc root) focus clip) = 
  DocumentLevel (RootDoc (reduceRoot root)) focus clip











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
replaceSubgraphsParas (sg:sgs) (SubgraphPara _ cap lab :paras) 
                                                       = let (paras',sgs')= replaceSubgraphsParas sgs paras
                                                         in  (SubgraphPara sg cap lab : paras', sgs') 
replaceSubgraphsParas sgs      (para:paras)            = let (paras',sgs')= replaceSubgraphsParas sgs paras
                                                         in  (para : paras', sgs')
replaceSubgraphsParas [] (para:paras)                  = 
   debug Err "Reducer.replaceSubgraphSectionList: too few subgraphs" $
     (para: paras, []) -- internal error: no more subgraphs, so stop replacing
     

{-
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
-}

{-
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
-}
