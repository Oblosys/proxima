module Reducer (reductionSheet) where

import CommonTypes hiding (Dirty (..))
import EvalLayerTypes

import EvalLayerUtils

import PresTypes -- for initDoc 

import DocTypes_Generated
import DocUtils_Generated
import DocumentEdit_Generated
import Text.ParserCombinators.Parsec

--translateIO :: LayerStatePres -> low -> high -> editLow -> IO (editHigh, state, low)
reductionSheet :: LayerStateEval -> EnrichedDocLevel EnrichedDoc -> DocumentLevel Document clip ->
               EditEnrichedDoc documentLevel EnrichedDoc -> 
               IO (EditDocument documentLevel Document, LayerStateEval, EnrichedDocLevel EnrichedDoc)
reductionSheet state low high editLow = 
  do { (editHigh, state', low') <- reduceIO state low high editLow
--     ; debugLnIO Prs $ "Edit Enriched:"++show editHigh
     ; return (editHigh, state', low')
     }


reduceIO :: LayerStateEval -> EnrichedDocLevel EnrichedDoc -> DocumentLevel Document clip ->
            EditEnrichedDoc documentLevel EnrichedDoc ->
            IO (EditDocument documentLevel Document, LayerStateEval, EnrichedDocLevel EnrichedDoc)
reduceIO state enrLvl docLvl                  (OpenFileEnr fpth) =   do { mDoc' <- openFile fpth 
																	    ; case mDoc' of
																	        Just doc' -> return (SetDoc doc', state, enrLvl)
																	        Nothing  -> return (SkipDoc 0, state, enrLvl) 
																	    }

reduceIO state enrLvl (DocumentLevel doc _ _) (SaveFileEnr fpth) = do {saveFile fpth doc; return (SkipDoc 0, state, enrLvl)}
-- on save, save xmlrep of previous doc. 
reduceIO state enrLvl docLvl InitEnr     = do { doc' <- initDoc 
                                              ; return (SetDoc doc', state, enrLvl) }

reduceIO state enrLvl docLvl EvaluateDocEnr = return (EvaluateDoc, state, enrLvl) 
reduceIO state enrLvl docLvl (SetEnr enrLvl')  = reduceEnrIO state enrLvl docLvl enrLvl'
reduceIO state enrLvl docLvl event = return $ reduce state enrLvl docLvl event


reduce :: LayerStateEval -> EnrichedDocLevel EnrichedDoc -> DocumentLevel Document clip ->
          EditEnrichedDoc documentLevel EnrichedDoc ->
          (EditDocument documentLevel Document, LayerStateEval, EnrichedDocLevel EnrichedDoc)
reduce state enrLvl docLvl (SkipEnr i) = (SkipDoc (i+1), state, enrLvl)
reduce state enrLvl docLvl NavUpDocEnr = (NavUpDoc, state, enrLvl)
reduce state enrLvl docLvl NavDownDocEnr = (NavDownDoc, state, enrLvl)
reduce state enrLvl docLvl NavLeftDocEnr = (NavLeftDoc, state, enrLvl)
reduce state enrLvl docLvl NavRightDocEnr = (NavRightDoc, state, enrLvl)
reduce state enrLvl docLvl CutDocEnr    = (CutDoc, state, enrLvl)
reduce state enrLvl docLvl CopyDocEnr   = (CopyDoc, state, enrLvl)
reduce state enrLvl docLvl PasteDocEnr  = (PasteDoc, state, enrLvl)
reduce state enrLvl docLvl DeleteDocEnr = (DeleteDoc, state, enrLvl)
reduce state enrLvl docLvl (UpdateDocEnr upd) = (UpdateDoc upd, state, enrLvl)

reduce state enrLvl docLvl _            = (SkipDoc 0, state, enrLvl)


-- just copy the enriched document
reduceEnrIO :: LayerStateEval -> EnrichedDocLevel EnrichedDoc -> DocumentLevel Document clip ->
               EnrichedDocLevel EnrichedDoc ->
               IO (EditDocument documentLevel Document, LayerStateEval, EnrichedDocLevel EnrichedDoc)
reduceEnrIO state _ (DocumentLevel (RootDoc idd _) _ _) enrDoc@(EnrichedDocLevel (RootEnr _ root _) _) = return $ -- other cases, just copy from decls
  (SetDoc (RootDoc idd (reduceRoot root)),state, enrDoc )
reduceEnrIO state _ _ enrDoc@(EnrichedDocLevel (HoleEnrichedDoc) oldfocus) = return $
  (SetDoc (HoleDocument),state, enrDoc )
reduceEnrIO state _ _ enrDoc@(EnrichedDocLevel (ParseErrEnrichedDoc prs) oldfocus) = return $
  (SetDoc (ParseErrDocument prs),state, enrDoc )  -- nd is not right


saveFile :: FilePath -> Document -> IO ()
saveFile filePath doc =
 do { debugLnIO Prs "Saving file"
    ; writeFile filePath $ showXML $ toXMLRootDoc doc
    ; return ()
    }


initDoc :: IO Document
initDoc = return defaultInitDoc

openFile :: String -> IO (Maybe Document)
openFile fileName =
 do { debugLnIO Prs $ "Opening file: "++fileName
    ; result <- parseFromFile parseXML_Root fileName
    ; case result of
        Right res -> return $ Just $ RootDoc NoIDD $ res
        Left err -> do { debugLnIO Err "Parse error"
                       ; debugLnIO Err $ show err
                       ; return $ Nothing
                       }
    }
  
defaultInitDoc = RootDoc NoIDD $ Root NoIDD (Bin NoIDD (Bin NoIDD (Leaf NoIDD) (Leaf NoIDD)) (Leaf NoIDD))
                                          (Graph NoIDD (Clean NoIDD) (List_Vertex NoIDD Nil_Vertex)
                                                                     (List_Edge NoIDD Nil_Edge))
                                          (List_Section NoIDD Nil_Section)
-- lines' works for Unix, Mac, and Dos format
lines'     :: String -> [String]
lines' ""   = []
lines' s    = let (l,s') = break (\c->c=='\n' || c=='\r') s
             in l : case s' of []      -> []
                               ('\r' :'\n':s'') -> lines' s''   -- a Dos "\n\r" encountered on Unix or Mac platform
                               ('\n' :s'') -> lines' s''         -- the current platform's linebreak (?)
                                                                 -- or a Unix "\n" encountered on a Dos or Mac platform
                               ('\r':s'') -> lines' s''          -- a  Mac "\r" encountered on Dos or Unix platform 
-- what happens with '\r' on mac? is it automatically converted to '\n'? If so, will a Dos file then contain "\n\n"?






isCleanDoc (Clean _) = True
isCleanDoc _         = False

reduceRoot (Root idd tree graph sections) =
  let subgraphs = getSubgraphs sections
      (graph', subgraphs') = resolveSubgraphs graph subgraphs
  in  Root idd tree graph' (replaceSubgraphs sections subgraphs')

getSubgraphs :: List_Section -> [Subgraph]
getSubgraphs sections = map getSubgraph (fromList_Section sections)
  where getSubgraph (Section _ _ subgraph) = subgraph

-- replace each subgraph in sections with a subgraph from subgraphs
replaceSubgraphs :: List_Section -> [Subgraph] -> List_Section
replaceSubgraphs sections subgraphs = 
  toList_Section $ zipWith replaceSubgraph (fromList_Section sections) subgraphs
  where replaceSubgraph (Section idd paras _) subgraph = Section idd paras subgraph
  
-- if the graph is clean, and one of the subgraphs dirty, resolveSubgraphs
-- adds the edges from the dirty subgraph to the graph
-- if the graph is dirty, any vertices not in the graph are removed from the subgraphs 
resolveSubgraphs :: Graph -> [Subgraph] -> (Graph, [Subgraph])
resolveSubgraphs graph@(Graph _ graphDirty vs _) subgraphs = debug Err (show graph) $
  if isCleanDoc graphDirty 
  then case filter (\(Subgraph _ d _ _) -> not $ isCleanDoc d) $ subgraphs of
         [] -> (graph, subgraphs) -- all are clean
         (dirtySubgraph:_) -> -- at least one subgraph dirty
            (addEdgesFromSubgraph dirtySubgraph graph, subgraphs)
  else -- graph is dirty    remove deleted nodes from subgraphs
    let superGraphIDs = map getID_Vertex $ fromList_Vertex vs
    in (graph, map (removeOldVertices superGraphIDs) subgraphs)

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
  in Graph id d vs graphEdges

getFrom_Edge (Edge _ (Int_ _ fromV) _) = fromV
getTo_Edge (Edge _ _ (Int_ _ toV)) = toV

getID_Vertex (Vertex _ _ (Int_ _ id) _ _) = id

