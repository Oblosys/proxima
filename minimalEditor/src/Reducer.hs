module Reducer (reductionSheet) where

import CommonTypes
import EvalLayerTypes

import EvalLayerUtils

import PresTypes -- for initDoc 

import DocTypes_Generated
import DocUtils_Generated


--translateIO :: LayerStatePres -> low -> high -> editLow -> IO (editHigh, state, low)
reductionSheet :: LayerStateEval -> EnrichedDocLevel EnrichedDoc -> DocumentLevel Document clip ->
               EditEnrichedDoc documentLevel EnrichedDoc -> 
               IO (EditDocument documentLevel Document, LayerStateEval, EnrichedDocLevel EnrichedDoc)
reductionSheet state low high editLow = 
  do { (editHigh, state', low') <- reduceIO state low high editLow
     ; debugLnIO Prs $ "Edit Enriched:"++show editHigh
     ; return (editHigh, state', low')
     }


reduceIO :: LayerStateEval -> EnrichedDocLevel EnrichedDoc -> DocumentLevel Document clip ->
            EditEnrichedDoc documentLevel EnrichedDoc ->
            IO (EditDocument documentLevel Document, LayerStateEval, EnrichedDocLevel EnrichedDoc)
reduceIO state enrLvl docLvl                  (OpenFileEnr upd) =  setUpd NothingUpdated $ debug Err "EvalTranslate.reduce: OpenFile Not implemented yet" $ return (SkipDoc 0, state, enrLvl)

reduceIO state enrLvl (DocumentLevel doc _ _) (SaveFileEnr fpth) = setUpd NothingUpdated $ do {saveFile fpth doc; return (SkipDoc 0, state, enrLvl)}
-- on save, save xmlrep of previous doc. 
reduceIO state enrLvl docLvl InitEnr     = do { doc' <- initDoc 
                                              ; return (SetDoc doc', state, enrLvl) }

reduceIO state enrLvl docLvl EvaluateDocEnr = return (EvaluateDoc, state, enrLvl) 
reduceIO state enrLvl docLvl (SetEnr enrLvl')  = setUpd AllUpdated $ reduceEnrIO state enrLvl docLvl enrLvl'
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
reduceEnrIO state _ (DocumentLevel (RootDoc idd _) _ _) enrDoc@(EnrichedDocLevel (RootEnr _ root _) _) =
 do { return (SetDoc (RootDoc idd root),state, enrDoc )
    }
--
reduceEnrIO state _ (DocumentLevel (RootDoc idd _) _ _) enrDoc@(EnrichedDocLevel (RootEnr _ root _) _) = return $ -- other cases, just copy from decls
  (SetDoc (RootDoc idd root),state, enrDoc )
reduceEnrIO state _ _ enrDoc@(EnrichedDocLevel (HoleEnrichedDoc) oldfocus) = return $
  (SetDoc (HoleDocument),state, enrDoc )
reduceEnrIO state _ _ enrDoc@(EnrichedDocLevel (ParseErrEnrichedDoc nd prs) oldfocus) = return $
  (SetDoc (ParseErrDocument nd prs),state, enrDoc )  -- nd is not right



saveFile :: FilePath -> Document -> IO ()
saveFile filePath doc =
 do { debugLnIO Prs "Saving file"
    ; writeFile filePath $ showXML $ toXMLRootDoc doc
    ; return ()
    }


initDoc :: IO Document
initDoc = 
 do { return $ RootDoc NoIDD $ Root NoIDD (Bin NoIDD (Bin NoIDD (Leaf NoIDD) (Leaf NoIDD)) (Leaf NoIDD))
    }
{- do { let filePath = "Proxima.txt"
    ; debugLnIO Prs $ "Opening file: "++"Proxima.txt"
    ; fileContents <- readFile filePath
    ; return $ RootDoc NoIDD $ ParseErrRoot (RootNode (Root NoIDD) []) (ColP NoIDP 0 . map (StringP NoIDP). lines' $ fileContents)
    }
-}
  
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







