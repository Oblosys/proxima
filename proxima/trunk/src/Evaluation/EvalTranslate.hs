module Evaluation.EvalTranslate where

import Common.CommonTypes
import Evaluation.EvalLayerTypes

import Evaluation.DocUtils
import Evaluation.DocumentEdit
import Text.ParserCombinators.Parsec


translateIO :: (Doc doc, ReductionSheet doc enr clip) =>
               LayerStateEval -> EnrichedDocLevel enr doc -> DocumentLevel doc clip ->
               EditEnrichedDoc (DocumentLevel doc clip) enr doc -> 
               IO (EditDocument doc clip, LayerStateEval, EnrichedDocLevel enr doc)

translateIO state low high editLow = -- extra indirection for debugging purposes
  do { (editHigh, state', low') <- reduceIO state low high editLow
--     ; debugLnIO Prs $ "Edit Enriched:"++show editHigh
     ; return (editHigh, state', low')
     }

reduceIO :: (Doc doc, ReductionSheet doc enr clip) =>
            LayerStateEval -> EnrichedDocLevel enr doc -> DocumentLevel doc clip ->
            EditEnrichedDoc (DocumentLevel doc clip) enr doc -> 
            IO (EditDocument doc clip, LayerStateEval, EnrichedDocLevel enr doc)
reduceIO state enrLvl (DocumentLevel _ _ clip) (OpenFileEnr fpth) = 
 do { mDoc' <- openFile fpth 
	; case mDoc' of
		Just doc' -> return (SetDoc (DocumentLevel doc' NoPathD clip), state, enrLvl)
		Nothing  -> return (SkipDoc 0, state, enrLvl) 
	}

reduceIO  state enrLvl (DocumentLevel doc _ _) (SaveFileEnr fpth) =
 do { saveFile fpth doc
    ; return (SkipDoc 0, state, enrLvl)
    }
-- on save, save xmlrep of previous doc. 

reduceIO state enrLvl (DocumentLevel _ _ clip) InitEnr =
 do { doc' <- initDoc 
    ; return (SetDoc (DocumentLevel doc' NoPathD clip), state, enrLvl) 
    }
reduceIO state enrLvl docLvl EvaluateDocEnr    = return (EvaluateDoc, state, enrLvl) 
reduceIO state enrLvl docLvl (SetEnr enrLvl')  = reductionSheet state enrLvl docLvl enrLvl'
reduceIO state enrLvl docLvl event             = return $ reduce state enrLvl docLvl event


reduce :: (Doc doc, ReductionSheet doc enr clip) =>
          LayerStateEval -> EnrichedDocLevel enr doc -> DocumentLevel doc clip ->
          EditEnrichedDoc (DocumentLevel doc clip) enr doc -> 
          (EditDocument doc clip, LayerStateEval, EnrichedDocLevel enr doc)
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

initDoc :: Doc doc => IO doc
initDoc = initialDoc

openFile :: Doc doc => String -> IO (Maybe doc)
openFile fileName =
 do { debugLnIO Prs $ "Opening file: "++fileName
    ; result <- parseFromFile parseXML fileName
    ; case result of
        Right res -> return $ Just res
        Left err -> do { debugLnIO Err "Parse error"
                       ; debugLnIO Err $ show err
                       ; return $ Nothing
                       }
    } `catch` \ioError -> do { putStr $ "**** IO Error ****\n" ++ show ioError; return Nothing }

saveFile :: Doc doc => FilePath -> doc -> IO ()
saveFile filePath doc =
 do { debugLnIO Prs "Saving file"
    ; writeFile filePath $ showXML $ toXML doc
    ; return ()
    } `catch` \ioError -> do { putStr $ "**** IO Error ****\n" ++ show ioError; return () }
  

