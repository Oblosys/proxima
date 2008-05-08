module Evaluation.EvalTranslate where

import Common.CommonTypes
import Evaluation.EvalLayerTypes

import Proxima.Wrap

import Evaluation.DocUtils
import Evaluation.DocumentEdit
import UU.Parsing.CharParser
import UU.Parsing

translateIO :: (Doc doc, ReductionSheet doc enr clip) =>
               LayerStateEval doc clip -> EnrichedDocLevel enr doc -> DocumentLevel doc clip ->
               EditEnrichedDoc (DocumentLevel doc clip) doc enr node clip token -> 
               IO (EditDocument (DocumentLevel doc clip) doc enr node clip token, LayerStateEval doc clip, EnrichedDocLevel enr doc)

translateIO state low high editLow = -- extra indirection for debugging purposes
  do { (editHigh, state', low') <- reduceIO state low high editLow
--     ; debugLnIO Prs $ "Edit Enriched:"++show editHigh
     ; return (editHigh, state', low')
     }

reduceIO :: (Doc doc, ReductionSheet doc enr clip) =>
            LayerStateEval doc clip -> EnrichedDocLevel enr doc -> DocumentLevel doc clip ->
            EditEnrichedDoc (DocumentLevel doc clip) doc enr node clip token -> 
            IO (EditDocument (DocumentLevel doc clip) doc enr node clip token, LayerStateEval doc clip, EnrichedDocLevel enr doc)
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
reduceIO state enrLvl docLvl (SetEnr enrLvl')  = reductionSheet state enrLvl docLvl enrLvl'
reduceIO state enrLvl docLvl event             = return $ reduce state enrLvl docLvl event


reduce :: (Doc doc, ReductionSheet doc enr clip) =>
          LayerStateEval doc clip -> EnrichedDocLevel enr doc -> DocumentLevel doc clip ->
          EditEnrichedDoc (DocumentLevel doc clip) doc enr node clip token -> 
          (EditDocument (DocumentLevel doc clip) doc enr node clip token, LayerStateEval doc clip, EnrichedDocLevel enr doc)
reduce state enrLvl docLvl (SkipEnr i) = (SkipDoc (i+1), state, enrLvl)
reduce state enrLvl docLvl (WrapEnr wrapped) = (unwrap wrapped, state, enrLvl)
reduce state enrLvl docLvl _            = (SkipDoc 0, state, enrLvl)

initDoc :: Doc doc => IO doc
initDoc = initialDoc

openFile :: Doc doc => String -> IO (Maybe doc)
openFile fileName =
 do { debugLnIO Prs $ "Opening file: "++fileName
    ; xmlStr <- readFile fileName 
    ; let result = parseEither parseXML xmlStr
    ; case result of
        Right res -> return $ Just res
        Left errs -> do { debugLnIO Err "Parse error"
                       ; debugLnIO Err $ show $ take 2 errs
                       ; return $ Nothing
                       }
    } `catch` \ioError -> do { putStr $ "**** IO Error ****\n" ++ show ioError; return Nothing }

parseEither pp inp =
      let res = parseString pp inp
          (Pair v final) = evalSteps (res) 
          errs = getMsgs (res) 
      in  if null errs then Right v else Left errs

saveFile :: Doc doc => FilePath -> doc -> IO ()
saveFile filePath doc =
 do { debugLnIO Prs "Saving file"
    ; writeFile filePath $ showXML $ toXML doc
    ; return ()
    } `catch` \ioError -> do { putStr $ "**** IO Error ****\n" ++ show ioError; return () }
  

