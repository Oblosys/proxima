module EvalTranslate where

import CommonTypes
import EvalLayerTypes

import EvalLayerUtils

import PresTypes -- for initDoc 

--translateIO :: LayerStatePres -> low -> high -> editLow -> IO (editHigh, state, low)
translateIO :: LayerStateEval -> EnrichedDocLevel -> DocumentLevel -> EditEnrichedDoc -> IO (EditDocument, LayerStateEval, EnrichedDocLevel)
translateIO state low high editLow = 
  do { (editHigh, state', low') <- reduceIO state low high editLow
--     ; debugLnIO Prs $ "Edit Enr:"++show editLow
     ; return (editHigh, state', low')
     }


reduceIO :: LayerStateEval -> EnrichedDocLevel -> DocumentLevel -> EditEnrichedDoc -> IO (EditDocument, LayerStateEval, EnrichedDocLevel)
reduceIO state enrLvl docLvl                  (OpenFileEnr upd) =  setUpd NothingUpdated $ debug Err "EvalTranslate.reduce: OpenFile Not implemented yet" $ return (SkipDoc 0, state, enrLvl)

reduceIO state enrLvl (DocumentLevel doc _ _) (SaveFileEnr fpth) = setUpd NothingUpdated $ do {saveFile fpth doc; return (SkipDoc 0, state, enrLvl)}
-- on save, save xmlrep of previous doc. 
reduceIO state enrLvl docLvl InitEnr     = do { doc' <- initDoc 
                                              ; return (SetDoc doc' {-([], emptyFM) -}, state, enrLvl) }

reduceIO state enrLvl docLvl event = return $ reduce state enrLvl docLvl event


reduce :: LayerStateEval -> EnrichedDocLevel -> DocumentLevel -> EditEnrichedDoc ->
         (EditDocument, LayerStateEval, EnrichedDocLevel)
reduce state enrLvl docLvl (SetEnr enrLvl')  = setUpd AllUpdated $ reduceEnr state enrLvl docLvl enrLvl'
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

reduce state enrLvl docLvl EvaluateDocEnr     = (EvaluateDoc, state, enrLvl)
reduce state enrLvl docLvl _            = (SkipDoc 0, state, enrLvl)


-- just copy the enriched document
reduceEnr :: LayerStateEval -> EnrichedDocLevel -> DocumentLevel -> EnrichedDocLevel ->
             (EditDocument, LayerStateEval, EnrichedDocLevel)
reduceEnr state (EnrichedDocLevel (RootEnr _ _ oldIdldcls oldDcls _ _) _) _ enrDoc@(EnrichedDocLevel (RootEnr idd idp idldcls dcls _ _) _) =
  if oldIdldcls == idldcls
  then (SetDoc (RootDoc idd idp dcls),state, enrDoc )
  else (SetDoc (RootDoc idd idp idldcls),state, enrDoc ) -- if list has been edited, take that one
--
reduceEnr state _ _ enrDoc@(EnrichedDocLevel (RootEnr idd idp idldcls dcls _ _) _) = -- other cases, just copy from decls
  (SetDoc (RootDoc idd idp dcls),state, enrDoc )
reduceEnr state _ _ enrDoc@(EnrichedDocLevel (HoleEnrichedDoc) oldfocus) =
  (SetDoc (HoleDoc),state, enrDoc )
reduceEnr state _ _ enrDoc@(EnrichedDocLevel (ParseErrEnrichedDoc nd prs) oldfocus) =
  (SetDoc (ParseErrDoc nd prs),state, enrDoc )  -- nd is not right



saveFile :: FilePath -> Document -> IO ()
saveFile filePath doc =
 do { debugLnIO Prs "Saving file"
    ; writeFile filePath $ showXML $ toXMLRoot doc
    ; return ()
    }


initDoc :: IO Document
initDoc = 
 do { let filePath = "Heliumfile.hs"
    ; debugLnIO Prs $ "Opening file: "++"Proxima.hs"
    ; fileContents <- readFile filePath
    ; return $ RootDoc NoIDD NoIDP $ ParseErrList_Decl (List_DeclNode (List_Decl NoIDD Nil_Decl) []) (ColP NoIDP 0 . map (StringP NoIDP). lines' $ fileContents) {- [] -}
    }
    
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
