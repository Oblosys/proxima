module EvalTranslate where

import CommonTypes
import EvalLayerTypes

import EvalLayerUtils

import PresTypes -- for initDoc 

import qualified EvaluateInv

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

reduceIO state enrLvl docLvl EvaluateDocEnr = return (EvaluateDoc, state, enrLvl) -- uncomment for Helium type checker
--reduceIO state enrLvl docLvl EvaluateDocEnr = do { (doc', state', enrLvl') <- reduceInvLevel state enrLvl docLvl 
--                                                   ; return (SetDoc doc', state', enrLvl') } -- uncomment for Inv interpreter
reduceIO state enrLvl docLvl (SetEnr enrLvl')  = setUpd AllUpdated $ reduceEnrIO state enrLvl docLvl enrLvl'
reduceIO state enrLvl docLvl event = return $ reduce state enrLvl docLvl event


reduce :: LayerStateEval -> EnrichedDocLevel -> DocumentLevel -> EditEnrichedDoc ->
         (EditDocument, LayerStateEval, EnrichedDocLevel)
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
reduceEnrIO :: LayerStateEval -> EnrichedDocLevel -> DocumentLevel -> EnrichedDocLevel ->
             IO (EditDocument, LayerStateEval, EnrichedDocLevel)
reduceEnrIO state (EnrichedDocLevel (RootEnr _ _ oldIdldcls oldDcls _ _) _) _ enrDoc@(EnrichedDocLevel (RootEnr idd idp idldcls dcls _ _) _) =
 do { let -- dcls' = if oldIdldcls == idldcls then dcls else idldcls -- if idlist has been edited, take dcls from idlist
          dcls' = dcls -- ignore updates on id list
    ; dcls'' <- reduceList_Decl dcls'
    ; return (SetDoc (RootDoc idd idp dcls''),state, enrDoc )
    }
--
reduceEnrIO state _ _ enrDoc@(EnrichedDocLevel (RootEnr idd idp idldcls dcls _ _) _) = return $ -- other cases, just copy from decls
  (SetDoc (RootDoc idd idp dcls),state, enrDoc )
reduceEnrIO state _ _ enrDoc@(EnrichedDocLevel (HoleEnrichedDoc) oldfocus) = return $
  (SetDoc (HoleDocument),state, enrDoc )
reduceEnrIO state _ _ enrDoc@(EnrichedDocLevel (ParseErrEnrichedDoc nd prs) oldfocus) = return $
  (SetDoc (ParseErrDocument nd prs),state, enrDoc )  -- nd is not right



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







reduceInvLevel :: LayerStateEval -> EnrichedDocLevel -> DocumentLevel -> IO (Document, LayerStateEval, EnrichedDocLevel)
reduceInvLevel state enrDocLvl@(EnrichedDocLevel (RootEnr idd idp idldcls dcls _ _) _) docLevel =
 do { dcls'' <- reduceList_Decl dcls
 --   ; dcls'' <- return $ evalList_Decl dcls'
    ; return ((RootDoc idd idp dcls''),state, enrDocLvl )
    }

reduceList_Decl :: List_Decl -> IO List_Decl
reduceList_Decl (List_Decl idd clst) =
  do { clst' <- reduceInvConsList_Decl clst
     ; return $ List_Decl idd clst'
     }
reduceList_Decl lst = return $ lst -- Hole or parseErr

reduceInvConsList_Decl :: ConsList_Decl -> IO ConsList_Decl
reduceInvConsList_Decl Nil_Decl             = return $ Nil_Decl
reduceInvConsList_Decl (Cons_Decl dcl clst) =
  do { dcl'  <- reduceInvDecl dcl
     ; clst' <- reduceInvConsList_Decl clst
     ; return $ Cons_Decl dcl' clst'
     }

reduceInvDecl :: Decl -> IO Decl
reduceInvDecl (InvDecl idd idp0 idp1 inv) =
  do { inv' <- reduceInv inv
     ; return $ InvDecl idd idp0 idp1 inv'
     }
reduceInvDecl dcl = return dcl



reduceInv :: Inv -> IO Inv
reduceInv inv@(Inv idd errDoc enr eval button) = 
  do { errDoc' <- reduceInvView eval enr
     ; enr'    <- evalErrDoc eval errDoc' enr
     ; return $ Inv idd errDoc' enr' eval (Skip NoIDD) 
     }
reduceInv inv = return inv

reduceInvView :: String_ -> View -> IO EitherDocView
reduceInvView eval view = EvaluateInv.reduce (string_ eval) view


evalErrDoc :: String_ -> EitherDocView -> View -> IO View
evalErrDoc eval errDoc oldEnr = EvaluateInv.evaluate (string_ eval) errDoc oldEnr
