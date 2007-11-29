module Reducer (reductionSheet) where

import CommonTypes
import EvalLayerTypes

import EvalLayerUtils

import PresTypes -- for initDoc 

import DocTypes_Generated
import DocUtils_Generated
import System.Directory
import Text.ParserCombinators.Parsec


--translateIO :: LayerStatePres -> low -> high -> editLow -> IO (editHigh, state, low)
reductionSheet :: LayerStateEval -> EnrichedDocLevel EnrichedDoc -> DocumentLevel Document clip ->
               EditEnrichedDoc documentLevel EnrichedDoc -> 
               IO (EditDocument documentLevel Document, LayerStateEval, EnrichedDocLevel EnrichedDoc)
reductionSheet state low high editLow = 
  do { (editHigh, state', low') <- reduceIO state low high editLow
--     ; debugLnIO Prs $ "Edit Enr:"++show editLow
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
                                              ; return (SetDoc doc' {-([], emptyFM) -}, state, enrLvl) }

reduceIO state enrLvl docLvl EvaluateDocEnr = return (EvaluateDoc, state, enrLvl) -- uncomment for Helium type checker
--reduceIO state enrLvl docLvl EvaluateDocEnr = do { (doc', state', enrLvl') <- reduceInvLevel state enrLvl docLvl 
--                                                   ; return (SetDoc doc', state', enrLvl') } -- uncomment for Inv interpreter
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
reduceEnrIO :: LayerStateEval -> EnrichedDocLevel EnrichedDoc -> DocumentLevel Document clip -> EnrichedDocLevel EnrichedDoc ->
             IO (EditDocument documentLevel Document, LayerStateEval, EnrichedDocLevel EnrichedDoc)
reduceEnrIO state (EnrichedDocLevel (RootEnr _ (RootE _  _ _ oldIdlDcls) _ _) _) _ 
           enrDoc@(EnrichedDocLevel (RootEnr idd1 (RootE idd2 idp dcls idldcls) _ _) _) =
 do { let dcls' = if oldIdlDcls == idldcls then dcls else idldcls -- if idlist has been edited, take dcls from idlist
          -- dcls' = dcls -- ignore updates on id list
    ; return (SetDoc (RootDoc idd1 (Root idd2 idp dcls')),state, enrDoc )
    }
--
reduceEnrIO state _ _ enrDoc@(EnrichedDocLevel (RootEnr idd1 (RootE idd2 idp dcls idldcls) _ _) _) = return $ -- other cases, just copy from decls
  (SetDoc (RootDoc idd1 (Root idd2 idp dcls)),state, enrDoc )
reduceEnrIO state _ _ enrDoc@(EnrichedDocLevel (HoleEnrichedDoc) oldfocus) = return $
  (SetDoc (HoleDocument),state, enrDoc )
reduceEnrIO state _ _ enrDoc@(EnrichedDocLevel (ParseErrEnrichedDoc prs) oldfocus) = return $
  (SetDoc (ParseErrDocument prs),state, enrDoc )

openFile :: String -> IO (Maybe Document)
openFile fileName =
 do { debugLnIO Prs $ "Opening file: "++fileName
    
    ; result <- parseFromFile parseXML_Document fileName
                      
    ; case result of
        Right res -> return $ Just res
        Left err -> do { debugLnIO Err "Parse error"
                       ; debugLnIO Err $ show err
                       ; return $ Nothing
                       }
    } `catch` \ioError -> do { putStr $ "**** IO Error ****\n" ++ show ioError; return Nothing }

saveFile :: FilePath -> Document -> IO ()
saveFile filePath doc =
 do { debugLnIO Prs "Saving file"
    ; writeFile filePath $ showXML $ toXMLDocument doc
    ; return ()
    } `catch` \ioError -> do { putStr $ "**** IO Error ****\n" ++ show ioError; return () }


initDoc :: IO Document
initDoc = 
 do { let filePath = "Heliumfile.hs"
    ; dir <- getCurrentDirectory
    ; debugLnIO Prs $ "InitDoc: opening file: "++"Proxima.hs"++ " at " ++dir  
    ; fileContents <- readFile filePath
    ; return $ RootDoc NoIDD $ Root NoIDD NoIDP $ ParseErrList_Decl (ColP NoIDP 0 NF . map (StringP NoIDP). lines' $ fileContents) {- [] -}
    }
    -- by putting the text in a parse error node, we don't need to specify a textual parser. Instead,
    -- the proxima parser is used when the presented document is parsed.
    
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









-- TODO: move to other module
instance Eq String_ where
  (String_ _ str1) == (String_ _ str2) = str1 == str2

-- simple implementation of Eq for Decls, to be used in reducer when comparing which decls list was edited
-- compare only needs to check the things that can be edited in identifier list presentation
--  and disregards presentation identities

-- in case of parse error, return true so other pres is used
instance Eq List_Decl where
  (List_Decl _ decls1) == (List_Decl _ decls2)        = decls1 == decls2
  HoleList_Decl              == HoleList_Decl         = True
  (ParseErrList_Decl _)      == _                     = True
  _                          == (ParseErrList_Decl _) = True
  _                          == _                     = False


instance Eq ConsList_Decl where
  (Cons_Decl decl1 decls1) == (Cons_Decl decl2 decls2) = decl1 == decl2 && decls1 == decls2
  Nil_Decl                 == Nil_Decl                 = True
  _                        == _                        = False

  
instance Eq Decl where
  (Decl id1 _ _ _ _ _ _ ident1 _) == (Decl id2 _ _ _ _ _ _ ident2 _) = id1 == id2 && ident1 == ident2
  (BoardDecl id1 _ _ _)           == (BoardDecl id2 _ _ _)           = True
  (PPPresentationDecl id1 _ _ _)  == (PPPresentationDecl id2 _ _ _)  = True
  HoleDecl                        == HoleDecl                      = True
  (ParseErrDecl _)                == _                             = True
  _                               == (ParseErrDecl _)              = True
  _                               == _                             = False        

instance Eq Ident where
  (Ident id1 _ _ str1)  == (Ident id2 _ _ str2)  = id1 == id2 && str1 == str2
  HoleIdent             == HoleIdent             = True
  (ParseErrIdent _)     == _                     = True
  _                     == (ParseErrIdent _)     = True
  _                     == _                     = False        

 {-
data Decl = Decl IDD IDP IDP IDP IDP Bool Bool Ident Exp
          | BoardDecl IDD IDP IDP Board
          | PPPresentationDecl IDD IDP IDP PPPresentation
          | HoleDecl
          | ParseErrDecl node Presentation deriving Show

data Ident = Ident IDD IDP String
           | HoleIdent
           | ParseErrIdent node Presentation deriving Show

  -}