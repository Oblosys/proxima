module Reducer where

import CommonTypes
import EvalLayerTypes

import EvalLayerUtils

import PresTypes -- for initDoc 

import DocTypes_Generated
import DocUtils_Generated
import Text.ParserCombinators.Parsec

instance ReductionSheet Document EnrichedDoc ClipDoc where

   -- just copy the enriched document
  reductionSheetSimple state (RootEnr _    (RootE _    _  _     oldIdlDcls) _ _) _ 
                      enrDoc@(RootEnr idd1 (RootE idd2 idp dcls idldcls)    _ _) =
        let dcls' = if oldIdlDcls == idldcls then dcls else idldcls -- if idlist has been edited, take dcls from idlist
           -- dcls' = dcls -- use this assignment to ignore updates on id list
        in  (RootDoc idd1 (Root idd2 idp dcls'), state, enrDoc)
  reductionSheetSimple state _ _ enrDoc =
    case enrDoc of 
      (RootEnr idd1 (RootE idd2 idp dcls idldcls) _ _) -> -- if oldEnr is not RootEnr, then just copy from dcls
        (RootDoc idd1 (Root idd2 idp dcls),state, enrDoc )
      HoleEnrichedDoc ->
        (HoleDocument,state, enrDoc )
      ParseErrEnrichedDoc prs ->
        (ParseErrDocument prs,state, enrDoc )


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