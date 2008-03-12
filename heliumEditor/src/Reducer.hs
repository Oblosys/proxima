module Reducer where

import Common.CommonTypes
import Evaluation.EvalLayerTypes

import Evaluation.EvalLayerUtils

import Presentation.PresTypes -- for initDoc 

import DocTypes_Generated
import DocUtils_Generated
import Text.ParserCombinators.Parsec

instance ReductionSheet Document EnrichedDoc ClipDoc where

   -- just copy the enriched document
  reductionSheetSimple state (RootEnr (RootE _  _     oldIdlDcls _) _) _ 
                      enrDoc@(RootEnr (RootE idp dcls idldcls _)    _) =
        let dcls' = if oldIdlDcls == idldcls then dcls else idldcls -- if idlist has been edited, take dcls from idlist
           -- dcls' = dcls -- use this assignment to ignore updates on id list
        in  (RootDoc (Root idp dcls'), state, enrDoc)
  reductionSheetSimple state _ _ enrDoc =
    case enrDoc of 
      (RootEnr (RootE idp dcls idldcls _) _) -> -- if oldEnr is not RootEnr, then just copy from dcls
        (RootDoc (Root idp dcls),state, enrDoc )
      HoleEnrichedDoc ->
        (HoleDocument,state, enrDoc )
      ParseErrEnrichedDoc prs ->
        (ParseErrDocument prs,state, enrDoc )


-- simple implementation of Eq for Decls, to be used in reducer when comparing which decls list was edited
-- compare only needs to check the things that can be edited in identifier list presentation
--  and disregards presentation identities

-- in case of parse error, return true so other pres is used
instance Eq List_Decl where
  (List_Decl decls1)    == (List_Decl decls2)    = decls1 == decls2
  HoleList_Decl         == HoleList_Decl         = True
  (ParseErrList_Decl _) == _                     = True
  _                     == (ParseErrList_Decl _) = True
  _                     == _                     = False


instance Eq ConsList_Decl where
  (Cons_Decl decl1 decls1) == (Cons_Decl decl2 decls2) = decl1 == decl2 && decls1 == decls2
  Nil_Decl                 == Nil_Decl                 = True
  _                        == _                        = False

  
instance Eq Decl where
  (Decl _ _ _ _ _ _ ident1 _) == (Decl _ _ _ _ _ _ ident2 _) = ident1 == ident2
  (BoardDecl _ _ _)           == (BoardDecl _ _ _)           = True
  (PPPresentationDecl _ _ _)  == (PPPresentationDecl _ _ _)  = True
  HoleDecl                    == HoleDecl                    = True
  (ParseErrDecl _)            == _                           = True
  _                           == (ParseErrDecl _)            = True
  _                           == _                           = False        

instance Eq Ident where
  (Ident _ _ str1)  == (Ident _ _ str2)  = str1 == str2
  HoleIdent         == HoleIdent         = True
  (ParseErrIdent _) == _                 = True
  _                 == (ParseErrIdent _) = True
  _                 == _                 = False        
