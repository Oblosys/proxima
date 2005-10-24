module EnrUtils (module DocUtils, module EnrUtils) where

import DocTypes
import DocTypes_Generated
import DocUtils


instance Eq String_ where
  (String_ _ str1) == (String_ _ str2) = str1 == str2

-- simple implementation of Eq for Decls, to be used in reducer when comparing which decls list was edited
-- compare only needs to check the things that can be edited in identifier list presentation
--  and disregards presentation identities

-- in case of parse error, return true so other pres is used
instance Eq List_Decl where
  (List_Decl _ decls1) == (List_Decl _ decls2)          = decls1 == decls2
  HoleList_Decl              == HoleList_Decl           = True
  (ParseErrList_Decl _ _)    == _                       = True
  _                          == (ParseErrList_Decl _ _) = True
  _                          == _                       = False


instance Eq ConsList_Decl where
  (Cons_Decl decl1 decls1) == (Cons_Decl decl2 decls2) = decl1 == decl2 && decls1 == decls2
  Nil_Decl                 == Nil_Decl                 = True
  _                        == _                        = False

  
instance Eq Decl where
  (Decl id1 _ _ _ _ _ _ ident1 _) == (Decl id2 _ _ _ _ _ _ ident2 _) = id1 == id2 && ident1 == ident2
  (BoardDecl id1 _ _ _)           == (BoardDecl id2 _ _ _)           = True
  (PPPresentationDecl id1 _ _ _)  == (PPPresentationDecl id2 _ _ _)  = True
  HoleDecl                        == HoleDecl                      = True
  (ParseErrDecl _ _)            == _                             = True
  _                               == (ParseErrDecl _ _)          = True
  _                               == _                             = False        

instance Eq Ident where
  (Ident id1 _ _ str1)  == (Ident id2 _ _ str2)  = id1 == id2 && str1 == str2
  HoleIdent             == HoleIdent             = True
  (ParseErrIdent _ _) == _                     = True
  _                     == (ParseErrIdent _ _) = True
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