module DocumentEdit_Generated where

import Common.CommonTypes
import Evaluation.DocTypes
import DocTypes_Generated
import DocUtils_Generated
import Evaluation.DocumentEdit
import Evaluation.DocUtils
import Presentation.PresTypes

instance Editable HeliumTypeInfo Document Node ClipDoc UserToken where
  hole =  ([],[],[])
  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- Clip instance                                                        --
--------------------------------------------------------------------------

instance Clip ClipDoc where
  arityClip Clip_Nothing = -1
  arityClip (Clip_Dummy x) = arity x
  arityClip (Clip_EnrichedDoc x) = arity x
  arityClip (Clip_Root x) = arity x
  arityClip (Clip_RootE x) = arity x
  arityClip (Clip_Decl x) = arity x
  arityClip (Clip_Ident x) = arity x
  arityClip (Clip_Exp x) = arity x
  arityClip (Clip_Alt x) = arity x
  arityClip (Clip_Board x) = arity x
  arityClip (Clip_BoardRow x) = arity x
  arityClip (Clip_BoardSquare x) = arity x
  arityClip (Clip_PPPresentation x) = arity x
  arityClip (Clip_Slide x) = arity x
  arityClip (Clip_ItemList x) = arity x
  arityClip (Clip_ListType x) = arity x
  arityClip (Clip_Item x) = arity x
  arityClip (Clip_List_Decl x) = arity x
  arityClip (Clip_List_Alt x) = arity x
  arityClip (Clip_List_Exp x) = arity x
  arityClip (Clip_List_Slide x) = arity x
  arityClip (Clip_List_Item x) = arity x
  arityClip (Clip_Document x) = arity x
  arityClip (Clip_Bool x) = arity x
  arityClip (Clip_Int x) = arity x
  arityClip (Clip_String x) = arity x
  arityClip (Clip_Float x) = arity x

  alternativesClip Clip_Nothing = []
  alternativesClip (Clip_Dummy x) = alternatives x
  alternativesClip (Clip_EnrichedDoc x) = alternatives x
  alternativesClip (Clip_Root x) = alternatives x
  alternativesClip (Clip_RootE x) = alternatives x
  alternativesClip (Clip_Decl x) = alternatives x
  alternativesClip (Clip_Ident x) = alternatives x
  alternativesClip (Clip_Exp x) = alternatives x
  alternativesClip (Clip_Alt x) = alternatives x
  alternativesClip (Clip_Board x) = alternatives x
  alternativesClip (Clip_BoardRow x) = alternatives x
  alternativesClip (Clip_BoardSquare x) = alternatives x
  alternativesClip (Clip_PPPresentation x) = alternatives x
  alternativesClip (Clip_Slide x) = alternatives x
  alternativesClip (Clip_ItemList x) = alternatives x
  alternativesClip (Clip_ListType x) = alternatives x
  alternativesClip (Clip_Item x) = alternatives x
  alternativesClip (Clip_List_Decl x) = alternatives x
  alternativesClip (Clip_List_Alt x) = alternatives x
  alternativesClip (Clip_List_Exp x) = alternatives x
  alternativesClip (Clip_List_Slide x) = alternatives x
  alternativesClip (Clip_List_Item x) = alternatives x
  alternativesClip (Clip_Document x) = alternatives x
  alternativesClip (Clip_Bool x) = alternatives x
  alternativesClip (Clip_Int x) = alternatives x
  alternativesClip (Clip_String x) = alternatives x
  alternativesClip (Clip_Float x) = alternatives x

  holeClip Clip_Nothing = Clip_Nothing
  holeClip (Clip_Dummy x) = Clip_Dummy hole
  holeClip (Clip_EnrichedDoc x) = Clip_EnrichedDoc hole
  holeClip (Clip_Root x) = Clip_Root hole
  holeClip (Clip_RootE x) = Clip_RootE hole
  holeClip (Clip_Decl x) = Clip_Decl hole
  holeClip (Clip_Ident x) = Clip_Ident hole
  holeClip (Clip_Exp x) = Clip_Exp hole
  holeClip (Clip_Alt x) = Clip_Alt hole
  holeClip (Clip_Board x) = Clip_Board hole
  holeClip (Clip_BoardRow x) = Clip_BoardRow hole
  holeClip (Clip_BoardSquare x) = Clip_BoardSquare hole
  holeClip (Clip_PPPresentation x) = Clip_PPPresentation hole
  holeClip (Clip_Slide x) = Clip_Slide hole
  holeClip (Clip_ItemList x) = Clip_ItemList hole
  holeClip (Clip_ListType x) = Clip_ListType hole
  holeClip (Clip_Item x) = Clip_Item hole
  holeClip (Clip_List_Decl x) = Clip_List_Decl hole
  holeClip (Clip_List_Alt x) = Clip_List_Alt hole
  holeClip (Clip_List_Exp x) = Clip_List_Exp hole
  holeClip (Clip_List_Slide x) = Clip_List_Slide hole
  holeClip (Clip_List_Item x) = Clip_List_Item hole
  holeClip (Clip_Document x) = Clip_Document hole
  holeClip (Clip_Bool x) = Clip_Bool hole
  holeClip (Clip_Int x) = Clip_Int hole
  holeClip (Clip_String x) = Clip_String hole
  holeClip (Clip_Float x) = Clip_Float hole

  isListClip Clip_Nothing = False
  isListClip (Clip_Dummy x) = isList x
  isListClip (Clip_EnrichedDoc x) = isList x
  isListClip (Clip_Root x) = isList x
  isListClip (Clip_RootE x) = isList x
  isListClip (Clip_Decl x) = isList x
  isListClip (Clip_Ident x) = isList x
  isListClip (Clip_Exp x) = isList x
  isListClip (Clip_Alt x) = isList x
  isListClip (Clip_Board x) = isList x
  isListClip (Clip_BoardRow x) = isList x
  isListClip (Clip_BoardSquare x) = isList x
  isListClip (Clip_PPPresentation x) = isList x
  isListClip (Clip_Slide x) = isList x
  isListClip (Clip_ItemList x) = isList x
  isListClip (Clip_ListType x) = isList x
  isListClip (Clip_Item x) = isList x
  isListClip (Clip_List_Decl x) = isList x
  isListClip (Clip_List_Alt x) = isList x
  isListClip (Clip_List_Exp x) = isList x
  isListClip (Clip_List_Slide x) = isList x
  isListClip (Clip_List_Item x) = isList x
  isListClip (Clip_Document x) = isList x
  isListClip (Clip_Bool x) = isList x
  isListClip (Clip_Int x) = isList x
  isListClip (Clip_String x) = isList x
  isListClip (Clip_Float x) = isList x

  insertListClip i c Clip_Nothing = Clip_Nothing
  insertListClip i c (Clip_Dummy x) = insertList i c x
  insertListClip i c (Clip_EnrichedDoc x) = insertList i c x
  insertListClip i c (Clip_Root x) = insertList i c x
  insertListClip i c (Clip_RootE x) = insertList i c x
  insertListClip i c (Clip_Decl x) = insertList i c x
  insertListClip i c (Clip_Ident x) = insertList i c x
  insertListClip i c (Clip_Exp x) = insertList i c x
  insertListClip i c (Clip_Alt x) = insertList i c x
  insertListClip i c (Clip_Board x) = insertList i c x
  insertListClip i c (Clip_BoardRow x) = insertList i c x
  insertListClip i c (Clip_BoardSquare x) = insertList i c x
  insertListClip i c (Clip_PPPresentation x) = insertList i c x
  insertListClip i c (Clip_Slide x) = insertList i c x
  insertListClip i c (Clip_ItemList x) = insertList i c x
  insertListClip i c (Clip_ListType x) = insertList i c x
  insertListClip i c (Clip_Item x) = insertList i c x
  insertListClip i c (Clip_List_Decl x) = insertList i c x
  insertListClip i c (Clip_List_Alt x) = insertList i c x
  insertListClip i c (Clip_List_Exp x) = insertList i c x
  insertListClip i c (Clip_List_Slide x) = insertList i c x
  insertListClip i c (Clip_List_Item x) = insertList i c x
  insertListClip i c (Clip_Document x) = insertList i c x
  insertListClip i c (Clip_Bool x) = insertList i c x
  insertListClip i c (Clip_Int x) = insertList i c x
  insertListClip i c (Clip_String x) = insertList i c x
  insertListClip i c (Clip_Float x) = insertList i c x

  removeListClip i Clip_Nothing = Clip_Nothing
  removeListClip i (Clip_Dummy x) = removeList i x
  removeListClip i (Clip_EnrichedDoc x) = removeList i x
  removeListClip i (Clip_Root x) = removeList i x
  removeListClip i (Clip_RootE x) = removeList i x
  removeListClip i (Clip_Decl x) = removeList i x
  removeListClip i (Clip_Ident x) = removeList i x
  removeListClip i (Clip_Exp x) = removeList i x
  removeListClip i (Clip_Alt x) = removeList i x
  removeListClip i (Clip_Board x) = removeList i x
  removeListClip i (Clip_BoardRow x) = removeList i x
  removeListClip i (Clip_BoardSquare x) = removeList i x
  removeListClip i (Clip_PPPresentation x) = removeList i x
  removeListClip i (Clip_Slide x) = removeList i x
  removeListClip i (Clip_ItemList x) = removeList i x
  removeListClip i (Clip_ListType x) = removeList i x
  removeListClip i (Clip_Item x) = removeList i x
  removeListClip i (Clip_List_Decl x) = removeList i x
  removeListClip i (Clip_List_Alt x) = removeList i x
  removeListClip i (Clip_List_Exp x) = removeList i x
  removeListClip i (Clip_List_Slide x) = removeList i x
  removeListClip i (Clip_List_Item x) = removeList i x
  removeListClip i (Clip_Document x) = removeList i x
  removeListClip i (Clip_Bool x) = removeList i x
  removeListClip i (Clip_Int x) = removeList i x
  removeListClip i (Clip_String x) = removeList i x
  removeListClip i (Clip_Float x) = removeList i x




--------------------------------------------------------------------------
-- Editable instances                                                   --
--------------------------------------------------------------------------

instance Editable Dummy Document Node ClipDoc UserToken where
  select [] x = Clip_Dummy x
  select (0:p) (Dummy x0 x1) = select p x0
  select (1:p) (Dummy x0 x1) = select p x1
  select _ _ = Clip_Nothing

  paste [] (Clip_Dummy c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Dummy") x
  paste (0:p) c (Dummy x0 x1) = Dummy (paste p c x0) x1
  paste (1:p) c (Dummy x0 x1) = Dummy x0 (paste p c x1)
  paste _ _ x = x

  alternatives _ = [ ("Dummy {Root} {Dummy} "  , Clip_Dummy $ Dummy hole hole)
                   ,("{Dummy}", Clip_Dummy hole)
                   ]

  arity (Dummy x0 x1) = 2
  arity _                        = 0

  parseErr = ParseErrDummy

  hole = HoleDummy

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Root Document Node ClipDoc UserToken where
  select [] x = Clip_Root x
  select (0:p) (Root _ x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Root c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Root") x
  paste (0:p) c (Root i0 x0) = Root i0 (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("Root {List_Decl} "  , Clip_Root $ Root NoIDP hole)
                   ,("{Root}", Clip_Root hole)
                   ]

  arity (Root _ x0) = 1
  arity _                        = 0

  parseErr = ParseErrRoot

  hole = HoleRoot

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable RootE Document Node ClipDoc UserToken where
  select [] x = Clip_RootE x
  select (0:p) (RootE _ x0 x1) = select p x0
  select (1:p) (RootE _ x0 x1) = select p x1
  select _ _ = Clip_Nothing

  paste [] (Clip_RootE c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on RootE") x
  paste (0:p) c (RootE i0 x0 x1) = RootE i0 (paste p c x0) x1
  paste (1:p) c (RootE i0 x0 x1) = RootE i0 x0 (paste p c x1)
  paste _ _ x = x

  alternatives _ = [ ("RootE {List_Decl} {List_Decl} "  , Clip_RootE $ RootE NoIDP hole hole)
                   ,("{RootE}", Clip_RootE hole)
                   ]

  arity (RootE _ x0 x1) = 2
  arity _                        = 0

  parseErr = ParseErrRootE

  hole = HoleRootE

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Decl Document Node ClipDoc UserToken where
  select [] x = Clip_Decl x
  select (0:p) (Decl _ _ _ _ x0 x1 x2 x3) = select p x0
  select (1:p) (Decl _ _ _ _ x0 x1 x2 x3) = select p x1
  select (2:p) (Decl _ _ _ _ x0 x1 x2 x3) = select p x2
  select (3:p) (Decl _ _ _ _ x0 x1 x2 x3) = select p x3
  select (0:p) (BoardDecl _ _ x0) = select p x0
  select (0:p) (PPPresentationDecl _ _ x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Decl c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Decl") x
  paste (0:p) c (Decl i0 i1 i2 i3 x0 x1 x2 x3) = Decl i0 i1 i2 i3 (paste p c x0) x1 x2 x3
  paste (1:p) c (Decl i0 i1 i2 i3 x0 x1 x2 x3) = Decl i0 i1 i2 i3 x0 (paste p c x1) x2 x3
  paste (2:p) c (Decl i0 i1 i2 i3 x0 x1 x2 x3) = Decl i0 i1 i2 i3 x0 x1 (paste p c x2) x3
  paste (3:p) c (Decl i0 i1 i2 i3 x0 x1 x2 x3) = Decl i0 i1 i2 i3 x0 x1 x2 (paste p c x3)
  paste (0:p) c (BoardDecl i0 i1 x0) = BoardDecl i0 i1 (paste p c x0)
  paste (0:p) c (PPPresentationDecl i0 i1 x0) = PPPresentationDecl i0 i1 (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("Decl {Bool} {Bool} {Ident} {Exp} "  , Clip_Decl $ Decl NoIDP NoIDP NoIDP NoIDP hole hole hole hole)
                   , ("BoardDecl {Board} "  , Clip_Decl $ BoardDecl NoIDP NoIDP hole)
                   , ("PPPresentationDecl {PPPresentation} "  , Clip_Decl $ PPPresentationDecl NoIDP NoIDP hole)
                   ,("{Decl}", Clip_Decl hole)
                   ]

  arity (Decl _ _ _ _ x0 x1 x2 x3) = 4
  arity (BoardDecl _ _ x0) = 1
  arity (PPPresentationDecl _ _ x0) = 1
  arity _                        = 0

  parseErr = ParseErrDecl

  hole = HoleDecl

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Ident Document Node ClipDoc UserToken where
  select [] x = Clip_Ident x
  select (0:p) (Ident _ _ x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Ident c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Ident") x
  paste (0:p) c (Ident i0 i1 x0) = Ident i0 i1 (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("Ident {String} "  , Clip_Ident $ Ident NoIDP NoIDP hole)
                   ,("{Ident}", Clip_Ident hole)
                   ]

  arity (Ident _ _ x0) = 1
  arity _                        = 0

  parseErr = ParseErrIdent

  hole = HoleIdent

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Exp Document Node ClipDoc UserToken where
  select [] x = Clip_Exp x
  select (0:p) (PlusExp _ x0 x1) = select p x0
  select (1:p) (PlusExp _ x0 x1) = select p x1
  select (0:p) (TimesExp _ x0 x1) = select p x0
  select (1:p) (TimesExp _ x0 x1) = select p x1
  select (0:p) (DivExp _ x0 x1) = select p x0
  select (1:p) (DivExp _ x0 x1) = select p x1
  select (0:p) (PowerExp _ x0 x1) = select p x0
  select (1:p) (PowerExp _ x0 x1) = select p x1
  select (0:p) (BoolExp _ x0) = select p x0
  select (0:p) (IntExp _ x0) = select p x0
  select (0:p) (LamExp _ _ x0 x1) = select p x0
  select (1:p) (LamExp _ _ x0 x1) = select p x1
  select (0:p) (AppExp x0 x1) = select p x0
  select (1:p) (AppExp x0 x1) = select p x1
  select (0:p) (CaseExp _ _ x0 x1) = select p x0
  select (1:p) (CaseExp _ _ x0 x1) = select p x1
  select (0:p) (LetExp _ _ x0 x1) = select p x0
  select (1:p) (LetExp _ _ x0 x1) = select p x1
  select (0:p) (IdentExp x0) = select p x0
  select (0:p) (IfExp _ _ _ x0 x1 x2) = select p x0
  select (1:p) (IfExp _ _ _ x0 x1 x2) = select p x1
  select (2:p) (IfExp _ _ _ x0 x1 x2) = select p x2
  select (0:p) (ParenExp _ _ x0) = select p x0
  select (0:p) (ListExp _ _ _ x0) = select p x0
  select (0:p) (ProductExp _ _ _ x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Exp c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Exp") x
  paste (0:p) c (PlusExp i0 x0 x1) = PlusExp i0 (paste p c x0) x1
  paste (1:p) c (PlusExp i0 x0 x1) = PlusExp i0 x0 (paste p c x1)
  paste (0:p) c (TimesExp i0 x0 x1) = TimesExp i0 (paste p c x0) x1
  paste (1:p) c (TimesExp i0 x0 x1) = TimesExp i0 x0 (paste p c x1)
  paste (0:p) c (DivExp i0 x0 x1) = DivExp i0 (paste p c x0) x1
  paste (1:p) c (DivExp i0 x0 x1) = DivExp i0 x0 (paste p c x1)
  paste (0:p) c (PowerExp i0 x0 x1) = PowerExp i0 (paste p c x0) x1
  paste (1:p) c (PowerExp i0 x0 x1) = PowerExp i0 x0 (paste p c x1)
  paste (0:p) c (BoolExp i0 x0) = BoolExp i0 (paste p c x0)
  paste (0:p) c (IntExp i0 x0) = IntExp i0 (paste p c x0)
  paste (0:p) c (LamExp i0 i1 x0 x1) = LamExp i0 i1 (paste p c x0) x1
  paste (1:p) c (LamExp i0 i1 x0 x1) = LamExp i0 i1 x0 (paste p c x1)
  paste (0:p) c (AppExp x0 x1) = AppExp (paste p c x0) x1
  paste (1:p) c (AppExp x0 x1) = AppExp x0 (paste p c x1)
  paste (0:p) c (CaseExp i0 i1 x0 x1) = CaseExp i0 i1 (paste p c x0) x1
  paste (1:p) c (CaseExp i0 i1 x0 x1) = CaseExp i0 i1 x0 (paste p c x1)
  paste (0:p) c (LetExp i0 i1 x0 x1) = LetExp i0 i1 (paste p c x0) x1
  paste (1:p) c (LetExp i0 i1 x0 x1) = LetExp i0 i1 x0 (paste p c x1)
  paste (0:p) c (IdentExp x0) = IdentExp (paste p c x0)
  paste (0:p) c (IfExp i0 i1 i2 x0 x1 x2) = IfExp i0 i1 i2 (paste p c x0) x1 x2
  paste (1:p) c (IfExp i0 i1 i2 x0 x1 x2) = IfExp i0 i1 i2 x0 (paste p c x1) x2
  paste (2:p) c (IfExp i0 i1 i2 x0 x1 x2) = IfExp i0 i1 i2 x0 x1 (paste p c x2)
  paste (0:p) c (ParenExp i0 i1 x0) = ParenExp i0 i1 (paste p c x0)
  paste (0:p) c (ListExp i0 i1 i2 x0) = ListExp i0 i1 i2 (paste p c x0)
  paste (0:p) c (ProductExp i0 i1 i2 x0) = ProductExp i0 i1 i2 (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("PlusExp {Exp} {Exp} "  , Clip_Exp $ PlusExp NoIDP hole hole)
                   , ("TimesExp {Exp} {Exp} "  , Clip_Exp $ TimesExp NoIDP hole hole)
                   , ("DivExp {Exp} {Exp} "  , Clip_Exp $ DivExp NoIDP hole hole)
                   , ("PowerExp {Exp} {Exp} "  , Clip_Exp $ PowerExp NoIDP hole hole)
                   , ("BoolExp {Bool} "  , Clip_Exp $ BoolExp NoIDP hole)
                   , ("IntExp {Int} "  , Clip_Exp $ IntExp NoIDP hole)
                   , ("LamExp {Ident} {Exp} "  , Clip_Exp $ LamExp NoIDP NoIDP hole hole)
                   , ("AppExp {Exp} {Exp} "  , Clip_Exp $ AppExp hole hole)
                   , ("CaseExp {Exp} {List_Alt} "  , Clip_Exp $ CaseExp NoIDP NoIDP hole hole)
                   , ("LetExp {List_Decl} {Exp} "  , Clip_Exp $ LetExp NoIDP NoIDP hole hole)
                   , ("IdentExp {Ident} "  , Clip_Exp $ IdentExp hole)
                   , ("IfExp {Exp} {Exp} {Exp} "  , Clip_Exp $ IfExp NoIDP NoIDP NoIDP hole hole hole)
                   , ("ParenExp {Exp} "  , Clip_Exp $ ParenExp NoIDP NoIDP hole)
                   , ("ListExp {List_Exp} "  , Clip_Exp $ ListExp NoIDP NoIDP [] hole)
                   , ("ProductExp {List_Exp} "  , Clip_Exp $ ProductExp NoIDP NoIDP [] hole)
                   ,("{Exp}", Clip_Exp hole)
                   ]

  arity (PlusExp _ x0 x1) = 2
  arity (TimesExp _ x0 x1) = 2
  arity (DivExp _ x0 x1) = 2
  arity (PowerExp _ x0 x1) = 2
  arity (BoolExp _ x0) = 1
  arity (IntExp _ x0) = 1
  arity (LamExp _ _ x0 x1) = 2
  arity (AppExp x0 x1) = 2
  arity (CaseExp _ _ x0 x1) = 2
  arity (LetExp _ _ x0 x1) = 2
  arity (IdentExp x0) = 1
  arity (IfExp _ _ _ x0 x1 x2) = 3
  arity (ParenExp _ _ x0) = 1
  arity (ListExp _ _ _ x0) = 1
  arity (ProductExp _ _ _ x0) = 1
  arity _                        = 0

  parseErr = ParseErrExp

  hole = HoleExp

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Alt Document Node ClipDoc UserToken where
  select [] x = Clip_Alt x
  select (0:p) (Alt _ _ x0 x1) = select p x0
  select (1:p) (Alt _ _ x0 x1) = select p x1
  select _ _ = Clip_Nothing

  paste [] (Clip_Alt c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Alt") x
  paste (0:p) c (Alt i0 i1 x0 x1) = Alt i0 i1 (paste p c x0) x1
  paste (1:p) c (Alt i0 i1 x0 x1) = Alt i0 i1 x0 (paste p c x1)
  paste _ _ x = x

  alternatives _ = [ ("Alt {Ident} {Exp} "  , Clip_Alt $ Alt NoIDP NoIDP hole hole)
                   ,("{Alt}", Clip_Alt hole)
                   ]

  arity (Alt _ _ x0 x1) = 2
  arity _                        = 0

  parseErr = ParseErrAlt

  hole = HoleAlt

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Board Document Node ClipDoc UserToken where
  select [] x = Clip_Board x
  select (0:p) (Board x0 x1 x2 x3 x4 x5 x6 x7) = select p x0
  select (1:p) (Board x0 x1 x2 x3 x4 x5 x6 x7) = select p x1
  select (2:p) (Board x0 x1 x2 x3 x4 x5 x6 x7) = select p x2
  select (3:p) (Board x0 x1 x2 x3 x4 x5 x6 x7) = select p x3
  select (4:p) (Board x0 x1 x2 x3 x4 x5 x6 x7) = select p x4
  select (5:p) (Board x0 x1 x2 x3 x4 x5 x6 x7) = select p x5
  select (6:p) (Board x0 x1 x2 x3 x4 x5 x6 x7) = select p x6
  select (7:p) (Board x0 x1 x2 x3 x4 x5 x6 x7) = select p x7
  select _ _ = Clip_Nothing

  paste [] (Clip_Board c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Board") x
  paste (0:p) c (Board x0 x1 x2 x3 x4 x5 x6 x7) = Board (paste p c x0) x1 x2 x3 x4 x5 x6 x7
  paste (1:p) c (Board x0 x1 x2 x3 x4 x5 x6 x7) = Board x0 (paste p c x1) x2 x3 x4 x5 x6 x7
  paste (2:p) c (Board x0 x1 x2 x3 x4 x5 x6 x7) = Board x0 x1 (paste p c x2) x3 x4 x5 x6 x7
  paste (3:p) c (Board x0 x1 x2 x3 x4 x5 x6 x7) = Board x0 x1 x2 (paste p c x3) x4 x5 x6 x7
  paste (4:p) c (Board x0 x1 x2 x3 x4 x5 x6 x7) = Board x0 x1 x2 x3 (paste p c x4) x5 x6 x7
  paste (5:p) c (Board x0 x1 x2 x3 x4 x5 x6 x7) = Board x0 x1 x2 x3 x4 (paste p c x5) x6 x7
  paste (6:p) c (Board x0 x1 x2 x3 x4 x5 x6 x7) = Board x0 x1 x2 x3 x4 x5 (paste p c x6) x7
  paste (7:p) c (Board x0 x1 x2 x3 x4 x5 x6 x7) = Board x0 x1 x2 x3 x4 x5 x6 (paste p c x7)
  paste _ _ x = x

  alternatives _ = [ ("Board {BoardRow} {BoardRow} {BoardRow} {BoardRow} {BoardRow} {BoardRow} {BoardRow} {BoardRow} "  , Clip_Board $ Board hole hole hole hole hole hole hole hole)
                   ,("{Board}", Clip_Board hole)
                   ]

  arity (Board x0 x1 x2 x3 x4 x5 x6 x7) = 8
  arity _                        = 0

  parseErr = ParseErrBoard

  hole = HoleBoard

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable BoardRow Document Node ClipDoc UserToken where
  select [] x = Clip_BoardRow x
  select (0:p) (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = select p x0
  select (1:p) (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = select p x1
  select (2:p) (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = select p x2
  select (3:p) (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = select p x3
  select (4:p) (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = select p x4
  select (5:p) (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = select p x5
  select (6:p) (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = select p x6
  select (7:p) (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = select p x7
  select _ _ = Clip_Nothing

  paste [] (Clip_BoardRow c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on BoardRow") x
  paste (0:p) c (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = BoardRow (paste p c x0) x1 x2 x3 x4 x5 x6 x7
  paste (1:p) c (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = BoardRow x0 (paste p c x1) x2 x3 x4 x5 x6 x7
  paste (2:p) c (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = BoardRow x0 x1 (paste p c x2) x3 x4 x5 x6 x7
  paste (3:p) c (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = BoardRow x0 x1 x2 (paste p c x3) x4 x5 x6 x7
  paste (4:p) c (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = BoardRow x0 x1 x2 x3 (paste p c x4) x5 x6 x7
  paste (5:p) c (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = BoardRow x0 x1 x2 x3 x4 (paste p c x5) x6 x7
  paste (6:p) c (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = BoardRow x0 x1 x2 x3 x4 x5 (paste p c x6) x7
  paste (7:p) c (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = BoardRow x0 x1 x2 x3 x4 x5 x6 (paste p c x7)
  paste _ _ x = x

  alternatives _ = [ ("BoardRow {BoardSquare} {BoardSquare} {BoardSquare} {BoardSquare} {BoardSquare} {BoardSquare} {BoardSquare} {BoardSquare} "  , Clip_BoardRow $ BoardRow hole hole hole hole hole hole hole hole)
                   ,("{BoardRow}", Clip_BoardRow hole)
                   ]

  arity (BoardRow x0 x1 x2 x3 x4 x5 x6 x7) = 8
  arity _                        = 0

  parseErr = ParseErrBoardRow

  hole = HoleBoardRow

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable BoardSquare Document Node ClipDoc UserToken where
  select [] x = Clip_BoardSquare x
  select (0:p) (Queen x0) = select p x0
  select (0:p) (King x0) = select p x0
  select (0:p) (Bishop x0) = select p x0
  select (0:p) (Knight x0) = select p x0
  select (0:p) (Rook x0) = select p x0
  select (0:p) (Pawn x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_BoardSquare c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on BoardSquare") x
  paste (0:p) c (Queen x0) = Queen (paste p c x0)
  paste (0:p) c (King x0) = King (paste p c x0)
  paste (0:p) c (Bishop x0) = Bishop (paste p c x0)
  paste (0:p) c (Knight x0) = Knight (paste p c x0)
  paste (0:p) c (Rook x0) = Rook (paste p c x0)
  paste (0:p) c (Pawn x0) = Pawn (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("Queen {Bool} "  , Clip_BoardSquare $ Queen hole)
                   , ("King {Bool} "  , Clip_BoardSquare $ King hole)
                   , ("Bishop {Bool} "  , Clip_BoardSquare $ Bishop hole)
                   , ("Knight {Bool} "  , Clip_BoardSquare $ Knight hole)
                   , ("Rook {Bool} "  , Clip_BoardSquare $ Rook hole)
                   , ("Pawn {Bool} "  , Clip_BoardSquare $ Pawn hole)
                   , ("Empty "  , Clip_BoardSquare $ Empty)
                   ,("{BoardSquare}", Clip_BoardSquare hole)
                   ]

  arity (Queen x0) = 1
  arity (King x0) = 1
  arity (Bishop x0) = 1
  arity (Knight x0) = 1
  arity (Rook x0) = 1
  arity (Pawn x0) = 1
  arity (Empty) = 0
  arity _                        = 0

  parseErr = ParseErrBoardSquare

  hole = HoleBoardSquare

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable PPPresentation Document Node ClipDoc UserToken where
  select [] x = Clip_PPPresentation x
  select (0:p) (PPPresentation x0 x1) = select p x0
  select (1:p) (PPPresentation x0 x1) = select p x1
  select _ _ = Clip_Nothing

  paste [] (Clip_PPPresentation c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on PPPresentation") x
  paste (0:p) c (PPPresentation x0 x1) = PPPresentation (paste p c x0) x1
  paste (1:p) c (PPPresentation x0 x1) = PPPresentation x0 (paste p c x1)
  paste _ _ x = x

  alternatives _ = [ ("PPPresentation {Bool} {List_Slide} "  , Clip_PPPresentation $ PPPresentation hole hole)
                   ,("{PPPresentation}", Clip_PPPresentation hole)
                   ]

  arity (PPPresentation x0 x1) = 2
  arity _                        = 0

  parseErr = ParseErrPPPresentation

  hole = HolePPPresentation

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Slide Document Node ClipDoc UserToken where
  select [] x = Clip_Slide x
  select (0:p) (Slide x0 x1) = select p x0
  select (1:p) (Slide x0 x1) = select p x1
  select _ _ = Clip_Nothing

  paste [] (Clip_Slide c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Slide") x
  paste (0:p) c (Slide x0 x1) = Slide (paste p c x0) x1
  paste (1:p) c (Slide x0 x1) = Slide x0 (paste p c x1)
  paste _ _ x = x

  alternatives _ = [ ("Slide {String} {ItemList} "  , Clip_Slide $ Slide hole hole)
                   ,("{Slide}", Clip_Slide hole)
                   ]

  arity (Slide x0 x1) = 2
  arity _                        = 0

  parseErr = ParseErrSlide

  hole = HoleSlide

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable ItemList Document Node ClipDoc UserToken where
  select [] x = Clip_ItemList x
  select (0:p) (ItemList x0 x1) = select p x0
  select (1:p) (ItemList x0 x1) = select p x1
  select _ _ = Clip_Nothing

  paste [] (Clip_ItemList c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on ItemList") x
  paste (0:p) c (ItemList x0 x1) = ItemList (paste p c x0) x1
  paste (1:p) c (ItemList x0 x1) = ItemList x0 (paste p c x1)
  paste _ _ x = x

  alternatives _ = [ ("ItemList {ListType} {List_Item} "  , Clip_ItemList $ ItemList hole hole)
                   ,("{ItemList}", Clip_ItemList hole)
                   ]

  arity (ItemList x0 x1) = 2
  arity _                        = 0

  parseErr = ParseErrItemList

  hole = HoleItemList

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable ListType Document Node ClipDoc UserToken where
  select [] x = Clip_ListType x
  select _ _ = Clip_Nothing

  paste [] (Clip_ListType c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on ListType") x
  paste _ _ x = x

  alternatives _ = [ ("Bullet "  , Clip_ListType $ Bullet)
                   , ("Number "  , Clip_ListType $ Number)
                   , ("Alpha "  , Clip_ListType $ Alpha)
                   ,("{ListType}", Clip_ListType hole)
                   ]

  arity (Bullet) = 0
  arity (Number) = 0
  arity (Alpha) = 0
  arity _                        = 0

  parseErr = ParseErrListType

  hole = HoleListType

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Item Document Node ClipDoc UserToken where
  select [] x = Clip_Item x
  select (0:p) (StringItem x0) = select p x0
  select (0:p) (HeliumItem x0) = select p x0
  select (0:p) (ListItem x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Item c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Item") x
  paste (0:p) c (StringItem x0) = StringItem (paste p c x0)
  paste (0:p) c (HeliumItem x0) = HeliumItem (paste p c x0)
  paste (0:p) c (ListItem x0) = ListItem (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("StringItem {String} "  , Clip_Item $ StringItem hole)
                   , ("HeliumItem {Exp} "  , Clip_Item $ HeliumItem hole)
                   , ("ListItem {ItemList} "  , Clip_Item $ ListItem hole)
                   ,("{Item}", Clip_Item hole)
                   ]

  arity (StringItem x0) = 1
  arity (HeliumItem x0) = 1
  arity (ListItem x0) = 1
  arity _                        = 0

  parseErr = ParseErrItem

  hole = HoleItem

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable List_Decl Document Node ClipDoc UserToken where
  select [] x = Clip_List_Decl x
  select (n:p) (List_Decl cxs) =
    let xs = fromConsList_Decl cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Decl c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Decl")   x
  paste (n:p) c (List_Decl cxs) =
    let xs = fromConsList_Decl cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Decl (replaceList_Decl n x' cxs)
        else List_Decl cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Decl}", Clip_List_Decl hole)
                   ]

  arity (List_Decl x1) = length (fromConsList_Decl x1)
  arity _ = 0

  parseErr = ParseErrList_Decl

  hole = List_Decl Nil_Decl

  isList _ = True

  insertList n (Clip_Decl c) (List_Decl cxs) = Clip_List_Decl $ List_Decl (insertList_Decl n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Decl xs
  insertList _ c xs = Clip_List_Decl xs

  removeList n (List_Decl cxs) = Clip_List_Decl $ List_Decl (removeList_Decl n cxs)
  removeList _ xs = Clip_List_Decl $ xs

instance Editable List_Alt Document Node ClipDoc UserToken where
  select [] x = Clip_List_Alt x
  select (n:p) (List_Alt cxs) =
    let xs = fromConsList_Alt cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Alt c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Alt")   x
  paste (n:p) c (List_Alt cxs) =
    let xs = fromConsList_Alt cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Alt (replaceList_Alt n x' cxs)
        else List_Alt cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Alt}", Clip_List_Alt hole)
                   ]

  arity (List_Alt x1) = length (fromConsList_Alt x1)
  arity _ = 0

  parseErr = ParseErrList_Alt

  hole = List_Alt Nil_Alt

  isList _ = True

  insertList n (Clip_Alt c) (List_Alt cxs) = Clip_List_Alt $ List_Alt (insertList_Alt n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Alt xs
  insertList _ c xs = Clip_List_Alt xs

  removeList n (List_Alt cxs) = Clip_List_Alt $ List_Alt (removeList_Alt n cxs)
  removeList _ xs = Clip_List_Alt $ xs

instance Editable List_Exp Document Node ClipDoc UserToken where
  select [] x = Clip_List_Exp x
  select (n:p) (List_Exp cxs) =
    let xs = fromConsList_Exp cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Exp c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Exp")   x
  paste (n:p) c (List_Exp cxs) =
    let xs = fromConsList_Exp cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Exp (replaceList_Exp n x' cxs)
        else List_Exp cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Exp}", Clip_List_Exp hole)
                   ]

  arity (List_Exp x1) = length (fromConsList_Exp x1)
  arity _ = 0

  parseErr = ParseErrList_Exp

  hole = List_Exp Nil_Exp

  isList _ = True

  insertList n (Clip_Exp c) (List_Exp cxs) = Clip_List_Exp $ List_Exp (insertList_Exp n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Exp xs
  insertList _ c xs = Clip_List_Exp xs

  removeList n (List_Exp cxs) = Clip_List_Exp $ List_Exp (removeList_Exp n cxs)
  removeList _ xs = Clip_List_Exp $ xs

instance Editable List_Slide Document Node ClipDoc UserToken where
  select [] x = Clip_List_Slide x
  select (n:p) (List_Slide cxs) =
    let xs = fromConsList_Slide cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Slide c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Slide")   x
  paste (n:p) c (List_Slide cxs) =
    let xs = fromConsList_Slide cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Slide (replaceList_Slide n x' cxs)
        else List_Slide cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Slide}", Clip_List_Slide hole)
                   ]

  arity (List_Slide x1) = length (fromConsList_Slide x1)
  arity _ = 0

  parseErr = ParseErrList_Slide

  hole = List_Slide Nil_Slide

  isList _ = True

  insertList n (Clip_Slide c) (List_Slide cxs) = Clip_List_Slide $ List_Slide (insertList_Slide n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Slide xs
  insertList _ c xs = Clip_List_Slide xs

  removeList n (List_Slide cxs) = Clip_List_Slide $ List_Slide (removeList_Slide n cxs)
  removeList _ xs = Clip_List_Slide $ xs

instance Editable List_Item Document Node ClipDoc UserToken where
  select [] x = Clip_List_Item x
  select (n:p) (List_Item cxs) =
    let xs = fromConsList_Item cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Item c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Item")   x
  paste (n:p) c (List_Item cxs) =
    let xs = fromConsList_Item cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Item (replaceList_Item n x' cxs)
        else List_Item cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Item}", Clip_List_Item hole)
                   ]

  arity (List_Item x1) = length (fromConsList_Item x1)
  arity _ = 0

  parseErr = ParseErrList_Item

  hole = List_Item Nil_Item

  isList _ = True

  insertList n (Clip_Item c) (List_Item cxs) = Clip_List_Item $ List_Item (insertList_Item n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Item xs
  insertList _ c xs = Clip_List_Item xs

  removeList n (List_Item cxs) = Clip_List_Item $ List_Item (removeList_Item n cxs)
  removeList _ xs = Clip_List_Item $ xs




--------------------------------------------------------------------------
-- Editable instances for Document, EnrichedDoc and primitive types     --
--------------------------------------------------------------------------

instance Editable Document Document Node ClipDoc UserToken where
  -- paths start below RootDoc, so on traversing the RootDoc constructor p is not modified
  select p (RootDoc x) = select p x
  paste p c (RootDoc x) = RootDoc $ paste p c x
  hole = HoleDocument
  parseErr = ParseErrDocument
  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing
  
-- for EnrichedDoc, only the member parseErr is used from Editable
instance Editable EnrichedDoc Document Node ClipDoc UserToken where
  hole = HoleEnrichedDoc
  parseErr = ParseErrEnrichedDoc

instance Editable Int Document Node ClipDoc UserToken where
  select [] x = Clip_Int x
  select _  _ = Clip_Nothing
  paste [] (Clip_Int c) x = c
  paste [] c            x =  debug Err ("Type error: pasting "++show c++" on Int") x
  paste _  _             x = x
  
  alternatives _ = [ ("0", Clip_Int 0) ]
  
  arity _ = 0
  parseErr _ = 0

  hole = 0

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Float Document Node ClipDoc UserToken where
  select [] x = Clip_Float x
  select _  _ = Clip_Nothing
  paste [] (Clip_Float c) x = c
  paste [] c              x =  debug Err ("Type error: pasting "++show c++" on Float") x
  paste _  _              x = x
  
  alternatives _ = [ ("0.0", Clip_Float 0.0) ]
  
  arity _ = 0
  parseErr _ = 0

  hole = 0

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Bool Document Node ClipDoc UserToken where
  select [] x = Clip_Bool x                            
  select _  _ = Clip_Nothing                           
  paste [] (Clip_Bool c) x = c                         
  paste [] c             x =  debug Err ("Type error: pasting "++show c++" on Bool") x
  paste _  _             x = x
  alternatives _ = [ ("True", Clip_Bool True)        
                   , ("False", Clip_Bool False)      
                   ]    
  arity _ = 0                                          
  parseErr _ = False

  hole = False

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable String Document Node ClipDoc UserToken where
  select [] x = Clip_String x
  select _  _ = Clip_Nothing
  paste [] (Clip_String c) x = c
  paste [] c             x =  debug Err ("Type error: pasting "++show c++" on String") x
  paste _  _             x = x

  alternatives _ = [ ("string", Clip_String "string")
                   ] 
 
  arity _ = 0
  parseErr _ = "{ParseErr}"

  hole = "{String}"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


