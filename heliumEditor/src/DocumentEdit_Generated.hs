module DocumentEdit_Generated where

import Common.CommonTypes
import Evaluation.DocTypes
import DocTypes_Generated
import Evaluation.DocumentEdit
import Evaluation.DocUtils
import Presentation.PresTypes

import Debug.Trace


-- not entirely generated: hole is special for chessboard and pppresentation, because hole is initialized
-- This is not good. hole must be just a hole for cut operations.
-- an initialized value must be specified separately class member initialValue?


-- Constructor for HoleClip can be put in class as toClip or inject



-- paths start below RootDoc, so on traversing the RootDoc constructor p is not modified
instance Editable Document Document Node ClipDoc UserToken where
  select p (RootDoc x) = select p x
  paste p c (RootDoc x) = RootDoc $ paste p c x
  hole = HoleDocument
  parseErr = ParseErrDocument
  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing
  
instance Editable HeliumTypeInfo Document Node ClipDoc UserToken where
  hole =  ([],[],[])
  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


-- from Editable, only the member parseErr is used for EnrichedDoc
instance Editable EnrichedDoc Document Node ClipDoc UserToken where
--  hole = HoleEnrichedDoc
  parseErr = ParseErrEnrichedDoc

instance Editable Int Document Node ClipDoc UserToken where
  select [] x = Clip_Int x
  select _  _ = Clip_Nothing
  paste [] (Clip_Int c) x = c
  paste [] c            x =  trace ("Type error: pasting "++show c++" on Int") x
  paste _  _             x = x
  
  alternatives _ = [ ("0", Clip_Int 0)
                   , ("1", Clip_Int 1)
                   , ("{Int}", Clip_Int hole) ]
  
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
  paste [] c             x =  trace ("Type error: pasting "++show c++" on Bool") x
  paste _  _             x = x
  alternatives _ = [ ("True", Clip_Bool True)        
                   , ("False", Clip_Bool False)      
                   , ("{Bool}", Clip_Bool hole) ]    
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
  paste [] c             x =  trace ("Type error: pasting "++show c++" on String") x
  paste _  _             x = x

  alternatives _ = [ ("a", Clip_String "a")
                   , ("ab", Clip_String "ab")
                   , ("{String}", Clip_String hole) ] 
 
  arity _ = 0
  parseErr _ = "{ParseErr}"

  hole = "{String}"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


{-
-- editable instances for boxed values

instance Editable String_ where
  select []    x                  = Clip_String_ x
  select (0:p) (String_ _ x1) = select p x1
  select _     _                  = Clip_Nothing

  paste [] (Clip_String_ c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on String_")   x
  paste (0:p) c (String_ i1 x1) = String_ i1 (paste p c x1)
  paste _  _  x                    = x

  alternatives _ = [("String_ "  , Clip_String_ $ String_ NoIDD hole)
                   ,("{String_}", Clip_String_ hole)
                   ]

  arity (String_ _ x1) = 1
  arity _                        = 0

  parseErr = ParseErrString_

  hole = HoleString_



instance Editable Bool_ where
  select []    x                  = Clip_Bool_ x
  select (0:p) (Bool_ _ x1) = select p x1
  select _     _                  = Clip_Nothing

  paste [] (Clip_Bool_ c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Bool_")   x
  paste (0:p) c (Bool_ i1 x1) = Bool_ i1 (paste p c x1)
  paste _  _  x                    = x

  alternatives _ = [("Bool_ "  , Clip_Bool_ $ Bool_ NoIDD hole)
                   ,("{Bool_}", Clip_Bool_ hole)
                   ]

  arity (Bool_ _ x1) = 1
  arity _                        = 0

  parseErr = ParseErrBool_

  hole = HoleBool_



instance Editable Int_ where
  select []    x                  = Clip_Int_ x
  select _     _                  = Clip_Nothing

  paste [] (Clip_Int_ c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Int_")   x
  paste (0:p) c (Int_ i1 x1) = Int_ i1 (paste p c x1)
  paste _  _  x                    = x

  alternatives _ = [("Int_ "  , Clip_Int_ $ Int_ NoIDD hole)
                   ,("{Int_}", Clip_Int_ hole)
                   ]

  arity _                        = 0

  parseErr = ParseErrInt_

  hole = HoleInt_

-}

----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

{- ------------------------------------

 generated part

-------------------------------------- -}


-- Generated clipfunctions  --

instance Clip ClipDoc where
  arityClip (Clip_Root x) = arity x
  arityClip (Clip_Dummy x) = arity x
  arityClip (Clip_RootE x) = arity x
  arityClip (Clip_HeliumTypeInfo x) = arity x
  arityClip (Clip_Document x) = arity x
  arityClip (Clip_List_Decl x) = arity x
  arityClip (Clip_Bool x) = arity x
  arityClip (Clip_Ident x) = arity x
  arityClip (Clip_Exp x) = arity x
  arityClip (Clip_Board x) = arity x
  arityClip (Clip_PPPresentation x) = arity x
  arityClip (Clip_String x) = arity x
  arityClip (Clip_Int x) = arity x
  arityClip (Clip_List_Alt x) = arity x
  arityClip (Clip_List_Exp x) = arity x
  arityClip (Clip_BoardRow x) = arity x
  arityClip (Clip_BoardSquare x) = arity x
  arityClip (Clip_List_Slide x) = arity x
  arityClip (Clip_ItemList x) = arity x
  arityClip (Clip_ListType x) = arity x
  arityClip (Clip_List_Item x) = arity x
  arityClip (Clip_Decl x) = arity x
  arityClip (Clip_Alt x) = arity x
  arityClip (Clip_Slide x) = arity x
  arityClip (Clip_Item x) = arity x
  arityClip (Clip_Nothing)   = -1
  alternativesClip (Clip_Root x) = alternatives x
  alternativesClip (Clip_Dummy x) = alternatives x
  alternativesClip (Clip_RootE x) = alternatives x
  alternativesClip (Clip_HeliumTypeInfo x) = alternatives x
  alternativesClip (Clip_Document x) = alternatives x
  alternativesClip (Clip_List_Decl x) = alternatives x
  alternativesClip (Clip_Bool x) = alternatives x
  alternativesClip (Clip_Ident x) = alternatives x
  alternativesClip (Clip_Exp x) = alternatives x
  alternativesClip (Clip_Board x) = alternatives x
  alternativesClip (Clip_PPPresentation x) = alternatives x
  alternativesClip (Clip_String x) = alternatives x
  alternativesClip (Clip_Int x) = alternatives x
  alternativesClip (Clip_List_Alt x) = alternatives x
  alternativesClip (Clip_List_Exp x) = alternatives x
  alternativesClip (Clip_BoardRow x) = alternatives x
  alternativesClip (Clip_BoardSquare x) = alternatives x
  alternativesClip (Clip_List_Slide x) = alternatives x
  alternativesClip (Clip_ItemList x) = alternatives x
  alternativesClip (Clip_ListType x) = alternatives x
  alternativesClip (Clip_List_Item x) = alternatives x
  alternativesClip (Clip_Decl x) = alternatives x
  alternativesClip (Clip_Alt x) = alternatives x
  alternativesClip (Clip_Slide x) = alternatives x
  alternativesClip (Clip_Item x) = alternatives x
  alternativesClip (Clip_Nothing)   = []

  holeClip (Clip_Root x) = Clip_Root hole
  holeClip (Clip_Dummy x) = Clip_Dummy hole
  holeClip (Clip_RootE x) = Clip_RootE hole
  holeClip (Clip_HeliumTypeInfo x) = Clip_HeliumTypeInfo hole
  holeClip (Clip_Document x) = Clip_Document hole
  holeClip (Clip_List_Decl x) = Clip_List_Decl hole
  holeClip (Clip_Bool x) = Clip_Bool hole
  holeClip (Clip_Ident x) = Clip_Ident hole
  holeClip (Clip_Exp x) = Clip_Exp hole
  holeClip (Clip_Board x) = Clip_Board hole
  holeClip (Clip_PPPresentation x) = Clip_PPPresentation hole
  holeClip (Clip_String x) = Clip_String hole
  holeClip (Clip_Int x) = Clip_Int hole
  holeClip (Clip_List_Alt x) = Clip_List_Alt hole
  holeClip (Clip_List_Exp x) = Clip_List_Exp hole
  holeClip (Clip_BoardRow x) = Clip_BoardRow hole
  holeClip (Clip_BoardSquare x) = Clip_BoardSquare hole
  holeClip (Clip_List_Slide x) = Clip_List_Slide hole
  holeClip (Clip_ItemList x) = Clip_ItemList hole
  holeClip (Clip_ListType x) = Clip_ListType hole
  holeClip (Clip_List_Item x) = Clip_List_Item hole
  holeClip (Clip_Decl x) = Clip_Decl hole
  holeClip (Clip_Alt x) = Clip_Alt hole
  holeClip (Clip_Slide x) = Clip_Slide hole
  holeClip (Clip_Item x) = Clip_Item hole
  holeClip Clip_Nothing   = Clip_Nothing

  isListClip (Clip_Root x) = isList x
  isListClip (Clip_Dummy x) = isList x
  isListClip (Clip_RootE x) = isList x
  isListClip (Clip_HeliumTypeInfo x) = isList x
  isListClip (Clip_Document x) = isList x
  isListClip (Clip_List_Decl x) = isList x
  isListClip (Clip_Bool x) = isList x
  isListClip (Clip_Ident x) = isList x
  isListClip (Clip_Exp x) = isList x
  isListClip (Clip_Board x) = isList x
  isListClip (Clip_PPPresentation x) = isList x
  isListClip (Clip_String x) = isList x
  isListClip (Clip_Int x) = isList x
  isListClip (Clip_List_Alt x) = isList x
  isListClip (Clip_List_Exp x) = isList x
  isListClip (Clip_BoardRow x) = isList x
  isListClip (Clip_BoardSquare x) = isList x
  isListClip (Clip_List_Slide x) = isList x
  isListClip (Clip_ItemList x) = isList x
  isListClip (Clip_ListType x) = isList x
  isListClip (Clip_List_Item x) = isList x
  isListClip (Clip_Decl x) = isList x
  isListClip (Clip_Alt x) = isList x
  isListClip (Clip_Slide x) = isList x
  isListClip (Clip_Item x) = isList x
  isListClip (Clip_Nothing)   = False

  insertListClip i c (Clip_Root x) = insertList i c x
  insertListClip i c (Clip_Dummy x) = insertList i c x
  insertListClip i c (Clip_RootE x) = insertList i c x
  insertListClip i c (Clip_HeliumTypeInfo x) = insertList i c x
  insertListClip i c (Clip_Document x) = insertList i c x
  insertListClip i c (Clip_List_Decl x) = insertList i c x
  insertListClip i c (Clip_Bool x) = insertList i c x
  insertListClip i c (Clip_Ident x) = insertList i c x
  insertListClip i c (Clip_Exp x) = insertList i c x
  insertListClip i c (Clip_Board x) = insertList i c x
  insertListClip i c (Clip_PPPresentation x) = insertList i c x
  insertListClip i c (Clip_String x) = insertList i c x
  insertListClip i c (Clip_Int x) = insertList i c x
  insertListClip i c (Clip_List_Alt x) = insertList i c x
  insertListClip i c (Clip_List_Exp x) = insertList i c x
  insertListClip i c (Clip_BoardRow x) = insertList i c x
  insertListClip i c (Clip_BoardSquare x) = insertList i c x
  insertListClip i c (Clip_List_Slide x) = insertList i c x
  insertListClip i c (Clip_ItemList x) = insertList i c x
  insertListClip i c (Clip_ListType x) = insertList i c x
  insertListClip i c (Clip_List_Item x) = insertList i c x
  insertListClip i c (Clip_Decl x) = insertList i c x
  insertListClip i c (Clip_Alt x) = insertList i c x
  insertListClip i c (Clip_Slide x) = insertList i c x
  insertListClip i c (Clip_Item x) = insertList i c x
  insertListClip i c (Clip_Nothing)   = Clip_Nothing

  removeListClip i (Clip_Root x) = removeList i x
  removeListClip i (Clip_Dummy x) = removeList i x
  removeListClip i (Clip_RootE x) = removeList i x
  removeListClip i (Clip_HeliumTypeInfo x) = removeList i x
  removeListClip i (Clip_Document x) = removeList i x
  removeListClip i (Clip_List_Decl x) = removeList i x
  removeListClip i (Clip_Bool x) = removeList i x
  removeListClip i (Clip_Ident x) = removeList i x
  removeListClip i (Clip_Exp x) = removeList i x
  removeListClip i (Clip_Board x) = removeList i x
  removeListClip i (Clip_PPPresentation x) = removeList i x
  removeListClip i (Clip_String x) = removeList i x
  removeListClip i (Clip_Int x) = removeList i x
  removeListClip i (Clip_List_Alt x) = removeList i x
  removeListClip i (Clip_List_Exp x) = removeList i x
  removeListClip i (Clip_BoardRow x) = removeList i x
  removeListClip i (Clip_BoardSquare x) = removeList i x
  removeListClip i (Clip_List_Slide x) = removeList i x
  removeListClip i (Clip_ItemList x) = removeList i x
  removeListClip i (Clip_ListType x) = removeList i x
  removeListClip i (Clip_List_Item x) = removeList i x
  removeListClip i (Clip_Decl x) = removeList i x
  removeListClip i (Clip_Alt x) = removeList i x
  removeListClip i (Clip_Slide x) = removeList i x
  removeListClip i (Clip_Item x) = removeList i x
  removeListClip i (Clip_Nothing)   = Clip_Nothing


-- Editable Instances --



instance Editable Dummy Document Node ClipDoc UserToken where
  select []    x                  = Clip_Dummy x
  select (0:p) (Dummy x1 x2) = select p x1
  select (1:p) (Dummy x1 x2) = select p x2
  select _     _                  = Clip_Nothing

  paste [] (Clip_Dummy c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Dummy")   x
  paste (0:p) c (Dummy x1 x2) = Dummy (paste p c x1) x2
  paste (1:p) c (Dummy x1 x2) = Dummy x1 (paste p c x2)
  paste _  _  x                    = x

  alternatives _ = [("Dummy {Root} {Dummy} "  , Clip_Dummy $ Dummy hole hole)
                   ,("{Dummy}", Clip_Dummy hole)
                   ]

  arity (Dummy x1 x2) = 2
  arity _                        = 0

  parseErr = ParseErrDummy

  hole = HoleDummy


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Root Document Node ClipDoc UserToken where
  select []    x                  = Clip_Root x
  select (0:p) (Root _ x1) = select p x1
  select _     _                  = Clip_Nothing

  paste [] (Clip_Root c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Root")   x
  paste (0:p) c (Root i1 x1) = Root i1 (paste p c x1)
  paste _  _  x                    = x

  alternatives _ = [("Root {Decls} "  , Clip_Root $ Root NoIDP hole)
                   ,("{Root}", Clip_Root hole)
                   ]

  arity (Root _ x1) = 1
  arity _                        = 0

  parseErr = ParseErrRoot

  hole = HoleRoot


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable RootE Document Node ClipDoc UserToken where
  select []    x                  = Clip_RootE x
  select (0:p) (RootE _ x1 x2) = select p x1
  select (1:p) (RootE _ x1 x2) = select p x2
  select _     _                  = Clip_Nothing

  paste [] (Clip_RootE c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on RootE")   x
  paste (0:p) c (RootE i1 x1 x2) = RootE i1 (paste p c x1) x2
  paste (1:p) c (RootE i1 x1 x2) = RootE i1 x1 (paste p c x2)
  paste _  _  x                    = x

  alternatives _ = [("RootE {Decls} {Decls} "  , Clip_RootE $ RootE NoIDP hole hole)
                   ,("{RootE}", Clip_RootE hole)
                   ]

  arity (RootE _ x1 x2) = 2
  arity _                        = 0

  parseErr = ParseErrRootE

  hole = HoleRootE


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Decl Document Node ClipDoc UserToken where
  select []    x                  = Clip_Decl x
  select (0:p) (Decl _ _ _ _ x1 x2 x3 x4) = select p x1
  select (1:p) (Decl _ _ _ _ x1 x2 x3 x4) = select p x2
  select (2:p) (Decl _ _ _ _ x1 x2 x3 x4) = select p x3
  select (3:p) (Decl _ _ _ _ x1 x2 x3 x4) = select p x4
  select (0:p) (BoardDecl _ _ x1) = select p x1
  select (0:p) (PPPresentationDecl _ _ x1) = select p x1
  select _     _                  = Clip_Nothing

  paste [] (Clip_Decl c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Decl")   x
  paste (0:p) c (Decl i1 i2 i3 i4 x1 x2 x3 x4) = Decl i1 i2 i3 i4 (paste p c x1) x2 x3 x4
  paste (1:p) c (Decl i1 i2 i3 i4 x1 x2 x3 x4) = Decl i1 i2 i3 i4 x1 (paste p c x2) x3 x4
  paste (2:p) c (Decl i1 i2 i3 i4 x1 x2 x3 x4) = Decl i1 i2 i3 i4 x1 x2 (paste p c x3) x4
  paste (3:p) c (Decl i1 i2 i3 i4 x1 x2 x3 x4) = Decl i1 i2 i3 i4 x1 x2 x3 (paste p c x4)
  paste (0:p) c (BoardDecl i1 i2 x1) = BoardDecl i1 i2 (paste p c x1)
  paste (0:p) c (PPPresentationDecl i1 i2 x1) = PPPresentationDecl i1 i2 (paste p c x1)
  paste _  _  x                    = x

  alternatives _ = [("Decl {Ident} {Exp} "  , Clip_Decl $ Decl NoIDP NoIDP NoIDP NoIDP hole hole hole hole)
                   ,("BoardDecl {Board} "  , Clip_Decl $ BoardDecl NoIDP NoIDP hole)
                   ,("PPPresentationDecl {PPPresentation} "  , Clip_Decl $ PPPresentationDecl NoIDP NoIDP hole)
                   ,("{Decl}", Clip_Decl hole)
                   ]

  arity (Decl _ _ _ _ x1 x2 x3 x4) = 4
  arity (BoardDecl _ _ x1) = 1
  arity (PPPresentationDecl _ _ x1) = 1
  arity _                        = 0

  parseErr = ParseErrDecl

  hole = HoleDecl


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Ident Document Node ClipDoc UserToken where
  select []    x                  = Clip_Ident x
  select (0:p) (Ident _ _ x1) = select p x1
  select _     _                  = Clip_Nothing

  paste [] (Clip_Ident c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Ident")   x
  paste (0:p) c (Ident i1 i2 x1) = Ident i1 i2 (paste p c x1)
  paste _  _  x                    = x

  alternatives _ = [("Ident "  , Clip_Ident $ Ident NoIDP NoIDP hole)
                   ,("{Ident}", Clip_Ident hole)
                   ]

  arity (Ident _ _ x1) = 1
  arity _                        = 0

  parseErr = ParseErrIdent

  hole = HoleIdent


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Exp Document Node ClipDoc UserToken where
  select []    x                  = Clip_Exp x
  select (0:p) (PlusExp _ x1 x2) = select p x1
  select (1:p) (PlusExp _ x1 x2) = select p x2
  select (0:p) (TimesExp _ x1 x2) = select p x1
  select (1:p) (TimesExp _ x1 x2) = select p x2
  select (0:p) (DivExp _ x1 x2) = select p x1
  select (1:p) (DivExp _ x1 x2) = select p x2
  select (0:p) (PowerExp _ x1 x2) = select p x1
  select (1:p) (PowerExp _ x1 x2) = select p x2
  select (0:p) (BoolExp _ x1) = select p x1
  select (0:p) (IntExp _ x1) = select p x1
  select (0:p) (LamExp _ _ x1 x2) = select p x1
  select (1:p) (LamExp _ _ x1 x2) = select p x2
  select (0:p) (AppExp x1 x2) = select p x1
  select (1:p) (AppExp x1 x2) = select p x2
  select (0:p) (CaseExp _ _ x1 x2) = select p x1
  select (1:p) (CaseExp _ _ x1 x2) = select p x2
  select (0:p) (LetExp _ _ x1 x2) = select p x1
  select (1:p) (LetExp _ _ x1 x2) = select p x2
  select (0:p) (IdentExp x1) = select p x1
  select (0:p) (IfExp _ _ _ x1 x2 x3) = select p x1
  select (1:p) (IfExp _ _ _ x1 x2 x3) = select p x2
  select (2:p) (IfExp _ _ _ x1 x2 x3) = select p x3
  select (0:p) (ParenExp _ _ x1) = select p x1
  select (0:p) (ListExp _ _ _ x1) = select p x1
  select (0:p) (ProductExp _ _ _ x1) = select p x1
  select _     _                  = Clip_Nothing

  paste [] (Clip_Exp c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Exp")   x
  paste (0:p) c (PlusExp i1 x1 x2) = PlusExp i1 (paste p c x1) x2
  paste (1:p) c (PlusExp i1 x1 x2) = PlusExp i1 x1 (paste p c x2)
  paste (0:p) c (TimesExp i1 x1 x2) = TimesExp i1 (paste p c x1) x2
  paste (1:p) c (TimesExp i1 x1 x2) = TimesExp i1 x1 (paste p c x2)
  paste (0:p) c (DivExp i1 x1 x2) = DivExp i1 (paste p c x1) x2
  paste (1:p) c (DivExp i1 x1 x2) = DivExp i1 x1 (paste p c x2)
  paste (0:p) c (PowerExp i1 x1 x2) = PowerExp i1 (paste p c x1) x2
  paste (1:p) c (PowerExp i1 x1 x2) = PowerExp i1 x1 (paste p c x2)
  paste (0:p) c (BoolExp i1 x1) = BoolExp i1 (paste p c x1)
  paste (0:p) c (IntExp i1 x1) = IntExp i1 (paste p c x1)
  paste (0:p) c (LamExp i1 i2 x1 x2) = LamExp i1 i2 (paste p c x1) x2
  paste (1:p) c (LamExp i1 i2 x1 x2) = LamExp i1 i2 x1 (paste p c x2)
  paste (0:p) c (AppExp x1 x2) = AppExp (paste p c x1) x2
  paste (1:p) c (AppExp x1 x2) = AppExp x1 (paste p c x2)
  paste (0:p) c (CaseExp i1 i2 x1 x2) = CaseExp i1 i2 (paste p c x1) x2
  paste (1:p) c (CaseExp i1 i2 x1 x2) = CaseExp i1 i2 x1 (paste p c x2)
  paste (0:p) c (LetExp i1 i2 x1 x2) = LetExp i1 i2 (paste p c x1) x2
  paste (1:p) c (LetExp i1 i2 x1 x2) = LetExp i1 i2 x1 (paste p c x2)
  paste (0:p) c (IdentExp x1) = IdentExp (paste p c x1)
  paste (0:p) c (IfExp i1 i2 i3 x1 x2 x3) = IfExp i1 i2 i3 (paste p c x1) x2 x3
  paste (1:p) c (IfExp i1 i2 i3 x1 x2 x3) = IfExp i1 i2 i3 x1 (paste p c x2) x3
  paste (2:p) c (IfExp i1 i2 i3 x1 x2 x3) = IfExp i1 i2 i3 x1 x2 (paste p c x3)
  paste (0:p) c (ParenExp i1 i2 x1) = ParenExp i1 i2 (paste p c x1)
  paste (0:p) c (ListExp i1 i2 i3 x1) = ListExp i1 i2 i3 (paste p c x1)
  paste (0:p) c (ProductExp i1 i2 i3 x1) = ProductExp i1 i2 i3 (paste p c x1)
  paste _  _  x                    = x

  alternatives _ = [("PlusExp {Exp} {Exp} "  , Clip_Exp $ PlusExp NoIDP hole hole)
                   ,("TimesExp {Exp} {Exp} "  , Clip_Exp $ TimesExp NoIDP hole hole)
                   ,("DivExp {Exp} {Exp} "  , Clip_Exp $ DivExp NoIDP hole hole)
                   ,("PowerExp {Exp} {Exp} "  , Clip_Exp $ PowerExp NoIDP hole hole)
                   ,("BoolExp "  , Clip_Exp $ BoolExp NoIDP hole)
                   ,("IntExp "  , Clip_Exp $ IntExp NoIDP hole)
                   ,("LamExp {Ident} {Exp} "  , Clip_Exp $ LamExp NoIDP NoIDP hole hole)
                   ,("AppExp {Exp} {Exp} "  , Clip_Exp $ AppExp hole hole)
                   ,("CaseExp {Exp} {Alts} "  , Clip_Exp $ CaseExp NoIDP NoIDP hole hole)
                   ,("LetExp {Decls} {Exp} "  , Clip_Exp $ LetExp NoIDP NoIDP hole hole)
                   ,("IdentExp {Ident} "  , Clip_Exp $ IdentExp hole)
                   ,("IfExp {Exp} {Exp} {Exp} "  , Clip_Exp $ IfExp NoIDP NoIDP NoIDP hole hole hole)
                   ,("ParenExp {Exp} "  , Clip_Exp $ ParenExp NoIDP NoIDP hole)
                   ,("ListExp {Exps} "  , Clip_Exp $ ListExp NoIDP NoIDP [] hole)
                   ,("ProductExp {Exps} "  , Clip_Exp $ ProductExp NoIDP NoIDP [] hole)
                   ,("{Exp}", Clip_Exp hole)
                   ]

  arity (PlusExp _ x1 x2) = 2
  arity (TimesExp _ x1 x2) = 2
  arity (DivExp _ x1 x2) = 2
  arity (PowerExp _ x1 x2) = 2
  arity (BoolExp _ x1) = 1
  arity (IntExp _ x1) = 1
  arity (LamExp _ _ x1 x2) = 2
  arity (AppExp x1 x2) = 2
  arity (CaseExp _ _ x1 x2) = 2
  arity (LetExp _ _ x1 x2) = 2
  arity (IdentExp x1) = 1
  arity (IfExp _ _ _ x1 x2 x3) = 3
  arity (ParenExp _ _ x1) = 1
  arity (ListExp _ _ _ x1) = 1
  arity (ProductExp _ _ _ x1) = 1
  arity _                        = 0

  parseErr = ParseErrExp

  hole = HoleExp


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Alt Document Node ClipDoc UserToken where
  select []    x                  = Clip_Alt x
  select (0:p) (Alt _ _ x1 x2) = select p x1
  select (1:p) (Alt _ _ x1 x2) = select p x2
  select _     _                  = Clip_Nothing

  paste [] (Clip_Alt c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Alt")   x
  paste (0:p) c (Alt i1 i2 x1 x2) = Alt i1 i2 (paste p c x1) x2
  paste (1:p) c (Alt i1 i2 x1 x2) = Alt i1 i2 x1 (paste p c x2)
  paste _  _  x                    = x

  alternatives _ = [("Alt {Ident} {Exp} "  , Clip_Alt $ Alt NoIDP NoIDP hole hole)
                   ,("{Alt}", Clip_Alt hole)
                   ]

  arity (Alt _ _ x1 x2) = 2
  arity _                        = 0

  parseErr = ParseErrAlt

  hole = HoleAlt


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Board Document Node ClipDoc UserToken where
  select []    x                  = Clip_Board x
  select (0:p) (Board x1 x2 x3 x4 x5 x6 x7 x8) = select p x1
  select (1:p) (Board x1 x2 x3 x4 x5 x6 x7 x8) = select p x2
  select (2:p) (Board x1 x2 x3 x4 x5 x6 x7 x8) = select p x3
  select (3:p) (Board x1 x2 x3 x4 x5 x6 x7 x8) = select p x4
  select (4:p) (Board x1 x2 x3 x4 x5 x6 x7 x8) = select p x5
  select (5:p) (Board x1 x2 x3 x4 x5 x6 x7 x8) = select p x6
  select (6:p) (Board x1 x2 x3 x4 x5 x6 x7 x8) = select p x7
  select (7:p) (Board x1 x2 x3 x4 x5 x6 x7 x8) = select p x8
  select _     _                  = Clip_Nothing

  paste [] (Clip_Board c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Board")   x
  paste (0:p) c (Board x1 x2 x3 x4 x5 x6 x7 x8) = Board (paste p c x1) x2 x3 x4 x5 x6 x7 x8
  paste (1:p) c (Board x1 x2 x3 x4 x5 x6 x7 x8) = Board x1 (paste p c x2) x3 x4 x5 x6 x7 x8
  paste (2:p) c (Board x1 x2 x3 x4 x5 x6 x7 x8) = Board x1 x2 (paste p c x3) x4 x5 x6 x7 x8
  paste (3:p) c (Board x1 x2 x3 x4 x5 x6 x7 x8) = Board x1 x2 x3 (paste p c x4) x5 x6 x7 x8
  paste (4:p) c (Board x1 x2 x3 x4 x5 x6 x7 x8) = Board x1 x2 x3 x4 (paste p c x5) x6 x7 x8
  paste (5:p) c (Board x1 x2 x3 x4 x5 x6 x7 x8) = Board x1 x2 x3 x4 x5 (paste p c x6) x7 x8
  paste (6:p) c (Board x1 x2 x3 x4 x5 x6 x7 x8) = Board x1 x2 x3 x4 x5 x6 (paste p c x7) x8
  paste (7:p) c (Board x1 x2 x3 x4 x5 x6 x7 x8) = Board x1 x2 x3 x4 x5 x6 x7 (paste p c x8)
  paste _  _  x                    = x

  alternatives _ = [("Board {BoardRow} {BoardRow} {BoardRow} {BoardRow} {BoardRow} {BoardRow} {BoardRow} {BoardRow} "  , Clip_Board $ Board hole hole hole hole hole hole hole hole)
                   ,("{Board}", Clip_Board hole)
                   ]

  arity (Board x1 x2 x3 x4 x5 x6 x7 x8) = 8
  arity _                        = 0

  parseErr = ParseErrBoard

  hole = HoleBoard


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable BoardRow Document Node ClipDoc UserToken where
  select []    x                  = Clip_BoardRow x
  select (0:p) (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = select p x1
  select (1:p) (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = select p x2
  select (2:p) (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = select p x3
  select (3:p) (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = select p x4
  select (4:p) (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = select p x5
  select (5:p) (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = select p x6
  select (6:p) (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = select p x7
  select (7:p) (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = select p x8
  select _     _                  = Clip_Nothing

  paste [] (Clip_BoardRow c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on BoardRow")   x
  paste (0:p) c (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = BoardRow (paste p c x1) x2 x3 x4 x5 x6 x7 x8
  paste (1:p) c (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = BoardRow x1 (paste p c x2) x3 x4 x5 x6 x7 x8
  paste (2:p) c (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = BoardRow x1 x2 (paste p c x3) x4 x5 x6 x7 x8
  paste (3:p) c (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = BoardRow x1 x2 x3 (paste p c x4) x5 x6 x7 x8
  paste (4:p) c (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = BoardRow x1 x2 x3 x4 (paste p c x5) x6 x7 x8
  paste (5:p) c (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = BoardRow x1 x2 x3 x4 x5 (paste p c x6) x7 x8
  paste (6:p) c (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = BoardRow x1 x2 x3 x4 x5 x6 (paste p c x7) x8
  paste (7:p) c (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = BoardRow x1 x2 x3 x4 x5 x6 x7 (paste p c x8)
  paste _  _  x                    = x

  alternatives _ = [("BoardRow {BoardSquare} {BoardSquare} {BoardSquare} {BoardSquare} {BoardSquare} {BoardSquare} {BoardSquare} {BoardSquare} "  , Clip_BoardRow $ BoardRow hole hole hole hole hole hole hole hole)
                   ,("{BoardRow}", Clip_BoardRow hole)
                   ]

  arity (BoardRow x1 x2 x3 x4 x5 x6 x7 x8) = 8
  arity _                        = 0

  parseErr = ParseErrBoardRow

  hole = HoleBoardRow


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable BoardSquare Document Node ClipDoc UserToken where
  select []    x                  = Clip_BoardSquare x
  select (0:p) (Queen x1) = select p x1
  select (0:p) (King x1) = select p x1
  select (0:p) (Bishop x1) = select p x1
  select (0:p) (Knight x1) = select p x1
  select (0:p) (Rook x1) = select p x1
  select (0:p) (Pawn x1) = select p x1
  select _     _                  = Clip_Nothing

  paste [] (Clip_BoardSquare c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on BoardSquare")   x
  paste (0:p) c (Queen x1) = Queen (paste p c x1)
  paste (0:p) c (King x1) = King (paste p c x1)
  paste (0:p) c (Bishop x1) = Bishop (paste p c x1)
  paste (0:p) c (Knight x1) = Knight (paste p c x1)
  paste (0:p) c (Rook x1) = Rook (paste p c x1)
  paste (0:p) c (Pawn x1) = Pawn (paste p c x1)
  paste _  _  x                    = x

  alternatives _ = [("Queen "  , Clip_BoardSquare $ Queen hole)
                   ,("King "  , Clip_BoardSquare $ King hole)
                   ,("Bishop "  , Clip_BoardSquare $ Bishop hole)
                   ,("Knight "  , Clip_BoardSquare $ Knight hole)
                   ,("Rook "  , Clip_BoardSquare $ Rook hole)
                   ,("Pawn "  , Clip_BoardSquare $ Pawn hole)
                   ,("Empty "  , Clip_BoardSquare $ Empty)
                   ,("{BoardSquare}", Clip_BoardSquare hole)
                   ]

  arity (Queen x1) = 1
  arity (King x1) = 1
  arity (Bishop x1) = 1
  arity (Knight x1) = 1
  arity (Rook x1) = 1
  arity (Pawn x1) = 1
  arity (Empty) = 0
  arity _                        = 0

  parseErr = ParseErrBoardSquare

  hole = HoleBoardSquare


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable PPPresentation Document Node ClipDoc UserToken where
  select []    x                  = Clip_PPPresentation x
  select (0:p) (PPPresentation x1 x2) = select p x1
  select (1:p) (PPPresentation x1 x2) = select p x2
  select _     _                  = Clip_Nothing

  paste [] (Clip_PPPresentation c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on PPPresentation")   x
  paste (0:p) c (PPPresentation x1 x2) = PPPresentation (paste p c x1) x2
  paste (1:p) c (PPPresentation x1 x2) = PPPresentation x1 (paste p c x2)
  paste _  _  x                    = x

  alternatives _ = [("PPPresentation {Slides} "  , Clip_PPPresentation $ PPPresentation hole hole)
                   ,("{PPPresentation}", Clip_PPPresentation hole)
                   ]

  arity (PPPresentation x1 x2) = 2
  arity _                        = 0

  parseErr = ParseErrPPPresentation

  hole = HolePPPresentation


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Slide Document Node ClipDoc UserToken where
  select []    x                  = Clip_Slide x
  select (0:p) (Slide x1 x2) = select p x1
  select (1:p) (Slide x1 x2) = select p x2
  select _     _                  = Clip_Nothing

  paste [] (Clip_Slide c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Slide")   x
  paste (0:p) c (Slide x1 x2) = Slide (paste p c x1) x2
  paste (1:p) c (Slide x1 x2) = Slide x1 (paste p c x2)
  paste _  _  x                    = x

  alternatives _ = [("Slide {ItemList} "  , Clip_Slide $ Slide hole hole)
                   ,("{Slide}", Clip_Slide hole)
                   ]

  arity (Slide x1 x2) = 2
  arity _                        = 0

  parseErr = ParseErrSlide

  hole = HoleSlide


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable ItemList Document Node ClipDoc UserToken where
  select []    x                  = Clip_ItemList x
  select (0:p) (ItemList x1 x2) = select p x1
  select (1:p) (ItemList x1 x2) = select p x2
  select _     _                  = Clip_Nothing

  paste [] (Clip_ItemList c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on ItemList")   x
  paste (0:p) c (ItemList x1 x2) = ItemList (paste p c x1) x2
  paste (1:p) c (ItemList x1 x2) = ItemList x1 (paste p c x2)
  paste _  _  x                    = x

  alternatives _ = [("ItemList {ListType} {Items} "  , Clip_ItemList $ ItemList hole hole)
                   ,("{ItemList}", Clip_ItemList hole)
                   ]

  arity (ItemList x1 x2) = 2
  arity _                        = 0

  parseErr = ParseErrItemList

  hole = HoleItemList


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable ListType Document Node ClipDoc UserToken where
  select []    x                  = Clip_ListType x
  select _     _                  = Clip_Nothing

  paste [] (Clip_ListType c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on ListType")   x
  paste _  _  x                    = x

  alternatives _ = [("Bullet "  , Clip_ListType $ Bullet)
                   ,("Number "  , Clip_ListType $ Number)
                   ,("Alpha "  , Clip_ListType $ Alpha)
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
  select []    x                  = Clip_Item x
  select (0:p) (StringItem x1) = select p x1
  select (0:p) (HeliumItem x1) = select p x1
  select (0:p) (ListItem x1) = select p x1
  select _     _                  = Clip_Nothing

  paste [] (Clip_Item c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Item")   x
  paste (0:p) c (StringItem x1) = StringItem (paste p c x1)
  paste (0:p) c (HeliumItem x1) = HeliumItem (paste p c x1)
  paste (0:p) c (ListItem x1) = ListItem (paste p c x1)
  paste _  _  x                    = x

  alternatives _ = [("StringItem "  , Clip_Item $ StringItem hole)
                   ,("HeliumItem {Exp} "  , Clip_Item $ HeliumItem hole)
                   ,("ListItem {ItemList} "  , Clip_Item $ ListItem hole)
                   ,("{Item}", Clip_Item hole)
                   ]

  arity (StringItem x1) = 1
  arity (HeliumItem x1) = 1
  arity (ListItem x1) = 1
  arity _                        = 0

  parseErr = ParseErrItem

  hole = HoleItem


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing
toList_Decl vs = List_Decl (toConsList_Decl vs)

fromList_Decl (List_Decl vs) = fromConsList_Decl vs
fromList_Decl _                  = []

toConsList_Decl [] = Nil_Decl
toConsList_Decl (x:xs) = Cons_Decl x (toConsList_Decl xs)

fromConsList_Decl Nil_Decl = []
fromConsList_Decl (Cons_Decl x xs) = x: fromConsList_Decl xs

replaceList_Decl _ x Nil_Decl = Nil_Decl -- replace beyond end of list
replaceList_Decl 0 x (Cons_Decl cx cxs) = Cons_Decl x cxs
replaceList_Decl n x (Cons_Decl cx cxs) = Cons_Decl cx (replaceList_Decl (n-1) x cxs)

insertList_Decl 0 x cxs = Cons_Decl x cxs
insertList_Decl _ x Nil_Decl  = Nil_Decl   -- insert beyond end of list
insertList_Decl n x (Cons_Decl cx cxs) = Cons_Decl cx (insertList_Decl (n-1) x cxs)

removeList_Decl _ Nil_Decl  = Nil_Decl -- remove beyond end of list
removeList_Decl 0 (Cons_Decl cx cxs) = cxs
removeList_Decl n (Cons_Decl cx cxs) = Cons_Decl cx (removeList_Decl (n-1) cxs)

instance Editable List_Decl Document Node ClipDoc UserToken where
  select []    x                  = Clip_List_Decl x
  select (n:p) (List_Decl cxs) = let xs = fromConsList_Decl cxs
                                  in  if n < length xs 
                                      then select p (xs !! n)
                                      else Clip_Nothing
  select _     _                  = Clip_Nothing

  paste [] (Clip_List_Decl c) _   = c
  paste [] c  x                  = trace ("Type error: pasting "++show c++" on List_Decl")   x
  paste (n:p) c (List_Decl cxs) = let xs = fromConsList_Decl cxs
                                    in  if n < length xs
                                        then let x  = xs!!n
                                                 x' = paste p c x
                                             in  List_Decl (replaceList_Decl n x' cxs)
                                        else List_Decl cxs -- paste beyond end of list
  paste _  _  x                  = x

  alternatives _ = [("{List_Decl}", Clip_List_Decl hole)
                   ]

  arity (List_Decl x1) = length (fromConsList_Decl x1)
  arity _                      = 0

  parseErr = ParseErrList_Decl

  hole = List_Decl Nil_Decl

  isList _ = True

  insertList n (Clip_Decl c) (List_Decl cxs) = Clip_List_Decl $ List_Decl (insertList_Decl n c cxs)
  insertList _ _             xs = trace "Type error, no paste" $ Clip_List_Decl xs
  insertList _ c xs                 = Clip_List_Decl xs

  removeList n (List_Decl cxs) = Clip_List_Decl $ List_Decl (removeList_Decl n cxs)
  removeList _ xs                        = Clip_List_Decl $ xs

toList_Alt vs = List_Alt (toConsList_Alt vs)

fromList_Alt (List_Alt vs) = fromConsList_Alt vs
fromList_Alt _                  = []

toConsList_Alt [] = Nil_Alt
toConsList_Alt (x:xs) = Cons_Alt x (toConsList_Alt xs)

fromConsList_Alt Nil_Alt = []
fromConsList_Alt (Cons_Alt x xs) = x: fromConsList_Alt xs

replaceList_Alt _ x Nil_Alt = Nil_Alt -- replace beyond end of list
replaceList_Alt 0 x (Cons_Alt cx cxs) = Cons_Alt x cxs
replaceList_Alt n x (Cons_Alt cx cxs) = Cons_Alt cx (replaceList_Alt (n-1) x cxs)

insertList_Alt 0 x cxs = Cons_Alt x cxs
insertList_Alt _ x Nil_Alt  = Nil_Alt   -- insert beyond end of list
insertList_Alt n x (Cons_Alt cx cxs) = Cons_Alt cx (insertList_Alt (n-1) x cxs)

removeList_Alt _ Nil_Alt  = Nil_Alt -- remove beyond end of list
removeList_Alt 0 (Cons_Alt cx cxs) = cxs
removeList_Alt n (Cons_Alt cx cxs) = Cons_Alt cx (removeList_Alt (n-1) cxs)

instance Editable List_Alt Document Node ClipDoc UserToken where
  select []    x                  = Clip_List_Alt x
  select (n:p) (List_Alt cxs) = let xs = fromConsList_Alt cxs
                                  in  if n < length xs 
                                      then select p (xs !! n)
                                      else Clip_Nothing
  select _     _                  = Clip_Nothing

  paste [] (Clip_List_Alt c) _   = c
  paste [] c  x                  = trace ("Type error: pasting "++show c++" on List_Alt")   x
  paste (n:p) c (List_Alt cxs) = let xs = fromConsList_Alt cxs
                                    in  if n < length xs
                                        then let x  = xs!!n
                                                 x' = paste p c x
                                             in  List_Alt (replaceList_Alt n x' cxs)
                                        else List_Alt cxs -- paste beyond end of list
  paste _  _  x                  = x

  alternatives _ = [("{List_Alt}", Clip_List_Alt hole)
                   ]

  arity (List_Alt x1) = length (fromConsList_Alt x1)
  arity _                      = 0

  parseErr = ParseErrList_Alt

  hole = List_Alt Nil_Alt

  isList _ = True

  insertList n (Clip_Alt c) (List_Alt cxs) = Clip_List_Alt $ List_Alt (insertList_Alt n c cxs)
  insertList _ _             xs = trace "Type error, no paste" $ Clip_List_Alt xs
  insertList _ c xs                 = Clip_List_Alt xs

  removeList n (List_Alt cxs) = Clip_List_Alt $ List_Alt (removeList_Alt n cxs)
  removeList _ xs                        = Clip_List_Alt $ xs

toList_Exp vs = List_Exp (toConsList_Exp vs)

fromList_Exp (List_Exp vs) = fromConsList_Exp vs
fromList_Exp _                  = []

toConsList_Exp [] = Nil_Exp
toConsList_Exp (x:xs) = Cons_Exp x (toConsList_Exp xs)

fromConsList_Exp Nil_Exp = []
fromConsList_Exp (Cons_Exp x xs) = x: fromConsList_Exp xs

replaceList_Exp _ x Nil_Exp = Nil_Exp -- replace beyond end of list
replaceList_Exp 0 x (Cons_Exp cx cxs) = Cons_Exp x cxs
replaceList_Exp n x (Cons_Exp cx cxs) = Cons_Exp cx (replaceList_Exp (n-1) x cxs)

insertList_Exp 0 x cxs = Cons_Exp x cxs
insertList_Exp _ x Nil_Exp  = Nil_Exp   -- insert beyond end of list
insertList_Exp n x (Cons_Exp cx cxs) = Cons_Exp cx (insertList_Exp (n-1) x cxs)

removeList_Exp _ Nil_Exp  = Nil_Exp -- remove beyond end of list
removeList_Exp 0 (Cons_Exp cx cxs) = cxs
removeList_Exp n (Cons_Exp cx cxs) = Cons_Exp cx (removeList_Exp (n-1) cxs)

instance Editable List_Exp Document Node ClipDoc UserToken where
  select []    x                  = Clip_List_Exp x
  select (n:p) (List_Exp cxs) = let xs = fromConsList_Exp cxs
                                  in  if n < length xs 
                                      then select p (xs !! n)
                                      else Clip_Nothing
  select _     _                  = Clip_Nothing

  paste [] (Clip_List_Exp c) _   = c
  paste [] c  x                  = trace ("Type error: pasting "++show c++" on List_Exp")   x
  paste (n:p) c (List_Exp cxs) = let xs = fromConsList_Exp cxs
                                    in  if n < length xs
                                        then let x  = xs!!n
                                                 x' = paste p c x
                                             in  List_Exp (replaceList_Exp n x' cxs)
                                        else List_Exp cxs -- paste beyond end of list
  paste _  _  x                  = x

  alternatives _ = [("{List_Exp}", Clip_List_Exp hole)
                   ]

  arity (List_Exp x1) = length (fromConsList_Exp x1)
  arity _                      = 0

  parseErr = ParseErrList_Exp

  hole = List_Exp Nil_Exp

  isList _ = True

  insertList n (Clip_Exp c) (List_Exp cxs) = Clip_List_Exp $ List_Exp (insertList_Exp n c cxs)
  insertList _ _             xs = trace "Type error, no paste" $ Clip_List_Exp xs
  insertList _ c xs                 = Clip_List_Exp xs

  removeList n (List_Exp cxs) = Clip_List_Exp $ List_Exp (removeList_Exp n cxs)
  removeList _ xs                        = Clip_List_Exp $ xs

toList_Slide vs = List_Slide (toConsList_Slide vs)

fromList_Slide (List_Slide vs) = fromConsList_Slide vs
fromList_Slide _                  = []

toConsList_Slide [] = Nil_Slide
toConsList_Slide (x:xs) = Cons_Slide x (toConsList_Slide xs)

fromConsList_Slide Nil_Slide = []
fromConsList_Slide (Cons_Slide x xs) = x: fromConsList_Slide xs

replaceList_Slide _ x Nil_Slide = Nil_Slide -- replace beyond end of list
replaceList_Slide 0 x (Cons_Slide cx cxs) = Cons_Slide x cxs
replaceList_Slide n x (Cons_Slide cx cxs) = Cons_Slide cx (replaceList_Slide (n-1) x cxs)

insertList_Slide 0 x cxs = Cons_Slide x cxs
insertList_Slide _ x Nil_Slide  = Nil_Slide   -- insert beyond end of list
insertList_Slide n x (Cons_Slide cx cxs) = Cons_Slide cx (insertList_Slide (n-1) x cxs)

removeList_Slide _ Nil_Slide  = Nil_Slide -- remove beyond end of list
removeList_Slide 0 (Cons_Slide cx cxs) = cxs
removeList_Slide n (Cons_Slide cx cxs) = Cons_Slide cx (removeList_Slide (n-1) cxs)

instance Editable List_Slide Document Node ClipDoc UserToken where
  select []    x                  = Clip_List_Slide x
  select (n:p) (List_Slide cxs) = let xs = fromConsList_Slide cxs
                                  in  if n < length xs 
                                      then select p (xs !! n)
                                      else Clip_Nothing
  select _     _                  = Clip_Nothing

  paste [] (Clip_List_Slide c) _   = c
  paste [] c  x                  = trace ("Type error: pasting "++show c++" on List_Slide")   x
  paste (n:p) c (List_Slide cxs) = let xs = fromConsList_Slide cxs
                                    in  if n < length xs
                                        then let x  = xs!!n
                                                 x' = paste p c x
                                             in  List_Slide (replaceList_Slide n x' cxs)
                                        else List_Slide cxs -- paste beyond end of list
  paste _  _  x                  = x

  alternatives _ = [("{List_Slide}", Clip_List_Slide hole)
                   ]

  arity (List_Slide x1) = length (fromConsList_Slide x1)
  arity _                      = 0

  parseErr = ParseErrList_Slide

  hole = List_Slide Nil_Slide

  isList _ = True

  insertList n (Clip_Slide c) (List_Slide cxs) = Clip_List_Slide $ List_Slide (insertList_Slide n c cxs)
  insertList _ _             xs = trace "Type error, no paste" $ Clip_List_Slide xs
  insertList _ c xs                 = Clip_List_Slide xs

  removeList n (List_Slide cxs) = Clip_List_Slide $ List_Slide (removeList_Slide n cxs)
  removeList _ xs                        = Clip_List_Slide $ xs

toList_Item vs = List_Item (toConsList_Item vs)

fromList_Item (List_Item vs) = fromConsList_Item vs
fromList_Item _                  = []

toConsList_Item [] = Nil_Item
toConsList_Item (x:xs) = Cons_Item x (toConsList_Item xs)

fromConsList_Item Nil_Item = []
fromConsList_Item (Cons_Item x xs) = x: fromConsList_Item xs

replaceList_Item _ x Nil_Item = Nil_Item -- replace beyond end of list
replaceList_Item 0 x (Cons_Item cx cxs) = Cons_Item x cxs
replaceList_Item n x (Cons_Item cx cxs) = Cons_Item cx (replaceList_Item (n-1) x cxs)

insertList_Item 0 x cxs = Cons_Item x cxs
insertList_Item _ x Nil_Item  = Nil_Item   -- insert beyond end of list
insertList_Item n x (Cons_Item cx cxs) = Cons_Item cx (insertList_Item (n-1) x cxs)

removeList_Item _ Nil_Item  = Nil_Item -- remove beyond end of list
removeList_Item 0 (Cons_Item cx cxs) = cxs
removeList_Item n (Cons_Item cx cxs) = Cons_Item cx (removeList_Item (n-1) cxs)

instance Editable List_Item Document Node ClipDoc UserToken where
  select []    x                  = Clip_List_Item x
  select (n:p) (List_Item cxs) = let xs = fromConsList_Item cxs
                                  in  if n < length xs 
                                      then select p (xs !! n)
                                      else Clip_Nothing
  select _     _                  = Clip_Nothing

  paste [] (Clip_List_Item c) _   = c
  paste [] c  x                  = trace ("Type error: pasting "++show c++" on List_Item")   x
  paste (n:p) c (List_Item cxs) = let xs = fromConsList_Item cxs
                                    in  if n < length xs
                                        then let x  = xs!!n
                                                 x' = paste p c x
                                             in  List_Item (replaceList_Item n x' cxs)
                                        else List_Item cxs -- paste beyond end of list
  paste _  _  x                  = x

  alternatives _ = [("{List_Item}", Clip_List_Item hole)
                   ]

  arity (List_Item x1) = length (fromConsList_Item x1)
  arity _                      = 0

  parseErr = ParseErrList_Item

  hole = List_Item Nil_Item

  isList _ = True

  insertList n (Clip_Item c) (List_Item cxs) = Clip_List_Item $ List_Item (insertList_Item n c cxs)
  insertList _ _             xs = trace "Type error, no paste" $ Clip_List_Item xs
  insertList _ c xs                 = Clip_List_Item xs

  removeList n (List_Item cxs) = Clip_List_Item $ List_Item (removeList_Item n cxs)
  removeList _ xs                        = Clip_List_Item $ xs

