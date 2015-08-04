{-# LANGUAGE FlexibleInstances #-}
module DocumentEdit_Generated where

import Common.CommonTypes hiding (Dirty (..))
import Evaluation.DocTypes
import DocTypes_Generated
import DocUtils_Generated
import Evaluation.DocumentEdit
import Evaluation.DocUtils
import Presentation.PresTypes

----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- Clip instance                                                        --
--------------------------------------------------------------------------

instance Clip ClipDoc where
  arityClip Clip_Nothing = -1
  arityClip (Clip_EnrichedDoc x) = arity x
  arityClip (Clip_Document x) = arity x
  arityClip (Clip_ChoiceDoc x) = arity x
  arityClip (Clip_Sudoku x) = arity x
  arityClip (Clip_Float_ x) = arity x
  arityClip (Clip_Fload_ x) = arity x
  arityClip (Clip_Bool x) = arity x
  arityClip (Clip_Int x) = arity x
  arityClip (Clip_String x) = arity x
  arityClip (Clip_Float x) = arity x

  alternativesClip Clip_Nothing = []
  alternativesClip (Clip_EnrichedDoc x) = alternatives x
  alternativesClip (Clip_Document x) = alternatives x
  alternativesClip (Clip_ChoiceDoc x) = alternatives x
  alternativesClip (Clip_Sudoku x) = alternatives x
  alternativesClip (Clip_Float_ x) = alternatives x
  alternativesClip (Clip_Fload_ x) = alternatives x
  alternativesClip (Clip_Bool x) = alternatives x
  alternativesClip (Clip_Int x) = alternatives x
  alternativesClip (Clip_String x) = alternatives x
  alternativesClip (Clip_Float x) = alternatives x

  holeClip Clip_Nothing = Clip_Nothing
  holeClip (Clip_EnrichedDoc x) = Clip_EnrichedDoc hole
  holeClip (Clip_Document x) = Clip_Document hole
  holeClip (Clip_ChoiceDoc x) = Clip_ChoiceDoc hole
  holeClip (Clip_Sudoku x) = Clip_Sudoku hole
  holeClip (Clip_Float_ x) = Clip_Float_ hole
  holeClip (Clip_Fload_ x) = Clip_Fload_ hole
  holeClip (Clip_Bool x) = Clip_Bool hole
  holeClip (Clip_Int x) = Clip_Int hole
  holeClip (Clip_String x) = Clip_String hole
  holeClip (Clip_Float x) = Clip_Float hole

  isListClip Clip_Nothing = False
  isListClip (Clip_EnrichedDoc x) = isList x
  isListClip (Clip_Document x) = isList x
  isListClip (Clip_ChoiceDoc x) = isList x
  isListClip (Clip_Sudoku x) = isList x
  isListClip (Clip_Float_ x) = isList x
  isListClip (Clip_Fload_ x) = isList x
  isListClip (Clip_Bool x) = isList x
  isListClip (Clip_Int x) = isList x
  isListClip (Clip_String x) = isList x
  isListClip (Clip_Float x) = isList x

  insertListClip i c Clip_Nothing = Clip_Nothing
  insertListClip i c (Clip_EnrichedDoc x) = insertList i c x
  insertListClip i c (Clip_Document x) = insertList i c x
  insertListClip i c (Clip_ChoiceDoc x) = insertList i c x
  insertListClip i c (Clip_Sudoku x) = insertList i c x
  insertListClip i c (Clip_Float_ x) = insertList i c x
  insertListClip i c (Clip_Fload_ x) = insertList i c x
  insertListClip i c (Clip_Bool x) = insertList i c x
  insertListClip i c (Clip_Int x) = insertList i c x
  insertListClip i c (Clip_String x) = insertList i c x
  insertListClip i c (Clip_Float x) = insertList i c x

  removeListClip i Clip_Nothing = Clip_Nothing
  removeListClip i (Clip_EnrichedDoc x) = removeList i x
  removeListClip i (Clip_Document x) = removeList i x
  removeListClip i (Clip_ChoiceDoc x) = removeList i x
  removeListClip i (Clip_Sudoku x) = removeList i x
  removeListClip i (Clip_Float_ x) = removeList i x
  removeListClip i (Clip_Fload_ x) = removeList i x
  removeListClip i (Clip_Bool x) = removeList i x
  removeListClip i (Clip_Int x) = removeList i x
  removeListClip i (Clip_String x) = removeList i x
  removeListClip i (Clip_Float x) = removeList i x




--------------------------------------------------------------------------
-- Editable instances                                                   --
--------------------------------------------------------------------------

instance Editable EnrichedDoc Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_EnrichedDoc x
  select (0:p) (RootEnr x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_EnrichedDoc c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on EnrichedDoc") x
  paste (0:p) c (RootEnr x0) = RootEnr (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("RootEnr {ChoiceDoc} "  , Clip_EnrichedDoc $ RootEnr hole)
                   ,("{EnrichedDoc}", Clip_EnrichedDoc hole)
                   ]

  arity (RootEnr x0) = 1
  arity _                        = 0

  toClip t = Clip_EnrichedDoc t

  fromClip (Clip_EnrichedDoc t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrEnrichedDoc

  hole = HoleEnrichedDoc

  holeNodeConstr = Node_HoleEnrichedDoc

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Document Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Document x
  select (0:p) (RootDoc x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Document c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Document") x
  paste (0:p) c (RootDoc x0) = RootDoc (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("RootDoc {ChoiceDoc} "  , Clip_Document $ RootDoc hole)
                   ,("{Document}", Clip_Document hole)
                   ]

  arity (RootDoc x0) = 1
  arity _                        = 0

  toClip t = Clip_Document t

  fromClip (Clip_Document t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrDocument

  hole = HoleDocument

  holeNodeConstr = Node_HoleDocument

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable ChoiceDoc Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_ChoiceDoc x
  select (0:p) (SudokuDoc x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_ChoiceDoc c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on ChoiceDoc") x
  paste (0:p) c (SudokuDoc x0) = SudokuDoc (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("SudokuDoc {Sudoku} "  , Clip_ChoiceDoc $ SudokuDoc hole)
                   ,("{ChoiceDoc}", Clip_ChoiceDoc hole)
                   ]

  arity (SudokuDoc x0) = 1
  arity _                        = 0

  toClip t = Clip_ChoiceDoc t

  fromClip (Clip_ChoiceDoc t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrChoiceDoc

  hole = HoleChoiceDoc

  holeNodeConstr = Node_HoleChoiceDoc

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Sudoku Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Sudoku x
  select (0:p) (Sudoku x0 x1) = select p x0
  select (1:p) (Sudoku x0 x1) = select p x1
  select _ _ = Clip_Nothing

  paste [] (Clip_Sudoku c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Sudoku") x
  paste (0:p) c (Sudoku x0 x1) = Sudoku (paste p c x0) x1
  paste (1:p) c (Sudoku x0 x1) = Sudoku x0 (paste p c x1)
  paste _ _ x = x

  alternatives _ = [ ("Sudoku {Float_} {Fload_} "  , Clip_Sudoku $ Sudoku hole hole)
                   ,("{Sudoku}", Clip_Sudoku hole)
                   ]

  arity (Sudoku x0 x1) = 2
  arity _                        = 0

  toClip t = Clip_Sudoku t

  fromClip (Clip_Sudoku t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrSudoku

  hole = HoleSudoku

  holeNodeConstr = Node_HoleSudoku

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Float_ Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Float_ x
  select (0:p) (Float_ x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Float_ c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Float_") x
  paste (0:p) c (Float_ x0) = Float_ (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("Float_ {Float} "  , Clip_Float_ $ Float_ hole)
                   ,("{Float_}", Clip_Float_ hole)
                   ]

  arity (Float_ x0) = 1
  arity _                        = 0

  toClip t = Clip_Float_ t

  fromClip (Clip_Float_ t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrFloat_

  hole = HoleFloat_

  holeNodeConstr = Node_HoleFloat_

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Fload_ Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Fload_ x
  select (0:p) (Fload_ x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Fload_ c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Fload_") x
  paste (0:p) c (Fload_ x0) = Fload_ (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("Fload_ {Int} "  , Clip_Fload_ $ Fload_ hole)
                   ,("{Fload_}", Clip_Fload_ hole)
                   ]

  arity (Fload_ x0) = 1
  arity _                        = 0

  toClip t = Clip_Fload_ t

  fromClip (Clip_Fload_ t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrFload_

  hole = HoleFload_

  holeNodeConstr = Node_HoleFload_

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing




--------------------------------------------------------------------------
-- Editable instances for Document, EnrichedDoc and primitive types     --
--------------------------------------------------------------------------

instance Editable Int Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Int x
  select _  _ = Clip_Nothing
  paste [] (Clip_Int c) x = c
  paste [] c            x =  debug Err ("Type error: pasting "++show c++" on Int") x
  paste _  _             x = x
  
  alternatives _ = [ ("0", Clip_Int 0) ]
  
  arity _ = 0

  toClip t = Clip_Int t

  fromClip (Clip_Int t) = Just t
  fromClip _            = Nothing

  parseErr _ = 0

  hole = 0

  holeNodeConstr = error "Type Int is primitive and has no hole node constructorstructor"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Float Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Float x
  select _  _ = Clip_Nothing
  paste [] (Clip_Float c) x = c
  paste [] c              x =  debug Err ("Type error: pasting "++show c++" on Float") x
  paste _  _              x = x
  
  alternatives _ = [ ("0.0", Clip_Float 0.0) ]
  
  arity _ = 0

  toClip t = Clip_Float t

  fromClip (Clip_Float t) = Just t
  fromClip _              = Nothing

  parseErr _ = 0

  hole = 0

  holeNodeConstr = error "Type Float is primitive and has no hole node constructorstructor"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Bool Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Bool x                            
  select _  _ = Clip_Nothing                           
  paste [] (Clip_Bool c) x = c                         
  paste [] c             x =  debug Err ("Type error: pasting "++show c++" on Bool") x
  paste _  _             x = x
  alternatives _ = [ ("True", Clip_Bool True)        
                   , ("False", Clip_Bool False)      
                   ]    
  arity _ = 0                                          

  toClip t = Clip_Bool t

  fromClip (Clip_Bool t) = Just t
  fromClip _             = Nothing

  parseErr _ = False

  hole = False

  holeNodeConstr = error "Type Bool is primitive and has no hole node constructorstructor"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable String Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_String x
  select _  _ = Clip_Nothing
  paste [] (Clip_String c) x = c
  paste [] c             x =  debug Err ("Type error: pasting "++show c++" on String") x
  paste _  _             x = x

  alternatives _ = [ ("string", Clip_String "string")
                   ] 
 
  arity _ = 0

  toClip t = Clip_String t

  fromClip (Clip_String t) = Just t
  fromClip _               = Nothing

  parseErr _ = "{ParseErr}"

  hole = "{String}"

  holeNodeConstr = error "Type String is primitive and has no hole node constructorstructor"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


