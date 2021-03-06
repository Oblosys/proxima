{-# LANGUAGE FlexibleInstances #-}
module DocumentEdit_Generated where

import Prelude hiding (Word)
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
  arityClip (Clip_Form x) = arity x
  arityClip (Clip_Expense x) = arity x
  arityClip (Clip_Currency x) = arity x
  arityClip (Clip_Tasks x) = arity x
  arityClip (Clip_Thing x) = arity x
  arityClip (Clip_Task x) = arity x
  arityClip (Clip_Description x) = arity x
  arityClip (Clip_Sudoku x) = arity x
  arityClip (Clip_Row x) = arity x
  arityClip (Clip_Field x) = arity x
  arityClip (Clip_Test x) = arity x
  arityClip (Clip_StyledText x) = arity x
  arityClip (Clip_Word x) = arity x
  arityClip (Clip_WordPart x) = arity x
  arityClip (Clip_TextStyle x) = arity x
  arityClip (Clip_Int_ x) = arity x
  arityClip (Clip_Float_ x) = arity x
  arityClip (Clip_List_Expense x) = arity x
  arityClip (Clip_List_Currency x) = arity x
  arityClip (Clip_List_Thing x) = arity x
  arityClip (Clip_List_Task x) = arity x
  arityClip (Clip_List_Word x) = arity x
  arityClip (Clip_List_WordPart x) = arity x
  arityClip (Clip_Bool x) = arity x
  arityClip (Clip_Int x) = arity x
  arityClip (Clip_String x) = arity x
  arityClip (Clip_Float x) = arity x

  alternativesClip Clip_Nothing = []
  alternativesClip (Clip_EnrichedDoc x) = alternatives x
  alternativesClip (Clip_Document x) = alternatives x
  alternativesClip (Clip_ChoiceDoc x) = alternatives x
  alternativesClip (Clip_Form x) = alternatives x
  alternativesClip (Clip_Expense x) = alternatives x
  alternativesClip (Clip_Currency x) = alternatives x
  alternativesClip (Clip_Tasks x) = alternatives x
  alternativesClip (Clip_Thing x) = alternatives x
  alternativesClip (Clip_Task x) = alternatives x
  alternativesClip (Clip_Description x) = alternatives x
  alternativesClip (Clip_Sudoku x) = alternatives x
  alternativesClip (Clip_Row x) = alternatives x
  alternativesClip (Clip_Field x) = alternatives x
  alternativesClip (Clip_Test x) = alternatives x
  alternativesClip (Clip_StyledText x) = alternatives x
  alternativesClip (Clip_Word x) = alternatives x
  alternativesClip (Clip_WordPart x) = alternatives x
  alternativesClip (Clip_TextStyle x) = alternatives x
  alternativesClip (Clip_Int_ x) = alternatives x
  alternativesClip (Clip_Float_ x) = alternatives x
  alternativesClip (Clip_List_Expense x) = alternatives x
  alternativesClip (Clip_List_Currency x) = alternatives x
  alternativesClip (Clip_List_Thing x) = alternatives x
  alternativesClip (Clip_List_Task x) = alternatives x
  alternativesClip (Clip_List_Word x) = alternatives x
  alternativesClip (Clip_List_WordPart x) = alternatives x
  alternativesClip (Clip_Bool x) = alternatives x
  alternativesClip (Clip_Int x) = alternatives x
  alternativesClip (Clip_String x) = alternatives x
  alternativesClip (Clip_Float x) = alternatives x

  holeClip Clip_Nothing = Clip_Nothing
  holeClip (Clip_EnrichedDoc x) = Clip_EnrichedDoc hole
  holeClip (Clip_Document x) = Clip_Document hole
  holeClip (Clip_ChoiceDoc x) = Clip_ChoiceDoc hole
  holeClip (Clip_Form x) = Clip_Form hole
  holeClip (Clip_Expense x) = Clip_Expense hole
  holeClip (Clip_Currency x) = Clip_Currency hole
  holeClip (Clip_Tasks x) = Clip_Tasks hole
  holeClip (Clip_Thing x) = Clip_Thing hole
  holeClip (Clip_Task x) = Clip_Task hole
  holeClip (Clip_Description x) = Clip_Description hole
  holeClip (Clip_Sudoku x) = Clip_Sudoku hole
  holeClip (Clip_Row x) = Clip_Row hole
  holeClip (Clip_Field x) = Clip_Field hole
  holeClip (Clip_Test x) = Clip_Test hole
  holeClip (Clip_StyledText x) = Clip_StyledText hole
  holeClip (Clip_Word x) = Clip_Word hole
  holeClip (Clip_WordPart x) = Clip_WordPart hole
  holeClip (Clip_TextStyle x) = Clip_TextStyle hole
  holeClip (Clip_Int_ x) = Clip_Int_ hole
  holeClip (Clip_Float_ x) = Clip_Float_ hole
  holeClip (Clip_List_Expense x) = Clip_List_Expense hole
  holeClip (Clip_List_Currency x) = Clip_List_Currency hole
  holeClip (Clip_List_Thing x) = Clip_List_Thing hole
  holeClip (Clip_List_Task x) = Clip_List_Task hole
  holeClip (Clip_List_Word x) = Clip_List_Word hole
  holeClip (Clip_List_WordPart x) = Clip_List_WordPart hole
  holeClip (Clip_Bool x) = Clip_Bool hole
  holeClip (Clip_Int x) = Clip_Int hole
  holeClip (Clip_String x) = Clip_String hole
  holeClip (Clip_Float x) = Clip_Float hole

  isListClip Clip_Nothing = False
  isListClip (Clip_EnrichedDoc x) = isList x
  isListClip (Clip_Document x) = isList x
  isListClip (Clip_ChoiceDoc x) = isList x
  isListClip (Clip_Form x) = isList x
  isListClip (Clip_Expense x) = isList x
  isListClip (Clip_Currency x) = isList x
  isListClip (Clip_Tasks x) = isList x
  isListClip (Clip_Thing x) = isList x
  isListClip (Clip_Task x) = isList x
  isListClip (Clip_Description x) = isList x
  isListClip (Clip_Sudoku x) = isList x
  isListClip (Clip_Row x) = isList x
  isListClip (Clip_Field x) = isList x
  isListClip (Clip_Test x) = isList x
  isListClip (Clip_StyledText x) = isList x
  isListClip (Clip_Word x) = isList x
  isListClip (Clip_WordPart x) = isList x
  isListClip (Clip_TextStyle x) = isList x
  isListClip (Clip_Int_ x) = isList x
  isListClip (Clip_Float_ x) = isList x
  isListClip (Clip_List_Expense x) = isList x
  isListClip (Clip_List_Currency x) = isList x
  isListClip (Clip_List_Thing x) = isList x
  isListClip (Clip_List_Task x) = isList x
  isListClip (Clip_List_Word x) = isList x
  isListClip (Clip_List_WordPart x) = isList x
  isListClip (Clip_Bool x) = isList x
  isListClip (Clip_Int x) = isList x
  isListClip (Clip_String x) = isList x
  isListClip (Clip_Float x) = isList x

  insertListClip i c Clip_Nothing = Clip_Nothing
  insertListClip i c (Clip_EnrichedDoc x) = insertList i c x
  insertListClip i c (Clip_Document x) = insertList i c x
  insertListClip i c (Clip_ChoiceDoc x) = insertList i c x
  insertListClip i c (Clip_Form x) = insertList i c x
  insertListClip i c (Clip_Expense x) = insertList i c x
  insertListClip i c (Clip_Currency x) = insertList i c x
  insertListClip i c (Clip_Tasks x) = insertList i c x
  insertListClip i c (Clip_Thing x) = insertList i c x
  insertListClip i c (Clip_Task x) = insertList i c x
  insertListClip i c (Clip_Description x) = insertList i c x
  insertListClip i c (Clip_Sudoku x) = insertList i c x
  insertListClip i c (Clip_Row x) = insertList i c x
  insertListClip i c (Clip_Field x) = insertList i c x
  insertListClip i c (Clip_Test x) = insertList i c x
  insertListClip i c (Clip_StyledText x) = insertList i c x
  insertListClip i c (Clip_Word x) = insertList i c x
  insertListClip i c (Clip_WordPart x) = insertList i c x
  insertListClip i c (Clip_TextStyle x) = insertList i c x
  insertListClip i c (Clip_Int_ x) = insertList i c x
  insertListClip i c (Clip_Float_ x) = insertList i c x
  insertListClip i c (Clip_List_Expense x) = insertList i c x
  insertListClip i c (Clip_List_Currency x) = insertList i c x
  insertListClip i c (Clip_List_Thing x) = insertList i c x
  insertListClip i c (Clip_List_Task x) = insertList i c x
  insertListClip i c (Clip_List_Word x) = insertList i c x
  insertListClip i c (Clip_List_WordPart x) = insertList i c x
  insertListClip i c (Clip_Bool x) = insertList i c x
  insertListClip i c (Clip_Int x) = insertList i c x
  insertListClip i c (Clip_String x) = insertList i c x
  insertListClip i c (Clip_Float x) = insertList i c x

  removeListClip i Clip_Nothing = Clip_Nothing
  removeListClip i (Clip_EnrichedDoc x) = removeList i x
  removeListClip i (Clip_Document x) = removeList i x
  removeListClip i (Clip_ChoiceDoc x) = removeList i x
  removeListClip i (Clip_Form x) = removeList i x
  removeListClip i (Clip_Expense x) = removeList i x
  removeListClip i (Clip_Currency x) = removeList i x
  removeListClip i (Clip_Tasks x) = removeList i x
  removeListClip i (Clip_Thing x) = removeList i x
  removeListClip i (Clip_Task x) = removeList i x
  removeListClip i (Clip_Description x) = removeList i x
  removeListClip i (Clip_Sudoku x) = removeList i x
  removeListClip i (Clip_Row x) = removeList i x
  removeListClip i (Clip_Field x) = removeList i x
  removeListClip i (Clip_Test x) = removeList i x
  removeListClip i (Clip_StyledText x) = removeList i x
  removeListClip i (Clip_Word x) = removeList i x
  removeListClip i (Clip_WordPart x) = removeList i x
  removeListClip i (Clip_TextStyle x) = removeList i x
  removeListClip i (Clip_Int_ x) = removeList i x
  removeListClip i (Clip_Float_ x) = removeList i x
  removeListClip i (Clip_List_Expense x) = removeList i x
  removeListClip i (Clip_List_Currency x) = removeList i x
  removeListClip i (Clip_List_Thing x) = removeList i x
  removeListClip i (Clip_List_Task x) = removeList i x
  removeListClip i (Clip_List_Word x) = removeList i x
  removeListClip i (Clip_List_WordPart x) = removeList i x
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
  select (0:p) (FormDoc x0) = select p x0
  select (0:p) (TaskDoc x0) = select p x0
  select (0:p) (SudokuDoc x0) = select p x0
  select (0:p) (TestDoc x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_ChoiceDoc c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on ChoiceDoc") x
  paste (0:p) c (FormDoc x0) = FormDoc (paste p c x0)
  paste (0:p) c (TaskDoc x0) = TaskDoc (paste p c x0)
  paste (0:p) c (SudokuDoc x0) = SudokuDoc (paste p c x0)
  paste (0:p) c (TestDoc x0) = TestDoc (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("FormDoc {Form} "  , Clip_ChoiceDoc $ FormDoc hole)
                   , ("TaskDoc {Tasks} "  , Clip_ChoiceDoc $ TaskDoc hole)
                   , ("SudokuDoc {Sudoku} "  , Clip_ChoiceDoc $ SudokuDoc hole)
                   , ("TestDoc {Test} "  , Clip_ChoiceDoc $ TestDoc hole)
                   ,("{ChoiceDoc}", Clip_ChoiceDoc hole)
                   ]

  arity (FormDoc x0) = 1
  arity (TaskDoc x0) = 1
  arity (SudokuDoc x0) = 1
  arity (TestDoc x0) = 1
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

instance Editable Form Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Form x
  select (0:p) (Form x0 x1 x2 x3 x4) = select p x0
  select (1:p) (Form x0 x1 x2 x3 x4) = select p x1
  select (2:p) (Form x0 x1 x2 x3 x4) = select p x2
  select (3:p) (Form x0 x1 x2 x3 x4) = select p x3
  select (4:p) (Form x0 x1 x2 x3 x4) = select p x4
  select _ _ = Clip_Nothing

  paste [] (Clip_Form c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Form") x
  paste (0:p) c (Form x0 x1 x2 x3 x4) = Form (paste p c x0) x1 x2 x3 x4
  paste (1:p) c (Form x0 x1 x2 x3 x4) = Form x0 (paste p c x1) x2 x3 x4
  paste (2:p) c (Form x0 x1 x2 x3 x4) = Form x0 x1 (paste p c x2) x3 x4
  paste (3:p) c (Form x0 x1 x2 x3 x4) = Form x0 x1 x2 (paste p c x3) x4
  paste (4:p) c (Form x0 x1 x2 x3 x4) = Form x0 x1 x2 x3 (paste p c x4)
  paste _ _ x = x

  alternatives _ = [ ("Form {Description} {Description} {List_Expense} {Int} {List_Currency} "  , Clip_Form $ Form hole hole hole hole hole)
                   ,("{Form}", Clip_Form hole)
                   ]

  arity (Form x0 x1 x2 x3 x4) = 5
  arity _                        = 0

  toClip t = Clip_Form t

  fromClip (Clip_Form t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrForm

  hole = HoleForm

  holeNodeConstr = Node_HoleForm

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Expense Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Expense x
  select (0:p) (Expense x0 x1 x2) = select p x0
  select (1:p) (Expense x0 x1 x2) = select p x1
  select (2:p) (Expense x0 x1 x2) = select p x2
  select _ _ = Clip_Nothing

  paste [] (Clip_Expense c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Expense") x
  paste (0:p) c (Expense x0 x1 x2) = Expense (paste p c x0) x1 x2
  paste (1:p) c (Expense x0 x1 x2) = Expense x0 (paste p c x1) x2
  paste (2:p) c (Expense x0 x1 x2) = Expense x0 x1 (paste p c x2)
  paste _ _ x = x

  alternatives _ = [ ("Expense {Description} {Float_} {Int} "  , Clip_Expense $ Expense hole hole hole)
                   ,("{Expense}", Clip_Expense hole)
                   ]

  arity (Expense x0 x1 x2) = 3
  arity _                        = 0

  toClip t = Clip_Expense t

  fromClip (Clip_Expense t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrExpense

  hole = HoleExpense

  holeNodeConstr = Node_HoleExpense

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Currency Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Currency x
  select (0:p) (Currency x0 x1) = select p x0
  select (1:p) (Currency x0 x1) = select p x1
  select _ _ = Clip_Nothing

  paste [] (Clip_Currency c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Currency") x
  paste (0:p) c (Currency x0 x1) = Currency (paste p c x0) x1
  paste (1:p) c (Currency x0 x1) = Currency x0 (paste p c x1)
  paste _ _ x = x

  alternatives _ = [ ("Currency {Description} {Float_} "  , Clip_Currency $ Currency hole hole)
                   ,("{Currency}", Clip_Currency hole)
                   ]

  arity (Currency x0 x1) = 2
  arity _                        = 0

  toClip t = Clip_Currency t

  fromClip (Clip_Currency t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrCurrency

  hole = HoleCurrency

  holeNodeConstr = Node_HoleCurrency

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Tasks Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Tasks x
  select (0:p) (Tasks x0 x1 x2) = select p x0
  select (1:p) (Tasks x0 x1 x2) = select p x1
  select (2:p) (Tasks x0 x1 x2) = select p x2
  select _ _ = Clip_Nothing

  paste [] (Clip_Tasks c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Tasks") x
  paste (0:p) c (Tasks x0 x1 x2) = Tasks (paste p c x0) x1 x2
  paste (1:p) c (Tasks x0 x1 x2) = Tasks x0 (paste p c x1) x2
  paste (2:p) c (Tasks x0 x1 x2) = Tasks x0 x1 (paste p c x2)
  paste _ _ x = x

  alternatives _ = [ ("Tasks {List_Thing} {Bool} {List_Task} "  , Clip_Tasks $ Tasks hole hole hole)
                   ,("{Tasks}", Clip_Tasks hole)
                   ]

  arity (Tasks x0 x1 x2) = 3
  arity _                        = 0

  toClip t = Clip_Tasks t

  fromClip (Clip_Tasks t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrTasks

  hole = HoleTasks

  holeNodeConstr = Node_HoleTasks

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Thing Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Thing x
  select (0:p) (Thing x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Thing c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Thing") x
  paste (0:p) c (Thing x0) = Thing (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("Thing {Int} "  , Clip_Thing $ Thing hole)
                   ,("{Thing}", Clip_Thing hole)
                   ]

  arity (Thing x0) = 1
  arity _                        = 0

  toClip t = Clip_Thing t

  fromClip (Clip_Thing t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrThing

  hole = HoleThing

  holeNodeConstr = Node_HoleThing

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Task Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Task x
  select (0:p) (BasicTask x0 x1) = select p x0
  select (1:p) (BasicTask x0 x1) = select p x1
  select (0:p) (CompositeTask x0 x1 x2) = select p x0
  select (1:p) (CompositeTask x0 x1 x2) = select p x1
  select (2:p) (CompositeTask x0 x1 x2) = select p x2
  select _ _ = Clip_Nothing

  paste [] (Clip_Task c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Task") x
  paste (0:p) c (BasicTask x0 x1) = BasicTask (paste p c x0) x1
  paste (1:p) c (BasicTask x0 x1) = BasicTask x0 (paste p c x1)
  paste (0:p) c (CompositeTask x0 x1 x2) = CompositeTask (paste p c x0) x1 x2
  paste (1:p) c (CompositeTask x0 x1 x2) = CompositeTask x0 (paste p c x1) x2
  paste (2:p) c (CompositeTask x0 x1 x2) = CompositeTask x0 x1 (paste p c x2)
  paste _ _ x = x

  alternatives _ = [ ("BasicTask {Description} {Bool} "  , Clip_Task $ BasicTask hole hole)
                   , ("CompositeTask {Bool} {Description} {List_Task} "  , Clip_Task $ CompositeTask hole hole hole)
                   ,("{Task}", Clip_Task hole)
                   ]

  arity (BasicTask x0 x1) = 2
  arity (CompositeTask x0 x1 x2) = 3
  arity _                        = 0

  toClip t = Clip_Task t

  fromClip (Clip_Task t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrTask

  hole = HoleTask

  holeNodeConstr = Node_HoleTask

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Description Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Description x
  select (0:p) (Description x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Description c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Description") x
  paste (0:p) c (Description x0) = Description (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("Description {String} "  , Clip_Description $ Description hole)
                   ,("{Description}", Clip_Description hole)
                   ]

  arity (Description x0) = 1
  arity _                        = 0

  toClip t = Clip_Description t

  fromClip (Clip_Description t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrDescription

  hole = HoleDescription

  holeNodeConstr = Node_HoleDescription

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Sudoku Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Sudoku x
  select (0:p) (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x0
  select (1:p) (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x1
  select (2:p) (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x2
  select (3:p) (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x3
  select (4:p) (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x4
  select (5:p) (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x5
  select (6:p) (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x6
  select (7:p) (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x7
  select (8:p) (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x8
  select _ _ = Clip_Nothing

  paste [] (Clip_Sudoku c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Sudoku") x
  paste (0:p) c (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = Sudoku (paste p c x0) x1 x2 x3 x4 x5 x6 x7 x8
  paste (1:p) c (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = Sudoku x0 (paste p c x1) x2 x3 x4 x5 x6 x7 x8
  paste (2:p) c (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = Sudoku x0 x1 (paste p c x2) x3 x4 x5 x6 x7 x8
  paste (3:p) c (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = Sudoku x0 x1 x2 (paste p c x3) x4 x5 x6 x7 x8
  paste (4:p) c (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = Sudoku x0 x1 x2 x3 (paste p c x4) x5 x6 x7 x8
  paste (5:p) c (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = Sudoku x0 x1 x2 x3 x4 (paste p c x5) x6 x7 x8
  paste (6:p) c (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = Sudoku x0 x1 x2 x3 x4 x5 (paste p c x6) x7 x8
  paste (7:p) c (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = Sudoku x0 x1 x2 x3 x4 x5 x6 (paste p c x7) x8
  paste (8:p) c (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = Sudoku x0 x1 x2 x3 x4 x5 x6 x7 (paste p c x8)
  paste _ _ x = x

  alternatives _ = [ ("Sudoku {Row} {Row} {Row} {Row} {Row} {Row} {Row} {Row} {Row} "  , Clip_Sudoku $ Sudoku hole hole hole hole hole hole hole hole hole)
                   ,("{Sudoku}", Clip_Sudoku hole)
                   ]

  arity (Sudoku x0 x1 x2 x3 x4 x5 x6 x7 x8) = 9
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

instance Editable Row Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Row x
  select (0:p) (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x0
  select (1:p) (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x1
  select (2:p) (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x2
  select (3:p) (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x3
  select (4:p) (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x4
  select (5:p) (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x5
  select (6:p) (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x6
  select (7:p) (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x7
  select (8:p) (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = select p x8
  select _ _ = Clip_Nothing

  paste [] (Clip_Row c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Row") x
  paste (0:p) c (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = Row (paste p c x0) x1 x2 x3 x4 x5 x6 x7 x8
  paste (1:p) c (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = Row x0 (paste p c x1) x2 x3 x4 x5 x6 x7 x8
  paste (2:p) c (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = Row x0 x1 (paste p c x2) x3 x4 x5 x6 x7 x8
  paste (3:p) c (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = Row x0 x1 x2 (paste p c x3) x4 x5 x6 x7 x8
  paste (4:p) c (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = Row x0 x1 x2 x3 (paste p c x4) x5 x6 x7 x8
  paste (5:p) c (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = Row x0 x1 x2 x3 x4 (paste p c x5) x6 x7 x8
  paste (6:p) c (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = Row x0 x1 x2 x3 x4 x5 (paste p c x6) x7 x8
  paste (7:p) c (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = Row x0 x1 x2 x3 x4 x5 x6 (paste p c x7) x8
  paste (8:p) c (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = Row x0 x1 x2 x3 x4 x5 x6 x7 (paste p c x8)
  paste _ _ x = x

  alternatives _ = [ ("Row {Field} {Field} {Field} {Field} {Field} {Field} {Field} {Field} {Field} "  , Clip_Row $ Row hole hole hole hole hole hole hole hole hole)
                   ,("{Row}", Clip_Row hole)
                   ]

  arity (Row x0 x1 x2 x3 x4 x5 x6 x7 x8) = 9
  arity _                        = 0

  toClip t = Clip_Row t

  fromClip (Clip_Row t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrRow

  hole = HoleRow

  holeNodeConstr = Node_HoleRow

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Field Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Field x
  select (0:p) (Field x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Field c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Field") x
  paste (0:p) c (Field x0) = Field (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("Field {Int_} "  , Clip_Field $ Field hole)
                   ,("{Field}", Clip_Field hole)
                   ]

  arity (Field x0) = 1
  arity _                        = 0

  toClip t = Clip_Field t

  fromClip (Clip_Field t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrField

  hole = HoleField

  holeNodeConstr = Node_HoleField

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Test Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Test x
  select (0:p) (Test x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Test c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Test") x
  paste (0:p) c (Test x0) = Test (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("Test {StyledText} "  , Clip_Test $ Test hole)
                   ,("{Test}", Clip_Test hole)
                   ]

  arity (Test x0) = 1
  arity _                        = 0

  toClip t = Clip_Test t

  fromClip (Clip_Test t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrTest

  hole = HoleTest

  holeNodeConstr = Node_HoleTest

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable StyledText Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_StyledText x
  select (0:p) (StyledText x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_StyledText c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on StyledText") x
  paste (0:p) c (StyledText x0) = StyledText (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("StyledText {List_Word} "  , Clip_StyledText $ StyledText hole)
                   ,("{StyledText}", Clip_StyledText hole)
                   ]

  arity (StyledText x0) = 1
  arity _                        = 0

  toClip t = Clip_StyledText t

  fromClip (Clip_StyledText t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrStyledText

  hole = HoleStyledText

  holeNodeConstr = Node_HoleStyledText

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Word Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Word x
  select (0:p) (Word x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Word c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Word") x
  paste (0:p) c (Word x0) = Word (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("Word {List_WordPart} "  , Clip_Word $ Word hole)
                   ,("{Word}", Clip_Word hole)
                   ]

  arity (Word x0) = 1
  arity _                        = 0

  toClip t = Clip_Word t

  fromClip (Clip_Word t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrWord

  hole = HoleWord

  holeNodeConstr = Node_HoleWord

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable WordPart Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_WordPart x
  select (0:p) (WordPart _ x0) = select p x0
  select (0:p) (OpenTag x0) = select p x0
  select (0:p) (CloseTag x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_WordPart c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on WordPart") x
  paste (0:p) c (WordPart i0 x0) = WordPart i0 (paste p c x0)
  paste (0:p) c (OpenTag x0) = OpenTag (paste p c x0)
  paste (0:p) c (CloseTag x0) = CloseTag (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("WordPart {String} "  , Clip_WordPart $ WordPart NoIDP hole)
                   , ("OpenTag {TextStyle} "  , Clip_WordPart $ OpenTag hole)
                   , ("CloseTag {TextStyle} "  , Clip_WordPart $ CloseTag hole)
                   ,("{WordPart}", Clip_WordPart hole)
                   ]

  arity (WordPart _ x0) = 1
  arity (OpenTag x0) = 1
  arity (CloseTag x0) = 1
  arity _                        = 0

  toClip t = Clip_WordPart t

  fromClip (Clip_WordPart t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrWordPart

  hole = HoleWordPart

  holeNodeConstr = Node_HoleWordPart

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable TextStyle Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_TextStyle x
  select (0:p) (TextFontSize x0) = select p x0
  select (0:p) (TextColor x0 x1 x2) = select p x0
  select (1:p) (TextColor x0 x1 x2) = select p x1
  select (2:p) (TextColor x0 x1 x2) = select p x2
  select _ _ = Clip_Nothing

  paste [] (Clip_TextStyle c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on TextStyle") x
  paste (0:p) c (TextFontSize x0) = TextFontSize (paste p c x0)
  paste (0:p) c (TextColor x0 x1 x2) = TextColor (paste p c x0) x1 x2
  paste (1:p) c (TextColor x0 x1 x2) = TextColor x0 (paste p c x1) x2
  paste (2:p) c (TextColor x0 x1 x2) = TextColor x0 x1 (paste p c x2)
  paste _ _ x = x

  alternatives _ = [ ("TextBold "  , Clip_TextStyle $ TextBold)
                   , ("TextItalic "  , Clip_TextStyle $ TextItalic)
                   , ("TextFontSize {Int} "  , Clip_TextStyle $ TextFontSize hole)
                   , ("TextColor {Int} {Int} {Int} "  , Clip_TextStyle $ TextColor hole hole hole)
                   ,("{TextStyle}", Clip_TextStyle hole)
                   ]

  arity (TextBold) = 0
  arity (TextItalic) = 0
  arity (TextFontSize x0) = 1
  arity (TextColor x0 x1 x2) = 3
  arity _                        = 0

  toClip t = Clip_TextStyle t

  fromClip (Clip_TextStyle t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrTextStyle

  hole = HoleTextStyle

  holeNodeConstr = Node_HoleTextStyle

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Int_ Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_Int_ x
  select (0:p) (Int_ x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Int_ c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Int_") x
  paste (0:p) c (Int_ x0) = Int_ (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("Int_ {Int} "  , Clip_Int_ $ Int_ hole)
                   ,("{Int_}", Clip_Int_ hole)
                   ]

  arity (Int_ x0) = 1
  arity _                        = 0

  toClip t = Clip_Int_ t

  fromClip (Clip_Int_ t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrInt_

  hole = HoleInt_

  holeNodeConstr = Node_HoleInt_

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

instance Editable List_Expense Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_List_Expense x
  select (n:p) (List_Expense cxs) =
    let xs = fromConsList_Expense cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Expense c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Expense")   x
  paste (n:p) c (List_Expense cxs) =
    let xs = fromConsList_Expense cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Expense (replaceList_Expense n x' cxs)
        else List_Expense cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Expense}", Clip_List_Expense hole)
                   ]

  arity (List_Expense x1) = length (fromConsList_Expense x1)
  arity _ = 0

  toClip t = Clip_List_Expense t

  fromClip (Clip_List_Expense t) = Just t
  fromClip _ = Nothing

  parseErr = ParseErrList_Expense

  hole = List_Expense Nil_Expense

  holeNodeConstr = Node_HoleList_Expense

  isList _ = True

  insertList n (Clip_Expense c) (List_Expense cxs) = Clip_List_Expense $ List_Expense (insertList_Expense n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Expense xs
  insertList _ c xs = Clip_List_Expense xs

  removeList n (List_Expense cxs) = Clip_List_Expense $ List_Expense (removeList_Expense n cxs)
  removeList _ xs = Clip_List_Expense $ xs

instance Editable List_Currency Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_List_Currency x
  select (n:p) (List_Currency cxs) =
    let xs = fromConsList_Currency cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Currency c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Currency")   x
  paste (n:p) c (List_Currency cxs) =
    let xs = fromConsList_Currency cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Currency (replaceList_Currency n x' cxs)
        else List_Currency cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Currency}", Clip_List_Currency hole)
                   ]

  arity (List_Currency x1) = length (fromConsList_Currency x1)
  arity _ = 0

  toClip t = Clip_List_Currency t

  fromClip (Clip_List_Currency t) = Just t
  fromClip _ = Nothing

  parseErr = ParseErrList_Currency

  hole = List_Currency Nil_Currency

  holeNodeConstr = Node_HoleList_Currency

  isList _ = True

  insertList n (Clip_Currency c) (List_Currency cxs) = Clip_List_Currency $ List_Currency (insertList_Currency n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Currency xs
  insertList _ c xs = Clip_List_Currency xs

  removeList n (List_Currency cxs) = Clip_List_Currency $ List_Currency (removeList_Currency n cxs)
  removeList _ xs = Clip_List_Currency $ xs

instance Editable List_Thing Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_List_Thing x
  select (n:p) (List_Thing cxs) =
    let xs = fromConsList_Thing cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Thing c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Thing")   x
  paste (n:p) c (List_Thing cxs) =
    let xs = fromConsList_Thing cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Thing (replaceList_Thing n x' cxs)
        else List_Thing cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Thing}", Clip_List_Thing hole)
                   ]

  arity (List_Thing x1) = length (fromConsList_Thing x1)
  arity _ = 0

  toClip t = Clip_List_Thing t

  fromClip (Clip_List_Thing t) = Just t
  fromClip _ = Nothing

  parseErr = ParseErrList_Thing

  hole = List_Thing Nil_Thing

  holeNodeConstr = Node_HoleList_Thing

  isList _ = True

  insertList n (Clip_Thing c) (List_Thing cxs) = Clip_List_Thing $ List_Thing (insertList_Thing n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Thing xs
  insertList _ c xs = Clip_List_Thing xs

  removeList n (List_Thing cxs) = Clip_List_Thing $ List_Thing (removeList_Thing n cxs)
  removeList _ xs = Clip_List_Thing $ xs

instance Editable List_Task Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_List_Task x
  select (n:p) (List_Task cxs) =
    let xs = fromConsList_Task cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Task c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Task")   x
  paste (n:p) c (List_Task cxs) =
    let xs = fromConsList_Task cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Task (replaceList_Task n x' cxs)
        else List_Task cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Task}", Clip_List_Task hole)
                   ]

  arity (List_Task x1) = length (fromConsList_Task x1)
  arity _ = 0

  toClip t = Clip_List_Task t

  fromClip (Clip_List_Task t) = Just t
  fromClip _ = Nothing

  parseErr = ParseErrList_Task

  hole = List_Task Nil_Task

  holeNodeConstr = Node_HoleList_Task

  isList _ = True

  insertList n (Clip_Task c) (List_Task cxs) = Clip_List_Task $ List_Task (insertList_Task n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Task xs
  insertList _ c xs = Clip_List_Task xs

  removeList n (List_Task cxs) = Clip_List_Task $ List_Task (removeList_Task n cxs)
  removeList _ xs = Clip_List_Task $ xs

instance Editable List_Word Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_List_Word x
  select (n:p) (List_Word cxs) =
    let xs = fromConsList_Word cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Word c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Word")   x
  paste (n:p) c (List_Word cxs) =
    let xs = fromConsList_Word cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Word (replaceList_Word n x' cxs)
        else List_Word cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Word}", Clip_List_Word hole)
                   ]

  arity (List_Word x1) = length (fromConsList_Word x1)
  arity _ = 0

  toClip t = Clip_List_Word t

  fromClip (Clip_List_Word t) = Just t
  fromClip _ = Nothing

  parseErr = ParseErrList_Word

  hole = List_Word Nil_Word

  holeNodeConstr = Node_HoleList_Word

  isList _ = True

  insertList n (Clip_Word c) (List_Word cxs) = Clip_List_Word $ List_Word (insertList_Word n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Word xs
  insertList _ c xs = Clip_List_Word xs

  removeList n (List_Word cxs) = Clip_List_Word $ List_Word (removeList_Word n cxs)
  removeList _ xs = Clip_List_Word $ xs

instance Editable List_WordPart Document EnrichedDoc Node ClipDoc UserToken where
  select [] x = Clip_List_WordPart x
  select (n:p) (List_WordPart cxs) =
    let xs = fromConsList_WordPart cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_WordPart c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_WordPart")   x
  paste (n:p) c (List_WordPart cxs) =
    let xs = fromConsList_WordPart cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_WordPart (replaceList_WordPart n x' cxs)
        else List_WordPart cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_WordPart}", Clip_List_WordPart hole)
                   ]

  arity (List_WordPart x1) = length (fromConsList_WordPart x1)
  arity _ = 0

  toClip t = Clip_List_WordPart t

  fromClip (Clip_List_WordPart t) = Just t
  fromClip _ = Nothing

  parseErr = ParseErrList_WordPart

  hole = List_WordPart Nil_WordPart

  holeNodeConstr = Node_HoleList_WordPart

  isList _ = True

  insertList n (Clip_WordPart c) (List_WordPart cxs) = Clip_List_WordPart $ List_WordPart (insertList_WordPart n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_WordPart xs
  insertList _ c xs = Clip_List_WordPart xs

  removeList n (List_WordPart cxs) = Clip_List_WordPart $ List_WordPart (removeList_WordPart n cxs)
  removeList _ xs = Clip_List_WordPart $ xs




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


