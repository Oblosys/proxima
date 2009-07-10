module ProxParser_Generated where

import Common.CommonTypes hiding (Dirty (..))
import Presentation.PresLayerTypes
import Presentation.PresLayerUtils

import Evaluation.DocumentEdit
import DocumentEdit_Generated
import DocUtils_Generated
import Evaluation.DocTypes
import DocTypes_Generated
import Presentation.PresentationParsing
import Data.Maybe
                                   
----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- ProxParser type synonym                                              --
--------------------------------------------------------------------------

type ProxParser a = ListParser Document Node ClipDoc UserToken a



--------------------------------------------------------------------------
-- Construct instance                                                   --
--------------------------------------------------------------------------

instance Construct Document Node ClipDoc UserToken where
  construct NoNode = error $ "ProxParser_Generated.construct not defined on NoNode"
  construct (Node_RootEnr _ _) = construct_RootEnr
  construct (Node_HoleEnrichedDoc _ _) = construct_HoleEnrichedDoc
  construct (Node_ParseErrEnrichedDoc _ _) = construct_ParseErrEnrichedDoc
  construct (Node_RootDoc _ _) = construct_RootDoc
  construct (Node_HoleDocument _ _) = construct_HoleDocument
  construct (Node_ParseErrDocument _ _) = construct_ParseErrDocument
  construct (Node_FormDoc _ _) = construct_FormDoc
  construct (Node_TaskDoc _ _) = construct_TaskDoc
  construct (Node_SudokuDoc _ _) = construct_SudokuDoc
  construct (Node_HoleChoiceDoc _ _) = construct_HoleChoiceDoc
  construct (Node_ParseErrChoiceDoc _ _) = construct_ParseErrChoiceDoc
  construct (Node_Form _ _) = construct_Form
  construct (Node_HoleForm _ _) = construct_HoleForm
  construct (Node_ParseErrForm _ _) = construct_ParseErrForm
  construct (Node_Expense _ _) = construct_Expense
  construct (Node_HoleExpense _ _) = construct_HoleExpense
  construct (Node_ParseErrExpense _ _) = construct_ParseErrExpense
  construct (Node_Currency _ _) = construct_Currency
  construct (Node_HoleCurrency _ _) = construct_HoleCurrency
  construct (Node_ParseErrCurrency _ _) = construct_ParseErrCurrency
  construct (Node_Tasks _ _) = construct_Tasks
  construct (Node_HoleTasks _ _) = construct_HoleTasks
  construct (Node_ParseErrTasks _ _) = construct_ParseErrTasks
  construct (Node_BasicTask _ _) = construct_BasicTask
  construct (Node_CompositeTask _ _) = construct_CompositeTask
  construct (Node_HoleTask _ _) = construct_HoleTask
  construct (Node_ParseErrTask _ _) = construct_ParseErrTask
  construct (Node_Description _ _) = construct_Description
  construct (Node_HoleDescription _ _) = construct_HoleDescription
  construct (Node_ParseErrDescription _ _) = construct_ParseErrDescription
  construct (Node_Sudoku _ _) = construct_Sudoku
  construct (Node_HoleSudoku _ _) = construct_HoleSudoku
  construct (Node_ParseErrSudoku _ _) = construct_ParseErrSudoku
  construct (Node_Row _ _) = construct_Row
  construct (Node_HoleRow _ _) = construct_HoleRow
  construct (Node_ParseErrRow _ _) = construct_ParseErrRow
  construct (Node_Field _ _) = construct_Field
  construct (Node_HoleField _ _) = construct_HoleField
  construct (Node_ParseErrField _ _) = construct_ParseErrField
  construct (Node_Int_ _ _) = construct_Int_
  construct (Node_HoleInt_ _ _) = construct_HoleInt_
  construct (Node_ParseErrInt_ _ _) = construct_ParseErrInt_
  construct (Node_Float_ _ _) = construct_Float_
  construct (Node_HoleFloat_ _ _) = construct_HoleFloat_
  construct (Node_ParseErrFloat_ _ _) = construct_ParseErrFloat_
  construct (Node_List_Expense _ _) = construct_List_Expense
  construct (Node_HoleList_Expense _ _) = construct_HoleList_Expense
  construct (Node_ParseErrList_Expense _ _) = construct_ParseErrList_Expense
  construct (Node_List_Currency _ _) = construct_List_Currency
  construct (Node_HoleList_Currency _ _) = construct_HoleList_Currency
  construct (Node_ParseErrList_Currency _ _) = construct_ParseErrList_Currency
  construct (Node_List_Task _ _) = construct_List_Task
  construct (Node_HoleList_Task _ _) = construct_HoleList_Task
  construct (Node_ParseErrList_Task _ _) = construct_ParseErrList_Task
construct_RootEnr tk ~[mClip0] = Clip_EnrichedDoc $ reuseRootEnr [tk]  (retrieveArg "RootEnr" "choiceDoc::ChoiceDoc" mClip0)
construct_HoleEnrichedDoc tk ~[] = Clip_EnrichedDoc $ hole
construct_ParseErrEnrichedDoc (StructuralTk _ _ pres _ _) ~[] = Clip_EnrichedDoc $ parseErr (StructuralParseErr pres)
construct_RootDoc tk ~[mClip0] = Clip_Document $ reuseRootDoc [tk]  (retrieveArg "RootDoc" "choiceDoc::ChoiceDoc" mClip0)
construct_HoleDocument tk ~[] = Clip_Document $ hole
construct_ParseErrDocument (StructuralTk _ _ pres _ _) ~[] = Clip_Document $ parseErr (StructuralParseErr pres)
construct_FormDoc tk ~[mClip0] = Clip_ChoiceDoc $ reuseFormDoc [tk]  (retrieveArg "FormDoc" "form::Form" mClip0)
construct_TaskDoc tk ~[mClip0] = Clip_ChoiceDoc $ reuseTaskDoc [tk]  (retrieveArg "TaskDoc" "tasks::Tasks" mClip0)
construct_SudokuDoc tk ~[mClip0] = Clip_ChoiceDoc $ reuseSudokuDoc [tk]  (retrieveArg "SudokuDoc" "sudoku::Sudoku" mClip0)
construct_HoleChoiceDoc tk ~[] = Clip_ChoiceDoc $ hole
construct_ParseErrChoiceDoc (StructuralTk _ _ pres _ _) ~[] = Clip_ChoiceDoc $ parseErr (StructuralParseErr pres)
construct_Form tk ~[mClip0,mClip1,mClip2,mClip3] = Clip_Form $ reuseForm [tk]  (retrieveArg "Form" "name::Description" mClip0) (retrieveArg "Form" "faculty::Description" mClip1) (retrieveArg "Form" "expenses::List_Expense" mClip2) (retrieveArg "Form" "currencies::List_Currency" mClip3)
construct_HoleForm tk ~[] = Clip_Form $ hole
construct_ParseErrForm (StructuralTk _ _ pres _ _) ~[] = Clip_Form $ parseErr (StructuralParseErr pres)
construct_Expense tk ~[mClip0,mClip1,mClip2] = Clip_Expense $ reuseExpense [tk]  (retrieveArg "Expense" "description::Description" mClip0) (retrieveArg "Expense" "amount::Float_" mClip1) (retrieveArg "Expense" "currencyIx::Int" mClip2)
construct_HoleExpense tk ~[] = Clip_Expense $ hole
construct_ParseErrExpense (StructuralTk _ _ pres _ _) ~[] = Clip_Expense $ parseErr (StructuralParseErr pres)
construct_Currency tk ~[mClip0,mClip1] = Clip_Currency $ reuseCurrency [tk]  (retrieveArg "Currency" "name::Description" mClip0) (retrieveArg "Currency" "euroRate::Float_" mClip1)
construct_HoleCurrency tk ~[] = Clip_Currency $ hole
construct_ParseErrCurrency (StructuralTk _ _ pres _ _) ~[] = Clip_Currency $ parseErr (StructuralParseErr pres)
construct_Tasks tk ~[mClip0,mClip1] = Clip_Tasks $ reuseTasks [tk]  (retrieveArg "Tasks" "showCompleted::Bool" mClip0) (retrieveArg "Tasks" "tasks::List_Task" mClip1)
construct_HoleTasks tk ~[] = Clip_Tasks $ hole
construct_ParseErrTasks (StructuralTk _ _ pres _ _) ~[] = Clip_Tasks $ parseErr (StructuralParseErr pres)
construct_BasicTask tk ~[mClip0,mClip1] = Clip_Task $ reuseBasicTask [tk]  (retrieveArg "BasicTask" "description::Description" mClip0) (retrieveArg "BasicTask" "completed::Bool" mClip1)
construct_CompositeTask tk ~[mClip0,mClip1,mClip2] = Clip_Task $ reuseCompositeTask [tk]  (retrieveArg "CompositeTask" "expanded::Bool" mClip0) (retrieveArg "CompositeTask" "description::Description" mClip1) (retrieveArg "CompositeTask" "subtasks::List_Task" mClip2)
construct_HoleTask tk ~[] = Clip_Task $ hole
construct_ParseErrTask (StructuralTk _ _ pres _ _) ~[] = Clip_Task $ parseErr (StructuralParseErr pres)
construct_Description tk ~[mClip0] = Clip_Description $ reuseDescription [tk]  (retrieveArg "Description" "str::String" mClip0)
construct_HoleDescription tk ~[] = Clip_Description $ hole
construct_ParseErrDescription (StructuralTk _ _ pres _ _) ~[] = Clip_Description $ parseErr (StructuralParseErr pres)
construct_Sudoku tk ~[mClip0,mClip1,mClip2,mClip3,mClip4,mClip5,mClip6,mClip7,mClip8] = Clip_Sudoku $ reuseSudoku [tk]  (retrieveArg "Sudoku" "r0::Row" mClip0) (retrieveArg "Sudoku" "r1::Row" mClip1) (retrieveArg "Sudoku" "r2::Row" mClip2) (retrieveArg "Sudoku" "r3::Row" mClip3) (retrieveArg "Sudoku" "r4::Row" mClip4) (retrieveArg "Sudoku" "r5::Row" mClip5) (retrieveArg "Sudoku" "r6::Row" mClip6) (retrieveArg "Sudoku" "r7::Row" mClip7) (retrieveArg "Sudoku" "r8::Row" mClip8)
construct_HoleSudoku tk ~[] = Clip_Sudoku $ hole
construct_ParseErrSudoku (StructuralTk _ _ pres _ _) ~[] = Clip_Sudoku $ parseErr (StructuralParseErr pres)
construct_Row tk ~[mClip0,mClip1,mClip2,mClip3,mClip4,mClip5,mClip6,mClip7,mClip8] = Clip_Row $ reuseRow [tk]  (retrieveArg "Row" "f0::Field" mClip0) (retrieveArg "Row" "f1::Field" mClip1) (retrieveArg "Row" "f2::Field" mClip2) (retrieveArg "Row" "f3::Field" mClip3) (retrieveArg "Row" "f4::Field" mClip4) (retrieveArg "Row" "f5::Field" mClip5) (retrieveArg "Row" "f6::Field" mClip6) (retrieveArg "Row" "f7::Field" mClip7) (retrieveArg "Row" "f8::Field" mClip8)
construct_HoleRow tk ~[] = Clip_Row $ hole
construct_ParseErrRow (StructuralTk _ _ pres _ _) ~[] = Clip_Row $ parseErr (StructuralParseErr pres)
construct_Field tk ~[mClip0] = Clip_Field $ reuseField [tk]  (retrieveArg "Field" "val::Int_" mClip0)
construct_HoleField tk ~[] = Clip_Field $ hole
construct_ParseErrField (StructuralTk _ _ pres _ _) ~[] = Clip_Field $ parseErr (StructuralParseErr pres)
construct_Int_ tk ~[mClip0] = Clip_Int_ $ reuseInt_ [tk]  (retrieveArg "Int_" "value::Int" mClip0)
construct_HoleInt_ tk ~[] = Clip_Int_ $ hole
construct_ParseErrInt_ (StructuralTk _ _ pres _ _) ~[] = Clip_Int_ $ parseErr (StructuralParseErr pres)
construct_Float_ tk ~[mClip0] = Clip_Float_ $ reuseFloat_ [tk]  (retrieveArg "Float_" "value::Float" mClip0)
construct_HoleFloat_ tk ~[] = Clip_Float_ $ hole
construct_ParseErrFloat_ (StructuralTk _ _ pres _ _) ~[] = Clip_Float_ $ parseErr (StructuralParseErr pres)
construct_List_Expense tk mClips = genericConstruct_List "Expense" toList_Expense mClips
construct_HoleList_Expense tk ~[] = Clip_List_Expense $ hole
construct_ParseErrList_Expense (StructuralTk _ _ pres _ _) ~[] = Clip_List_Expense $ parseErr (StructuralParseErr pres)
construct_List_Currency tk mClips = genericConstruct_List "Currency" toList_Currency mClips
construct_HoleList_Currency tk ~[] = Clip_List_Currency $ hole
construct_ParseErrList_Currency (StructuralTk _ _ pres _ _) ~[] = Clip_List_Currency $ parseErr (StructuralParseErr pres)
construct_List_Task tk mClips = genericConstruct_List "Task" toList_Task mClips
construct_HoleList_Task tk ~[] = Clip_List_Task $ hole
construct_ParseErrList_Task (StructuralTk _ _ pres _ _) ~[] = Clip_List_Task $ parseErr (StructuralParseErr pres)



--------------------------------------------------------------------------
-- reuse functions                                                      --
--------------------------------------------------------------------------

reuseRootEnr :: [Token doc Node clip token] -> Maybe ChoiceDoc -> EnrichedDoc
reuseRootEnr nodes ma0
  = case extractFromTokens extractRootEnr defaultRootEnr nodes of
           (RootEnr a0) -> genericReuse1 RootEnr a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseRootEnr"

reuseRootDoc :: [Token doc Node clip token] -> Maybe ChoiceDoc -> Document
reuseRootDoc nodes ma0
  = case extractFromTokens extractRootDoc defaultRootDoc nodes of
           (RootDoc a0) -> genericReuse1 RootDoc a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseRootDoc"

reuseFormDoc :: [Token doc Node clip token] -> Maybe Form -> ChoiceDoc
reuseFormDoc nodes ma0
  = case extractFromTokens extractFormDoc defaultFormDoc nodes of
           (FormDoc a0) -> genericReuse1 FormDoc a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseFormDoc"

reuseTaskDoc :: [Token doc Node clip token] -> Maybe Tasks -> ChoiceDoc
reuseTaskDoc nodes ma0
  = case extractFromTokens extractTaskDoc defaultTaskDoc nodes of
           (TaskDoc a0) -> genericReuse1 TaskDoc a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseTaskDoc"

reuseSudokuDoc :: [Token doc Node clip token] -> Maybe Sudoku -> ChoiceDoc
reuseSudokuDoc nodes ma0
  = case extractFromTokens extractSudokuDoc defaultSudokuDoc nodes of
           (SudokuDoc a0) -> genericReuse1 SudokuDoc a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseSudokuDoc"

reuseForm :: [Token doc Node clip token] -> Maybe Description -> Maybe Description -> Maybe List_Expense -> Maybe List_Currency -> Form
reuseForm nodes ma0 ma1 ma2 ma3
  = case extractFromTokens extractForm defaultForm nodes of
           (Form a0 a1 a2 a3) -> genericReuse4 Form a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "Internal error:ProxParser_Generated.reuseForm"

reuseExpense :: [Token doc Node clip token] -> Maybe Description -> Maybe Float_ -> Maybe Int -> Expense
reuseExpense nodes ma0 ma1 ma2
  = case extractFromTokens extractExpense defaultExpense nodes of
           (Expense a0 a1 a2) -> genericReuse3 Expense a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reuseExpense"

reuseCurrency :: [Token doc Node clip token] -> Maybe Description -> Maybe Float_ -> Currency
reuseCurrency nodes ma0 ma1
  = case extractFromTokens extractCurrency defaultCurrency nodes of
           (Currency a0 a1) -> genericReuse2 Currency a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseCurrency"

reuseTasks :: [Token doc Node clip token] -> Maybe Bool -> Maybe List_Task -> Tasks
reuseTasks nodes ma0 ma1
  = case extractFromTokens extractTasks defaultTasks nodes of
           (Tasks a0 a1) -> genericReuse2 Tasks a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseTasks"

reuseBasicTask :: [Token doc Node clip token] -> Maybe Description -> Maybe Bool -> Task
reuseBasicTask nodes ma0 ma1
  = case extractFromTokens extractBasicTask defaultBasicTask nodes of
           (BasicTask a0 a1) -> genericReuse2 BasicTask a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseBasicTask"

reuseCompositeTask :: [Token doc Node clip token] -> Maybe Bool -> Maybe Description -> Maybe List_Task -> Task
reuseCompositeTask nodes ma0 ma1 ma2
  = case extractFromTokens extractCompositeTask defaultCompositeTask nodes of
           (CompositeTask a0 a1 a2) -> genericReuse3 CompositeTask a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reuseCompositeTask"

reuseDescription :: [Token doc Node clip token] -> Maybe String -> Description
reuseDescription nodes ma0
  = case extractFromTokens extractDescription defaultDescription nodes of
           (Description a0) -> genericReuse1 Description a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseDescription"

reuseSudoku :: [Token doc Node clip token] -> Maybe Row -> Maybe Row -> Maybe Row -> Maybe Row -> Maybe Row -> Maybe Row -> Maybe Row -> Maybe Row -> Maybe Row -> Sudoku
reuseSudoku nodes ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8
  = case extractFromTokens extractSudoku defaultSudoku nodes of
           (Sudoku a0 a1 a2 a3 a4 a5 a6 a7 a8) -> genericReuse9 Sudoku a0 a1 a2 a3 a4 a5 a6 a7 a8 ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8
           _ -> error "Internal error:ProxParser_Generated.reuseSudoku"

reuseRow :: [Token doc Node clip token] -> Maybe Field -> Maybe Field -> Maybe Field -> Maybe Field -> Maybe Field -> Maybe Field -> Maybe Field -> Maybe Field -> Maybe Field -> Row
reuseRow nodes ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8
  = case extractFromTokens extractRow defaultRow nodes of
           (Row a0 a1 a2 a3 a4 a5 a6 a7 a8) -> genericReuse9 Row a0 a1 a2 a3 a4 a5 a6 a7 a8 ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8
           _ -> error "Internal error:ProxParser_Generated.reuseRow"

reuseField :: [Token doc Node clip token] -> Maybe Int_ -> Field
reuseField nodes ma0
  = case extractFromTokens extractField defaultField nodes of
           (Field a0) -> genericReuse1 Field a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseField"

reuseInt_ :: [Token doc Node clip token] -> Maybe Int -> Int_
reuseInt_ nodes ma0
  = case extractFromTokens extractInt_ defaultInt_ nodes of
           (Int_ a0) -> genericReuse1 Int_ a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseInt_"

reuseFloat_ :: [Token doc Node clip token] -> Maybe Float -> Float_
reuseFloat_ nodes ma0
  = case extractFromTokens extractFloat_ defaultFloat_ nodes of
           (Float_ a0) -> genericReuse1 Float_ a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseFloat_"

reuseList_Expense :: [Token doc Node clip token] -> Maybe ConsList_Expense -> List_Expense
reuseList_Expense nodes ma0
  = case extractFromTokens extractList_Expense defaultList_Expense nodes of
           (List_Expense a0) -> genericReuse1 List_Expense a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Expense"

reuseList_Currency :: [Token doc Node clip token] -> Maybe ConsList_Currency -> List_Currency
reuseList_Currency nodes ma0
  = case extractFromTokens extractList_Currency defaultList_Currency nodes of
           (List_Currency a0) -> genericReuse1 List_Currency a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Currency"

reuseList_Task :: [Token doc Node clip token] -> Maybe ConsList_Task -> List_Task
reuseList_Task nodes ma0
  = case extractFromTokens extractList_Task defaultList_Task nodes of
           (List_Task a0) -> genericReuse1 List_Task a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Task"




--------------------------------------------------------------------------
-- extract functions                                                    --
--------------------------------------------------------------------------

extractRootEnr :: Maybe Node -> Maybe EnrichedDoc
extractRootEnr (Just (Node_RootEnr x@(RootEnr _) _)) = Just x
extractRootEnr _ = Nothing

extractRootDoc :: Maybe Node -> Maybe Document
extractRootDoc (Just (Node_RootDoc x@(RootDoc _) _)) = Just x
extractRootDoc _ = Nothing

extractFormDoc :: Maybe Node -> Maybe ChoiceDoc
extractFormDoc (Just (Node_FormDoc x@(FormDoc _) _)) = Just x
extractFormDoc _ = Nothing

extractTaskDoc :: Maybe Node -> Maybe ChoiceDoc
extractTaskDoc (Just (Node_TaskDoc x@(TaskDoc _) _)) = Just x
extractTaskDoc _ = Nothing

extractSudokuDoc :: Maybe Node -> Maybe ChoiceDoc
extractSudokuDoc (Just (Node_SudokuDoc x@(SudokuDoc _) _)) = Just x
extractSudokuDoc _ = Nothing

extractForm :: Maybe Node -> Maybe Form
extractForm (Just (Node_Form x@(Form _ _ _ _) _)) = Just x
extractForm _ = Nothing

extractExpense :: Maybe Node -> Maybe Expense
extractExpense (Just (Node_Expense x@(Expense _ _ _) _)) = Just x
extractExpense _ = Nothing

extractCurrency :: Maybe Node -> Maybe Currency
extractCurrency (Just (Node_Currency x@(Currency _ _) _)) = Just x
extractCurrency _ = Nothing

extractTasks :: Maybe Node -> Maybe Tasks
extractTasks (Just (Node_Tasks x@(Tasks _ _) _)) = Just x
extractTasks _ = Nothing

extractBasicTask :: Maybe Node -> Maybe Task
extractBasicTask (Just (Node_BasicTask x@(BasicTask _ _) _)) = Just x
extractBasicTask _ = Nothing

extractCompositeTask :: Maybe Node -> Maybe Task
extractCompositeTask (Just (Node_CompositeTask x@(CompositeTask _ _ _) _)) = Just x
extractCompositeTask _ = Nothing

extractDescription :: Maybe Node -> Maybe Description
extractDescription (Just (Node_Description x@(Description _) _)) = Just x
extractDescription _ = Nothing

extractSudoku :: Maybe Node -> Maybe Sudoku
extractSudoku (Just (Node_Sudoku x@(Sudoku _ _ _ _ _ _ _ _ _) _)) = Just x
extractSudoku _ = Nothing

extractRow :: Maybe Node -> Maybe Row
extractRow (Just (Node_Row x@(Row _ _ _ _ _ _ _ _ _) _)) = Just x
extractRow _ = Nothing

extractField :: Maybe Node -> Maybe Field
extractField (Just (Node_Field x@(Field _) _)) = Just x
extractField _ = Nothing

extractInt_ :: Maybe Node -> Maybe Int_
extractInt_ (Just (Node_Int_ x@(Int_ _) _)) = Just x
extractInt_ _ = Nothing

extractFloat_ :: Maybe Node -> Maybe Float_
extractFloat_ (Just (Node_Float_ x@(Float_ _) _)) = Just x
extractFloat_ _ = Nothing

extractList_Expense :: Maybe Node -> Maybe List_Expense
extractList_Expense (Just (Node_List_Expense x@(List_Expense _) _)) = Just x
extractList_Expense _ = Nothing

extractList_Currency :: Maybe Node -> Maybe List_Currency
extractList_Currency (Just (Node_List_Currency x@(List_Currency _) _)) = Just x
extractList_Currency _ = Nothing

extractList_Task :: Maybe Node -> Maybe List_Task
extractList_Task (Just (Node_List_Task x@(List_Task _) _)) = Just x
extractList_Task _ = Nothing




--------------------------------------------------------------------------
-- default functions                                                    --
--------------------------------------------------------------------------

defaultRootEnr :: EnrichedDoc
defaultRootEnr = RootEnr hole

defaultRootDoc :: Document
defaultRootDoc = RootDoc hole

defaultFormDoc :: ChoiceDoc
defaultFormDoc = FormDoc hole

defaultTaskDoc :: ChoiceDoc
defaultTaskDoc = TaskDoc hole

defaultSudokuDoc :: ChoiceDoc
defaultSudokuDoc = SudokuDoc hole

defaultForm :: Form
defaultForm = Form hole hole hole hole

defaultExpense :: Expense
defaultExpense = Expense hole hole hole

defaultCurrency :: Currency
defaultCurrency = Currency hole hole

defaultTasks :: Tasks
defaultTasks = Tasks hole hole

defaultBasicTask :: Task
defaultBasicTask = BasicTask hole hole

defaultCompositeTask :: Task
defaultCompositeTask = CompositeTask hole hole hole

defaultDescription :: Description
defaultDescription = Description hole

defaultSudoku :: Sudoku
defaultSudoku = Sudoku hole hole hole hole hole hole hole hole hole

defaultRow :: Row
defaultRow = Row hole hole hole hole hole hole hole hole hole

defaultField :: Field
defaultField = Field hole

defaultInt_ :: Int_
defaultInt_ = Int_ hole

defaultFloat_ :: Float_
defaultFloat_ = Float_ hole

defaultList_Expense :: List_Expense
defaultList_Expense = List_Expense Nil_Expense

defaultList_Currency :: List_Currency
defaultList_Currency = List_Currency Nil_Currency

defaultList_Task :: List_Task
defaultList_Task = List_Task Nil_Task




--------------------------------------------------------------------------
-- extractFromTokens                                                    --
--------------------------------------------------------------------------

-- return result of the first extraction application in the list that is not Nothing
extractFromTokens :: (Maybe Node -> Maybe a) -> a -> [Token doc Node clip token] -> a
extractFromTokens extr def []     = def
extractFromTokens extr def (t:ts) = maybe (extractFromTokens extr def ts) id (extr (tokenNode t))



--------------------------------------------------------------------------
-- genericReuse functions                                               --
--------------------------------------------------------------------------

genericReuse0 :: (r) ->
                 
                 r
genericReuse0 f =
  f

genericReuse1 :: (a0 -> r) ->
                 a0 -> 
                 Maybe a0 -> r
genericReuse1 f a0 ma0 =
  f (maybe a0 id ma0)

genericReuse2 :: (a0 -> a1 -> r) ->
                 a0 -> a1 -> 
                 Maybe a0 -> Maybe a1 -> r
genericReuse2 f a0 a1 ma0 ma1 =
  f (maybe a0 id ma0) (maybe a1 id ma1)

genericReuse3 :: (a0 -> a1 -> a2 -> r) ->
                 a0 -> a1 -> a2 -> 
                 Maybe a0 -> Maybe a1 -> Maybe a2 -> r
genericReuse3 f a0 a1 a2 ma0 ma1 ma2 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2)

genericReuse4 :: (a0 -> a1 -> a2 -> a3 -> r) ->
                 a0 -> a1 -> a2 -> a3 -> 
                 Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> r
genericReuse4 f a0 a1 a2 a3 ma0 ma1 ma2 ma3 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3)

genericReuse5 :: (a0 -> a1 -> a2 -> a3 -> a4 -> r) ->
                 a0 -> a1 -> a2 -> a3 -> a4 -> 
                 Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> r
genericReuse5 f a0 a1 a2 a3 a4 ma0 ma1 ma2 ma3 ma4 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4)

genericReuse6 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> r) ->
                 a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> 
                 Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> r
genericReuse6 f a0 a1 a2 a3 a4 a5 ma0 ma1 ma2 ma3 ma4 ma5 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) (maybe a5 id ma5)

genericReuse7 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r) ->
                 a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> 
                 Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> Maybe a6 -> r
genericReuse7 f a0 a1 a2 a3 a4 a5 a6 ma0 ma1 ma2 ma3 ma4 ma5 ma6 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) (maybe a5 id ma5) (maybe a6 id ma6)

genericReuse8 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r) ->
                 a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> 
                 Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> Maybe a6 -> Maybe a7 -> r
genericReuse8 f a0 a1 a2 a3 a4 a5 a6 a7 ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) (maybe a5 id ma5) (maybe a6 id ma6) (maybe a7 id ma7)

genericReuse9 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> r) ->
                 a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> 
                 Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> Maybe a6 -> Maybe a7 -> Maybe a8 -> r
genericReuse9 f a0 a1 a2 a3 a4 a5 a6 a7 a8 ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) (maybe a5 id ma5) (maybe a6 id ma6) (maybe a7 id ma7) (maybe a8 id ma8)



