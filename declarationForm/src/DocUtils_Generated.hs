module DocUtils_Generated where

import Evaluation.DocTypes
import DocTypes_Generated
import Evaluation.DocUtils
import Evaluation.DocumentEdit
import Presentation.PresTypes
import Presentation.XprezLib
import Common.DebugLevels

import UU.Parsing
import UU.Parsing.CharParser

import Common.CommonTypes hiding (Clean, Dirty)

initialDocument :: IO Document
initialDocument = return $ RootDoc $
{-    TestDoc $
      Test $ StyledText $ toList_Word [ Word $ toList_WordPart [ WordPart NoIDP "bla"
                                                               , OpenTag $ TextColor  255 0 0, WordPart NoIDP "bla", CloseTag $ TextColor 255 0 0
                                                               , WordPart NoIDP "bla" ]]
--      Test (StyledText $ toList_StringOrStyled [ String NoIDP "styledtext" ])
-}{-  TaskDoc $
    Tasks (toList_Thing [Thing 1, Thing 2, Thing 3]) (toList_Thing [])True $ toList_Task
      [ BasicTask (Description "Pinpas bestellen") True
      , CompositeTask True (Description "Declaratie editor bouwen") $ toList_Task
          [ CompositeTask True (Description "Upgrade Proxima") $ toList_Task
              [ BasicTask (Description "Drag & Drop") True
              , BasicTask (Description "Xprez") True
              ]
          , BasicTask (Description "Instantie maken") False 
          ]         
      ]       
-}                        
  
  FormDoc                 (Form (Description "Martijn") (Description "Informatica") 
                             (toList_Expense 
                               [ Expense (Description "Koffie") (Float_ 1) 0
                               , Expense (Description "Caipirinha") (Float_ 3) 1
                               ])
                              0
                             (toList_Currency
                               [ Currency (Description "Euro")   (Float_ 1.0)
                               , Currency (Description "Real")   (Float_ 0.345715)
                               , Currency (Description "Dollar") (Float_ 0.790938)
                               ])
                           )
  
{-  
SudokuDoc $ Sudoku 
    (Row (Field $ toInt_ 0) (Field $ toInt_ 0) (Field $ toInt_ 3)  (Field $ toInt_ 2) (Field $ toInt_ 0) (Field $ toInt_ 0)  (Field $ toInt_ 5) (Field $ toInt_ 7) (Field $ toInt_ 0)) 
    (Row (Field $ toInt_ 0) (Field $ toInt_ 0) (Field $ toInt_ 7)  (Field $ toInt_ 0) (Field $ toInt_ 0) (Field $ toInt_ 0)  (Field $ toInt_ 0) (Field $ toInt_ 0) (Field $ toInt_ 0)) 
    (Row (Field $ toInt_ 0) (Field $ toInt_ 0) (Field $ toInt_ 0)  (Field $ toInt_ 6) (Field $ toInt_ 0) (Field $ toInt_ 0)  (Field $ toInt_ 3) (Field $ toInt_ 0) (Field $ toInt_ 0)) 
    
    (Row (Field $ toInt_ 0) (Field $ toInt_ 2) (Field $ toInt_ 0)  (Field $ toInt_ 0) (Field $ toInt_ 3) (Field $ toInt_ 9)  (Field $ toInt_ 0) (Field $ toInt_ 1) (Field $ toInt_ 0)) 
    (Row (Field $ toInt_ 9) (Field $ toInt_ 0) (Field $ toInt_ 0)  (Field $ toInt_ 0) (Field $ toInt_ 1) (Field $ toInt_ 0)  (Field $ toInt_ 0) (Field $ toInt_ 0) (Field $ toInt_ 4)) 
    (Row (Field $ toInt_ 0) (Field $ toInt_ 5) (Field $ toInt_ 0)  (Field $ toInt_ 7) (Field $ toInt_ 2) (Field $ toInt_ 0)  (Field $ toInt_ 0) (Field $ toInt_ 8) (Field $ toInt_ 0)) 
    
    (Row (Field $ toInt_ 0) (Field $ toInt_ 0) (Field $ toInt_ 9)  (Field $ toInt_ 0) (Field $ toInt_ 0) (Field $ toInt_ 3)  (Field $ toInt_ 0) (Field $ toInt_ 0) (Field $ toInt_ 0)) 
    (Row (Field $ toInt_ 0) (Field $ toInt_ 0) (Field $ toInt_ 0)  (Field $ toInt_ 0) (Field $ toInt_ 0) (Field $ toInt_ 0)  (Field $ toInt_ 4) (Field $ toInt_ 0) (Field $ toInt_ 0)) 
    (Row (Field $ toInt_ 0) (Field $ toInt_ 4) (Field $ toInt_ 5)  (Field $ toInt_ 0) (Field $ toInt_ 0) (Field $ toInt_ 8)  (Field $ toInt_ 6) (Field $ toInt_ 0) (Field $ toInt_ 0)) 
-}  

toInt_ x = Int_ x
 
toFloat_ x = Float_ x

instance Eq TextStyle where
  TextBold   == TextBold   = True
  TextBold   == _          = False
  TextItalic == TextItalic = True
  TextItalic == _          = False
  TextFontSize s1 == TextFontSize s2 = s1 == s2
  TextFontSize s1 == _           = False
  TextColor r1 g1 b1 == TextColor r2 g2 b2 = r1 == r2 && g1 == g2 && b1 == b2
  TextColor _ _ _    == _  = False
-- no _ == _ = False case, because then we don't get an error when == has not been implemented for a new TextStyle

----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- rankNode                                                             --
--------------------------------------------------------------------------

rankNode :: Node -> Int
rankNode NoNode = 0
rankNode (Node_RootEnr _ _) = 1
rankNode (Node_HoleEnrichedDoc _ _) = 2
rankNode (Node_ParseErrEnrichedDoc _ _) = 3
rankNode (Node_RootDoc _ _) = 4
rankNode (Node_HoleDocument _ _) = 5
rankNode (Node_ParseErrDocument _ _) = 6
rankNode (Node_FormDoc _ _) = 7
rankNode (Node_TaskDoc _ _) = 8
rankNode (Node_SudokuDoc _ _) = 9
rankNode (Node_TestDoc _ _) = 10
rankNode (Node_HoleChoiceDoc _ _) = 11
rankNode (Node_ParseErrChoiceDoc _ _) = 12
rankNode (Node_Form _ _) = 13
rankNode (Node_HoleForm _ _) = 14
rankNode (Node_ParseErrForm _ _) = 15
rankNode (Node_Expense _ _) = 16
rankNode (Node_HoleExpense _ _) = 17
rankNode (Node_ParseErrExpense _ _) = 18
rankNode (Node_Currency _ _) = 19
rankNode (Node_HoleCurrency _ _) = 20
rankNode (Node_ParseErrCurrency _ _) = 21
rankNode (Node_Tasks _ _) = 22
rankNode (Node_HoleTasks _ _) = 23
rankNode (Node_ParseErrTasks _ _) = 24
rankNode (Node_Thing _ _) = 25
rankNode (Node_HoleThing _ _) = 26
rankNode (Node_ParseErrThing _ _) = 27
rankNode (Node_BasicTask _ _) = 28
rankNode (Node_CompositeTask _ _) = 29
rankNode (Node_HoleTask _ _) = 30
rankNode (Node_ParseErrTask _ _) = 31
rankNode (Node_Description _ _) = 32
rankNode (Node_HoleDescription _ _) = 33
rankNode (Node_ParseErrDescription _ _) = 34
rankNode (Node_Sudoku _ _) = 35
rankNode (Node_HoleSudoku _ _) = 36
rankNode (Node_ParseErrSudoku _ _) = 37
rankNode (Node_Row _ _) = 38
rankNode (Node_HoleRow _ _) = 39
rankNode (Node_ParseErrRow _ _) = 40
rankNode (Node_Field _ _) = 41
rankNode (Node_HoleField _ _) = 42
rankNode (Node_ParseErrField _ _) = 43
rankNode (Node_Test _ _) = 44
rankNode (Node_HoleTest _ _) = 45
rankNode (Node_ParseErrTest _ _) = 46
rankNode (Node_StyledText _ _) = 47
rankNode (Node_HoleStyledText _ _) = 48
rankNode (Node_ParseErrStyledText _ _) = 49
rankNode (Node_Word _ _) = 50
rankNode (Node_HoleWord _ _) = 51
rankNode (Node_ParseErrWord _ _) = 52
rankNode (Node_WordPart _ _) = 53
rankNode (Node_OpenTag _ _) = 54
rankNode (Node_CloseTag _ _) = 55
rankNode (Node_HoleWordPart _ _) = 56
rankNode (Node_ParseErrWordPart _ _) = 57
rankNode (Node_TextBold _ _) = 58
rankNode (Node_TextItalic _ _) = 59
rankNode (Node_TextFontSize _ _) = 60
rankNode (Node_TextColor _ _) = 61
rankNode (Node_HoleTextStyle _ _) = 62
rankNode (Node_ParseErrTextStyle _ _) = 63
rankNode (Node_Int_ _ _) = 64
rankNode (Node_HoleInt_ _ _) = 65
rankNode (Node_ParseErrInt_ _ _) = 66
rankNode (Node_Float_ _ _) = 67
rankNode (Node_HoleFloat_ _ _) = 68
rankNode (Node_ParseErrFloat_ _ _) = 69
rankNode (Node_List_Expense _ _) = 70
rankNode (Node_HoleList_Expense _ _) = 71
rankNode (Node_ParseErrList_Expense _ _) = 72
rankNode (Node_List_Currency _ _) = 73
rankNode (Node_HoleList_Currency _ _) = 74
rankNode (Node_ParseErrList_Currency _ _) = 75
rankNode (Node_List_Thing _ _) = 76
rankNode (Node_HoleList_Thing _ _) = 77
rankNode (Node_ParseErrList_Thing _ _) = 78
rankNode (Node_List_Task _ _) = 79
rankNode (Node_HoleList_Task _ _) = 80
rankNode (Node_ParseErrList_Task _ _) = 81
rankNode (Node_List_Word _ _) = 82
rankNode (Node_HoleList_Word _ _) = 83
rankNode (Node_ParseErrList_Word _ _) = 84
rankNode (Node_List_WordPart _ _) = 85
rankNode (Node_HoleList_WordPart _ _) = 86
rankNode (Node_ParseErrList_WordPart _ _) = 87



--------------------------------------------------------------------------
-- DocNode instance for Node                                            --
--------------------------------------------------------------------------

instance DocNode Node where
  noNode = NoNode

  pathNode NoNode            = NoPathD
  pathNode (Node_RootEnr _ pth) = PathD pth
  pathNode (Node_HoleEnrichedDoc _ pth) = PathD pth
  pathNode (Node_ParseErrEnrichedDoc _ pth) = PathD pth
  pathNode (Node_RootDoc _ pth) = PathD pth
  pathNode (Node_HoleDocument _ pth) = PathD pth
  pathNode (Node_ParseErrDocument _ pth) = PathD pth
  pathNode (Node_FormDoc _ pth) = PathD pth
  pathNode (Node_TaskDoc _ pth) = PathD pth
  pathNode (Node_SudokuDoc _ pth) = PathD pth
  pathNode (Node_TestDoc _ pth) = PathD pth
  pathNode (Node_HoleChoiceDoc _ pth) = PathD pth
  pathNode (Node_ParseErrChoiceDoc _ pth) = PathD pth
  pathNode (Node_Form _ pth) = PathD pth
  pathNode (Node_HoleForm _ pth) = PathD pth
  pathNode (Node_ParseErrForm _ pth) = PathD pth
  pathNode (Node_Expense _ pth) = PathD pth
  pathNode (Node_HoleExpense _ pth) = PathD pth
  pathNode (Node_ParseErrExpense _ pth) = PathD pth
  pathNode (Node_Currency _ pth) = PathD pth
  pathNode (Node_HoleCurrency _ pth) = PathD pth
  pathNode (Node_ParseErrCurrency _ pth) = PathD pth
  pathNode (Node_Tasks _ pth) = PathD pth
  pathNode (Node_HoleTasks _ pth) = PathD pth
  pathNode (Node_ParseErrTasks _ pth) = PathD pth
  pathNode (Node_Thing _ pth) = PathD pth
  pathNode (Node_HoleThing _ pth) = PathD pth
  pathNode (Node_ParseErrThing _ pth) = PathD pth
  pathNode (Node_BasicTask _ pth) = PathD pth
  pathNode (Node_CompositeTask _ pth) = PathD pth
  pathNode (Node_HoleTask _ pth) = PathD pth
  pathNode (Node_ParseErrTask _ pth) = PathD pth
  pathNode (Node_Description _ pth) = PathD pth
  pathNode (Node_HoleDescription _ pth) = PathD pth
  pathNode (Node_ParseErrDescription _ pth) = PathD pth
  pathNode (Node_Sudoku _ pth) = PathD pth
  pathNode (Node_HoleSudoku _ pth) = PathD pth
  pathNode (Node_ParseErrSudoku _ pth) = PathD pth
  pathNode (Node_Row _ pth) = PathD pth
  pathNode (Node_HoleRow _ pth) = PathD pth
  pathNode (Node_ParseErrRow _ pth) = PathD pth
  pathNode (Node_Field _ pth) = PathD pth
  pathNode (Node_HoleField _ pth) = PathD pth
  pathNode (Node_ParseErrField _ pth) = PathD pth
  pathNode (Node_Test _ pth) = PathD pth
  pathNode (Node_HoleTest _ pth) = PathD pth
  pathNode (Node_ParseErrTest _ pth) = PathD pth
  pathNode (Node_StyledText _ pth) = PathD pth
  pathNode (Node_HoleStyledText _ pth) = PathD pth
  pathNode (Node_ParseErrStyledText _ pth) = PathD pth
  pathNode (Node_Word _ pth) = PathD pth
  pathNode (Node_HoleWord _ pth) = PathD pth
  pathNode (Node_ParseErrWord _ pth) = PathD pth
  pathNode (Node_WordPart _ pth) = PathD pth
  pathNode (Node_OpenTag _ pth) = PathD pth
  pathNode (Node_CloseTag _ pth) = PathD pth
  pathNode (Node_HoleWordPart _ pth) = PathD pth
  pathNode (Node_ParseErrWordPart _ pth) = PathD pth
  pathNode (Node_TextBold _ pth) = PathD pth
  pathNode (Node_TextItalic _ pth) = PathD pth
  pathNode (Node_TextFontSize _ pth) = PathD pth
  pathNode (Node_TextColor _ pth) = PathD pth
  pathNode (Node_HoleTextStyle _ pth) = PathD pth
  pathNode (Node_ParseErrTextStyle _ pth) = PathD pth
  pathNode (Node_Int_ _ pth) = PathD pth
  pathNode (Node_HoleInt_ _ pth) = PathD pth
  pathNode (Node_ParseErrInt_ _ pth) = PathD pth
  pathNode (Node_Float_ _ pth) = PathD pth
  pathNode (Node_HoleFloat_ _ pth) = PathD pth
  pathNode (Node_ParseErrFloat_ _ pth) = PathD pth
  pathNode (Node_List_Expense _ pth) = PathD pth
  pathNode (Node_HoleList_Expense _ pth) = PathD pth
  pathNode (Node_ParseErrList_Expense _ pth) = PathD pth
  pathNode (Node_List_Currency _ pth) = PathD pth
  pathNode (Node_HoleList_Currency _ pth) = PathD pth
  pathNode (Node_ParseErrList_Currency _ pth) = PathD pth
  pathNode (Node_List_Thing _ pth) = PathD pth
  pathNode (Node_HoleList_Thing _ pth) = PathD pth
  pathNode (Node_ParseErrList_Thing _ pth) = PathD pth
  pathNode (Node_List_Task _ pth) = PathD pth
  pathNode (Node_HoleList_Task _ pth) = PathD pth
  pathNode (Node_ParseErrList_Task _ pth) = PathD pth
  pathNode (Node_List_Word _ pth) = PathD pth
  pathNode (Node_HoleList_Word _ pth) = PathD pth
  pathNode (Node_ParseErrList_Word _ pth) = PathD pth
  pathNode (Node_List_WordPart _ pth) = PathD pth
  pathNode (Node_HoleList_WordPart _ pth) = PathD pth
  pathNode (Node_ParseErrList_WordPart _ pth) = PathD pth

  typeOfNode (Node_RootEnr _ _) = BasicType "EnrichedDoc"
  typeOfNode (Node_HoleEnrichedDoc _ _) = BasicType "EnrichedDoc"
  typeOfNode (Node_ParseErrEnrichedDoc _ _) = BasicType "EnrichedDoc"
  typeOfNode (Node_RootDoc _ _) = BasicType "Document"
  typeOfNode (Node_HoleDocument _ _) = BasicType "Document"
  typeOfNode (Node_ParseErrDocument _ _) = BasicType "Document"
  typeOfNode (Node_FormDoc _ _) = BasicType "ChoiceDoc"
  typeOfNode (Node_TaskDoc _ _) = BasicType "ChoiceDoc"
  typeOfNode (Node_SudokuDoc _ _) = BasicType "ChoiceDoc"
  typeOfNode (Node_TestDoc _ _) = BasicType "ChoiceDoc"
  typeOfNode (Node_HoleChoiceDoc _ _) = BasicType "ChoiceDoc"
  typeOfNode (Node_ParseErrChoiceDoc _ _) = BasicType "ChoiceDoc"
  typeOfNode (Node_Form _ _) = BasicType "Form"
  typeOfNode (Node_HoleForm _ _) = BasicType "Form"
  typeOfNode (Node_ParseErrForm _ _) = BasicType "Form"
  typeOfNode (Node_Expense _ _) = BasicType "Expense"
  typeOfNode (Node_HoleExpense _ _) = BasicType "Expense"
  typeOfNode (Node_ParseErrExpense _ _) = BasicType "Expense"
  typeOfNode (Node_Currency _ _) = BasicType "Currency"
  typeOfNode (Node_HoleCurrency _ _) = BasicType "Currency"
  typeOfNode (Node_ParseErrCurrency _ _) = BasicType "Currency"
  typeOfNode (Node_Tasks _ _) = BasicType "Tasks"
  typeOfNode (Node_HoleTasks _ _) = BasicType "Tasks"
  typeOfNode (Node_ParseErrTasks _ _) = BasicType "Tasks"
  typeOfNode (Node_Thing _ _) = BasicType "Thing"
  typeOfNode (Node_HoleThing _ _) = BasicType "Thing"
  typeOfNode (Node_ParseErrThing _ _) = BasicType "Thing"
  typeOfNode (Node_BasicTask _ _) = BasicType "Task"
  typeOfNode (Node_CompositeTask _ _) = BasicType "Task"
  typeOfNode (Node_HoleTask _ _) = BasicType "Task"
  typeOfNode (Node_ParseErrTask _ _) = BasicType "Task"
  typeOfNode (Node_Description _ _) = BasicType "Description"
  typeOfNode (Node_HoleDescription _ _) = BasicType "Description"
  typeOfNode (Node_ParseErrDescription _ _) = BasicType "Description"
  typeOfNode (Node_Sudoku _ _) = BasicType "Sudoku"
  typeOfNode (Node_HoleSudoku _ _) = BasicType "Sudoku"
  typeOfNode (Node_ParseErrSudoku _ _) = BasicType "Sudoku"
  typeOfNode (Node_Row _ _) = BasicType "Row"
  typeOfNode (Node_HoleRow _ _) = BasicType "Row"
  typeOfNode (Node_ParseErrRow _ _) = BasicType "Row"
  typeOfNode (Node_Field _ _) = BasicType "Field"
  typeOfNode (Node_HoleField _ _) = BasicType "Field"
  typeOfNode (Node_ParseErrField _ _) = BasicType "Field"
  typeOfNode (Node_Test _ _) = BasicType "Test"
  typeOfNode (Node_HoleTest _ _) = BasicType "Test"
  typeOfNode (Node_ParseErrTest _ _) = BasicType "Test"
  typeOfNode (Node_StyledText _ _) = BasicType "StyledText"
  typeOfNode (Node_HoleStyledText _ _) = BasicType "StyledText"
  typeOfNode (Node_ParseErrStyledText _ _) = BasicType "StyledText"
  typeOfNode (Node_Word _ _) = BasicType "Word"
  typeOfNode (Node_HoleWord _ _) = BasicType "Word"
  typeOfNode (Node_ParseErrWord _ _) = BasicType "Word"
  typeOfNode (Node_WordPart _ _) = BasicType "WordPart"
  typeOfNode (Node_OpenTag _ _) = BasicType "WordPart"
  typeOfNode (Node_CloseTag _ _) = BasicType "WordPart"
  typeOfNode (Node_HoleWordPart _ _) = BasicType "WordPart"
  typeOfNode (Node_ParseErrWordPart _ _) = BasicType "WordPart"
  typeOfNode (Node_TextBold _ _) = BasicType "TextStyle"
  typeOfNode (Node_TextItalic _ _) = BasicType "TextStyle"
  typeOfNode (Node_TextFontSize _ _) = BasicType "TextStyle"
  typeOfNode (Node_TextColor _ _) = BasicType "TextStyle"
  typeOfNode (Node_HoleTextStyle _ _) = BasicType "TextStyle"
  typeOfNode (Node_ParseErrTextStyle _ _) = BasicType "TextStyle"
  typeOfNode (Node_Int_ _ _) = BasicType "Int_"
  typeOfNode (Node_HoleInt_ _ _) = BasicType "Int_"
  typeOfNode (Node_ParseErrInt_ _ _) = BasicType "Int_"
  typeOfNode (Node_Float_ _ _) = BasicType "Float_"
  typeOfNode (Node_HoleFloat_ _ _) = BasicType "Float_"
  typeOfNode (Node_ParseErrFloat_ _ _) = BasicType "Float_"
  typeOfNode (Node_List_Expense _ _) = ListType "Expense"
  typeOfNode (Node_HoleList_Expense _ _) = ListType "Expense"
  typeOfNode (Node_ParseErrList_Expense _ _) = ListType "Expense"
  typeOfNode (Node_List_Currency _ _) = ListType "Currency"
  typeOfNode (Node_HoleList_Currency _ _) = ListType "Currency"
  typeOfNode (Node_ParseErrList_Currency _ _) = ListType "Currency"
  typeOfNode (Node_List_Thing _ _) = ListType "Thing"
  typeOfNode (Node_HoleList_Thing _ _) = ListType "Thing"
  typeOfNode (Node_ParseErrList_Thing _ _) = ListType "Thing"
  typeOfNode (Node_List_Task _ _) = ListType "Task"
  typeOfNode (Node_HoleList_Task _ _) = ListType "Task"
  typeOfNode (Node_ParseErrList_Task _ _) = ListType "Task"
  typeOfNode (Node_List_Word _ _) = ListType "Word"
  typeOfNode (Node_HoleList_Word _ _) = ListType "Word"
  typeOfNode (Node_ParseErrList_Word _ _) = ListType "Word"
  typeOfNode (Node_List_WordPart _ _) = ListType "WordPart"
  typeOfNode (Node_HoleList_WordPart _ _) = ListType "WordPart"
  typeOfNode (Node_ParseErrList_WordPart _ _) = ListType "WordPart"



--------------------------------------------------------------------------
-- toXML functions                                                      --
--------------------------------------------------------------------------

toXMLEnrichedDoc (RootEnr choiceDoc) = Elt "RootEnr" [] $ [toXMLChoiceDoc choiceDoc]
toXMLEnrichedDoc (HoleEnrichedDoc) = EmptyElt "HoleEnrichedDoc" [] 
toXMLEnrichedDoc (ParseErrEnrichedDoc error) = EmptyElt "ParseErrEnrichedDoc" []
toXMLDocument (RootDoc choiceDoc) = Elt "RootDoc" [] $ [toXMLChoiceDoc choiceDoc]
toXMLDocument (HoleDocument) = EmptyElt "HoleDocument" [] 
toXMLDocument (ParseErrDocument error) = EmptyElt "ParseErrDocument" []
toXMLChoiceDoc (FormDoc form) = Elt "FormDoc" [] $ [toXMLForm form]
toXMLChoiceDoc (TaskDoc tasks) = Elt "TaskDoc" [] $ [toXMLTasks tasks]
toXMLChoiceDoc (SudokuDoc sudoku) = Elt "SudokuDoc" [] $ [toXMLSudoku sudoku]
toXMLChoiceDoc (TestDoc test) = Elt "TestDoc" [] $ [toXMLTest test]
toXMLChoiceDoc (HoleChoiceDoc) = EmptyElt "HoleChoiceDoc" [] 
toXMLChoiceDoc (ParseErrChoiceDoc error) = EmptyElt "ParseErrChoiceDoc" []
toXMLForm (Form name faculty expenses baseCurrency currencies) = Elt "Form" [] $ [toXMLDescription name] ++ [toXMLDescription faculty] ++ toXMLList_Expense expenses ++ [toXMLInt baseCurrency] ++ toXMLList_Currency currencies
toXMLForm (HoleForm) = EmptyElt "HoleForm" [] 
toXMLForm (ParseErrForm error) = EmptyElt "ParseErrForm" []
toXMLExpense (Expense description amount currencyIx) = Elt "Expense" [] $ [toXMLDescription description] ++ [toXMLFloat_ amount] ++ [toXMLInt currencyIx]
toXMLExpense (HoleExpense) = EmptyElt "HoleExpense" [] 
toXMLExpense (ParseErrExpense error) = EmptyElt "ParseErrExpense" []
toXMLCurrency (Currency name euroRate) = Elt "Currency" [] $ [toXMLDescription name] ++ [toXMLFloat_ euroRate]
toXMLCurrency (HoleCurrency) = EmptyElt "HoleCurrency" [] 
toXMLCurrency (ParseErrCurrency error) = EmptyElt "ParseErrCurrency" []
toXMLTasks (Tasks things1 things2 showCompleted tasks) = Elt "Tasks" [] $ toXMLList_Thing things1 ++ toXMLList_Thing things2 ++ [toXMLBool showCompleted] ++ toXMLList_Task tasks
toXMLTasks (HoleTasks) = EmptyElt "HoleTasks" [] 
toXMLTasks (ParseErrTasks error) = EmptyElt "ParseErrTasks" []
toXMLThing (Thing nr) = Elt "Thing" [] $ [toXMLInt nr]
toXMLThing (HoleThing) = EmptyElt "HoleThing" [] 
toXMLThing (ParseErrThing error) = EmptyElt "ParseErrThing" []
toXMLTask (BasicTask description completed) = Elt "BasicTask" [] $ [toXMLDescription description] ++ [toXMLBool completed]
toXMLTask (CompositeTask expanded description subtasks) = Elt "CompositeTask" [] $ [toXMLBool expanded] ++ [toXMLDescription description] ++ toXMLList_Task subtasks
toXMLTask (HoleTask) = EmptyElt "HoleTask" [] 
toXMLTask (ParseErrTask error) = EmptyElt "ParseErrTask" []
toXMLDescription (Description str) = Elt "Description" [] $ [toXMLString str]
toXMLDescription (HoleDescription) = EmptyElt "HoleDescription" [] 
toXMLDescription (ParseErrDescription error) = EmptyElt "ParseErrDescription" []
toXMLSudoku (Sudoku r0 r1 r2 r3 r4 r5 r6 r7 r8) = Elt "Sudoku" [] $ [toXMLRow r0] ++ [toXMLRow r1] ++ [toXMLRow r2] ++ [toXMLRow r3] ++ [toXMLRow r4] ++ [toXMLRow r5] ++ [toXMLRow r6] ++ [toXMLRow r7] ++ [toXMLRow r8]
toXMLSudoku (HoleSudoku) = EmptyElt "HoleSudoku" [] 
toXMLSudoku (ParseErrSudoku error) = EmptyElt "ParseErrSudoku" []
toXMLRow (Row f0 f1 f2 f3 f4 f5 f6 f7 f8) = Elt "Row" [] $ [toXMLField f0] ++ [toXMLField f1] ++ [toXMLField f2] ++ [toXMLField f3] ++ [toXMLField f4] ++ [toXMLField f5] ++ [toXMLField f6] ++ [toXMLField f7] ++ [toXMLField f8]
toXMLRow (HoleRow) = EmptyElt "HoleRow" [] 
toXMLRow (ParseErrRow error) = EmptyElt "ParseErrRow" []
toXMLField (Field val) = Elt "Field" [] $ [toXMLInt_ val]
toXMLField (HoleField) = EmptyElt "HoleField" [] 
toXMLField (ParseErrField error) = EmptyElt "ParseErrField" []
toXMLTest (Test styledText) = Elt "Test" [] $ [toXMLStyledText styledText]
toXMLTest (HoleTest) = EmptyElt "HoleTest" [] 
toXMLTest (ParseErrTest error) = EmptyElt "ParseErrTest" []
toXMLStyledText (StyledText words) = Elt "StyledText" [] $ toXMLList_Word words
toXMLStyledText (HoleStyledText) = EmptyElt "HoleStyledText" [] 
toXMLStyledText (ParseErrStyledText error) = EmptyElt "ParseErrStyledText" []
toXMLWord (Word parts) = Elt "Word" [] $ toXMLList_WordPart parts
toXMLWord (HoleWord) = EmptyElt "HoleWord" [] 
toXMLWord (ParseErrWord error) = EmptyElt "ParseErrWord" []
toXMLWordPart (WordPart _ word) = Elt "WordPart" [] $ [toXMLString word]
toXMLWordPart (OpenTag style) = Elt "OpenTag" [] $ [toXMLTextStyle style]
toXMLWordPart (CloseTag style) = Elt "CloseTag" [] $ [toXMLTextStyle style]
toXMLWordPart (HoleWordPart) = EmptyElt "HoleWordPart" [] 
toXMLWordPart (ParseErrWordPart error) = EmptyElt "ParseErrWordPart" []
toXMLTextStyle (TextBold) = EmptyElt "TextBold" [] 
toXMLTextStyle (TextItalic) = EmptyElt "TextItalic" [] 
toXMLTextStyle (TextFontSize s) = Elt "TextFontSize" [] $ [toXMLInt s]
toXMLTextStyle (TextColor r g b) = Elt "TextColor" [] $ [toXMLInt r] ++ [toXMLInt g] ++ [toXMLInt b]
toXMLTextStyle (HoleTextStyle) = EmptyElt "HoleTextStyle" [] 
toXMLTextStyle (ParseErrTextStyle error) = EmptyElt "ParseErrTextStyle" []
toXMLInt_ (Int_ value) = Elt "Int_" [] $ [toXMLInt value]
toXMLInt_ (HoleInt_) = EmptyElt "HoleInt_" [] 
toXMLInt_ (ParseErrInt_ error) = EmptyElt "ParseErrInt_" []
toXMLFloat_ (Float_ value) = Elt "Float_" [] $ [toXMLFloat value]
toXMLFloat_ (HoleFloat_) = EmptyElt "HoleFloat_" [] 
toXMLFloat_ (ParseErrFloat_ error) = EmptyElt "ParseErrFloat_" []
toXMLList_Expense (List_Expense xs) = toXMLConsList_Expense xs
toXMLList_Expense HoleList_Expense = []
toXMLList_Expense (ParseErrList_Expense _) = []
toXMLList_Currency (List_Currency xs) = toXMLConsList_Currency xs
toXMLList_Currency HoleList_Currency = []
toXMLList_Currency (ParseErrList_Currency _) = []
toXMLList_Thing (List_Thing xs) = toXMLConsList_Thing xs
toXMLList_Thing HoleList_Thing = []
toXMLList_Thing (ParseErrList_Thing _) = []
toXMLList_Task (List_Task xs) = toXMLConsList_Task xs
toXMLList_Task HoleList_Task = []
toXMLList_Task (ParseErrList_Task _) = []
toXMLList_Word (List_Word xs) = toXMLConsList_Word xs
toXMLList_Word HoleList_Word = []
toXMLList_Word (ParseErrList_Word _) = []
toXMLList_WordPart (List_WordPart xs) = toXMLConsList_WordPart xs
toXMLList_WordPart HoleList_WordPart = []
toXMLList_WordPart (ParseErrList_WordPart _) = []
toXMLConsList_Expense (Cons_Expense x xs) = toXMLExpense x : toXMLConsList_Expense xs
toXMLConsList_Expense Nil_Expense             = []
toXMLConsList_Currency (Cons_Currency x xs) = toXMLCurrency x : toXMLConsList_Currency xs
toXMLConsList_Currency Nil_Currency             = []
toXMLConsList_Thing (Cons_Thing x xs) = toXMLThing x : toXMLConsList_Thing xs
toXMLConsList_Thing Nil_Thing             = []
toXMLConsList_Task (Cons_Task x xs) = toXMLTask x : toXMLConsList_Task xs
toXMLConsList_Task Nil_Task             = []
toXMLConsList_Word (Cons_Word x xs) = toXMLWord x : toXMLConsList_Word xs
toXMLConsList_Word Nil_Word             = []
toXMLConsList_WordPart (Cons_WordPart x xs) = toXMLWordPart x : toXMLConsList_WordPart xs
toXMLConsList_WordPart Nil_WordPart             = []



--------------------------------------------------------------------------
-- parseXML functions                                                   --
--------------------------------------------------------------------------

parseXML_EnrichedDoc = parseXMLCns_RootEnr <|> parseHoleAndParseErr "EnrichedDoc" HoleEnrichedDoc
parseXMLCns_RootEnr = RootEnr <$ startTag "RootEnr" <*> parseXML_ChoiceDoc<* endTag "RootEnr"
parseXML_Document = parseXMLCns_RootDoc <|> parseHoleAndParseErr "Document" HoleDocument
parseXMLCns_RootDoc = RootDoc <$ startTag "RootDoc" <*> parseXML_ChoiceDoc<* endTag "RootDoc"
parseXML_ChoiceDoc = parseXMLCns_FormDoc <|> parseXMLCns_TaskDoc <|> parseXMLCns_SudokuDoc <|> parseXMLCns_TestDoc <|> parseHoleAndParseErr "ChoiceDoc" HoleChoiceDoc
parseXMLCns_FormDoc = FormDoc <$ startTag "FormDoc" <*> parseXML_Form<* endTag "FormDoc"
parseXMLCns_TaskDoc = TaskDoc <$ startTag "TaskDoc" <*> parseXML_Tasks<* endTag "TaskDoc"
parseXMLCns_SudokuDoc = SudokuDoc <$ startTag "SudokuDoc" <*> parseXML_Sudoku<* endTag "SudokuDoc"
parseXMLCns_TestDoc = TestDoc <$ startTag "TestDoc" <*> parseXML_Test<* endTag "TestDoc"
parseXML_Form = parseXMLCns_Form <|> parseHoleAndParseErr "Form" HoleForm
parseXMLCns_Form = Form <$ startTag "Form" <*> parseXML_Description <*> parseXML_Description <*> parseXML_List_Expense <*> parseXML_Int <*> parseXML_List_Currency<* endTag "Form"
parseXML_Expense = parseXMLCns_Expense <|> parseHoleAndParseErr "Expense" HoleExpense
parseXMLCns_Expense = Expense <$ startTag "Expense" <*> parseXML_Description <*> parseXML_Float_ <*> parseXML_Int<* endTag "Expense"
parseXML_Currency = parseXMLCns_Currency <|> parseHoleAndParseErr "Currency" HoleCurrency
parseXMLCns_Currency = Currency <$ startTag "Currency" <*> parseXML_Description <*> parseXML_Float_<* endTag "Currency"
parseXML_Tasks = parseXMLCns_Tasks <|> parseHoleAndParseErr "Tasks" HoleTasks
parseXMLCns_Tasks = Tasks <$ startTag "Tasks" <*> parseXML_List_Thing <*> parseXML_List_Thing <*> parseXML_Bool <*> parseXML_List_Task<* endTag "Tasks"
parseXML_Thing = parseXMLCns_Thing <|> parseHoleAndParseErr "Thing" HoleThing
parseXMLCns_Thing = Thing <$ startTag "Thing" <*> parseXML_Int<* endTag "Thing"
parseXML_Task = parseXMLCns_BasicTask <|> parseXMLCns_CompositeTask <|> parseHoleAndParseErr "Task" HoleTask
parseXMLCns_BasicTask = BasicTask <$ startTag "BasicTask" <*> parseXML_Description <*> parseXML_Bool<* endTag "BasicTask"
parseXMLCns_CompositeTask = CompositeTask <$ startTag "CompositeTask" <*> parseXML_Bool <*> parseXML_Description <*> parseXML_List_Task<* endTag "CompositeTask"
parseXML_Description = parseXMLCns_Description <|> parseHoleAndParseErr "Description" HoleDescription
parseXMLCns_Description = Description <$ startTag "Description" <*> parseXML_String<* endTag "Description"
parseXML_Sudoku = parseXMLCns_Sudoku <|> parseHoleAndParseErr "Sudoku" HoleSudoku
parseXMLCns_Sudoku = Sudoku <$ startTag "Sudoku" <*> parseXML_Row <*> parseXML_Row <*> parseXML_Row <*> parseXML_Row <*> parseXML_Row <*> parseXML_Row <*> parseXML_Row <*> parseXML_Row <*> parseXML_Row<* endTag "Sudoku"
parseXML_Row = parseXMLCns_Row <|> parseHoleAndParseErr "Row" HoleRow
parseXMLCns_Row = Row <$ startTag "Row" <*> parseXML_Field <*> parseXML_Field <*> parseXML_Field <*> parseXML_Field <*> parseXML_Field <*> parseXML_Field <*> parseXML_Field <*> parseXML_Field <*> parseXML_Field<* endTag "Row"
parseXML_Field = parseXMLCns_Field <|> parseHoleAndParseErr "Field" HoleField
parseXMLCns_Field = Field <$ startTag "Field" <*> parseXML_Int_<* endTag "Field"
parseXML_Test = parseXMLCns_Test <|> parseHoleAndParseErr "Test" HoleTest
parseXMLCns_Test = Test <$ startTag "Test" <*> parseXML_StyledText<* endTag "Test"
parseXML_StyledText = parseXMLCns_StyledText <|> parseHoleAndParseErr "StyledText" HoleStyledText
parseXMLCns_StyledText = StyledText <$ startTag "StyledText" <*> parseXML_List_Word<* endTag "StyledText"
parseXML_Word = parseXMLCns_Word <|> parseHoleAndParseErr "Word" HoleWord
parseXMLCns_Word = Word <$ startTag "Word" <*> parseXML_List_WordPart<* endTag "Word"
parseXML_WordPart = parseXMLCns_WordPart <|> parseXMLCns_OpenTag <|> parseXMLCns_CloseTag <|> parseHoleAndParseErr "WordPart" HoleWordPart
parseXMLCns_WordPart = WordPart NoIDP <$ startTag "WordPart" <*> parseXML_String<* endTag "WordPart"
parseXMLCns_OpenTag = OpenTag <$ startTag "OpenTag" <*> parseXML_TextStyle<* endTag "OpenTag"
parseXMLCns_CloseTag = CloseTag <$ startTag "CloseTag" <*> parseXML_TextStyle<* endTag "CloseTag"
parseXML_TextStyle = parseXMLCns_TextBold <|> parseXMLCns_TextItalic <|> parseXMLCns_TextFontSize <|> parseXMLCns_TextColor <|> parseHoleAndParseErr "TextStyle" HoleTextStyle
parseXMLCns_TextBold = TextBold <$ emptyTag "TextBold"
parseXMLCns_TextItalic = TextItalic <$ emptyTag "TextItalic"
parseXMLCns_TextFontSize = TextFontSize <$ startTag "TextFontSize" <*> parseXML_Int<* endTag "TextFontSize"
parseXMLCns_TextColor = TextColor <$ startTag "TextColor" <*> parseXML_Int <*> parseXML_Int <*> parseXML_Int<* endTag "TextColor"
parseXML_Int_ = parseXMLCns_Int_ <|> parseHoleAndParseErr "Int_" HoleInt_
parseXMLCns_Int_ = Int_ <$ startTag "Int_" <*> parseXML_Int<* endTag "Int_"
parseXML_Float_ = parseXMLCns_Float_ <|> parseHoleAndParseErr "Float_" HoleFloat_
parseXMLCns_Float_ = Float_ <$ startTag "Float_" <*> parseXML_Float<* endTag "Float_"
parseXML_List_Expense = mkList List_Expense Cons_Expense Nil_Expense <$> pList_ng parseXML_Expense
parseXML_List_Currency = mkList List_Currency Cons_Currency Nil_Currency <$> pList_ng parseXML_Currency
parseXML_List_Thing = mkList List_Thing Cons_Thing Nil_Thing <$> pList_ng parseXML_Thing
parseXML_List_Task = mkList List_Task Cons_Task Nil_Task <$> pList_ng parseXML_Task
parseXML_List_Word = mkList List_Word Cons_Word Nil_Word <$> pList_ng parseXML_Word
parseXML_List_WordPart = mkList List_WordPart Cons_WordPart Nil_WordPart <$> pList_ng parseXML_WordPart



--------------------------------------------------------------------------
-- List utility functions                                               --
--------------------------------------------------------------------------

toList_Expense vs = List_Expense (toConsList_Expense vs)

fromList_Expense (List_Expense vs) = fromConsList_Expense vs
fromList_Expense _ = []

toConsList_Expense [] = Nil_Expense
toConsList_Expense (x:xs) = Cons_Expense x (toConsList_Expense xs)

fromConsList_Expense Nil_Expense = []
fromConsList_Expense (Cons_Expense x xs) = x: fromConsList_Expense xs

replaceList_Expense _ x Nil_Expense = Nil_Expense  -- replace beyond end of list
replaceList_Expense 0 x (Cons_Expense cx cxs) = Cons_Expense x cxs
replaceList_Expense n x (Cons_Expense cx cxs) = Cons_Expense cx (replaceList_Expense (n-1) x cxs)

insertList_Expense 0 x cxs = Cons_Expense x cxs
insertList_Expense _ x Nil_Expense  = Nil_Expense  -- insert beyond end of list
insertList_Expense n x (Cons_Expense cx cxs) = Cons_Expense cx (insertList_Expense (n-1) x cxs)

removeList_Expense _ Nil_Expense  = Nil_Expense  -- remove beyond end of list
removeList_Expense 0 (Cons_Expense cx cxs) = cxs
removeList_Expense n (Cons_Expense cx cxs) = Cons_Expense cx (removeList_Expense (n-1) cxs)

toList_Currency vs = List_Currency (toConsList_Currency vs)

fromList_Currency (List_Currency vs) = fromConsList_Currency vs
fromList_Currency _ = []

toConsList_Currency [] = Nil_Currency
toConsList_Currency (x:xs) = Cons_Currency x (toConsList_Currency xs)

fromConsList_Currency Nil_Currency = []
fromConsList_Currency (Cons_Currency x xs) = x: fromConsList_Currency xs

replaceList_Currency _ x Nil_Currency = Nil_Currency  -- replace beyond end of list
replaceList_Currency 0 x (Cons_Currency cx cxs) = Cons_Currency x cxs
replaceList_Currency n x (Cons_Currency cx cxs) = Cons_Currency cx (replaceList_Currency (n-1) x cxs)

insertList_Currency 0 x cxs = Cons_Currency x cxs
insertList_Currency _ x Nil_Currency  = Nil_Currency  -- insert beyond end of list
insertList_Currency n x (Cons_Currency cx cxs) = Cons_Currency cx (insertList_Currency (n-1) x cxs)

removeList_Currency _ Nil_Currency  = Nil_Currency  -- remove beyond end of list
removeList_Currency 0 (Cons_Currency cx cxs) = cxs
removeList_Currency n (Cons_Currency cx cxs) = Cons_Currency cx (removeList_Currency (n-1) cxs)

toList_Thing vs = List_Thing (toConsList_Thing vs)

fromList_Thing (List_Thing vs) = fromConsList_Thing vs
fromList_Thing _ = []

toConsList_Thing [] = Nil_Thing
toConsList_Thing (x:xs) = Cons_Thing x (toConsList_Thing xs)

fromConsList_Thing Nil_Thing = []
fromConsList_Thing (Cons_Thing x xs) = x: fromConsList_Thing xs

replaceList_Thing _ x Nil_Thing = Nil_Thing  -- replace beyond end of list
replaceList_Thing 0 x (Cons_Thing cx cxs) = Cons_Thing x cxs
replaceList_Thing n x (Cons_Thing cx cxs) = Cons_Thing cx (replaceList_Thing (n-1) x cxs)

insertList_Thing 0 x cxs = Cons_Thing x cxs
insertList_Thing _ x Nil_Thing  = Nil_Thing  -- insert beyond end of list
insertList_Thing n x (Cons_Thing cx cxs) = Cons_Thing cx (insertList_Thing (n-1) x cxs)

removeList_Thing _ Nil_Thing  = Nil_Thing  -- remove beyond end of list
removeList_Thing 0 (Cons_Thing cx cxs) = cxs
removeList_Thing n (Cons_Thing cx cxs) = Cons_Thing cx (removeList_Thing (n-1) cxs)

toList_Task vs = List_Task (toConsList_Task vs)

fromList_Task (List_Task vs) = fromConsList_Task vs
fromList_Task _ = []

toConsList_Task [] = Nil_Task
toConsList_Task (x:xs) = Cons_Task x (toConsList_Task xs)

fromConsList_Task Nil_Task = []
fromConsList_Task (Cons_Task x xs) = x: fromConsList_Task xs

replaceList_Task _ x Nil_Task = Nil_Task  -- replace beyond end of list
replaceList_Task 0 x (Cons_Task cx cxs) = Cons_Task x cxs
replaceList_Task n x (Cons_Task cx cxs) = Cons_Task cx (replaceList_Task (n-1) x cxs)

insertList_Task 0 x cxs = Cons_Task x cxs
insertList_Task _ x Nil_Task  = Nil_Task  -- insert beyond end of list
insertList_Task n x (Cons_Task cx cxs) = Cons_Task cx (insertList_Task (n-1) x cxs)

removeList_Task _ Nil_Task  = Nil_Task  -- remove beyond end of list
removeList_Task 0 (Cons_Task cx cxs) = cxs
removeList_Task n (Cons_Task cx cxs) = Cons_Task cx (removeList_Task (n-1) cxs)

toList_Word vs = List_Word (toConsList_Word vs)

fromList_Word (List_Word vs) = fromConsList_Word vs
fromList_Word _ = []

toConsList_Word [] = Nil_Word
toConsList_Word (x:xs) = Cons_Word x (toConsList_Word xs)

fromConsList_Word Nil_Word = []
fromConsList_Word (Cons_Word x xs) = x: fromConsList_Word xs

replaceList_Word _ x Nil_Word = Nil_Word  -- replace beyond end of list
replaceList_Word 0 x (Cons_Word cx cxs) = Cons_Word x cxs
replaceList_Word n x (Cons_Word cx cxs) = Cons_Word cx (replaceList_Word (n-1) x cxs)

insertList_Word 0 x cxs = Cons_Word x cxs
insertList_Word _ x Nil_Word  = Nil_Word  -- insert beyond end of list
insertList_Word n x (Cons_Word cx cxs) = Cons_Word cx (insertList_Word (n-1) x cxs)

removeList_Word _ Nil_Word  = Nil_Word  -- remove beyond end of list
removeList_Word 0 (Cons_Word cx cxs) = cxs
removeList_Word n (Cons_Word cx cxs) = Cons_Word cx (removeList_Word (n-1) cxs)

toList_WordPart vs = List_WordPart (toConsList_WordPart vs)

fromList_WordPart (List_WordPart vs) = fromConsList_WordPart vs
fromList_WordPart _ = []

toConsList_WordPart [] = Nil_WordPart
toConsList_WordPart (x:xs) = Cons_WordPart x (toConsList_WordPart xs)

fromConsList_WordPart Nil_WordPart = []
fromConsList_WordPart (Cons_WordPart x xs) = x: fromConsList_WordPart xs

replaceList_WordPart _ x Nil_WordPart = Nil_WordPart  -- replace beyond end of list
replaceList_WordPart 0 x (Cons_WordPart cx cxs) = Cons_WordPart x cxs
replaceList_WordPart n x (Cons_WordPart cx cxs) = Cons_WordPart cx (replaceList_WordPart (n-1) x cxs)

insertList_WordPart 0 x cxs = Cons_WordPart x cxs
insertList_WordPart _ x Nil_WordPart  = Nil_WordPart  -- insert beyond end of list
insertList_WordPart n x (Cons_WordPart cx cxs) = Cons_WordPart cx (insertList_WordPart (n-1) x cxs)

removeList_WordPart _ Nil_WordPart  = Nil_WordPart  -- remove beyond end of list
removeList_WordPart 0 (Cons_WordPart cx cxs) = cxs
removeList_WordPart n (Cons_WordPart cx cxs) = Cons_WordPart cx (removeList_WordPart (n-1) cxs)




--------------------------------------------------------------------------
-- Miscellaneous                                                        --
--------------------------------------------------------------------------

type Presentation_ = Presentation Document EnrichedDoc Node ClipDoc UserToken

instance Doc Document where
  initialDoc = initialDocument
  toXML = toXMLDocument
  parseXML = parseXML_Document <* pCharSpaces

instance Eq Node where
  nd1 == nd2 = rankNode nd1 == rankNode nd2

instance Ord Node where
  nd1 <= nd2 = rankNode nd1 <= rankNode nd2


-- toXML for primitive types

toXMLInt i = EmptyElt "Integer" [("val", show i)]

toXMLFloat f = EmptyElt "Float" [("val", show f)]

toXMLBool b = EmptyElt "Bool" [("val", show b)]

toXMLString str = Elt "String" [] [PCData str] 


-- parseXML for primitive types

parseXML_Int :: CharParser Int
parseXML_Int  =
      read 
  <$  pCharSpaces
  <*  pCharString "<Integer val=\""
  <*> pList (pExcept ('\0','\255','x') "\"") 
  <*  pCharString "\"/>"

parseXML_Float :: CharParser Float
parseXML_Float  =
      read 
  <$  pCharSpaces
  <*  pCharString "<Float val=\""
  <*> pList (pExcept ('\0','\255','x') "\"") 
  <*  pCharString "\"/>"

parseXML_Bool :: CharParser Bool
parseXML_Bool  =
      read 
  <$  pCharSpaces
  <*  pCharString "<Bool val=\""
  <*> pList (pExcept ('\0','\255','x') "\"") 
  <*  pCharString "\"/>"

parseXML_String :: CharParser String
parseXML_String  =
      id
  <$  pCharSpaces
  <*  pCharString "<String>"
  <*> pList (pExcept ('\0','\255','x') "<") 
  <*  pCharString "</String>"
 

-- Xprez XML presentation for primitive types

presentPrimXMLInt :: Int -> Presentation_
presentPrimXMLInt x = text $ "<Int>"++show x++"<Int/>"

presentPrimXMLFloat :: Float -> Presentation_
presentPrimXMLFloat x = text $ "<Float>"++show x++"<Float>"

presentPrimXMLBool :: Bool -> Presentation_
presentPrimXMLBool x = text $ "<Bool>"++show x++"<Bool/>"

presentPrimXMLString :: String -> Presentation_
presentPrimXMLString x = text $ "<String>"++x++"<String>"


-- Xprez tree presentation for primitive types

presentPrimTreeInt :: Int -> Presentation_
presentPrimTreeInt x =  mkTreeLeaf False $ text $ "Int: "++show x

presentPrimTreeFloat :: Float -> Presentation_
presentPrimTreeFloat x =  mkTreeLeaf False $ text $ "Float: "++show x

presentPrimTreeBool :: Bool -> Presentation_
presentPrimTreeBool x =  mkTreeLeaf False $ text $ "Bool: "++show x

presentPrimTreeString :: String -> Presentation_
presentPrimTreeString x =  mkTreeLeaf False $ text $ "String: "++x


