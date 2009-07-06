module DocTypes_Generated where

import Common.CommonTypes hiding (Dirty (..))
import Evaluation.DocTypes

import Presentation.PresTypes
import List
import Char
import Data.Generics

data UserToken = BinToken | LeafToken | IntToken | SymToken String deriving (Show, Eq, Ord, Typeable)

----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- Proxima data type                                                    --
--------------------------------------------------------------------------

data EnrichedDoc = RootEnr Form
                 | HoleEnrichedDoc
                 | ParseErrEnrichedDoc (ParseError Document Node ClipDoc UserToken)
                     deriving (Show, Data, Typeable)

data Document = RootDoc Form
              | HoleDocument
              | ParseErrDocument (ParseError Document Node ClipDoc UserToken)
                  deriving (Show, Data, Typeable)

data Form = Form String String List_Expense List_Currency
          | HoleForm
          | ParseErrForm (ParseError Document Node ClipDoc UserToken)
              deriving (Show, Data, Typeable)

data Expense = Expense String Float Int
             | HoleExpense
             | ParseErrExpense (ParseError Document Node ClipDoc UserToken)
                 deriving (Show, Data, Typeable)

data Currency = Currency String Float
              | HoleCurrency
              | ParseErrCurrency (ParseError Document Node ClipDoc UserToken)
                  deriving (Show, Data, Typeable)

data List_Expense = List_Expense ConsList_Expense
                  | HoleList_Expense
                  | ParseErrList_Expense (ParseError Document Node ClipDoc UserToken)
                      deriving (Show, Data, Typeable)

data List_Currency = List_Currency ConsList_Currency
                   | HoleList_Currency
                   | ParseErrList_Currency (ParseError Document Node ClipDoc UserToken)
                       deriving (Show, Data, Typeable)

data ConsList_Expense = Cons_Expense Expense ConsList_Expense
                      | Nil_Expense
                          deriving (Show, Data, Typeable)

data ConsList_Currency = Cons_Currency Currency ConsList_Currency
                       | Nil_Currency
                           deriving (Show, Data, Typeable)




--------------------------------------------------------------------------
-- ClipDoc                                                              --
--------------------------------------------------------------------------

data ClipDoc = Clip_EnrichedDoc EnrichedDoc
             | Clip_Document Document
             | Clip_Form Form
             | Clip_Expense Expense
             | Clip_Currency Currency
             | Clip_List_Expense List_Expense
             | Clip_List_Currency List_Currency
             | Clip_Bool Bool
             | Clip_Int Int
             | Clip_String String
             | Clip_Float Float
             | Clip_Nothing deriving (Show, Typeable)



--------------------------------------------------------------------------
-- Node                                                                 --
--------------------------------------------------------------------------

data Node = NoNode
          | Node_RootEnr EnrichedDoc Path
          | Node_HoleEnrichedDoc EnrichedDoc Path
          | Node_ParseErrEnrichedDoc EnrichedDoc Path
          | Node_RootDoc Document Path
          | Node_HoleDocument Document Path
          | Node_ParseErrDocument Document Path
          | Node_Form Form Path
          | Node_HoleForm Form Path
          | Node_ParseErrForm Form Path
          | Node_Expense Expense Path
          | Node_HoleExpense Expense Path
          | Node_ParseErrExpense Expense Path
          | Node_Currency Currency Path
          | Node_HoleCurrency Currency Path
          | Node_ParseErrCurrency Currency Path
          | Node_List_Expense List_Expense Path
          | Node_HoleList_Expense List_Expense Path
          | Node_ParseErrList_Expense List_Expense Path
          | Node_List_Currency List_Currency Path
          | Node_HoleList_Currency List_Currency Path
          | Node_ParseErrList_Currency List_Currency Path
            deriving Typeable



--------------------------------------------------------------------------
-- Show instance for Node                                               --
--------------------------------------------------------------------------

instance Show Node where
  show NoNode = "NoNode"
  show (Node_RootEnr _ _) = "Node_RootEnr" 
  show (Node_HoleEnrichedDoc _ _) = "Node_HoleEnrichedDoc" 
  show (Node_ParseErrEnrichedDoc _ _) = "Node_ParseErrEnrichedDoc" 
  show (Node_RootDoc _ _) = "Node_RootDoc" 
  show (Node_HoleDocument _ _) = "Node_HoleDocument" 
  show (Node_ParseErrDocument _ _) = "Node_ParseErrDocument" 
  show (Node_Form _ _) = "Node_Form" 
  show (Node_HoleForm _ _) = "Node_HoleForm" 
  show (Node_ParseErrForm _ _) = "Node_ParseErrForm" 
  show (Node_Expense _ _) = "Node_Expense" 
  show (Node_HoleExpense _ _) = "Node_HoleExpense" 
  show (Node_ParseErrExpense _ _) = "Node_ParseErrExpense" 
  show (Node_Currency _ _) = "Node_Currency" 
  show (Node_HoleCurrency _ _) = "Node_HoleCurrency" 
  show (Node_ParseErrCurrency _ _) = "Node_ParseErrCurrency" 
  show (Node_List_Expense _ _) = "Node_List_Expense" 
  show (Node_HoleList_Expense _ _) = "Node_HoleList_Expense" 
  show (Node_ParseErrList_Expense _ _) = "Node_ParseErrList_Expense" 
  show (Node_List_Currency _ _) = "Node_List_Currency" 
  show (Node_HoleList_Currency _ _) = "Node_HoleList_Currency" 
  show (Node_ParseErrList_Currency _ _) = "Node_ParseErrList_Currency" 


