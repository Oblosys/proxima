module DocTypes_Generated where

import Common.CommonTypes hiding (Dirty (..))
import Evaluation.DocTypes

import Presentation.PresTypes
import Data.List
import Data.Char
import Data.Generics

data UserToken = WordTk | KeyTk String | FloatTk | IntTk | SymTk String deriving (Show, Eq, Ord, Typeable)

----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- Proxima data type                                                    --
--------------------------------------------------------------------------

data EnrichedDoc = RootEnr ChoiceDoc
                 | HoleEnrichedDoc
                 | ParseErrEnrichedDoc (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                     deriving (Show, Data, Typeable)

data Document = RootDoc ChoiceDoc
              | HoleDocument
              | ParseErrDocument (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                  deriving (Show, Data, Typeable)

data ChoiceDoc = SudokuDoc Sudoku
               | HoleChoiceDoc
               | ParseErrChoiceDoc (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                   deriving (Show, Data, Typeable)

data Sudoku = Sudoku Float_ Fload_
            | HoleSudoku
            | ParseErrSudoku (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                deriving (Show, Data, Typeable)

data Float_ = Float_ Float
            | HoleFloat_
            | ParseErrFloat_ (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                deriving (Show, Data, Typeable)

data Fload_ = Fload_ Float
            | HoleFload_
            | ParseErrFload_ (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                deriving (Show, Data, Typeable)




--------------------------------------------------------------------------
-- ClipDoc                                                              --
--------------------------------------------------------------------------

data ClipDoc = Clip_EnrichedDoc EnrichedDoc
             | Clip_Document Document
             | Clip_ChoiceDoc ChoiceDoc
             | Clip_Sudoku Sudoku
             | Clip_Float_ Float_
             | Clip_Fload_ Fload_
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
          | Node_SudokuDoc ChoiceDoc Path
          | Node_HoleChoiceDoc ChoiceDoc Path
          | Node_ParseErrChoiceDoc ChoiceDoc Path
          | Node_Sudoku Sudoku Path
          | Node_HoleSudoku Sudoku Path
          | Node_ParseErrSudoku Sudoku Path
          | Node_Float_ Float_ Path
          | Node_HoleFloat_ Float_ Path
          | Node_ParseErrFloat_ Float_ Path
          | Node_Fload_ Fload_ Path
          | Node_HoleFload_ Fload_ Path
          | Node_ParseErrFload_ Fload_ Path
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
  show (Node_SudokuDoc _ _) = "Node_SudokuDoc" 
  show (Node_HoleChoiceDoc _ _) = "Node_HoleChoiceDoc" 
  show (Node_ParseErrChoiceDoc _ _) = "Node_ParseErrChoiceDoc" 
  show (Node_Sudoku _ _) = "Node_Sudoku" 
  show (Node_HoleSudoku _ _) = "Node_HoleSudoku" 
  show (Node_ParseErrSudoku _ _) = "Node_ParseErrSudoku" 
  show (Node_Float_ _ _) = "Node_Float_" 
  show (Node_HoleFloat_ _ _) = "Node_HoleFloat_" 
  show (Node_ParseErrFloat_ _ _) = "Node_ParseErrFloat_" 
  show (Node_Fload_ _ _) = "Node_Fload_" 
  show (Node_HoleFload_ _ _) = "Node_HoleFload_" 
  show (Node_ParseErrFload_ _ _) = "Node_ParseErrFload_" 


