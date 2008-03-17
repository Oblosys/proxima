module DocTypes_Generated where

import Common.CommonTypes hiding (Dirty (..))
import Evaluation.DocTypes

import Presentation.PresTypes
import List
import Char

data UserToken = BinToken | LeafToken | SymToken String deriving (Show, Eq, Ord)

----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- Proxima data type                                                    --
--------------------------------------------------------------------------

data EnrichedDoc = RootEnr Tree
                 | HoleEnrichedDoc
                 | ParseErrEnrichedDoc (ParseError Document Node ClipDoc UserToken)
                     deriving Show

data Document = RootDoc Tree
              | HoleDocument
              | ParseErrDocument (ParseError Document Node ClipDoc UserToken)
                  deriving Show

data Tree = Bin Tree Tree
          | Leaf
          | HoleTree
          | ParseErrTree (ParseError Document Node ClipDoc UserToken)
              deriving Show




--------------------------------------------------------------------------
-- ClipDoc                                                              --
--------------------------------------------------------------------------

data ClipDoc = Clip_EnrichedDoc EnrichedDoc
             | Clip_Document Document
             | Clip_Tree Tree
             | Clip_Bool Bool
             | Clip_Int Int
             | Clip_String String
             | Clip_Float Float
             | Clip_Nothing deriving Show



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
          | Node_Bin Tree Path
          | Node_Leaf Tree Path
          | Node_HoleTree Tree Path
          | Node_ParseErrTree Tree Path



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
  show (Node_Bin _ _) = "Node_Bin" 
  show (Node_Leaf _ _) = "Node_Leaf" 
  show (Node_HoleTree _ _) = "Node_HoleTree" 
  show (Node_ParseErrTree _ _) = "Node_ParseErrTree" 


