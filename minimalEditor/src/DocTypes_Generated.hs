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

data EnrichedDoc = RootEnr List_Tree List_Tree
                 | HoleEnrichedDoc
                 | ParseErrEnrichedDoc (ParseError Document Node ClipDoc UserToken)
                     deriving (Show, Data, Typeable)

data Document = RootDoc List_Tree List_Tree
              | HoleDocument
              | ParseErrDocument (ParseError Document Node ClipDoc UserToken)
                  deriving (Show, Data, Typeable)

data Tree = Bin IDP IDP IDP IDP IDP Tree Tree
          | Leaf IDP IDP Int
          | HoleTree
          | ParseErrTree (ParseError Document Node ClipDoc UserToken)
              deriving (Show, Data, Typeable)

data List_Tree = List_Tree ConsList_Tree
               | HoleList_Tree
               | ParseErrList_Tree (ParseError Document Node ClipDoc UserToken)
                   deriving (Show, Data, Typeable)

data ConsList_Tree = Cons_Tree Tree ConsList_Tree
                   | Nil_Tree
                       deriving (Show, Data, Typeable)




--------------------------------------------------------------------------
-- ClipDoc                                                              --
--------------------------------------------------------------------------

data ClipDoc = Clip_EnrichedDoc EnrichedDoc
             | Clip_Document Document
             | Clip_Tree Tree
             | Clip_List_Tree List_Tree
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
          | Node_Bin Tree Path
          | Node_Leaf Tree Path
          | Node_HoleTree Tree Path
          | Node_ParseErrTree Tree Path
          | Node_List_Tree List_Tree Path
          | Node_HoleList_Tree List_Tree Path
          | Node_ParseErrList_Tree List_Tree Path
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
  show (Node_Bin _ _) = "Node_Bin" 
  show (Node_Leaf _ _) = "Node_Leaf" 
  show (Node_HoleTree _ _) = "Node_HoleTree" 
  show (Node_ParseErrTree _ _) = "Node_ParseErrTree" 
  show (Node_List_Tree _ _) = "Node_List_Tree" 
  show (Node_HoleList_Tree _ _) = "Node_HoleList_Tree" 
  show (Node_ParseErrList_Tree _ _) = "Node_ParseErrList_Tree" 


