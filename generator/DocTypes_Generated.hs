module DocTypes_Generated where

import Common.CommonTypes hiding (Dirty (..))
import Evaluation.DocTypes

import Presentation.PresTypes
import List
import Char


data Document = RootDoc Root 
              | HoleDocument
              | ParseErrDocument (Presentation Document Node ClipDoc UserToken)
                 deriving Show

data UserToken = KeyTk String
               | WordTk 
               | NodeRefTk
               | LabelTk
               | LabelRefTk deriving (Show, Eq, Ord)


----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----
--------------------------------------------------------------------------------
-- Proxima data type                                                          --
--------------------------------------------------------------------------------
data Bla = Bloe IDP Int Int
         | HoleBla
         | ParseErr_Bla (Presentation Document Node ClipDoc UserToken)
             deriving Show



--------------------------------------------------------------------------------
-- ClipDoc                                                                    --
--------------------------------------------------------------------------------
data ClipDoc = Clip_Bla Bla deriving Show


--------------------------------------------------------------------------------
-- Node                                                                       --
--------------------------------------------------------------------------------
data Node = NoNode
          | BloeNode Bla


--------------------------------------------------------------------------------
-- Show instance for Node                                                     --
--------------------------------------------------------------------------------
instance Show Node where
  show NoNode = "NoNode"
  show (Bloe _ _ _) = "Bloe" 


