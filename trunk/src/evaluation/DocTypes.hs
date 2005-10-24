module DocTypes ( module DocTypes, FocusDoc, PathDoc (..)) where

import DocTypes_Generated (PathDoc, FocusDoc, ClipDoc)

import CommonTypes

import PresTypes
import List
import Char


data DocumentLevel document = DocumentLevel document FocusDoc ClipDoc

--class HasPath node where
--  pathNode :: node -> PathDoc

{-
data PathDoc = PathD [Int]
             | NoPathD deriving (Show, Eq, Ord)

type FocusDoc = PathDoc  -- just a simple path focus for now

type PathD = [Int] -- we don't use PathDoc because NoPath makes algorithms unreadable



data HeliumMessage =
         HMessage [String] 
       | HError [String] [PathDoc] [PathDoc] [PathDoc] deriving Show
-}
-- PathDoc FocusDoc PathD and HeliumMessage need to be in DocumentTypes_Generated
-- due to stupid acyclic module restriction. Recursive import support is incomprehensible
-- and buggy

data EditDocument' documentLevel doc = -- Document in SetDoc' should be a DocumentLevel!
    SetDoc' doc -- (InsertedTokenList, DeletedTokenMap)
  | UpdateDoc' (documentLevel -> documentLevel)
  | NavUpDoc'
  | NavDownDoc'
  | NavLeftDoc'
  | NavRightDoc'
  | CutDoc'
  | CopyDoc'
   | PasteDoc'
  | DeleteDoc'
  | EvaluateDoc' -- for type evaluation
  | SkipDoc' Int

data EditDocument documentLevel doc =
    InitDoc
  | CloseDoc
  | SetDoc doc -- (InsertedTokenList, DeletedTokenMap)
  | UpdateDoc (documentLevel -> documentLevel)
  | NavUpDoc
  | NavDownDoc
  | NavLeftDoc
  | NavRightDoc
  | CutDoc
  | CopyDoc
  | PasteDoc
  | DeleteDoc
  | EvaluateDoc -- for type evaluation
  | SkipDoc Int

instance Show (EditDocument documentLevel doc) where
  show InitDoc         = "InitDoc" 
  show CloseDoc        = "CloseDoc"
  show (SetDoc doc )    = "(SetDoc {Document} {inserted&deleted} )"
  show (UpdateDoc upd) = "(UpdateDoc <function>)"
  show NavUpDoc        = "NavUpDoc"
  show NavDownDoc      = "NavDownDoc"
  show NavLeftDoc      = "NavLeftDoc"
  show NavRightDoc     = "NavRightDoc"
  show CutDoc          = "CutDoc"
  show CopyDoc         = "CopyDoc"
  show PasteDoc        = "PasteDoc"
  show DeleteDoc       = "DeleteDoc"
  show EvaluateDoc     = "EvaluateDoc"
  show (SkipDoc i)     = "(SkipDoc " ++ show i ++ ")"   


instance Show doc => Show (EditDocument' documentLevel doc) where
  show (SetDoc' doc )    = "(SetDoc' "++show doc++" {inserted&deleted} )"
  show (UpdateDoc' upd) = "(UpdateDoc' <function>)"
  show NavUpDoc'        = "NavUpDoc'"
  show NavDownDoc'      = "NavDownDoc'"
  show NavLeftDoc'      = "NavLeftDoc'"
  show NavRightDoc'     = "NavRightDoc'"
  show CutDoc'          = "CutDoc'"
  show CopyDoc'         = "CopyDoc'"
  show PasteDoc'        = "PasteDoc'"
  show DeleteDoc'       = "DeleteDoc'"
  show EvaluateDoc'     = "EvaluateDoc'"
  show (SkipDoc' i)     = "(SkipDoc' " ++ show i ++ ")"   
  
  




{-

Notes:

1^3 shows that we do want to parse columns. If a digit is inserted before the 1 a parse error occurs, whereas
it should be added before the 1. However, maybe this can be fixed by editing the string inside the column rather
than add before it.

Keeping original mappings at Layout level is hard. Maybe new ones will be generated every edit op.



-}



