module DocTypes where


import CommonTypes

import List
import Char
import Text.ParserCombinators.Parsec

data PathDoc = PathD [Int]
             | NoPathD deriving (Show, Eq, Ord)

type FocusDoc = PathDoc  -- just a simple path focus for now

type PathD = [Int] -- we don't use PathDoc because NoPath makes algorithms unreadable

data DocumentLevel doc clip = DocumentLevel doc FocusDoc clip

class HasPath node where
  pathNode :: node -> PathDoc

class Show doc => Doc doc where
  initialDoc :: IO doc
  toXML :: doc -> XML
  parseXML :: Parser doc
  
data EditDocument' doc clip =
    SetDoc' (DocumentLevel doc clip)
  | UpdateDoc' (DocumentLevel doc clip -> DocumentLevel doc clip)
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

data EditDocument doc clip =
    InitDoc
  | CloseDoc
  | SetDoc (DocumentLevel doc clip)
  | UpdateDoc (DocumentLevel doc clip -> DocumentLevel doc clip)
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

instance Show (EditDocument doc clip) where
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


instance Show (EditDocument' doc clip) where
  show (SetDoc' doc )    = "(SetDoc' {Document})"
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



