module Evaluation.DocTypes where


import Common.CommonTypes

import List
import Char
import UU.Parsing.CharParser

type FocusDoc = PathDoc  -- just a simple path focus for now

data DocumentLevel doc clip = DocumentLevel doc FocusDoc clip

class Show doc => Doc doc where
  initialDoc :: IO doc
  toXML :: doc -> XML
  parseXML :: CharParser doc
  
data EditDocument' doc clip =
    SetDoc' (DocumentLevel doc clip)
  | UndoDoc'
  | RedoDoc'
  | UpdateDoc' (DocumentLevel doc clip -> DocumentLevel doc clip)
  | NavPathDoc' PathDoc
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
  | UndoDoc
  | RedoDoc
  | UpdateDoc (DocumentLevel doc clip -> DocumentLevel doc clip)
  | NavPathDoc PathDoc
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
  show UndoDoc         = "UndoDoc"
  show RedoDoc         = "RedoDoc"
  show (UpdateDoc upd) = "(UpdateDoc <function>)"
  show (NavPathDoc path) = "NavPathDoc"++show path
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
  show UndoDoc'         = "UndoDoc'"
  show RedoDoc'         = "RedoDoc'"
  show (UpdateDoc' upd) = "(UpdateDoc' <function>)"
  show (NavPathDoc' path) = "NavPathDoc'"++show path
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
