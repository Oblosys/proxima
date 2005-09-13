module LayTypes (module PresTypes, module LayTypes) where

import CommonTypes
import PresTypes

import CommonUtils


import DocTypes -- for UpdateDocLay
-- Layout imports and exports PresTypes because the level type is the same for both
-- levels. A separate module imported by both PresTypes and LayTypes would be a bit
-- cleaner, as Presentation specific definitions (eg. PresentationLevel) are visible at
-- LayoutLevel. This is not really a problem, however.

data LayoutLevel = LayoutLevel Presentation FocusPres DiffTree deriving Show
                                                    -- DiffTree is experimental for incrementality
data EditLayout' =
    SetLay' LayoutLevel
  | SkipLay' Int deriving Show

data EditLayout =
    SkipLay Int
  | SetFocusLay FocusPres
  | SetLay LayoutLevel
  | InitLay
  | CloseLay
  | InsertLay Char
  | CutLay
  | CopyLay
  | PasteLay
  | DeleteLay
  | SplitLay
  | LeftDeleteLay
  | RightDeleteLay
  | LeftLay
  | RightLay
  | EnlargeLeftLay
  | EnlargeRightLay
  | MouseDownLay PathPres Modifiers Int
  | NormalizeLay
  | DocumentLoadedLay String 
  | OpenFileLay String
  | SaveFileLay String
  | TestLay
  | Test2Lay
  
  | UpdateDocLay (DocumentLevel -> DocumentLevel) -- should encapsulate these so they automatically go to doc level
  | NavUpDocLay
  | NavDownDocLay
  | NavLeftDocLay
  | NavRightDocLay
  | CutDocLay
  | CopyDocLay
  | PasteDocLay
  | DeleteDocLay deriving Show


-- No special type Layout. Instead, Presentation is used.