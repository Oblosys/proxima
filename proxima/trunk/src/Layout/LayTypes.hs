module Layout.LayTypes (module Presentation.PresTypes, module Layout.LayTypes) where

import Common.CommonTypes
import Presentation.PresTypes

import Common.CommonUtils


import Evaluation.DocTypes -- for UpdateDocLay
-- Layout imports and exports PresTypes because the level type is the same for both
-- levels. A separate module imported by both PresTypes and LayTypes would be a bit
-- cleaner, as Presentation specific definitions (eg. PresentationLevel) are visible at
-- LayoutLevel. This is not really a problem, however.

data LayoutLevel doc node clip token = 
  LayoutLevel (Layout doc node clip token) FocusPres DiffTree deriving Show
                                                    -- DiffTree is experimental for incrementality
data EditLayout'_ wrapped doc enr node clip token =
    SetLay' (LayoutLevel doc node clip token)
  | SkipLay' Int 
  | WrapLay' wrapped deriving Show

data EditLayout_ wrapped doc enr node clip token =
    SkipLay Int
  | SetFocusLay FocusPres
  | SetLay (LayoutLevel doc node clip token)
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
  | AddVertexLay [Int] (Int,Int) -- path to the graph and destination position for new vertex
  | AddEdgeLay   [Int] -- path to to-vertex for new edge (from-vertex is assumed to be in focus)
  | MoveVertexLay [Int] (Int,Int) -- presentation path to the vertex
  | NormalizeLay
  | OpenFileLay String
  | SaveFileLay String
  | ParseLay
  | Test2Lay
  
  | WrapLay wrapped deriving Show


data Layout_

type Layout doc node clip token = PresentationBase doc node clip token Layout_

-- this guarantees that values containing a TokenP are not of type Layout (see
-- definition of Presentation.PresTypes.PresentationBase for more information.

type LayoutList doc node clip token = [Layout doc node clip token]
