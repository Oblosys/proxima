module RenTypes where

import Graphics.UI.WX hiding (Size, Modifiers)
import Graphics.UI.WXCore (DC)

import CommonTypes
import CommonUtils

import DocTypes

data RenderingLevel documentLevel = RenderingLevel Scale (GUICommand documentLevel) Rendering Size Debugging UpdatedRegions

type LocalStateRen = ()

data EditRendering' documentLevel =
    SetRen' (RenderingLevel documentLevel)
  | SkipRen' Int deriving Show

data EditRendering documentLevel =
    SkipRen Int
  | InitRen
  | CloseRen
  | KeyCharRen Char 
  | KeySpecialRen SpecialKey Modifiers
  | MouseDownRen Int Int Modifiers Int -- x y modifiers nrOfClicks
  | MouseDragRen Int Int Modifiers
  | MouseUpRen Int Int Modifiers
  | UpdateDocRen (documentLevel -> documentLevel)   -- TODO: don't really want this doc ref in rendering level
  | OpenFileRen String
  | SaveFileRen String
  | DocumentLoadedRen String deriving Show


instance Show (RenderingLevel documentLevel) where
  show (RenderingLevel scale _ _ size debugging updRegions) =
       "RenderingLevel {Scale: "++show scale++"} {GUICommand} {Rendering} "++show size++" {debug:" ++ show debugging ++ "}\n"
    ++ "               {updated regions:"++show updRegions++"}"

-- GUICommand is to specify GUI commands like menu creation etc. This cannot be done in the repaint draw monad.

type Scale = Double
type GUICommand documentLevel = ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) ->
                  Var (RenderingLevel documentLevel) -> ScrolledWindow () -> 
                                Int -> Int -> IO ()
--type GUICommand = Id -> ((RenderingLevel, EditRendering) -> IO (RenderingLevel, EditRendering')) ->
--                  Int -> Int -> GUI RenderingLevel ()
type Rendering = DC () -> IO ()
--type Rendering = Draw ()
type Debugging = Bool
type UpdatedRegions = (Rect    , Rect, Rect    )
type Size = (Int, Int)
--                    (current focus, old focus, edited region)
-- only for flickering reduction, until we have double buffering

emptyR :: Rendering
emptyR dc = return ()

origin :: (Int, Int)
origin = (0,0)
