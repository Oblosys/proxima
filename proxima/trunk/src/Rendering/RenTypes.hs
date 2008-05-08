module Rendering.RenTypes where

import Graphics.UI.Gtk hiding (Scale, Size, Rectangle)
import Data.IORef

import Common.CommonTypes
import qualified Common.CommonTypes as CommonTypes
import Common.CommonUtils

import Evaluation.DocTypes

data RenderingLevel_ wrapped docLevel doc enr node clip token = 
       RenderingLevel Scale (GUICommand wrapped docLevel doc enr node clip token) 
                      Rendering -- rendering
                      Rendering -- focus rendering
                      Size Debugging UpdatedRegions LeftButtonDown
                  
type LocalStateRen = ()
                                   

data EditRendering'_ wrapped docLevel doc enr node clip token =
    SetRen' (RenderingLevel_ wrapped docLevel doc enr node clip token)
  | SkipRen' Int
  | WrapRen' wrapped deriving Show

data EditRendering_ wrapped docLevel doc enr node clip token =
    SkipRen Int
  | InitRen
  | CloseRen
  | KeyCharRen Char 
  | KeySpecialRen SpecialKey Modifiers
  | MouseDownRen Int Int Modifiers Int -- x y modifiers nrOfClicks
  | MouseDragRen Int Int Modifiers
  | MouseUpRen Int Int Modifiers
  | UpdateDocRen (docLevel -> docLevel)   -- TODO: don't really want this doc ref in rendering level
  | OpenFileRen String
  | SaveFileRen String
  | DocumentLoadedRen String
  | WrapRen wrapped deriving Show


instance Show (RenderingLevel_ wrapped docLevel doc enr node clip token) where
  show (RenderingLevel scale _ _ _ size debugging updRegions leftButtonDown) =
       "RenderingLevel {Scale: "++show scale++"} {GUICommand} {Rendering} "++show size++" {debug:" ++ show debugging ++ "}\n"
    ++ "               {updated regions:"++show updRegions++"}{leftButtonDown:"++show leftButtonDown++"}\n"


type Scale = Double
type GUICommand wrapped docLevel doc enr node clip token = ((RenderingLevel_ wrapped docLevel doc enr node clip token, EditRendering_ wrapped docLevel doc enr node clip token) -> IO (RenderingLevel_ wrapped docLevel doc enr node clip token, EditRendering'_ wrapped docLevel doc enr node clip token)) ->
                  IORef (RenderingLevel_ wrapped docLevel doc enr node clip token) -> IORef (Maybe Pixmap) -> IORef CommonTypes.Rectangle -> Window -> Viewport -> DrawingArea -> 
                                Int -> Int -> IO (Maybe Menu)
-- GUICommand is currently only used for popup menus

type Rendering = DrawableClass drawWindow => (Window, drawWindow, GC) -> (Point,Size) -> IO ()
                                                                      -- viewd area ((x,y),(w,h))
type Debugging = Bool
type Size = (Int, Int)

emptyR :: Rendering
emptyR dc va = return ()

origin :: (Int, Int)
origin = (0,0)

type UpdatedRegions = [Rectangle]

type LeftButtonDown = Bool 
-- For distinguishing between mouse move and mouse drag events. It is updated by RenTranslate.
-- Currently, Proxima only handles left dragging.
