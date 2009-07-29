{-# LANGUAGE CPP #-}
module Rendering.RenTypes where

#ifdef SERVER
import Control.Monad.Writer
#else
import Graphics.UI.Gtk hiding (Scale, Size, Rectangle)
import Graphics.Rendering.Cairo
#endif

import Data.IORef

import Common.CommonTypes
import qualified Common.CommonTypes as CommonTypes
import Common.CommonUtils
import Evaluation.DocTypes

import Presentation.PresTypes (PopupMenuItem)

data RenderingLevel_ wrapped doc enr node clip token = 
       RenderingLevel Scale 
                      (GUICommand wrapped doc enr node clip token) -- used for popups
                      Rendering -- rendering
                      Rendering -- focus rendering
                      Size Debugging UpdatedRegions LeftButtonDown
                  
type LocalStateRen = ()
                                   

data EditRendering'_ wrapped doc enr node clip token =
    SetRen' (RenderingLevel_ wrapped doc enr node clip token)
  | SkipRen' Int
  | WrapRen' wrapped deriving Show

data EditRendering_ wrapped doc enr node clip token =
    SkipRen Int
  | InitRen
  | CloseRen
  | KeyCharRen Char 
  | KeySpecialRen SpecialKey Modifiers
  | MouseDownRen Int Int Modifiers Int -- x y modifiers nrOfClicks
  | MouseDragRen Int Int Modifiers
  | MouseUpRen Int Int Modifiers
  | DragStartRen Int Int
  | DropRen Int Int
  | OpenFileRen String
  | SaveFileRen String
  | WrapRen wrapped deriving Show


instance Show (RenderingLevel_ wrapped doc enr node clip token) where
  show (RenderingLevel scale _ _ _ size debugging updRegions leftButtonDown) =
       "RenderingLevel {Scale: "++show scale++"} {GUICommand} {Rendering} "++show size++" {debug:" ++ show debugging ++ "}\n"
    ++ "               {updated regions:"++show updRegions++"}{leftButtonDown:"++show leftButtonDown++"}\n"


type Scale = Double


#ifndef SERVER
type GUICommand wrapped doc enr node clip token = ((RenderingLevel_ wrapped doc enr node clip token, EditRendering_ wrapped doc enr node clip token) -> IO (RenderingLevel_ wrapped doc enr node clip token, [EditRendering'_ wrapped doc enr node clip token])) ->
                  IORef (RenderingLevel_ wrapped doc enr node clip token) -> IORef (Maybe Pixmap) -> IORef CommonTypes.Rectangle -> Window -> Viewport -> DrawingArea -> 
                                Int -> Int -> IO (Maybe Menu)
-- GUICommand is currently only used for popup menus


type Rendering = DrawableClass drawWindow => (Window, drawWindow, GC) -> (Point,Size) -> Render ()
                                                                      -- viewd area ((x,y),(w,h))

emptyGUICommand :: GUICommand wrapped doc enr node clip token
emptyGUICommand = (\_ _ _ _ _ _ _ x y -> return Nothing)

emptyRendering :: Rendering
emptyRendering = \dc va -> return ()

#else

type GUICommand wrapped doc enr node clip token = Int -> Int -> [PopupMenuItem doc clip]
                      
type Rendering = (Point,Size) -> Writer String ()
                 -- viewed area ((x,y),(w,h))

--
emptyGUICommand :: GUICommand wrapped doc enr node clip token
emptyGUICommand = (\_ _ -> [])

emptyRendering :: Rendering
emptyRendering = \va -> return ()

type Point = (Int, Int)

#endif

type Debugging = Bool
type Size = (Int, Int)

origin :: (Int, Int)
origin = (0,0)

type UpdatedRegions = [Rectangle]

type LeftButtonDown = Bool 
-- For distinguishing between mouse move and mouse drag events. It is updated by RenTranslate.
-- Currently, Proxima only handles left dragging.
