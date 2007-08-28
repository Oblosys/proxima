module CommonTypes ( module Char 
                   , module List
                   , module DebugLevels
                   , module CommonTypes ) where

{- GHCI defs:
:def R (\_ -> return ":r")


-}
import DebugLevels 

import Char
import List
import Data.IORef
import System.IO.Unsafe

-- is not exported by all ...Types modules (like CommonUtils) because sometimes CommonTypes
-- (and only CommonTypes) must be imported qualified due to name clashes with libraries.

instance Show (a -> b) where
  show _ = "function: (a -> b)"


-- Necessary to create Locators with NoNode, without importing DocTypes.
-- Putting Node in a Maybe would also work, but causes more pattern matching.
class DocNode node where
  noNode :: node
  
type Path = [Int]

type Color = (Int,Int,Int)

type Rectangle = ((Int,Int),(Int,Int))

-- transparent only works for background color
transparent :: Color
transparent = (-1, -1, -1)

isTransparent :: Color -> Bool
isTransparent c  = c == transparent

-- to determine the outline of a graph node (for drawing the arrows correctly)
type Outline = Double -> (Int, Int)
            -- incoming angle -> (x, y)
-- fileds have f, so names can be used for combinators. Otherwise, CommonTypes must export these qualified
-- or module structure has to change. Too much of a hassle right now.

-- Eq could be optimized for fast lookup of fonts, depending on how much time is spent on this lookup
data Font = Font { fFamily :: String, fSize :: Int
                 , fBold :: Bool , fUnderline :: Bool
                 , fItalic :: Bool, fStrikeOut :: Bool } deriving (Show, Eq, Ord, Read)

defaultFont = Font "Times New Roman" 24 False False False False

data Style = Solid | Transparent deriving (Show, Eq, Read)

data ImgStyle = Tile | Stretch deriving (Show, Eq, Read) -- also Center?

data SpecialKey =
    EnterKey
  | BackspaceKey
  | DeleteKey
  | LeftKey
  | RightKey
  | UpKey
  | DownKey
  | F1Key
  | F2Key
  | F3Key
  | F4Key
  | F5Key
  | F6Key
  | F7Key
  | F8Key
  | F9Key
  | F10Key
  | F11Key
  | F12Key
  | CharKey Char deriving Show   -- for CTRL char keys

data Modifiers = Modifiers 
                   { shift :: Bool
                   , ctrl  :: Bool
                   , alt   :: Bool
                   } deriving Show

data Dirty = Dirty | Clean deriving Show

isClean :: Dirty -> Bool
isClean Dirty = False
isClean Clean = True

data DiffTree = DiffLeaf Bool | DiffNode Bool Bool [DiffTree]
--                                       children self 
-- DiffTree may contain infinite lists, so don't use it to recurse on (use a path arrangement instead)

-- True is Clean, False is Dirty       --selfandChildren self
-- True True: all clean
-- False True: children dirty self clean
-- False False: all dirty
-- True False: not possible          (does not seem to make sense)
-- self dirty, children clean cannot be expressed possible.
-- Maybe choose different representation. However, this requires pattern matching on two args in most algorithms
-- Diff is too simple now. (inserted children?)
instance Show DiffTree where
   show (DiffLeaf c) = "DiffLeaf "++show c
   show (DiffNode c c' dts) = if c&&c' 
                              then "<Clean>"
                              else "DiffNode "++show c ++ " " ++ show c' ++ " " ++ show dts

isCleanDT :: DiffTree -> Bool
isCleanDT (DiffLeaf c) = c
isCleanDT (DiffNode c c' _) = c && c'

isSelfCleanDT :: DiffTree -> Bool
isSelfCleanDT (DiffLeaf c) = c
isSelfCleanDT (DiffNode c c' _) = c'

{-
black = (0,0,0) :: (Int,Int,Int)
blue = (0,0,255) :: (Int,Int,Int)
green = (0,255,0) :: (Int,Int,Int)
cyan = (0,255,255) :: (Int,Int,Int)
red = (255,0,0) :: (Int,Int,Int)
magenta = (255,0,255) :: (Int,Int,Int)
yellow = (255,255,0) :: (Int,Int,Int)
grey = (128,128,128) :: (Int,Int,Int)
lightGrey = (240,240,240) :: (Int,Int,Int)
-}

white = (255,255, 255) :: (Int,Int,Int)
-- colors from http://www.tntluoma.com/sidebars/triplecolors/

black = (0,0,0) :: (Int, Int, Int)
green = (0,128,0) :: (Int, Int, Int)
darkGreen = (0,100,0) :: (Int, Int, Int)
blue = (0,0,255) :: (Int, Int, Int)
darkBlue = (0,0,139) :: (Int, Int, Int)
violet = (238,130,238) :: (Int, Int, Int)
red = (255,0,0) :: (Int, Int, Int)
indigo = (75,0,130) :: (Int, Int, Int)
cyan = (0,255,255) :: (Int, Int, Int)
magenta = (255,0,255) :: (Int, Int, Int)
yellow = (255,255,0) :: (Int, Int, Int)
lightYellow = (255,255,224) :: (Int, Int, Int)
darkRed = (139,0,0) :: (Int, Int, Int)
darkMagenta = (139,0,139) :: (Int, Int, Int)
lightBlue = (173,216,230) :: (Int, Int, Int)
darkOrange = (255,140,0) :: (Int, Int, Int)
orange = (255,165,0) :: (Int, Int, Int)
pink = (255,192,203) :: (Int, Int, Int)
lightPink = (255,182,193) :: (Int, Int, Int)
purple = (128,0,128) :: (Int, Int, Int)
lightGreen = (144,238,144) :: (Int, Int, Int)
mediumPurple = (147,112,219) :: (Int, Int, Int)
darkViolet = (148,0,211) :: (Int, Int, Int)

grey = (128,128,128) :: (Int, Int, Int)
gray = (128,128,128) :: (Int, Int, Int)
darkGrey = (169,169,169) :: (Int, Int, Int) -- ligher than grey?
darkGray = (169,169,169) :: (Int, Int, Int)
lightGrey = (211,211,211) :: (Int, Int, Int)
lightGray = (211,211,211) :: (Int, Int, Int)
silver = (192,192,192) :: (Int, Int, Int)
whiteSmoke = (245,245,245) :: (Int, Int, Int)

aqua = (0,255,255) :: (Int, Int, Int)
teal = (0,128,128) :: (Int, Int, Int)
maroon = (128,0,0) :: (Int, Int, Int)
olive = (128,128,0) :: (Int, Int, Int)
sienna = (160,82,45) :: (Int, Int, Int)
brown = (165,42,42) :: (Int, Int, Int)
fuchsia = (255,0,255) :: (Int, Int, Int)
turquoise = (64,224,208) :: (Int, Int, Int)
orangeRed = (255,69,0) :: (Int, Int, Int)
gold = (255,215,0) :: (Int, Int, Int)
darkSlateGray = (47,79,79) :: (Int, Int, Int)

{-

navy  = (0,0,128) :: (Int, Int, Int)
mediumBlue = (0,0,205) :: (Int, Int, Int)
darkCyan = (0,139,139) :: (Int, Int, Int)
deepSkyBlue = (0,191,255) :: (Int, Int, Int)
darkTurquoise = (0,206,209) :: (Int, Int, Int)
mediumSpringGreen = (0,250,154) :: (Int, Int, Int)
lime = (0,255,0) :: (Int, Int, Int)
springGreen = (0,255,127) :: (Int, Int, Int)
midnightBlue = (25,25,112) :: (Int, Int, Int)
dodgerBlue = (30,144,255) :: (Int, Int, Int)
lightSeaGreen = (32,178,170) :: (Int, Int, Int)
forestGreen = (34,139,34) :: (Int, Int, Int)
seaGreen = (46,139,87) :: (Int, Int, Int)
limeGreen = (50,205,50) :: (Int, Int, Int)
mediumSeaGreen = (60,179,113) :: (Int, Int, Int)
royalBlue = (65,105,225) :: (Int, Int, Int)
steelBlue = (70,130,180) :: (Int, Int, Int)
darkSlateBlue = (72,61,139) :: (Int, Int, Int)
mediumTurquoise = (72,209,204) :: (Int, Int, Int)
darkOliveGreen = (85,107,47) :: (Int, Int, Int)
cadetBlue = (95,158,160) :: (Int, Int, Int)
cornflowerBlue = (100,149,237) :: (Int, Int, Int)
mediumAquamarine = (102,205,170) :: (Int, Int, Int)
dimGray = (105,105,105) :: (Int, Int, Int)
slateBlue = (106,90,205) :: (Int, Int, Int)
oliveDrab = (107,142,35) :: (Int, Int, Int)
slateGray = (112,128,144) :: (Int, Int, Int)
lightSlateGray = (119,136,153) :: (Int, Int, Int)
mediumSlateBlue = (123,104,238) :: (Int, Int, Int)
lawnGreen = (124,252,0) :: (Int, Int, Int)
chartreuse = (127,255,0) :: (Int, Int, Int)
aquamarine = (127,255,212) :: (Int, Int, Int)
skyBlue = (135,206,235) :: (Int, Int, Int)
lightSkyBlue = (135,206,250) :: (Int, Int, Int)
blueViolet = (138,43,226) :: (Int, Int, Int)
saddleBrown = (139,69,19) :: (Int, Int, Int)
darkSeaGreen = (143,188,143) :: (Int, Int, Int)
paleGreen = (152,251,152) :: (Int, Int, Int)
darkOrchid = (153,50,204) :: (Int, Int, Int)
yellowGreen = (154,205,50) :: (Int, Int, Int)
greenYellow = (173,255,47) :: (Int, Int, Int)
paleTurquoise = (175,238,238) :: (Int, Int, Int)
lightSteelBlue = (176,196,222) :: (Int, Int, Int)
powderBlue = (176,224,230) :: (Int, Int, Int)
fireBrick = (178,34,34) :: (Int, Int, Int)
darkGoldenrod = (184,134,11) :: (Int, Int, Int)
mediumOrchid = (186,85,211) :: (Int, Int, Int)
rosyBrown = (188,143,143) :: (Int, Int, Int)
darkKhaki = (189,183,107) :: (Int, Int, Int)
mediumVioletRed = (199,21,133) :: (Int, Int, Int)
indianRed = (205,92,92) :: (Int, Int, Int)
peru = (205,133,63) :: (Int, Int, Int)
chocolate = (210,105,30) :: (Int, Int, Int)
tan = (210,180,140) :: (Int, Int, Int)
thistle = (216,191,216) :: (Int, Int, Int)
orchid = (218,112,214) :: (Int, Int, Int)
goldenrod = (218,165,32) :: (Int, Int, Int)
paleVioletRed = (219,112,147) :: (Int, Int, Int)
crimson = (220,20,60) :: (Int, Int, Int)
gainsboro = (220,220,220) :: (Int, Int, Int)
plum = (221,160,221) :: (Int, Int, Int)
burlyWood = (222,184,135) :: (Int, Int, Int)
lightCyan = (224,255,255) :: (Int, Int, Int)
lavender = (230,230,250) :: (Int, Int, Int)
darkSalmon = (233,150,122) :: (Int, Int, Int)
paleGoldenrod = (238,232,170) :: (Int, Int, Int)
lightCoral = (240,128,128) :: (Int, Int, Int)
khaki = (240,230,140) :: (Int, Int, Int)
aliceBlue = (240,248,255) :: (Int, Int, Int)
honeydew = (240,255,240) :: (Int, Int, Int)
azure = (240,255,255) :: (Int, Int, Int)
sandyBrown = (244,164,96) :: (Int, Int, Int)
wheat = (245,222,179) :: (Int, Int, Int)
beige = (245,245,220) :: (Int, Int, Int)
mintCream = (245,255,250) :: (Int, Int, Int)
ghostWhite = (248,248,255) :: (Int, Int, Int)
salmon = (250,128,114) :: (Int, Int, Int)
antiqueWhite = (250,235,215) :: (Int, Int, Int)
linen = (250,240,230) :: (Int, Int, Int)
lightGoldenrodYellow = (250,250,210) :: (Int, Int, Int)
oldLace = (253,245,230) :: (Int, Int, Int)
deepPink = (255,20,147) :: (Int, Int, Int)
tomato = (255,99,71) :: (Int, Int, Int)
hotPink = (255,105,180) :: (Int, Int, Int)
coral = (255,127,80) :: (Int, Int, Int)
lightSalmon = (255,160,122) :: (Int, Int, Int)
peachPuff = (255,218,185) :: (Int, Int, Int)
navajoWhite = (255,222,173) :: (Int, Int, Int)
moccasin = (255,228,181) :: (Int, Int, Int)
bisque = (255,228,196) :: (Int, Int, Int)
mistyRose = (255,228,225) :: (Int, Int, Int)
blanchedAlmond = (255,235,205) :: (Int, Int, Int)
papayaWhip = (255,239,213) :: (Int, Int, Int)
lavenderBlush = (255,240,245) :: (Int, Int, Int)
seashell = (255,245,238) :: (Int, Int, Int)
cornsilk = (255,248,220) :: (Int, Int, Int)
lemonChiffon = (255,250,205) :: (Int, Int, Int)
floralWhite = (255,250,240) :: (Int, Int, Int)
snow = (255,250,250) :: (Int, Int, Int)
ivory = (255,255,240) :: (Int, Int, Int)
-}