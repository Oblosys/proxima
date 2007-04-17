module FontLibGTK where

import CommonTypes

import qualified Data.Map as Map
import Data.Map (Map)

import Array

import Char
import Data.IORef

-- use different structure to make lookup more efficient? Or is this a waste of time
type FontMetrics = Map Font (Int, Int, Array Int Int)

type FontMetricsRef = IORef FontMetrics

newFontMetricsRef :: IO FontMetricsRef
newFontMetricsRef = newIORef Map.empty 

initFontMetrics :: IO FontMetricsRef
initFontMetrics = newFontMetricsRef
   
mkFontMetrics :: [(Font,(Int, Int, [Int]))] -> FontMetrics
mkFontMetrics ms = Map.fromList $ map (\(f,(h, b, ws)) -> (f, (h, b,  listArray (0,223) ws))) ms

metricsLookup :: Font -> FontMetrics -> (Int, Int, Array Int Int)
metricsLookup font fontMetrics = 
  -- debug Err ("looking up: " ++ show (fSize font) ++ " " ++ (fFamily font)) $
  case Map.lookup font fontMetrics  of
            Just metrics -> metrics
            Nothing      -> debug Err "metrics for font not queried" $ (0,0, listArray (0,223) (repeat 0))
    


queryFont :: Font -> IO (Font,(Int, Int, [Int]))
queryFont font@(Font fFam fSiz fBld fUnderln fItlc fStrkt) =
 do { debugLnIO Arr $ "Querying: " ++ show (fSize font) ++ " " ++ (fFamily font)

{-
    ; dc <- screenDCCreate
    
    ; dcSetFontStyle dc $ fontDefault { _fontFace = fFamily font
                              , _fontSize = fSize font
                              , _fontWeight = if fBold font then WeightBold else WeightNormal
                              , _fontShape  = if fItalic font then ShapeItalic else ShapeNormal
                              , _fontUnderline = fUnderline font }
-}
    ; (_, descent,leading) <- return (0,10,3) -- * getFullTextExtent dc "m"

 
 -- from 32 because of QT legacy. If QT renderer is dumped, maybe switch to 0
   -- ; sizes <- sequence [ getTextExtent dc [chr c] | c <-[32..255]]
    ; let widths = replicate (255-32+1) 10 -- * map ((\x -> x).sizeW) sizes
    ; let height = 8-- maximum $ map sizeH sizes
    ; let ascent = height - descent

-- GTKTODO: 

{-    
    ; debugLnIO Arr $    "ascent:   " ++ show ascent
                        ++ "\ndescent:  " ++ show descent
                        ++ "\nleading:  " ++ show leading
                        ++ "\nheight: " ++ show height
    ; debugLnIO Arr $    "\nwidths:   " ++ show widths  
-}   
    
    -- WX only has ascent and leading. take height as max char height. or is char height always the same?
    -- then why not just let getFullTextExtent return ascent or height?
    
    ; return (font, (height,ascent,widths))
    ;-- return (font, (fAscent fontMetrics+fDescent fontMetrics,fAscent fontMetrics,widths))
    }


forceEval :: Show a => a -> IO ()
forceEval a = seq (last (show a)) (return ())


-- TODO: get rid of this QT stuff.
-- QT gives incorrect metrics for characters under 32. They have a width > 1, but when rendered as part
-- a QString, they have no appearance and no width on the screen "b\1la" is rendered as bla.

textWidth :: FontMetrics -> Font -> String -> Int
textWidth fms f str = let (h,b,ws) = metricsLookup f fms
                          toWidth c = let i = ord c 
                                      in  if i < 32 then 0 else ws ! (ord c - 32)
                      in sum (map toWidth str)
-- round (fromInt (length str) * charWidth fs)

-- Is it accurate enough to add the widhts of the characters? The width of the string might
-- be different due to rounding errors. We could use a stringwidth function, but this results 
-- in more communication with the renderer.
cumulativeCharWidths :: FontMetrics -> Font -> String -> [Int]
cumulativeCharWidths fms f str = let (h,b,ws) = metricsLookup f fms
                                     toWidth c = let i = ord c 
                                                 in  if i < 32 then 0 else ws ! (ord c - 32)
                                 in  scanl (+) 0 (map toWidth str)

charHeight :: FontMetrics -> Font -> Int
charHeight fms f  = let (h,b,ws) = metricsLookup f fms
                    in  (h)

baseLine :: FontMetrics -> Font -> Int
baseLine fms f = let (h,b,ws) = metricsLookup f fms
                 in  (b)
 {---
                       ,"required "++id++"_height = "++ show (charHeight lhs_fontSize 32)
                       ,"required "++id++"_hRef = "++show (baseLine lhs_fontSize) ]
		       -- ,"required "++id++"_hRef =

                       -}
                       