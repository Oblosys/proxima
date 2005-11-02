module FontLib where

import CommonTypes

import Array

import Char
import IOExts

import Graphics.UI.ObjectIO hiding (Font, FontMetrics, fSize)
import qualified Graphics.UI.ObjectIO

-- use different structure to make lookup more efficient? Or is this a waste of time
type FontMetrics = FiniteMap Font (Int, Int, Array Int Int)

type FontMetricsRef = IORef FontMetrics

newFontMetricsRef :: IO FontMetricsRef
newFontMetricsRef = newIORef emptyFM 

initFontMetrics :: IO FontMetricsRef
initFontMetrics = newFontMetricsRef
   
mkFontMetrics :: [(Font,(Int, Int, [Int]))] -> FontMetrics
mkFontMetrics ms = listToFM $ map (\(f,(h, b, ws)) -> (f, (h, b,  listArray (0,223) ws))) ms

{-
lookup (fs, ff) fms
 unsafePerformIO 
  fms <- var fms
  r = lookup fms
 case r of Just -> return
   Nothing 
  do
    queryfont
-} 
{-
metricsLookup :: (Int, String) -> FontMetrics -> (Int, Int, Array Int Int)
metricsLookup font fms = 
  unsafePerformIO $ metricsLookup' font fms  -- is this safe??
                                             -- the metrics don't change so the function is pure
                                             -- but won't the communication with the renderer
                                             -- cause problems? Maybe not because the comm. is 
                                             -- atomic.
-}
metricsLookup :: Font -> FontMetrics -> (Int, Int, Array Int Int)
metricsLookup font metrics = 
  -- debug Err ("looking up: " ++ show (fSize font) ++ " " ++ (fFamily font)) $
  case lookupFM metrics font  of
            Just metrics -> metrics
            Nothing      -> debug Err "metrics for font not queried" $ (0,0, listArray (0,223) (repeat 0))
    


queryFont :: Font -> IO (Font,(Int, Int, [Int]))
queryFont font = doScreenDraw $
 do { debugLnDraw Arr $ "Querying: " ++ show (fSize font) ++ " " ++ (fFamily font)

     
    ; let fontOIO = Graphics.UI.ObjectIO.Font (fFamily font) (fSize font) (fBold font) (fUnderline font)
                                                                        (fItalic font) (fStrikeOut font)
    ; fontMetrics <- getFontMetrics fontOIO

 -- from 32 because of QT legacy. If QT renderer is dumped, maybe switch to 0
    ; widths <- getFontCharWidths fontOIO ['\32' .. '\255']
    {-
    ; debugLnDraw Arr $    "ascent:   " ++ show (fAscent fontMetrics)  
                        ++ "\ndescent:  " ++ show (fDescent fontMetrics)
                        ++ "\nleading:  " ++ show (fLeading fontMetrics)
                        ++ "\nmaxwidth: " ++ show (fMaxWidth fontMetrics)
    ; debugLnDraw Arr $    "\nwidths:   " ++ show widths  
  -} 
        
    ; return (font, (fAscent fontMetrics+fDescent fontMetrics,fAscent fontMetrics,widths))
    }
{-
  -- for profiling without objectio
queryFont :: Font -> IO (Font,(Int, Int, [Int]))
queryFont font@(Font fFam fSiz fBld fUnderln fItlc fStrkt)  = 
 do { return (font, (fSiz,fSiz `div` 2, [fSiz `div` 2 |i <- [32..255]]))
    }
-}

forceEval :: Show a => a -> IO ()
forceEval a = seq (last (show a)) (return ())

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