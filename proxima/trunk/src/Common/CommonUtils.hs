module Common.CommonUtils where

import Common.CommonTypes
import qualified Common.CommonTypes as CommonTypes
--import Graphics.UI.Gtk hiding (Rectangle)
import Data.Time.Clock
import Data.IORef
-- is exported (and imported) by all ...Utils modules

-- safer version of !! that can report the origin of the index call
-- non-crashing functions are desirable, because at a crash in a program with a GUI, ghci crashes as well
-- WARNING:: this version does not work with infinite lists!!!!
index callerName [] i = error ("*** CommonUtils.index (called by "++callerName++"): indexing in empty list **** ")
index callerName l  i = let i' = if i < 0 then debug Err ("*** CommonUtils.index (called by " ++callerName++"): index < 0 **** ") 0
                                else if i >= length l then debug Err ("*** CommonUtils.index (called by "++callerName++"): index >= length **** ") ((length l)-1)
                                else i
                        in  l !! i'

clip lo hi x = if x<lo then lo else if x>hi then hi else x
--

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix xs ys = map fst . takeWhile (uncurry (==)) $ zip xs ys

-- | Replace the i'the element in xs by x.
replace :: String -> Int -> [a] -> a -> [a]
replace callerName i xs x = case splitAt i xs of
                   (left, _: right) -> left ++ [x] ++ right
                   _                -> debug Err ("**** CommonUtils.replace (called by "++callerName++"): index out of bounds") xs

-- scaling seems to be rather expensive. For now it is turned off.
scaleInt :: Double -> Int -> Int
scaleInt scale x = x -- round (fromIntegral x * scale)  

descaleInt :: Double -> Int -> Int
descaleInt scale x = x -- round (fromIntegral x / scale)

-- Compute angle of the line (fromx,fromy) (tox,toy), result lies in [0..2*pi>
computeAngle :: Int -> Int -> Int -> Int -> Double
computeAngle fromx fromy tox toy = 
  atan (fromIntegral (-(toy-fromy))/fromIntegral (tox-fromx)) + 
  if tox < fromx then pi else if fromy < toy then 2* pi else 0 
{-
In quadrants  II  I  , atan a is between 0 and pi/2 or -pi/2:  - +
              III IV                                           + -
              
Therefore, we add   pi 0   , based on the quadrant the line is pointing in.
                    pi 2pi

Note, remember that screen y is flipped wrt. mathematical y
-}

-- | Perform an action if condition c holds
when :: Bool -> IO () -> IO ()
when c act = if c then act else return ()

-- | Return True iff the two rectangles overlap.
overlap :: Rectangle -> Rectangle -> Bool
overlap ((x, y), (w, h)) ((x', y'),(w',h')) =
  not (x >= x' + w' || x + w <= x' || y >= y' + h' || y + h <= y')

-- | Return True iff the second rectangle lies inside the first one.

contains :: Rectangle -> Rectangle -> Bool
contains ((x, y), (w, h)) ((x', y'),(w',h')) =
  (x' >= x && x'+w' <= x+w &&  y' >= y && y'+h' <= y+h)
  

-- | Compute the difference (rectangle - rectangle). The result is a list of a maximum of
--   4 rectangles.
-- First, four segments of the second rectangle's complement are computed, for each of which we
-- take the intersection with the first rectangle.
difference :: Rectangle -> Rectangle -> [Rectangle]
difference ((x,y),(w,h)) ((x',y'),(w',h')) =
  let lu@(lx,uy) = (x,y) -- switch to absolute coordinates to make computation easier
      rl@(rx,ly) = (x+w,y+h)
      (lx',uy') = (x',y')
      (rx',ly') = (x'+w',y'+h')
      left  = ((minBound,minBound),(lx',maxBound))
      right = ((rx',minBound),(maxBound,maxBound))
      upper = ((lx', minBound), (rx',uy'))
      lower = ((lx', ly'), (rx',maxBound))
  in  filter isNonEmptyRectangle $ map (toRelRect . intersectionAbs (lu,rl)) [ left, right, upper, lower ]
 where toRelRect ((lx,uy),(rx,ly)) = ((lx,uy),(rx-lx,ly-uy))

       intersectionAbs ((boundlx,bounduy),(boundrx,boundly)) ((lx,uy),(rx,ly)) =
         ( (clip boundlx boundrx lx, clip bounduy boundly uy)  -- note that upper y < lower y
         , (clip boundlx boundrx rx, clip bounduy boundly ly)
         )
           
       isNonEmptyRectangle ((x,y),(w,h)) = w>0 && h>0

-- Some basic timer functions for benchmarking

startTimer = 
 do { time <- getCurrentTime
    ; newIORef time
    }
    
resetTimer timer = 
 do { time <- getCurrentTime
    ; writeIORef timer time
    }
    
getTimerValue timer =
 do { startTime <- readIORef timer
    ; time <- getCurrentTime
    ; return $ diffUTCTime time startTime
    }

-- lines' works for Unix, Mac, and Dos format
lines'     :: String -> [String]
lines' ""   = []
lines' s    = let (l,s') = break (\c->c=='\n' || c=='\r') s
             in l : case s' of []      -> []
                               ('\r' :'\n':s'') -> lines' s''   -- a Dos "\n\r" encountered on Unix or Mac platform
                               ('\n' :s'') -> lines' s''         -- the current platform's linebreak (?)
                                                                 -- or a Unix "\n" encountered on a Dos or Mac platform
                               ('\r':s'') -> lines' s''          -- a  Mac "\r" encountered on Dos or Unix platform 
-- what happens with '\r' on mac? is it automatically converted to '\n'? If so, will a Dos file then contain "\n\n"?

-- Version of head that reports the caller in case of empty list.
head' caller xs = case xs of
                    []  -> error $ caller ++ ": called head' []"
                    x:_ -> x 
                
                    
-- mark what parts of the arrangement were reused from the previous one

-- This setting is not part of Settings.settings, since it would add to the
-- run-time cost of the arranger to pass the record around as an inherited attribute.

markArrangementBackground :: Bool
markArrangementBackground = False

