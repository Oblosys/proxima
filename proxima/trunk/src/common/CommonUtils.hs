module CommonUtils where

import CommonTypes

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
  not (x > x' + w' || x + w < x' || y > y' + h' || y + h < y')
