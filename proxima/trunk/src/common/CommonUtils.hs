module CommonUtils where

import CommonTypes

-- is exported (and imported) by all ...Utils modules


-- safer version of !! that can report the origin of the index call
-- non-crashing functions are desirable, because at a crash in a program with a GUI, ghci crashes as well
-- WARNING:: this version does not work with infinite lists!!!!
index callerName i [] = error ("**** "++callerName++": indexing in empty list **** ")
index callerName i l = let i' = if i < 0 then debug Err ("**** "++callerName++": indexing with index < 0 **** ") 0
                                else if i >= length l then debug Err ("**** "++callerName++": indexing index >= length **** ") ((length l)-1)
                                else i
                       in  l !! i'

-- delete this one and replace all occurrences with index ".."
-- WARNING:: this version does not work with infinite lists!!!!
[] !!! i = error "**** !!!: selection from empty list **** "
l !!! i = let i' = if i < 0 then debug Err "**** !!!: index < 0 **** "  0
                   else if i >= length l then debug Err "**** !!!: index >= length **** " (length l)-1 
                   else i
          in  l !! i'

clip lo hi x = if x<lo then lo else if x>hi then hi else x
--

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix xs ys = map fst . takeWhile (uncurry (==)) $ zip xs ys


-- scaling seems to be rather expensive. For now it is turned off.
scaleInt :: Double -> Int -> Int
scaleInt scale x = x -- round (fromIntegral x * scale)  

descaleInt :: Double -> Int -> Int
descaleInt scale x = x -- round (fromIntegral x / scale)

-- Compute angle of the line (fromx,fromy) (tox,toy)
computeAngle :: Int -> Int -> Int -> Int -> Double
computeAngle fromx fromy tox toy = atan (fromIntegral (toy-fromy)/fromIntegral (fromx-tox)) +
                                   if fromx < tox then if fromy <= toy then pi else -pi else 0
-- toy and fromy switched because tan uses mathematical coordinate system (positive is up)
-- in upper left quadrant, add pi, and in lower left quadrant, subtract pi, so the resulting angle is between -pi and pi