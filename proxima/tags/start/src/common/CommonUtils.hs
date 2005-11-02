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

-- scaling seems to be rather expensive. For now it is turned off.
scaleInt :: Double -> Int -> Int
scaleInt scale x = x -- round (fromIntegral x * scale)  

descaleInt :: Double -> Int -> Int
descaleInt scale x = x -- round (fromIntegral x / scale)

