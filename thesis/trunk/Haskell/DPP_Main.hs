module DPP_Main where
-- obsolete, class version (DPPClass_Main) is used in thesis.
-- step & res parameters have not been swapped

import DPP_Lib
import Char

import Layers
-- direct parameter passing
-- continuing to abstract 
-- same as previous, but now with class for Step


-- computing combine automatically is too complex.

-- doing it recursively is complex because fix adds constructors and takes
-- symmetry away. Doing it with all params makes big instances, probably more
-- complex than non class solution.



-- trying to make a class for fitting a bunch of functions
--         b              e                       b  e   
--         |              |                       |  |     
--    a -> f -> d    d -> g -> i     into    a ->       -> i                                     
--         |              |                       |  |
--         c              h                       c  h

{-
  layer0 = f `glue` g `glue` h 
  layer1 = i `glue` j `glue` k

not:  layers = layer0 `combine` layer1

combine = 
 Isues:

direction of vertical args. It can be up or down. Maybe use a class to combine
and have the member function implement the direction

a horizontal computation is split into a number of steps, each with a direction

then for each layer, appropriate f's are made instance of each of the steps
-}


------------ architecture specific part: ------------



type Step nextstep a b c d = (a ->(b, nextstep c d a b))
newtype PresStep a b c d = PresStep (Step TransStep a b c d)
newtype TransStep a b c d = TransStep(Step PresStep a b c d)

{-
type Step ns a b c d = (a-> (b, ns c d a b)) 

newtype PresStep a b c d = PresStep (Step TransStep a b c d)
newtype TransStep a b c d = TransStep (Step PresStep a b c d)
-}


dictPresStep = (PresStep, \(PresStep f) -> f, \(PresStep f) -> f)
dictTransStep = (TransStep, \(TransStep f) -> f, \(TransStep f) -> f)

lift simple =
    fix $ f dictPresStep (present simple) . f dictTransStep (translate simple) 

combine = 
  fix $ g dictPresStep combineUp . g dictTransStep combineUp 


editLoop present doc = 
 do { let (pres, TransStep translate) = present doc

    ; showRendering pres
    ; gesture <- getGesture
    
    ; let (update, PresStep present') = translate gesture
    
    ; let doc' = updateDocument update doc
    
    ; editLoop present' doc'
    }

main layer0 layer1 layer2 =
 do { (state0, state1, state2) <- initStates
    ; doc <- initDoc 
    ; let PresStep present =           lift layer0 state0  
                             `combine` lift layer1 state1
                             `combine` lift layer2 state2
    ; doc <- initDoc
    ; editLoop present doc
    }
