module DPPClass_Main where

import DPPClass_Lib
import Layers


type Step nextstep a b c d = (a ->(b, nextstep c d a b))

newtype PresStep doc pres gest upd = 
            PresStep {presStep :: Step TransStep doc pres gest upd}
newtype TransStep gest upd doc pres = 
            TransStep {transStep :: Step PresStep gest upd doc pres}

instance Pack (PresStep a b c d) a b (TransStep c d a b) where
  pack = PresStep
  unpack = presStep

instance Pack (TransStep a b c d) a b (PresStep c d a b) where
  pack = TransStep
  unpack = transStep

lift :: Simple a b c d e f  -> a -> PresStep c d e f
lift simple =
  fix $ liftStep (present simple) 
      . liftStep (translate simple) 

combine :: PresStep a b e f -> PresStep b c d e -> PresStep a c d f
combine upr lwr =  
  fix (combineStepDown . combineStepUp) upr lwr



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


