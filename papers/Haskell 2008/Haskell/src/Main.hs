{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}

module Main where

import Layers 
import DPPMonad_Lib ()
import DPP_Lib () 
import DPP_Main ()
import Derivation (dppMain)
import AutoLiftCombine () 
import Magic (combineTest)
import qualified Magic
import MagicMonad ()
import DerivationNilStep ()

--type Layer m a b c d = Fix m (Step a b :.: Step c d :.: NilStep)
{-
lift :: Simple a b c d e f  -> a -> PresStep c d e f
lift simple =
  fix $ liftStep (present simple) 
      . liftStep (interpret simple) 

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
-}
main = undefined -- Magic.main layer0 layer1 layer2 -- combineTest
{-
 do { (state0, state1, state2) <- initStates
    ; doc <- initDoc 
    ; let PresStep present =           lift layer0 state0  
                             `combine` lift layer1 state1
                             `combine` lift layer2 state2
    ; doc <- initDoc
    ; editLoop present doc
    }

-}