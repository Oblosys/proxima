module DPPClass_Main where

{-# OPTIONS -fglasgow-exts  #-}
-- fno-monomorphism-restriction

import DPPClass_Lib
import Layers


type Step nStep a b c d = (a -> (nStep c d a b, b))

newtype PresStep doc pres gest upd = 
            PresStep  {presStep  :: Step IntrStep doc pres gest upd}
newtype IntrStep gest upd doc pres = 
            IntrStep {intrStep :: Step PresStep gest upd doc pres}

instance Pack (PresStep a b c d) a b (IntrStep c d a b) where
  pack = PresStep
  unpack = presStep

instance Pack (IntrStep a b c d) a b (PresStep c d a b) where
  pack = IntrStep
  unpack = intrStep

lift :: Simple a b c d e f  -> a -> PresStep c d e f
lift simple =
  fix $ liftStep (present simple) . liftStep (interpret simple) 

combine :: PresStep a b e f -> PresStep b c d e -> PresStep a c d f
combine higher lower =  
  (fix $ combineStepDown
      . combineStepUp) higher lower



editLoop present doc = 
 do { let (IntrStep interpret, pres) = present doc

    ; showRendering pres
    ; gesture <- getGesture
    
    ; let (PresStep present', update) = interpret gesture
    
    ; let doc' = updateDocument update doc
    
    ; editLoop present' doc'
    }



main ::        Simple State0 b h1 m1 m h
        -> Simple State1 b1 m1 m3 m2 m
           -> Simple State2 b2 m3 l1 l m2 -> IO ()
--main:: Simple State0 Mapping0 Document Presentation EditPresentation EditDocument ->
--       Simple State1 Mapping1 Presentation Arrangement EditArrangement EditPresentation ->
--       Simple State2 Mapping2 Arrangement Rendering EditRendering EditArrangement -> IO ()
main layer0 layer1 layer2 =
 do { (state0, state1, state2) <- initStates
    ; doc <- initDoc 
    ; let PresStep present =           lift layer0 state0  
                             `combine` lift layer1 state1
                             `combine` lift layer2 state2
--    ; doc <- initDoc
--    ; editLoop present doc
    ; return ()
    }


main' = main layer0 layer1 layer2