{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module DPP where

import Layers
--import DPP_Lib
import DPPClass_Lib 

import Char 

{-
liftE' :: Simple state map doc pres gest upd ->
         state -> LayerE doc pres gest upd
liftE' layer state = presStep state 
 where presStep state = 
         LayerE ( 
           \doc -> let (pres, (map,state)) = present layer state doc                                         
                   in (pres,  (\gest -> let (upd, state') = interpret layer (map, state) gest                     
                                         in  (upd, presStep state')
                               ))
                  )

combineE' :: LayerE high med emed ehigh -> LayerE med low elow emed -> 
             LayerE high low elow ehigh
combineE' upperPres lowerPres = LayerE $                                                         
  \high ->                                                                    
    let LayerE upper = upperPres
        LayerE lower = lowerPres
        (med ,upperIntr) = upper high
        (low, lowerIntr) = lower med
    in (low, \elow ->
                let upper' = upperIntr 
                    lower' = lowerIntr
                    (emed, lowerPres') = lower' elow
                    (ehigh, upperPres') = upper' emed
                in  (ehigh, combineE' upperPres' lowerPres'))
-}

-- DPP without meta combinators

newtype Layer doc pres gest upd = 
          Layer ( doc -> 
                   ( pres, 
                       ( gest -> 
                           ( upd 
                           , Layer doc pres gest upd))))

--------------------------------------------------------
lift :: Simple state map doc pres gest upd ->
        state -> Layer doc pres gest upd
lift simple state = presStep state 
 where presStep state = Layer $
         \doc ->  let (pres, (map,state)) = 
                         present simple state doc                                         
                  in  (pres, intrStep (map,state))
       intrStep (map,state) =
         \gest -> let (upd, state') = 
                        interpret simple (map,state) gest                     
                  in  (upd, presStep state')

combine :: Layer high med emed ehigh -> 
           Layer med low elow emed -> 
           Layer high low elow ehigh
combine = presStep
 where presStep (Layer upr) (Layer lwr) = Layer $ 
           \high -> let (med, uprIntr) = upr high
                        (low, lwrIntr) = lwr med
                    in  (low, intrStep uprIntr lwrIntr)
       intrStep upr lwr = 
           \elow -> let (emed, lwrPres) = lwr elow
                        (ehigh, uprPres) = upr emed
                    in  (ehigh, presStep uprPres lwrPres)






--editLoop :: Layer Document Rendering EditRendering EditDocument -> Document -> IO ()
editLoop (Layer presentStep) doc = 
 do { let (pres , interpretStep) = presentStep doc
    
    ; showRendering pres
    ; gesture <- getGesture
    
    ; let (update, presentStep') = interpretStep gesture
    
    ; let doc' = updateDocument update doc
    
    ; editLoop presentStep' doc'
    }

--main :: IO ()
main layer0 layer1 layer2 =
 do { (state0, state1, state2) <- initStates
    ; doc <- initDoc 
    ; let layers =           lift layer0 state0 
                   `combine` lift layer1 state1
                   `combine` lift layer2 state2
    ; editLoop layers doc
    }
