module Main where

import Layers

newtype PresStep doc pres gest upd = 
          PresStep  (doc ->  (IntrStep gest upd doc pres, pres))
newtype IntrStep gest upd doc pres = 
          IntrStep (gest -> (PresStep doc pres gest upd, upd)) 

type Layer doc pres gest upd = PresStep doc pres gest upd  


lift :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift simple state = presStep state 
 where presStep state = PresStep $
           \doc ->  let ((mapping,state), pres) = (present simple) state doc                                         
                    in  (intrStep (mapping,state), pres)
       intrStep (mapping,state) = IntrStep $
           \gest -> let (state', upd) = (interpret simple) (mapping, state) gest                     
                    in  (presStep state', upd)





combine :: Layer high med emed ehigh -> Layer med low elow emed -> 
             Layer high low elow ehigh
combine = presStep
 where presStep (PresStep hghr) (PresStep lwr) = PresStep $ 
           \high -> let (hghrIntr, med) = hghr high
                        (lwrIntr, low) = lwr med
                    in  (intrStep hghrIntr lwrIntr, low)
       intrStep (IntrStep hghr) (IntrStep lwr) = IntrStep $
           \elow -> let (lwrPres, emed) = lwr elow
                        (hghrPres, ehigh) = hghr emed
                    in  (presStep hghrPres lwrPres, ehigh)

 

editLoop presentStep doc = 
 do { -- Presentation process:
      let (IntrStep interpretStep, pres) = presentStep doc

    ; showRendering pres
    ; gesture <- getGesture
    
      -- Interpretation process:
    ; let (PresStep presentStep', update) = interpretStep gesture
    
    ; let doc' = updateDocument update doc
    
    ; editLoop presentStep' doc'
    }

--main :: IO ()
main' layer0 layer1 layer2 =
 do { (state0, state1, state2) <- initStates
    ; doc <- initDoc 
    ; let PresStep presentStep =           lift layer0 state0 
                                 `combine` lift layer1 state1
                                 `combine` lift layer2 state2
    ; editLoop presentStep doc
    }

    
    
    
    
{- original  (nstep, vres) instead of (vres, nstep)
newtype PresStep doc pres gest upd = 
          PresStep  (doc ->  (pres, IntrStep gest upd doc pres))
newtype IntrStep gest upd doc pres = 
          IntrStep (gest -> (upd,  PresStep doc pres gest upd)) 

type Layer doc pres gest upd = PresStep doc pres gest upd  


lift' :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift' layer state = presStep state 
 where presStep state = 
         PresStep ( 
           \doc -> let ((mapping,state), pres) = present layer state doc                                         
                   in (pres, IntrStep 
                               (\gest -> let (state', upd) = interpret layer (mapping, state) gest                     
                                         in  (upd, presStep state')
                               ))
                  )


lift :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift simple state = presStep state 
 where presStep state = PresStep $
           \doc ->  let ((mapping,state), pres) = present simple state doc                                         
                    in  (pres, intrStep (mapping,state))
       intrStep (mapping,state) = IntrStep $
           \gest -> let (state', upd) = interpret simple (mapping, state) gest                     
                    in  (upd, presStep state')


combine' :: Layer high med emed ehigh -> Layer med low elow emed -> 
             Layer high low elow ehigh
combine' upperPres lowerPres = PresStep $                                                         
  \high ->                                                                    
    let PresStep upper = upperPres
        PresStep lower = lowerPres
        (med ,upperIntr) = upper high
        (low, lowerIntr) = lower med
    in (low, IntrStep $ \elow ->
                let IntrStep upper' = upperIntr 
                    IntrStep lower' = lowerIntr
                    (emed, lowerPres') = lower' elow
                    (ehigh, upperPres') = upper' emed
                in  (ehigh, combine' upperPres' lowerPres'))


combine :: Layer high med emed ehigh -> Layer med low elow emed -> 
             Layer high low elow ehigh
combine = presStep
 where presStep (PresStep upr) (PresStep lwr) = PresStep $ 
           \high -> let (med, uprIntr) = upr high
                        (low, lwrIntr) = lwr med
                    in  (low, intrStep uprIntr lwrIntr)
       intrStep (IntrStep upr) (IntrStep lwr) = IntrStep $
           \elow -> let (emed, lwrPres) = lwr elow
                        (ehigh, uprPres) = upr emed
                    in  (ehigh, presStep uprPres lwrPres)

 

editLoop presentStep doc = 
 do { let (pres, IntrStep interpretStep) = presentStep doc

    ; showRendering pres
    ; gesture <- getGesture
    
    ; let (update, PresStep presentStep') = interpretStep gesture
    
    ; let doc' = updateDocument update doc
    
    ; editLoop presentStep' doc'
    }

--main :: IO ()
main' layer0 layer1 layer2 =
 do { (state0, state1, state2) <- initStates
    ; doc <- initDoc 
    ; let PresStep presentStep =           lift layer0 state0 
                                 `combine` lift layer1 state1
                                 `combine` lift layer2 state2
    ; editLoop presentStep doc
    }
-}