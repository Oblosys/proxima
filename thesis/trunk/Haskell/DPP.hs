module Main where

import Layers

newtype PresStep doc pres gest upd = 
          PresStep  (doc ->  (TransStep gest upd doc pres, pres))
newtype TransStep gest upd doc pres = 
          TransStep (gest -> (PresStep doc pres gest upd, upd)) 

type Layer doc pres gest upd = PresStep doc pres gest upd  


lift :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift simple state = presStep state 
 where presStep state = PresStep $
           \doc ->  let ((mapping,state), pres) = present simple state doc                                         
                    in  (transStep (mapping,state), pres)
       transStep (mapping,state) = TransStep $
           \gest -> let (state', upd) = translate simple (mapping, state) gest                     
                    in  (presStep state', upd)





combine :: Layer high med emed ehigh -> Layer med low elow emed -> 
             Layer high low elow ehigh
combine = presStep
 where presStep (PresStep upr) (PresStep lwr) = PresStep $ 
           \high -> let (uprTrans, med) = upr high
                        (lwrTrans, low) = lwr med
                    in  (transStep uprTrans lwrTrans, low)
       transStep (TransStep upr) (TransStep lwr) = TransStep $
           \elow -> let (lwrPres, emed) = lwr elow
                        (uprPres, ehigh) = upr emed
                    in  (presStep uprPres lwrPres, ehigh)

 

editLoop presentStep doc = 
 do { let (TransStep translateStep, pres) = presentStep doc

    ; showRendering pres
    ; gesture <- getGesture
    
    ; let (PresStep presentStep', update) = translateStep gesture
    
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
          PresStep  (doc ->  (pres, TransStep gest upd doc pres))
newtype TransStep gest upd doc pres = 
          TransStep (gest -> (upd,  PresStep doc pres gest upd)) 

type Layer doc pres gest upd = PresStep doc pres gest upd  


lift' :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift' layer state = presStep state 
 where presStep state = 
         PresStep ( 
           \doc -> let ((mapping,state), pres) = present layer state doc                                         
                   in (pres, TransStep 
                               (\gest -> let (state', upd) = translate layer (mapping, state) gest                     
                                         in  (upd, presStep state')
                               ))
                  )


lift :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift simple state = presStep state 
 where presStep state = PresStep $
           \doc ->  let ((mapping,state), pres) = present simple state doc                                         
                    in  (pres, transStep (mapping,state))
       transStep (mapping,state) = TransStep $
           \gest -> let (state', upd) = translate simple (mapping, state) gest                     
                    in  (upd, presStep state')


combine' :: Layer high med emed ehigh -> Layer med low elow emed -> 
             Layer high low elow ehigh
combine' upperPres lowerPres = PresStep $                                                         
  \high ->                                                                    
    let PresStep upper = upperPres
        PresStep lower = lowerPres
        (med ,upperTrans) = upper high
        (low, lowerTrans) = lower med
    in (low, TransStep $ \elow ->
                let TransStep upper' = upperTrans 
                    TransStep lower' = lowerTrans
                    (emed, lowerPres') = lower' elow
                    (ehigh, upperPres') = upper' emed
                in  (ehigh, combine' upperPres' lowerPres'))


combine :: Layer high med emed ehigh -> Layer med low elow emed -> 
             Layer high low elow ehigh
combine = presStep
 where presStep (PresStep upr) (PresStep lwr) = PresStep $ 
           \high -> let (med, uprTrans) = upr high
                        (low, lwrTrans) = lwr med
                    in  (low, transStep uprTrans lwrTrans)
       transStep (TransStep upr) (TransStep lwr) = TransStep $
           \elow -> let (emed, lwrPres) = lwr elow
                        (ehigh, uprPres) = upr emed
                    in  (ehigh, presStep uprPres lwrPres)

 

editLoop presentStep doc = 
 do { let (pres, TransStep translateStep) = presentStep doc

    ; showRendering pres
    ; gesture <- getGesture
    
    ; let (update, PresStep presentStep') = translateStep gesture
    
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