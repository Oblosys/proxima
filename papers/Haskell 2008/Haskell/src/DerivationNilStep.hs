-----------------------------------------------------------------------------------------
{-| Module      : DerivationNilStep
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module DerivationNilStep where

import Layers
--- from lib

fix :: (a->a) -> a
fix a = let fixa = a fixa
        in  fixa





newtype Fix f = Fix (f (Fix f))

infixr :.:

newtype (:.:) f g ns  = Comp (f (g ns))

newtype Step a b ns = Step (a -> (b, ns))

newtype NilStep t = NilStep t

--------------------------------------------------------
lift0, lift1, lift2, lift4, lift5, lift, lift'  
      :: Simple state map doc pres gest upd -> state ->
         Fix (Step doc pres  :.: Step gest upd :.: NilStep)


lift0 simple state = step1 state 
 where step1 hArg = Fix . Comp . Step $
         \vArg -> let (pres, hRes) = 
                       present simple hArg vArg
                  in  (pres, step2 hRes)
       step2 hArg = Comp . Step $
         \vArg -> let (upd, hRes) = 
                        interpret simple hArg vArg
                  in  (upd, lNilStep hRes)
       lNilStep hRes = NilStep $ step1 hRes

lift1 simple state =  
  step1 (step2 (lNilStep (lift1 simple))) state
 where step1 next hArg = Fix . Comp $ Step $
         \vArg -> let (pres, hRes) = 
                        present simple hArg vArg
                  in  (pres, next hRes)
       step2 next hArg = Comp . Step $
         \vArg -> let (upd, hRes) = 
                        interpret simple hArg vArg
                  in  (upd, next  hRes)

lNilStep next hRes = NilStep $ next hRes

liftStep :: LayerFn hArg vArg hRes vRes -> (hRes -> g ns) -> hArg -> (Step vArg vRes :.: g) ns
liftStep f next horArgs = Comp . Step $ 
  \vArg -> let (vertRes, horRes) = f horArgs vArg
           in  (vertRes, next horRes)

lift2 simple state =
  step1 (step2 (lNilStep (lift2 simple))) state
 where step1 next hArg = Fix $  
         liftStep (present simple) next hArg
       step2 next hArg =
         liftStep `(interpret simple) next hArg 
      
lift4 simple = fix (step1 . step2 . lNilStep)
 where step1 next hArg = Fix $
         liftStep (present simple) next hArg
       step2 next hArg =
         liftStep (interpret simple) next hArg

lfix f = fix f'  where f' n =  (Fix . (f . lNilStep) n) 

lift5 simple =  lfix (step1 . step2)
 where step1 next hArg = liftStep (present simple)  next hArg
       step2 next hArg = liftStep (interpret simple) next hArg
       
lift simple =  lfix (liftStep (present simple) . liftStep (interpret simple))

  
lift simple =  lfix $ liftStep (present simple) . liftStep (interpret simple)


-- or with a composition that removes left step. No need for liftStep's, but does need the lNilStep

infixr 8 .:

(.:) f g = liftStep f . g
lfix' f = fix f'  where f' n =  (Fix . f n) 

lift' simple =  lfix' $ present simple .: interpret simple .: lNilStep

combine0, combine1, combine2, combine3, combine5, combine::
  Layer2 high med emed ehigh ->
  Layer2 med low elow emed -> 
  Layer2 high low elow ehigh 

combine0 upr lwr = step1 upr lwr
 where step1 (Fix (Comp (Step upr))) 
                (Fix (Comp (Step lwr))) = 
         Fix . Comp . Step $ 
         \high -> let (med, uprIntr) = upr high
                      (low, lwrIntr) = lwr med
                  in  (low, step2 uprIntr lwrIntr)
       step2 (Comp (Step upr)) (Comp (Step lwr)) = Comp . Step $
         \low -> let (med, lwrPres) = lwr low
                     (high, uprPres) = upr med
                 in  (high, cNilStep uprPres lwrPres)
       cNilStep (NilStep u) (NilStep l) = NilStep $ step1 u l 




cNilStep next (NilStep u) (NilStep l) = NilStep $ next u l 
 
combine1 upr lwr = (step1 (step2 (cNilStep combine1))) upr lwr
 where step1 next (Fix (Comp (Step upr))) 
                (Fix (Comp (Step lwr))) = 
         Fix . Comp . Step $ 
         \high -> let (med, uprIntr) = upr high
                      (low, lwrIntr) = lwr med
                  in  (low, next uprIntr lwrIntr)
       step2 next (Comp (Step upr)) (Comp ( Step lwr)) = Comp . Step $
         \low -> let (med, lwrPres) = lwr low
                     (high, uprPres) = upr med
                 in  (high, next uprPres lwrPres)
 

combine2 upr lwr = fix (step1 . step2  . cNilStep) upr lwr
 where step1 next (Fix (Comp (Step upr))) 
                (Fix (Comp (Step lwr))) = 
         Fix . Comp . Step $ 
         \high -> let (med, uprIntr) = upr high
                      (low, lwrIntr) = lwr med
                  in  (low, next uprIntr lwrIntr)
       step2 next (Comp (Step upr)) (Comp ( Step lwr)) = Comp . Step $
         \low -> let (med, lwrPres) = lwr low
                     (high, uprPres) = upr med
                 in  (high, next uprPres lwrPres)

combineStepDown :: (f x -> g y -> h ns) ->
               (Step a b :.: f) x -> (Step b c :.: g) y -> (Step a c :.: h) ns
combineStepDown next (Comp (Step upper)) (Comp (Step lower)) = Comp . Step $
  \h -> let (m ,upperf) = upper h
            (l, lowerf) = lower m
        in  (l, next upperf lowerf)   

combineStepUp :: (f x -> g y -> h ns) ->
               (Step b c :.: f) x -> (Step a b :.: g) y -> (Step a c :.: h) ns
combineStepUp next (Comp (Step upper)) (Comp (Step lower)) = Comp . Step $ 
  \l -> let (m, lowerf) = lower l
            (h, upperf) = upper m
        in  (h, next upperf lowerf)   

combine3 upr lwr = fix (step1 . step2  . cNilStep) upr lwr
 where step1 next (Fix upr) 
                (Fix lwr) = 
         Fix $ combineStepDown next upr lwr
       step2 = combineStepUp

cfix f = fix f' 
  where f' n (Fix u) (Fix l) = Fix $ (f . cNilStep) n u l

combine5 upr lwr = cfix (step1 . step2) upr lwr
 where step1 next upr lwr = 
         combineStepDown next upr lwr
       step2 = combineStepUp

combine = cfix (combineStepDown . combineStepUp)









-- testing

                
type Layer2 a b a2 b2 = Fix (Step a b :.: Step a2 b2 :.: NilStep)

combineTest =
 do { (state0, state1, state2) <- initStates
    ; doc <- initDoc
    --; let (state0, state1, state2) = (0, 10, 20)
    --; let doc = "DOC"
    
    ; let layers =                lift layer0 state0 
                   `combine0` lift layer1 state1
                   `combine0` lift layer2 state2
                   :: Layer2 Document Rendering EditRendering EditDocument
                   -- :: Layer2 String String String String
    ; let (Fix (compPresentStep)) = layers
    ; let (Comp (Step presentStep)) = compPresentStep
    ; let (pres , Comp (Step interpretStep)) = presentStep $ doc
    ; let interpretStep = interpretStep
    ; print pres
    ; gesture <- getGesture
--    ; let gesture = "Gest"
    
    ; let (update::EditDocument, next) = interpretStep $ gesture
    
--    ; print update
    ; getChar
    ; return ()
    } 









