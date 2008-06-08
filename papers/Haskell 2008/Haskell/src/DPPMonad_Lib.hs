{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module DPPMonad_Lib where

import Layers hiding (LayerFn, Simple (..))
-- monads

fix :: (a->a) -> a
fix a = let fixa = a fixa
        in  fixa

type LayerFn m horArgs vertArg horRess vertRes =
       horArgs -> vertArg -> m (vertRes, horRess)

newtype Fix m f = Fix (f m (Fix m f))

newtype (:.:) f g m ns  = Comp (f m (g m ns))

newtype Step a b m ns = Step { unStep :: (a -> m (b, ns)) }


lfix f = fix f' 
 where f' n =  Fix . f n

lcomp f g = (\n s -> Comp $ f n s) . g

liftStep :: Monad m => (hArg -> vArg-> m (vRes, hRes)) -> 
            (hRes -> ns) -> hArg -> Step vArg vRes m ns
liftStep f next horArgs = Step $ 
  \vArg -> do { (vertRes, horRes) <- f horArgs vArg
              ; return (vertRes, next horRes)
              }


cfix f = fix f' 
  where f' n (Fix u) (Fix l) = Fix $ f n u l

ccomp f g = f' . g
 where f' m (Comp u) (Comp l) = Comp $ f m u l

combineStepDown :: Monad m => (nsU -> nsL -> nsC) ->
                   Step h md m nsU -> Step md l m nsL ->
                   Step h l m nsC
combineStepDown next (Step upper) (Step lower) = Step $
  \h -> do { (m ,upperf) <- upper h
           ; (l, lowerf) <- lower m
           ; return (l, next upperf lowerf)   
           }

combineStepUp :: Monad m => (nsU -> nsL -> nsC) ->
                 Step md h m nsU -> Step l md m nsL ->
                 Step l h m nsC
combineStepUp next (Step upper) (Step lower) = Step $ 
  \l -> do { (m, lowerf) <- lower l
           ; (h, upperf) <- upper m
           ; return (h, next upperf lowerf)
           }


data Simple m state map doc pres gest upd =
       Simple { present ::   LayerFn m state doc (map, state) pres
              , interpret :: LayerFn m (map, state) gest state upd
              }

type Layer m doc pres gest upd = 
       Fix m (Step doc pres :.: Step gest upd)

{- lift :: Monad m =>
        Simple m state map doc pres gest upd ->
        state -> Layer m doc pres gest upd
-}
lift simple = lfix $  liftStep (present simple) 
              `lcomp` liftStep (interpret simple) 

{-
combine :: Monad m => Layer m high med emed ehigh ->
                      Layer m med low elow emed -> 
                      Layer m high low elow ehigh
-}
combine = cfix $ combineStepDown `ccomp` combineStepUp









editLoop :: Layer IO Document Rendering EditRendering EditDocument -> Document -> IO ()
editLoop (Fix (Comp presentStep)) doc = 
 do { (pres , interpretStep) <- unStep presentStep $ 
                                  doc
    
    ; showRendering pres
    ; gesture <- getGesture
    
    ; (update, presentStep') <- unStep interpretStep $ 
                                  gesture
    
    ; let doc' = updateDocument update doc
    
    ; editLoop presentStep' doc'
    }



main layer0 layer1 layer2 =
 do { (state0, state1, state2) <- initStates
    ; doc <- initDoc 
    ; let layers =           lift layer0 state0 
                   `combine` lift layer1 state1
                   `combine` lift layer2 state2
    ; editLoop layers doc
    }




