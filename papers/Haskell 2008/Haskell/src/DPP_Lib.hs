{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module DPP_Lib where

fix :: (a->a) -> a
fix a = let fixa = a fixa
        in  fixa

type LayerFn horArgs vertArg horRess vertRes =
       horArgs -> vertArg -> (vertRes, horRess)

newtype Fix f = Fix (f (Fix f))

newtype (:.:) f g ns  = Comp (f (g ns))

newtype Step a b ns = Step (a -> (b, ns))





{-
lfix :: ((a -> Fix f) -> a -> f (Fix f)) -> a -> Fix f
lcomp :: (b -> t -> f (g ns)) -> (a -> b) -> a -> t -> (:.:) f g ns
cfix :: ((Fix t -> Fix t1 -> Fix f) -> t (Fix t) -> t1 (Fix t1) -> f (Fix f))
        -> Fix t
        -> Fix t1
        -> Fix f
ccomp :: (b -> t (t1 t2) -> t3 (t4 t5) -> f (g ns))
         -> (a -> b)
         -> a
         -> (:.:) t t1 t2
         -> (:.:) t3 t4 t5
         -> (:.:) f g ns
Prelude DPP_Lib>




-}




lfix f = fix f' 
 where f' n =  Fix . f n

--lcomp :: (b -> t -> (f (g ns))) -> 
--         (a->b) -> 
--         a -> t -> ((f :.: g) ns)
lcomp f g = (\n s -> Comp $ f n s) . g

liftStep :: (hArg -> vArg-> (vRes, hRes)) -> 
            (hRes -> ns) -> hArg -> Step vArg vRes ns
liftStep f next horArgs = Step $ 
  \vArg -> let (vertRes, horRes) = f horArgs vArg
           in  (vertRes, next horRes)


cfix f = fix f' 
  where f' n (Fix u) (Fix l) = Fix $ f n u l

ccomp f g = f' . g
 where f' m (Comp u) (Comp l) = Comp $ f m u l

combineStepDown :: (nsU -> nsL -> nsC) ->
                   Step h md nsU -> Step md l nsL ->
                   Step h l nsC
combineStepDown next (Step upper) (Step lower) = Step $
  \h -> let (m ,upperf) = upper h
            (l, lowerf) = lower m
        in  (l, next upperf lowerf)   

combineStepUp :: (nsU -> nsL -> nsC) ->
                 Step md h nsU -> Step l md nsL ->
                 Step l h nsC
combineStepUp next (Step upper) (Step lower) = Step $ 
  \l -> let (m, lowerf) = lower l
            (h, upperf) = upper m
        in  (h, next upperf lowerf)   


data Simple state map doc pres gest upd =
       Simple { present ::   LayerFn state doc (map, state) pres
              , interpret :: LayerFn (map, state) gest state upd
              }

type Layer doc pres gest upd = 
       Fix (Step doc pres :.: Step gest upd)

lift :: Simple state map doc pres gest upd ->
        state -> Layer doc pres gest upd
lift simple = lfix $  liftStep (present simple) 
              `lcomp` liftStep (interpret simple) 


combine :: Layer high med emed ehigh ->
           Layer med low elow emed -> 
           Layer high low elow ehigh
combine = cfix $ combineStepDown `ccomp` combineStepUp
