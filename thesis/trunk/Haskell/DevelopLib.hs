module DevelopLib where

import Layers


-- factorize types:

newtype PresStep' doc pres gest upd = 
            PresStep' (doc -> (IntrStep' doc pres gest upd, pres))
newtype IntrStep' doc pres gest upd = 
            IntrStep' (gest -> (PresStep' doc pres gest upd, upd))

type Layer doc pres gest upd = PresStep doc pres gest upd  

newtype PresStep doc pres gest upd = 
            PresStep {presStep :: Step IntrStep doc pres gest upd}
newtype IntrStep gest upd doc pres = 
            IntrStep {intrStep :: Step PresStep gest upd doc pres}

newtype Step1 a b c d = Step1 (a -> (Step2 c d a b, b))
newtype Step2 a b c d = Step2 (a -> (Step1 c d a b, b))

type Step nStep a b c d = (a ->(nStep c d a b, b))

newtype Step1F a b c d = Step1F (Step Step2 a b c d)
newtype Step2F a b c d = Step2F (Step Step1 a b c d)

newtype Step1' a b c d e f = Step1' (a -> (Step2' a b c d e f, b))
newtype Step2' a b c d e f = Step2' (a -> (Step3' a b c d e f, b))
newtype Step3' a b c d e f = Step3' (a -> (Step1' a b c d e f, b))


type F' nStep a b c d e f = (a ->(nStep c d e f a b, b))

newtype Step1F' a b c d e f = Step1F' (F' Step2' a b c d e f)
newtype Step2F' a b c d e f = Step2F' (F' Step3' a b c d e f)
newtype Step3F' a b c d e f = Step3F' (F' Step1' a b c d e f)




-- constructor destructor stuff


-------- factorize lift
{-
-- dpp lift:
lift'''' :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift'''' layer state = presStep state 
 where presStep state = 
         PresStep ( 
           \doc -> let ((mapping,state), pres) = present layer state doc                                         
                   in (pres, IntrStep 
                               (\gest -> let (state', upd) = (interpret layer) (mapping, state) gest                     
                                         in  (upd, presStep state')
                               ))
                  )
-- all steps on one level: no longer access horizontals using scope, all explicitly passed.
-- TODO: get rid of previous lift, because this one is clearer. Scoping was used when horizontal parameters
-- between subsequent layer function weren't of equal type yet.
-}

lift1 :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift1 layer state = presStep state 
 where presStep state = PresStep $ 
           \doc -> let ((mapping,state), pres) = (present layer) state doc                                         
                   in  (intrStep (mapping,state), pres)
       intrStep (mapping,state) = IntrStep $
           \gest -> let (state', upd) = (interpret layer) (mapping, state) gest                     
                    in  (presStep state', upd)

-- don't mention types after 1 because they don't change
-- horizontal and vertical args and res are uniform now (vertical already were)

lift2 :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift2 layer state = step1 state 
 where step1 horArg = PresStep $ 
           \vertArg -> let (horRes, vertRes) = (present layer) horArg vertArg                                         
                       in  (step2 horRes, vertRes)
       step2 horArg = IntrStep $
           \vertArg -> let (horRes, vertRes) = (interpret layer) horArg vertArg                     
                       in  (step1 horRes, vertRes)


-- still references to each other in let. So now pass next step as parameter

--lift3 layer state = lifted state 
-- where lifted = (step1 (step2 lifted)) 



lift3 :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift3 layer state = (step1 (step2 (lift3 layer))) state 
--lift3 layer = (step1.step2) (lift3 layer)
 where step1 nStep horArg = PresStep $ 
           \vertArg -> let (horRes, vertRes) = (present layer) horArg vertArg                                         
                       in  (nStep horRes, vertRes)
       step2 nStep horArg = IntrStep $
           \vertArg -> let (horRes, vertRes) = (interpret layer) horArg vertArg                     
                       in  (nStep horRes, vertRes)
-- use fix and .

f' a b c = g c
 where g = f0 (f1 g)
       f0 = undefined
       f1 = undefined

f'' a b  = fix $ f0.f1
 where f0 = undefined
       f1 = undefined

--

lift4 :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift4 layer = fix $ step1 . step2 
 where step1 nStep horArg = PresStep $ 
           \vertArg -> let (horRes, vertRes) = (present layer) horArg vertArg                                         
                       in  (nStep horRes, vertRes)
       step2 nStep horArg = IntrStep $
           \vertArg -> let (horRes, vertRes) = (interpret layer) horArg vertArg                     
                       in  (nStep horRes, vertRes)

-- Get rid of constructor and destructor applications, and explicit layer function calls, all are params

lift5 :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift5 layer = fix $ step1 PresStep (present layer) 
                  . step2 IntrStep (interpret layer) 
 where step1 pack layerF nStep horArg = pack $ 
           \vertArg -> let (horRes, vertRes) = layerF horArg vertArg                                         
                       in (nStep horRes, vertRes)
       step2 pack layerF nStep horArg = pack $
           \vertArg -> let (horRes, vertRes) = layerF horArg vertArg                     
                    in  (nStep horRes, vertRes)

-- now step1 and step2 are the same and we use a generic liftStep


fix a = let fixa = a fixa
        in  fixa


-- not final because pack will be a 3 tuple in the final bit.
liftStep :: ((vArg -> (nStep, vRes)) -> step ) ->
            (hArg -> vArg -> (hRes,vRes)) -> (hRes -> nStep) -> hArg -> step
liftStep pack layerF nStep horArg = pack $
    \vertArg -> let (horRes, vertRes) = layerF horArg vertArg                     
                in  (nStep horRes, vertRes)

lift6 :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift6 layer = 
  fix $ liftStep PresStep (present layer) 
      . liftStep IntrStep (interpret layer)

---- now the same process for combine:

-- my first combine:

combine :: Layer high med emed ehigh -> Layer med low elow emed -> 
             Layer high low elow ehigh
combine = step1
 where step1 (PresStep hghr) (PresStep lwr) = PresStep $ 
           \high -> let (hghrIntr, med) = hghr high
                        (lwrIntr, low) = lwr med
                    in  (step2 hghrIntr lwrIntr, low)
       step2 (IntrStep hghr) (IntrStep lwr) = IntrStep $
           \elow -> let (lwrPres, emed) = lwr elow
                        (hghrPres, ehigh) = hghr emed
                    in  (step1 hghrPres lwrPres, ehigh)
 
-- rename variables.

combine1 :: Layer high med emed ehigh -> Layer med low elow emed -> 
             Layer high low elow ehigh
combine1 = step1
 where step1 (PresStep hghr) (PresStep lwr) = PresStep $
           \high -> let (nextHghr, med) = hghr high
                        (nextLwr, low) = lwr med
                    in  (step2 nextHghr nextLwr, low)
       step2 (IntrStep hghr) (IntrStep lwr) = IntrStep $
           \low ->  let (nextLwr, med) = lwr low
                        (nextHghr, high) = hghr med
                    in  (step1 nextHghr nextLwr, high)
-- reference each other so next step as parameter

combine2 :: Layer high med emed ehigh -> Layer med low elow emed -> 
             Layer high low elow ehigh
combine2 = step1 (step2 combine2)
 where step1 nStep (PresStep hghr) (PresStep lwr) = PresStep $
           \high -> let (nextHghr, med) = hghr high
                        (nextLwr, low) = lwr med
                    in  (nStep nextHghr nextLwr, low)
       step2 nStep (IntrStep hghr) (IntrStep lwr) = IntrStep $
           \low ->  let (nextLwr, med) = lwr low
                        (nextHghr, high) = hghr med
                    in  (nStep nextHghr nextLwr, high)
-- use fix

combine3 :: Layer high med emed ehigh -> Layer med low elow emed -> 
             Layer high low elow ehigh
combine3 = fix $ step1 . step2
 where step1 nStep (PresStep hghr) (PresStep lwr) = PresStep $
           \high -> let (nextHghr, med) = hghr high
                        (nextLwr, low) = lwr med
                    in  (nStep nextHghr nextLwr, low)
       step2 nStep (IntrStep hghr) (IntrStep lwr) = IntrStep $
           \low ->  let (nextLwr, med) = lwr low
                        (nextHghr, high) = hghr med
                    in  (nStep nextHghr nextLwr, high)

-- get rid of constructors and in this case also destructors. Destr. are pattern matching instead of
-- combine ... (Constr layer) we will use combine unpack x = ... unpack x
-- problem with type system. so double destructor pack in 3 tuple with constr.

combine4 :: Layer high med emed ehigh -> Layer med low elow emed -> 
            Layer high low elow ehigh
combine4 = fix $ step1 (PresStep,presStep,presStep) 
               . step2 (IntrStep,intrStep,intrStep) 
 where step1 (pack, unpackH, unpackL) nStep hghr lwr = pack $
           \high -> let (nextHghr, med) = (unpackH hghr) high
                        (nextLwr, low) = (unpackL lwr) med
                    in  (nStep nextHghr nextLwr, low)
       step2 (pack, unpackH, unpackL) nStep hghr lwr = pack $
           \low ->  let (nextLwr, med) = (unpackL lwr) low
                        (nextHghr, high) = (unpackH hghr) med
                    in  (nStep nextHghr nextLwr, high)
-- factorize up and down. now up and down don't contain spec. for the step, so can be used by any step

combine5 :: Layer high med emed ehigh -> Layer med low elow emed -> 
            Layer high low elow ehigh
combine5 = fix $ combineStepDown (PresStep,presStep,presStep) 
               . combineStepUp (IntrStep,intrStep,intrStep) 

combineStepDown :: ( (h -> (nStepC, l)) -> stepC 
                   , stepH -> h -> (nStepH, m)
                   , stepL -> m -> (nStepL, l)) -> 
                   (nStepH -> nStepL -> nStepC) -> stepH -> stepL -> stepC
combineStepDown (pack, unpackH, unpackL) nStep hghr lwr = pack $
    \high -> let (nextHghr, med) = (unpackH hghr) high
                 (nextLwr, low) = (unpackL lwr) med
             in  (nStep nextHghr nextLwr, low)

combineStepUp :: ( (l -> (nStepC, h)) -> stepC 
                 , stepH -> m -> (nStepH,h )
                 , stepL -> l -> (nStepL, m)) -> 
                 (nStepH -> nStepL -> nStepC) -> stepH -> stepL -> stepC
combineStepUp (pack, unpackH, unpackL) nStep hghr lwr = pack $
    \low -> let (nextLwr, med) = (unpackL lwr) low
                (nextHghr, high) = (unpackH hghr) med
            in  (nStep nextHghr nextLwr, high)



-- Done factorizing. 
-- figure contains the entire library together with fix, and slightly modified liftStep that 
-- gets a 3-tuple instead of just a constructor.


liftStep' :: ((a -> (b,c)) -> d,e,f) ->
             (g -> a -> (h,c)) -> (h -> b) -> g -> d
liftStep' (pack, _, _) layerF nStep horArg = pack $
    \vertArg -> let (horRes, vertRes) = layerF horArg vertArg                     
                in  (nStep horRes, vertRes)


-- by defining unpack = (cons, unpack, unpack) for each step type, definitions of lift and combine become:

{-
dictPresStep = (PresStep, presStep, presStep)
dictIntrStep = (IntrStep, intrStep, intrStep)

lift layer=
  fix $ liftStep' dictPresStep (present layer) 
      . liftStep' dictIntrStep (interpret layer)

combine = 
  fix $ combineStepDown dictPresStep
      . combineStepUp dictIntrStep
-}

-- END:


---- Old stuff:
--Still has different order for nstep&res args

-- quantified combineStep
-- won't work, result type h i j k parameters are different for up and down step.
--- aargh!
{-
combineStepUpq :: (forall a b c d h i j k . (a -> (b, g c d a b ) -> f a b c d 
                                            ,f h i j k -> h  -> (i, g j k h i)
                                            )) 
                  -> (g a b h i-> g c a j k ->n) -> f h i a b  -> f j k c a  -> f  
combineStepUpq packunpack nStep hghr lwr = pack $
    \low -> let (med, nextLwr) = (unpack lwr) low
                (high, nextHghr) = (unpack hghr) med
            in  (high, nStep nextHghr nextLwr)
 where (pack,unpack) = packunpack
-}

{-
f (c,d,u) f k horArg = c $ \vertArg -> 
           let (vertRes, horRes) = f horArg vertArg
           in (vertRes, k horRes)
-}

{-
g (c, desd, u)  dir nextst upperst lowerst = 
  let d = u desd
  in  dir c (d upperst) (d lowerst) nextst
-}

-- alternative definition:
-- combineStepDown and combineStepUp only differ in order of layer args in rhs of let bindings:
-- combine c upper lower comb = 
--  c $ \x -> let (m, lowerf) = lower x 
--                (r, upperf) = upper m
--            in  (r, comb upperf lowerf)
-- flip args of combine as well as args of comb. double use requires nasty type sig for forall  
-- type sig is not really an option, what is the type??? 

--g' :: ((a -> (b,c)) -> d,e,f) ->
--      (forall m n o p. (m,n) -> (o,p)) ->
--      ((a -> (i,g)) -> (i -> (b,h)) -> c) -> g -> h -> d
g' (c, d, d') dir  dir' comb upperst lowerst = 
    c $ \a -> let (first, second) = dir (upperst, lowerst)
                  (m, firstf) = first a
                  (r, secondf) = second m
                  (upperf, lowerf) = dir' (secondf, firstf)
              in  (r, comb upperf lowerf)

dirDown (upper, lower) = (upper, lower)

dirUp   (upper, lower) = (lower, upper)

