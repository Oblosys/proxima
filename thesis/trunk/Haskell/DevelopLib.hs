module DevelopLib where

import Layers


-- factorize types:

newtype PresStep' doc pres gest upd = 
            PresStep' (doc -> (TransStep' doc pres gest upd, pres))
newtype TransStep' doc pres gest upd = 
            TransStep' (gest -> (PresStep' doc pres gest upd, upd))

type Layer doc pres gest upd = PresStep doc pres gest upd  

newtype PresStep doc pres gest upd = 
            PresStep {presStep :: Step TransStep doc pres gest upd}
newtype TransStep gest upd doc pres = 
            TransStep {transStep :: Step PresStep gest upd doc pres}

newtype Step1 a b c d = Step1 (a -> (Step2 c d a b, b))
newtype Step2 a b c d = Step2 (a -> (Step1 c d a b, b))

type Step nextstep a b c d = (a ->(nextstep c d a b, b))

newtype Step1F a b c d = Step1F (Step Step2 a b c d)
newtype Step2F a b c d = Step2F (Step Step1 a b c d)

newtype Step1' a b c d e f = Step1' (a -> (Step2' a b c d e f, b))
newtype Step2' a b c d e f = Step2' (a -> (Step3' a b c d e f, b))
newtype Step3' a b c d e f = Step3' (a -> (Step1' a b c d e f, b))


type F' nextstep a b c d e f = (a ->(nextstep c d e f a b, b))

newtype Step1F' a b c d e f = Step1F' (F' Step2' a b c d e f)
newtype Step2F' a b c d e f = Step2F' (F' Step3' a b c d e f)
newtype Step3F' a b c d e f = Step3F' (F' Step1' a b c d e f)




-- constructor destructor stuff


-------- factorize lift
{-
-- dpp lift:
lift'''' :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift'''' simple state = presStep state 
 where presStep state = 
         PresStep ( 
           \doc -> let ((mapping,state), pres) = present simple state doc                                         
                   in (pres, TransStep 
                               (\gest -> let (state', upd) = translate simple (mapping, state) gest                     
                                         in  (upd, presStep state')
                               ))
                  )
-- all steps on one level: no longer access horizontals using scope, all explicitly passed.
-- TODO: get rid of previous lift, because this one is clearer. Scoping was used when horizontal parameters
-- between subsequent layer function weren't of equal type yet.
-}

lift1 :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift1 simple state = presStep state 
 where presStep state = PresStep $ 
           \doc -> let ((mapping,state), pres) = present simple state doc                                         
                   in  (transStep (mapping,state), pres)
       transStep (mapping,state) = TransStep $
           \gest -> let (state', upd) = translate simple (mapping, state) gest                     
                    in  (presStep state', upd)

-- don't mention types after 1 because they don't change
-- horizontal and vertical args and res are uniform now (vertical already were)

lift2 :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift2 simple state = step1 state 
 where step1 horArg = PresStep $ 
           \vertArg -> let (horRes, vertRes) = present simple horArg vertArg                                         
                       in  (step2 horRes, vertRes)
       step2 horArg = TransStep $
           \vertArg -> let (horRes, vertRes) = translate simple horArg vertArg                     
                       in  (step1 horRes, vertRes)


-- still references to each other in let. So now pass next step as parameter

--lift3 simple state = lifted state 
-- where lifted = (step1 (step2 lifted)) 



lift3 :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
--lift3 simple state = (step1 (step2 (lift3 simple))) state 
lift3 simple = (step1.step2) (lift3 simple)
 where step1 next horArg = PresStep $ 
           \vertArg -> let (horRes, vertRes) = present simple horArg vertArg                                         
                       in  (next horRes, vertRes)
       step2 next horArg = TransStep $
           \vertArg -> let (horRes, vertRes) = translate simple horArg vertArg                     
                       in  (next horRes, vertRes)
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
lift4 simple = fix $ step1 . step2 
 where step1 next horArg = PresStep $ 
           \vertArg -> let (horRes, vertRes) = present simple horArg vertArg                                         
                       in  (next horRes, vertRes)
       step2 next horArg = TransStep $
           \vertArg -> let (horRes, vertRes) = translate simple horArg vertArg                     
                       in  (next horRes, vertRes)

-- Get rid of constructor and destructor applications, and explicit layer function calls, all are params

lift5 :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift5 simple = fix $ step1 PresStep (present simple) 
                  . step2 TransStep (translate simple) 
 where step1 pack layerF next horArg = pack $ 
           \vertArg -> let (horRes, vertRes) = layerF horArg vertArg                                         
                       in (next horRes, vertRes)
       step2 pack layerF next horArg = pack $
           \vertArg -> let (horRes, vertRes) = layerF horArg vertArg                     
                    in  (next horRes, vertRes)

-- now step1 and step2 are the same and we use a generic liftStep


fix a = let fixa = a fixa
        in  fixa


-- not final because pack will be a 3 tuple in the final bit.
liftStep :: ((vArg -> (nStep, vRes)) -> step ) ->
            (hArg -> vArg -> (hRes,vRes)) -> (hRes -> nStep) -> hArg -> step
liftStep pack layerF next horArg = pack $
    \vertArg -> let (horRes, vertRes) = layerF horArg vertArg                     
                in  (next horRes, vertRes)

lift6 :: Simple state mapping doc pres gest upd ->
         state -> Layer doc pres gest upd
lift6 simple = 
  fix $ liftStep PresStep (present simple) 
      . liftStep TransStep (translate simple)

---- now the same process for combine:

-- my first combine:

combine0 :: Layer high med emed ehigh -> Layer med low elow emed -> 
             Layer high low elow ehigh
combine0 = step1
 where step1 (PresStep upr) (PresStep lwr) = PresStep $ \high ->                                                                    
                    let (uprTrans, med) = upr high
                        (lwrTrans, low) = lwr med
                    in  (step2 uprTrans lwrTrans, low)
       step2 (TransStep upr) (TransStep lwr) = TransStep $ \elow ->
                    let (lwrPres', emed) = lwr elow
                        (uprPres', ehigh) = upr emed
                    in  (step1 uprPres' lwrPres', ehigh)
 
-- rename variables.

combine1 :: Layer high med emed ehigh -> Layer med low elow emed -> 
             Layer high low elow ehigh
combine1 = step1
 where step1 (PresStep upr) (PresStep lwr) = PresStep $
           \high -> let (nextUpr, med) = upr high
                        (nextLwr, low) = lwr med
                    in  (step2 nextUpr nextLwr, low)
       step2 (TransStep upr) (TransStep lwr) = TransStep $
           \low ->  let (nextLwr, med) = lwr low
                        (nextUpr, high) = upr med
                    in  (step1 nextUpr nextLwr, high)
-- reference each other so next step as parameter

combine2 :: Layer high med emed ehigh -> Layer med low elow emed -> 
             Layer high low elow ehigh
combine2 = step1 (step2 combine2)
 where step1 nextStep (PresStep upr) (PresStep lwr) = PresStep $
           \high -> let (nextUpr, med) = upr high
                        (nextLwr, low) = lwr med
                    in  (nextStep nextUpr nextLwr, low)
       step2 nextStep (TransStep upr) (TransStep lwr) = TransStep $
           \low ->  let (nextLwr, med) = lwr low
                        (nextUpr, high) = upr med
                    in  (nextStep nextUpr nextLwr, high)
-- use fix

combine3 :: Layer high med emed ehigh -> Layer med low elow emed -> 
             Layer high low elow ehigh
combine3 = fix $ step1 . step2
 where step1 nextStep (PresStep upr) (PresStep lwr) = PresStep $
           \high -> let (nextUpr, med) = upr high
                        (nextLwr, low) = lwr med
                    in  (nextStep nextUpr nextLwr, low)
       step2 nextStep (TransStep upr) (TransStep lwr) = TransStep $
           \low ->  let (nextLwr, med) = lwr low
                        (nextUpr, high) = upr med
                    in  (nextStep nextUpr nextLwr, high)

-- get rid of constructors and in this case also destructors. Destr. are pattern matching instead of
-- combine ... (Constr layer) we will use combine unpack x = ... unpack x
-- problem with type system. so double destructor pack in 3 tuple with constr.

combine4 :: Layer high med emed ehigh -> Layer med low elow emed -> 
            Layer high low elow ehigh
combine4 = fix $ step1 (PresStep,presStep,presStep) 
               . step2 (TransStep,transStep,transStep) 
 where step1 (pack, unpackU, unpackL) nextStep upr lwr = pack $
           \high -> let (nextUpr, med) = (unpackU upr) high
                        (nextLwr, low) = (unpackL lwr) med
                    in  (nextStep nextUpr nextLwr, low)
       step2 (pack, unpackU, unpackL) nextStep upr lwr = pack $
           \low ->  let (nextLwr, med) = (unpackL lwr) low
                        (nextUpr, high) = (unpackU upr) med
                    in  (nextStep nextUpr nextLwr, high)
-- factorize up and down. now up and down don't contain spec. for the step, so can be used by any step

combine5 :: Layer high med emed ehigh -> Layer med low elow emed -> 
            Layer high low elow ehigh
combine5 = fix $ combineStepDown (PresStep,presStep,presStep) 
               . combineStepUp (TransStep,transStep,transStep) 

combineStepDown :: ( (h -> (nStepC, l)) -> stepC 
                   , stepU -> h -> (nStepU, m)
                   , stepL -> m -> (nStepL, l)) -> 
                   (nStepU -> nStepL -> nStepC) -> stepU -> stepL -> stepC
combineStepDown (pack, unpackU, unpackL) nextStep upr lwr = pack $
    \high -> let (nextUpr, med) = (unpackU upr) high
                 (nextLwr, low) = (unpackL lwr) med
             in  (nextStep nextUpr nextLwr, low)

combineStepUp :: ( (l -> (nStepC, h)) -> stepC 
                 , stepU -> m -> (nStepU,h )
                 , stepL -> l -> (nStepL, m)) -> 
                 (nStepU -> nStepL -> nStepC) -> stepU -> stepL -> stepC
combineStepUp (pack, unpackU, unpackL) nextStep upr lwr = pack $
    \low -> let (nextLwr, med) = (unpackL lwr) low
                (nextUpr, high) = (unpackU upr) med
            in  (nextStep nextUpr nextLwr, high)



-- Done factorizing. 
-- figure contains the entire library together with fix, and slightly modified liftStep that 
-- gets a 3-tuple instead of just a constructor.


liftStep' :: ((a -> (b,c)) -> d,e,f) ->
             (g -> a -> (h,c)) -> (h -> b) -> g -> d
liftStep' (pack, _, _) layerF next horArg = pack $
    \vertArg -> let (horRes, vertRes) = layerF horArg vertArg                     
                in  (next horRes, vertRes)


-- by defining unpack = (cons, unpack, unpack) for each step type, definitions of lift and combine become:

{-
dictPresStep = (PresStep, presStep, presStep)
dictTransStep = (TransStep, transStep, transStep)

lift simple=
  fix $ liftStep' dictPresStep (present simple) 
      . liftStep' dictTransStep (translate simple)

combine = 
  fix $ combineStepDown dictPresStep
      . combineStepUp dictTransStep
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
combineStepUpq packunpack nextStep upr lwr = pack $
    \low -> let (med, nextLwr) = (unpack lwr) low
                (high, nextUpr) = (unpack upr) med
            in  (high, nextStep nextUpr nextLwr)
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

