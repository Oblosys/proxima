module DPP_Lib where
-- obsolete, class version (DPPClass_Lib) is used in thesis.
{- 
type Step nextstap a0 .. a2n = ( a0 -> (a1, nextstep a2 .. a2n a0 a1))

newtype Step0 a0 .. a2n = Step0 (Step Step1 a0 .. a2n)
newtype Step1 a0 .. a2n = Step1 (Step Step2 a0 .. a2n)
 ..
newtype Stepn a0 .. a2n = Stepn (Step Step0 a0 .. a2n)

desStep0 = (Step0, \(Step0 f) -> f, \(Step0 f) -> f)
desStep0 = (Step1, \(Step1 f) -> f, \(Step1 f) -> f)
..
desStepn = (Stepn, \(Stepn f) -> f, \(Stepn f) -> f)

lift f0 .. fn = 
  fix $ f desStep0 f0 . f desStep1 f1 . ... . f desStepn fn
combine = 
  fix $ g desStep0 combineUp/Down . g desStep1 f1 CombineUpd/
        . ... . g desStepn combineUp/Down

-}


fix a = let fixa = a fixa
         in fixa

type LayerFunction horArgs vertArg horRess vertRes = 
       (horArgs -> vertArg -> (vertRes, horRess))

-- BEGIN: 
f (c,d,d') f k horArg =
  c $ \vertArg -> let (vertRes, horRes) = f horArg vertArg
                  in (vertRes, k horRes)

g (c, d, d') dir nextst upperst lowerst = dir c (d upperst) (d' lowerst) nextst

combineDown c upper lower comb = 
  c $ \h -> let (m ,upperf) = upper h
                (l, lowerf) = lower m
            in  (l, comb upperf lowerf)   

combineUp c upper lower comb = 
  c $ \l -> let (m, lowerf) = lower l
                (h, upperf) = upper m
            in  (h, comb upperf lowerf)   
-- END:
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
-- combineDown and combineUp only differ in order of layer args in rhs of let bindings:
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


