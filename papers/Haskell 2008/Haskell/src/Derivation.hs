{-# OPTIONS_GHC -fglasgow-exts -fallow-incoherent-instances -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module Derivation where

import Layers
--import DPP_Lib
--import DPPClass_Lib 

import Char 


newtype InfTupleExplicit a b = InfTupleExplicit (a, (b, InfTupleExplicit a b))

newtype TupleStep a inft = I (a, inft)

type InfTuple2 a b = Fix ((TupleStep a) :.: (TupleStep b))
type InfTuple3 a b c = Fix ((TupleStep a) :.: (TupleStep b) :.: (TupleStep c))


inftup2 :: InfTuple2 Char Bool 
inftup2 = Fix $ Comp (I ('c', I (True, inftup2)))

       
          

lft f = I . f

comp f g  = (\x -> Comp $ (f . g) x)

fix :: (a->a) -> a
fix a = let fixa = a fixa
        in  fixa

fFix f = let fixf = Fix . f $ fFix f
         in  fixf

inftup2Compositional :: InfTuple2 Char Bool
inftup2Compositional = fFix $ lft a1 `comp` lft a2
 where a1 n = ('c', n)
       a2 n = (True, n)

inftup3Compositional :: InfTuple3 Char Bool Int
inftup3Compositional = fFix $ lft a1 `comp` lft a2 `comp` lft a3
 where a1 n = ('c', n)
       a2 n = (True, n)
       a3 n = (8::Int, n)


compterm :: ((TupleStep Bool) :.: ((TupleStep Char) :.: (TupleStep Int))) ()
compterm = (Comp . I)  ( True
                       , (Comp . I) ('a', I (0, ()))
                       )


newtype Fix f = Fix (f (Fix f))

newtype (:.:) f g ns  = Comp (f (g ns))

newtype Step a b ns = Step { unStep :: (a -> (b, ns)) }
newtype DownStep a b ns = DownStep { unDownStep :: (a -> (b, ns)) }

type Layer doc pres gest upd = 
       Fix (Step doc pres :.: Step gest upd)
           

lift0, lift1, lift2, lift3, lift4, lift5 ::
  Simple state map doc pres gest upd ->
  state -> Layer doc pres gest upd

lift0 simple state = step1 state 
 where step1 hArg = Fix . Comp . Step $
         \vArg -> let (pres, hRes) = 
                       present simple hArg vArg
                  in  (pres, step2 hRes)
       step2 hArg = Step $
         \vArg -> let (upd, hRes) = 
                        interpret simple hArg vArg
                  in  (upd, step1  hRes)

-- factorize out next step

lift1 simple state =
  step1 (step2 (lift1 simple)) state
 where step1 next hArg = Fix . Comp . Step $
         \vArg -> let (pres, hRes) = 
                       present simple hArg vArg
                  in  (pres, next hRes)
       step2 next hArg = Step $
         \vArg -> let (upd, hRes) = 
                        interpret simple hArg vArg
                  in  (upd, next hRes)


-- now use liftStep to remove Step

liftStep f next horArgs = Step $ 
  \vArg -> let (vertRes, horRes) = f horArgs vArg
           in  (vertRes, next horRes)

lift2 simple state =
  step1 (step2 (lift2 simple)) state
 where step1 next hArg = Fix . Comp $
           liftStep (present simple) next hArg
       step2 next hArg =
           liftStep (interpret simple) next hArg


-- capture recursion with fix
lift3 simple = fix $ step1 . step2
 where step1 next hArg = Fix . Comp $
           liftStep (present simple) next hArg
       step2 next hArg = 
           liftStep (interpret simple) next hArg

-- remove Fix with specifal fix
lfix f = fix f' 
 where f' n =  (Fix . f n) 

lift4 simple = lfix $ step1 . step2
 where step1 next hArg = Comp $
           liftStep (present simple) next hArg
       step2 next hArg = 
           liftStep (interpret simple) next hArg



-- remove Comp with special composition
lcomp :: (b -> t -> (f (g ns))) -> 
         (a->b) -> 
         a -> t -> ((f :.: g) ns)
lcomp f g = (\n s -> Comp $ f n s) . g

--lcomp' f g  = (\n s -> Comp ((f . g) n s))

lift5 simple = lfix $ step1 `lcomp` step2
 where step1 next args = 
           liftStep (present simple) next args
       step2 next args = 
           liftStep (interpret simple) next args

-- rewrite, dropping parameters


lift :: Simple state map doc pres gest upd ->
        state -> Layer doc pres gest upd
lift simple = lfix $  liftStep (present simple) 
              `lcomp` liftStep (interpret simple) 
           




combine0, combine2, combine3, combine4, combine5, combine6 ::
  Layer high med emed ehigh ->
  Layer med low elow emed -> 
  Layer high low elow ehigh

combine0 upr lwr = step1 upr lwr
 where step1 (Fix (Comp (Step upr))) 
                (Fix (Comp (Step lwr))) = 
         Fix . Comp . Step $ 
         \high -> let (med, uprIntr) = upr high
                      (low, lwrIntr) = lwr med
                  in  (low, step2 uprIntr lwrIntr)
       step2 (Step upr) (Step lwr) = Step $
         \low -> let (med, lwrPres) = lwr low
                     (high, uprPres) = upr med
                 in  (high, step1 uprPres lwrPres)
 

-- pass next step as arg and rewrite to fix


combine2 = fix $ (step1 . step2) 
 where step1 next (Fix (Comp (Step upr)))
                     (Fix (Comp (Step lwr))) =
         Fix . Comp . Step $ 
         \high -> let (med, uprIntr) = upr high
                      (low, lwrIntr) = lwr med
                  in  (low, next uprIntr lwrIntr)
       step2 next (Step upr) (Step lwr) = Step  $
         \low -> let (med, lwrPres) = lwr low
                     (high, uprPres) = upr med
                 in  (high, next uprPres lwrPres)

-- now we capture the up down with these functions:

combineStepDown :: (x -> y -> ns) ->
               Step a b x -> Step b c y -> Step a c ns
combineStepDown next (Step upper) (Step lower) = Step $
  \h -> let (m ,upperf) = upper h
            (l, lowerf) = lower m
        in  (l, next upperf lowerf)   

combineStepUp :: (x -> y -> ns) -> 
             Step b c x -> Step a b y -> Step a c ns
combineStepUp next (Step upper) (Step lower) = Step $ 
  \l -> let (m, lowerf) = lower l
            (h, upperf) = upper m
        in  (h, next upperf lowerf)   


combine3 = fix $ (step1 . step2) 
 where step1 next (Fix (Comp upr)) (Fix (Comp lwr)) =
         Fix . Comp  $ combineStepDown next upr lwr  
       step2 = combineStepUp



combine4 = fix (\n (Fix u) (Fix l)-> Fix $
                   (step1 . step2) n u l) 
 where step1 next (Comp upr) (Comp lwr) = Comp $ 
          combineStepDown next upr lwr  
       step2 = combineStepUp

--------------------------------------------------------

cfix  f = fix f' 
  where f' n (Fix u) (Fix l) = Fix $ f n u l


combine5 = cfix (step1 . step2)
 where step1 next (Comp upr) (Comp lwr) = Comp $ 
          combineStepDown next upr lwr  
       step2 = combineStepUp

ccomp f g = f' . g
 where f' m (Comp u) (Comp l) = Comp $ f m u l

combine6 = cfix $ step1 `ccomp` step2
 where step1 next upr lwr =  
          combineStepDown next upr lwr  
       step2 = combineStepUp



combine :: Layer high med emed ehigh ->
           Layer med low elow emed -> 
           Layer high low elow ehigh
combine = cfix $ combineStepDown `ccomp` combineStepUp

{-
editLoop :: forall a . Layer Document Rendering (EditRendering a) (EditDocument a) -> Document -> IO ()
editLoop (Fix compPresentStep) doc = 
 do { let presentStep = decomp compPresentStep
    ; let (pres :: Rendering , interpretStep) = unStep presentStep $ doc
    
    ; showRendering pres
    ; gesture :: EditRendering a <- getGesture
    
    ; let ( update :: EditDocument a , 
            presentStep' :: Layer Document Rendering (EditRendering a) (EditDocument a)
           ) = unStep interpretStep $ gesture
    
    ; let doc' :: Document = updateDocument update doc
    
    ; editLoop presentStep' doc'
    }
-}
 -- no type sigs
editLoop :: Layer Document Rendering (EditRendering) (EditDocument) -> Document -> IO ()
editLoop (Fix (compPresentStep)) doc = 
 do { let presentStep = decomp2 compPresentStep
    ; let (pres , interpretStep) = unStep presentStep $ doc
    
    ; showRendering pres
    ; gesture <- getGesture
    
    ; let (update, presentStep') = unStep interpretStep $ gesture
    
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

main4 state2 = 
 do { let layers4 = liftl4 foursteplayer state2

--    ; let Fix comps = layers4
--          editStep = decomp4 comps
    ; editLoop4 layers4 (undefined :: Arrangement)
    }

combinel4 = cfix $ combineStepDown `ccomp` combineStepUp `ccomp` combineStepDown `ccomp` combineStepUp 

liftl4 l4 = lfix $ liftStep (present1 l4) `lcomp` liftStep (interpret1 l4) `lcomp`
                   liftStep (present2 l4) `lcomp` liftStep (interpret2 l4)


--(liftl4,combinel4) = afix $ (liftStep (present1 l4), combineStepDown)
foursteplayer :: FourStep State2 Mapping2 Arrangement Rendering (EditRendering) (EditArrangement)
foursteplayer = undefined

data FourStep state map doc pres gest upd =
       FourStep { present1 ::   LayerFn state doc (map, state) pres
                 , interpret1 :: LayerFn (map, state) gest state upd
                 , present2 ::   LayerFn state doc (map, state) pres
                 , interpret2 :: LayerFn (map, state) gest state upd
                 }

unfixCompL f (Fix x) = f x
unfixComp = unfixCompL $ \(Comp (Comp (Comp ztep))) -> ztep


-- Explicit ones:

--instance Decomp (f :.: g :.: h) t  (f (g (h t))) where
--  decomp (Comp (Comp f)) = f

--instance Decomp (f :.: g :.: h :.: i) t (f (g (h (i t)))) where
--  decomp (Comp (Comp (Comp f))) = f


type Layer4 doc pres gest upd = 
       Fix (Step doc pres :.: Step gest upd :.: Step doc pres :.: Step gest upd)

editLoop4 :: forall a . Layer4 Arrangement Rendering (EditRendering) (EditArrangement) -> Arrangement -> IO ()
editLoop4 (Fix comps) doc = 
 do { let Step presentStep = decomp4 comps
    ; let (pres::Rendering, Step interpretStep) = presentStep doc

--    ; showRendering pres
    ; gesture :: EditRendering  <- getGesture
    
    ; let (update::EditArrangement, Step presentStep2) = interpretStep gesture
    
    ; let doc'   
             = undefined update 
    ; let (pres2 :: Rendering, Step interpret2Step) = presentStep2 doc'

--    ; showRendering pres2
    ; gesture2 :: EditRendering <- getGesture
    
    ; let (update :: EditArrangement, presentStep') = interpret2Step gesture2
    
    
    ; editLoop4  presentStep' (undefined :: Arrangement)
    }









--- A class to get rid of Fix and Comp applications

{-
-- old comment: "This works, but without functional dependency, all polymorphism must be removed with explicit
-- type sigs." now it does not work though..
class Decomp compx comp where
  decomp :: compx -> comp

instance Decomp ((f :.: g) t)  (f (g t)) where
  decomp (Comp f) = f
  
instance Decomp ((f :.: g :.: h) t)  (f (g (h t))) where
  decomp (Comp (Comp f)) = f

instance Decomp ((p :.: q :.: r :.: s) t)  (p (q (r (s t)))) where
  decomp (Comp (Comp (Comp f))) = f
-}
decomp4 :: ((p :.: q :.: r :.: s) t) -> (p (q (r (s t))))    
decomp4 (Comp (Comp (Comp step))) = step
--decomp4 = decomp

--decomp3 :: (((:-) :.: p :.: q :.: r) t) -> (p (q (r t)))    
data (:-) a = CompNil a

term :: (Maybe :.: (:-)) ()
term = Comp $ Just (CompNil ())

decomp3 :: (((:-) :.: p :.: r) t) -> (p (r t))
decomp3 (Comp (Comp (CompNil step))) = step
decomp2 (Comp s) = s

-- this one does work!
-- fundeps seem impossible now :-(


class Decomp comp t decomp where -- | comp t -> decomp where
  decomp :: comp t -> decomp

instance Decomp f t (f t)  where
  decomp f = f

instance Decomp f (g t) h  => Decomp (f :.: g) t h  where
  decomp (Comp f) = decomp f
--instance Decomp (f :.: g) t  (f (g t)) where
--  decomp (Comp f) = f


{-
--instance Decomp (:-) t t where
--  decomp (CompNil x) = x
  -}


{-
class Decomp compx x comp (g :: * -> *) | comp -> g where
  decomp :: compx x -> comp

instance Decomp (f :.: g) t  (f (g t)) g where
  decomp (Comp f) = f

instance Decomp f (g t) h  i => Decomp (f :.: g) t  h g  where
  decomp (Comp f) = decomp f
-}
--decompTest :: (f (),g (),t) ->  (((:-) :.: f :.: g) t) -> (f (g t))
--decompTest dummy = decomp
--decompTest dummy = decomp (undefined :: ((:-) :.: f :.: g) t) :: (f (g t))
--decompTest3 = decomp (undefined :: (f :.: g :.: h) t) :: (f (g (h t)))

newtype NilT t = NilT t

class Combine (comp :: * -> *) t f | comp t -> f where
  combineC :: f

instance Combine NilT t ((NilT x) -> (NilT y) -> (x -> y -> t) -> NilT t) where
  combineC = \(NilT x) (NilT y) next ->  NilT (next x y) 

instance Combine (Step a r :.:  h) t 
                 ((Step a m :.: f) ft -> 
                  (Step m r :.: g) gt -> 
                  (f ft -> g gt -> h t) -> (Step a r :.: h) t) where
  combineC = \(Comp (Step f)) (Comp (Step g)) next -> 
                 Comp $ Step $ \h -> let (m ,upperf) = f h
                                         (l, lowerf) = g m
                                     in  (l, next upperf lowerf)   

{- Let's see if we can make lift automatically -}
--lcomp :: (b -> t -> (f (g ns))) -> 
--         (a->b) -> 
--         a -> t -> ((f :.: g) ns)




{-

gliftc1 r f = liftStep f 
gliftc2 r f g = liftStep f `lcomp'` liftStep g
gliftc3 r f g h = liftStep f `lcomp'` liftStep g `lcomp` liftStep h
gliftc4 r f g h i = liftStep f `lcomp'` liftStep g `lcomp` liftStep h `lcomp'` liftStep i
-}

f1 a = a 
f2 a b = a . b
f3 a b c = a . b . c

f0' r = r
f1' r a = (r . a)
f2' r b = f1' (r . b)
f3' r c = f2' (r . c)


class Comp (ctype :: * -> *) r f c where
  compose :: ctype t -> r -> f -> c

instance Comp (f :.: g)  (b->res) (a->b) (a->res)  where
  compose ctype r f = r . f 

instance (Comp f  (b -> res) (a -> b) (a -> res)) =>
         Comp (f :.: g) (y->res) (b->y) ((a->b) -> (a->res)) where
  compose ctype r f = compose (getLeftType ctype) (r . f)  

--fixCompose :: Comp x => Fix x -> 
fixCompose fix = let Fix f = fix
                 in  compose (getCType fix)

getCType :: Fix ctype -> ctype t
getCType = undefined
                 
getLeftType :: (f :.: g) t -> (f t')
getLeftType = undefined 

test x = compose (undefined :: (x :.: y) t)  :: (b->res) -> (a ->b) -> (a->res)

test2 = compose (undefined :: (x :.: y :.: z) t) :: (c -> d) -> (b->c) -> (a ->b) -> (a->d)
test'  = fixCompose (undefined :: Layer doc pres gest upd) :: (b->c) -> (a ->b) -> (a->c)

dppMain = do { print $ compPeano (undefined :: ((:-) :.: x :.: y) ())
             ; print $ comp2
                          (toUpper) (toUpper) 'a'
             ; print $ composeC (Succ Zero)   -- :: (c->res) -> (b->c) -> (a ->b) -> (a->res)) 
                          (toUpper) (toUpper) (toUpper) 'a'
             ; getLine
             }

data Zero = Zero deriving Show
data Succ a = Succ a deriving Show
comp2 = composeC Zero
 
-- composeC Zero = \f g -> f . g
-- composeC (Succ Zero) = \f g h -> f . g . h
-- etc. 
{-
class CompC num r f c bereik | num r -> c, r -> bereik where
  composeC :: num -> r -> f -> c

instance CompC Zero (b->res) (a->b) (a->res) res where
  composeC ctype r f = r . f 

instance forall a b y res n .
         CompC n  (b -> res) (a -> b) (a -> res) res  =>
         CompC (Succ n) (y->res) (b->y) ((a->b) -> (a->res)) res where
  composeC (Succ n) r f = let comp = composeC :: n -> (b -> res) -> (a -> b) -> (a -> res)
                          in  comp n (r . f)  
  
 -}
-- ?? this also works now?


class CompC num r f c | num r -> c where
  composeC :: num -> r -> f -> c

instance CompC Zero (b->res) (a->b) (a->res)  where
  composeC ctype r f = r . f 

instance forall a b y res n .
         CompC n  (b -> res) (a -> b) (a -> res)  =>
         CompC (Succ n) (y->res) (b->y) ((a->b) -> (a->res)) where
  composeC (Succ n) r f = let comp = composeC :: n -> (b -> res) -> (a -> b) -> (a -> res)
                          in  comp n (r . f)  




liftfinal' :: Simple state map doc pres gest upd ->
         state -> Layer doc pres gest upd
liftfinal' simple = glift2 (present simple) (interpret simple) 

liftl4' l4 = glift4 (present1 l4) (interpret1 l4) (present2 l4) (interpret2 l4) 

glift1  = gfix1 (gliftc1 id)
glift2  = gfix2 (gliftc2 id)
glift3  = gfix3 (gliftc3 id)
glift4  = gfix4 (gliftc4 id)

gfix0 f = lfix f
gfix1 f x = gfix0 (f x)
gfix2 f x = gfix1 (f x) 
gfix3 f x = gfix2 (f x) 
gfix4 f x = gfix3 (f x) 
--gfix1 c f = gfix0 c id f 

gliftc1 r f = r . liftStep f 
gliftc2 r f = gliftc1 (r `lcomp` liftStep f)
gliftc3 r f = gliftc2 (r `lcomp` liftStep f)
gliftc4 r f = gliftc3 (r `lcomp` liftStep f)
--glift4 = undefined


class CompPeano comp peano | comp -> peano where
  compPeano :: comp -> peano
  
instance CompPeano ((:-) t) Zero where
   compPeano (CompNil a) = Zero
  
instance CompPeano (f (g t)) n => CompPeano ((f:.:g) t) (Succ n) where
   compPeano (Comp f) = Succ (compPeano f) 
