{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
-----------------------------------------------------------------------------------------
{-| Module      : AutoLiftCombine
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module AutoLiftCombine where

import Char
import Layers -- hiding (layer0, layer1, layer2)

newtype Fix f = Fix (f (Fix f))

infixr :.:
newtype (:.:) f g ns  = Comp (f (g ns))

newtype UpStep a b ns = UpStep (a -> (b, ns))
newtype DownStep a b ns = DownStep (a -> (b, ns))

newtype NilStep t = NilStep t


class Comp (comp :: * -> *) r c | comp -> r c where
  compose :: comp t -> r -> c

instance Comp (NilStep) (b->res) (b->res)  where
  compose comp r = r  

instance forall g y res cmp f a b .
         Comp g (a->res) cmp =>
         Comp (f :.: g) (y->res) ((a->y) -> cmp) where
  compose comp r = \atob -> compose (undefined :: g t')  (r . atob)


composeTest = compose (undefined :: (f :.: NilStep) t) id isUpper 'C'


f1 :: Char -> Int
f1 c = 31

f2 :: Char -> Bool -> Int
f2 c b = 12

zero = undefined :: NilStep t
one = undefined :: (DownStep a b :.: NilStep) t
two = undefined :: (DownStep a b  :.: DownStep a b  :.: NilStep) t
three = undefined :: (DownStep a b  :.: DownStep a b  :.: DownStep a b  :.: NilStep) t

app2 :: (a->b) -> (x -> y -> a) -> (x -> y -> b)
app2 f f2 = \x y -> f $ f2 x y
app1 f f1 = \x -> f $ f x


appTest = 
 do { print $ "" -- (app two (+1) f2) 'a' True
    ; print $ "" -- (app one (+1) f1) 'a'
    }

class App (comp :: * -> *) f fx r | comp f -> fx r  where
  app :: comp t -> f -> fx -> r

instance App (NilStep) (a->b) a b  where
  app comp f a = f a

{- -- not step-specific: 
instance forall g y res cmp ff f a b c d e.
         App g          (a->b)  d      e =>
         App (f :.: g) (a->b) (c->d) (c->e) where
  
             app comp    f      fx = \x -> (app (undefined :: g t') f (fx x)) 

-}
-- step specific
instance forall g y res cmp ff f a b c d e ar rs hArg vArg hRes vRes ns .
         App g          (a->b)  d      e =>
         App (UpStep ar rs :.: g) (a->b) (((hRes -> g ns) -> hArg -> (UpStep vArg vRes :.: g) ns) ->d) 
                                         (LayerFn hArg vArg hRes vRes ->e) where
  
             app comp    f      fx = \x -> (app (undefined :: g t') f (fx (liftUpStep x))) 

instance forall g y res cmp ff f a b c d e ar rs hArg vArg hRes vRes ns .
         App g          (a->b)  d      e =>
         App (DownStep ar rs :.: g) (a->b) (((hRes -> g ns) -> hArg -> (DownStep vArg vRes :.: g) ns) ->d) 
                                         (LayerFn hArg vArg hRes vRes ->e) where
  
             app comp    f      fx = \x -> (app (undefined :: g t') f (fx (liftDownStep x))) 
--fixCompose :: Comp x => Fix x -> 
fixCompose fix = let Fix f = fix
                 in  compose (getCType fix)

getCType :: Fix ctype -> ctype t
getCType = undefined
                 
getLeftType :: (f :.: g) t -> (f t')
getLeftType = undefined 

--liftL2 :: forall state map doc pres gest upd t. 
--          Simple state map doc pres gest upd ->
--          state -> Layer2 doc pres gest upd
liftL2 simple = liftt  (present simple) (interpret simple)
{-
liftL1 :: forall state map doc pres gest upd t. 
          Simple state map doc pres gest upd ->
          state -> Fix (DownStep doc pres :.: NilStep)
liftL1 simple = liftt' (liftDownStep (undefined :: LayerFn state doc state pres)) 
-}
{-
liftL0 :: forall state map doc pres gest upd t. 
          Simple state map doc pres gest upd ->
          state -> Fix (DownStep doc pres :.: NilStep)
liftL0 simple = liftt (liftDownStep (undefined :: LayerFn state doc state pres)) 
-}
-- this is possible! but we want to obtain one automatically. Seems we can only get it by parameterizing
-- our own result type.. Need anothre class ResType
liftt = app comptype  genericLift (compose comptype id)
  where comptype = resType liftt
        
               
class ResType f res | f -> res where
  resType :: f -> res

instance ResType (Fix ct) (ct t) where
  resType _ = undefined
  
instance ResType f r => ResType (a -> f) r where
  resType _ = undefined
  
{-
instance ResType g b t =>
         ResType (f :.: g) (a -> b) t where
  resType _ = undefined
-}
liftt' = app (resType liftt') genericLift (compose (resType liftt') id)

{-
-- directly is very tricky
class Lift (comp :: * -> *) t f | comp t -> f where
  liftC :: comp t -> f
  

instance Lift NilStep t (t -> NilStep t) where
  liftC _ = NilStep

{-
liftStep f next horArgs = Step $ 
  \vArg -> let (vertRes, horRes) = f horArgs vArg
           in  (vertRes, next horRes)

-}

instance Lift (DownStep a r :.: f) t (LayerFn ha va hr vr-> t -> (DownStep va vr :.: f) t) where
  liftC _ layerFn n = Comp . DownStep $ undefined
  
magicLift :: Fix NilStep
magicLift = Fix res
  where res = liftC (undefined :: NilStep t) magicLift
-}

class Combine (comp :: * -> *) t f | comp t -> f where
  combineC :: comp t -> f

instance Combine NilStep t ((x -> y -> t) -> (NilStep x) -> (NilStep y) -> NilStep t) where
  combineC _ = \next (NilStep x) (NilStep y) ->  NilStep (next x y) 
 
instance  (Combine h t ((ft -> gt -> t) -> f ft -> g gt-> h t)) =>
         Combine (DownStep a r :.:  h) t 
                 ((ft -> gt -> t) ->
                  (DownStep a m :.: f) ft -> 
                  (DownStep m r :.: g) gt -> 
                  (DownStep a r :.: h) t) where
  combineC _ = \next f g -> combineStepDown (combineC (undefined :: h t) next) f g

instance  (Combine h t ((ft -> gt -> t) -> f ft -> g gt -> h t)) =>
         Combine (UpStep a r :.:  h) t 
                 ((ft -> gt -> t) -> 
                  (UpStep m r :.: f) ft -> 
                  (UpStep a m :.: g) gt -> 
                  (UpStep a r :.: h) t) where
  combineC _ = \next f g -> combineStepUp (combineC (undefined :: h t) next) f g



comb0 = combineC :: NilStep t -> (x -> y -> t) -> (NilStep x) -> (NilStep y) -> NilStep t


comb1 = combineC :: (DownStep a r :.: NilStep) t -> (ft -> gt -> t) -> (DownStep a m :.: NilStep) ft -> (DownStep m r :.: NilStep) gt ->
                    (DownStep a r :.: NilStep) t

comb2 = combineC :: (DownStep a r :.: DownStep a2 r2 :.: NilStep) t -> (ft -> gt -> t) -> (DownStep a m :.: DownStep a2 m2 :.: NilStep) ft -> 
                    (DownStep m r :.: DownStep m2 r2 :.: NilStep) gt ->
                    (DownStep a r :.: DownStep a2 r2 :.: NilStep) t


--magicCombine (Fix l1) (Fix l2) = Fix $ combineC (resType magicCombine) magicCombine l1 l2
magicCombine = cfix (combineC (resType magicCombine))
 where x = resType magicCombine

type Layer0 t = (NilStep t)

type Layer1 a b = Fix (DownStep a b :.: NilStep)

type Layer2 a b a2 b2 = Fix (DownStep a b :.: UpStep a2 b2 :.: NilStep)

{- 
-- this one loops GHC
combinel0 :: Fix NilStep  -> Fix NilStep -> Fix NilStep
combinel0 = magicCombine
-}
       
combinel1 :: Layer1 a b -> Layer1 b c -> Layer1 a c
combinel1 = magicCombine

--combinel2 :: Layer2 a b b2 c2 -> Layer2 b c a2 b2 -> Layer2 a c a2 c2
--combinel2 = magicCombine
    
{-
present ::   LayerFn state doc (map, state) pres
interpret :: LayerFn (map, state) gest state upd
               
-}  
{-  
layer0 :: Simple Int String String String String String
layer0 = Simple { present = \state doc -> ("p1{"++show state++"}"++doc, ("map",state+1)) 
                , interpret = \(map, state) pres -> ("i1{"++show state++"}"++pres,state)
                }
layer1 :: Simple Int String String String String String
layer1 = Simple { present = \state doc -> ("p2{"++show state++"}"++doc, ("map",state+2)) 
                , interpret = \(map, state) pres -> ("i2{"++show state++"}"++pres,state)
                }
layer2 :: Simple Int String String String String String
layer2 = Simple { present = \state doc -> ("p3{"++show state++"}"++doc, ("map",state+3)) 
                , interpret = \(map, state) pres -> ("i3{"++show state++"}"++pres,state)
                }
 -}
     
                
       
combineTest =
 do { appTest
    ; (state0, state1, state2) <- initStates
    ; doc <- initDoc
    --; let (state0, state1, state2) = (0, 10, 20)
    --; let doc = "DOC"
    ; let magicLift :: Simple state map doc pres gest upd ->
                       state -> Layer2 doc pres gest upd
          magicLift l = liftt (present l) (interpret l)
    
    ; let layers =            magicLift layer0 state0 
                   `magicCombine` magicLift layer1 state1
                   `magicCombine` magicLift layer2 state2
                   :: Layer2 Document Rendering EditRendering EditDocument
                   -- :: Layer2 String String String String
    ; let (Fix (compPresentStep)) = layers
    --; let (Comp (DownStep presentStep)) = compPresentStep
    ; let presentStep = unCompStep compPresentStep
    --; let (pres , Comp (DownStep interpretStep)) = presentStep $ doc
    ; let (pres :: Rendering , compInterpretStep) = presentStep $ doc
    ; let interpretStep = unCompStep compInterpretStep
    ; print pres
    ; gesture <- getGesture
--    ; let gesture = "Gest"
    
    ; let (update::EditDocument, next) = interpretStep $ gesture
    
--    ; print update
    ; getChar
    ; return ()
    } 
    
unCompStep (Comp step) = unstep step


fix :: (a->a) -> a
fix a = let fixa = a fixa
        in  fixa



-- derivation for lift with new types
lift0 :: Simple state map doc pres gest upd ->
        state -> Fix (DownStep  doc pres  :.: UpStep gest upd :.: NilStep)
lift0 simple state = step1 state 
 where step1 hArg = Fix . Comp $ DownStep $
         \vArg -> let (pres, hRes) = 
                       present simple hArg vArg
                  in  (pres, step2 hRes)
       step2 hArg = Comp . UpStep $
         \vArg -> let (upd, hRes) = 
                        interpret simple hArg vArg
                  in  (upd, nilStep hRes)
       nilStep hRes = NilStep $ step1 hRes
       
lift1 :: Simple state map doc pres gest upd ->
        state -> Fix (DownStep  doc pres  :.: UpStep gest upd :.: NilStep)

lift1 simple state =   step1 (step2 (nilStep (lift1 simple))) state
 where step1 next hArg = Fix . Comp $ DownStep $
         \vArg -> let (pres, hRes) = 
                       present simple hArg vArg
                  in  (pres, next hRes)
       step2 next hArg = Comp . UpStep $
         \vArg -> let (upd, hRes) = 
                        interpret simple hArg vArg
                  in  (upd, next  hRes)

nilStep next hRes = NilStep $ next hRes


lift2 simple state =  step1 (step2 (nilStep (lift1 simple))) state
 where step1 next hArg = Fix . Comp $ DownStep $
         \vArg -> let (pres, hRes) = 
                       present simple hArg vArg
                  in  (pres, next hRes)
       step2 next hArg = Comp . UpStep $
         \vArg -> let (upd, hRes) = 
                        interpret simple hArg vArg
                  in  (upd, next  hRes)
      
                  
liftUpStep :: LayerFn hArg vArg hRes vRes -> (hRes -> g ns) -> hArg -> (UpStep vArg vRes :.: g) ns
liftUpStep f next horArgs = Comp . UpStep $ 
  \vArg -> let (vertRes, horRes) = f horArgs vArg
           in  (vertRes, next horRes)

liftDownStep :: LayerFn hArg vArg hRes vRes -> (hRes -> g ns) -> hArg -> (DownStep vArg vRes :.: g) ns
liftDownStep f next horArgs = Comp . DownStep $ 
  \vArg -> let (vertRes, horRes) = f horArgs vArg
           in  (vertRes, next horRes)

lift3 simple state =  step1 (step2 (nilStep (lift2 simple))) state
 where step1 next hArg = Fix $ liftDownStep (present simple)  next hArg
       step2 next hArg =       liftUpStep (interpret simple) next hArg

lift4 simple =  fix (step1 . step2 . nilStep)
 where step1 next hArg = Fix $ liftDownStep (present simple)  next hArg
       step2 next hArg =       liftUpStep (interpret simple) next hArg

lfix f = fix f' 
 where f' n =  (Fix . f n) 

lift5 simple =  lfix (step1 . step2 . nilStep)
 where step1 next hArg = liftDownStep (present simple)  next hArg
       step2 next hArg = liftUpStep (interpret simple) next hArg
       
lift6 simple =  lfix (liftDownStep (present simple) . liftUpStep (interpret simple) . nilStep)


genericLift f = lfix (f . nilStep)

lift7 :: Simple state map doc pres gest upd ->
         state -> Fix (DownStep  doc pres  :.: UpStep gest upd :.: NilStep)
lift7 simple =  genericLift  $ liftDownStep (present simple) . liftUpStep (interpret simple)

class Step res vArg vRes g where
  step :: LayerFn hArg vArg hRes vRes -> (hRes -> g ns) -> hArg -> (res :.: g) ns
 
instance Step (UpStep vArg vRes) vArg vRes g where
  step = liftUpStep

instance Step (DownStep vArg vRes) vArg vRes g where
  step = liftDownStep
  
class UnStep step where
  unstep :: step a b n -> a -> (b, n)

instance UnStep DownStep where
  unstep (DownStep s) = s
instance UnStep UpStep where
  unstep (UpStep s) = s
  
lift :: Simple state map doc pres gest upd ->
         state -> Layer2 doc pres gest upd
lift simple =  genericLift  $ step (present simple) . step (interpret simple)

-- or
infixr 8 .:
(.:) f g = step f . g
lift' :: Simple state map doc pres gest upd ->
         state -> Layer2 doc pres gest upd
lift' simple =  lfix $ present simple .: interpret simple .: nilStep
-- fix (present simple) (intepret simple)



combine0, combine1, combine2, combine3 :: --, combine2, combine3, combine4, combine5, combine6 ::
  Layer2 high med emed ehigh ->
  Layer2 med low elow emed -> 
  Layer2 high low elow ehigh 
combine0 upr lwr = step1 upr lwr
 where step1 (Fix (Comp (DownStep upr))) 
                (Fix (Comp (DownStep lwr))) = 
         Fix . Comp . DownStep $ 
         \high -> let (med, uprIntr) = upr high
                      (low, lwrIntr) = lwr med
                  in  (low, step2 uprIntr lwrIntr)
       step2 (Comp (UpStep upr)) (Comp ( UpStep lwr)) = Comp . UpStep $
         \low -> let (med, lwrPres) = lwr low
                     (high, uprPres) = upr med
                 in  (high, nilStep uprPres lwrPres)
       nilStep (NilStep u) (NilStep l) = NilStep $ step1 u l 

cNilStep next (NilStep u) (NilStep l) = NilStep $ next u l 
 
combine1 upr lwr = (step1 (step2 (cNilStep combine1))) upr lwr
 where step1 next (Fix (Comp (DownStep upr))) 
                (Fix (Comp (DownStep lwr))) = 
         Fix . Comp . DownStep $ 
         \high -> let (med, uprIntr) = upr high
                      (low, lwrIntr) = lwr med
                  in  (low, next uprIntr lwrIntr)
       step2 next (Comp (UpStep upr)) (Comp ( UpStep lwr)) = Comp . UpStep $
         \low -> let (med, lwrPres) = lwr low
                     (high, uprPres) = upr med
                 in  (high, next uprPres lwrPres)
 

combine2 upr lwr = fix (step1 . step2  . cNilStep) upr lwr
 where step1 next (Fix (Comp (DownStep upr))) 
                (Fix (Comp (DownStep lwr))) = 
         Fix . Comp . DownStep $ 
         \high -> let (med, uprIntr) = upr high
                      (low, lwrIntr) = lwr med
                  in  (low, next uprIntr lwrIntr)
       step2 next (Comp (UpStep upr)) (Comp ( UpStep lwr)) = Comp . UpStep $
         \low -> let (med, lwrPres) = lwr low
                     (high, uprPres) = upr med
                 in  (high, next uprPres lwrPres)

combineStepDown :: (f x -> g y -> h ns) ->
               (DownStep a b :.: f) x -> (DownStep b c :.: g) y -> (DownStep a c :.: h) ns
combineStepDown next (Comp (DownStep upper)) (Comp (DownStep lower)) = Comp . DownStep $
  \h -> let (m ,upperf) = upper h
            (l, lowerf) = lower m
        in  (l, next upperf lowerf)   

combineStepUp :: (f x -> g y -> h ns) ->
               (UpStep b c :.: f) x -> (UpStep a b :.: g) y -> (UpStep a c :.: h) ns
combineStepUp next (Comp (UpStep upper)) (Comp (UpStep lower)) = Comp . UpStep $ 
  \l -> let (m, lowerf) = lower l
            (h, upperf) = upper m
        in  (h, next upperf lowerf)   

combine3 upr lwr = fix (step1 . step2  . cNilStep) upr lwr
 where step1 next (Fix upr) 
                (Fix lwr) = 
         Fix $ combineStepDown next upr lwr
       step2 = combineStepUp

cfix  f = fix f' 
  where f' n (Fix u) (Fix l) = Fix $ f n u l

combine5 upr lwr = cfix (step1 . step2  . cNilStep) upr lwr
 where step1 next upr lwr = 
         combineStepDown next upr lwr
       step2 = combineStepUp


combine6 = cfix (combineStepDown . combineStepUp . cNilStep)

genericCombine f = cfix $ f . cNilStep

combine = genericCombine (combineStepDown . combineStepUp)

