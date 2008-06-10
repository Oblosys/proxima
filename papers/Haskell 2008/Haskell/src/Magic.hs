{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
-----------------------------------------------------------------------------------------
{-| Module      : Magic
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Magic where
import Layers


--- from lib

fix :: (a->a) -> a
fix a = let fixa = a fixa
        in  fixa

liftStep :: LayerFn hArg vArg hRes vRes -> (hRes -> g ns) -> hArg -> (Step d vArg vRes :.: g) ns
liftStep f next horArgs = Comp . Step $ 
  \vArg -> let (vertRes, horRes) = f horArgs vArg
           in  (vertRes, next horRes)

lfix f = fix f' where f' n = Fix . (f . lNilStep) n

lNilStep next hRes = NilStep $ next hRes


cfix  f = fix f' 
  where f' n (Fix u) (Fix l) = Fix $ f n u l

combineStepDown :: (f x -> g y -> h ns) ->
                   (Step Down a b :.: f) x ->
                   (Step Down b c :.: g) y ->
                   (Step Down a c :.: h) ns
combineStepDown next (Comp (Step upper)) 
                     (Comp (Step lower)) = Comp . Step $
  \h -> let (m ,upperf) = upper h
            (l, lowerf) = lower m
        in  (l, next upperf lowerf)   

combineStepUp :: (f x -> g y -> h ns) ->
                 (Step Up b c :.: f) x ->
                 (Step Up a b :.: g) y ->
                 (Step Up a c :.: h) ns
combineStepUp next (Comp (Step upper))
                   (Comp (Step lower)) = Comp . Step $ 
  \l -> let (m, lowerf) = lower l
            (h, upperf) = upper m
        in  (h, next upperf lowerf)   

unStep (Comp (Step step)) = step
unNil (NilStep step) = step



newtype Fix f = Fix (f (Fix f))

infixr :.:
newtype (:.:) f g ns  = Comp (f (g ns))

newtype NilStep t = NilStep t

--

newtype Step dir a b ns = Step (a -> (b, ns))
data Up 
data Down 


class Comp (cmp :: * -> *) r c | cmp -> r c where
  comp :: cmp t -> r -> c

instance Comp (NilStep) (b->res) (b->res)  where
  comp cmp r = r  

instance Comp g (a->res) cmp =>
         Comp (f :.: g) (y->res) ((a->y) -> cmp) where
  comp cmp r = \ab -> comp (rightType cmp) (r.ab)

rightType :: (f :.: g) t -> g t
rightType = undefined

compose c = comp c id



class App (cmp :: * -> *) f fx r | cmp f -> fx r  where
  app :: cmp t -> f -> fx -> r

instance App (NilStep) (a->b) a b  where
  app cmp f a = f a

instance App g (a->b) d e =>
         App (Step dr ar rs :.: g) (a->b) 
              (((hRes -> g ns) -> hArg -> 
                (Step dr vArg vRes :.: g) ns) ->d) 
             (LayerFn hArg vArg hRes vRes ->e) where
  
  app cmp f fx = \lf -> (app (rightType cmp) f
                             (fx (liftStep lf))) 


class ResType f res | f -> res where
  resType :: f -> res
  resType = undefined

instance ResType (Fix ct) (ct t)
  
instance ResType f r => ResType (a -> f) r

-- derived sig is not accepted :-(  
{-
genericLift :: ( ResType f (cmp t)
               , Comp cmp (a1 -> a1) c
               , App cmp
                   (((a -> NilStep (Fix f1)) -> a -> f1 (Fix f1)) -> a -> Fix f1)
                     c
                     f) => f
-}


-- combine
class Combine (cmp :: * -> *) t f | cmp t -> f where
  combineC :: cmp t -> f

instance Combine NilStep t ((u -> l -> c) -> 
          (NilStep u) -> (NilStep l) -> NilStep c) where
  combineC _ = \next (NilStep u) (NilStep l) ->
                 NilStep (next u l) 
 
instance (Combine c ct ( (ut -> lt -> ct) ->
                        u ut -> l lt-> c ct) ) =>
         Combine (Step Down a r :.: c) ct
                 ((ut -> lt -> ct) ->
                  (Step Down a m :.: u) ut -> 
                  (Step Down m r :.: l) lt -> 
                  (Step Down a r :.: c) ct) where
  combineC cmp = \next u l ->
    combineStepDown (combineC (rightType cmp) next) u l

instance (Combine c ct ( (ut -> lt -> ct) ->
                        u ut -> l lt-> c ct) ) =>
         Combine (Step Up a r :.: c) ct
                 ((ut -> lt -> ct) -> 
                  (Step Up m r :.: u) ut -> 
                  (Step Up a m :.: l) lt -> 
                  (Step Up a r :.: c) ct) where
  combineC cmp = \next f g ->
    combineStepUp (combineC (rightType cmp) next) f g

 -- derived sig is not accepted, but this one is: (replace comp by f)
genericCombine :: (Combine f t ( (Fix t1 -> Fix t2 -> Fix f) ->
                                 t1 (Fix t1) -> t2 (Fix t2) ->
                                 f (Fix f))
                  , ResType (Fix t1 -> Fix t2 -> Fix f) (f t)
                  ) =>
                  Fix t1 -> Fix t2 -> Fix f
genericCombine = cfix (combineC (resType genericCombine))

-- combine that uses first arg for recursion:
{-
class Combine (cmp :: * -> *) t f | cmp t -> f where
  combineC :: cmp t -> f

instance Combine NilStep u ((u -> l -> c) -> 
          (NilStep u) -> (NilStep l) -> NilStep c) where
  combineC _ = \next (NilStep u) (NilStep l) ->
                 NilStep (next u l) 
 
instance (Combine u ut ( (ut -> lt -> ct) ->
                        u ut -> l lt-> c ct) ) =>
         Combine (Step Down a m :.: u) ut
                 ((ut -> lt -> ct) ->
                  (Step Down a m :.: u) ut -> 
                  (Step Down m r :.: l) lt -> 
                  (Step Down a r :.: c) ct) where
  combineC cmp = \next u l ->
    combineStepDown (combineC (rightType cmp) next) u l

instance (Combine u ut ( (ut -> lt -> ct) ->
                        u ut -> l lt-> c ct) ) =>
         Combine (Step Up m r :.: u) ut
                 ((ut -> lt -> ct) -> 
                  (Step Up m r :.: u) ut -> 
                  (Step Up a m :.: l) lt -> 
                  (Step Up a r :.: c) ct) where
  combineC cmp = \next f g ->
    combineStepUp (combineC (rightType cmp) next) f g

 -- derived sig is not accepted, but this one is: (replace comp by f)

genericCombine :: (Combine t1 (Fix t1) ( (Fix t1 -> Fix t2 -> Fix f) ->
                                 t1 (Fix t1) -> t2 (Fix t2) ->
                                 f (Fix f))
                  ) =>
                  Fix t1 -> Fix t2 -> Fix f

genericCombine l1 l2 = cfix (combineC (unFix l1)) l1 l2

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x
-}



-- testing



type Layer dc prs gst upd = 
  Fix (Step Up dc prs :.: Step Down gst upd :.: NilStep)

                
lift :: Simple state map doc pres gest upd ->
               state -> Layer doc pres gest upd

lift smpl = genericLift (present smpl) (interpret smpl)

main layer1 layer2 layer3 =
 do { (state1, state2, state3) <- initStates
    ; doc <- initDoc 

    ; let layers = lift layer1 state1 `genericCombine` 
                   lift layer2 state2 `genericCombine`
                   lift layer3 state3
 --                  :: Layer Document Rendering EditRendering EditDocument
    ; editLoop layers doc
    }

editLoop (Fix presentStep) doc = 
 do { let (pres , interpretStep) = 
            unStep presentStep $ doc
    
    ; showRendering pres
    ; gesture <- getGesture
    
    ; let (update, presentStep') =
            unStep interpretStep $ gesture
    
    ; let doc' = updateDocument update doc
    ; 
    ; editLoop (unNil presentStep') doc'
    }

type Layer2 a b a2 b2 = Fix (Step Down a b :.: Step Up a2 b2 :.: NilStep)

combineTest =
 do { print testAppMap 
    ; print testResMap
    ; getChar
    ; (state0, state1, state2) <- initStates
    ; doc <- initDoc
    --; let (state0, state1, state2) = (0, 10, 20)
    --; let doc = "DOC"
    ; let lift :: Simple state map doc pres gest upd ->
                  state -> Layer2 doc pres gest upd
          lift l = genericLift (present l) (interpret l)
    
    ; let layers =                lift layer0 state0 
                   `genericCombine` lift layer1 state1
                   `genericCombine` lift layer2 state2
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
    ; return ()
    } 



isA 'a' = True
isA _   = False

testAppMap = (appMap two (\x y -> (x,y)) (\x -> [x])) 'a' 'b' 
testResMap = (resMap two (\(x,y) -> (y,x)) (\x y -> (x,y))) True 'b'

genericLift = app (resType genericLift) lfix 
                  (compose (resType genericLift))

genericLift' = resMap (resType genericLift) lfix $
               appMap (resType genericLift)  
                  (compose (resType genericLift)) liftStep


class ResMap (cmp :: * -> *) f r r' | cmp f  ->  r' r  where
  resMap :: cmp t -> f -> r -> r'

instance ResMap (NilStep) (r->r') r r'  where
  resMap cmp fm r = fm r

instance ResMap g fm f f' =>
         ResMap (s :.: g) fm (x -> f) (x -> f')  where
  resMap cmp fm r = \x -> resMap (rightType cmp) fm (r x)


class AppMap (cmp :: * -> *) f fm  r | cmp f -> fm r  where
  appMap :: cmp t -> f -> fm -> r

instance AppMap (NilStep) f fm f  where
  appMap cmp f _ = f

instance AppMap g f (x->y) r =>
         AppMap (s :.: g) 
              (y -> f) (x->y) (x ->r) where
  appMap cmp f fm = \x -> appMap (rightType cmp) (f (fm x)) fm
                                     

zero = undefined :: NilStep t
one = undefined :: (Step Down a b :.: NilStep) t
two = undefined :: (Step Down a b  :.: Step Down a b  :.: NilStep) t
