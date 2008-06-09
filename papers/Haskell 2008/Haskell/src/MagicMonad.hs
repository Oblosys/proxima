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

module MagicMonad where

import Layers hiding (LayerFn, Simple (..))

data Simple m state map doc pres gest upd =
       Simple { present ::   LayerFn m state doc (map, state) pres
              , interpret :: LayerFn m (map, state) gest state upd
              }


--- from lib

fix :: (a->a) -> a
fix a = let fixa = a fixa
        in  fixa

type LayerFn m horArgs vertArg horRess vertRes =
       horArgs -> vertArg -> m (vertRes, horRess)

liftStep :: Monad m => LayerFn m hArg vArg hRes vRes -> (hRes -> g m ns) -> hArg -> (Step d vArg vRes :.: g) m ns
liftStep f next horArgs = Comp . Step $ 
  \vArg -> do { (vertRes, horRes) <- f horArgs vArg
              ; return (vertRes, next horRes)
              }


lfix f = fix f' where f' n = Fix . (f . lNilStep) n

lNilStep next hRes = NilStep $ next hRes


cfix  f = fix f' 
  where f' n (Fix u) (Fix l) = Fix $ f n u l

combineStepDown :: Monad m => (f m x -> g m y -> h m ns) ->
                   (Step Down a b :.: f) m x ->
                   (Step Down b c :.: g) m y ->
                   (Step Down a c :.: h) m ns
combineStepDown next (Comp (Step upper))
                     (Comp (Step lower)) = Comp . Step $
  \h -> do { (m ,upperf) <- upper h
           ; (l, lowerf) <- lower m
           ; return (l, next upperf lowerf)   
           }


combineStepUp :: Monad m => (f m x -> g m y -> h m ns) ->
               (Step Up b c :.: f) m x ->
               (Step Up a b :.: g) m y ->
               (Step Up a c :.: h) m ns
combineStepUp next (Comp (Step upper))
                   (Comp (Step lower)) = Comp . Step $ 
  \l -> do { (m, lowerf) <- lower l
           ; (h, upperf) <- upper m
           ; return (h, next upperf lowerf)
           }

unStep (Comp (Step step)) = step
unNil (NilStep step) = step



newtype Fix m f = Fix (f m (Fix m f))

infixr :.:

newtype (:.:) f g (m :: * -> *) ns  = Comp (f m (g m ns))
-- kind sig because otherwise m may get * if there are no applications


newtype NilStep m t = NilStep t

newtype Step dir a b m ns = Step (a -> m (b, ns))

data Up 
data Down 



class Comp (cmp :: (* -> *) -> * -> *) r c | cmp -> r c where
  compose :: cmp m t -> r -> c

instance Comp (NilStep) (b->res) (b->res)  where
  compose cmp r = r  

instance Comp g (a->res) cmp =>
         Comp (f :.: g) (y->res) ((a->y) -> cmp) where
  compose cmp r = \ab -> compose (rightType cmp) (r.ab)

rightType :: (f :.: g) m t -> g m t
rightType = undefined

class App (cmp :: (* -> *) -> * -> *) f fx r | cmp f -> fx r  where
  app :: cmp m t -> f -> fx -> r

instance App (NilStep) (a->b) a b  where
  app cmp f a = f a



instance ( Monad m  
         , App g (a->b) d e ) =>
         App (Step dr ar rs :.: g) (a->b) 
              (((hRes -> g m ns) -> hArg -> 
                (Step dr vArg vRes :.: g) m ns) ->d) 
             (LayerFn m hArg vArg hRes vRes ->e) where
  
             app cmp f fx = \lf -> (app (rightType cmp) f
                                        (fx (liftStep lf))) 


class ResType f res | f -> res where
  resType :: f -> res
  resType = undefined

instance ResType (Fix m ct) (ct m t)
  
instance ResType f r => ResType (a -> f) r

genericLift = app (resType genericLift) lfix 
                  (compose (resType genericLift) id)

-- combine
class Combine (cmp :: (* -> *) -> * -> *) t f | cmp t -> f where
  combineC :: cmp m t -> f

instance Monad m => Combine NilStep t ((u -> l -> c) -> 
          (NilStep m u) -> (NilStep m l) -> NilStep m c) where
  combineC _ = \next (NilStep u) (NilStep l) ->
                 NilStep (next u l) 

instance ( Monad m
         , Combine c ct ( (ut -> lt -> ct) ->
                        u m ut -> l m lt-> c m ct) ) =>
         Combine (Step Down a r :.: c) ct
                 ((ut -> lt -> ct) ->
                  (Step Down a m' :.: u) m ut -> 
                  (Step Down m' r :.: l) m lt -> 
                  (Step Down a r :.: c) m ct) where
  combineC cmp = \next u l ->
    combineStepDown (combineC (rightType cmp) next) u l

instance ( Monad m
         , Combine c ct ( (ut -> lt -> ct) ->
                        u m ut -> l m lt-> c m ct) ) =>
         Combine (Step Up a r :.: c) ct
                 ((ut -> lt -> ct) -> 
                  (Step Up m' r :.: u) m ut -> 
                  (Step Up a m' :.: l) m lt -> 
                  (Step Up a r :.: c) m ct) where
  combineC cmp = \next f g ->
    combineStepUp (combineC (rightType cmp) next) f g

 -- derived sig is not accepted, but this one is: (replace comp by f)
{- non monadic
genericCombine :: (Combine f t ( (Fix t1 -> Fix t2 -> Fix f) ->
                                 t1 (Fix t1) -> t2 (Fix t2) ->
                                 f (Fix f))
                  , ResType (Fix t1 -> Fix t2 -> Fix f) (f t)
                  ) =>
                  Fix t1 -> Fix t2 -> Fix f
-}
genericCombine = cfix (combineC (resType genericCombine))







-- testing



type Layer m dc prs gst upd = 
  Fix m (Step Down dc prs :.: Step Up gst upd :.: NilStep)

                
lift :: Monad m => Simple m state map doc pres gest upd ->
               state -> Layer m doc pres gest upd
lift smpl = genericLift (present smpl) (interpret smpl)

main layer1 layer2 layer3 =
 do { (state1, state2, state3) <- initStates
    ; doc <- initDoc 

    ; let layers = lift layer1 state1 `genericCombine` 
                   lift layer2 state2 `genericCombine`
                   lift layer3 state3
    ; editLoop layers doc
    }

editLoop (Fix presentStep) doc = 
 do { (pres , interpretStep) <-
            unStep presentStep $ doc
    
    ; showRendering pres
    ; gesture <- getGesture
    
    ; (update, presentStep') <-
            unStep interpretStep $ gesture
    
    ; let doc' = updateDocument update doc
    ; 
    ; editLoop (unNil presentStep') doc'
    }
{-
type Layer2 a b a2 b2 = Fix (Step Down a b :.: Step Up a2 b2 :.: NilStep)

combineTest =
 do { (state0, state1, state2) <- initStates
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
    ; getChar
    ; return ()
    } 




-}