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


lfix f = fix f'  where f' n =  (Fix . f n) 

nilStep next hRes = NilStep $ next hRes

genericLift f = lfix (f . nilStep)

cfix  f = fix f' 
  where f' n (Fix u) (Fix l) = Fix $ f n u l


combineStepDown :: (f x -> g y -> h ns) ->
               (Step Down a b :.: f) x -> (Step Down b c :.: g) y -> (Step Down a c :.: h) ns
combineStepDown next (Comp (Step upper)) (Comp (Step lower)) = Comp . Step $
  \h -> let (m ,upperf) = upper h
            (l, lowerf) = lower m
        in  (l, next upperf lowerf)   

combineStepUp :: (f x -> g y -> h ns) ->
               (Step Up b c :.: f) x -> (Step Up a b :.: g) y -> (Step Up a c :.: h) ns
combineStepUp next (Comp (Step upper)) (Comp (Step lower)) = Comp . Step $ 
  \l -> let (m, lowerf) = lower l
            (h, upperf) = upper m
        in  (h, next upperf lowerf)   




newtype Fix f = Fix (f (Fix f))

infixr :.:
newtype (:.:) f g ns  = Comp (f (g ns))

data Up 
data Down 
newtype Step d a b ns = Step (a -> (b, ns))

newtype NilStep t = NilStep t

rightType :: (f :.: g) t -> g t
rightType = undefined

class Comp (comp :: * -> *) r c | comp -> r c where
  compose :: comp t -> r -> c

instance Comp (NilStep) (b->res) (b->res)  where
  compose comp r = r  

instance forall g y res cmp f a b .
         Comp g (a->res) cmp =>
         Comp (f :.: g) (y->res) ((a->y) -> cmp) where
  compose comp r = \atob -> compose (rightType comp)  (r . atob)


class App (comp :: * -> *) f fx r | comp f -> fx r  where
  app :: comp t -> f -> fx -> r

instance App (NilStep) (a->b) a b  where
  app comp f a = f a

instance App g (a->b) d e =>
         App (Step dr ar rs :.: g) (a->b) (((hRes -> g ns) -> hArg -> (Step dr vArg vRes :.: g) ns) ->d) 
                                         (LayerFn hArg vArg hRes vRes ->e) where
  
             app comp    f      fx = \x -> (app (rightType comp) f (fx (liftStep x))) 


class ResType f res | f -> res where
  resType :: f -> res
  resType = undefined

instance ResType (Fix ct) (ct t)
  
instance ResType f r => ResType (a -> f) r

{- -- derived sig is not accepted :-(
magicLift :: ( ResType f (comp t), Comp comp (a1 -> a1) c
             , App comp
              (((a -> NilStep (Fix f1)) -> a -> f1 (Fix f1)) -> a -> Fix f1) c f) =>
              f
-}
magicLift = app (resType magicLift) genericLift (compose (resType magicLift) id)


-- combine

class Combine (comp :: * -> *) t f | comp t -> f where
  combineC :: comp t -> f

instance Combine NilStep t ((x -> y -> t) -> (NilStep x) -> (NilStep y) -> NilStep t) where
  combineC _ = \next (NilStep x) (NilStep y) ->  NilStep (next x y) 
 
instance (Combine h t ((ft -> gt -> t) -> f ft -> g gt-> h t)) =>
         Combine (Step Down a r :.:  h) t
                 ((ft -> gt -> t) ->
                  (Step Down a m :.: f) ft -> 
                  (Step Down m r :.: g) gt -> 
                  (Step Down a r :.: h) t) where
  combineC comp = \next f g -> combineStepDown (combineC (rightType comp) next) f g

instance (Combine h t ((ft -> gt -> t) -> f ft -> g gt -> h t)) =>
         Combine (Step Up a r :.:  h) t
                 ((ft -> gt -> t) -> 
                  (Step Up m r :.: f) ft -> 
                  (Step Up a m :.: g) gt -> 
                  (Step Up a r :.: h) t) where
  combineC comp = \next f g -> combineStepUp (combineC (rightType comp) next) f g

{- -- derived sig is not accepted :-(
magicCombine :: (Combine comp t ((Fix t1 -> Fix t2 -> Fix f)
                                -> t1 (Fix t1)
                                -> t2 (Fix t2)
                                -> f (Fix f))
                , ResType (Fix t1 -> Fix t2 -> Fix f) (comp t)) =>
                Fix t1 -> Fix t2 -> Fix f
-}
magicCombine = cfix (combineC (resType magicCombine))







-- testing

unStep (Comp (Step step)) = step
unNil (NilStep step) = step


type Layer doc pres gest upd = 
  Fix (Step Down doc pres  :.: Step Up gest upd :.: NilStep)

                
lift :: Simple state map doc pres gest upd ->
               state -> Layer doc pres gest upd
lift l = magicLift (present l) (interpret l)

main layer1 layer2 layer3 =
 do { (state1, state2, state3) <- initStates
    ; doc <- initDoc 

    ; let layers = lift layer1 state1 `magicCombine` 
                   lift layer2 state2 `magicCombine`
                   lift layer3 state3
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
 do { (state0, state1, state2) <- initStates
    ; doc <- initDoc
    --; let (state0, state1, state2) = (0, 10, 20)
    --; let doc = "DOC"
    ; let lift :: Simple state map doc pres gest upd ->
                  state -> Layer2 doc pres gest upd
          lift l = magicLift (present l) (interpret l)
    
    ; let layers =                lift layer0 state0 
                   `magicCombine` lift layer1 state1
                   `magicCombine` lift layer2 state2
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




