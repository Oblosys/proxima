module Proxima.ArchitectureLibM where



 -- magic
type TLayer (m :: * -> *) a b c d = Fix m (Step Up a b :.: Step Down c d :.: NilStep)


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


{-
-- non magic

type TLayer (m :: * -> *) a b c d = Fix m (Step a b :.: Step c d :.: NilStep)


fix :: (a->a) -> a
fix a = let fixa = a fixa
        in  fixa

type LayerFn m horArgs vertArg horRess vertRes =
       horArgs -> vertArg -> m (vertRes, horRess)


newtype Fix m f = Fix (f m (Fix m f))

infixr :.:

newtype (:.:) f g (m :: * -> *) ns  = Comp (f m (g m ns))

newtype NilStep m t = NilStep t

newtype Step a b m ns = Step (a -> m (b, ns))

unStep (Comp (Step step)) = step
unNil (NilStep step) = step


lfix f = fix f' where f' n = Fix . (f . lNilStep) n

lNilStep next hRes = NilStep $ next hRes

liftStep f next horArgs = Comp . Step $ 
  \vArg -> do { (vertRes, horRes) <- f horArgs vArg
              ; return (vertRes, next horRes)
              }
              

cfix f = fix f' 
 where f' n (Fix u) (Fix l) = Fix $ (f . cNilStep) n u l

cNilStep next (NilStep u) (NilStep l) = 
  NilStep $ next u l

combineStepDown next (Comp (Step upper)) 
                     (Comp (Step lower)) = Comp . Step $
  \h -> do { (m ,upperf) <- upper h
           ; (l, lowerf) <- lower m
           ; return (l, next upperf lowerf)   
           }

combineStepUp next (Comp (Step upper)) 
                   (Comp (Step lower)) = Comp . Step $
  \l -> do { (m, lowerf) <- lower l
           ; (h, upperf) <- upper m
           ; return (h, next upperf lowerf)
           }

-- types:

combineStepDown :: Monad m => 
                   (f m x -> g m y -> h m ns) ->
                   (Step a b :.: f) m x -> 
                   (Step b c :.: g) m y ->
                   (Step a c :.: h) m ns

combineStepUp :: Monad m => 
                 (f m x -> g m y -> h m ns) ->
                 (Step b c :.: f) m x ->
                 (Step a b :.: g) m y ->
                 (Step a c :.: h) m ns

liftStep :: Monad m => LayerFn m hArg vArg hRes vRes -> 
            (hRes -> g m ns) -> hArg -> 
            (Step vArg vRes :.: g) m ns
-}






{- -- old
fix :: (a->a) -> a
fix a = let fixa = a fixa
         in fixa

class Pack m step arg res nStep | step -> m arg res nStep where
  pack :: (arg -> m (res, nStep)) -> step
  unpack :: step -> arg -> m (res, nStep)

liftStep :: (Monad m, Pack m step vArg vRes nStep) => 
            (hArgs -> vArg -> m (vRes,hRess)) -> (hRess -> nStep) -> hArgs -> step
liftStep layerF next horArgs = pack $ 
    \vertArg -> do {(vertRes, horRess) <- layerF horArgs vertArg
                   ; return (vertRes, next horRess)
                   }
                   

combineStepDown :: ( Monad md
                   , Pack md stepC h l nStepC
                   , Pack md stepU h m nStepU
                   , Pack md stepL m l nStepL ) => 
                   (nStepU -> nStepL -> nStepC) -> stepU -> stepL -> stepC
combineStepDown nextStep upr lwr = pack $
    \high -> do { (med, nextUpr) <- (unpack upr) high
                ; (low, nextLwr) <- (unpack lwr) med
                ; return (low, nextStep nextUpr nextLwr)
                }

combineStepUp :: ( Monad md
                 , Pack md stepC l h nStepC
                 , Pack md stepU m h nStepU
                 , Pack md stepL l m nStepL ) => 
                 (nStepU -> nStepL -> nStepC) -> stepU -> stepL -> stepC
combineStepUp nextStep upr lwr = pack $
    \low -> do { (med, nextLwr) <- (unpack lwr) low
               ; (high, nextUpr) <- (unpack upr) med
               ; return (high, nextStep nextUpr nextLwr)
               }


-}