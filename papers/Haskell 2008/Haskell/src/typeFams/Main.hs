{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances  -fallow-undecidable-instances -fallow-incoherent-instances #-}

module Main where

--- A class to get rid of Fix and Comp applications

newtype TNil t = TNil t

newtype (:.:) f g ns  = Comp (f (g ns))
{-
class C t1 t2 | t1 -> t2 
 where f :: t1 -> t2
 
instance C Char Bool
 where f c = True
 
instance C x x
 where f x = x


myF = f 'a'
-}


class Decomp comp t decomp | comp t -> decomp where
  decomp :: comp t -> decomp


instance Decomp TNil t t where
  decomp (TNil t) = t
  {-
instance Decomp f t (f t)  where
  decomp f = f
-}
instance Decomp f (g t) h  => Decomp (f :.: g) t h  where
  decomp (Comp f) = decomp f



class FD c t | c -> t where
  toTFD :: c -> t 

instance FD Bool Bool where
  toTFD _ = True

instance FD x x where
  toTFD x = x
  
class C c where
  type T c
  toT :: c -> T c


{-
class Decomp comp t where
  type Decmp comp t
  decomp :: comp t -> (Decmp comp t) 

instance Decomp f t where
  type Decmp f t = f t
  decomp f = f

instance Decomp f (g t) => Decomp (f :.: g) t where
  type Decmp (f :.: g) t = Decmp f (g t)
  decomp (Comp f) = decomp f
-}
--instance Decomp (f :.: g) t  (f (g t)) where
--  decomp (Comp f) = f


--decompTest :: (f (),g (),t) ->  ((f :.: g) t) -> (f (g t))
--decompTest dummy = decomp
--decompTest dummy = decomp (undefined :: ((f :.: g) t))  -- :: f t
--decompTest3 = decomp (undefined :: (f :.: g :.: h) t) :: (f (g (h t)))


--decomp4 :: ((p :.: q :.: r :.: s) t) -> (p (q (r (s t))))    
--decomp4 (Comp (Comp (Comp step))) = step
--decomp4 = decomp
