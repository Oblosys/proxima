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

newtype Fix f = Fix (f (Fix f))

newtype NilT t = NilT t

class Combine (comp :: * -> *) t f | comp t -> f where
  combineC :: comp t -> f

instance Combine NilT t ((NilT x) -> (NilT y) -> (x -> y -> t) -> NilT t) where
  combineC _ = \(NilT x) (NilT y) next ->  NilT (next x y) 

combine :: Fix NilT  -> Fix NilT -> Fix NilT
combine (Fix l1) (Fix l2) = Fix $ x
 where x = combineC x l1 l2 combine


