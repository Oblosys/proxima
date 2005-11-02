> module GetPut (eval, invert, get, put) where

> import qualified Data.Tree
> import Control.Monad.Error

> import View
> import X
> import Error


> type M a = Either (Err a) a

> invert :: X v -> X v
> invert Id = Id 
> invert Assocr = Assocl
> invert Assocl = Assocr
> invert Swap = Swap
> invert (f:.:g) = invert g :.: invert f
> invert (f:*:g) = invert f :*: invert g
> invert (f:**:g) = invert f :**: invert g
> invert (f:|:g) = invert f :|: invert g
> invert (Inj (f,g)) = Inj (g,f)
> invert (Cmpr cmp) = Cmpr cmp
> invert (Inv f) = f
> invert (Define name f) = Define (name ++ "^o") (invert f)
> invert f = (Inv f)

> type SymTable a = [(String, ([String], X a))]


> get st = eval st
> put st f = liftM normalise . eval st (invert f)

> eval :: SymTable View -> X View -> View -> M View

> eval st Id a  = return a

> eval st Nil a = return ANil
> eval st (Inv Nil) ANil = return ANil
> eval st (Inv Nil) a = throwErr (OutDom (Inv Nil) a)

> eval st Cons (Pr a x) | isList x = return (Ls a x)
>                    	| otherwise = throwErr (OutDom Cons (Pr a x))
> eval st Cons (SndP False a x) = return (DelL a x)      -- snd^o; cons = del
> eval st Cons (SndP True  a x) = return (InsL a x)      -- (a,)^o; cons = ins
> eval st Cons x = throwErr (OutDom Cons x)

> eval st (Inv Cons) (Ls a x) = return (Pr a x)
> eval st (Inv Cons) (DelL a x) = return (SndP False a x)   -- del^o = cons^o; snd
> eval st (Inv Cons) (InsL a x) = return (SndP True a x) 
> eval st (Inv Cons) x = throwErr (OutDom (Inv Cons) x)

> eval st Zero a = return (AN 0)
> eval st (Inv Zero) (AN 0) = return ANil
> eval st (Inv Zero) x = throwErr (OutDom (Inv Zero) x)

> eval st Succ (AN i) = return (AN (i+1))
> eval st (Inv Succ) (AN 0) = throwErr (OutDom (Inv Succ) (AN 0))
> eval st (Inv Succ) (AN (i+1)) = return (AN i)

> eval st Inl a = return (L a)
> eval st (Inv Inl) (L a) = return a
> eval st (Inv Inl) Undef = return Undef     -- ok?
> eval st (Inv Inl) x = throwErr (OutDom (Inv Inl) x)

> eval st Inr a = return (R a)
> eval st (Inv Inr) (R a) = return a
> eval st (Inv Inr) Undef = return Undef     -- ok?
> eval st (Inv Inr) x = throwErr (OutDom (Inv Inr) x)

> eval st Node (Pr a x) | allTrees x = return (Tr a x)
>                    	| otherwise = throwErr (OutDom Node (Pr a x))
>   where allTrees ANil = True
>         allTrees (Ls ANil x) = allTrees x
>         allTrees (Ls (Tr _ _) x) = allTrees x
>         allTrees _ = False
> eval st (Inv Node) (Tr a ts) = return (Pr a ts)
> eval st (Inv Node) x = throwErr (OutDom (Inv Node) x)

> eval st Swap (Pr a b) = return (Pr b a)
> eval st Swap (SndP f a b) = return (FstP f b a)
> eval st Swap (FstP f a b) = return (SndP f b a)
> eval st Swap x = throwErr (OutDom Swap x)

> eval st Assocr (Pr (Pr a b) c) = return (Pr a (Pr b c))
> eval st Assocr (Pr (FstP f a b) c) = return (Pr a (SndP f b c))
> eval st Assocr (Pr (SndP f a b) c) = return (SndP f a (Pr b c))
> eval st Assocr (SndP f (Pr a b) c) = return (SndP f a (SndP f b c))
> eval st Assocr x = throwErr (OutDom Assocr x)

> eval st Assocl (Pr a (Pr b c)) = return (Pr (Pr a b) c)
> eval st Assocl (Pr a (SndP f b c)) = return (Pr (FstP f a b) c)
> eval st Assocl (SndP f a (Pr b c)) = return (Pr (SndP f a b) c)
> eval st Assocl (SndP f a (SndP _ b c)) = return (SndP f (Pr a b) c)  
> eval st Assocl x = throwErr (OutDom Assocl x)

> eval st (Dup DNil) (IfNil f a) = return (SndP f a ANil)
> eval st (Dup w) x = 
>   do { a <- dupWith w x; return (Pr x a) }

> eval st (Cmpr cmp) (SndP f a b) | cmpr cmp a b = return (SndP f a b)
>                                 | otherwise = throwErr (CmprFail cmp a b)
> eval st (Cmpr cmp) (FstP f a b) | cmpr cmp a b = return (FstP f a b)
>                                 | otherwise = throwErr (CmprFail cmp a b)
> eval st (Cmpr cmp) (Pr a b) | cmpr cmp a b = return (Pr a b)
>                             | otherwise = throwErr (CmprFail cmp a b)
> eval st (Cmpr cmp) x = throwErr (OutDom (Cmpr cmp) x)

> eval st (Inj ((p,f),g)) x | p x = return (f x)
>                           | otherwise = throwErr (OutDom (Inj ((p,f),g)) x)

> eval st (Inv (Dup DNil)) (SndP f a ANil) = return (IfNil f a)
> eval st (Inv (Dup w)) (Pr a b) = eqWith w a b
> eval st (Inv (Dup w)) x = throwErr (OutDom (Inv (Dup w)) x)

> eval st (f:.:g) a = eval st g =<< eval st f a


> eval st (Inv Cons:**:Inv Cons) (Pr (DelL a x) (Ls b y)) =
>     return (Pr (SndP False a x) (SndP False b y))
> eval st (Inv Cons:**:Inv Cons) (Pr (Ls a x) (DelL b y)) =
>     return (Pr (SndP False a x) (SndP False b y))
> eval st (Inv Cons:**:Inv Cons) (Pr (InsL a x) y) =
>     return (Pr (SndP True a x) (SndP True Undef y))
> eval st (Inv Cons:**:Inv Cons) (Pr x (InsL b y)) =
>     return (Pr (SndP True Undef x) (SndP True b y))
> eval st (f:**:g) x = eval st (f:*:g) x

> eval st (f:*:g) (Pr a b) = liftM2 Pr (eval st f a) (eval st g b)
> eval st (f:*:g) (SndP k a b) = liftM2 (SndP k) (eval st f a) (eval st g b)
> eval st (f:*:g) x = throwErr (OutDom (f:*:g) x)

> eval st (f:|:g) a = catchError (eval st f a) (const (eval st g a))

> eval st (Fix f) a = eval st (f (Fix f)) a
> eval st (Inv (Fix f)) a = eval st (Fix (Inv . f . Inv)) a

> eval st (Define name f) a = 
>    catchError (eval st f a)
>        (\ (Err e h) -> throwError (Err e ((a,name):h)))

> eval st (Inv (Ident v xs)) a = 
>    case lookup v st of
>      Just (us,f) -> eval (extend st us xs) (Inv f) a
>      Nothing -> throwErr (UndefinedVar v)
>  where extend st us xs =
>          zipWith (\ u x -> (u,([],x))) us xs ++ st
   
> eval st (Ident v xs) a =
>    case lookup v st of
>      Just (us,f) -> eval (extend st us xs) f a
>      Nothing -> throwErr (UndefinedVar v)
>  where extend st us xs =
>          zipWith (\ u x -> (u,([],x))) us xs ++ st

> eval st (Inv (Define name f)) a = 
>    catchError (eval st (Inv f) a)
>        (\ (Err e h) -> throwError (Err e ((a,name++"^o"):h)))

> eval st (Inv f) a = eval st (invert f) a


> eval st f a = throwErr (OutDom f a) 


> cross f g (Pr a b) = Pr (f a) (g b)

> isList ANil = True
> isList (Ls _ _) = True
> isList (DelL a x) = isList x
> isList (InsL a x) = isList x
> isList _ = False

> dupWith :: DWith View-> View -> M View
> dupWith DNil _ = return ANil
> dupWith DZero _ = return (AN 0)
> dupWith (DStr s) _ = return (AS s)
> dupWith (DF f) x = return (f x)
> dupWith (DP dp) x = visit dp x

> visit [] x = return x
> visit (DFst:dp) (Pr x _) = visit dp x
> visit (DSnd:dp) (Pr _ x) = visit dp x
> visit (DCons:dp) (Ls a x) = visit dp (Pr a x)
> visit (DNode:dp) (Tr a x) = visit dp (Pr a x)
> visit p (DelL a x) = liftM (DelL a) (visit p x)
> visit p Undef = return Undef
> visit p x = throwErr (ProjFail (DP p) x)

> eqWith :: DWith View -> View -> View -> M View
> eqWith DNil x ANil = return x
> eqWith DNil x (DelL a ANil) = throwErr (EqFail x (DelL a ANil))
> eqWith DNil x Undef = return x  -- ok?
> eqWith DNil x y = throwErr (EqFail x y)
> eqWith DZero x (AN 0) = return x
> eqWith DZero x Undef = return x -- ok?
> eqWith DZero x y = throwErr (EqFail x y)
> eqWith (DStr s) x (AS s') | s == s' = return x
>                           | otherwise = throwErr (EqFail (AS s) (AS s'))
> eqWith (DF f) x _ = return x   -- is this right ?
> eqWith (DP dp) x a =
>    do a' <- dupWith (DP dp) x
>       a'' <- eq a a'
>       return (invite dp x a'')

> invite [] _ a = a
> invite (DFst:dp) (Pr x y) a = Pr (invite dp x a) y
> invite (DFst:dp) Undef a = Pr (invite dp Undef a) Undef 
> invite (DSnd:dp) (Pr x y) a = Pr x (invite dp y a)
> invite (DSnd:dp) Undef a = Pr Undef (invite dp Undef a)
> invite (DCons:dp) (Ls b x) a = doCons (invite dp (Pr b x) a)
> invite (DNode:dp) (Tr b x) a = doNode (invite dp (Pr b x) a)
> invite dp (DelL a x) b = DelL a (invite dp x b)

> doCons (Pr a b) = Ls a b
> doNode (Pr a b) = Tr a b

> eq (Mark a) b = return (Mark a)
> eq a (Mark b) = return (Mark b)
> eq (Pr a b) (Pr c d) = liftM2 Pr (eq a c) (eq b d)
> eq (Ls a x) (Ls b y) = liftM2 Ls (eq a b) (eq x y)
> eq (Ls a x) (DelL b y) = liftM (DelL a) (eq x y)
> eq (DelL a x) (Ls b y) = liftM (DelL a) (eq x y) 
> eq (Tr a x) (Tr b y) = liftM2 Tr (eq a b) (eq x y)
> eq (DelL a x) (DelL _ y) = liftM (DelL a) (eq x y)   -- otherwise we have \ twice
> eq (DelL a x) y = liftM (DelL a) (eq x y)
> eq x (DelL b y) = liftM (DelL b) (eq x y)   -- is this right?
> eq a Undef = return a
> eq Undef a = return a
> eq a b | a == b = return a
>        | otherwise = throwErr (EqFail a b)

> cmpr c (Mark a) b = cmpr c a b
> cmpr c a (Mark b) = cmpr c a b
> cmpr Neq a b = a /= b
> cmpr Lt  (AN a) (AN b) = a < b
> cmpr Leq (AN a) (AN b) = a <= b
> cmpr Geq (AN a) (AN b) = a >= b
> cmpr Gt  (AN a) (AN b) = a > b
> cmpr _ _ _ = False
