
> module X ( X(..), DWith(..), DP(..), Cmp(..),
>            (<.>), (<|>), beta) where


> data X v = Inv (X v)
>          | Cons | Nil
>          | Succ | Zero
>          | Node
>          | Swap | Assocr | Assocl
>          | Dup (DWith v)
>          | Cmpr Cmp
>          | X v :.: X v | Id
>          | X v :*: X v
>          | X v :**: X v
>          | X v :|: X v
>          | Inl | Inr
>          | Inj (PFun v v, PFun v v)    -- User-defined injective function

>         -- | Del | Snd                 -- dealing with holes

>          | Fix (X v -> X v)

>          | Define Name (X v) | Dummy   -- for better error message
>          | Ident Name [X v]            -- identifiers!


> data DWith v = DP [DP]
>              | DNil | DZero | DStr String  -- constants
>              | DF (v -> v)                 -- non-injective function

> data Cmp = Neq | Lt | Leq | Gt | Geq

> data DP = DFst | DSnd | DCons | DNode

> type PFun a b = (a -> Bool, a -> b)

> type Name = String


Fixity declarations.

> infixr 8 :.:
> infixr 8 <.>
> (<.>) :: X v -> X v -> X v
> (<.>) = (:.:)

> infix 7 :*:

> infixr 6 :|:
> infixr 6 <|>
> (<|>) :: X v -> X v -> X v
> (<|>) = (:|:)




> instance Show (X v) where
>   showsPrec n (Define _ f) = showsX n f
>   showsPrec n x = showsX n x

> showsX :: Int -> X v -> ShowS

> showsX _ Id = ("id" ++)
> showsX _ Succ = ("succ" ++)
> showsX _ Cons = ("cons" ++)
> showsX _ Nil = ("nil"++)
> showsX _ Zero = ("zero"++)
> showsX _ Node = ("node"++)
> showsX _ Inl = ("inl"++)
> showsX _ Inr = ("inr"++)

> -- showsX _ Del = ("del"++)
> -- showsX _ Snd = ("snd"++)

> showsX _ (Dup (DP [])) = ("dup"++)
> showsX n (Dup w) = bracket 5 n (("dup " ++) . shows w)

> showsX _ (Cmpr cmp) = shows cmp

> showsX _ Assocr = ("assocr" ++)
> showsX _ Assocl = ("assocl" ++)
> showsX _ Swap = ("swap" ++)

> showsX n (f :.: g) = bracket 5 n (showsX 5 f . (';':) . showsX 5 g)
> showsX n (f :*: g) = bracket 3 n (showsX 4 f . ('*':) . showsX 4 g)
> showsX n (f :**: g) = bracket 3 n (showsX 4 f . ("**"++) . showsX 4 g)
> showsX n (f :|: g) = bracket 2 n (showsX 2 f . (" U "++) . showsX 2 g)

> showsX n (Inj _) = ("Inj"++)

> showsX n (Inv f) = bracket 6 n (showsX 6 f . ("^o"++))

> showsX n (Fix f) = ("u(X -> "++) . shows (f Dummy) . (')':)
> showsX _ Dummy = ('X':)

> showsX _ (Define name f) = (name ++)
> showsX n (Ident v []) = (v ++)
> showsX n (Ident v xs) = (v ++) . ('(':) . showsArgs xs . (')':)
>     where showsArgs [x] = showsX 5 x
>           showsArgs (x:xs) = showsX 5 x . (',':) . showsArgs xs

> bracket m n ss | m < n = ('(':).ss.(')':)
>                | otherwise = ss

> instance Show (DWith v) where
>   showsPrec _ DNil = ("nil"++)
>   showsPrec _ DZero = ("zero"++)
>   showsPrec _ (DStr str) = ("(str "++) . (str ++) . (')':)
>   showsPrec _ (DF f) = ("<fun>"++)
>   showsPrec _ (DP []) = id
>   showsPrec _ (DP [d]) = shows d
>   showsPrec _ (DP dp) = ('(':). showsdps dp . (')':)
>     where showsdps [d] = shows d
>           showsdps (d:dp) = shows d . ('.':) . showsdps dp

> instance Show DP where
>   showsPrec _ DFst = ("fst"++)
>   showsPrec _ DSnd = ("snd"++)
>   showsPrec _ DCons = ("cons^o"++)
>   showsPrec _ DNode = ("node^o"++)

> instance Show Cmp where
>   showsPrec _ Neq = ("neq"++)
>   showsPrec _ Lt = ("lt"++)
>   showsPrec _ Leq = ("leq"++)
>   showsPrec _ Geq = ("geq"++)
>   showsPrec _ Gt = ("gt"++)


Beta reduction. So far it's there only for the parser!

> beta :: Name -> X a -> X a -> X a
> beta v x (Ident v' []) | v == v' = x                   -- not right!
> beta v x (Ident v' fs) = Ident v' (map (beta v x) fs)  -- but it's only for the parser
> beta v x (e1 :.: e2) = (beta v x e1 :.: beta v x e2)
> beta v x (e1 :*: e2) = (beta v x e1 :*: beta v x e2)
> beta v x (e1 :|: e2) = (beta v x e1 :|: beta v x e2)
> beta v x (e1 :**: e2) = (beta v x e1 :**: beta v x e2)
> beta v x (Inv e) = Inv (beta v x e)
> beta v x (Define name e) = Define name (beta v x e)
> beta v x (Fix f) = Fix (\y -> beta v x (f y))
> beta v x e = e