> module View (View(..), normalise) where

> import Char
> import qualified Data.Tree

> data View = ANil
>           | AN Int
>           | AS String
>           | Pr View View
>           | Ls View View
>           | Tr View View
>           | L View
>           | R View

>           | Mark View
>           | DelL View View 
>           | InsL View View
>           | SndP Bool View View
>           | FstP Bool View View
>           | IfNil Bool View
>           | Undef
>   deriving Eq

> instance Show View where
>   showsPrec _ ANil = ("[]"++)
>   showsPrec _ (AN i) = shows i
>   showsPrec _ (AS s) = ('\'':) . (s++) . ('\'':)
>   showsPrec _ (Pr x y) = ('<':) . showsPrec 2 x . (","++) . showsPrec 2 y . ('>':)
>   showsPrec n (Ls a x) = 
>      bracket 3 n (showsPrec 4 a . (':':) . showsPrec 3 x)
>   showsPrec n (Tr a x) =
>     ('{':) . shows a . (',':) . showsPrec 3 x . ('}':) 
>   showsPrec n (L a) = bracket 3 n (("L "++) . showsPrec 4 a)
>   showsPrec n (R a) = bracket 3 n (("R "++) . showsPrec 4 a)
>   showsPrec n (Mark x) =
>      bracket 4 n (('*':) . showsPrec 4 x)
>   showsPrec n (DelL a x) =
>      bracket 3 n (("\\("++) . showsPrec 2 a . (':':) . showsPrec 3 x . (')':))
>   showsPrec n (InsL a x) =
>      bracket 3 n (("^("++) . showsPrec 2 a . (':':) . showsPrec 3 x . (')':))
>   showsPrec _ (SndP False a b) = ("<-"++) . showsPrec 2 a . (',':) . showsPrec 2 b . ('>':)
>   showsPrec _ (FstP False a b) = ('<':) . showsPrec 2 a . (',':) . showsPrec 2 b . ("->"++)
>   showsPrec _ (SndP True a b) = ("<+"++) . showsPrec 2 a . (',':) . showsPrec 2 b . ('>':)
>   showsPrec _ (FstP True a b) = ('<':) . showsPrec 2 a . (',':) . showsPrec 2 b . ("+>"++)
>   showsPrec _ (IfNil False a) = ("[]->"++) . showsPrec 2 a
>   showsPrec _ (IfNil True a) = ("[]+>"++) . showsPrec 2 a
>   showsPrec _ Undef = ("_|_"++)
 
> bracket m n ss | m < n = ('(':).ss.(')':)
>                | otherwise = ss

> instance Read View where
>   readsPrec _ x = readsView (convQuote x)
>    where readsView x =
>             [(v2,x3) | (t,x1) <- lex x, 
>                        (v1,x2) <- readsV (t,x1),
>                        (v2,x3) <- readsL v1 x2]

>          readsV (t,x)
>              | isDigit (head t) = [(AN (read t), x)]
>          readsV ('"':t,x) 
>              | last t == '"' = [(AS (init t), x)]
>          readsV ("L",x) =
>              [(L a, x1) | (a,x1) <- reads x]
>          readsV ("R",x) =
>              [(R a, x1) | (a,x1) <- reads x]
>          readsV ('*':t,x) =
>              [(Mark a,x2) |
>                   (t',x1) <- lex (t++x), 
>                   (a,x2) <- readsV (t',x1)]
>          readsV ('\\':t,x) =
>              [(DelL a y,x2) |
>                   (t',x1) <- lex (t++x), 
>                   (Ls a y,x2) <- readsV (t',x1)]
>          readsV ('^':t,x) =
>              [(InsL a y,x2) |
>                   (t',x1) <- lex (t++x), 
>                   (Ls a y,x2) <- readsV (t',x1)]
>          readsV ('<':t,x) =
>              [(Pr a b,t'++x4) | (a, x1) <- reads (t++x),
>                                 (",",x2) <- lex x1,
>                                 (b, x3) <- reads x2,
>                                 ('>':t',x4) <- lex x3 ]
>          readsV ("(",x) = [(a,x2) | (a,x1) <- reads x, (")",x2) <- lex x1]
>          readsV ("{",x) =
>           [ (Tr a ts,x4)  | (a,x1) <- reads x,
>                             (",",x2) <- lex x1,
>                             (ts,x3) <- reads x2,
>                             ("}",x4) <- lex x3]
>                -- no unit ()
>          readsV ("[",x) =
>            case lex x of
>              (("]",x1):_) -> [(ANil,x1)]

>          readsL v1 "" = [(v1,"")]
>          readsL v1 (':':x) =
>              [(Ls v1 v2, x1) | (v2, x1) <- reads x]
>          readsL v1 x = [(v1,x)]

>          convQuote = map conv   -- a hack to ease parsing strings
>             where conv '\'' = '"'
>                   conv a = a


> normalise :: View -> View
> normalise (Pr a b) = Pr (normalise a) (normalise b)
> normalise (Ls a b) = Ls (normalise a) (normalise b)
> normalise (Tr a b) = Tr (normalise a) (normalise b)
> normalise (L a) = L (normalise a)
> normalise (R a) = R (normalise a)
> normalise (SndP f a b) = SndP f (normalise a) (normalise b)
> normalise (FstP f a b) = FstP f (normalise a) (normalise b)
> normalise (IfNil f a) = IfNil f (normalise a)

> normalise (Mark a) = normalise a
> normalise (DelL _ x) = normalise x
> normalise (InsL a x) = Ls (normalise a) (normalise x)

> normalise a = a
