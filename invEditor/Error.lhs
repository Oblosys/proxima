> module Error (Err(..), ErrType(..), throwErr) where

> import Control.Monad.Error
> import X

> data Err v = Err (ErrType v) (ErrHist v)
>            | Branch (Err v) (Err v)

> data ErrType v = OutDom (X v) v
>                | EqFail v v
>                | CmprFail Cmp v v
>                | ProjFail (DWith v) v
>                | Modified String               -- for put
>                | Partial (Err v) (Resume v)    -- for put
>		 | UndefinedVar String

> type ErrHist v = [(v, String)]
> type Resume v = (X v, v)

> throwErr err = throwError (Err err [])

> instance Error (Err v) where
>   noMsg = error "undefined nomsg"
>   strMsg = error "undefined strmsg"

> instance Show v => Show (Err v) where
>  showsPrec _ (Err etype hist) =
>     showsHist hist . showsEType etype
>   where showsHist [] = id
>         showsHist ((v,fn):hs) =
>           ("value " ++) . shows v . (" rejected by " ++) .
>           (fn ++) . (" because\n" ++) . showsHist hs
>         showsEType (OutDom fn v) =
>           ("value " ++) . shows v . (" not in the domain of " ++) .
>           shows fn
>         showsEType (EqFail v1 v2) =
>           ("the equality check fails: "++) . shows v1 .
>           ("=/=" ++) . shows v2
>         showsEType (CmprFail cmp v1 v2) =
>           ("the comparison fails: "++) . shows v1 .
>           (" `"++) . shows cmp . ("` "++) . shows v2
>         showsEType (Modified fn) =
>           ("the result of non-injective function " ++) . (fn ++) .
>           (" was modified" ++)
>         showsEType (ProjFail p x) =
>           ("failure extracting field "++) . shows p .
>           (" from value "++) . shows x 
>         showsEType (Partial err (f,x)) = 
>           ("partial error "++) . shows err . ("\n" ++) .
>           ("  with continuation ("++) . shows f . (","++) . shows x . (')':)
>	  showsEType (UndefinedVar v) =
>           ("Undefined variable " ++) . (v++)
>  showsPrec _ (Branch err1 err2) =
>     shows err1 . ("\n and " ++) . shows err2

