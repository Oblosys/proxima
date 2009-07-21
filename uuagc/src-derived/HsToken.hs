

-- UUAGC 0.9.10 (HsToken.ag)
module HsToken where

import CommonTypes
import UU.Scanner.Position(Pos)
-- HsToken -----------------------------------------------------
{-
   alternatives:
      alternative AGField:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
      alternative AGLocal:
         child var            : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
      alternative CharToken:
         child value          : {String}
         child pos            : {Pos}
      alternative Err:
         child mesg           : {String}
         child pos            : {Pos}
      alternative HsToken:
         child value          : {String}
         child pos            : {Pos}
      alternative StrToken:
         child value          : {String}
         child pos            : {Pos}
-}
data HsToken  = AGField (Identifier) (Identifier) (Pos) (Maybe String) 
              | AGLocal (Identifier) (Pos) (Maybe String) 
              | CharToken (String) (Pos) 
              | Err (String) (Pos) 
              | HsToken (String) (Pos) 
              | StrToken (String) (Pos) 
-- HsTokens ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : HsToken 
         child tl             : HsTokens 
      alternative Nil:
-}
type HsTokens  = [(HsToken)]
-- HsTokensRoot ------------------------------------------------
{-
   alternatives:
      alternative HsTokensRoot:
         child tokens         : HsTokens 
-}
data HsTokensRoot  = HsTokensRoot (HsTokens) 