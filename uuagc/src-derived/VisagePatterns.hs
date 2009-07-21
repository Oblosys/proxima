

-- UUAGC 0.9.10 (VisagePatterns.ag)
module VisagePatterns where

import UU.Scanner.Position(Pos)
import CommonTypes
-- VisagePattern -----------------------------------------------
{-
   alternatives:
      alternative VAlias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : VisagePattern 
      alternative VConstr:
         child name           : {ConstructorIdent}
         child pats           : VisagePatterns 
      alternative VProduct:
         child pos            : {Pos}
         child pats           : VisagePatterns 
      alternative VUnderscore:
         child pos            : {Pos}
      alternative VVar:
         child field          : {Identifier}
         child attr           : {Identifier}
-}
data VisagePattern  = VAlias (Identifier) (Identifier) (VisagePattern) 
                    | VConstr (ConstructorIdent) (VisagePatterns) 
                    | VProduct (Pos) (VisagePatterns) 
                    | VUnderscore (Pos) 
                    | VVar (Identifier) (Identifier) 
-- VisagePatterns ----------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : VisagePattern 
         child tl             : VisagePatterns 
      alternative Nil:
-}
type VisagePatterns  = [(VisagePattern)]