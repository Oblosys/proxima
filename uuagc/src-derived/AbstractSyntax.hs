

-- UUAGC 0.9.10 (AbstractSyntax.ag)
module AbstractSyntax where

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import CommonTypes
-- Child -------------------------------------------------------
{-
   alternatives:
      alternative Child:
         child name           : {Identifier}
         child tp             : {Type}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child higherOrder    : {Bool}
-}
data Child  = Child (Identifier) (Type) (Attributes) (Attributes) (Bool) 
-- Children ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Child 
         child tl             : Children 
      alternative Nil:
-}
type Children  = [(Child)]
-- Grammar -----------------------------------------------------
{-
   alternatives:
      alternative Grammar:
         child typeSyns       : {TypeSyns}
         child useMap         : {UseMap}
         child derivings      : {Derivings}
         child wrappers       : {Set NontermIdent}
         child nonts          : Nonterminals 
         child pragmas        : {PragmaMap}
         child manualAttrOrderMap : {AttrOrderMap}
         child paramMap       : {ParamMap}
         child contextMap     : {ContextMap}
         child uniqueMap      : {UniqueMap}
-}
data Grammar  = Grammar (TypeSyns) (UseMap) (Derivings) (Set NontermIdent) (Nonterminals) (PragmaMap) (AttrOrderMap) (ParamMap) (ContextMap) (UniqueMap) 
-- Nonterminal -------------------------------------------------
{-
   alternatives:
      alternative Nonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : Productions 
-}
data Nonterminal  = Nonterminal (NontermIdent) ([Identifier]) (Attributes) (Attributes) (Productions) 
-- Nonterminals ------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Nonterminal 
         child tl             : Nonterminals 
      alternative Nil:
-}
type Nonterminals  = [(Nonterminal)]
-- Production --------------------------------------------------
{-
   alternatives:
      alternative Production:
         child con            : {ConstructorIdent}
         child children       : Children 
         child rules          : Rules 
         child typeSigs       : TypeSigs 
-}
data Production  = Production (ConstructorIdent) (Children) (Rules) (TypeSigs) 
-- Productions -------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Production 
         child tl             : Productions 
      alternative Nil:
-}
type Productions  = [(Production)]
-- Rule --------------------------------------------------------
{-
   alternatives:
      alternative Rule:
         child pattern        : {Pattern}
         child rhs            : {Expression}
         child owrt           : {Bool}
         child origin         : {String}
-}
data Rule  = Rule (Pattern) (Expression) (Bool) (String) 
-- Rules -------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Rule 
         child tl             : Rules 
      alternative Nil:
-}
type Rules  = [(Rule)]
-- TypeSig -----------------------------------------------------
{-
   alternatives:
      alternative TypeSig:
         child name           : {Identifier}
         child tp             : {Type}
-}
data TypeSig  = TypeSig (Identifier) (Type) 
-- TypeSigs ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : TypeSig 
         child tl             : TypeSigs 
      alternative Nil:
-}
type TypeSigs  = [(TypeSig)]