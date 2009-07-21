

-- UUAGC 0.9.10 (Code.ag)
module Code where

import Pretty
import Patterns
import Data.List(partition)
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map

-- Unboxed tuples
--   unbox  Whether unboxed tuples are wanted or not
--   inh    The inherited attributes. 
--          If there are none, no unboxing can take place, 
--          because in that case the semantic function (a top-level identifier) would have an unboxed type.
-- Of course we can't have an unboxed 1-tuple
mkTupleExpr :: Bool -> Bool -> Exprs -> Expr
mkTupleExpr unbox noInh exprs | not unbox || noInh || length exprs == 1 = TupleExpr exprs
                              | otherwise                               = UnboxedTupleExpr exprs
mkTupleType :: Bool -> Bool -> Types -> Type
mkTupleType unbox noInh tps | not unbox || noInh || length tps == 1 = TupleType tps
                            | otherwise                             = UnboxedTupleType tps
mkTupleLhs :: Bool -> Bool -> [String] -> Lhs
mkTupleLhs  unbox noInh comps | not unbox || noInh || length comps == 1 = TupleLhs comps
                              | otherwise                               = UnboxedTupleLhs comps
-- CaseAlt -----------------------------------------------------
{-
   alternatives:
      alternative CaseAlt:
         child left           : Lhs 
         child expr           : Expr 
-}
data CaseAlt  = CaseAlt (Lhs) (Expr) 
-- CaseAlts ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : CaseAlt 
         child tl             : CaseAlts 
      alternative Nil:
-}
type CaseAlts  = [(CaseAlt)]
-- Chunk -------------------------------------------------------
{-
   alternatives:
      alternative Chunk:
         child name           : {String}
         child comment        : Decl 
         child info           : Decls 
         child dataDef        : Decls 
         child cataFun        : Decls 
         child semDom         : Decls 
         child semWrapper     : Decls 
         child semFunctions   : Decls 
         child semNames       : {[String]}
-}
data Chunk  = Chunk (String) (Decl) (Decls) (Decls) (Decls) (Decls) (Decls) (Decls) ([String]) 
-- Chunks ------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Chunk 
         child tl             : Chunks 
      alternative Nil:
-}
type Chunks  = [(Chunk)]
-- DataAlt -----------------------------------------------------
{-
   alternatives:
      alternative DataAlt:
         child name           : {String}
         child args           : {[String]}
      alternative Record:
         child name           : {String}
         child args           : {[(String,String)]}
-}
data DataAlt  = DataAlt (String) ([String]) 
              | Record (String) ([(String,String)]) 
-- DataAlts ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : DataAlt 
         child tl             : DataAlts 
      alternative Nil:
-}
type DataAlts  = [(DataAlt)]
-- Decl --------------------------------------------------------
{-
   alternatives:
      alternative Comment:
         child txt            : {String}
      alternative Data:
         child name           : {String}
         child params         : {[String]}
         child alts           : DataAlts 
         child strict         : {Bool}
         child derivings      : {[String]}
      alternative Decl:
         child left           : Lhs 
         child rhs            : Expr 
         child binds          : {Set String}
         child uses           : {Set String}
      alternative NewType:
         child name           : {String}
         child params         : {[String]}
         child con            : {String}
         child tp             : Type 
      alternative PragmaDecl:
         child txt            : {String}
      alternative TSig:
         child name           : {String}
         child tp             : Type 
      alternative Type:
         child name           : {String}
         child params         : {[String]}
         child tp             : Type 
-}
data Decl  = Comment (String) 
           | Data (String) ([String]) (DataAlts) (Bool) ([String]) 
           | Decl (Lhs) (Expr) (Set String) (Set String) 
           | NewType (String) ([String]) (String) (Type) 
           | PragmaDecl (String) 
           | TSig (String) (Type) 
           | Type (String) ([String]) (Type) 
-- Decls -------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Decl 
         child tl             : Decls 
      alternative Nil:
-}
type Decls  = [(Decl)]
-- Expr --------------------------------------------------------
{-
   alternatives:
      alternative App:
         child name           : {String}
         child args           : Exprs 
      alternative Case:
         child expr           : Expr 
         child alts           : CaseAlts 
      alternative Lambda:
         child args           : Exprs 
         child body           : Expr 
      alternative Let:
         child decls          : Decls 
         child body           : Expr 
      alternative LineExpr:
         child expr           : Expr 
      alternative PragmaExpr:
         child onLeftSide     : {Bool}
         child onNewLine      : {Bool}
         child txt            : {String}
         child expr           : Expr 
      alternative SimpleExpr:
         child txt            : {String}
      alternative TextExpr:
         child lns            : {[String]}
      alternative Trace:
         child txt            : {String}
         child expr           : Expr 
      alternative TupleExpr:
         child exprs          : Exprs 
      alternative TypedExpr:
         child expr           : Expr 
         child tp             : Type 
      alternative UnboxedTupleExpr:
         child exprs          : Exprs 
-}
data Expr  = App (String) (Exprs) 
           | Case (Expr) (CaseAlts) 
           | Lambda (Exprs) (Expr) 
           | Let (Decls) (Expr) 
           | LineExpr (Expr) 
           | PragmaExpr (Bool) (Bool) (String) (Expr) 
           | SimpleExpr (String) 
           | TextExpr ([String]) 
           | Trace (String) (Expr) 
           | TupleExpr (Exprs) 
           | TypedExpr (Expr) (Type) 
           | UnboxedTupleExpr (Exprs) 
-- Exprs -------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Expr 
         child tl             : Exprs 
      alternative Nil:
-}
type Exprs  = [(Expr)]
-- Lhs ---------------------------------------------------------
{-
   alternatives:
      alternative Fun:
         child name           : {String}
         child args           : Exprs 
      alternative Pattern3:
         child pat3           : {Pattern}
      alternative Pattern3SM:
         child pat3           : {Pattern}
      alternative TupleLhs:
         child comps          : {[String]}
      alternative UnboxedTupleLhs:
         child comps          : {[String]}
-}
data Lhs  = Fun (String) (Exprs) 
          | Pattern3 (Pattern) 
          | Pattern3SM (Pattern) 
          | TupleLhs ([String]) 
          | UnboxedTupleLhs ([String]) 
-- Program -----------------------------------------------------
{-
   alternatives:
      alternative Program:
         child chunks         : Chunks 
-}
data Program  = Program (Chunks) 
-- Type --------------------------------------------------------
{-
   alternatives:
      alternative Arr:
         child left           : Type 
         child right          : Type 
      alternative CtxApp:
         child left           : {[(String, [String])]}
         child right          : Type 
      alternative List:
         child tp             : Type 
      alternative SimpleType:
         child txt            : {String}
      alternative TupleType:
         child tps            : Types 
      alternative TypeApp:
         child func           : Type 
         child args           : Types 
      alternative UnboxedTupleType:
         child tps            : Types 
-}
data Type  = Arr (Type) (Type) 
           | CtxApp ([(String, [String])]) (Type) 
           | List (Type) 
           | SimpleType (String) 
           | TupleType (Types) 
           | TypeApp (Type) (Types) 
           | UnboxedTupleType (Types) 
           deriving ( Show)
-- Types -------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Type 
         child tl             : Types 
      alternative Nil:
-}
type Types  = [(Type)]