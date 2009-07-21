

-- UUAGC 0.9.10 (Visage.ag)
module Visage where

import UU.Scanner.Position(Pos(..))
import CommonTypes
import ATermAbstractSyntax
import Expression
import VisagePatterns
import VisageSyntax
import qualified Data.Map as Map
import Data.Map(Map)
import Data.List(intersperse)
import TokenDef


import CommonTypes
import UU.Pretty
import AbstractSyntax
import VisagePatterns
import Expression


import UU.Scanner.Position(Pos)
import CommonTypes


import UU.Scanner.Position(Pos)
import HsToken

convert :: String -> String
convert [] = []
convert (c:ct) | c == '\n' = '\\' : 'n' : convert ct
               | otherwise = c : convert ct
 
sQ :: String -> String
sQ []     = []
sQ (x:xs) = if (x=='"') then rest else x:rest
    where
      rest = if not (null xs) && last xs == '"' then init xs else xs

showAGPos :: Pos -> String
showAGPos (Pos l c f) | l == (-1) = ""
                      | otherwise = let file = if null f then "" else f -- No show of f
                                        lc = "(line " ++ show l ++ ", column " ++ show c ++")"
                                    in file ++ lc

showMap :: (Show a, Show b) => Map a b -> String
showMap
  = braces . concat . intersperse "," . map (uncurry assign) . Map.assocs
  where
    braces s = "{" ++ s ++ "}"
    assign a b = show a ++ ":=" ++ show b
-- Expression --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterm                : ATerm
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
-}
-- cata
sem_Expression :: Expression  ->
                  T_Expression 
sem_Expression (Expression _pos _tks )  =
    (sem_Expression_Expression _pos _tks )
-- semantic domain
newtype T_Expression  = T_Expression (( ATerm))
data Inh_Expression  = Inh_Expression {}
data Syn_Expression  = Syn_Expression {aterm_Syn_Expression :: ATerm}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression (T_Expression sem ) (Inh_Expression )  =
    (let ( _lhsOaterm) =
             (sem )
     in  (Syn_Expression _lhsOaterm ))
sem_Expression_Expression :: Pos ->
                             ([HsToken]) ->
                             T_Expression 
sem_Expression_Expression pos_ tks_  =
    (T_Expression (let _lhsOaterm :: ATerm
                       -- "Visage.ag"(line 103, column 17)
                       _lhsOaterm =
                           AAppl "Expression" [AString (sQ (showAGPos pos_)), AString (sQ (unlines . showTokens . tokensToStrings $ tks_))]
                   in  ( _lhsOaterm)) )
-- VisageChild -------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterm                : ATerm
   alternatives:
      alternative VChild:
         child name           : {Identifier}
         child tp             : {Type}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child rules          : VisageRules 
-}
-- cata
sem_VisageChild :: VisageChild  ->
                   T_VisageChild 
sem_VisageChild (VChild _name _tp _inh _syn _rules )  =
    (sem_VisageChild_VChild _name _tp _inh _syn (sem_VisageRules _rules ) )
-- semantic domain
newtype T_VisageChild  = T_VisageChild (( ATerm))
data Inh_VisageChild  = Inh_VisageChild {}
data Syn_VisageChild  = Syn_VisageChild {aterm_Syn_VisageChild :: ATerm}
wrap_VisageChild :: T_VisageChild  ->
                    Inh_VisageChild  ->
                    Syn_VisageChild 
wrap_VisageChild (T_VisageChild sem ) (Inh_VisageChild )  =
    (let ( _lhsOaterm) =
             (sem )
     in  (Syn_VisageChild _lhsOaterm ))
sem_VisageChild_VChild :: Identifier ->
                          Type ->
                          Attributes ->
                          Attributes ->
                          T_VisageRules  ->
                          T_VisageChild 
sem_VisageChild_VChild name_ tp_ inh_ syn_ (T_VisageRules rules_ )  =
    (T_VisageChild (let _lhsOaterm :: ATerm
                        _rulesOisLoc :: Bool
                        _rulesIaterms :: ([ATerm])
                        -- "Visage.ag"(line 85, column 18)
                        _lhsOaterm =
                            AAppl "Child" [AString (sQ (getName name_)), AString (sQ (show tp_)),
                                           AString (sQ (showMap inh_)),
                                           AString (sQ (showMap syn_)),
                                           AAppl "Rules" _rulesIaterms]
                        -- "Visage.ag"(line 89, column 18)
                        _rulesOisLoc =
                            False
                        ( _rulesIaterms) =
                            (rules_ _rulesOisLoc )
                    in  ( _lhsOaterm)) )
-- VisageChildren ----------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterms               : [ATerm]
   alternatives:
      alternative Cons:
         child hd             : VisageChild 
         child tl             : VisageChildren 
      alternative Nil:
-}
-- cata
sem_VisageChildren :: VisageChildren  ->
                      T_VisageChildren 
sem_VisageChildren list  =
    (Prelude.foldr sem_VisageChildren_Cons sem_VisageChildren_Nil (Prelude.map sem_VisageChild list) )
-- semantic domain
newtype T_VisageChildren  = T_VisageChildren (( ([ATerm])))
data Inh_VisageChildren  = Inh_VisageChildren {}
data Syn_VisageChildren  = Syn_VisageChildren {aterms_Syn_VisageChildren :: [ATerm]}
wrap_VisageChildren :: T_VisageChildren  ->
                       Inh_VisageChildren  ->
                       Syn_VisageChildren 
wrap_VisageChildren (T_VisageChildren sem ) (Inh_VisageChildren )  =
    (let ( _lhsOaterms) =
             (sem )
     in  (Syn_VisageChildren _lhsOaterms ))
sem_VisageChildren_Cons :: T_VisageChild  ->
                           T_VisageChildren  ->
                           T_VisageChildren 
sem_VisageChildren_Cons (T_VisageChild hd_ ) (T_VisageChildren tl_ )  =
    (T_VisageChildren (let _lhsOaterms :: ([ATerm])
                           _hdIaterm :: ATerm
                           _tlIaterms :: ([ATerm])
                           -- "Visage.ag"(line 80, column 17)
                           _lhsOaterms =
                               _hdIaterm : _tlIaterms
                           ( _hdIaterm) =
                               (hd_ )
                           ( _tlIaterms) =
                               (tl_ )
                       in  ( _lhsOaterms)) )
sem_VisageChildren_Nil :: T_VisageChildren 
sem_VisageChildren_Nil  =
    (T_VisageChildren (let _lhsOaterms :: ([ATerm])
                           -- "Visage.ag"(line 81, column 17)
                           _lhsOaterms =
                               []
                       in  ( _lhsOaterms)) )
-- VisageGrammar -----------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterm                : ATerm
   alternatives:
      alternative VGrammar:
         child nonts          : VisageNonterminals 
-}
-- cata
sem_VisageGrammar :: VisageGrammar  ->
                     T_VisageGrammar 
sem_VisageGrammar (VGrammar _nonts )  =
    (sem_VisageGrammar_VGrammar (sem_VisageNonterminals _nonts ) )
-- semantic domain
newtype T_VisageGrammar  = T_VisageGrammar (( ATerm))
data Inh_VisageGrammar  = Inh_VisageGrammar {}
data Syn_VisageGrammar  = Syn_VisageGrammar {aterm_Syn_VisageGrammar :: ATerm}
wrap_VisageGrammar :: T_VisageGrammar  ->
                      Inh_VisageGrammar  ->
                      Syn_VisageGrammar 
wrap_VisageGrammar (T_VisageGrammar sem ) (Inh_VisageGrammar )  =
    (let ( _lhsOaterm) =
             (sem )
     in  (Syn_VisageGrammar _lhsOaterm ))
sem_VisageGrammar_VGrammar :: T_VisageNonterminals  ->
                              T_VisageGrammar 
sem_VisageGrammar_VGrammar (T_VisageNonterminals nonts_ )  =
    (T_VisageGrammar (let _lhsOaterm :: ATerm
                          _nontsIaterms :: ([ATerm])
                          -- "Visage.ag"(line 54, column 18)
                          _lhsOaterm =
                              AAppl "Productions" _nontsIaterms
                          ( _nontsIaterms) =
                              (nonts_ )
                      in  ( _lhsOaterm)) )
-- VisageNonterminal -------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterm                : ATerm
   alternatives:
      alternative VNonterminal:
         child nt             : {NontermIdent}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child alts           : VisageProductions 
-}
-- cata
sem_VisageNonterminal :: VisageNonterminal  ->
                         T_VisageNonterminal 
sem_VisageNonterminal (VNonterminal _nt _inh _syn _alts )  =
    (sem_VisageNonterminal_VNonterminal _nt _inh _syn (sem_VisageProductions _alts ) )
-- semantic domain
newtype T_VisageNonterminal  = T_VisageNonterminal (( ATerm))
data Inh_VisageNonterminal  = Inh_VisageNonterminal {}
data Syn_VisageNonterminal  = Syn_VisageNonterminal {aterm_Syn_VisageNonterminal :: ATerm}
wrap_VisageNonterminal :: T_VisageNonterminal  ->
                          Inh_VisageNonterminal  ->
                          Syn_VisageNonterminal 
wrap_VisageNonterminal (T_VisageNonterminal sem ) (Inh_VisageNonterminal )  =
    (let ( _lhsOaterm) =
             (sem )
     in  (Syn_VisageNonterminal _lhsOaterm ))
sem_VisageNonterminal_VNonterminal :: NontermIdent ->
                                      Attributes ->
                                      Attributes ->
                                      T_VisageProductions  ->
                                      T_VisageNonterminal 
sem_VisageNonterminal_VNonterminal nt_ inh_ syn_ (T_VisageProductions alts_ )  =
    (T_VisageNonterminal (let _lhsOaterm :: ATerm
                              _altsIaterms :: ([ATerm])
                              -- "Visage.ag"(line 63, column 19)
                              _lhsOaterm =
                                  AAppl "Production" [AString (sQ (getName nt_)), AString (sQ(showMap inh_)),
                                                     AString (sQ(showMap syn_)), AAppl "Alternatives" _altsIaterms]
                              ( _altsIaterms) =
                                  (alts_ )
                          in  ( _lhsOaterm)) )
-- VisageNonterminals ------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterms               : [ATerm]
   alternatives:
      alternative Cons:
         child hd             : VisageNonterminal 
         child tl             : VisageNonterminals 
      alternative Nil:
-}
-- cata
sem_VisageNonterminals :: VisageNonterminals  ->
                          T_VisageNonterminals 
sem_VisageNonterminals list  =
    (Prelude.foldr sem_VisageNonterminals_Cons sem_VisageNonterminals_Nil (Prelude.map sem_VisageNonterminal list) )
-- semantic domain
newtype T_VisageNonterminals  = T_VisageNonterminals (( ([ATerm])))
data Inh_VisageNonterminals  = Inh_VisageNonterminals {}
data Syn_VisageNonterminals  = Syn_VisageNonterminals {aterms_Syn_VisageNonterminals :: [ATerm]}
wrap_VisageNonterminals :: T_VisageNonterminals  ->
                           Inh_VisageNonterminals  ->
                           Syn_VisageNonterminals 
wrap_VisageNonterminals (T_VisageNonterminals sem ) (Inh_VisageNonterminals )  =
    (let ( _lhsOaterms) =
             (sem )
     in  (Syn_VisageNonterminals _lhsOaterms ))
sem_VisageNonterminals_Cons :: T_VisageNonterminal  ->
                               T_VisageNonterminals  ->
                               T_VisageNonterminals 
sem_VisageNonterminals_Cons (T_VisageNonterminal hd_ ) (T_VisageNonterminals tl_ )  =
    (T_VisageNonterminals (let _lhsOaterms :: ([ATerm])
                               _hdIaterm :: ATerm
                               _tlIaterms :: ([ATerm])
                               -- "Visage.ag"(line 58, column 17)
                               _lhsOaterms =
                                   _hdIaterm : _tlIaterms
                               ( _hdIaterm) =
                                   (hd_ )
                               ( _tlIaterms) =
                                   (tl_ )
                           in  ( _lhsOaterms)) )
sem_VisageNonterminals_Nil :: T_VisageNonterminals 
sem_VisageNonterminals_Nil  =
    (T_VisageNonterminals (let _lhsOaterms :: ([ATerm])
                               -- "Visage.ag"(line 59, column 17)
                               _lhsOaterms =
                                   []
                           in  ( _lhsOaterms)) )
-- VisagePattern -----------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterm                : ATerm
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
-- cata
sem_VisagePattern :: VisagePattern  ->
                     T_VisagePattern 
sem_VisagePattern (VAlias _field _attr _pat )  =
    (sem_VisagePattern_VAlias _field _attr (sem_VisagePattern _pat ) )
sem_VisagePattern (VConstr _name _pats )  =
    (sem_VisagePattern_VConstr _name (sem_VisagePatterns _pats ) )
sem_VisagePattern (VProduct _pos _pats )  =
    (sem_VisagePattern_VProduct _pos (sem_VisagePatterns _pats ) )
sem_VisagePattern (VUnderscore _pos )  =
    (sem_VisagePattern_VUnderscore _pos )
sem_VisagePattern (VVar _field _attr )  =
    (sem_VisagePattern_VVar _field _attr )
-- semantic domain
newtype T_VisagePattern  = T_VisagePattern (( ATerm))
data Inh_VisagePattern  = Inh_VisagePattern {}
data Syn_VisagePattern  = Syn_VisagePattern {aterm_Syn_VisagePattern :: ATerm}
wrap_VisagePattern :: T_VisagePattern  ->
                      Inh_VisagePattern  ->
                      Syn_VisagePattern 
wrap_VisagePattern (T_VisagePattern sem ) (Inh_VisagePattern )  =
    (let ( _lhsOaterm) =
             (sem )
     in  (Syn_VisagePattern _lhsOaterm ))
sem_VisagePattern_VAlias :: Identifier ->
                            Identifier ->
                            T_VisagePattern  ->
                            T_VisagePattern 
sem_VisagePattern_VAlias field_ attr_ (T_VisagePattern pat_ )  =
    (T_VisagePattern (let _lhsOaterm :: ATerm
                          _patIaterm :: ATerm
                          -- "Visage.ag"(line 119, column 18)
                          _lhsOaterm =
                              AAppl "Pattern" [AAppl "Alias" [AString (sQ (showAGPos (getPos field_))),
                                                              AString (sQ (getName field_ ++ "." ++ getName attr_)), _patIaterm]]
                          ( _patIaterm) =
                              (pat_ )
                      in  ( _lhsOaterm)) )
sem_VisagePattern_VConstr :: ConstructorIdent ->
                             T_VisagePatterns  ->
                             T_VisagePattern 
sem_VisagePattern_VConstr name_ (T_VisagePatterns pats_ )  =
    (T_VisagePattern (let _lhsOaterm :: ATerm
                          _patsIaterms :: ([ATerm])
                          -- "Visage.ag"(line 112, column 18)
                          _lhsOaterm =
                              AAppl "Pattern" [AAppl "Constr" [AString (sQ (showAGPos (getPos name_))),
                                               AString (sQ (getName name_)),
                                               AAppl "Patterns" _patsIaterms]]
                          ( _patsIaterms) =
                              (pats_ )
                      in  ( _lhsOaterm)) )
sem_VisagePattern_VProduct :: Pos ->
                              T_VisagePatterns  ->
                              T_VisagePattern 
sem_VisagePattern_VProduct pos_ (T_VisagePatterns pats_ )  =
    (T_VisagePattern (let _lhsOaterm :: ATerm
                          _patsIaterms :: ([ATerm])
                          -- "Visage.ag"(line 115, column 18)
                          _lhsOaterm =
                              AAppl "Pattern" [AAppl "Product" [AString (sQ (showAGPos pos_)),
                                                                AAppl "Patterns" _patsIaterms]]
                          ( _patsIaterms) =
                              (pats_ )
                      in  ( _lhsOaterm)) )
sem_VisagePattern_VUnderscore :: Pos ->
                                 T_VisagePattern 
sem_VisagePattern_VUnderscore pos_  =
    (T_VisagePattern (let _lhsOaterm :: ATerm
                          -- "Visage.ag"(line 121, column 18)
                          _lhsOaterm =
                              AAppl "Pattern" [AAppl "Underscore" [AString (sQ (showAGPos pos_))]]
                      in  ( _lhsOaterm)) )
sem_VisagePattern_VVar :: Identifier ->
                          Identifier ->
                          T_VisagePattern 
sem_VisagePattern_VVar field_ attr_  =
    (T_VisagePattern (let _lhsOaterm :: ATerm
                          -- "Visage.ag"(line 117, column 18)
                          _lhsOaterm =
                              AAppl "Pattern" [AAppl "Var" [AString (sQ (showAGPos (getPos field_))),
                                                            AString (sQ (getName field_ ++ "." ++ getName attr_))]]
                      in  ( _lhsOaterm)) )
-- VisagePatterns ----------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterms               : [ATerm]
   alternatives:
      alternative Cons:
         child hd             : VisagePattern 
         child tl             : VisagePatterns 
      alternative Nil:
-}
-- cata
sem_VisagePatterns :: VisagePatterns  ->
                      T_VisagePatterns 
sem_VisagePatterns list  =
    (Prelude.foldr sem_VisagePatterns_Cons sem_VisagePatterns_Nil (Prelude.map sem_VisagePattern list) )
-- semantic domain
newtype T_VisagePatterns  = T_VisagePatterns (( ([ATerm])))
data Inh_VisagePatterns  = Inh_VisagePatterns {}
data Syn_VisagePatterns  = Syn_VisagePatterns {aterms_Syn_VisagePatterns :: [ATerm]}
wrap_VisagePatterns :: T_VisagePatterns  ->
                       Inh_VisagePatterns  ->
                       Syn_VisagePatterns 
wrap_VisagePatterns (T_VisagePatterns sem ) (Inh_VisagePatterns )  =
    (let ( _lhsOaterms) =
             (sem )
     in  (Syn_VisagePatterns _lhsOaterms ))
sem_VisagePatterns_Cons :: T_VisagePattern  ->
                           T_VisagePatterns  ->
                           T_VisagePatterns 
sem_VisagePatterns_Cons (T_VisagePattern hd_ ) (T_VisagePatterns tl_ )  =
    (T_VisagePatterns (let _lhsOaterms :: ([ATerm])
                           _hdIaterm :: ATerm
                           _tlIaterms :: ([ATerm])
                           -- "Visage.ag"(line 107, column 17)
                           _lhsOaterms =
                               _hdIaterm : _tlIaterms
                           ( _hdIaterm) =
                               (hd_ )
                           ( _tlIaterms) =
                               (tl_ )
                       in  ( _lhsOaterms)) )
sem_VisagePatterns_Nil :: T_VisagePatterns 
sem_VisagePatterns_Nil  =
    (T_VisagePatterns (let _lhsOaterms :: ([ATerm])
                           -- "Visage.ag"(line 108, column 17)
                           _lhsOaterms =
                               []
                       in  ( _lhsOaterms)) )
-- VisageProduction --------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterm                : ATerm
   alternatives:
      alternative VProduction:
         child con            : {ConstructorIdent}
         child children       : VisageChildren 
         child rules          : VisageRules 
         child locrules       : VisageRules 
-}
-- cata
sem_VisageProduction :: VisageProduction  ->
                        T_VisageProduction 
sem_VisageProduction (VProduction _con _children _rules _locrules )  =
    (sem_VisageProduction_VProduction _con (sem_VisageChildren _children ) (sem_VisageRules _rules ) (sem_VisageRules _locrules ) )
-- semantic domain
newtype T_VisageProduction  = T_VisageProduction (( ATerm))
data Inh_VisageProduction  = Inh_VisageProduction {}
data Syn_VisageProduction  = Syn_VisageProduction {aterm_Syn_VisageProduction :: ATerm}
wrap_VisageProduction :: T_VisageProduction  ->
                         Inh_VisageProduction  ->
                         Syn_VisageProduction 
wrap_VisageProduction (T_VisageProduction sem ) (Inh_VisageProduction )  =
    (let ( _lhsOaterm) =
             (sem )
     in  (Syn_VisageProduction _lhsOaterm ))
sem_VisageProduction_VProduction :: ConstructorIdent ->
                                    T_VisageChildren  ->
                                    T_VisageRules  ->
                                    T_VisageRules  ->
                                    T_VisageProduction 
sem_VisageProduction_VProduction con_ (T_VisageChildren children_ ) (T_VisageRules rules_ ) (T_VisageRules locrules_ )  =
    (T_VisageProduction (let _lhsOaterm :: ATerm
                             _locrulesOisLoc :: Bool
                             _rulesOisLoc :: Bool
                             _childrenIaterms :: ([ATerm])
                             _rulesIaterms :: ([ATerm])
                             _locrulesIaterms :: ([ATerm])
                             -- "Visage.ag"(line 73, column 17)
                             _lhsOaterm =
                                 AAppl "Alternative" [AString (sQ (getName con_)), AAppl "Children" _childrenIaterms,
                                                       AAppl "Rules" _rulesIaterms,
                                                       AAppl "LocRules" _locrulesIaterms]
                             -- "Visage.ag"(line 76, column 18)
                             _locrulesOisLoc =
                                 True
                             -- "Visage.ag"(line 77, column 18)
                             _rulesOisLoc =
                                 False
                             ( _childrenIaterms) =
                                 (children_ )
                             ( _rulesIaterms) =
                                 (rules_ _rulesOisLoc )
                             ( _locrulesIaterms) =
                                 (locrules_ _locrulesOisLoc )
                         in  ( _lhsOaterm)) )
-- VisageProductions -------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterms               : [ATerm]
   alternatives:
      alternative Cons:
         child hd             : VisageProduction 
         child tl             : VisageProductions 
      alternative Nil:
-}
-- cata
sem_VisageProductions :: VisageProductions  ->
                         T_VisageProductions 
sem_VisageProductions list  =
    (Prelude.foldr sem_VisageProductions_Cons sem_VisageProductions_Nil (Prelude.map sem_VisageProduction list) )
-- semantic domain
newtype T_VisageProductions  = T_VisageProductions (( ([ATerm])))
data Inh_VisageProductions  = Inh_VisageProductions {}
data Syn_VisageProductions  = Syn_VisageProductions {aterms_Syn_VisageProductions :: [ATerm]}
wrap_VisageProductions :: T_VisageProductions  ->
                          Inh_VisageProductions  ->
                          Syn_VisageProductions 
wrap_VisageProductions (T_VisageProductions sem ) (Inh_VisageProductions )  =
    (let ( _lhsOaterms) =
             (sem )
     in  (Syn_VisageProductions _lhsOaterms ))
sem_VisageProductions_Cons :: T_VisageProduction  ->
                              T_VisageProductions  ->
                              T_VisageProductions 
sem_VisageProductions_Cons (T_VisageProduction hd_ ) (T_VisageProductions tl_ )  =
    (T_VisageProductions (let _lhsOaterms :: ([ATerm])
                              _hdIaterm :: ATerm
                              _tlIaterms :: ([ATerm])
                              -- "Visage.ag"(line 68, column 17)
                              _lhsOaterms =
                                  _hdIaterm : _tlIaterms
                              ( _hdIaterm) =
                                  (hd_ )
                              ( _tlIaterms) =
                                  (tl_ )
                          in  ( _lhsOaterms)) )
sem_VisageProductions_Nil :: T_VisageProductions 
sem_VisageProductions_Nil  =
    (T_VisageProductions (let _lhsOaterms :: ([ATerm])
                              -- "Visage.ag"(line 69, column 17)
                              _lhsOaterms =
                                  []
                          in  ( _lhsOaterms)) )
-- VisageRule --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         isLoc                : Bool
      synthesized attribute:
         aterm                : ATerm
   alternatives:
      alternative VRule:
         child fieldattrs     : {[(Identifier,Identifier)]}
         child attr           : {Identifier}
         child pat            : VisagePattern 
         child rhs            : Expression 
         child owrt           : {Bool}
-}
-- cata
sem_VisageRule :: VisageRule  ->
                  T_VisageRule 
sem_VisageRule (VRule _fieldattrs _attr _pat _rhs _owrt )  =
    (sem_VisageRule_VRule _fieldattrs _attr (sem_VisagePattern _pat ) (sem_Expression _rhs ) _owrt )
-- semantic domain
newtype T_VisageRule  = T_VisageRule (Bool ->
                                      ( ATerm))
data Inh_VisageRule  = Inh_VisageRule {isLoc_Inh_VisageRule :: Bool}
data Syn_VisageRule  = Syn_VisageRule {aterm_Syn_VisageRule :: ATerm}
wrap_VisageRule :: T_VisageRule  ->
                   Inh_VisageRule  ->
                   Syn_VisageRule 
wrap_VisageRule (T_VisageRule sem ) (Inh_VisageRule _lhsIisLoc )  =
    (let ( _lhsOaterm) =
             (sem _lhsIisLoc )
     in  (Syn_VisageRule _lhsOaterm ))
sem_VisageRule_VRule :: ([(Identifier,Identifier)]) ->
                        Identifier ->
                        T_VisagePattern  ->
                        T_Expression  ->
                        Bool ->
                        T_VisageRule 
sem_VisageRule_VRule fieldattrs_ attr_ (T_VisagePattern pat_ ) (T_Expression rhs_ ) owrt_  =
    (T_VisageRule (\ _lhsIisLoc ->
                       (let _lhsOaterm :: ATerm
                            _patIaterm :: ATerm
                            _rhsIaterm :: ATerm
                            -- "Visage.ag"(line 97, column 18)
                            _lhsOaterm =
                                AAppl (if _lhsIisLoc then "LocRule" else "Rule")
                                      ([AString (sQ (getName attr_)), _patIaterm, _rhsIaterm] ++ if _lhsIisLoc then [AString (sQ (show owrt_))] else [])
                            ( _patIaterm) =
                                (pat_ )
                            ( _rhsIaterm) =
                                (rhs_ )
                        in  ( _lhsOaterm))) )
-- VisageRules -------------------------------------------------
{-
   visit 0:
      inherited attribute:
         isLoc                : Bool
      synthesized attribute:
         aterms               : [ATerm]
   alternatives:
      alternative Cons:
         child hd             : VisageRule 
         child tl             : VisageRules 
      alternative Nil:
-}
-- cata
sem_VisageRules :: VisageRules  ->
                   T_VisageRules 
sem_VisageRules list  =
    (Prelude.foldr sem_VisageRules_Cons sem_VisageRules_Nil (Prelude.map sem_VisageRule list) )
-- semantic domain
newtype T_VisageRules  = T_VisageRules (Bool ->
                                        ( ([ATerm])))
data Inh_VisageRules  = Inh_VisageRules {isLoc_Inh_VisageRules :: Bool}
data Syn_VisageRules  = Syn_VisageRules {aterms_Syn_VisageRules :: [ATerm]}
wrap_VisageRules :: T_VisageRules  ->
                    Inh_VisageRules  ->
                    Syn_VisageRules 
wrap_VisageRules (T_VisageRules sem ) (Inh_VisageRules _lhsIisLoc )  =
    (let ( _lhsOaterms) =
             (sem _lhsIisLoc )
     in  (Syn_VisageRules _lhsOaterms ))
sem_VisageRules_Cons :: T_VisageRule  ->
                        T_VisageRules  ->
                        T_VisageRules 
sem_VisageRules_Cons (T_VisageRule hd_ ) (T_VisageRules tl_ )  =
    (T_VisageRules (\ _lhsIisLoc ->
                        (let _lhsOaterms :: ([ATerm])
                             _hdOisLoc :: Bool
                             _tlOisLoc :: Bool
                             _hdIaterm :: ATerm
                             _tlIaterms :: ([ATerm])
                             -- "Visage.ag"(line 92, column 17)
                             _lhsOaterms =
                                 _hdIaterm : _tlIaterms
                             -- copy rule (down)
                             _hdOisLoc =
                                 _lhsIisLoc
                             -- copy rule (down)
                             _tlOisLoc =
                                 _lhsIisLoc
                             ( _hdIaterm) =
                                 (hd_ _hdOisLoc )
                             ( _tlIaterms) =
                                 (tl_ _tlOisLoc )
                         in  ( _lhsOaterms))) )
sem_VisageRules_Nil :: T_VisageRules 
sem_VisageRules_Nil  =
    (T_VisageRules (\ _lhsIisLoc ->
                        (let _lhsOaterms :: ([ATerm])
                             -- "Visage.ag"(line 93, column 17)
                             _lhsOaterms =
                                 []
                         in  ( _lhsOaterms))) )