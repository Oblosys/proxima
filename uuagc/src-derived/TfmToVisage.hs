

-- UUAGC 0.9.10 (TfmToVisage.ag)
module TfmToVisage where

import AbstractSyntax
import VisagePatterns
import VisageSyntax


-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import CommonTypes


-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)


import UU.Scanner.Position(Pos)
import HsToken

-- Maps a rule to a pair 
-- Later, I expect to map to a list of rules, because we might need to unfold.


-- Checks that a certain alias is in fact a Var in the old representation of the AG system
isVar (Alias _ _ (Underscore _) _) = True
isVar _ = False

type VisageRuleMap = [(String, VisageRule)]

splitVRules :: [VisageRule] -> VisageRuleMap
splitVRules vrs = concat (map unfoldvrs vrs)

unfoldvrs :: VisageRule -> VisageRuleMap
unfoldvrs vr@(VRule attrfields _ _ _ _) = zip (map (getName . fst) attrfields) (map (copyRule vr) attrfields)

copyRule :: VisageRule -> (Identifier,Identifier) -> VisageRule
copyRule (VRule attrfields _ pat expr owrt) (field,attr) = VRule attrfields attr pat expr owrt

getForField :: String -> VisageRuleMap -> [VisageRule]
getForField field xs = map snd (filter ((field ==) . fst) xs)

{-
   Delivers a map from fieldname to VisageRule with all references to others underscored.
   So, (lhs.x, rt.y, loc.z) = (0,1,2) becomes something like
   [("lhs", (lhs.x,_,_) = (0,1,2)

   At this point, we do not use this anymore.
   
allways :: VisageRule -> VisageRuleMap
allways vr@(VRule vrfields _ _ _ _) = zip vrfields (map (underScoreRule vr) (nub vrfields))

splitVRules :: [VisageRule] -> VisageRuleMap
splitVRules vrs = concat (map allways vrs)

underScoreRule :: VisageRule -> String -> VisageRule
underScoreRule (VRule fields pat expr owrt rule) s = VRule fields (underScore s pat) expr owrt rule

underScore :: String -> VisagePattern -> VisagePattern
underScore field (VConstr name pats) = VConstr name (map (underScore field) pats) 
underScore field (VProduct pos pats) = VProduct pos (map (underScore field) pats)
underScore field vp@(VVar vfield attr)  = 
   if (field == getName vfield) 
   then vp
   else (VUnderscore (getPos vfield))
-- Should I recurse into the pat of VAlias?
underScore field vp@(VAlias afield attr pat) =
   if (field == getName afield) 
   then vp
   else (VUnderscore (getPos afield))
underScore field vp@(VUnderscore pos) = vp
                 
-}
-- Child -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rulemap              : VisageRuleMap
      synthesized attribute:
         vchild               : VisageChild
   alternatives:
      alternative Child:
         child name           : {Identifier}
         child tp             : {Type}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child higherOrder    : {Bool}
-}
-- cata
sem_Child :: Child  ->
             T_Child 
sem_Child (Child _name _tp _inh _syn _higherOrder )  =
    (sem_Child_Child _name _tp _inh _syn _higherOrder )
-- semantic domain
newtype T_Child  = T_Child (VisageRuleMap ->
                            ( VisageChild))
data Inh_Child  = Inh_Child {rulemap_Inh_Child :: VisageRuleMap}
data Syn_Child  = Syn_Child {vchild_Syn_Child :: VisageChild}
wrap_Child :: T_Child  ->
              Inh_Child  ->
              Syn_Child 
wrap_Child (T_Child sem ) (Inh_Child _lhsIrulemap )  =
    (let ( _lhsOvchild) =
             (sem _lhsIrulemap )
     in  (Syn_Child _lhsOvchild ))
sem_Child_Child :: Identifier ->
                   Type ->
                   Attributes ->
                   Attributes ->
                   Bool ->
                   T_Child 
sem_Child_Child name_ tp_ inh_ syn_ higherOrder_  =
    (T_Child (\ _lhsIrulemap ->
                  (let _lhsOvchild :: VisageChild
                       -- "TfmToVisage.ag"(line 111, column 11)
                       _lhsOvchild =
                           VChild name_ tp_ inh_ syn_ (getForField (getName name_) _lhsIrulemap)
                   in  ( _lhsOvchild))) )
-- Children ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rulemap              : VisageRuleMap
      synthesized attribute:
         vchildren            : [VisageChild]
   alternatives:
      alternative Cons:
         child hd             : Child 
         child tl             : Children 
      alternative Nil:
-}
-- cata
sem_Children :: Children  ->
                T_Children 
sem_Children list  =
    (Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list) )
-- semantic domain
newtype T_Children  = T_Children (VisageRuleMap ->
                                  ( ([VisageChild])))
data Inh_Children  = Inh_Children {rulemap_Inh_Children :: VisageRuleMap}
data Syn_Children  = Syn_Children {vchildren_Syn_Children :: [VisageChild]}
wrap_Children :: T_Children  ->
                 Inh_Children  ->
                 Syn_Children 
wrap_Children (T_Children sem ) (Inh_Children _lhsIrulemap )  =
    (let ( _lhsOvchildren) =
             (sem _lhsIrulemap )
     in  (Syn_Children _lhsOvchildren ))
sem_Children_Cons :: T_Child  ->
                     T_Children  ->
                     T_Children 
sem_Children_Cons (T_Child hd_ ) (T_Children tl_ )  =
    (T_Children (\ _lhsIrulemap ->
                     (let _lhsOvchildren :: ([VisageChild])
                          _hdOrulemap :: VisageRuleMap
                          _tlOrulemap :: VisageRuleMap
                          _hdIvchild :: VisageChild
                          _tlIvchildren :: ([VisageChild])
                          -- "TfmToVisage.ag"(line 107, column 17)
                          _lhsOvchildren =
                              _hdIvchild : _tlIvchildren
                          -- copy rule (down)
                          _hdOrulemap =
                              _lhsIrulemap
                          -- copy rule (down)
                          _tlOrulemap =
                              _lhsIrulemap
                          ( _hdIvchild) =
                              (hd_ _hdOrulemap )
                          ( _tlIvchildren) =
                              (tl_ _tlOrulemap )
                      in  ( _lhsOvchildren))) )
sem_Children_Nil :: T_Children 
sem_Children_Nil  =
    (T_Children (\ _lhsIrulemap ->
                     (let _lhsOvchildren :: ([VisageChild])
                          -- "TfmToVisage.ag"(line 108, column 17)
                          _lhsOvchildren =
                              []
                      in  ( _lhsOvchildren))) )
-- Expression --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
         visit 0:
            local self        : _
-}
-- cata
sem_Expression :: Expression  ->
                  T_Expression 
sem_Expression (Expression _pos _tks )  =
    (sem_Expression_Expression _pos _tks )
-- semantic domain
newtype T_Expression  = T_Expression (( Expression))
data Inh_Expression  = Inh_Expression {}
data Syn_Expression  = Syn_Expression {self_Syn_Expression :: Expression}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression (T_Expression sem ) (Inh_Expression )  =
    (let ( _lhsOself) =
             (sem )
     in  (Syn_Expression _lhsOself ))
sem_Expression_Expression :: Pos ->
                             ([HsToken]) ->
                             T_Expression 
sem_Expression_Expression pos_ tks_  =
    (T_Expression (let _lhsOself :: Expression
                       -- self rule
                       _self =
                           Expression pos_ tks_
                       -- self rule
                       _lhsOself =
                           _self
                   in  ( _lhsOself)) )
-- Grammar -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         visage               : VisageGrammar
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
-- cata
sem_Grammar :: Grammar  ->
               T_Grammar 
sem_Grammar (Grammar _typeSyns _useMap _derivings _wrappers _nonts _pragmas _manualAttrOrderMap _paramMap _contextMap _uniqueMap )  =
    (sem_Grammar_Grammar _typeSyns _useMap _derivings _wrappers (sem_Nonterminals _nonts ) _pragmas _manualAttrOrderMap _paramMap _contextMap _uniqueMap )
-- semantic domain
newtype T_Grammar  = T_Grammar (( VisageGrammar))
data Inh_Grammar  = Inh_Grammar {}
data Syn_Grammar  = Syn_Grammar {visage_Syn_Grammar :: VisageGrammar}
wrap_Grammar :: T_Grammar  ->
                Inh_Grammar  ->
                Syn_Grammar 
wrap_Grammar (T_Grammar sem ) (Inh_Grammar )  =
    (let ( _lhsOvisage) =
             (sem )
     in  (Syn_Grammar _lhsOvisage ))
sem_Grammar_Grammar :: TypeSyns ->
                       UseMap ->
                       Derivings ->
                       (Set NontermIdent) ->
                       T_Nonterminals  ->
                       PragmaMap ->
                       AttrOrderMap ->
                       ParamMap ->
                       ContextMap ->
                       UniqueMap ->
                       T_Grammar 
sem_Grammar_Grammar typeSyns_ useMap_ derivings_ wrappers_ (T_Nonterminals nonts_ ) pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ uniqueMap_  =
    (T_Grammar (let _lhsOvisage :: VisageGrammar
                    _nontsIvnonts :: ([VisageNonterminal])
                    -- "TfmToVisage.ag"(line 86, column 17)
                    _lhsOvisage =
                        VGrammar _nontsIvnonts
                    ( _nontsIvnonts) =
                        (nonts_ )
                in  ( _lhsOvisage)) )
-- Nonterminal -------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         vnont                : VisageNonterminal
   alternatives:
      alternative Nonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : Productions 
-}
-- cata
sem_Nonterminal :: Nonterminal  ->
                   T_Nonterminal 
sem_Nonterminal (Nonterminal _nt _params _inh _syn _prods )  =
    (sem_Nonterminal_Nonterminal _nt _params _inh _syn (sem_Productions _prods ) )
-- semantic domain
newtype T_Nonterminal  = T_Nonterminal (( VisageNonterminal))
data Inh_Nonterminal  = Inh_Nonterminal {}
data Syn_Nonterminal  = Syn_Nonterminal {vnont_Syn_Nonterminal :: VisageNonterminal}
wrap_Nonterminal :: T_Nonterminal  ->
                    Inh_Nonterminal  ->
                    Syn_Nonterminal 
wrap_Nonterminal (T_Nonterminal sem ) (Inh_Nonterminal )  =
    (let ( _lhsOvnont) =
             (sem )
     in  (Syn_Nonterminal _lhsOvnont ))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions  ->
                               T_Nonterminal 
sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ (T_Productions prods_ )  =
    (T_Nonterminal (let _lhsOvnont :: VisageNonterminal
                        _prodsIvprods :: ([VisageProduction])
                        -- "TfmToVisage.ag"(line 93, column 18)
                        _lhsOvnont =
                            VNonterminal nt_ inh_ syn_ _prodsIvprods
                        ( _prodsIvprods) =
                            (prods_ )
                    in  ( _lhsOvnont)) )
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         vnonts               : [VisageNonterminal]
   alternatives:
      alternative Cons:
         child hd             : Nonterminal 
         child tl             : Nonterminals 
      alternative Nil:
-}
-- cata
sem_Nonterminals :: Nonterminals  ->
                    T_Nonterminals 
sem_Nonterminals list  =
    (Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list) )
-- semantic domain
newtype T_Nonterminals  = T_Nonterminals (( ([VisageNonterminal])))
data Inh_Nonterminals  = Inh_Nonterminals {}
data Syn_Nonterminals  = Syn_Nonterminals {vnonts_Syn_Nonterminals :: [VisageNonterminal]}
wrap_Nonterminals :: T_Nonterminals  ->
                     Inh_Nonterminals  ->
                     Syn_Nonterminals 
wrap_Nonterminals (T_Nonterminals sem ) (Inh_Nonterminals )  =
    (let ( _lhsOvnonts) =
             (sem )
     in  (Syn_Nonterminals _lhsOvnonts ))
sem_Nonterminals_Cons :: T_Nonterminal  ->
                         T_Nonterminals  ->
                         T_Nonterminals 
sem_Nonterminals_Cons (T_Nonterminal hd_ ) (T_Nonterminals tl_ )  =
    (T_Nonterminals (let _lhsOvnonts :: ([VisageNonterminal])
                         _hdIvnont :: VisageNonterminal
                         _tlIvnonts :: ([VisageNonterminal])
                         -- "TfmToVisage.ag"(line 89, column 17)
                         _lhsOvnonts =
                             _hdIvnont : _tlIvnonts
                         ( _hdIvnont) =
                             (hd_ )
                         ( _tlIvnonts) =
                             (tl_ )
                     in  ( _lhsOvnonts)) )
sem_Nonterminals_Nil :: T_Nonterminals 
sem_Nonterminals_Nil  =
    (T_Nonterminals (let _lhsOvnonts :: ([VisageNonterminal])
                         -- "TfmToVisage.ag"(line 90, column 17)
                         _lhsOvnonts =
                             []
                     in  ( _lhsOvnonts)) )
-- Pattern -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         fieldattrs           :  [(Identifier,Identifier)] 
         self                 : SELF 
         vpat                 : VisagePattern
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         child parts          : Patterns 
         visit 0:
            local copy        : _
            local self        : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
            local self        : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
            local self        : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
            local self        : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
            local self        : _
-}
-- cata
sem_Pattern :: Pattern  ->
               T_Pattern 
sem_Pattern (Alias _field _attr _pat _parts )  =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat ) (sem_Patterns _parts ) )
sem_Pattern (Constr _name _pats )  =
    (sem_Pattern_Constr _name (sem_Patterns _pats ) )
sem_Pattern (Irrefutable _pat )  =
    (sem_Pattern_Irrefutable (sem_Pattern _pat ) )
sem_Pattern (Product _pos _pats )  =
    (sem_Pattern_Product _pos (sem_Patterns _pats ) )
sem_Pattern (Underscore _pos )  =
    (sem_Pattern_Underscore _pos )
-- semantic domain
newtype T_Pattern  = T_Pattern (( Pattern,( [(Identifier,Identifier)] ),Pattern,VisagePattern))
data Inh_Pattern  = Inh_Pattern {}
data Syn_Pattern  = Syn_Pattern {copy_Syn_Pattern :: Pattern,fieldattrs_Syn_Pattern ::  [(Identifier,Identifier)] ,self_Syn_Pattern :: Pattern,vpat_Syn_Pattern :: VisagePattern}
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern (T_Pattern sem ) (Inh_Pattern )  =
    (let ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpat) =
             (sem )
     in  (Syn_Pattern _lhsOcopy _lhsOfieldattrs _lhsOself _lhsOvpat ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Alias field_ attr_ (T_Pattern pat_ ) (T_Patterns parts_ )  =
    (T_Pattern (let _lhsOvpat :: VisagePattern
                    _lhsOfieldattrs :: ( [(Identifier,Identifier)] )
                    _lhsOcopy :: Pattern
                    _lhsOself :: Pattern
                    _patIcopy :: Pattern
                    _patIfieldattrs :: ( [(Identifier,Identifier)] )
                    _patIself :: Pattern
                    _patIvpat :: VisagePattern
                    _partsIcopy :: Patterns
                    _partsIfieldattrs :: ( [(Identifier,Identifier)] )
                    _partsIself :: Patterns
                    _partsIvpats :: ([VisagePattern])
                    -- "TfmToVisage.ag"(line 128, column 17)
                    _lhsOvpat =
                        if (isVar _self)
                        then VVar field_ attr_
                        else VAlias field_ attr_ _patIvpat
                    -- "TfmToVisage.ag"(line 137, column 17)
                    _lhsOfieldattrs =
                        [(field_, attr_)]
                    -- self rule
                    _copy =
                        Alias field_ attr_ _patIcopy _partsIcopy
                    -- self rule
                    _self =
                        Alias field_ attr_ _patIself _partsIself
                    -- self rule
                    _lhsOcopy =
                        _copy
                    -- self rule
                    _lhsOself =
                        _self
                    ( _patIcopy,_patIfieldattrs,_patIself,_patIvpat) =
                        (pat_ )
                    ( _partsIcopy,_partsIfieldattrs,_partsIself,_partsIvpats) =
                        (parts_ )
                in  ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpat)) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr name_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOvpat :: VisagePattern
                    _lhsOfieldattrs :: ( [(Identifier,Identifier)] )
                    _lhsOcopy :: Pattern
                    _lhsOself :: Pattern
                    _patsIcopy :: Patterns
                    _patsIfieldattrs :: ( [(Identifier,Identifier)] )
                    _patsIself :: Patterns
                    _patsIvpats :: ([VisagePattern])
                    -- "TfmToVisage.ag"(line 126, column 17)
                    _lhsOvpat =
                        VConstr name_ _patsIvpats
                    -- use rule "TfmToVisage.ag"(line 134, column 43)
                    _lhsOfieldattrs =
                        _patsIfieldattrs
                    -- self rule
                    _copy =
                        Constr name_ _patsIcopy
                    -- self rule
                    _self =
                        Constr name_ _patsIself
                    -- self rule
                    _lhsOcopy =
                        _copy
                    -- self rule
                    _lhsOself =
                        _self
                    ( _patsIcopy,_patsIfieldattrs,_patsIself,_patsIvpats) =
                        (pats_ )
                in  ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpat)) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable (T_Pattern pat_ )  =
    (T_Pattern (let _lhsOfieldattrs :: ( [(Identifier,Identifier)] )
                    _lhsOcopy :: Pattern
                    _lhsOself :: Pattern
                    _lhsOvpat :: VisagePattern
                    _patIcopy :: Pattern
                    _patIfieldattrs :: ( [(Identifier,Identifier)] )
                    _patIself :: Pattern
                    _patIvpat :: VisagePattern
                    -- use rule "TfmToVisage.ag"(line 134, column 43)
                    _lhsOfieldattrs =
                        _patIfieldattrs
                    -- self rule
                    _copy =
                        Irrefutable _patIcopy
                    -- self rule
                    _self =
                        Irrefutable _patIself
                    -- self rule
                    _lhsOcopy =
                        _copy
                    -- self rule
                    _lhsOself =
                        _self
                    -- copy rule (up)
                    _lhsOvpat =
                        _patIvpat
                    ( _patIcopy,_patIfieldattrs,_patIself,_patIvpat) =
                        (pat_ )
                in  ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpat)) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product pos_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOvpat :: VisagePattern
                    _lhsOfieldattrs :: ( [(Identifier,Identifier)] )
                    _lhsOcopy :: Pattern
                    _lhsOself :: Pattern
                    _patsIcopy :: Patterns
                    _patsIfieldattrs :: ( [(Identifier,Identifier)] )
                    _patsIself :: Patterns
                    _patsIvpats :: ([VisagePattern])
                    -- "TfmToVisage.ag"(line 127, column 17)
                    _lhsOvpat =
                        VProduct pos_ _patsIvpats
                    -- use rule "TfmToVisage.ag"(line 134, column 43)
                    _lhsOfieldattrs =
                        _patsIfieldattrs
                    -- self rule
                    _copy =
                        Product pos_ _patsIcopy
                    -- self rule
                    _self =
                        Product pos_ _patsIself
                    -- self rule
                    _lhsOcopy =
                        _copy
                    -- self rule
                    _lhsOself =
                        _self
                    ( _patsIcopy,_patsIfieldattrs,_patsIself,_patsIvpats) =
                        (pats_ )
                in  ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpat)) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore pos_  =
    (T_Pattern (let _lhsOvpat :: VisagePattern
                    _lhsOfieldattrs :: ( [(Identifier,Identifier)] )
                    _lhsOcopy :: Pattern
                    _lhsOself :: Pattern
                    -- "TfmToVisage.ag"(line 131, column 17)
                    _lhsOvpat =
                        VUnderscore pos_
                    -- use rule "TfmToVisage.ag"(line 134, column 43)
                    _lhsOfieldattrs =
                        []
                    -- self rule
                    _copy =
                        Underscore pos_
                    -- self rule
                    _self =
                        Underscore pos_
                    -- self rule
                    _lhsOcopy =
                        _copy
                    -- self rule
                    _lhsOself =
                        _self
                in  ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpat)) )
-- Patterns ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         fieldattrs           :  [(Identifier,Identifier)] 
         self                 : SELF 
         vpats                : [VisagePattern]
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 0:
            local copy        : _
            local self        : _
      alternative Nil:
         visit 0:
            local copy        : _
            local self        : _
-}
-- cata
sem_Patterns :: Patterns  ->
                T_Patterns 
sem_Patterns list  =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list) )
-- semantic domain
newtype T_Patterns  = T_Patterns (( Patterns,( [(Identifier,Identifier)] ),Patterns,([VisagePattern])))
data Inh_Patterns  = Inh_Patterns {}
data Syn_Patterns  = Syn_Patterns {copy_Syn_Patterns :: Patterns,fieldattrs_Syn_Patterns ::  [(Identifier,Identifier)] ,self_Syn_Patterns :: Patterns,vpats_Syn_Patterns :: [VisagePattern]}
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns (T_Patterns sem ) (Inh_Patterns )  =
    (let ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpats) =
             (sem )
     in  (Syn_Patterns _lhsOcopy _lhsOfieldattrs _lhsOself _lhsOvpats ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons (T_Pattern hd_ ) (T_Patterns tl_ )  =
    (T_Patterns (let _lhsOvpats :: ([VisagePattern])
                     _lhsOfieldattrs :: ( [(Identifier,Identifier)] )
                     _lhsOcopy :: Patterns
                     _lhsOself :: Patterns
                     _hdIcopy :: Pattern
                     _hdIfieldattrs :: ( [(Identifier,Identifier)] )
                     _hdIself :: Pattern
                     _hdIvpat :: VisagePattern
                     _tlIcopy :: Patterns
                     _tlIfieldattrs :: ( [(Identifier,Identifier)] )
                     _tlIself :: Patterns
                     _tlIvpats :: ([VisagePattern])
                     -- "TfmToVisage.ag"(line 122, column 17)
                     _lhsOvpats =
                         _hdIvpat : _tlIvpats
                     -- use rule "TfmToVisage.ag"(line 134, column 43)
                     _lhsOfieldattrs =
                         _hdIfieldattrs  ++  _tlIfieldattrs
                     -- self rule
                     _copy =
                         (:) _hdIcopy _tlIcopy
                     -- self rule
                     _self =
                         (:) _hdIself _tlIself
                     -- self rule
                     _lhsOcopy =
                         _copy
                     -- self rule
                     _lhsOself =
                         _self
                     ( _hdIcopy,_hdIfieldattrs,_hdIself,_hdIvpat) =
                         (hd_ )
                     ( _tlIcopy,_tlIfieldattrs,_tlIself,_tlIvpats) =
                         (tl_ )
                 in  ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpats)) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (let _lhsOvpats :: ([VisagePattern])
                     _lhsOfieldattrs :: ( [(Identifier,Identifier)] )
                     _lhsOcopy :: Patterns
                     _lhsOself :: Patterns
                     -- "TfmToVisage.ag"(line 123, column 17)
                     _lhsOvpats =
                         []
                     -- use rule "TfmToVisage.ag"(line 134, column 43)
                     _lhsOfieldattrs =
                         []
                     -- self rule
                     _copy =
                         []
                     -- self rule
                     _self =
                         []
                     -- self rule
                     _lhsOcopy =
                         _copy
                     -- self rule
                     _lhsOself =
                         _self
                 in  ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpats)) )
-- Production --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         vprod                : VisageProduction
   alternatives:
      alternative Production:
         child con            : {ConstructorIdent}
         child children       : Children 
         child rules          : Rules 
         child typeSigs       : TypeSigs 
         visit 0:
            local splitVRules : _
            local locrules    : _
            local lhsrules    : _
-}
-- cata
sem_Production :: Production  ->
                  T_Production 
sem_Production (Production _con _children _rules _typeSigs )  =
    (sem_Production_Production _con (sem_Children _children ) (sem_Rules _rules ) (sem_TypeSigs _typeSigs ) )
-- semantic domain
newtype T_Production  = T_Production (( VisageProduction))
data Inh_Production  = Inh_Production {}
data Syn_Production  = Syn_Production {vprod_Syn_Production :: VisageProduction}
wrap_Production :: T_Production  ->
                   Inh_Production  ->
                   Syn_Production 
wrap_Production (T_Production sem ) (Inh_Production )  =
    (let ( _lhsOvprod) =
             (sem )
     in  (Syn_Production _lhsOvprod ))
sem_Production_Production :: ConstructorIdent ->
                             T_Children  ->
                             T_Rules  ->
                             T_TypeSigs  ->
                             T_Production 
sem_Production_Production con_ (T_Children children_ ) (T_Rules rules_ ) (T_TypeSigs typeSigs_ )  =
    (T_Production (let _lhsOvprod :: VisageProduction
                       _childrenOrulemap :: VisageRuleMap
                       _childrenIvchildren :: ([VisageChild])
                       _rulesIvrules :: ([VisageRule])
                       -- "TfmToVisage.ag"(line 100, column 17)
                       _lhsOvprod =
                           VProduction con_ _childrenIvchildren _lhsrules _locrules
                       -- "TfmToVisage.ag"(line 101, column 17)
                       _splitVRules =
                           splitVRules _rulesIvrules
                       -- "TfmToVisage.ag"(line 102, column 17)
                       _locrules =
                           getForField "loc" _splitVRules
                       -- "TfmToVisage.ag"(line 103, column 17)
                       _lhsrules =
                           getForField "lhs" _splitVRules
                       -- "TfmToVisage.ag"(line 104, column 17)
                       _childrenOrulemap =
                           _splitVRules
                       ( _childrenIvchildren) =
                           (children_ _childrenOrulemap )
                       ( _rulesIvrules) =
                           (rules_ )
                   in  ( _lhsOvprod)) )
-- Productions -------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         vprods               : [VisageProduction]
   alternatives:
      alternative Cons:
         child hd             : Production 
         child tl             : Productions 
      alternative Nil:
-}
-- cata
sem_Productions :: Productions  ->
                   T_Productions 
sem_Productions list  =
    (Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list) )
-- semantic domain
newtype T_Productions  = T_Productions (( ([VisageProduction])))
data Inh_Productions  = Inh_Productions {}
data Syn_Productions  = Syn_Productions {vprods_Syn_Productions :: [VisageProduction]}
wrap_Productions :: T_Productions  ->
                    Inh_Productions  ->
                    Syn_Productions 
wrap_Productions (T_Productions sem ) (Inh_Productions )  =
    (let ( _lhsOvprods) =
             (sem )
     in  (Syn_Productions _lhsOvprods ))
sem_Productions_Cons :: T_Production  ->
                        T_Productions  ->
                        T_Productions 
sem_Productions_Cons (T_Production hd_ ) (T_Productions tl_ )  =
    (T_Productions (let _lhsOvprods :: ([VisageProduction])
                        _hdIvprod :: VisageProduction
                        _tlIvprods :: ([VisageProduction])
                        -- "TfmToVisage.ag"(line 96, column 17)
                        _lhsOvprods =
                            _hdIvprod : _tlIvprods
                        ( _hdIvprod) =
                            (hd_ )
                        ( _tlIvprods) =
                            (tl_ )
                    in  ( _lhsOvprods)) )
sem_Productions_Nil :: T_Productions 
sem_Productions_Nil  =
    (T_Productions (let _lhsOvprods :: ([VisageProduction])
                        -- "TfmToVisage.ag"(line 97, column 17)
                        _lhsOvprods =
                            []
                    in  ( _lhsOvprods)) )
-- Rule --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         vrule                : VisageRule
   alternatives:
      alternative Rule:
         child pattern        : Pattern 
         child rhs            : Expression 
         child owrt           : {Bool}
         child origin         : {String}
-}
-- cata
sem_Rule :: Rule  ->
            T_Rule 
sem_Rule (Rule _pattern _rhs _owrt _origin )  =
    (sem_Rule_Rule (sem_Pattern _pattern ) (sem_Expression _rhs ) _owrt _origin )
-- semantic domain
newtype T_Rule  = T_Rule (( VisageRule))
data Inh_Rule  = Inh_Rule {}
data Syn_Rule  = Syn_Rule {vrule_Syn_Rule :: VisageRule}
wrap_Rule :: T_Rule  ->
             Inh_Rule  ->
             Syn_Rule 
wrap_Rule (T_Rule sem ) (Inh_Rule )  =
    (let ( _lhsOvrule) =
             (sem )
     in  (Syn_Rule _lhsOvrule ))
sem_Rule_Rule :: T_Pattern  ->
                 T_Expression  ->
                 Bool ->
                 String ->
                 T_Rule 
sem_Rule_Rule (T_Pattern pattern_ ) (T_Expression rhs_ ) owrt_ origin_  =
    (T_Rule (let _lhsOvrule :: VisageRule
                 _patternIcopy :: Pattern
                 _patternIfieldattrs :: ( [(Identifier,Identifier)] )
                 _patternIself :: Pattern
                 _patternIvpat :: VisagePattern
                 _rhsIself :: Expression
                 -- "TfmToVisage.ag"(line 119, column 11)
                 _lhsOvrule =
                     VRule _patternIfieldattrs undefined _patternIvpat _rhsIself owrt_
                 ( _patternIcopy,_patternIfieldattrs,_patternIself,_patternIvpat) =
                     (pattern_ )
                 ( _rhsIself) =
                     (rhs_ )
             in  ( _lhsOvrule)) )
-- Rules -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         vrules               : [VisageRule]
   alternatives:
      alternative Cons:
         child hd             : Rule 
         child tl             : Rules 
      alternative Nil:
-}
-- cata
sem_Rules :: Rules  ->
             T_Rules 
sem_Rules list  =
    (Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list) )
-- semantic domain
newtype T_Rules  = T_Rules (( ([VisageRule])))
data Inh_Rules  = Inh_Rules {}
data Syn_Rules  = Syn_Rules {vrules_Syn_Rules :: [VisageRule]}
wrap_Rules :: T_Rules  ->
              Inh_Rules  ->
              Syn_Rules 
wrap_Rules (T_Rules sem ) (Inh_Rules )  =
    (let ( _lhsOvrules) =
             (sem )
     in  (Syn_Rules _lhsOvrules ))
sem_Rules_Cons :: T_Rule  ->
                  T_Rules  ->
                  T_Rules 
sem_Rules_Cons (T_Rule hd_ ) (T_Rules tl_ )  =
    (T_Rules (let _lhsOvrules :: ([VisageRule])
                  _hdIvrule :: VisageRule
                  _tlIvrules :: ([VisageRule])
                  -- "TfmToVisage.ag"(line 114, column 17)
                  _lhsOvrules =
                      _hdIvrule : _tlIvrules
                  ( _hdIvrule) =
                      (hd_ )
                  ( _tlIvrules) =
                      (tl_ )
              in  ( _lhsOvrules)) )
sem_Rules_Nil :: T_Rules 
sem_Rules_Nil  =
    (T_Rules (let _lhsOvrules :: ([VisageRule])
                  -- "TfmToVisage.ag"(line 115, column 17)
                  _lhsOvrules =
                      []
              in  ( _lhsOvrules)) )
-- TypeSig -----------------------------------------------------
{-
   alternatives:
      alternative TypeSig:
         child name           : {Identifier}
         child tp             : {Type}
-}
-- cata
sem_TypeSig :: TypeSig  ->
               T_TypeSig 
sem_TypeSig (TypeSig _name _tp )  =
    (sem_TypeSig_TypeSig _name _tp )
-- semantic domain
newtype T_TypeSig  = T_TypeSig (( ))
data Inh_TypeSig  = Inh_TypeSig {}
data Syn_TypeSig  = Syn_TypeSig {}
wrap_TypeSig :: T_TypeSig  ->
                Inh_TypeSig  ->
                Syn_TypeSig 
wrap_TypeSig (T_TypeSig sem ) (Inh_TypeSig )  =
    (let ( ) =
             (sem )
     in  (Syn_TypeSig ))
sem_TypeSig_TypeSig :: Identifier ->
                       Type ->
                       T_TypeSig 
sem_TypeSig_TypeSig name_ tp_  =
    (T_TypeSig (let 
                in  ( )) )
-- TypeSigs ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : TypeSig 
         child tl             : TypeSigs 
      alternative Nil:
-}
-- cata
sem_TypeSigs :: TypeSigs  ->
                T_TypeSigs 
sem_TypeSigs list  =
    (Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list) )
-- semantic domain
newtype T_TypeSigs  = T_TypeSigs (( ))
data Inh_TypeSigs  = Inh_TypeSigs {}
data Syn_TypeSigs  = Syn_TypeSigs {}
wrap_TypeSigs :: T_TypeSigs  ->
                 Inh_TypeSigs  ->
                 Syn_TypeSigs 
wrap_TypeSigs (T_TypeSigs sem ) (Inh_TypeSigs )  =
    (let ( ) =
             (sem )
     in  (Syn_TypeSigs ))
sem_TypeSigs_Cons :: T_TypeSig  ->
                     T_TypeSigs  ->
                     T_TypeSigs 
sem_TypeSigs_Cons (T_TypeSig hd_ ) (T_TypeSigs tl_ )  =
    (T_TypeSigs (let 
                 in  ( )) )
sem_TypeSigs_Nil :: T_TypeSigs 
sem_TypeSigs_Nil  =
    (T_TypeSigs (let 
                 in  ( )) )