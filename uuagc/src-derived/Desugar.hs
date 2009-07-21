{-# OPTIONS_GHC -fbang-patterns #-}

-- UUAGC 0.9.10 (Desugar.ag)
module Desugar where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq,(><))
import UU.Scanner.Position(Pos(..))
import Maybe
import Data.List(intersperse)

import AbstractSyntax
import ErrorMessages
import Options
import HsToken
import HsTokenScanner
import TokenDef


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


import CommonTypes
import UU.Scanner.Position(Pos)

addl :: Int -> Pos -> Pos
addl n (Pos l c f) = Pos (l+n) c f


maybeError :: a -> Error -> Maybe a -> (a, Seq Error)
maybeError def err mb
  = maybe (def, Seq.singleton err) (\r -> (r, Seq.empty)) mb

findField :: Identifier -> Identifier -> [(Identifier,Identifier)] -> Maybe Identifier
findField fld attr list
  | fld == _FIRST = f list
  | fld == _LAST  = f (reverse list)
  | otherwise     = Just fld
  where
    f = lookup attr


mergeAttributes :: AttrMap -> AttrMap -> AttrMap
mergeAttributes = Map.unionWith $ Map.unionWith $ Set.union
-- Child -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         output               : SELF 
   alternatives:
      alternative Child:
         child name           : {Identifier}
         child tp             : {Type}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child higherOrder    : {Bool}
         visit 0:
            local output      : _
-}
-- cata
sem_Child :: Child  ->
             T_Child 
sem_Child !(Child _name _tp _inh _syn _higherOrder )  =
    (sem_Child_Child _name _tp _inh _syn _higherOrder )
-- semantic domain
newtype T_Child  = T_Child (( ([(Identifier, Identifier)]),([(Identifier, Identifier)]),Child))
data Inh_Child  = Inh_Child {}
data Syn_Child  = Syn_Child {childInhs_Syn_Child :: !([(Identifier, Identifier)]),childSyns_Syn_Child :: !([(Identifier, Identifier)]),output_Syn_Child :: !(Child)}
wrap_Child :: T_Child  ->
              Inh_Child  ->
              Syn_Child 
wrap_Child !(T_Child sem ) !(Inh_Child )  =
    (let ( !_lhsOchildInhs,!_lhsOchildSyns,!_lhsOoutput) =
             (sem )
     in  (Syn_Child _lhsOchildInhs _lhsOchildSyns _lhsOoutput ))
sem_Child_Child :: Identifier ->
                   Type ->
                   Attributes ->
                   Attributes ->
                   Bool ->
                   T_Child 
sem_Child_Child !name_ !tp_ !inh_ !syn_ !higherOrder_  =
    (T_Child (case ([(i, name_) | i <- Map.keys inh_ ]) of
              { !_lhsOchildInhs ->
              (case ([(s, name_) | s <- Map.keys syn_ ]) of
               { !_lhsOchildSyns ->
               (case (Child name_ tp_ inh_ syn_ higherOrder_) of
                { !_output ->
                (case (_output) of
                 { !_lhsOoutput ->
                 ( _lhsOchildInhs,_lhsOchildSyns,_lhsOoutput) }) }) }) }) )
-- Children ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         output               : SELF 
   alternatives:
      alternative Cons:
         child hd             : Child 
         child tl             : Children 
         visit 0:
            local output      : _
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_Children :: Children  ->
                T_Children 
sem_Children !list  =
    (Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list) )
-- semantic domain
newtype T_Children  = T_Children (( ([(Identifier, Identifier)]),([(Identifier, Identifier)]),Children))
data Inh_Children  = Inh_Children {}
data Syn_Children  = Syn_Children {childInhs_Syn_Children :: !([(Identifier, Identifier)]),childSyns_Syn_Children :: !([(Identifier, Identifier)]),output_Syn_Children :: !(Children)}
wrap_Children :: T_Children  ->
                 Inh_Children  ->
                 Syn_Children 
wrap_Children !(T_Children sem ) !(Inh_Children )  =
    (let ( !_lhsOchildInhs,!_lhsOchildSyns,!_lhsOoutput) =
             (sem )
     in  (Syn_Children _lhsOchildInhs _lhsOchildSyns _lhsOoutput ))
sem_Children_Cons :: T_Child  ->
                     T_Children  ->
                     T_Children 
sem_Children_Cons !(T_Child hd_ ) !(T_Children tl_ )  =
    (T_Children (case ((tl_ )) of
                 { ( !_tlIchildInhs,!_tlIchildSyns,!_tlIoutput) ->
                 (case ((hd_ )) of
                  { ( !_hdIchildInhs,!_hdIchildSyns,!_hdIoutput) ->
                  (case (_hdIchildInhs ++ _tlIchildInhs) of
                   { !_lhsOchildInhs ->
                   (case (_hdIchildSyns ++ _tlIchildSyns) of
                    { !_lhsOchildSyns ->
                    (case ((:) _hdIoutput _tlIoutput) of
                     { !_output ->
                     (case (_output) of
                      { !_lhsOoutput ->
                      ( _lhsOchildInhs,_lhsOchildSyns,_lhsOoutput) }) }) }) }) }) }) )
sem_Children_Nil :: T_Children 
sem_Children_Nil  =
    (T_Children (case ([]) of
                 { !_lhsOchildInhs ->
                 (case ([]) of
                  { !_lhsOchildSyns ->
                  (case ([]) of
                   { !_output ->
                   (case (_output) of
                    { !_lhsOoutput ->
                    ( _lhsOchildInhs,_lhsOchildSyns,_lhsOoutput) }) }) }) }) )
-- Expression --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         con                  : ConstructorIdent
         nt                   : NontermIdent
         options              : Options
         ruleDescr            : String
      synthesized attributes:
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
         visit 0:
            local _tup1       : _
            local tks'        : _
-}
-- cata
sem_Expression :: Expression  ->
                  T_Expression 
sem_Expression !(Expression _pos _tks )  =
    (sem_Expression_Expression _pos _tks )
-- semantic domain
newtype T_Expression  = T_Expression (([(Identifier, Identifier)]) ->
                                      ([(Identifier, Identifier)]) ->
                                      ConstructorIdent ->
                                      NontermIdent ->
                                      Options ->
                                      String ->
                                      ( (Seq Error),Expression))
data Inh_Expression  = Inh_Expression {childInhs_Inh_Expression :: !([(Identifier, Identifier)]),childSyns_Inh_Expression :: !([(Identifier, Identifier)]),con_Inh_Expression :: !(ConstructorIdent),nt_Inh_Expression :: !(NontermIdent),options_Inh_Expression :: !(Options),ruleDescr_Inh_Expression :: !(String)}
data Syn_Expression  = Syn_Expression {errors_Syn_Expression :: !(Seq Error),output_Syn_Expression :: !(Expression)}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression !(T_Expression sem ) !(Inh_Expression _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIoptions _lhsIruleDescr )  =
    (let ( !_lhsOerrors,!_lhsOoutput) =
             (sem _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIoptions _lhsIruleDescr )
     in  (Syn_Expression _lhsOerrors _lhsOoutput ))
sem_Expression_Expression :: Pos ->
                             ([HsToken]) ->
                             T_Expression 
sem_Expression_Expression !pos_ !tks_  =
    (T_Expression (\ (!_lhsIchildInhs)
                     (!_lhsIchildSyns)
                     (!_lhsIcon)
                     (!_lhsInt)
                     (!_lhsIoptions)
                     (!_lhsIruleDescr) ->
                       (case (let inh = Inh_HsTokensRoot { childInhs_Inh_HsTokensRoot     = _lhsIchildInhs
                                                         , childSyns_Inh_HsTokensRoot     = _lhsIchildSyns
                                                         , nt_Inh_HsTokensRoot            = _lhsInt
                                                         , con_Inh_HsTokensRoot           = _lhsIcon
                                                         , ruleDescr_Inh_HsTokensRoot     = _lhsIruleDescr
                                                         , useFieldIdent_Inh_HsTokensRoot = genUseTraces _lhsIoptions
                                                         }
                                  sem = sem_HsTokensRoot (HsTokensRoot tks_)
                                  syn = wrap_HsTokensRoot sem inh
                              in (tks_Syn_HsTokensRoot syn, errors_Syn_HsTokensRoot syn)) of
                        { !__tup1 ->
                        (case (__tup1) of
                         { !(_,!_lhsOerrors) ->
                         (case (__tup1) of
                          { !(!_tks',_) ->
                          (case (Expression pos_ _tks') of
                           { !_lhsOoutput ->
                           ( _lhsOerrors,_lhsOoutput) }) }) }) })) )
-- Grammar -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         forcedIrrefutables   : AttrMap
         options              : Options
      synthesized attributes:
         allAttributes        : AttrMap
         errors               : Seq Error
         output               : SELF 
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
         visit 0:
            local output      : _
-}
-- cata
sem_Grammar :: Grammar  ->
               T_Grammar 
sem_Grammar !(Grammar _typeSyns _useMap _derivings _wrappers _nonts _pragmas _manualAttrOrderMap _paramMap _contextMap _uniqueMap )  =
    (sem_Grammar_Grammar _typeSyns _useMap _derivings _wrappers (sem_Nonterminals _nonts ) _pragmas _manualAttrOrderMap _paramMap _contextMap _uniqueMap )
-- semantic domain
newtype T_Grammar  = T_Grammar (AttrMap ->
                                Options ->
                                ( AttrMap,(Seq Error),Grammar))
data Inh_Grammar  = Inh_Grammar {forcedIrrefutables_Inh_Grammar :: !(AttrMap),options_Inh_Grammar :: !(Options)}
data Syn_Grammar  = Syn_Grammar {allAttributes_Syn_Grammar :: !(AttrMap),errors_Syn_Grammar :: !(Seq Error),output_Syn_Grammar :: !(Grammar)}
wrap_Grammar :: T_Grammar  ->
                Inh_Grammar  ->
                Syn_Grammar 
wrap_Grammar !(T_Grammar sem ) !(Inh_Grammar _lhsIforcedIrrefutables _lhsIoptions )  =
    (let ( !_lhsOallAttributes,!_lhsOerrors,!_lhsOoutput) =
             (sem _lhsIforcedIrrefutables _lhsIoptions )
     in  (Syn_Grammar _lhsOallAttributes _lhsOerrors _lhsOoutput ))
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
sem_Grammar_Grammar !typeSyns_ !useMap_ !derivings_ !wrappers_ !(T_Nonterminals nonts_ ) !pragmas_ !manualAttrOrderMap_ !paramMap_ !contextMap_ !uniqueMap_  =
    (T_Grammar (\ (!_lhsIforcedIrrefutables)
                  (!_lhsIoptions) ->
                    (case (_lhsIoptions) of
                     { !_nontsOoptions ->
                     (case (_lhsIforcedIrrefutables) of
                      { !_nontsOforcedIrrefutables ->
                      (case ((nonts_ _nontsOforcedIrrefutables _nontsOoptions )) of
                       { ( !_nontsIallAttributes,!_nontsIerrors,!_nontsIoutput) ->
                       (case (_nontsIallAttributes) of
                        { !_lhsOallAttributes ->
                        (case (_nontsIerrors) of
                         { !_lhsOerrors ->
                         (case (Grammar typeSyns_ useMap_ derivings_ wrappers_ _nontsIoutput pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ uniqueMap_) of
                          { !_output ->
                          (case (_output) of
                           { !_lhsOoutput ->
                           ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) })) )
-- HsToken -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         con                  : ConstructorIdent
         nt                   : NontermIdent
         ruleDescr            : String
         useFieldIdent        : Bool
      chained attribute:
         addLines             : Int
      synthesized attributes:
         errors               : Seq Error
         tks                  : SELF 
   alternatives:
      alternative AGField:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
         visit 0:
            local mField      : _
            local field'      : _
            local tks         : _
      alternative AGLocal:
         child var            : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
         visit 0:
            local tks         : _
      alternative CharToken:
         child value          : {String}
         child pos            : {Pos}
         visit 0:
            local tks         : _
      alternative Err:
         child mesg           : {String}
         child pos            : {Pos}
         visit 0:
            local tks         : _
      alternative HsToken:
         child value          : {String}
         child pos            : {Pos}
         visit 0:
            local tks         : _
      alternative StrToken:
         child value          : {String}
         child pos            : {Pos}
         visit 0:
            local tks         : _
-}
-- cata
sem_HsToken :: HsToken  ->
               T_HsToken 
sem_HsToken !(AGField _field _attr _pos _rdesc )  =
    (sem_HsToken_AGField _field _attr _pos _rdesc )
sem_HsToken !(AGLocal _var _pos _rdesc )  =
    (sem_HsToken_AGLocal _var _pos _rdesc )
sem_HsToken !(CharToken _value _pos )  =
    (sem_HsToken_CharToken _value _pos )
sem_HsToken !(Err _mesg _pos )  =
    (sem_HsToken_Err _mesg _pos )
sem_HsToken !(HsToken _value _pos )  =
    (sem_HsToken_HsToken _value _pos )
sem_HsToken !(StrToken _value _pos )  =
    (sem_HsToken_StrToken _value _pos )
-- semantic domain
newtype T_HsToken  = T_HsToken (Int ->
                                ([(Identifier, Identifier)]) ->
                                ([(Identifier, Identifier)]) ->
                                ConstructorIdent ->
                                NontermIdent ->
                                String ->
                                Bool ->
                                ( Int,(Seq Error),HsToken))
data Inh_HsToken  = Inh_HsToken {addLines_Inh_HsToken :: !(Int),childInhs_Inh_HsToken :: !([(Identifier, Identifier)]),childSyns_Inh_HsToken :: !([(Identifier, Identifier)]),con_Inh_HsToken :: !(ConstructorIdent),nt_Inh_HsToken :: !(NontermIdent),ruleDescr_Inh_HsToken :: !(String),useFieldIdent_Inh_HsToken :: !(Bool)}
data Syn_HsToken  = Syn_HsToken {addLines_Syn_HsToken :: !(Int),errors_Syn_HsToken :: !(Seq Error),tks_Syn_HsToken :: !(HsToken)}
wrap_HsToken :: T_HsToken  ->
                Inh_HsToken  ->
                Syn_HsToken 
wrap_HsToken !(T_HsToken sem ) !(Inh_HsToken _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent )  =
    (let ( !_lhsOaddLines,!_lhsOerrors,!_lhsOtks) =
             (sem _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent )
     in  (Syn_HsToken _lhsOaddLines _lhsOerrors _lhsOtks ))
sem_HsToken_AGField :: Identifier ->
                       Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken 
sem_HsToken_AGField !field_ !attr_ !pos_ !rdesc_  =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (findField field_ attr_ _lhsIchildSyns) of
                     { !_mField ->
                     (case (maybe field_ id _mField) of
                      { !_field' ->
                      (case (if _lhsIuseFieldIdent || length (getName field_) < length (getName _field'    )
                             then _lhsIaddLines + 1
                             else _lhsIaddLines) of
                       { !_lhsOaddLines ->
                       (case (maybe (Seq.singleton (UndefAttr _lhsInt _lhsIcon field_ (Ident "<ANY>" (getPos field_)) False)) (const Seq.empty) _mField) of
                        { !_lhsOerrors ->
                        (case (AGField _field'     attr_ (addl _lhsIaddLines pos_) (if _lhsIuseFieldIdent then Just _lhsIruleDescr else Nothing)) of
                         { !_tks ->
                         (case (_tks) of
                          { !_lhsOtks ->
                          ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) }) }) })) )
sem_HsToken_AGLocal :: Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken 
sem_HsToken_AGLocal !var_ !pos_ !rdesc_  =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (if _lhsIuseFieldIdent
                           then _lhsIaddLines + 1
                           else _lhsIaddLines) of
                     { !_lhsOaddLines ->
                     (case (Seq.empty) of
                      { !_lhsOerrors ->
                      (case (AGLocal var_ (addl _lhsIaddLines pos_) (if _lhsIuseFieldIdent then Just _lhsIruleDescr else Nothing)) of
                       { !_tks ->
                       (case (_tks) of
                        { !_lhsOtks ->
                        ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) })) )
sem_HsToken_CharToken :: String ->
                         Pos ->
                         T_HsToken 
sem_HsToken_CharToken !value_ !pos_  =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (_lhsIaddLines) of
                     { !_lhsOaddLines ->
                     (case (Seq.empty) of
                      { !_lhsOerrors ->
                      (case (CharToken value_ (addl _lhsIaddLines pos_)) of
                       { !_tks ->
                       (case (_tks) of
                        { !_lhsOtks ->
                        ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) })) )
sem_HsToken_Err :: String ->
                   Pos ->
                   T_HsToken 
sem_HsToken_Err !mesg_ !pos_  =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (_lhsIaddLines) of
                     { !_lhsOaddLines ->
                     (case (Seq.empty) of
                      { !_lhsOerrors ->
                      (case (Err mesg_ (addl _lhsIaddLines pos_)) of
                       { !_tks ->
                       (case (_tks) of
                        { !_lhsOtks ->
                        ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) })) )
sem_HsToken_HsToken :: String ->
                       Pos ->
                       T_HsToken 
sem_HsToken_HsToken !value_ !pos_  =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (_lhsIaddLines) of
                     { !_lhsOaddLines ->
                     (case (Seq.empty) of
                      { !_lhsOerrors ->
                      (case (HsToken value_ (addl _lhsIaddLines pos_)) of
                       { !_tks ->
                       (case (_tks) of
                        { !_lhsOtks ->
                        ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) })) )
sem_HsToken_StrToken :: String ->
                        Pos ->
                        T_HsToken 
sem_HsToken_StrToken !value_ !pos_  =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (_lhsIaddLines) of
                     { !_lhsOaddLines ->
                     (case (Seq.empty) of
                      { !_lhsOerrors ->
                      (case (StrToken value_ (addl _lhsIaddLines pos_)) of
                       { !_tks ->
                       (case (_tks) of
                        { !_lhsOtks ->
                        ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) })) )
-- HsTokens ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         con                  : ConstructorIdent
         nt                   : NontermIdent
         ruleDescr            : String
         useFieldIdent        : Bool
      chained attribute:
         addLines             : Int
      synthesized attributes:
         errors               : Seq Error
         tks                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : HsToken 
         child tl             : HsTokens 
         visit 0:
            local tks         : _
      alternative Nil:
         visit 0:
            local tks         : _
-}
-- cata
sem_HsTokens :: HsTokens  ->
                T_HsTokens 
sem_HsTokens !list  =
    (Prelude.foldr sem_HsTokens_Cons sem_HsTokens_Nil (Prelude.map sem_HsToken list) )
-- semantic domain
newtype T_HsTokens  = T_HsTokens (Int ->
                                  ([(Identifier, Identifier)]) ->
                                  ([(Identifier, Identifier)]) ->
                                  ConstructorIdent ->
                                  NontermIdent ->
                                  String ->
                                  Bool ->
                                  ( Int,(Seq Error),HsTokens))
data Inh_HsTokens  = Inh_HsTokens {addLines_Inh_HsTokens :: !(Int),childInhs_Inh_HsTokens :: !([(Identifier, Identifier)]),childSyns_Inh_HsTokens :: !([(Identifier, Identifier)]),con_Inh_HsTokens :: !(ConstructorIdent),nt_Inh_HsTokens :: !(NontermIdent),ruleDescr_Inh_HsTokens :: !(String),useFieldIdent_Inh_HsTokens :: !(Bool)}
data Syn_HsTokens  = Syn_HsTokens {addLines_Syn_HsTokens :: !(Int),errors_Syn_HsTokens :: !(Seq Error),tks_Syn_HsTokens :: !(HsTokens)}
wrap_HsTokens :: T_HsTokens  ->
                 Inh_HsTokens  ->
                 Syn_HsTokens 
wrap_HsTokens !(T_HsTokens sem ) !(Inh_HsTokens _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent )  =
    (let ( !_lhsOaddLines,!_lhsOerrors,!_lhsOtks) =
             (sem _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent )
     in  (Syn_HsTokens _lhsOaddLines _lhsOerrors _lhsOtks ))
sem_HsTokens_Cons :: T_HsToken  ->
                     T_HsTokens  ->
                     T_HsTokens 
sem_HsTokens_Cons !(T_HsToken hd_ ) !(T_HsTokens tl_ )  =
    (T_HsTokens (\ (!_lhsIaddLines)
                   (!_lhsIchildInhs)
                   (!_lhsIchildSyns)
                   (!_lhsIcon)
                   (!_lhsInt)
                   (!_lhsIruleDescr)
                   (!_lhsIuseFieldIdent) ->
                     (case (_lhsIuseFieldIdent) of
                      { !_tlOuseFieldIdent ->
                      (case (_lhsIchildSyns) of
                       { !_tlOchildSyns ->
                       (case (_lhsIuseFieldIdent) of
                        { !_hdOuseFieldIdent ->
                        (case (_lhsIchildSyns) of
                         { !_hdOchildSyns ->
                         (case (_lhsIaddLines) of
                          { !_hdOaddLines ->
                          (case (_lhsIruleDescr) of
                           { !_hdOruleDescr ->
                           (case (_lhsInt) of
                            { !_hdOnt ->
                            (case (_lhsIcon) of
                             { !_hdOcon ->
                             (case (_lhsIchildInhs) of
                              { !_hdOchildInhs ->
                              (case ((hd_ _hdOaddLines _hdOchildInhs _hdOchildSyns _hdOcon _hdOnt _hdOruleDescr _hdOuseFieldIdent )) of
                               { ( !_hdIaddLines,!_hdIerrors,!_hdItks) ->
                               (case (_hdIaddLines) of
                                { !_tlOaddLines ->
                                (case (_lhsIruleDescr) of
                                 { !_tlOruleDescr ->
                                 (case (_lhsInt) of
                                  { !_tlOnt ->
                                  (case (_lhsIcon) of
                                   { !_tlOcon ->
                                   (case (_lhsIchildInhs) of
                                    { !_tlOchildInhs ->
                                    (case ((tl_ _tlOaddLines _tlOchildInhs _tlOchildSyns _tlOcon _tlOnt _tlOruleDescr _tlOuseFieldIdent )) of
                                     { ( !_tlIaddLines,!_tlIerrors,!_tlItks) ->
                                     (case (_tlIaddLines) of
                                      { !_lhsOaddLines ->
                                      (case (_hdIerrors Seq.>< _tlIerrors) of
                                       { !_lhsOerrors ->
                                       (case ((:) _hdItks _tlItks) of
                                        { !_tks ->
                                        (case (_tks) of
                                         { !_lhsOtks ->
                                         ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_HsTokens_Nil :: T_HsTokens 
sem_HsTokens_Nil  =
    (T_HsTokens (\ (!_lhsIaddLines)
                   (!_lhsIchildInhs)
                   (!_lhsIchildSyns)
                   (!_lhsIcon)
                   (!_lhsInt)
                   (!_lhsIruleDescr)
                   (!_lhsIuseFieldIdent) ->
                     (case (_lhsIaddLines) of
                      { !_lhsOaddLines ->
                      (case (Seq.empty) of
                       { !_lhsOerrors ->
                       (case ([]) of
                        { !_tks ->
                        (case (_tks) of
                         { !_lhsOtks ->
                         ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) })) )
-- HsTokensRoot ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         con                  : ConstructorIdent
         nt                   : NontermIdent
         ruleDescr            : String
         useFieldIdent        : Bool
      synthesized attributes:
         errors               : Seq Error
         tks                  : [HsToken]
   alternatives:
      alternative HsTokensRoot:
         child tokens         : HsTokens 
-}
-- cata
sem_HsTokensRoot :: HsTokensRoot  ->
                    T_HsTokensRoot 
sem_HsTokensRoot !(HsTokensRoot _tokens )  =
    (sem_HsTokensRoot_HsTokensRoot (sem_HsTokens _tokens ) )
-- semantic domain
newtype T_HsTokensRoot  = T_HsTokensRoot (([(Identifier, Identifier)]) ->
                                          ([(Identifier, Identifier)]) ->
                                          ConstructorIdent ->
                                          NontermIdent ->
                                          String ->
                                          Bool ->
                                          ( (Seq Error),([HsToken])))
data Inh_HsTokensRoot  = Inh_HsTokensRoot {childInhs_Inh_HsTokensRoot :: !([(Identifier, Identifier)]),childSyns_Inh_HsTokensRoot :: !([(Identifier, Identifier)]),con_Inh_HsTokensRoot :: !(ConstructorIdent),nt_Inh_HsTokensRoot :: !(NontermIdent),ruleDescr_Inh_HsTokensRoot :: !(String),useFieldIdent_Inh_HsTokensRoot :: !(Bool)}
data Syn_HsTokensRoot  = Syn_HsTokensRoot {errors_Syn_HsTokensRoot :: !(Seq Error),tks_Syn_HsTokensRoot :: !([HsToken])}
wrap_HsTokensRoot :: T_HsTokensRoot  ->
                     Inh_HsTokensRoot  ->
                     Syn_HsTokensRoot 
wrap_HsTokensRoot !(T_HsTokensRoot sem ) !(Inh_HsTokensRoot _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent )  =
    (let ( !_lhsOerrors,!_lhsOtks) =
             (sem _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent )
     in  (Syn_HsTokensRoot _lhsOerrors _lhsOtks ))
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens  ->
                                 T_HsTokensRoot 
sem_HsTokensRoot_HsTokensRoot !(T_HsTokens tokens_ )  =
    (T_HsTokensRoot (\ (!_lhsIchildInhs)
                       (!_lhsIchildSyns)
                       (!_lhsIcon)
                       (!_lhsInt)
                       (!_lhsIruleDescr)
                       (!_lhsIuseFieldIdent) ->
                         (case (_lhsInt) of
                          { !_tokensOnt ->
                          (case (_lhsIcon) of
                           { !_tokensOcon ->
                           (case (_lhsIchildSyns) of
                            { !_tokensOchildSyns ->
                            (case (_lhsIuseFieldIdent) of
                             { !_tokensOuseFieldIdent ->
                             (case (_lhsIruleDescr) of
                              { !_tokensOruleDescr ->
                              (case (_lhsIchildInhs) of
                               { !_tokensOchildInhs ->
                               (case (0) of
                                { !_tokensOaddLines ->
                                (case ((tokens_ _tokensOaddLines _tokensOchildInhs _tokensOchildSyns _tokensOcon _tokensOnt _tokensOruleDescr _tokensOuseFieldIdent )) of
                                 { ( !_tokensIaddLines,!_tokensIerrors,!_tokensItks) ->
                                 (case (_tokensIerrors) of
                                  { !_lhsOerrors ->
                                  (case (_tokensItks) of
                                   { !_lhsOtks ->
                                   ( _lhsOerrors,_lhsOtks) }) }) }) }) }) }) }) }) }) })) )
-- Nonterminal -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         forcedIrrefutables   : AttrMap
         options              : Options
      synthesized attributes:
         allAttributes        : AttrMap
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Nonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : Productions 
         visit 0:
            local output      : _
-}
-- cata
sem_Nonterminal :: Nonterminal  ->
                   T_Nonterminal 
sem_Nonterminal !(Nonterminal _nt _params _inh _syn _prods )  =
    (sem_Nonterminal_Nonterminal _nt _params _inh _syn (sem_Productions _prods ) )
-- semantic domain
newtype T_Nonterminal  = T_Nonterminal (AttrMap ->
                                        Options ->
                                        ( AttrMap,(Seq Error),Nonterminal))
data Inh_Nonterminal  = Inh_Nonterminal {forcedIrrefutables_Inh_Nonterminal :: !(AttrMap),options_Inh_Nonterminal :: !(Options)}
data Syn_Nonterminal  = Syn_Nonterminal {allAttributes_Syn_Nonterminal :: !(AttrMap),errors_Syn_Nonterminal :: !(Seq Error),output_Syn_Nonterminal :: !(Nonterminal)}
wrap_Nonterminal :: T_Nonterminal  ->
                    Inh_Nonterminal  ->
                    Syn_Nonterminal 
wrap_Nonterminal !(T_Nonterminal sem ) !(Inh_Nonterminal _lhsIforcedIrrefutables _lhsIoptions )  =
    (let ( !_lhsOallAttributes,!_lhsOerrors,!_lhsOoutput) =
             (sem _lhsIforcedIrrefutables _lhsIoptions )
     in  (Syn_Nonterminal _lhsOallAttributes _lhsOerrors _lhsOoutput ))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions  ->
                               T_Nonterminal 
sem_Nonterminal_Nonterminal !nt_ !params_ !inh_ !syn_ !(T_Productions prods_ )  =
    (T_Nonterminal (\ (!_lhsIforcedIrrefutables)
                      (!_lhsIoptions) ->
                        (case (nt_) of
                         { !_prodsOnt ->
                         (case (_lhsIoptions) of
                          { !_prodsOoptions ->
                          (case (_lhsIforcedIrrefutables) of
                           { !_prodsOforcedIrrefutables ->
                           (case ((prods_ _prodsOforcedIrrefutables _prodsOnt _prodsOoptions )) of
                            { ( !_prodsIallAttributes,!_prodsIerrors,!_prodsIoutput) ->
                            (case (_prodsIallAttributes) of
                             { !_lhsOallAttributes ->
                             (case (_prodsIerrors) of
                              { !_lhsOerrors ->
                              (case (Nonterminal nt_ params_ inh_ syn_ _prodsIoutput) of
                               { !_output ->
                               (case (_output) of
                                { !_lhsOoutput ->
                                ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) })) )
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         forcedIrrefutables   : AttrMap
         options              : Options
      synthesized attributes:
         allAttributes        : AttrMap
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Cons:
         child hd             : Nonterminal 
         child tl             : Nonterminals 
         visit 0:
            local output      : _
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_Nonterminals :: Nonterminals  ->
                    T_Nonterminals 
sem_Nonterminals !list  =
    (Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list) )
-- semantic domain
newtype T_Nonterminals  = T_Nonterminals (AttrMap ->
                                          Options ->
                                          ( AttrMap,(Seq Error),Nonterminals))
data Inh_Nonterminals  = Inh_Nonterminals {forcedIrrefutables_Inh_Nonterminals :: !(AttrMap),options_Inh_Nonterminals :: !(Options)}
data Syn_Nonterminals  = Syn_Nonterminals {allAttributes_Syn_Nonterminals :: !(AttrMap),errors_Syn_Nonterminals :: !(Seq Error),output_Syn_Nonterminals :: !(Nonterminals)}
wrap_Nonterminals :: T_Nonterminals  ->
                     Inh_Nonterminals  ->
                     Syn_Nonterminals 
wrap_Nonterminals !(T_Nonterminals sem ) !(Inh_Nonterminals _lhsIforcedIrrefutables _lhsIoptions )  =
    (let ( !_lhsOallAttributes,!_lhsOerrors,!_lhsOoutput) =
             (sem _lhsIforcedIrrefutables _lhsIoptions )
     in  (Syn_Nonterminals _lhsOallAttributes _lhsOerrors _lhsOoutput ))
sem_Nonterminals_Cons :: T_Nonterminal  ->
                         T_Nonterminals  ->
                         T_Nonterminals 
sem_Nonterminals_Cons !(T_Nonterminal hd_ ) !(T_Nonterminals tl_ )  =
    (T_Nonterminals (\ (!_lhsIforcedIrrefutables)
                       (!_lhsIoptions) ->
                         (case (_lhsIoptions) of
                          { !_tlOoptions ->
                          (case (_lhsIforcedIrrefutables) of
                           { !_tlOforcedIrrefutables ->
                           (case ((tl_ _tlOforcedIrrefutables _tlOoptions )) of
                            { ( !_tlIallAttributes,!_tlIerrors,!_tlIoutput) ->
                            (case (_lhsIoptions) of
                             { !_hdOoptions ->
                             (case (_lhsIforcedIrrefutables) of
                              { !_hdOforcedIrrefutables ->
                              (case ((hd_ _hdOforcedIrrefutables _hdOoptions )) of
                               { ( !_hdIallAttributes,!_hdIerrors,!_hdIoutput) ->
                               (case (_hdIallAttributes `mergeAttributes` _tlIallAttributes) of
                                { !_lhsOallAttributes ->
                                (case (_hdIerrors Seq.>< _tlIerrors) of
                                 { !_lhsOerrors ->
                                 (case ((:) _hdIoutput _tlIoutput) of
                                  { !_output ->
                                  (case (_output) of
                                   { !_lhsOoutput ->
                                   ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) })) )
sem_Nonterminals_Nil :: T_Nonterminals 
sem_Nonterminals_Nil  =
    (T_Nonterminals (\ (!_lhsIforcedIrrefutables)
                       (!_lhsIoptions) ->
                         (case (Map.empty) of
                          { !_lhsOallAttributes ->
                          (case (Seq.empty) of
                           { !_lhsOerrors ->
                           (case ([]) of
                            { !_output ->
                            (case (_output) of
                             { !_lhsOoutput ->
                             ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) })) )
-- Pattern -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         defsCollect          : Set (Identifier, Identifier)
   visit 1:
      inherited attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         con                  : ConstructorIdent
         defs                 : Set (Identifier, Identifier)
         forcedIrrefutables   : AttrMap
         nt                   : NontermIdent
      synthesized attributes:
         allAttributes        : AttrMap
         copy                 : SELF 
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         child parts          : Patterns 
         visit 0:
            local def         : _
         visit 1:
            local copy        : _
            local _tup2       : _
            local field'      : _
            local err2        : _
            local err1        : _
            local output      : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 1:
            local copy        : _
            local output      : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 1:
            local copy        : _
            local output      : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 1:
            local copy        : _
            local output      : _
      alternative Underscore:
         child pos            : {Pos}
         visit 1:
            local copy        : _
            local output      : _
-}
-- cata
sem_Pattern :: Pattern  ->
               T_Pattern 
sem_Pattern !(Alias _field _attr _pat _parts )  =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat ) (sem_Patterns _parts ) )
sem_Pattern !(Constr _name _pats )  =
    (sem_Pattern_Constr _name (sem_Patterns _pats ) )
sem_Pattern !(Irrefutable _pat )  =
    (sem_Pattern_Irrefutable (sem_Pattern _pat ) )
sem_Pattern !(Product _pos _pats )  =
    (sem_Pattern_Product _pos (sem_Patterns _pats ) )
sem_Pattern !(Underscore _pos )  =
    (sem_Pattern_Underscore _pos )
-- semantic domain
newtype T_Pattern  = T_Pattern (( (Set (Identifier, Identifier)),T_Pattern_1 ))
newtype T_Pattern_1  = T_Pattern_1 (([(Identifier, Identifier)]) ->
                                    ([(Identifier, Identifier)]) ->
                                    ConstructorIdent ->
                                    (Set (Identifier, Identifier)) ->
                                    AttrMap ->
                                    NontermIdent ->
                                    ( AttrMap,Pattern,(Seq Error),Pattern))
data Inh_Pattern  = Inh_Pattern {childInhs_Inh_Pattern :: !([(Identifier, Identifier)]),childSyns_Inh_Pattern :: !([(Identifier, Identifier)]),con_Inh_Pattern :: !(ConstructorIdent),defs_Inh_Pattern :: !(Set (Identifier, Identifier)),forcedIrrefutables_Inh_Pattern :: !(AttrMap),nt_Inh_Pattern :: !(NontermIdent)}
data Syn_Pattern  = Syn_Pattern {allAttributes_Syn_Pattern :: !(AttrMap),copy_Syn_Pattern :: !(Pattern),defsCollect_Syn_Pattern :: !(Set (Identifier, Identifier)),errors_Syn_Pattern :: !(Seq Error),output_Syn_Pattern :: !(Pattern)}
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern !(T_Pattern sem ) !(Inh_Pattern _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt )  =
    (let ( !_lhsOdefsCollect,!T_Pattern_1 sem_1) =
             (sem )
         ( !_lhsOallAttributes,!_lhsOcopy,!_lhsOerrors,!_lhsOoutput) =
             (sem_1 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt )
     in  (Syn_Pattern _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Alias !field_ !attr_ !(T_Pattern pat_ ) !(T_Patterns parts_ )  =
    (T_Pattern (case (Set.singleton (field_, attr_)) of
                { !_def ->
                (case ((parts_ )) of
                 { ( !_partsIdefsCollect,!T_Patterns_1 parts_1) ->
                 (case ((pat_ )) of
                  { ( !_patIdefsCollect,!T_Pattern_1 pat_1) ->
                  (case (_def     `Set.union` _patIdefsCollect `Set.union` _partsIdefsCollect) of
                   { !_lhsOdefsCollect ->
                   (case ((sem_Pattern_Alias_1 (T_Pattern_1 pat_1 ) attr_ field_ (T_Patterns_1 parts_1 ) )) of
                    { ( !sem_Pattern_1) ->
                    ( _lhsOdefsCollect,sem_Pattern_1) }) }) }) }) }) )
sem_Pattern_Alias_1 :: T_Pattern_1  ->
                       Identifier ->
                       Identifier ->
                       T_Patterns_1  ->
                       T_Pattern_1 
sem_Pattern_Alias_1 !(T_Pattern_1 pat_1 ) !attr_ !field_ !(T_Patterns_1 parts_1 )  =
    (T_Pattern_1 (\ (!_lhsIchildInhs)
                    (!_lhsIchildSyns)
                    (!_lhsIcon)
                    (!_lhsIdefs)
                    (!_lhsIforcedIrrefutables)
                    (!_lhsInt) ->
                      (case (_lhsInt) of
                       { !_patOnt ->
                       (case (_lhsIcon) of
                        { !_patOcon ->
                        (case (_lhsIforcedIrrefutables) of
                         { !_patOforcedIrrefutables ->
                         (case (_lhsIdefs) of
                          { !_patOdefs ->
                          (case (_lhsIchildSyns) of
                           { !_patOchildSyns ->
                           (case (_lhsIchildInhs) of
                            { !_patOchildInhs ->
                            (case ((pat_1 _patOchildInhs _patOchildSyns _patOcon _patOdefs _patOforcedIrrefutables _patOnt )) of
                             { ( !_patIallAttributes,!_patIcopy,!_patIerrors,!_patIoutput) ->
                             (case ((Map.singleton _lhsInt $ Map.singleton _lhsIcon $ Set.singleton (field_, attr_)) `mergeAttributes` _patIallAttributes) of
                              { !_lhsOallAttributes ->
                              (case (_lhsInt) of
                               { !_partsOnt ->
                               (case (_lhsIforcedIrrefutables) of
                                { !_partsOforcedIrrefutables ->
                                (case (_lhsIdefs) of
                                 { !_partsOdefs ->
                                 (case (_lhsIcon) of
                                  { !_partsOcon ->
                                  (case (_lhsIchildSyns) of
                                   { !_partsOchildSyns ->
                                   (case (_lhsIchildInhs) of
                                    { !_partsOchildInhs ->
                                    (case ((parts_1 _partsOchildInhs _partsOchildSyns _partsOcon _partsOdefs _partsOforcedIrrefutables _partsOnt )) of
                                     { ( !_partsIallAttributes,!_partsIcopy,!_partsIerrors,!_partsIoutput) ->
                                     (case (Alias field_ attr_ _patIcopy _partsIcopy) of
                                      { !_copy ->
                                      (case (_copy) of
                                       { !_lhsOcopy ->
                                       (case (maybeError field_ (UndefAttr _lhsInt _lhsIcon (Ident "<ANY>" (getPos field_)) attr_ True) $
                                                findField field_ attr_ _lhsIchildInhs) of
                                        { !__tup2 ->
                                        (case (__tup2) of
                                         { !(!_field',_) ->
                                         (case (if _field'     == field_
                                                then Seq.empty
                                                else if (_field'    , attr_) `Set.member` _lhsIdefs
                                                     then Seq.singleton $ DupRule _lhsInt _lhsIcon field_ attr_ _field'
                                                     else Seq.empty) of
                                          { !_err2 ->
                                          (case (__tup2) of
                                           { !(_,!_err1) ->
                                           (case (_err1     Seq.>< _err2     Seq.>< _patIerrors Seq.>< _partsIerrors) of
                                            { !_lhsOerrors ->
                                            (case (Alias _field'     attr_ _patIoutput _partsIoutput) of
                                             { !_output ->
                                             (case (if Set.member (field_, attr_) $ Map.findWithDefault Set.empty _lhsIcon $ Map.findWithDefault Map.empty _lhsInt $ _lhsIforcedIrrefutables
                                                    then Irrefutable _output
                                                    else _output) of
                                              { !_lhsOoutput ->
                                              ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr !name_ !(T_Patterns pats_ )  =
    (T_Pattern (case ((pats_ )) of
                { ( !_patsIdefsCollect,!T_Patterns_1 pats_1) ->
                (case (_patsIdefsCollect) of
                 { !_lhsOdefsCollect ->
                 (case ((sem_Pattern_Constr_1 (T_Patterns_1 pats_1 ) name_ )) of
                  { ( !sem_Pattern_1) ->
                  ( _lhsOdefsCollect,sem_Pattern_1) }) }) }) )
sem_Pattern_Constr_1 :: T_Patterns_1  ->
                        ConstructorIdent ->
                        T_Pattern_1 
sem_Pattern_Constr_1 !(T_Patterns_1 pats_1 ) !name_  =
    (T_Pattern_1 (\ (!_lhsIchildInhs)
                    (!_lhsIchildSyns)
                    (!_lhsIcon)
                    (!_lhsIdefs)
                    (!_lhsIforcedIrrefutables)
                    (!_lhsInt) ->
                      (case (_lhsInt) of
                       { !_patsOnt ->
                       (case (_lhsIcon) of
                        { !_patsOcon ->
                        (case (_lhsIforcedIrrefutables) of
                         { !_patsOforcedIrrefutables ->
                         (case (_lhsIdefs) of
                          { !_patsOdefs ->
                          (case (_lhsIchildSyns) of
                           { !_patsOchildSyns ->
                           (case (_lhsIchildInhs) of
                            { !_patsOchildInhs ->
                            (case ((pats_1 _patsOchildInhs _patsOchildSyns _patsOcon _patsOdefs _patsOforcedIrrefutables _patsOnt )) of
                             { ( !_patsIallAttributes,!_patsIcopy,!_patsIerrors,!_patsIoutput) ->
                             (case (_patsIallAttributes) of
                              { !_lhsOallAttributes ->
                              (case (Constr name_ _patsIcopy) of
                               { !_copy ->
                               (case (_copy) of
                                { !_lhsOcopy ->
                                (case (_patsIerrors) of
                                 { !_lhsOerrors ->
                                 (case (Constr name_ _patsIoutput) of
                                  { !_output ->
                                  (case (_output) of
                                   { !_lhsOoutput ->
                                   ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable !(T_Pattern pat_ )  =
    (T_Pattern (case ((pat_ )) of
                { ( !_patIdefsCollect,!T_Pattern_1 pat_1) ->
                (case (_patIdefsCollect) of
                 { !_lhsOdefsCollect ->
                 (case ((sem_Pattern_Irrefutable_1 (T_Pattern_1 pat_1 ) )) of
                  { ( !sem_Pattern_1) ->
                  ( _lhsOdefsCollect,sem_Pattern_1) }) }) }) )
sem_Pattern_Irrefutable_1 :: T_Pattern_1  ->
                             T_Pattern_1 
sem_Pattern_Irrefutable_1 !(T_Pattern_1 pat_1 )  =
    (T_Pattern_1 (\ (!_lhsIchildInhs)
                    (!_lhsIchildSyns)
                    (!_lhsIcon)
                    (!_lhsIdefs)
                    (!_lhsIforcedIrrefutables)
                    (!_lhsInt) ->
                      (case (Map.empty) of
                       { !_lhsOallAttributes ->
                       (case (_lhsInt) of
                        { !_patOnt ->
                        (case (_lhsIforcedIrrefutables) of
                         { !_patOforcedIrrefutables ->
                         (case (_lhsIdefs) of
                          { !_patOdefs ->
                          (case (_lhsIcon) of
                           { !_patOcon ->
                           (case (_lhsIchildSyns) of
                            { !_patOchildSyns ->
                            (case (_lhsIchildInhs) of
                             { !_patOchildInhs ->
                             (case ((pat_1 _patOchildInhs _patOchildSyns _patOcon _patOdefs _patOforcedIrrefutables _patOnt )) of
                              { ( !_patIallAttributes,!_patIcopy,!_patIerrors,!_patIoutput) ->
                              (case (Irrefutable _patIcopy) of
                               { !_copy ->
                               (case (_copy) of
                                { !_lhsOcopy ->
                                (case (_patIerrors) of
                                 { !_lhsOerrors ->
                                 (case (Irrefutable _patIoutput) of
                                  { !_output ->
                                  (case (_output) of
                                   { !_lhsOoutput ->
                                   ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product !pos_ !(T_Patterns pats_ )  =
    (T_Pattern (case ((pats_ )) of
                { ( !_patsIdefsCollect,!T_Patterns_1 pats_1) ->
                (case (_patsIdefsCollect) of
                 { !_lhsOdefsCollect ->
                 (case ((sem_Pattern_Product_1 (T_Patterns_1 pats_1 ) pos_ )) of
                  { ( !sem_Pattern_1) ->
                  ( _lhsOdefsCollect,sem_Pattern_1) }) }) }) )
sem_Pattern_Product_1 :: T_Patterns_1  ->
                         Pos ->
                         T_Pattern_1 
sem_Pattern_Product_1 !(T_Patterns_1 pats_1 ) !pos_  =
    (T_Pattern_1 (\ (!_lhsIchildInhs)
                    (!_lhsIchildSyns)
                    (!_lhsIcon)
                    (!_lhsIdefs)
                    (!_lhsIforcedIrrefutables)
                    (!_lhsInt) ->
                      (case (_lhsInt) of
                       { !_patsOnt ->
                       (case (_lhsIcon) of
                        { !_patsOcon ->
                        (case (_lhsIforcedIrrefutables) of
                         { !_patsOforcedIrrefutables ->
                         (case (_lhsIdefs) of
                          { !_patsOdefs ->
                          (case (_lhsIchildSyns) of
                           { !_patsOchildSyns ->
                           (case (_lhsIchildInhs) of
                            { !_patsOchildInhs ->
                            (case ((pats_1 _patsOchildInhs _patsOchildSyns _patsOcon _patsOdefs _patsOforcedIrrefutables _patsOnt )) of
                             { ( !_patsIallAttributes,!_patsIcopy,!_patsIerrors,!_patsIoutput) ->
                             (case (_patsIallAttributes) of
                              { !_lhsOallAttributes ->
                              (case (Product pos_ _patsIcopy) of
                               { !_copy ->
                               (case (_copy) of
                                { !_lhsOcopy ->
                                (case (_patsIerrors) of
                                 { !_lhsOerrors ->
                                 (case (Product pos_ _patsIoutput) of
                                  { !_output ->
                                  (case (_output) of
                                   { !_lhsOoutput ->
                                   ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore !pos_  =
    (T_Pattern (case (Set.empty) of
                { !_lhsOdefsCollect ->
                (case ((sem_Pattern_Underscore_1 pos_ )) of
                 { ( !sem_Pattern_1) ->
                 ( _lhsOdefsCollect,sem_Pattern_1) }) }) )
sem_Pattern_Underscore_1 :: Pos ->
                            T_Pattern_1 
sem_Pattern_Underscore_1 !pos_  =
    (T_Pattern_1 (\ (!_lhsIchildInhs)
                    (!_lhsIchildSyns)
                    (!_lhsIcon)
                    (!_lhsIdefs)
                    (!_lhsIforcedIrrefutables)
                    (!_lhsInt) ->
                      (case (Map.empty) of
                       { !_lhsOallAttributes ->
                       (case (Underscore pos_) of
                        { !_copy ->
                        (case (_copy) of
                         { !_lhsOcopy ->
                         (case (Seq.empty) of
                          { !_lhsOerrors ->
                          (case (Underscore pos_) of
                           { !_output ->
                           (case (_output) of
                            { !_lhsOoutput ->
                            ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) })) )
-- Patterns ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         defsCollect          : Set (Identifier, Identifier)
   visit 1:
      inherited attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         con                  : ConstructorIdent
         defs                 : Set (Identifier, Identifier)
         forcedIrrefutables   : AttrMap
         nt                   : NontermIdent
      synthesized attributes:
         allAttributes        : AttrMap
         copy                 : SELF 
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 1:
            local copy        : _
            local output      : _
      alternative Nil:
         visit 1:
            local copy        : _
            local output      : _
-}
-- cata
sem_Patterns :: Patterns  ->
                T_Patterns 
sem_Patterns !list  =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list) )
-- semantic domain
newtype T_Patterns  = T_Patterns (( (Set (Identifier, Identifier)),T_Patterns_1 ))
newtype T_Patterns_1  = T_Patterns_1 (([(Identifier, Identifier)]) ->
                                      ([(Identifier, Identifier)]) ->
                                      ConstructorIdent ->
                                      (Set (Identifier, Identifier)) ->
                                      AttrMap ->
                                      NontermIdent ->
                                      ( AttrMap,Patterns,(Seq Error),Patterns))
data Inh_Patterns  = Inh_Patterns {childInhs_Inh_Patterns :: !([(Identifier, Identifier)]),childSyns_Inh_Patterns :: !([(Identifier, Identifier)]),con_Inh_Patterns :: !(ConstructorIdent),defs_Inh_Patterns :: !(Set (Identifier, Identifier)),forcedIrrefutables_Inh_Patterns :: !(AttrMap),nt_Inh_Patterns :: !(NontermIdent)}
data Syn_Patterns  = Syn_Patterns {allAttributes_Syn_Patterns :: !(AttrMap),copy_Syn_Patterns :: !(Patterns),defsCollect_Syn_Patterns :: !(Set (Identifier, Identifier)),errors_Syn_Patterns :: !(Seq Error),output_Syn_Patterns :: !(Patterns)}
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns !(T_Patterns sem ) !(Inh_Patterns _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt )  =
    (let ( !_lhsOdefsCollect,!T_Patterns_1 sem_1) =
             (sem )
         ( !_lhsOallAttributes,!_lhsOcopy,!_lhsOerrors,!_lhsOoutput) =
             (sem_1 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt )
     in  (Syn_Patterns _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons !(T_Pattern hd_ ) !(T_Patterns tl_ )  =
    (T_Patterns (case ((tl_ )) of
                 { ( !_tlIdefsCollect,!T_Patterns_1 tl_1) ->
                 (case ((hd_ )) of
                  { ( !_hdIdefsCollect,!T_Pattern_1 hd_1) ->
                  (case (_hdIdefsCollect `Set.union` _tlIdefsCollect) of
                   { !_lhsOdefsCollect ->
                   (case ((sem_Patterns_Cons_1 (T_Patterns_1 tl_1 ) (T_Pattern_1 hd_1 ) )) of
                    { ( !sem_Patterns_1) ->
                    ( _lhsOdefsCollect,sem_Patterns_1) }) }) }) }) )
sem_Patterns_Cons_1 :: T_Patterns_1  ->
                       T_Pattern_1  ->
                       T_Patterns_1 
sem_Patterns_Cons_1 !(T_Patterns_1 tl_1 ) !(T_Pattern_1 hd_1 )  =
    (T_Patterns_1 (\ (!_lhsIchildInhs)
                     (!_lhsIchildSyns)
                     (!_lhsIcon)
                     (!_lhsIdefs)
                     (!_lhsIforcedIrrefutables)
                     (!_lhsInt) ->
                       (case (_lhsInt) of
                        { !_tlOnt ->
                        (case (_lhsIcon) of
                         { !_tlOcon ->
                         (case (_lhsInt) of
                          { !_hdOnt ->
                          (case (_lhsIcon) of
                           { !_hdOcon ->
                           (case (_lhsIforcedIrrefutables) of
                            { !_tlOforcedIrrefutables ->
                            (case (_lhsIdefs) of
                             { !_tlOdefs ->
                             (case (_lhsIchildSyns) of
                              { !_tlOchildSyns ->
                              (case (_lhsIchildInhs) of
                               { !_tlOchildInhs ->
                               (case ((tl_1 _tlOchildInhs _tlOchildSyns _tlOcon _tlOdefs _tlOforcedIrrefutables _tlOnt )) of
                                { ( !_tlIallAttributes,!_tlIcopy,!_tlIerrors,!_tlIoutput) ->
                                (case (_lhsIforcedIrrefutables) of
                                 { !_hdOforcedIrrefutables ->
                                 (case (_lhsIdefs) of
                                  { !_hdOdefs ->
                                  (case (_lhsIchildSyns) of
                                   { !_hdOchildSyns ->
                                   (case (_lhsIchildInhs) of
                                    { !_hdOchildInhs ->
                                    (case ((hd_1 _hdOchildInhs _hdOchildSyns _hdOcon _hdOdefs _hdOforcedIrrefutables _hdOnt )) of
                                     { ( !_hdIallAttributes,!_hdIcopy,!_hdIerrors,!_hdIoutput) ->
                                     (case (_hdIallAttributes `mergeAttributes` _tlIallAttributes) of
                                      { !_lhsOallAttributes ->
                                      (case ((:) _hdIcopy _tlIcopy) of
                                       { !_copy ->
                                       (case (_copy) of
                                        { !_lhsOcopy ->
                                        (case (_hdIerrors Seq.>< _tlIerrors) of
                                         { !_lhsOerrors ->
                                         (case ((:) _hdIoutput _tlIoutput) of
                                          { !_output ->
                                          (case (_output) of
                                           { !_lhsOoutput ->
                                           ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (case (Set.empty) of
                 { !_lhsOdefsCollect ->
                 (case ((sem_Patterns_Nil_1 )) of
                  { ( !sem_Patterns_1) ->
                  ( _lhsOdefsCollect,sem_Patterns_1) }) }) )
sem_Patterns_Nil_1 :: T_Patterns_1 
sem_Patterns_Nil_1  =
    (T_Patterns_1 (\ (!_lhsIchildInhs)
                     (!_lhsIchildSyns)
                     (!_lhsIcon)
                     (!_lhsIdefs)
                     (!_lhsIforcedIrrefutables)
                     (!_lhsInt) ->
                       (case (Map.empty) of
                        { !_lhsOallAttributes ->
                        (case ([]) of
                         { !_copy ->
                         (case (_copy) of
                          { !_lhsOcopy ->
                          (case (Seq.empty) of
                           { !_lhsOerrors ->
                           (case ([]) of
                            { !_output ->
                            (case (_output) of
                             { !_lhsOoutput ->
                             ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) })) )
-- Production --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         forcedIrrefutables   : AttrMap
         nt                   : NontermIdent
         options              : Options
      synthesized attributes:
         allAttributes        : AttrMap
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Production:
         child con            : {ConstructorIdent}
         child children       : Children 
         child rules          : Rules 
         child typeSigs       : TypeSigs 
         visit 0:
            local output      : _
-}
-- cata
sem_Production :: Production  ->
                  T_Production 
sem_Production !(Production _con _children _rules _typeSigs )  =
    (sem_Production_Production _con (sem_Children _children ) (sem_Rules _rules ) (sem_TypeSigs _typeSigs ) )
-- semantic domain
newtype T_Production  = T_Production (AttrMap ->
                                      NontermIdent ->
                                      Options ->
                                      ( AttrMap,(Seq Error),Production))
data Inh_Production  = Inh_Production {forcedIrrefutables_Inh_Production :: !(AttrMap),nt_Inh_Production :: !(NontermIdent),options_Inh_Production :: !(Options)}
data Syn_Production  = Syn_Production {allAttributes_Syn_Production :: !(AttrMap),errors_Syn_Production :: !(Seq Error),output_Syn_Production :: !(Production)}
wrap_Production :: T_Production  ->
                   Inh_Production  ->
                   Syn_Production 
wrap_Production !(T_Production sem ) !(Inh_Production _lhsIforcedIrrefutables _lhsInt _lhsIoptions )  =
    (let ( !_lhsOallAttributes,!_lhsOerrors,!_lhsOoutput) =
             (sem _lhsIforcedIrrefutables _lhsInt _lhsIoptions )
     in  (Syn_Production _lhsOallAttributes _lhsOerrors _lhsOoutput ))
sem_Production_Production :: ConstructorIdent ->
                             T_Children  ->
                             T_Rules  ->
                             T_TypeSigs  ->
                             T_Production 
sem_Production_Production !con_ !(T_Children children_ ) !(T_Rules rules_ ) !(T_TypeSigs typeSigs_ )  =
    (T_Production (\ (!_lhsIforcedIrrefutables)
                     (!_lhsInt)
                     (!_lhsIoptions) ->
                       (case (_lhsInt) of
                        { !_rulesOnt ->
                        (case (con_) of
                         { !_rulesOcon ->
                         (case ((rules_ )) of
                          { ( !_rulesIdefsCollect,!T_Rules_1 rules_1) ->
                          (case (_lhsIoptions) of
                           { !_rulesOoptions ->
                           (case (_lhsIforcedIrrefutables) of
                            { !_rulesOforcedIrrefutables ->
                            (case ((children_ )) of
                             { ( !_childrenIchildInhs,!_childrenIchildSyns,!_childrenIoutput) ->
                             (case (_childrenIchildSyns) of
                              { !_rulesOchildSyns ->
                              (case (_childrenIchildInhs) of
                               { !_rulesOchildInhs ->
                               (case (_rulesIdefsCollect) of
                                { !_rulesOdefs ->
                                (case ((rules_1 _rulesOchildInhs _rulesOchildSyns _rulesOcon _rulesOdefs _rulesOforcedIrrefutables _rulesOnt _rulesOoptions )) of
                                 { ( !_rulesIallAttributes,!_rulesIerrors,!_rulesIoutput) ->
                                 (case (_rulesIallAttributes) of
                                  { !_lhsOallAttributes ->
                                  (case (_rulesIerrors) of
                                   { !_lhsOerrors ->
                                   (case ((typeSigs_ )) of
                                    { ( !_typeSigsIoutput) ->
                                    (case (Production con_ _childrenIoutput _rulesIoutput _typeSigsIoutput) of
                                     { !_output ->
                                     (case (_output) of
                                      { !_lhsOoutput ->
                                      ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
-- Productions -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         forcedIrrefutables   : AttrMap
         nt                   : NontermIdent
         options              : Options
      synthesized attributes:
         allAttributes        : AttrMap
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Cons:
         child hd             : Production 
         child tl             : Productions 
         visit 0:
            local output      : _
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_Productions :: Productions  ->
                   T_Productions 
sem_Productions !list  =
    (Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list) )
-- semantic domain
newtype T_Productions  = T_Productions (AttrMap ->
                                        NontermIdent ->
                                        Options ->
                                        ( AttrMap,(Seq Error),Productions))
data Inh_Productions  = Inh_Productions {forcedIrrefutables_Inh_Productions :: !(AttrMap),nt_Inh_Productions :: !(NontermIdent),options_Inh_Productions :: !(Options)}
data Syn_Productions  = Syn_Productions {allAttributes_Syn_Productions :: !(AttrMap),errors_Syn_Productions :: !(Seq Error),output_Syn_Productions :: !(Productions)}
wrap_Productions :: T_Productions  ->
                    Inh_Productions  ->
                    Syn_Productions 
wrap_Productions !(T_Productions sem ) !(Inh_Productions _lhsIforcedIrrefutables _lhsInt _lhsIoptions )  =
    (let ( !_lhsOallAttributes,!_lhsOerrors,!_lhsOoutput) =
             (sem _lhsIforcedIrrefutables _lhsInt _lhsIoptions )
     in  (Syn_Productions _lhsOallAttributes _lhsOerrors _lhsOoutput ))
sem_Productions_Cons :: T_Production  ->
                        T_Productions  ->
                        T_Productions 
sem_Productions_Cons !(T_Production hd_ ) !(T_Productions tl_ )  =
    (T_Productions (\ (!_lhsIforcedIrrefutables)
                      (!_lhsInt)
                      (!_lhsIoptions) ->
                        (case (_lhsInt) of
                         { !_tlOnt ->
                         (case (_lhsInt) of
                          { !_hdOnt ->
                          (case (_lhsIoptions) of
                           { !_tlOoptions ->
                           (case (_lhsIforcedIrrefutables) of
                            { !_tlOforcedIrrefutables ->
                            (case ((tl_ _tlOforcedIrrefutables _tlOnt _tlOoptions )) of
                             { ( !_tlIallAttributes,!_tlIerrors,!_tlIoutput) ->
                             (case (_lhsIoptions) of
                              { !_hdOoptions ->
                              (case (_lhsIforcedIrrefutables) of
                               { !_hdOforcedIrrefutables ->
                               (case ((hd_ _hdOforcedIrrefutables _hdOnt _hdOoptions )) of
                                { ( !_hdIallAttributes,!_hdIerrors,!_hdIoutput) ->
                                (case (_hdIallAttributes `mergeAttributes` _tlIallAttributes) of
                                 { !_lhsOallAttributes ->
                                 (case (_hdIerrors Seq.>< _tlIerrors) of
                                  { !_lhsOerrors ->
                                  (case ((:) _hdIoutput _tlIoutput) of
                                   { !_output ->
                                   (case (_output) of
                                    { !_lhsOoutput ->
                                    ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Productions_Nil :: T_Productions 
sem_Productions_Nil  =
    (T_Productions (\ (!_lhsIforcedIrrefutables)
                      (!_lhsInt)
                      (!_lhsIoptions) ->
                        (case (Map.empty) of
                         { !_lhsOallAttributes ->
                         (case (Seq.empty) of
                          { !_lhsOerrors ->
                          (case ([]) of
                           { !_output ->
                           (case (_output) of
                            { !_lhsOoutput ->
                            ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) })) )
-- Rule --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         defsCollect          : Set (Identifier, Identifier)
   visit 1:
      inherited attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         con                  : ConstructorIdent
         defs                 : Set (Identifier, Identifier)
         forcedIrrefutables   : AttrMap
         nt                   : NontermIdent
         options              : Options
      synthesized attributes:
         allAttributes        : AttrMap
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Rule:
         child pattern        : Pattern 
         child rhs            : Expression 
         child owrt           : {Bool}
         child origin         : {String}
         visit 1:
            local ruleDescr   : _
            local output      : _
-}
-- cata
sem_Rule :: Rule  ->
            T_Rule 
sem_Rule !(Rule _pattern _rhs _owrt _origin )  =
    (sem_Rule_Rule (sem_Pattern _pattern ) (sem_Expression _rhs ) _owrt _origin )
-- semantic domain
newtype T_Rule  = T_Rule (( (Set (Identifier, Identifier)),T_Rule_1 ))
newtype T_Rule_1  = T_Rule_1 (([(Identifier, Identifier)]) ->
                              ([(Identifier, Identifier)]) ->
                              ConstructorIdent ->
                              (Set (Identifier, Identifier)) ->
                              AttrMap ->
                              NontermIdent ->
                              Options ->
                              ( AttrMap,(Seq Error),Rule))
data Inh_Rule  = Inh_Rule {childInhs_Inh_Rule :: !([(Identifier, Identifier)]),childSyns_Inh_Rule :: !([(Identifier, Identifier)]),con_Inh_Rule :: !(ConstructorIdent),defs_Inh_Rule :: !(Set (Identifier, Identifier)),forcedIrrefutables_Inh_Rule :: !(AttrMap),nt_Inh_Rule :: !(NontermIdent),options_Inh_Rule :: !(Options)}
data Syn_Rule  = Syn_Rule {allAttributes_Syn_Rule :: !(AttrMap),defsCollect_Syn_Rule :: !(Set (Identifier, Identifier)),errors_Syn_Rule :: !(Seq Error),output_Syn_Rule :: !(Rule)}
wrap_Rule :: T_Rule  ->
             Inh_Rule  ->
             Syn_Rule 
wrap_Rule !(T_Rule sem ) !(Inh_Rule _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions )  =
    (let ( !_lhsOdefsCollect,!T_Rule_1 sem_1) =
             (sem )
         ( !_lhsOallAttributes,!_lhsOerrors,!_lhsOoutput) =
             (sem_1 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions )
     in  (Syn_Rule _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput ))
sem_Rule_Rule :: T_Pattern  ->
                 T_Expression  ->
                 Bool ->
                 String ->
                 T_Rule 
sem_Rule_Rule !(T_Pattern pattern_ ) !(T_Expression rhs_ ) !owrt_ !origin_  =
    (T_Rule (case ((pattern_ )) of
             { ( !_patternIdefsCollect,!T_Pattern_1 pattern_1) ->
             (case (_patternIdefsCollect) of
              { !_lhsOdefsCollect ->
              (case ((sem_Rule_Rule_1 (T_Pattern_1 pattern_1 ) _patternIdefsCollect (T_Expression rhs_ ) origin_ owrt_ )) of
               { ( !sem_Rule_1) ->
               ( _lhsOdefsCollect,sem_Rule_1) }) }) }) )
sem_Rule_Rule_1 :: T_Pattern_1  ->
                   (Set (Identifier, Identifier)) ->
                   T_Expression  ->
                   String ->
                   Bool ->
                   T_Rule_1 
sem_Rule_Rule_1 !(T_Pattern_1 pattern_1 ) !_patternIdefsCollect !(T_Expression rhs_ ) !origin_ !owrt_  =
    (T_Rule_1 (\ (!_lhsIchildInhs)
                 (!_lhsIchildSyns)
                 (!_lhsIcon)
                 (!_lhsIdefs)
                 (!_lhsIforcedIrrefutables)
                 (!_lhsInt)
                 (!_lhsIoptions) ->
                   (case (_lhsInt) of
                    { !_patternOnt ->
                    (case (_lhsIcon) of
                     { !_patternOcon ->
                     (case (_lhsIforcedIrrefutables) of
                      { !_patternOforcedIrrefutables ->
                      (case (_lhsIdefs) of
                       { !_patternOdefs ->
                       (case (_lhsIchildSyns) of
                        { !_patternOchildSyns ->
                        (case (_lhsIchildInhs) of
                         { !_patternOchildInhs ->
                         (case ((pattern_1 _patternOchildInhs _patternOchildSyns _patternOcon _patternOdefs _patternOforcedIrrefutables _patternOnt )) of
                          { ( !_patternIallAttributes,!_patternIcopy,!_patternIerrors,!_patternIoutput) ->
                          (case (_patternIallAttributes) of
                           { !_lhsOallAttributes ->
                           (case (show _lhsInt ++ " :: " ++ show _lhsIcon ++ " :: " ++ (concat $ intersperse "," $ map (\(f,a) -> show f ++ "." ++ show a) $ Set.toList _patternIdefsCollect)) of
                            { !_ruleDescr ->
                            (case (_ruleDescr) of
                             { !_rhsOruleDescr ->
                             (case (_lhsIoptions) of
                              { !_rhsOoptions ->
                              (case (_lhsInt) of
                               { !_rhsOnt ->
                               (case (_lhsIcon) of
                                { !_rhsOcon ->
                                (case (_lhsIchildSyns) of
                                 { !_rhsOchildSyns ->
                                 (case (_lhsIchildInhs) of
                                  { !_rhsOchildInhs ->
                                  (case ((rhs_ _rhsOchildInhs _rhsOchildSyns _rhsOcon _rhsOnt _rhsOoptions _rhsOruleDescr )) of
                                   { ( !_rhsIerrors,!_rhsIoutput) ->
                                   (case (_patternIerrors Seq.>< _rhsIerrors) of
                                    { !_lhsOerrors ->
                                    (case (Rule _patternIoutput _rhsIoutput owrt_ origin_) of
                                     { !_output ->
                                     (case (_output) of
                                      { !_lhsOoutput ->
                                      ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
-- Rules -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         defsCollect          : Set (Identifier, Identifier)
   visit 1:
      inherited attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         con                  : ConstructorIdent
         defs                 : Set (Identifier, Identifier)
         forcedIrrefutables   : AttrMap
         nt                   : NontermIdent
         options              : Options
      synthesized attributes:
         allAttributes        : AttrMap
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Cons:
         child hd             : Rule 
         child tl             : Rules 
         visit 1:
            local output      : _
      alternative Nil:
         visit 1:
            local output      : _
-}
-- cata
sem_Rules :: Rules  ->
             T_Rules 
sem_Rules !list  =
    (Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list) )
-- semantic domain
newtype T_Rules  = T_Rules (( (Set (Identifier, Identifier)),T_Rules_1 ))
newtype T_Rules_1  = T_Rules_1 (([(Identifier, Identifier)]) ->
                                ([(Identifier, Identifier)]) ->
                                ConstructorIdent ->
                                (Set (Identifier, Identifier)) ->
                                AttrMap ->
                                NontermIdent ->
                                Options ->
                                ( AttrMap,(Seq Error),Rules))
data Inh_Rules  = Inh_Rules {childInhs_Inh_Rules :: !([(Identifier, Identifier)]),childSyns_Inh_Rules :: !([(Identifier, Identifier)]),con_Inh_Rules :: !(ConstructorIdent),defs_Inh_Rules :: !(Set (Identifier, Identifier)),forcedIrrefutables_Inh_Rules :: !(AttrMap),nt_Inh_Rules :: !(NontermIdent),options_Inh_Rules :: !(Options)}
data Syn_Rules  = Syn_Rules {allAttributes_Syn_Rules :: !(AttrMap),defsCollect_Syn_Rules :: !(Set (Identifier, Identifier)),errors_Syn_Rules :: !(Seq Error),output_Syn_Rules :: !(Rules)}
wrap_Rules :: T_Rules  ->
              Inh_Rules  ->
              Syn_Rules 
wrap_Rules !(T_Rules sem ) !(Inh_Rules _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions )  =
    (let ( !_lhsOdefsCollect,!T_Rules_1 sem_1) =
             (sem )
         ( !_lhsOallAttributes,!_lhsOerrors,!_lhsOoutput) =
             (sem_1 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions )
     in  (Syn_Rules _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput ))
sem_Rules_Cons :: T_Rule  ->
                  T_Rules  ->
                  T_Rules 
sem_Rules_Cons !(T_Rule hd_ ) !(T_Rules tl_ )  =
    (T_Rules (case ((tl_ )) of
              { ( !_tlIdefsCollect,!T_Rules_1 tl_1) ->
              (case ((hd_ )) of
               { ( !_hdIdefsCollect,!T_Rule_1 hd_1) ->
               (case (_hdIdefsCollect `Set.union` _tlIdefsCollect) of
                { !_lhsOdefsCollect ->
                (case ((sem_Rules_Cons_1 (T_Rules_1 tl_1 ) (T_Rule_1 hd_1 ) )) of
                 { ( !sem_Rules_1) ->
                 ( _lhsOdefsCollect,sem_Rules_1) }) }) }) }) )
sem_Rules_Cons_1 :: T_Rules_1  ->
                    T_Rule_1  ->
                    T_Rules_1 
sem_Rules_Cons_1 !(T_Rules_1 tl_1 ) !(T_Rule_1 hd_1 )  =
    (T_Rules_1 (\ (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsIdefs)
                  (!_lhsIforcedIrrefutables)
                  (!_lhsInt)
                  (!_lhsIoptions) ->
                    (case (_lhsInt) of
                     { !_tlOnt ->
                     (case (_lhsIcon) of
                      { !_tlOcon ->
                      (case (_lhsInt) of
                       { !_hdOnt ->
                       (case (_lhsIcon) of
                        { !_hdOcon ->
                        (case (_lhsIoptions) of
                         { !_tlOoptions ->
                         (case (_lhsIforcedIrrefutables) of
                          { !_tlOforcedIrrefutables ->
                          (case (_lhsIdefs) of
                           { !_tlOdefs ->
                           (case (_lhsIchildSyns) of
                            { !_tlOchildSyns ->
                            (case (_lhsIchildInhs) of
                             { !_tlOchildInhs ->
                             (case ((tl_1 _tlOchildInhs _tlOchildSyns _tlOcon _tlOdefs _tlOforcedIrrefutables _tlOnt _tlOoptions )) of
                              { ( !_tlIallAttributes,!_tlIerrors,!_tlIoutput) ->
                              (case (_lhsIoptions) of
                               { !_hdOoptions ->
                               (case (_lhsIforcedIrrefutables) of
                                { !_hdOforcedIrrefutables ->
                                (case (_lhsIdefs) of
                                 { !_hdOdefs ->
                                 (case (_lhsIchildSyns) of
                                  { !_hdOchildSyns ->
                                  (case (_lhsIchildInhs) of
                                   { !_hdOchildInhs ->
                                   (case ((hd_1 _hdOchildInhs _hdOchildSyns _hdOcon _hdOdefs _hdOforcedIrrefutables _hdOnt _hdOoptions )) of
                                    { ( !_hdIallAttributes,!_hdIerrors,!_hdIoutput) ->
                                    (case (_hdIallAttributes `mergeAttributes` _tlIallAttributes) of
                                     { !_lhsOallAttributes ->
                                     (case (_hdIerrors Seq.>< _tlIerrors) of
                                      { !_lhsOerrors ->
                                      (case ((:) _hdIoutput _tlIoutput) of
                                       { !_output ->
                                       (case (_output) of
                                        { !_lhsOoutput ->
                                        ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Rules_Nil :: T_Rules 
sem_Rules_Nil  =
    (T_Rules (case (Set.empty) of
              { !_lhsOdefsCollect ->
              (case ((sem_Rules_Nil_1 )) of
               { ( !sem_Rules_1) ->
               ( _lhsOdefsCollect,sem_Rules_1) }) }) )
sem_Rules_Nil_1 :: T_Rules_1 
sem_Rules_Nil_1  =
    (T_Rules_1 (\ (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsIdefs)
                  (!_lhsIforcedIrrefutables)
                  (!_lhsInt)
                  (!_lhsIoptions) ->
                    (case (Map.empty) of
                     { !_lhsOallAttributes ->
                     (case (Seq.empty) of
                      { !_lhsOerrors ->
                      (case ([]) of
                       { !_output ->
                       (case (_output) of
                        { !_lhsOoutput ->
                        ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) })) )
-- TypeSig -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         output               : SELF 
   alternatives:
      alternative TypeSig:
         child name           : {Identifier}
         child tp             : {Type}
         visit 0:
            local output      : _
-}
-- cata
sem_TypeSig :: TypeSig  ->
               T_TypeSig 
sem_TypeSig !(TypeSig _name _tp )  =
    (sem_TypeSig_TypeSig _name _tp )
-- semantic domain
newtype T_TypeSig  = T_TypeSig (( TypeSig))
data Inh_TypeSig  = Inh_TypeSig {}
data Syn_TypeSig  = Syn_TypeSig {output_Syn_TypeSig :: !(TypeSig)}
wrap_TypeSig :: T_TypeSig  ->
                Inh_TypeSig  ->
                Syn_TypeSig 
wrap_TypeSig !(T_TypeSig sem ) !(Inh_TypeSig )  =
    (let ( !_lhsOoutput) =
             (sem )
     in  (Syn_TypeSig _lhsOoutput ))
sem_TypeSig_TypeSig :: Identifier ->
                       Type ->
                       T_TypeSig 
sem_TypeSig_TypeSig !name_ !tp_  =
    (T_TypeSig (case (TypeSig name_ tp_) of
                { !_output ->
                (case (_output) of
                 { !_lhsOoutput ->
                 ( _lhsOoutput) }) }) )
-- TypeSigs ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         output               : SELF 
   alternatives:
      alternative Cons:
         child hd             : TypeSig 
         child tl             : TypeSigs 
         visit 0:
            local output      : _
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_TypeSigs :: TypeSigs  ->
                T_TypeSigs 
sem_TypeSigs !list  =
    (Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list) )
-- semantic domain
newtype T_TypeSigs  = T_TypeSigs (( TypeSigs))
data Inh_TypeSigs  = Inh_TypeSigs {}
data Syn_TypeSigs  = Syn_TypeSigs {output_Syn_TypeSigs :: !(TypeSigs)}
wrap_TypeSigs :: T_TypeSigs  ->
                 Inh_TypeSigs  ->
                 Syn_TypeSigs 
wrap_TypeSigs !(T_TypeSigs sem ) !(Inh_TypeSigs )  =
    (let ( !_lhsOoutput) =
             (sem )
     in  (Syn_TypeSigs _lhsOoutput ))
sem_TypeSigs_Cons :: T_TypeSig  ->
                     T_TypeSigs  ->
                     T_TypeSigs 
sem_TypeSigs_Cons !(T_TypeSig hd_ ) !(T_TypeSigs tl_ )  =
    (T_TypeSigs (case ((tl_ )) of
                 { ( !_tlIoutput) ->
                 (case ((hd_ )) of
                  { ( !_hdIoutput) ->
                  (case ((:) _hdIoutput _tlIoutput) of
                   { !_output ->
                   (case (_output) of
                    { !_lhsOoutput ->
                    ( _lhsOoutput) }) }) }) }) )
sem_TypeSigs_Nil :: T_TypeSigs 
sem_TypeSigs_Nil  =
    (T_TypeSigs (case ([]) of
                 { !_output ->
                 (case (_output) of
                  { !_lhsOoutput ->
                  ( _lhsOoutput) }) }) )