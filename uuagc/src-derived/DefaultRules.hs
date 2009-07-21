{-# OPTIONS_GHC -fbang-patterns #-}

-- UUAGC 0.9.10 (DefaultRules.ag)
module DefaultRules where

import qualified List (delete,intersperse)

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq,(><))
import UU.Scanner.Position(noPos)
import Pretty
import Maybe
import HsToken
import HsTokenScanner

import AbstractSyntax
import ErrorMessages

import Options(Options,modcopy,rename)


-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import CommonTypes


-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)

fieldName n       = '@' : getName n
	
locName n         = '@' : getName n

attrName fld attr
 | fld == _LOC    = '@' :                       getName attr 
 | otherwise      = '@' : getName fld ++ "." ++ getName attr

_ACHILD = Ident "(" noPos -- hack
	
	
getConName typeSyns rename nt con1 
 | nt `elem` map fst typeSyns  =  synonym
 | otherwise                   =  normalName
 where con                            = getName con1
       normalName | rename            = getName nt++"_"++ con
                  | otherwise         =  con
       synonym    | con == "Cons"     = "(:)"
                  | con == "Nil"      = "[]"
                  | con == "Just"     = "Just"
                  | con == "Nothing"  = "Nothing"
                  | otherwise         = normalName



concatSeq = foldr (Seq.><) Seq.empty

splitAttrs :: Map Identifier a -> [Identifier] -> ([(Identifier,a)],[Identifier])	  -- a used as (String,String)
splitAttrs _      []           
  =  ([],[])
splitAttrs useMap (n:rest) 
  =  let (uses,normals) = splitAttrs useMap rest
     in case Map.lookup n useMap of
          Just x  -> ((n,x):uses ,   normals )       
          Nothing -> (      uses , n:normals )

removeDefined ::  Set (Identifier,Identifier) -> (Identifier,Attributes) -> (Identifier,[Identifier])
removeDefined defined (fld,as) 
  = ( fld
    , [ a 
      | a <- Map.keys as
      , not (Set.member (fld,a) defined) 
      ]
    )



	
	
	
deprecatedCopyRuleError nt con fld a
 = let mesg = 
                "In the definitions for alternative" 
            >#< getName con 
            >#< "of nonterminal" 
            >#< getName nt 
            >|< "," 
            >-< "the value of field" 
            >#< getName a  
            >#< "is copied by a copy-rule."                                    
            >-< "Copying the value of a field using a copy-rule is deprecated"
            >-< "Please add the following lines to your code:"
            >-< (    "SEM" 
                >#< getName nt
                >-< indent 2 (      "|" 
                             >#< getName con 
                             >#< getName fld 
                             >#< "." 
                             >#< a 
                             >#< "=" 
                             >#< "@" 
                             >|< a
                             )
                )  
    in  CustomError True (getPos a) mesg
	
	
missingRuleErrorExpr nt con fld a
 = "error \"missing rule: " 
   ++ show nt  ++ "." ++ show con ++ "." 
   ++ show fld ++ "." ++ show a   ++ "\""
	
	
	
makeRule :: (Identifier,Identifier) -> Expression -> String -> Rule
makeRule (f1,a1) expr origin 
 = Rule (Alias f1 a1 (Underscore noPos) []) 
        expr 
        False 
        origin


useRule :: Set Identifier -> [(Identifier,Attributes)] -> (Identifier,(String,String,String)) -> Rule
useRule locals ch_outs (n,(op,e,pos)) 
 =  let elems = [ fld 
                | (fld,as) <- ch_outs
                , Map.member n as
                ]

        expr | Set.member n locals  =  attrName _LOC n
             | null elems           =  e
             | otherwise            =  foldr1 (\x y -> x ++ " " ++ op ++ " " ++ y) 
                                              (map (flip attrName n) elems)

        tks | Set.member n locals  =  [AGLocal n noPos Nothing]
            | null elems           =  lexTokens noPos e
            | otherwise            =  lexTokens noPos str
                                      where
                                        str = foldr1 (\x y -> x ++ " " ++ op ++ " " ++ y) 
                                                (map (flip attrName n) elems)

    in makeRule (_LHS,n) 
                (Expression noPos tks)
                ("use rule " ++ pos)




selfRule lhsNecLoc attr x   
 = let expr | lhsNecLoc  = locName attr
            | otherwise  = x

       tks | lhsNecLoc   = [AGLocal attr noPos Nothing]
           | otherwise   = lexTokens noPos x

   in makeRule (if lhsNecLoc then _LHS else _LOC,attr)
               (Expression noPos tks)
               "self rule"
               



concatRE rsess = let (rss,ess) = unzip rsess
                 in (concat rss, concatSeq ess)


copyRule :: Identifier -> Identifier -> Bool -> Set Identifier -> (Map Identifier Identifier, (Identifier,[Identifier])) -> ([Rule], Seq Error)
copyRule nt con modcopy locals (env,(fld,as)) 
 = concatRE (map copyRu as)
 
 where 
       copyRu a
           = ( [ makeRule (fld,a) 
                          (Expression noPos tks)
                          (cruletxt sel)
               ]
             , err
             )
                
        where 
              sel 
               |    not modcopy 
                 && Set.member a locals  =  Just _LOC
               | otherwise               =  Map.lookup a env

              (expr,err) 
               = case sel of
                  Nothing         -> ( missingRuleErrorExpr nt con fld a
                                     , Seq.singleton (MissingRule nt con fld a)
                                     )
                  Just f 
                   | f == _ACHILD -> ( fieldName a
                                     , Seq.singleton (deprecatedCopyRuleError nt con fld a)
                                     )
                   | otherwise    -> ( attrName f a
                                     , Seq.empty
                                     )
              
              (tks,err')
               = case sel of
                  Nothing         -> ( [HsToken (missingRuleErrorExpr nt con fld a) noPos]
                                     , Seq.singleton (MissingRule nt con fld a)
                                     )
                  Just f 
                   | f == _ACHILD -> ( [AGLocal a noPos Nothing]
                                     , Seq.singleton (deprecatedCopyRuleError nt con fld a)
                                     )
                   | otherwise    -> ( [AGField f a noPos Nothing]
                                     , Seq.empty
                                     )

              cruletxt sel
               | local                            = "copy rule (from local)"
               | deprChild                        = "deprecated child copy"
               | Set.member a locals && nonlocal  = "modified copy rule"
               | incoming && outgoing             = "copy rule (chain)"
               | incoming                         = "copy rule (down)"
               | outgoing                         = "copy rule (up)"
               | otherwise                        = "copy rule (chain)"
                where outgoing  =  fld == _LHS
                      incoming  =  maybe False (== _LHS)    sel
                      nonlocal  =  maybe False (/= _LOC)    sel
                      local     =  maybe False (== _LOC)    sel
                      deprChild =  maybe False (== _ACHILD) sel


{- 
multiRule replaces
  loc.(a,b) = e
by  
  loc.tup1  = e
  loc.(a,_) = @loc.tup1
  loc.(_,b) = @loc.tup1
It needs to thread a unique number for inventing names for the tuples.

It also works for nested tuples:
  loc.(a,(b,c)) = e
becomes
  loc.tup1      = e
  loc.(a,_)     = @loc.tup1
  loc.(_,tup2)  = @loc.tup1
  loc.(b,_)     = @loc.tup2
  loc.(_,c)     = @loc.tup2
-}

multiRule :: Rule -> Int -> ([Rule], Int)	
multiRule (Rule pat expr owrt origin) uniq
  =  let f :: (Pattern->Pattern) -> Expression -> Pattern -> Int -> (Pattern, ([Rule], Int))
         f w e (Product pos pats) n 
           = let freshName = Ident ("_tup" ++ show n) pos 
                 freshExpr = Expression pos freshTks
                 freshTks  = [AGField _LOC freshName pos Nothing]
                 freshPat  = Alias _LOC freshName (Underscore pos) pats
                 a = length pats - 1
                 us b p = Product pos (replicate (a-b) (Underscore pos) ++ [p] ++ replicate b (Underscore pos))
                 g :: Pattern -> ([Pattern],[Rule],Int) -> ([Pattern],[Rule],Int)
                 g p (xs1,rs1,n1)   = let (x2,(rs2,n2)) = f (us (length xs1)) freshExpr p n1
                                      in  (x2:xs1, rs2++rs1, n2)
                 (xs9,rs9,n9) = foldr g ([], [], n+1) pats
             in  ( freshPat
                 , ( Rule (w freshPat) e owrt origin : rs9
                   , n9
                   )
                 )
         f w e p n 
           = ( p
             , ( [Rule (w p) e owrt origin]
               , n
               )
             )
     in snd (f id expr pat uniq)

-- Child -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         cr                   : Bool
         nt                   : NontermIdent
      synthesized attributes:
         errors               : Seq Error
         field                :  (Identifier,Type,Bool) 
         inherited            : Attributes
         name                 : Identifier
         output               : SELF 
         synthesized          : Attributes
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
newtype T_Child  = T_Child (ConstructorIdent ->
                            Bool ->
                            NontermIdent ->
                            ( (Seq Error),( (Identifier,Type,Bool) ),Attributes,Identifier,Child,Attributes))
data Inh_Child  = Inh_Child {con_Inh_Child :: !(ConstructorIdent),cr_Inh_Child :: !(Bool),nt_Inh_Child :: !(NontermIdent)}
data Syn_Child  = Syn_Child {errors_Syn_Child :: !(Seq Error),field_Syn_Child :: !( (Identifier,Type,Bool) ),inherited_Syn_Child :: !(Attributes),name_Syn_Child :: !(Identifier),output_Syn_Child :: !(Child),synthesized_Syn_Child :: !(Attributes)}
wrap_Child :: T_Child  ->
              Inh_Child  ->
              Syn_Child 
wrap_Child !(T_Child sem ) !(Inh_Child _lhsIcon _lhsIcr _lhsInt )  =
    (let ( !_lhsOerrors,!_lhsOfield,!_lhsOinherited,!_lhsOname,!_lhsOoutput,!_lhsOsynthesized) =
             (sem _lhsIcon _lhsIcr _lhsInt )
     in  (Syn_Child _lhsOerrors _lhsOfield _lhsOinherited _lhsOname _lhsOoutput _lhsOsynthesized ))
sem_Child_Child :: Identifier ->
                   Type ->
                   Attributes ->
                   Attributes ->
                   Bool ->
                   T_Child 
sem_Child_Child !name_ !tp_ !inh_ !syn_ !higherOrder_  =
    (T_Child (\ (!_lhsIcon)
                (!_lhsIcr)
                (!_lhsInt) ->
                  (case (Seq.empty) of
                   { !_lhsOerrors ->
                   (case ((name_,tp_,higherOrder_)) of
                    { !_lhsOfield ->
                    (case (inh_) of
                     { !_lhsOinherited ->
                     (case (name_) of
                      { !_lhsOname ->
                      (case (Child name_ tp_ inh_ syn_ higherOrder_) of
                       { !_output ->
                       (case (_output) of
                        { !_lhsOoutput ->
                        (case (syn_) of
                         { !_lhsOsynthesized ->
                         ( _lhsOerrors,_lhsOfield,_lhsOinherited,_lhsOname,_lhsOoutput,_lhsOsynthesized) }) }) }) }) }) }) })) )
-- Children ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         cr                   : Bool
         nt                   : NontermIdent
      synthesized attributes:
         errors               : Seq Error
         fields               : [(Identifier,Type,Bool)]
         inputs               : [(Identifier, Attributes)]
         output               : SELF 
         outputs              : [(Identifier, Attributes)]
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
newtype T_Children  = T_Children (ConstructorIdent ->
                                  Bool ->
                                  NontermIdent ->
                                  ( (Seq Error),([(Identifier,Type,Bool)]),([(Identifier, Attributes)]),Children,([(Identifier, Attributes)])))
data Inh_Children  = Inh_Children {con_Inh_Children :: !(ConstructorIdent),cr_Inh_Children :: !(Bool),nt_Inh_Children :: !(NontermIdent)}
data Syn_Children  = Syn_Children {errors_Syn_Children :: !(Seq Error),fields_Syn_Children :: !([(Identifier,Type,Bool)]),inputs_Syn_Children :: !([(Identifier, Attributes)]),output_Syn_Children :: !(Children),outputs_Syn_Children :: !([(Identifier, Attributes)])}
wrap_Children :: T_Children  ->
                 Inh_Children  ->
                 Syn_Children 
wrap_Children !(T_Children sem ) !(Inh_Children _lhsIcon _lhsIcr _lhsInt )  =
    (let ( !_lhsOerrors,!_lhsOfields,!_lhsOinputs,!_lhsOoutput,!_lhsOoutputs) =
             (sem _lhsIcon _lhsIcr _lhsInt )
     in  (Syn_Children _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutput _lhsOoutputs ))
sem_Children_Cons :: T_Child  ->
                     T_Children  ->
                     T_Children 
sem_Children_Cons !(T_Child hd_ ) !(T_Children tl_ )  =
    (T_Children (\ (!_lhsIcon)
                   (!_lhsIcr)
                   (!_lhsInt) ->
                     (case (_lhsInt) of
                      { !_tlOnt ->
                      (case (_lhsIcr) of
                       { !_tlOcr ->
                       (case (_lhsIcon) of
                        { !_tlOcon ->
                        (case ((tl_ _tlOcon _tlOcr _tlOnt )) of
                         { ( !_tlIerrors,!_tlIfields,!_tlIinputs,!_tlIoutput,!_tlIoutputs) ->
                         (case (_lhsInt) of
                          { !_hdOnt ->
                          (case (_lhsIcr) of
                           { !_hdOcr ->
                           (case (_lhsIcon) of
                            { !_hdOcon ->
                            (case ((hd_ _hdOcon _hdOcr _hdOnt )) of
                             { ( !_hdIerrors,!_hdIfield,!_hdIinherited,!_hdIname,!_hdIoutput,!_hdIsynthesized) ->
                             (case (_hdIerrors Seq.>< _tlIerrors) of
                              { !_lhsOerrors ->
                              (case (_hdIfield : _tlIfields) of
                               { !_lhsOfields ->
                               (case ((_hdIname, _hdIinherited) : _tlIinputs) of
                                { !_lhsOinputs ->
                                (case ((:) _hdIoutput _tlIoutput) of
                                 { !_output ->
                                 (case (_output) of
                                  { !_lhsOoutput ->
                                  (case ((_hdIname, _hdIsynthesized) : _tlIoutputs) of
                                   { !_lhsOoutputs ->
                                   ( _lhsOerrors,_lhsOfields,_lhsOinputs,_lhsOoutput,_lhsOoutputs) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Children_Nil :: T_Children 
sem_Children_Nil  =
    (T_Children (\ (!_lhsIcon)
                   (!_lhsIcr)
                   (!_lhsInt) ->
                     (case (Seq.empty) of
                      { !_lhsOerrors ->
                      (case ([]) of
                       { !_lhsOfields ->
                       (case ([]) of
                        { !_lhsOinputs ->
                        (case ([]) of
                         { !_output ->
                         (case (_output) of
                          { !_lhsOoutput ->
                          (case ([]) of
                           { !_lhsOoutputs ->
                           ( _lhsOerrors,_lhsOfields,_lhsOinputs,_lhsOoutput,_lhsOoutputs) }) }) }) }) }) })) )
-- Grammar -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
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
newtype T_Grammar  = T_Grammar (Options ->
                                ( (Seq Error),Grammar))
data Inh_Grammar  = Inh_Grammar {options_Inh_Grammar :: !(Options)}
data Syn_Grammar  = Syn_Grammar {errors_Syn_Grammar :: !(Seq Error),output_Syn_Grammar :: !(Grammar)}
wrap_Grammar :: T_Grammar  ->
                Inh_Grammar  ->
                Syn_Grammar 
wrap_Grammar !(T_Grammar sem ) !(Inh_Grammar _lhsIoptions )  =
    (let ( !_lhsOerrors,!_lhsOoutput) =
             (sem _lhsIoptions )
     in  (Syn_Grammar _lhsOerrors _lhsOoutput ))
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
    (T_Grammar (\ (!_lhsIoptions) ->
                    (case (manualAttrOrderMap_) of
                     { !_nontsOmanualAttrOrderMap ->
                     (case (typeSyns_) of
                      { !_nontsOtypeSyns ->
                      (case (useMap_) of
                       { !_nontsOuseMap ->
                       (case (modcopy   _lhsIoptions) of
                        { !_nontsOcr ->
                        (case (rename    _lhsIoptions) of
                         { !_nontsOo_rename ->
                         (case ((nonts_ )) of
                          { ( !_nontsIcollect_nts,!T_Nonterminals_1 nonts_1) ->
                          (case (1) of
                           { !_nontsOuniq ->
                           (case (_nontsIcollect_nts) of
                            { !_nontsOnonterminals ->
                            (case ((nonts_1 _nontsOcr _nontsOmanualAttrOrderMap _nontsOnonterminals _nontsOo_rename _nontsOtypeSyns _nontsOuniq _nontsOuseMap )) of
                             { ( !_nontsIerrors,!_nontsIoutput,!_nontsIuniq) ->
                             (case (_nontsIerrors) of
                              { !_lhsOerrors ->
                              (case (Grammar typeSyns_ useMap_ derivings_ wrappers_ _nontsIoutput pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ uniqueMap_) of
                               { !_output ->
                               (case (_output) of
                                { !_lhsOoutput ->
                                ( _lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) })) )
-- Nonterminal -------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         collect_nts          : Set NontermIdent
   visit 1:
      inherited attributes:
         cr                   : Bool
         manualAttrOrderMap   : AttrOrderMap
         nonterminals         : Set NontermIdent
         o_rename             : Bool
         typeSyns             : TypeSyns
         useMap               : UseMap
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Nonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : Productions 
         visit 1:
            local output      : _
-}
-- cata
sem_Nonterminal :: Nonterminal  ->
                   T_Nonterminal 
sem_Nonterminal !(Nonterminal _nt _params _inh _syn _prods )  =
    (sem_Nonterminal_Nonterminal _nt _params _inh _syn (sem_Productions _prods ) )
-- semantic domain
newtype T_Nonterminal  = T_Nonterminal (( (Set NontermIdent),T_Nonterminal_1 ))
newtype T_Nonterminal_1  = T_Nonterminal_1 (Bool ->
                                            AttrOrderMap ->
                                            (Set NontermIdent) ->
                                            Bool ->
                                            TypeSyns ->
                                            Int ->
                                            UseMap ->
                                            ( (Seq Error),Nonterminal,Int))
data Inh_Nonterminal  = Inh_Nonterminal {cr_Inh_Nonterminal :: !(Bool),manualAttrOrderMap_Inh_Nonterminal :: !(AttrOrderMap),nonterminals_Inh_Nonterminal :: !(Set NontermIdent),o_rename_Inh_Nonterminal :: !(Bool),typeSyns_Inh_Nonterminal :: !(TypeSyns),uniq_Inh_Nonterminal :: !(Int),useMap_Inh_Nonterminal :: !(UseMap)}
data Syn_Nonterminal  = Syn_Nonterminal {collect_nts_Syn_Nonterminal :: !(Set NontermIdent),errors_Syn_Nonterminal :: !(Seq Error),output_Syn_Nonterminal :: !(Nonterminal),uniq_Syn_Nonterminal :: !(Int)}
wrap_Nonterminal :: T_Nonterminal  ->
                    Inh_Nonterminal  ->
                    Syn_Nonterminal 
wrap_Nonterminal !(T_Nonterminal sem ) !(Inh_Nonterminal _lhsIcr _lhsImanualAttrOrderMap _lhsInonterminals _lhsIo_rename _lhsItypeSyns _lhsIuniq _lhsIuseMap )  =
    (let ( !_lhsOcollect_nts,!T_Nonterminal_1 sem_1) =
             (sem )
         ( !_lhsOerrors,!_lhsOoutput,!_lhsOuniq) =
             (sem_1 _lhsIcr _lhsImanualAttrOrderMap _lhsInonterminals _lhsIo_rename _lhsItypeSyns _lhsIuniq _lhsIuseMap )
     in  (Syn_Nonterminal _lhsOcollect_nts _lhsOerrors _lhsOoutput _lhsOuniq ))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions  ->
                               T_Nonterminal 
sem_Nonterminal_Nonterminal !nt_ !params_ !inh_ !syn_ !(T_Productions prods_ )  =
    (T_Nonterminal (case (Set.singleton nt_) of
                    { !_lhsOcollect_nts ->
                    (case ((sem_Nonterminal_Nonterminal_1 nt_ syn_ inh_ (T_Productions prods_ ) params_ )) of
                     { ( !sem_Nonterminal_1) ->
                     ( _lhsOcollect_nts,sem_Nonterminal_1) }) }) )
sem_Nonterminal_Nonterminal_1 :: NontermIdent ->
                                 Attributes ->
                                 Attributes ->
                                 T_Productions  ->
                                 ([Identifier]) ->
                                 T_Nonterminal_1 
sem_Nonterminal_Nonterminal_1 !nt_ !syn_ !inh_ !(T_Productions prods_ ) !params_  =
    (T_Nonterminal_1 (\ (!_lhsIcr)
                        (!_lhsImanualAttrOrderMap)
                        (!_lhsInonterminals)
                        (!_lhsIo_rename)
                        (!_lhsItypeSyns)
                        (!_lhsIuniq)
                        (!_lhsIuseMap) ->
                          (case (_lhsItypeSyns) of
                           { !_prodsOtypeSyns ->
                           (case (_lhsIo_rename) of
                            { !_prodsOo_rename ->
                            (case (_lhsImanualAttrOrderMap) of
                             { !_prodsOmanualAttrOrderMap ->
                             (case (_lhsIcr) of
                              { !_prodsOcr ->
                              (case (nt_) of
                               { !_prodsOnt ->
                               (case (Map.findWithDefault Map.empty nt_ _lhsIuseMap) of
                                { !_prodsOuseMap ->
                                (case (syn_) of
                                 { !_prodsOsyn ->
                                 (case (inh_) of
                                  { !_prodsOinh ->
                                  (case (_lhsIuniq) of
                                   { !_prodsOuniq ->
                                   (case (_lhsInonterminals) of
                                    { !_prodsOnonterminals ->
                                    (case ((prods_ _prodsOcr _prodsOinh _prodsOmanualAttrOrderMap _prodsOnonterminals _prodsOnt _prodsOo_rename _prodsOsyn _prodsOtypeSyns _prodsOuniq _prodsOuseMap )) of
                                     { ( !_prodsIerrors,!_prodsIoutput,!_prodsIuniq) ->
                                     (case (_prodsIerrors) of
                                      { !_lhsOerrors ->
                                      (case (Nonterminal nt_ params_ inh_ syn_ _prodsIoutput) of
                                       { !_output ->
                                       (case (_output) of
                                        { !_lhsOoutput ->
                                        (case (_prodsIuniq) of
                                         { !_lhsOuniq ->
                                         ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         collect_nts          : Set NontermIdent
   visit 1:
      inherited attributes:
         cr                   : Bool
         manualAttrOrderMap   : AttrOrderMap
         nonterminals         : Set NontermIdent
         o_rename             : Bool
         typeSyns             : TypeSyns
         useMap               : UseMap
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Cons:
         child hd             : Nonterminal 
         child tl             : Nonterminals 
         visit 1:
            local output      : _
      alternative Nil:
         visit 1:
            local output      : _
-}
-- cata
sem_Nonterminals :: Nonterminals  ->
                    T_Nonterminals 
sem_Nonterminals !list  =
    (Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list) )
-- semantic domain
newtype T_Nonterminals  = T_Nonterminals (( (Set NontermIdent),T_Nonterminals_1 ))
newtype T_Nonterminals_1  = T_Nonterminals_1 (Bool ->
                                              AttrOrderMap ->
                                              (Set NontermIdent) ->
                                              Bool ->
                                              TypeSyns ->
                                              Int ->
                                              UseMap ->
                                              ( (Seq Error),Nonterminals,Int))
data Inh_Nonterminals  = Inh_Nonterminals {cr_Inh_Nonterminals :: !(Bool),manualAttrOrderMap_Inh_Nonterminals :: !(AttrOrderMap),nonterminals_Inh_Nonterminals :: !(Set NontermIdent),o_rename_Inh_Nonterminals :: !(Bool),typeSyns_Inh_Nonterminals :: !(TypeSyns),uniq_Inh_Nonterminals :: !(Int),useMap_Inh_Nonterminals :: !(UseMap)}
data Syn_Nonterminals  = Syn_Nonterminals {collect_nts_Syn_Nonterminals :: !(Set NontermIdent),errors_Syn_Nonterminals :: !(Seq Error),output_Syn_Nonterminals :: !(Nonterminals),uniq_Syn_Nonterminals :: !(Int)}
wrap_Nonterminals :: T_Nonterminals  ->
                     Inh_Nonterminals  ->
                     Syn_Nonterminals 
wrap_Nonterminals !(T_Nonterminals sem ) !(Inh_Nonterminals _lhsIcr _lhsImanualAttrOrderMap _lhsInonterminals _lhsIo_rename _lhsItypeSyns _lhsIuniq _lhsIuseMap )  =
    (let ( !_lhsOcollect_nts,!T_Nonterminals_1 sem_1) =
             (sem )
         ( !_lhsOerrors,!_lhsOoutput,!_lhsOuniq) =
             (sem_1 _lhsIcr _lhsImanualAttrOrderMap _lhsInonterminals _lhsIo_rename _lhsItypeSyns _lhsIuniq _lhsIuseMap )
     in  (Syn_Nonterminals _lhsOcollect_nts _lhsOerrors _lhsOoutput _lhsOuniq ))
sem_Nonterminals_Cons :: T_Nonterminal  ->
                         T_Nonterminals  ->
                         T_Nonterminals 
sem_Nonterminals_Cons !(T_Nonterminal hd_ ) !(T_Nonterminals tl_ )  =
    (T_Nonterminals (case ((tl_ )) of
                     { ( !_tlIcollect_nts,!T_Nonterminals_1 tl_1) ->
                     (case ((hd_ )) of
                      { ( !_hdIcollect_nts,!T_Nonterminal_1 hd_1) ->
                      (case (_hdIcollect_nts `Set.union` _tlIcollect_nts) of
                       { !_lhsOcollect_nts ->
                       (case ((sem_Nonterminals_Cons_1 (T_Nonterminal_1 hd_1 ) (T_Nonterminals_1 tl_1 ) )) of
                        { ( !sem_Nonterminals_1) ->
                        ( _lhsOcollect_nts,sem_Nonterminals_1) }) }) }) }) )
sem_Nonterminals_Cons_1 :: T_Nonterminal_1  ->
                           T_Nonterminals_1  ->
                           T_Nonterminals_1 
sem_Nonterminals_Cons_1 !(T_Nonterminal_1 hd_1 ) !(T_Nonterminals_1 tl_1 )  =
    (T_Nonterminals_1 (\ (!_lhsIcr)
                         (!_lhsImanualAttrOrderMap)
                         (!_lhsInonterminals)
                         (!_lhsIo_rename)
                         (!_lhsItypeSyns)
                         (!_lhsIuniq)
                         (!_lhsIuseMap) ->
                           (case (_lhsIuseMap) of
                            { !_tlOuseMap ->
                            (case (_lhsItypeSyns) of
                             { !_tlOtypeSyns ->
                             (case (_lhsIo_rename) of
                              { !_tlOo_rename ->
                              (case (_lhsImanualAttrOrderMap) of
                               { !_tlOmanualAttrOrderMap ->
                               (case (_lhsIcr) of
                                { !_tlOcr ->
                                (case (_lhsIuseMap) of
                                 { !_hdOuseMap ->
                                 (case (_lhsItypeSyns) of
                                  { !_hdOtypeSyns ->
                                  (case (_lhsIo_rename) of
                                   { !_hdOo_rename ->
                                   (case (_lhsImanualAttrOrderMap) of
                                    { !_hdOmanualAttrOrderMap ->
                                    (case (_lhsIcr) of
                                     { !_hdOcr ->
                                     (case (_lhsIuniq) of
                                      { !_hdOuniq ->
                                      (case (_lhsInonterminals) of
                                       { !_hdOnonterminals ->
                                       (case ((hd_1 _hdOcr _hdOmanualAttrOrderMap _hdOnonterminals _hdOo_rename _hdOtypeSyns _hdOuniq _hdOuseMap )) of
                                        { ( !_hdIerrors,!_hdIoutput,!_hdIuniq) ->
                                        (case (_hdIuniq) of
                                         { !_tlOuniq ->
                                         (case (_lhsInonterminals) of
                                          { !_tlOnonterminals ->
                                          (case ((tl_1 _tlOcr _tlOmanualAttrOrderMap _tlOnonterminals _tlOo_rename _tlOtypeSyns _tlOuniq _tlOuseMap )) of
                                           { ( !_tlIerrors,!_tlIoutput,!_tlIuniq) ->
                                           (case (_hdIerrors Seq.>< _tlIerrors) of
                                            { !_lhsOerrors ->
                                            (case ((:) _hdIoutput _tlIoutput) of
                                             { !_output ->
                                             (case (_output) of
                                              { !_lhsOoutput ->
                                              (case (_tlIuniq) of
                                               { !_lhsOuniq ->
                                               ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Nonterminals_Nil :: T_Nonterminals 
sem_Nonterminals_Nil  =
    (T_Nonterminals (case (Set.empty) of
                     { !_lhsOcollect_nts ->
                     (case ((sem_Nonterminals_Nil_1 )) of
                      { ( !sem_Nonterminals_1) ->
                      ( _lhsOcollect_nts,sem_Nonterminals_1) }) }) )
sem_Nonterminals_Nil_1 :: T_Nonterminals_1 
sem_Nonterminals_Nil_1  =
    (T_Nonterminals_1 (\ (!_lhsIcr)
                         (!_lhsImanualAttrOrderMap)
                         (!_lhsInonterminals)
                         (!_lhsIo_rename)
                         (!_lhsItypeSyns)
                         (!_lhsIuniq)
                         (!_lhsIuseMap) ->
                           (case (Seq.empty) of
                            { !_lhsOerrors ->
                            (case ([]) of
                             { !_output ->
                             (case (_output) of
                              { !_lhsOoutput ->
                              (case (_lhsIuniq) of
                               { !_lhsOuniq ->
                               ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) })) )
-- Pattern -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         nt                   : NontermIdent
      synthesized attributes:
         containsVars         : Bool
         copy                 : SELF 
         definedAttrs         : Set (Identifier,Identifier)
         errors               : Seq Error
         locals               : Set Identifier
         output               : SELF 
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         child parts          : Patterns 
         visit 0:
            local copy        : _
            local output      : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
            local output      : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
            local output      : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
            local output      : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
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
newtype T_Pattern  = T_Pattern (ConstructorIdent ->
                                NontermIdent ->
                                ( Bool,Pattern,(Set (Identifier,Identifier)),(Seq Error),(Set Identifier),Pattern))
data Inh_Pattern  = Inh_Pattern {con_Inh_Pattern :: !(ConstructorIdent),nt_Inh_Pattern :: !(NontermIdent)}
data Syn_Pattern  = Syn_Pattern {containsVars_Syn_Pattern :: !(Bool),copy_Syn_Pattern :: !(Pattern),definedAttrs_Syn_Pattern :: !(Set (Identifier,Identifier)),errors_Syn_Pattern :: !(Seq Error),locals_Syn_Pattern :: !(Set Identifier),output_Syn_Pattern :: !(Pattern)}
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern !(T_Pattern sem ) !(Inh_Pattern _lhsIcon _lhsInt )  =
    (let ( !_lhsOcontainsVars,!_lhsOcopy,!_lhsOdefinedAttrs,!_lhsOerrors,!_lhsOlocals,!_lhsOoutput) =
             (sem _lhsIcon _lhsInt )
     in  (Syn_Pattern _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Alias !field_ !attr_ !(T_Pattern pat_ ) !(T_Patterns parts_ )  =
    (T_Pattern (\ (!_lhsIcon)
                  (!_lhsInt) ->
                    (case (True) of
                     { !_lhsOcontainsVars ->
                     (case (_lhsInt) of
                      { !_partsOnt ->
                      (case (_lhsIcon) of
                       { !_partsOcon ->
                       (case ((parts_ _partsOcon _partsOnt )) of
                        { ( !_partsIcontainsVars,!_partsIcopy,!_partsIdefinedAttrs,!_partsIerrors,!_partsIlocals,!_partsIoutput) ->
                        (case (_lhsInt) of
                         { !_patOnt ->
                         (case (_lhsIcon) of
                          { !_patOcon ->
                          (case ((pat_ _patOcon _patOnt )) of
                           { ( !_patIcontainsVars,!_patIcopy,!_patIdefinedAttrs,!_patIerrors,!_patIlocals,!_patIoutput) ->
                           (case (Alias field_ attr_ _patIcopy _partsIcopy) of
                            { !_copy ->
                            (case (_copy) of
                             { !_lhsOcopy ->
                             (case (Set.insert (field_,attr_) _patIdefinedAttrs) of
                              { !_lhsOdefinedAttrs ->
                              (case (_patIerrors Seq.>< _partsIerrors) of
                               { !_lhsOerrors ->
                               (case (if field_ == _LOC
                                         then Set.insert attr_ _patIlocals
                                         else _patIlocals) of
                                { !_lhsOlocals ->
                                (case (Alias field_ attr_ _patIoutput _partsIoutput) of
                                 { !_output ->
                                 (case (_output) of
                                  { !_lhsOoutput ->
                                  ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr !name_ !(T_Patterns pats_ )  =
    (T_Pattern (\ (!_lhsIcon)
                  (!_lhsInt) ->
                    (case (_lhsInt) of
                     { !_patsOnt ->
                     (case (_lhsIcon) of
                      { !_patsOcon ->
                      (case ((pats_ _patsOcon _patsOnt )) of
                       { ( !_patsIcontainsVars,!_patsIcopy,!_patsIdefinedAttrs,!_patsIerrors,!_patsIlocals,!_patsIoutput) ->
                       (case (_patsIcontainsVars) of
                        { !_lhsOcontainsVars ->
                        (case (Constr name_ _patsIcopy) of
                         { !_copy ->
                         (case (_copy) of
                          { !_lhsOcopy ->
                          (case (_patsIdefinedAttrs) of
                           { !_lhsOdefinedAttrs ->
                           (case (_patsIerrors) of
                            { !_lhsOerrors ->
                            (case (_patsIlocals) of
                             { !_lhsOlocals ->
                             (case (Constr name_ _patsIoutput) of
                              { !_output ->
                              (case (_output) of
                               { !_lhsOoutput ->
                               ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) })) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable !(T_Pattern pat_ )  =
    (T_Pattern (\ (!_lhsIcon)
                  (!_lhsInt) ->
                    (case (_lhsInt) of
                     { !_patOnt ->
                     (case (_lhsIcon) of
                      { !_patOcon ->
                      (case ((pat_ _patOcon _patOnt )) of
                       { ( !_patIcontainsVars,!_patIcopy,!_patIdefinedAttrs,!_patIerrors,!_patIlocals,!_patIoutput) ->
                       (case (_patIcontainsVars) of
                        { !_lhsOcontainsVars ->
                        (case (Irrefutable _patIcopy) of
                         { !_copy ->
                         (case (_copy) of
                          { !_lhsOcopy ->
                          (case (_patIdefinedAttrs) of
                           { !_lhsOdefinedAttrs ->
                           (case (_patIerrors) of
                            { !_lhsOerrors ->
                            (case (_patIlocals) of
                             { !_lhsOlocals ->
                             (case (Irrefutable _patIoutput) of
                              { !_output ->
                              (case (_output) of
                               { !_lhsOoutput ->
                               ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) })) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product !pos_ !(T_Patterns pats_ )  =
    (T_Pattern (\ (!_lhsIcon)
                  (!_lhsInt) ->
                    (case (_lhsInt) of
                     { !_patsOnt ->
                     (case (_lhsIcon) of
                      { !_patsOcon ->
                      (case ((pats_ _patsOcon _patsOnt )) of
                       { ( !_patsIcontainsVars,!_patsIcopy,!_patsIdefinedAttrs,!_patsIerrors,!_patsIlocals,!_patsIoutput) ->
                       (case (_patsIcontainsVars) of
                        { !_lhsOcontainsVars ->
                        (case (Product pos_ _patsIcopy) of
                         { !_copy ->
                         (case (_copy) of
                          { !_lhsOcopy ->
                          (case (_patsIdefinedAttrs) of
                           { !_lhsOdefinedAttrs ->
                           (case (_patsIerrors) of
                            { !_lhsOerrors ->
                            (case (_patsIlocals) of
                             { !_lhsOlocals ->
                             (case (Product pos_ _patsIoutput) of
                              { !_output ->
                              (case (_output) of
                               { !_lhsOoutput ->
                               ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) })) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore !pos_  =
    (T_Pattern (\ (!_lhsIcon)
                  (!_lhsInt) ->
                    (case (False) of
                     { !_lhsOcontainsVars ->
                     (case (Underscore pos_) of
                      { !_copy ->
                      (case (_copy) of
                       { !_lhsOcopy ->
                       (case (Set.empty) of
                        { !_lhsOdefinedAttrs ->
                        (case (Seq.empty) of
                         { !_lhsOerrors ->
                         (case (Set.empty) of
                          { !_lhsOlocals ->
                          (case (Underscore pos_) of
                           { !_output ->
                           (case (_output) of
                            { !_lhsOoutput ->
                            ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) })) )
-- Patterns ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         nt                   : NontermIdent
      synthesized attributes:
         containsVars         : Bool
         copy                 : SELF 
         definedAttrs         : Set (Identifier,Identifier)
         errors               : Seq Error
         locals               : Set Identifier
         output               : SELF 
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 0:
            local copy        : _
            local output      : _
      alternative Nil:
         visit 0:
            local copy        : _
            local output      : _
-}
-- cata
sem_Patterns :: Patterns  ->
                T_Patterns 
sem_Patterns !list  =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list) )
-- semantic domain
newtype T_Patterns  = T_Patterns (ConstructorIdent ->
                                  NontermIdent ->
                                  ( Bool,Patterns,(Set (Identifier,Identifier)),(Seq Error),(Set Identifier),Patterns))
data Inh_Patterns  = Inh_Patterns {con_Inh_Patterns :: !(ConstructorIdent),nt_Inh_Patterns :: !(NontermIdent)}
data Syn_Patterns  = Syn_Patterns {containsVars_Syn_Patterns :: !(Bool),copy_Syn_Patterns :: !(Patterns),definedAttrs_Syn_Patterns :: !(Set (Identifier,Identifier)),errors_Syn_Patterns :: !(Seq Error),locals_Syn_Patterns :: !(Set Identifier),output_Syn_Patterns :: !(Patterns)}
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns !(T_Patterns sem ) !(Inh_Patterns _lhsIcon _lhsInt )  =
    (let ( !_lhsOcontainsVars,!_lhsOcopy,!_lhsOdefinedAttrs,!_lhsOerrors,!_lhsOlocals,!_lhsOoutput) =
             (sem _lhsIcon _lhsInt )
     in  (Syn_Patterns _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons !(T_Pattern hd_ ) !(T_Patterns tl_ )  =
    (T_Patterns (\ (!_lhsIcon)
                   (!_lhsInt) ->
                     (case (_lhsInt) of
                      { !_tlOnt ->
                      (case (_lhsIcon) of
                       { !_tlOcon ->
                       (case ((tl_ _tlOcon _tlOnt )) of
                        { ( !_tlIcontainsVars,!_tlIcopy,!_tlIdefinedAttrs,!_tlIerrors,!_tlIlocals,!_tlIoutput) ->
                        (case (_lhsInt) of
                         { !_hdOnt ->
                         (case (_lhsIcon) of
                          { !_hdOcon ->
                          (case ((hd_ _hdOcon _hdOnt )) of
                           { ( !_hdIcontainsVars,!_hdIcopy,!_hdIdefinedAttrs,!_hdIerrors,!_hdIlocals,!_hdIoutput) ->
                           (case (_hdIcontainsVars || _tlIcontainsVars) of
                            { !_lhsOcontainsVars ->
                            (case ((:) _hdIcopy _tlIcopy) of
                             { !_copy ->
                             (case (_copy) of
                              { !_lhsOcopy ->
                              (case (_hdIdefinedAttrs `Set.union` _tlIdefinedAttrs) of
                               { !_lhsOdefinedAttrs ->
                               (case (_hdIerrors Seq.>< _tlIerrors) of
                                { !_lhsOerrors ->
                                (case (_hdIlocals `Set.union` _tlIlocals) of
                                 { !_lhsOlocals ->
                                 (case ((:) _hdIoutput _tlIoutput) of
                                  { !_output ->
                                  (case (_output) of
                                   { !_lhsOoutput ->
                                   ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (\ (!_lhsIcon)
                   (!_lhsInt) ->
                     (case (False) of
                      { !_lhsOcontainsVars ->
                      (case ([]) of
                       { !_copy ->
                       (case (_copy) of
                        { !_lhsOcopy ->
                        (case (Set.empty) of
                         { !_lhsOdefinedAttrs ->
                         (case (Seq.empty) of
                          { !_lhsOerrors ->
                          (case (Set.empty) of
                           { !_lhsOlocals ->
                           (case ([]) of
                            { !_output ->
                            (case (_output) of
                             { !_lhsOoutput ->
                             ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) })) )
-- Production --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cr                   : Bool
         inh                  : Attributes
         manualAttrOrderMap   : AttrOrderMap
         nonterminals         : Set NontermIdent
         nt                   : NontermIdent
         o_rename             : Bool
         syn                  : Attributes
         typeSyns             : TypeSyns
         useMap               : Map Identifier (String,String,String)
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Production:
         child con            : {ConstructorIdent}
         child children       : Children 
         child rules          : Rules 
         child typeSigs       : TypeSigs 
         visit 0:
            local orderDeps   : _
            local orderErrs   : _
            local _tup1       : _
            local errs        : _
            local newRls      : _
-}
-- cata
sem_Production :: Production  ->
                  T_Production 
sem_Production !(Production _con _children _rules _typeSigs )  =
    (sem_Production_Production _con (sem_Children _children ) (sem_Rules _rules ) (sem_TypeSigs _typeSigs ) )
-- semantic domain
newtype T_Production  = T_Production (Bool ->
                                      Attributes ->
                                      AttrOrderMap ->
                                      (Set NontermIdent) ->
                                      NontermIdent ->
                                      Bool ->
                                      Attributes ->
                                      TypeSyns ->
                                      Int ->
                                      (Map Identifier (String,String,String)) ->
                                      ( (Seq Error),Production,Int))
data Inh_Production  = Inh_Production {cr_Inh_Production :: !(Bool),inh_Inh_Production :: !(Attributes),manualAttrOrderMap_Inh_Production :: !(AttrOrderMap),nonterminals_Inh_Production :: !(Set NontermIdent),nt_Inh_Production :: !(NontermIdent),o_rename_Inh_Production :: !(Bool),syn_Inh_Production :: !(Attributes),typeSyns_Inh_Production :: !(TypeSyns),uniq_Inh_Production :: !(Int),useMap_Inh_Production :: !(Map Identifier (String,String,String))}
data Syn_Production  = Syn_Production {errors_Syn_Production :: !(Seq Error),output_Syn_Production :: !(Production),uniq_Syn_Production :: !(Int)}
wrap_Production :: T_Production  ->
                   Inh_Production  ->
                   Syn_Production 
wrap_Production !(T_Production sem ) !(Inh_Production _lhsIcr _lhsIinh _lhsImanualAttrOrderMap _lhsInonterminals _lhsInt _lhsIo_rename _lhsIsyn _lhsItypeSyns _lhsIuniq _lhsIuseMap )  =
    (let ( !_lhsOerrors,!_lhsOoutput,!_lhsOuniq) =
             (sem _lhsIcr _lhsIinh _lhsImanualAttrOrderMap _lhsInonterminals _lhsInt _lhsIo_rename _lhsIsyn _lhsItypeSyns _lhsIuniq _lhsIuseMap )
     in  (Syn_Production _lhsOerrors _lhsOoutput _lhsOuniq ))
sem_Production_Production :: ConstructorIdent ->
                             T_Children  ->
                             T_Rules  ->
                             T_TypeSigs  ->
                             T_Production 
sem_Production_Production !con_ !(T_Children children_ ) !(T_Rules rules_ ) !(T_TypeSigs typeSigs_ )  =
    (T_Production (\ (!_lhsIcr)
                     (!_lhsIinh)
                     (!_lhsImanualAttrOrderMap)
                     (!_lhsInonterminals)
                     (!_lhsInt)
                     (!_lhsIo_rename)
                     (!_lhsIsyn)
                     (!_lhsItypeSyns)
                     (!_lhsIuniq)
                     (!_lhsIuseMap) ->
                       (case (Set.toList $ Map.findWithDefault Set.empty con_ $ Map.findWithDefault Map.empty _lhsInt _lhsImanualAttrOrderMap) of
                        { !_orderDeps ->
                        (case (_lhsIuniq) of
                         { !_rulesOuniq ->
                         (case (_lhsInt) of
                          { !_rulesOnt ->
                          (case (con_) of
                           { !_rulesOcon ->
                           (case ((rules_ _rulesOcon _rulesOnt _rulesOuniq )) of
                            { ( !_rulesIdefinedAttrs,!_rulesIerrors,!_rulesIlocals,!_rulesIoutput,!_rulesIuniq) ->
                            (case (_lhsInt) of
                             { !_childrenOnt ->
                             (case (_lhsIcr) of
                              { !_childrenOcr ->
                              (case (con_) of
                               { !_childrenOcon ->
                               (case ((children_ _childrenOcon _childrenOcr _childrenOnt )) of
                                { ( !_childrenIerrors,!_childrenIfields,!_childrenIinputs,!_childrenIoutput,!_childrenIoutputs) ->
                                (case (let chldOutMap = Map.fromList [ (k, Map.keysSet s) | (k,s) <- _childrenIoutputs ]
                                           chldInMap  = Map.fromList [ (k, Map.keysSet s) | (k,s) <- _childrenIinputs ]
                                           isInAttribute :: Identifier -> Identifier -> [Error]
                                           isInAttribute fld nm
                                              | fld == _LOC = if nm `Set.member` _rulesIlocals
                                                              then []
                                                              else [UndefAttr _lhsInt con_ fld nm False]
                                              | fld == _LHS = if nm `Map.member` _lhsIinh
                                                              then []
                                                              else [UndefAttr _lhsInt con_ fld nm False]
                                              | otherwise   = if nm `Set.member` (Map.findWithDefault Set.empty fld chldOutMap)
                                                              then []
                                                              else [UndefAttr _lhsInt con_ fld nm False]
                                           isOutAttribute :: Identifier -> Identifier -> [Error]
                                           isOutAttribute fld nm
                                              | fld == _LOC = if nm `Set.member` _rulesIlocals
                                                              then []
                                                              else [UndefAttr _lhsInt con_ fld nm True]
                                              | fld == _LHS = if nm `Map.member` _lhsIsyn
                                                              then []
                                                              else [UndefAttr _lhsInt con_ fld nm True]
                                              | otherwise   = if nm `Set.member` (Map.findWithDefault Set.empty fld chldInMap)
                                                              then []
                                                              else [UndefAttr _lhsInt con_ fld nm True]
                                       in Seq.fromList . concat $
                                          [ isInAttribute fldA nmA ++ isOutAttribute fldB nmB
                                          | dep@(Dependency (fldA,nmA) (fldB,nmB)) <- _orderDeps
                                          ]) of
                                 { !_orderErrs ->
                                 (case (let locals       = _rulesIlocals
                                            initenv      = Map.fromList (  [ (a,_ACHILD)
                                                                           | (a,_,_) <- _childrenIfields
                                                                           ]
                                                                        ++ attrs(_LHS, _lhsIinh)
                                                                        ++ [ (a,_LOC)
                                                                           |  a <- Set.toList locals
                                                                           ]
                                                                        )
                                            attrs (n,as) = [ (a,n) | a <- Map.keys as ]
                                            envs       = scanl (flip Map.union)
                                                               initenv
                                                               (map (Map.fromList . attrs ) _childrenIoutputs)
                                            child_envs = init envs
                                            lhs_env    = last envs
                                            (selfAttrs, normalAttrs)
                                              = Map.partition isSELFNonterminal _lhsIsyn
                                            (_,undefAttrs)
                                              = removeDefined _rulesIdefinedAttrs (_LHS, normalAttrs)
                                            (useAttrs,others)
                                              = splitAttrs _lhsIuseMap undefAttrs
                                            (rules1, errors1)
                                              = concatRE $ map (copyRule _lhsInt con_ _lhsIcr locals)
                                                               (zip envs (map (removeDefined _rulesIdefinedAttrs) _childrenIinputs))
                                            uRules
                                              = map (useRule locals _childrenIoutputs) useAttrs
                                            selfLocRules
                                              =  [ selfRule False attr (constructor [childSelf attr nm tp | (nm,tp,ho) <- _childrenIfields, not ho])
                                                 | attr <- Map.keys selfAttrs
                                                 , not (Set.member attr locals)
                                                 ]
                                                 where
                                                   childSelf self nm tp
                                                     = case tp of NT nt _                         -> attrName nm self
                                                                  _      | nm `Set.member` locals -> locname nm
                                                                         | otherwise              -> fieldName nm
                                                   constructor fs
                                                    | getName con_ == "Tuple" && _lhsInt `elem` map fst _lhsItypeSyns
                                                      = "(" ++ concat (List.intersperse "," fs) ++ ")"
                                                    | otherwise
                                                      = getConName _lhsItypeSyns _lhsIo_rename _lhsInt con_ ++ " " ++ unwords fs
                                            selfRules
                                              = [ selfRule True attr undefined
                                                | attr <- Map.keys selfAttrs
                                                , not (Set.member (_LHS,attr) _rulesIdefinedAttrs)
                                                ]
                                            (rules5, errs5)
                                              = copyRule _lhsInt
                                                         con_
                                                         _lhsIcr
                                                         locals
                                                         (lhs_env, (_LHS, others))
                                        in (uRules++selfLocRules++selfRules++rules5++rules1, errors1><errs5)) of
                                  { !__tup1 ->
                                  (case (__tup1) of
                                   { !(_,!_errs) ->
                                   (case (_childrenIerrors >< _errs >< _rulesIerrors >< _orderErrs) of
                                    { !_lhsOerrors ->
                                    (case (__tup1) of
                                     { !(!_newRls,_) ->
                                     (case ((typeSigs_ )) of
                                      { ( !_typeSigsIoutput) ->
                                      (case (Production con_ _childrenIoutput (_rulesIoutput ++ _newRls) _typeSigsIoutput) of
                                       { !_lhsOoutput ->
                                       (case (_rulesIuniq) of
                                        { !_lhsOuniq ->
                                        ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
-- Productions -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cr                   : Bool
         inh                  : Attributes
         manualAttrOrderMap   : AttrOrderMap
         nonterminals         : Set NontermIdent
         nt                   : NontermIdent
         o_rename             : Bool
         syn                  : Attributes
         typeSyns             : TypeSyns
         useMap               : Map Identifier (String,String,String)
      chained attribute:
         uniq                 : Int
      synthesized attributes:
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
newtype T_Productions  = T_Productions (Bool ->
                                        Attributes ->
                                        AttrOrderMap ->
                                        (Set NontermIdent) ->
                                        NontermIdent ->
                                        Bool ->
                                        Attributes ->
                                        TypeSyns ->
                                        Int ->
                                        (Map Identifier (String,String,String)) ->
                                        ( (Seq Error),Productions,Int))
data Inh_Productions  = Inh_Productions {cr_Inh_Productions :: !(Bool),inh_Inh_Productions :: !(Attributes),manualAttrOrderMap_Inh_Productions :: !(AttrOrderMap),nonterminals_Inh_Productions :: !(Set NontermIdent),nt_Inh_Productions :: !(NontermIdent),o_rename_Inh_Productions :: !(Bool),syn_Inh_Productions :: !(Attributes),typeSyns_Inh_Productions :: !(TypeSyns),uniq_Inh_Productions :: !(Int),useMap_Inh_Productions :: !(Map Identifier (String,String,String))}
data Syn_Productions  = Syn_Productions {errors_Syn_Productions :: !(Seq Error),output_Syn_Productions :: !(Productions),uniq_Syn_Productions :: !(Int)}
wrap_Productions :: T_Productions  ->
                    Inh_Productions  ->
                    Syn_Productions 
wrap_Productions !(T_Productions sem ) !(Inh_Productions _lhsIcr _lhsIinh _lhsImanualAttrOrderMap _lhsInonterminals _lhsInt _lhsIo_rename _lhsIsyn _lhsItypeSyns _lhsIuniq _lhsIuseMap )  =
    (let ( !_lhsOerrors,!_lhsOoutput,!_lhsOuniq) =
             (sem _lhsIcr _lhsIinh _lhsImanualAttrOrderMap _lhsInonterminals _lhsInt _lhsIo_rename _lhsIsyn _lhsItypeSyns _lhsIuniq _lhsIuseMap )
     in  (Syn_Productions _lhsOerrors _lhsOoutput _lhsOuniq ))
sem_Productions_Cons :: T_Production  ->
                        T_Productions  ->
                        T_Productions 
sem_Productions_Cons !(T_Production hd_ ) !(T_Productions tl_ )  =
    (T_Productions (\ (!_lhsIcr)
                      (!_lhsIinh)
                      (!_lhsImanualAttrOrderMap)
                      (!_lhsInonterminals)
                      (!_lhsInt)
                      (!_lhsIo_rename)
                      (!_lhsIsyn)
                      (!_lhsItypeSyns)
                      (!_lhsIuniq)
                      (!_lhsIuseMap) ->
                        (case (_lhsIuseMap) of
                         { !_tlOuseMap ->
                         (case (_lhsItypeSyns) of
                          { !_tlOtypeSyns ->
                          (case (_lhsIsyn) of
                           { !_tlOsyn ->
                           (case (_lhsIo_rename) of
                            { !_tlOo_rename ->
                            (case (_lhsInt) of
                             { !_tlOnt ->
                             (case (_lhsImanualAttrOrderMap) of
                              { !_tlOmanualAttrOrderMap ->
                              (case (_lhsIinh) of
                               { !_tlOinh ->
                               (case (_lhsIcr) of
                                { !_tlOcr ->
                                (case (_lhsIuseMap) of
                                 { !_hdOuseMap ->
                                 (case (_lhsItypeSyns) of
                                  { !_hdOtypeSyns ->
                                  (case (_lhsIsyn) of
                                   { !_hdOsyn ->
                                   (case (_lhsIo_rename) of
                                    { !_hdOo_rename ->
                                    (case (_lhsInt) of
                                     { !_hdOnt ->
                                     (case (_lhsImanualAttrOrderMap) of
                                      { !_hdOmanualAttrOrderMap ->
                                      (case (_lhsIinh) of
                                       { !_hdOinh ->
                                       (case (_lhsIcr) of
                                        { !_hdOcr ->
                                        (case (_lhsIuniq) of
                                         { !_hdOuniq ->
                                         (case (_lhsInonterminals) of
                                          { !_hdOnonterminals ->
                                          (case ((hd_ _hdOcr _hdOinh _hdOmanualAttrOrderMap _hdOnonterminals _hdOnt _hdOo_rename _hdOsyn _hdOtypeSyns _hdOuniq _hdOuseMap )) of
                                           { ( !_hdIerrors,!_hdIoutput,!_hdIuniq) ->
                                           (case (_hdIuniq) of
                                            { !_tlOuniq ->
                                            (case (_lhsInonterminals) of
                                             { !_tlOnonterminals ->
                                             (case ((tl_ _tlOcr _tlOinh _tlOmanualAttrOrderMap _tlOnonterminals _tlOnt _tlOo_rename _tlOsyn _tlOtypeSyns _tlOuniq _tlOuseMap )) of
                                              { ( !_tlIerrors,!_tlIoutput,!_tlIuniq) ->
                                              (case (_hdIerrors Seq.>< _tlIerrors) of
                                               { !_lhsOerrors ->
                                               (case ((:) _hdIoutput _tlIoutput) of
                                                { !_output ->
                                                (case (_output) of
                                                 { !_lhsOoutput ->
                                                 (case (_tlIuniq) of
                                                  { !_lhsOuniq ->
                                                  ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Productions_Nil :: T_Productions 
sem_Productions_Nil  =
    (T_Productions (\ (!_lhsIcr)
                      (!_lhsIinh)
                      (!_lhsImanualAttrOrderMap)
                      (!_lhsInonterminals)
                      (!_lhsInt)
                      (!_lhsIo_rename)
                      (!_lhsIsyn)
                      (!_lhsItypeSyns)
                      (!_lhsIuniq)
                      (!_lhsIuseMap) ->
                        (case (Seq.empty) of
                         { !_lhsOerrors ->
                         (case ([]) of
                          { !_output ->
                          (case (_output) of
                           { !_lhsOoutput ->
                           (case (_lhsIuniq) of
                            { !_lhsOuniq ->
                            ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) })) )
-- Rule --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         nt                   : NontermIdent
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         containsVars         : Bool
         definedAttrs         : Set (Identifier,Identifier)
         errors               : Seq Error
         locals               : Set Identifier
         output               : SELF 
         outputs              : Rules 
   alternatives:
      alternative Rule:
         child pattern        : Pattern 
         child rhs            : {Expression}
         child owrt           : {Bool}
         child origin         : {String}
         visit 0:
            local output      : _
            local _tup2       : {(Rules,Int)}
-}
-- cata
sem_Rule :: Rule  ->
            T_Rule 
sem_Rule !(Rule _pattern _rhs _owrt _origin )  =
    (sem_Rule_Rule (sem_Pattern _pattern ) _rhs _owrt _origin )
-- semantic domain
newtype T_Rule  = T_Rule (ConstructorIdent ->
                          NontermIdent ->
                          Int ->
                          ( Bool,(Set (Identifier,Identifier)),(Seq Error),(Set Identifier),Rule,Rules,Int))
data Inh_Rule  = Inh_Rule {con_Inh_Rule :: !(ConstructorIdent),nt_Inh_Rule :: !(NontermIdent),uniq_Inh_Rule :: !(Int)}
data Syn_Rule  = Syn_Rule {containsVars_Syn_Rule :: !(Bool),definedAttrs_Syn_Rule :: !(Set (Identifier,Identifier)),errors_Syn_Rule :: !(Seq Error),locals_Syn_Rule :: !(Set Identifier),output_Syn_Rule :: !(Rule),outputs_Syn_Rule :: !(Rules),uniq_Syn_Rule :: !(Int)}
wrap_Rule :: T_Rule  ->
             Inh_Rule  ->
             Syn_Rule 
wrap_Rule !(T_Rule sem ) !(Inh_Rule _lhsIcon _lhsInt _lhsIuniq )  =
    (let ( !_lhsOcontainsVars,!_lhsOdefinedAttrs,!_lhsOerrors,!_lhsOlocals,!_lhsOoutput,!_lhsOoutputs,!_lhsOuniq) =
             (sem _lhsIcon _lhsInt _lhsIuniq )
     in  (Syn_Rule _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOoutputs _lhsOuniq ))
sem_Rule_Rule :: T_Pattern  ->
                 Expression ->
                 Bool ->
                 String ->
                 T_Rule 
sem_Rule_Rule !(T_Pattern pattern_ ) !rhs_ !owrt_ !origin_  =
    (T_Rule (\ (!_lhsIcon)
               (!_lhsInt)
               (!_lhsIuniq) ->
                 (case (_lhsInt) of
                  { !_patternOnt ->
                  (case (_lhsIcon) of
                   { !_patternOcon ->
                   (case ((pattern_ _patternOcon _patternOnt )) of
                    { ( !_patternIcontainsVars,!_patternIcopy,!_patternIdefinedAttrs,!_patternIerrors,!_patternIlocals,!_patternIoutput) ->
                    (case (_patternIcontainsVars) of
                     { !_lhsOcontainsVars ->
                     (case (_patternIdefinedAttrs) of
                      { !_lhsOdefinedAttrs ->
                      (case (_patternIerrors) of
                       { !_lhsOerrors ->
                       (case (_patternIlocals) of
                        { !_lhsOlocals ->
                        (case (Rule _patternIoutput rhs_ owrt_ origin_) of
                         { !_output ->
                         (case (_output) of
                          { !_lhsOoutput ->
                          (case (multiRule _output     _lhsIuniq) of
                           { !__tup2 ->
                           (case (__tup2) of
                            { !(!_lhsOoutputs,_) ->
                            (case (__tup2) of
                             { !(_,!_lhsOuniq) ->
                             ( _lhsOcontainsVars,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput,_lhsOoutputs,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) })) )
-- Rules -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         nt                   : NontermIdent
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         definedAttrs         : Set (Identifier,Identifier)
         errors               : Seq Error
         locals               : Set Identifier
         output               : SELF 
   alternatives:
      alternative Cons:
         child hd             : Rule 
         child tl             : Rules 
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_Rules :: Rules  ->
             T_Rules 
sem_Rules !list  =
    (Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list) )
-- semantic domain
newtype T_Rules  = T_Rules (ConstructorIdent ->
                            NontermIdent ->
                            Int ->
                            ( (Set (Identifier,Identifier)),(Seq Error),(Set Identifier),Rules,Int))
data Inh_Rules  = Inh_Rules {con_Inh_Rules :: !(ConstructorIdent),nt_Inh_Rules :: !(NontermIdent),uniq_Inh_Rules :: !(Int)}
data Syn_Rules  = Syn_Rules {definedAttrs_Syn_Rules :: !(Set (Identifier,Identifier)),errors_Syn_Rules :: !(Seq Error),locals_Syn_Rules :: !(Set Identifier),output_Syn_Rules :: !(Rules),uniq_Syn_Rules :: !(Int)}
wrap_Rules :: T_Rules  ->
              Inh_Rules  ->
              Syn_Rules 
wrap_Rules !(T_Rules sem ) !(Inh_Rules _lhsIcon _lhsInt _lhsIuniq )  =
    (let ( !_lhsOdefinedAttrs,!_lhsOerrors,!_lhsOlocals,!_lhsOoutput,!_lhsOuniq) =
             (sem _lhsIcon _lhsInt _lhsIuniq )
     in  (Syn_Rules _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOuniq ))
sem_Rules_Cons :: T_Rule  ->
                  T_Rules  ->
                  T_Rules 
sem_Rules_Cons !(T_Rule hd_ ) !(T_Rules tl_ )  =
    (T_Rules (\ (!_lhsIcon)
                (!_lhsInt)
                (!_lhsIuniq) ->
                  (case (_lhsIuniq) of
                   { !_hdOuniq ->
                   (case (_lhsInt) of
                    { !_hdOnt ->
                    (case (_lhsIcon) of
                     { !_hdOcon ->
                     (case ((hd_ _hdOcon _hdOnt _hdOuniq )) of
                      { ( !_hdIcontainsVars,!_hdIdefinedAttrs,!_hdIerrors,!_hdIlocals,!_hdIoutput,!_hdIoutputs,!_hdIuniq) ->
                      (case (_hdIuniq) of
                       { !_tlOuniq ->
                       (case (_lhsInt) of
                        { !_tlOnt ->
                        (case (_lhsIcon) of
                         { !_tlOcon ->
                         (case ((tl_ _tlOcon _tlOnt _tlOuniq )) of
                          { ( !_tlIdefinedAttrs,!_tlIerrors,!_tlIlocals,!_tlIoutput,!_tlIuniq) ->
                          (case (_hdIdefinedAttrs `Set.union` _tlIdefinedAttrs) of
                           { !_lhsOdefinedAttrs ->
                           (case (_hdIerrors Seq.>< _tlIerrors) of
                            { !_lhsOerrors ->
                            (case (_hdIlocals `Set.union` _tlIlocals) of
                             { !_lhsOlocals ->
                             (case (if _hdIcontainsVars then _hdIoutputs ++ _tlIoutput else _tlIoutput) of
                              { !_lhsOoutput ->
                              (case (_tlIuniq) of
                               { !_lhsOuniq ->
                               ( _lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Rules_Nil :: T_Rules 
sem_Rules_Nil  =
    (T_Rules (\ (!_lhsIcon)
                (!_lhsInt)
                (!_lhsIuniq) ->
                  (case (Set.empty) of
                   { !_lhsOdefinedAttrs ->
                   (case (Seq.empty) of
                    { !_lhsOerrors ->
                    (case (Set.empty) of
                     { !_lhsOlocals ->
                     (case ([]) of
                      { !_output ->
                      (case (_output) of
                       { !_lhsOoutput ->
                       (case (_lhsIuniq) of
                        { !_lhsOuniq ->
                        ( _lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput,_lhsOuniq) }) }) }) }) }) })) )
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