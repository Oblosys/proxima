

-- UUAGC 0.9.10 (CodeSyntaxDump.ag)
module CodeSyntaxDump where

import Data.List
import qualified Data.Map as Map

import Pretty
import PPUtil

import CodeSyntax


import Patterns
import CommonTypes
import Data.Map(Map)
import Data.Set(Set)


-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)

ppChild :: (Identifier,Type,Bool) -> PP_Doc
ppChild (nm,tp,b)
  = pp nm >#< "::" >#< pp (show tp) >#< "<" >|< ppBool b >|< ">"
  
ppVertexMap :: Map Int (Identifier,Identifier,Maybe Type) -> PP_Doc
ppVertexMap m
  = ppVList [ ppF (show k) $ ppAttr v | (k,v) <- Map.toList m ]

ppAttr :: (Identifier,Identifier,Maybe Type) -> PP_Doc
ppAttr (fld,nm,mTp)
  = pp fld >|< "." >|< pp nm >#<
    case mTp of
      Just tp -> pp "::" >#< show tp
      Nothing -> empty

ppBool :: Bool -> PP_Doc
ppBool True  = pp "T"
ppBool False = pp "F"

ppMaybeShow :: Show a => Maybe a -> PP_Doc
ppMaybeShow (Just x) = pp (show x)
ppMaybeShow Nothing  = pp "_"

ppStrings :: [String] -> PP_Doc
ppStrings = vlist
-- CGrammar ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative CGrammar:
         child typeSyns       : {TypeSyns}
         child derivings      : {Derivings}
         child wrappers       : {Set NontermIdent}
         child nonts          : CNonterminals 
         child pragmas        : {PragmaMap}
         child paramMap       : {ParamMap}
         child contextMap     : {ContextMap}
-}
-- cata
sem_CGrammar :: CGrammar  ->
                T_CGrammar 
sem_CGrammar (CGrammar _typeSyns _derivings _wrappers _nonts _pragmas _paramMap _contextMap )  =
    (sem_CGrammar_CGrammar _typeSyns _derivings _wrappers (sem_CNonterminals _nonts ) _pragmas _paramMap _contextMap )
-- semantic domain
newtype T_CGrammar  = T_CGrammar (( PP_Doc))
data Inh_CGrammar  = Inh_CGrammar {}
data Syn_CGrammar  = Syn_CGrammar {pp_Syn_CGrammar :: PP_Doc}
wrap_CGrammar :: T_CGrammar  ->
                 Inh_CGrammar  ->
                 Syn_CGrammar 
wrap_CGrammar (T_CGrammar sem ) (Inh_CGrammar )  =
    (let ( _lhsOpp) =
             (sem )
     in  (Syn_CGrammar _lhsOpp ))
sem_CGrammar_CGrammar :: TypeSyns ->
                         Derivings ->
                         (Set NontermIdent) ->
                         T_CNonterminals  ->
                         PragmaMap ->
                         ParamMap ->
                         ContextMap ->
                         T_CGrammar 
sem_CGrammar_CGrammar typeSyns_ derivings_ wrappers_ (T_CNonterminals nonts_ ) pragmas_ paramMap_ contextMap_  =
    (T_CGrammar (let _lhsOpp :: PP_Doc
                     _nontsIpp :: PP_Doc
                     _nontsIppL :: ([PP_Doc])
                     -- "CodeSyntaxDump.ag"(line 47, column 21)
                     _lhsOpp =
                         ppNestInfo ["CGrammar","CGrammar"] []
                                    [ ppF "typeSyns"  $ ppAssocL typeSyns_
                                    , ppF "derivings" $ ppMap $ derivings_
                                    , ppF "nonts"     $ ppVList _nontsIppL
                                    ] []
                     ( _nontsIpp,_nontsIppL) =
                         (nonts_ )
                 in  ( _lhsOpp)) )
-- CInterface --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative CInterface:
         child seg            : CSegments 
-}
-- cata
sem_CInterface :: CInterface  ->
                  T_CInterface 
sem_CInterface (CInterface _seg )  =
    (sem_CInterface_CInterface (sem_CSegments _seg ) )
-- semantic domain
newtype T_CInterface  = T_CInterface (( PP_Doc))
data Inh_CInterface  = Inh_CInterface {}
data Syn_CInterface  = Syn_CInterface {pp_Syn_CInterface :: PP_Doc}
wrap_CInterface :: T_CInterface  ->
                   Inh_CInterface  ->
                   Syn_CInterface 
wrap_CInterface (T_CInterface sem ) (Inh_CInterface )  =
    (let ( _lhsOpp) =
             (sem )
     in  (Syn_CInterface _lhsOpp ))
sem_CInterface_CInterface :: T_CSegments  ->
                             T_CInterface 
sem_CInterface_CInterface (T_CSegments seg_ )  =
    (T_CInterface (let _lhsOpp :: PP_Doc
                       _segIpp :: PP_Doc
                       _segIppL :: ([PP_Doc])
                       -- "CodeSyntaxDump.ag"(line 57, column 21)
                       _lhsOpp =
                           ppNestInfo ["CInterface","CInterface"] [] [ppF "seg" $ ppVList _segIppL] []
                       ( _segIpp,_segIppL) =
                           (seg_ )
                   in  ( _lhsOpp)) )
-- CNonterminal ------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative CNonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : CProductions 
         child inter          : CInterface 
-}
-- cata
sem_CNonterminal :: CNonterminal  ->
                    T_CNonterminal 
sem_CNonterminal (CNonterminal _nt _params _inh _syn _prods _inter )  =
    (sem_CNonterminal_CNonterminal _nt _params _inh _syn (sem_CProductions _prods ) (sem_CInterface _inter ) )
-- semantic domain
newtype T_CNonterminal  = T_CNonterminal (( PP_Doc))
data Inh_CNonterminal  = Inh_CNonterminal {}
data Syn_CNonterminal  = Syn_CNonterminal {pp_Syn_CNonterminal :: PP_Doc}
wrap_CNonterminal :: T_CNonterminal  ->
                     Inh_CNonterminal  ->
                     Syn_CNonterminal 
wrap_CNonterminal (T_CNonterminal sem ) (Inh_CNonterminal )  =
    (let ( _lhsOpp) =
             (sem )
     in  (Syn_CNonterminal _lhsOpp ))
sem_CNonterminal_CNonterminal :: NontermIdent ->
                                 ([Identifier]) ->
                                 Attributes ->
                                 Attributes ->
                                 T_CProductions  ->
                                 T_CInterface  ->
                                 T_CNonterminal 
sem_CNonterminal_CNonterminal nt_ params_ inh_ syn_ (T_CProductions prods_ ) (T_CInterface inter_ )  =
    (T_CNonterminal (let _lhsOpp :: PP_Doc
                         _prodsIpp :: PP_Doc
                         _prodsIppL :: ([PP_Doc])
                         _interIpp :: PP_Doc
                         -- "CodeSyntaxDump.ag"(line 54, column 33)
                         _lhsOpp =
                             ppNestInfo ["CNonterminal","CNonterminal"] (pp nt_ : map pp params_) [ppF "inh" $ ppMap inh_, ppF "syn" $ ppMap syn_, ppF "prods" $ ppVList _prodsIppL, ppF "inter" _interIpp] []
                         ( _prodsIpp,_prodsIppL) =
                             (prods_ )
                         ( _interIpp) =
                             (inter_ )
                     in  ( _lhsOpp)) )
-- CNonterminals -----------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : CNonterminal 
         child tl             : CNonterminals 
      alternative Nil:
-}
-- cata
sem_CNonterminals :: CNonterminals  ->
                     T_CNonterminals 
sem_CNonterminals list  =
    (Prelude.foldr sem_CNonterminals_Cons sem_CNonterminals_Nil (Prelude.map sem_CNonterminal list) )
-- semantic domain
newtype T_CNonterminals  = T_CNonterminals (( PP_Doc,([PP_Doc])))
data Inh_CNonterminals  = Inh_CNonterminals {}
data Syn_CNonterminals  = Syn_CNonterminals {pp_Syn_CNonterminals :: PP_Doc,ppL_Syn_CNonterminals :: [PP_Doc]}
wrap_CNonterminals :: T_CNonterminals  ->
                      Inh_CNonterminals  ->
                      Syn_CNonterminals 
wrap_CNonterminals (T_CNonterminals sem ) (Inh_CNonterminals )  =
    (let ( _lhsOpp,_lhsOppL) =
             (sem )
     in  (Syn_CNonterminals _lhsOpp _lhsOppL ))
sem_CNonterminals_Cons :: T_CNonterminal  ->
                          T_CNonterminals  ->
                          T_CNonterminals 
sem_CNonterminals_Cons (T_CNonterminal hd_ ) (T_CNonterminals tl_ )  =
    (T_CNonterminals (let _lhsOppL :: ([PP_Doc])
                          _lhsOpp :: PP_Doc
                          _hdIpp :: PP_Doc
                          _tlIpp :: PP_Doc
                          _tlIppL :: ([PP_Doc])
                          -- "CodeSyntaxDump.ag"(line 102, column 33)
                          _lhsOppL =
                              _hdIpp : _tlIppL
                          -- use rule "CodeSyntaxDump.ag"(line 44, column 40)
                          _lhsOpp =
                              _hdIpp >-< _tlIpp
                          ( _hdIpp) =
                              (hd_ )
                          ( _tlIpp,_tlIppL) =
                              (tl_ )
                      in  ( _lhsOpp,_lhsOppL)) )
sem_CNonterminals_Nil :: T_CNonterminals 
sem_CNonterminals_Nil  =
    (T_CNonterminals (let _lhsOppL :: ([PP_Doc])
                          _lhsOpp :: PP_Doc
                          -- "CodeSyntaxDump.ag"(line 103, column 33)
                          _lhsOppL =
                              []
                          -- use rule "CodeSyntaxDump.ag"(line 44, column 40)
                          _lhsOpp =
                              empty
                      in  ( _lhsOpp,_lhsOppL)) )
-- CProduction -------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative CProduction:
         child con            : {ConstructorIdent}
         child visits         : CVisits 
         child children       : {[(Identifier,Type,Bool)]}
         child terminals      : {[Identifier]}
-}
-- cata
sem_CProduction :: CProduction  ->
                   T_CProduction 
sem_CProduction (CProduction _con _visits _children _terminals )  =
    (sem_CProduction_CProduction _con (sem_CVisits _visits ) _children _terminals )
-- semantic domain
newtype T_CProduction  = T_CProduction (( PP_Doc))
data Inh_CProduction  = Inh_CProduction {}
data Syn_CProduction  = Syn_CProduction {pp_Syn_CProduction :: PP_Doc}
wrap_CProduction :: T_CProduction  ->
                    Inh_CProduction  ->
                    Syn_CProduction 
wrap_CProduction (T_CProduction sem ) (Inh_CProduction )  =
    (let ( _lhsOpp) =
             (sem )
     in  (Syn_CProduction _lhsOpp ))
sem_CProduction_CProduction :: ConstructorIdent ->
                               T_CVisits  ->
                               ([(Identifier,Type,Bool)]) ->
                               ([Identifier]) ->
                               T_CProduction 
sem_CProduction_CProduction con_ (T_CVisits visits_ ) children_ terminals_  =
    (T_CProduction (let _lhsOpp :: PP_Doc
                        _visitsIpp :: PP_Doc
                        _visitsIppL :: ([PP_Doc])
                        -- "CodeSyntaxDump.ag"(line 63, column 17)
                        _lhsOpp =
                            ppNestInfo ["CProduction","CProduction"] [pp con_] [ppF "visits" $ ppVList _visitsIppL, ppF "children" $ ppVList (map ppChild children_),ppF "terminals" $ ppVList (map ppShow terminals_)] []
                        ( _visitsIpp,_visitsIppL) =
                            (visits_ )
                    in  ( _lhsOpp)) )
-- CProductions ------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : CProduction 
         child tl             : CProductions 
      alternative Nil:
-}
-- cata
sem_CProductions :: CProductions  ->
                    T_CProductions 
sem_CProductions list  =
    (Prelude.foldr sem_CProductions_Cons sem_CProductions_Nil (Prelude.map sem_CProduction list) )
-- semantic domain
newtype T_CProductions  = T_CProductions (( PP_Doc,([PP_Doc])))
data Inh_CProductions  = Inh_CProductions {}
data Syn_CProductions  = Syn_CProductions {pp_Syn_CProductions :: PP_Doc,ppL_Syn_CProductions :: [PP_Doc]}
wrap_CProductions :: T_CProductions  ->
                     Inh_CProductions  ->
                     Syn_CProductions 
wrap_CProductions (T_CProductions sem ) (Inh_CProductions )  =
    (let ( _lhsOpp,_lhsOppL) =
             (sem )
     in  (Syn_CProductions _lhsOpp _lhsOppL ))
sem_CProductions_Cons :: T_CProduction  ->
                         T_CProductions  ->
                         T_CProductions 
sem_CProductions_Cons (T_CProduction hd_ ) (T_CProductions tl_ )  =
    (T_CProductions (let _lhsOppL :: ([PP_Doc])
                         _lhsOpp :: PP_Doc
                         _hdIpp :: PP_Doc
                         _tlIpp :: PP_Doc
                         _tlIppL :: ([PP_Doc])
                         -- "CodeSyntaxDump.ag"(line 94, column 33)
                         _lhsOppL =
                             _hdIpp : _tlIppL
                         -- use rule "CodeSyntaxDump.ag"(line 44, column 40)
                         _lhsOpp =
                             _hdIpp >-< _tlIpp
                         ( _hdIpp) =
                             (hd_ )
                         ( _tlIpp,_tlIppL) =
                             (tl_ )
                     in  ( _lhsOpp,_lhsOppL)) )
sem_CProductions_Nil :: T_CProductions 
sem_CProductions_Nil  =
    (T_CProductions (let _lhsOppL :: ([PP_Doc])
                         _lhsOpp :: PP_Doc
                         -- "CodeSyntaxDump.ag"(line 95, column 33)
                         _lhsOppL =
                             []
                         -- use rule "CodeSyntaxDump.ag"(line 44, column 40)
                         _lhsOpp =
                             empty
                     in  ( _lhsOpp,_lhsOppL)) )
-- CRule -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative CChildVisit:
         child name           : {Identifier}
         child nt             : {NontermIdent}
         child nr             : {Int}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child isLast         : {Bool}
      alternative CRule:
         child name           : {Identifier}
         child isIn           : {Bool}
         child hasCode        : {Bool}
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child childnt        : {Maybe NontermIdent}
         child tp             : {Maybe Type}
         child pattern        : Pattern 
         child rhs            : {[String]}
         child defines        : {Map Int (Identifier,Identifier,Maybe Type)}
         child owrt           : {Bool}
         child origin         : {String}
         child uses           : {Set (Identifier, Identifier)}
-}
-- cata
sem_CRule :: CRule  ->
             T_CRule 
sem_CRule (CChildVisit _name _nt _nr _inh _syn _isLast )  =
    (sem_CRule_CChildVisit _name _nt _nr _inh _syn _isLast )
sem_CRule (CRule _name _isIn _hasCode _nt _con _field _childnt _tp _pattern _rhs _defines _owrt _origin _uses )  =
    (sem_CRule_CRule _name _isIn _hasCode _nt _con _field _childnt _tp (sem_Pattern _pattern ) _rhs _defines _owrt _origin _uses )
-- semantic domain
newtype T_CRule  = T_CRule (( PP_Doc))
data Inh_CRule  = Inh_CRule {}
data Syn_CRule  = Syn_CRule {pp_Syn_CRule :: PP_Doc}
wrap_CRule :: T_CRule  ->
              Inh_CRule  ->
              Syn_CRule 
wrap_CRule (T_CRule sem ) (Inh_CRule )  =
    (let ( _lhsOpp) =
             (sem )
     in  (Syn_CRule _lhsOpp ))
sem_CRule_CChildVisit :: Identifier ->
                         NontermIdent ->
                         Int ->
                         Attributes ->
                         Attributes ->
                         Bool ->
                         T_CRule 
sem_CRule_CChildVisit name_ nt_ nr_ inh_ syn_ isLast_  =
    (T_CRule (let _lhsOpp :: PP_Doc
                  -- "CodeSyntaxDump.ag"(line 70, column 21)
                  _lhsOpp =
                      ppNestInfo ["CRule","CChildVisit"] [pp name_] [ppF "nt" $ pp nt_, ppF "nr" $ ppShow nr_, ppF "inh" $ ppMap inh_, ppF "syn" $ ppMap syn_, ppF "last" $ ppBool isLast_] []
              in  ( _lhsOpp)) )
sem_CRule_CRule :: Identifier ->
                   Bool ->
                   Bool ->
                   NontermIdent ->
                   ConstructorIdent ->
                   Identifier ->
                   (Maybe NontermIdent) ->
                   (Maybe Type) ->
                   T_Pattern  ->
                   ([String]) ->
                   (Map Int (Identifier,Identifier,Maybe Type)) ->
                   Bool ->
                   String ->
                   (Set (Identifier, Identifier)) ->
                   T_CRule 
sem_CRule_CRule name_ isIn_ hasCode_ nt_ con_ field_ childnt_ tp_ (T_Pattern pattern_ ) rhs_ defines_ owrt_ origin_ uses_  =
    (T_CRule (let _lhsOpp :: PP_Doc
                  _patternIcopy :: Pattern
                  _patternIpp :: PP_Doc
                  -- "CodeSyntaxDump.ag"(line 69, column 33)
                  _lhsOpp =
                      ppNestInfo ["CRule","CRule"] [pp name_] [ppF "isIn" $ ppBool isIn_, ppF "hasCode" $ ppBool hasCode_, ppF "nt" $ pp nt_, ppF "con" $ pp con_, ppF "field" $ pp field_, ppF "childnt" $ ppMaybeShow childnt_, ppF "tp" $ ppMaybeShow tp_, ppF "pattern" $ if isIn_ then pp "<no pat because In>" else _patternIpp, ppF "rhs" $ ppStrings rhs_, ppF "defines" $ ppVertexMap defines_, ppF "owrt" $ ppBool owrt_, ppF "origin" $ pp origin_] []
                  ( _patternIcopy,_patternIpp) =
                      (pattern_ )
              in  ( _lhsOpp)) )
-- CSegment ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative CSegment:
         child inh            : {Attributes}
         child syn            : {Attributes}
-}
-- cata
sem_CSegment :: CSegment  ->
                T_CSegment 
sem_CSegment (CSegment _inh _syn )  =
    (sem_CSegment_CSegment _inh _syn )
-- semantic domain
newtype T_CSegment  = T_CSegment (( PP_Doc))
data Inh_CSegment  = Inh_CSegment {}
data Syn_CSegment  = Syn_CSegment {pp_Syn_CSegment :: PP_Doc}
wrap_CSegment :: T_CSegment  ->
                 Inh_CSegment  ->
                 Syn_CSegment 
wrap_CSegment (T_CSegment sem ) (Inh_CSegment )  =
    (let ( _lhsOpp) =
             (sem )
     in  (Syn_CSegment _lhsOpp ))
sem_CSegment_CSegment :: Attributes ->
                         Attributes ->
                         T_CSegment 
sem_CSegment_CSegment inh_ syn_  =
    (T_CSegment (let _lhsOpp :: PP_Doc
                     -- "CodeSyntaxDump.ag"(line 60, column 21)
                     _lhsOpp =
                         ppNestInfo ["CSegment","CSegment"] [] [ppF "inh" $ ppMap inh_, ppF "syn" $ ppMap syn_] []
                 in  ( _lhsOpp)) )
-- CSegments ---------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : CSegment 
         child tl             : CSegments 
      alternative Nil:
-}
-- cata
sem_CSegments :: CSegments  ->
                 T_CSegments 
sem_CSegments list  =
    (Prelude.foldr sem_CSegments_Cons sem_CSegments_Nil (Prelude.map sem_CSegment list) )
-- semantic domain
newtype T_CSegments  = T_CSegments (( PP_Doc,([PP_Doc])))
data Inh_CSegments  = Inh_CSegments {}
data Syn_CSegments  = Syn_CSegments {pp_Syn_CSegments :: PP_Doc,ppL_Syn_CSegments :: [PP_Doc]}
wrap_CSegments :: T_CSegments  ->
                  Inh_CSegments  ->
                  Syn_CSegments 
wrap_CSegments (T_CSegments sem ) (Inh_CSegments )  =
    (let ( _lhsOpp,_lhsOppL) =
             (sem )
     in  (Syn_CSegments _lhsOpp _lhsOppL ))
sem_CSegments_Cons :: T_CSegment  ->
                      T_CSegments  ->
                      T_CSegments 
sem_CSegments_Cons (T_CSegment hd_ ) (T_CSegments tl_ )  =
    (T_CSegments (let _lhsOppL :: ([PP_Doc])
                      _lhsOpp :: PP_Doc
                      _hdIpp :: PP_Doc
                      _tlIpp :: PP_Doc
                      _tlIppL :: ([PP_Doc])
                      -- "CodeSyntaxDump.ag"(line 98, column 33)
                      _lhsOppL =
                          _hdIpp : _tlIppL
                      -- use rule "CodeSyntaxDump.ag"(line 44, column 40)
                      _lhsOpp =
                          _hdIpp >-< _tlIpp
                      ( _hdIpp) =
                          (hd_ )
                      ( _tlIpp,_tlIppL) =
                          (tl_ )
                  in  ( _lhsOpp,_lhsOppL)) )
sem_CSegments_Nil :: T_CSegments 
sem_CSegments_Nil  =
    (T_CSegments (let _lhsOppL :: ([PP_Doc])
                      _lhsOpp :: PP_Doc
                      -- "CodeSyntaxDump.ag"(line 99, column 33)
                      _lhsOppL =
                          []
                      -- use rule "CodeSyntaxDump.ag"(line 44, column 40)
                      _lhsOpp =
                          empty
                  in  ( _lhsOpp,_lhsOppL)) )
-- CVisit ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative CVisit:
         child inh            : {Attributes}
         child syn            : {Attributes}
         child vss            : Sequence 
         child intra          : Sequence 
         child ordered        : {Bool}
-}
-- cata
sem_CVisit :: CVisit  ->
              T_CVisit 
sem_CVisit (CVisit _inh _syn _vss _intra _ordered )  =
    (sem_CVisit_CVisit _inh _syn (sem_Sequence _vss ) (sem_Sequence _intra ) _ordered )
-- semantic domain
newtype T_CVisit  = T_CVisit (( PP_Doc))
data Inh_CVisit  = Inh_CVisit {}
data Syn_CVisit  = Syn_CVisit {pp_Syn_CVisit :: PP_Doc}
wrap_CVisit :: T_CVisit  ->
               Inh_CVisit  ->
               Syn_CVisit 
wrap_CVisit (T_CVisit sem ) (Inh_CVisit )  =
    (let ( _lhsOpp) =
             (sem )
     in  (Syn_CVisit _lhsOpp ))
sem_CVisit_CVisit :: Attributes ->
                     Attributes ->
                     T_Sequence  ->
                     T_Sequence  ->
                     Bool ->
                     T_CVisit 
sem_CVisit_CVisit inh_ syn_ (T_Sequence vss_ ) (T_Sequence intra_ ) ordered_  =
    (T_CVisit (let _lhsOpp :: PP_Doc
                   _vssIppL :: ([PP_Doc])
                   _intraIppL :: ([PP_Doc])
                   -- "CodeSyntaxDump.ag"(line 66, column 21)
                   _lhsOpp =
                       ppNestInfo ["CVisit","CVisit"] [] [ppF "inh" $ ppMap inh_, ppF "syn" $ ppMap syn_, ppF "sequence" $ ppVList _vssIppL, ppF "intra" $ ppVList _intraIppL, ppF "ordered" $ ppBool ordered_] []
                   ( _vssIppL) =
                       (vss_ )
                   ( _intraIppL) =
                       (intra_ )
               in  ( _lhsOpp)) )
-- CVisits -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : CVisit 
         child tl             : CVisits 
      alternative Nil:
-}
-- cata
sem_CVisits :: CVisits  ->
               T_CVisits 
sem_CVisits list  =
    (Prelude.foldr sem_CVisits_Cons sem_CVisits_Nil (Prelude.map sem_CVisit list) )
-- semantic domain
newtype T_CVisits  = T_CVisits (( PP_Doc,([PP_Doc])))
data Inh_CVisits  = Inh_CVisits {}
data Syn_CVisits  = Syn_CVisits {pp_Syn_CVisits :: PP_Doc,ppL_Syn_CVisits :: [PP_Doc]}
wrap_CVisits :: T_CVisits  ->
                Inh_CVisits  ->
                Syn_CVisits 
wrap_CVisits (T_CVisits sem ) (Inh_CVisits )  =
    (let ( _lhsOpp,_lhsOppL) =
             (sem )
     in  (Syn_CVisits _lhsOpp _lhsOppL ))
sem_CVisits_Cons :: T_CVisit  ->
                    T_CVisits  ->
                    T_CVisits 
sem_CVisits_Cons (T_CVisit hd_ ) (T_CVisits tl_ )  =
    (T_CVisits (let _lhsOppL :: ([PP_Doc])
                    _lhsOpp :: PP_Doc
                    _hdIpp :: PP_Doc
                    _tlIpp :: PP_Doc
                    _tlIppL :: ([PP_Doc])
                    -- "CodeSyntaxDump.ag"(line 90, column 33)
                    _lhsOppL =
                        _hdIpp : _tlIppL
                    -- use rule "CodeSyntaxDump.ag"(line 44, column 40)
                    _lhsOpp =
                        _hdIpp >-< _tlIpp
                    ( _hdIpp) =
                        (hd_ )
                    ( _tlIpp,_tlIppL) =
                        (tl_ )
                in  ( _lhsOpp,_lhsOppL)) )
sem_CVisits_Nil :: T_CVisits 
sem_CVisits_Nil  =
    (T_CVisits (let _lhsOppL :: ([PP_Doc])
                    _lhsOpp :: PP_Doc
                    -- "CodeSyntaxDump.ag"(line 91, column 33)
                    _lhsOppL =
                        []
                    -- use rule "CodeSyntaxDump.ag"(line 44, column 40)
                    _lhsOpp =
                        empty
                in  ( _lhsOpp,_lhsOppL)) )
-- Pattern -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         pp                   : PP_Doc
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         child parts          : Patterns 
         visit 0:
            local copy        : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
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
newtype T_Pattern  = T_Pattern (( Pattern,PP_Doc))
data Inh_Pattern  = Inh_Pattern {}
data Syn_Pattern  = Syn_Pattern {copy_Syn_Pattern :: Pattern,pp_Syn_Pattern :: PP_Doc}
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern (T_Pattern sem ) (Inh_Pattern )  =
    (let ( _lhsOcopy,_lhsOpp) =
             (sem )
     in  (Syn_Pattern _lhsOcopy _lhsOpp ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Alias field_ attr_ (T_Pattern pat_ ) (T_Patterns parts_ )  =
    (T_Pattern (let _lhsOpp :: PP_Doc
                    _lhsOcopy :: Pattern
                    _patIcopy :: Pattern
                    _patIpp :: PP_Doc
                    _partsIcopy :: Patterns
                    _partsIpp :: PP_Doc
                    _partsIppL :: ([PP_Doc])
                    -- "CodeSyntaxDump.ag"(line 75, column 33)
                    _lhsOpp =
                        ppNestInfo ["Pattern","Alias"] [pp field_, pp attr_] [ppF "pat" $ _patIpp] []
                    -- self rule
                    _copy =
                        Alias field_ attr_ _patIcopy _partsIcopy
                    -- self rule
                    _lhsOcopy =
                        _copy
                    ( _patIcopy,_patIpp) =
                        (pat_ )
                    ( _partsIcopy,_partsIpp,_partsIppL) =
                        (parts_ )
                in  ( _lhsOcopy,_lhsOpp)) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr name_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOpp :: PP_Doc
                    _lhsOcopy :: Pattern
                    _patsIcopy :: Patterns
                    _patsIpp :: PP_Doc
                    _patsIppL :: ([PP_Doc])
                    -- "CodeSyntaxDump.ag"(line 73, column 33)
                    _lhsOpp =
                        ppNestInfo ["Pattern","Constr"] [pp name_] [ppF "pats" $ ppVList _patsIppL] []
                    -- self rule
                    _copy =
                        Constr name_ _patsIcopy
                    -- self rule
                    _lhsOcopy =
                        _copy
                    ( _patsIcopy,_patsIpp,_patsIppL) =
                        (pats_ )
                in  ( _lhsOcopy,_lhsOpp)) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable (T_Pattern pat_ )  =
    (T_Pattern (let _lhsOpp :: PP_Doc
                    _lhsOcopy :: Pattern
                    _patIcopy :: Pattern
                    _patIpp :: PP_Doc
                    -- use rule "CodeSyntaxDump.ag"(line 44, column 40)
                    _lhsOpp =
                        _patIpp
                    -- self rule
                    _copy =
                        Irrefutable _patIcopy
                    -- self rule
                    _lhsOcopy =
                        _copy
                    ( _patIcopy,_patIpp) =
                        (pat_ )
                in  ( _lhsOcopy,_lhsOpp)) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product pos_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOpp :: PP_Doc
                    _lhsOcopy :: Pattern
                    _patsIcopy :: Patterns
                    _patsIpp :: PP_Doc
                    _patsIppL :: ([PP_Doc])
                    -- "CodeSyntaxDump.ag"(line 74, column 33)
                    _lhsOpp =
                        ppNestInfo ["Pattern","Product"] [ppShow pos_] [ppF "pats" $ ppVList _patsIppL] []
                    -- self rule
                    _copy =
                        Product pos_ _patsIcopy
                    -- self rule
                    _lhsOcopy =
                        _copy
                    ( _patsIcopy,_patsIpp,_patsIppL) =
                        (pats_ )
                in  ( _lhsOcopy,_lhsOpp)) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore pos_  =
    (T_Pattern (let _lhsOpp :: PP_Doc
                    _lhsOcopy :: Pattern
                    -- "CodeSyntaxDump.ag"(line 76, column 25)
                    _lhsOpp =
                        ppNestInfo ["Pattern","Underscore"] [ppShow pos_] [] []
                    -- self rule
                    _copy =
                        Underscore pos_
                    -- self rule
                    _lhsOcopy =
                        _copy
                in  ( _lhsOcopy,_lhsOpp)) )
-- Patterns ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
-- cata
sem_Patterns :: Patterns  ->
                T_Patterns 
sem_Patterns list  =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list) )
-- semantic domain
newtype T_Patterns  = T_Patterns (( Patterns,PP_Doc,([PP_Doc])))
data Inh_Patterns  = Inh_Patterns {}
data Syn_Patterns  = Syn_Patterns {copy_Syn_Patterns :: Patterns,pp_Syn_Patterns :: PP_Doc,ppL_Syn_Patterns :: [PP_Doc]}
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns (T_Patterns sem ) (Inh_Patterns )  =
    (let ( _lhsOcopy,_lhsOpp,_lhsOppL) =
             (sem )
     in  (Syn_Patterns _lhsOcopy _lhsOpp _lhsOppL ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons (T_Pattern hd_ ) (T_Patterns tl_ )  =
    (T_Patterns (let _lhsOppL :: ([PP_Doc])
                     _lhsOpp :: PP_Doc
                     _lhsOcopy :: Patterns
                     _hdIcopy :: Pattern
                     _hdIpp :: PP_Doc
                     _tlIcopy :: Patterns
                     _tlIpp :: PP_Doc
                     _tlIppL :: ([PP_Doc])
                     -- "CodeSyntaxDump.ag"(line 82, column 33)
                     _lhsOppL =
                         _hdIpp : _tlIppL
                     -- use rule "CodeSyntaxDump.ag"(line 44, column 40)
                     _lhsOpp =
                         _hdIpp >-< _tlIpp
                     -- self rule
                     _copy =
                         (:) _hdIcopy _tlIcopy
                     -- self rule
                     _lhsOcopy =
                         _copy
                     ( _hdIcopy,_hdIpp) =
                         (hd_ )
                     ( _tlIcopy,_tlIpp,_tlIppL) =
                         (tl_ )
                 in  ( _lhsOcopy,_lhsOpp,_lhsOppL)) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (let _lhsOppL :: ([PP_Doc])
                     _lhsOpp :: PP_Doc
                     _lhsOcopy :: Patterns
                     -- "CodeSyntaxDump.ag"(line 83, column 33)
                     _lhsOppL =
                         []
                     -- use rule "CodeSyntaxDump.ag"(line 44, column 40)
                     _lhsOpp =
                         empty
                     -- self rule
                     _copy =
                         []
                     -- self rule
                     _lhsOcopy =
                         _copy
                 in  ( _lhsOcopy,_lhsOpp,_lhsOppL)) )
-- Sequence ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : CRule 
         child tl             : Sequence 
      alternative Nil:
-}
-- cata
sem_Sequence :: Sequence  ->
                T_Sequence 
sem_Sequence list  =
    (Prelude.foldr sem_Sequence_Cons sem_Sequence_Nil (Prelude.map sem_CRule list) )
-- semantic domain
newtype T_Sequence  = T_Sequence (( ([PP_Doc])))
data Inh_Sequence  = Inh_Sequence {}
data Syn_Sequence  = Syn_Sequence {ppL_Syn_Sequence :: [PP_Doc]}
wrap_Sequence :: T_Sequence  ->
                 Inh_Sequence  ->
                 Syn_Sequence 
wrap_Sequence (T_Sequence sem ) (Inh_Sequence )  =
    (let ( _lhsOppL) =
             (sem )
     in  (Syn_Sequence _lhsOppL ))
sem_Sequence_Cons :: T_CRule  ->
                     T_Sequence  ->
                     T_Sequence 
sem_Sequence_Cons (T_CRule hd_ ) (T_Sequence tl_ )  =
    (T_Sequence (let _lhsOppL :: ([PP_Doc])
                     _hdIpp :: PP_Doc
                     _tlIppL :: ([PP_Doc])
                     -- "CodeSyntaxDump.ag"(line 86, column 33)
                     _lhsOppL =
                         _hdIpp : _tlIppL
                     ( _hdIpp) =
                         (hd_ )
                     ( _tlIppL) =
                         (tl_ )
                 in  ( _lhsOppL)) )
sem_Sequence_Nil :: T_Sequence 
sem_Sequence_Nil  =
    (T_Sequence (let _lhsOppL :: ([PP_Doc])
                     -- "CodeSyntaxDump.ag"(line 87, column 33)
                     _lhsOppL =
                         []
                 in  ( _lhsOppL)) )