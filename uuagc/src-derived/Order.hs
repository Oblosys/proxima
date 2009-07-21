

-- UUAGC 0.9.10 (Order.ag)
module Order where

-- From uuagc
import CommonTypes
import Patterns
import ErrorMessages
import AbstractSyntax
import Code hiding (Type)
import qualified Code
import Expression
import Options
import SequentialComputation
import SequentialTypes
import CodeSyntax
import GrammarInfo
import HsToken(HsTokensRoot(HsTokensRoot))
import HsTokenScanner(lexTokens)
import SemHsTokens(sem_HsTokensRoot,wrap_HsTokensRoot, Syn_HsTokensRoot(..),Inh_HsTokensRoot(..))
-- From uulib
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Map(Map) 
import Data.Set(Set)
import Data.Sequence(Seq, (><))
import UU.Util.Utils
import UU.Scanner.Position(Pos(..),initPos)
import Data.Foldable(toList)

-- From haskell libraries
import Control.Monad(liftM)
import qualified Data.Array as Array
import Data.Array((!),bounds,inRange)
import Data.List(elemIndex,partition,sort,mapAccumL,find,nubBy,intersperse,groupBy,transpose)
import qualified Data.Tree as Tree
import Maybe


-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)


import UU.Scanner.Position(Pos)
import HsToken


-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import CommonTypes

-- Terminates with an error if the key is not in the map
findWithErr1 :: (Ord k, Show k) => String -> k -> Map k a -> a
findWithErr1 s k
  = Map.findWithDefault (error ("findWithErr1 " ++ s ++ ": key " ++ show k ++ " not in map.")) k

findWithErr2 :: (Ord k, Show k, Show a) => k -> Map k a -> a
findWithErr2 k m
  = Map.findWithDefault (error ("findWithErr2: key " ++ show k ++ " not in map: " ++ show m)) k m


getNtName :: Type -> NontermIdent
getNtName (NT nt _) = nt
getNtName _         = nullIdent


data AltAttr = AltAttr Identifier Identifier Bool 
               deriving (Eq, Ord, Show)


substSelf nt tp
  = case tp of
      NT n tps | n == _SELF -> NT nt tps
      _                     -> tp
	
haskellTupel :: [Type] -> Maybe Type
haskellTupel ts =  Just ( Haskell ( '(' : (concat (intersperse "," (map show ts))) ++ ")" ))


lookupVertices :: Identifier -> Identifier -> Map AltAttr Int -> [Int]
lookupVertices fld nm attrs
  = catMaybes (  [Map.lookup (AltAttr fld nm False) attrs]  -- look for all "out" attributes
              ++ if fld == _LOC
                 then [Map.lookup (AltAttr fld nm True) attrs]
                 else []
              )


swap (a,b) = (b,a)

showPath :: Table CRule -> [Vertex] -> [String]
showPath ruleTable path
  =  let  look a | inRange (bounds ruleTable) a = [showOrigin (ruleTable ! a)]
                 | otherwise = ["Vertex " ++ show a]
          showOrigin cr  | getHasCode cr && getName (getAttr cr) /= "self" = prettyCRule cr ++ " (" ++ show (getPos (getAttr cr)) ++ ")"
                         | otherwise = prettyCRule cr
     in concatMap look path


showPathLocal :: Table CRule -> [Vertex] -> [String]
showPathLocal _ [] = []
showPathLocal ruleTable xs = showP (xs++[-1])
 where showP []         = []
       showP (v1:v2:vs) = let line  = step v1 v2
                              lines = showP vs
                          in  line:lines
       step v1 v2  = " - " ++ a1
        where r1 = ruleTable ! v1
              a1 = show (getAttr  r1)


limitTo :: Int -> [String] -> [String]
limitTo _ [] = [] 
limitTo 0 _ = ["....etcetera, etcetera...."]
limitTo n (x:xs) = x : limitTo (n-1) xs

showPathNice :: Table CRule -> [Vertex] -> [String]
showPathNice _ [] = []
showPathNice ruleTable xs = limitTo 100 (showP ((-1):xs++[-1]))
 where [maxf, maxa, maxn, maxc] = maxWidths ruleTable (take 100 xs)
       showP []         = []
       showP (v1:v2:vs) = let line  = step v1 v2
                              lines = showP vs
                          in  if null line  then lines  else line:lines
       step v1 v2  |  last &&      first    = induced
                   |  last &&     isSyn r1  = "pass up        "  ++ alignR maxf ""    ++ " " ++ alignL maxa a1 ++ " in " ++ alignR maxn n1 ++ "|" ++ c1 ++ induced
                   |  first&& not(isSyn r2) = "get from above "  ++ alignR maxf ""    ++ " " ++ alignL maxa a2 ++ " in " ++ alignR maxn n2 ++ "|" ++ c2
                   |  last                  = "pass down      "  ++ alignR maxf f1    ++ "." ++ a1                                                      ++ induced
                   |              isSyn r2  = "get from below "  ++ alignR maxf f2    ++ "." ++ alignL maxa a2 ++ " in " ++ alignR maxn n2 ++ "|" ++ c2
                   |  isLocal r1  = if head a1 == '_' 
                                         then ""
                                         else "calculate      "  ++ alignR maxf "loc" ++ "." ++ a1
                   |  otherwise             = "pass down      "  ++ alignR maxf f1    ++ "." ++ alignL maxa a1 ++ " to " ++ alignR maxn n2 ++ "|" ++ c2
          where
              first = v1<0
              last  = v2<0
              r1 = ruleTable ! v1
              r2 = ruleTable ! v2
              a1 = show (getAttr  r1)
              a2 = show (getAttr  r2)
              f1 = show (getField r1)
              f2 = show (getField r2)
              n1 = show (getLhsNt r1)
              n2 = show (getLhsNt r2)
              c1 = show (getCon   r1)
              c2 = show (getCon   r2)
              induced | v2== -2   =  " INDUCED dependency to "
                      | otherwise = ""


maxWidths ruleTable vs
  = map maximum (transpose (map getWidth vs))
  where getWidth v | v<0       = [0,0,0,0]
                   | otherwise = map (length . show . ($ (ruleTable!v))) [getField, getAttr, getLhsNt, getCon]

alignL n xs | k<n       = xs ++ replicate (n-k) ' '
            | otherwise = xs
              where k = length xs

alignR n xs | k<n       = replicate (n-k) ' ' ++ xs
            | otherwise = xs
              where k = length xs

localCycleErr :: Table CRule -> Bool -> Route -> Error
localCycleErr ruleTable o_visit (s:path)
  =  let cr = ruleTable ! s
         attr = getAttr cr
         nt = getLhsNt cr
         con = getCon cr
     in LocalCirc nt con attr o_visit (showPathLocal ruleTable path)

instCycleErr :: Table CRule -> Bool -> Route -> Error
instCycleErr ruleTable o_visit (s:path)
  =  let cr = ruleTable ! s
         attr = getAttr cr
         nt = getLhsNt cr
         con = getCon cr
     in InstCirc nt con attr o_visit (showPathLocal ruleTable path)

directCycleErrs :: Table NTAttr -> Table CRule -> Bool -> [EdgeRoutes] -> [Error]
directCycleErrs attrTable ruleTable o_visit xs
  = let getNont v = case attrTable ! v of
                      NTASyn nt _ _ -> nt
                      NTAInh nt _ _ -> nt
        getAttr v = case attrTable ! v of
                      NTASyn _ a _  -> a
                      NTAInh _ a _  -> a
        sameNont ((v1,_),_,_) ((v2,_),_,_) =  getNont v1 == getNont v2
        procCycle ((v1,v2),p1,p2) = ((getAttr v1, getAttr v2), showPathNice ruleTable p1, showPathNice ruleTable p2)
        wrapGroup gr@(((v1,_),_,_):_) = DirectCirc (getNont v1) o_visit (map procCycle gr)
    in  map wrapGroup (groupBy sameNont xs)

inducedCycleErrs :: Table NTAttr -> Table CRule -> CInterfaceMap -> [EdgeRoutes] -> [Error]
inducedCycleErrs attrTable ruleTable cim xs
  = let getNont v = case attrTable ! v of
                      NTASyn nt _ _ -> nt
                      NTAInh nt _ _ -> nt
        getAttr v = case attrTable ! v of
                      NTASyn _ a _  -> a
                      NTAInh _ a _  -> a
        sameNont ((v1,_),_,_) ((v2,_),_,_) =  getNont v1 == getNont v2
        procCycle ((v1,v2),p1,p2) = ((getAttr v1, getAttr v2), showPathNice ruleTable p1, showPathNice ruleTable p2)
        wrapGroup gr@(((v1,_),_,_):_) = InducedCirc (getNont v1) (findWithErr1 "inducedCycleErr.cinter" (getNont v1) cim) (map procCycle gr)
    in  map wrapGroup (groupBy sameNont xs)
-- Child -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,Bool)]
         allnts               : [Identifier]
         attrs                : [(Identifier,Identifier)]
         con                  : Identifier
         inh                  : Attributes
         nt                   : Identifier
         o_unbox              : Bool
         syn                  : Attributes
      synthesized attributes:
         attributes           : [(Identifier,Attributes,Attributes)]
         collectChildrenSyns  : Map Identifier Attributes 
         errors               : Seq Error
         field                : (Identifier,Type,Bool)
         gathAltAttrs         : [AltAttr]
         gathRules            : Seq CRule
         inhs                 : Seq (Identifier,Attributes)
         nts                  : Seq (Identifier,NontermIdent)
         singlevisits         : [CRule]
         terminals            : [Identifier]
   alternatives:
      alternative Child:
         child name           : {Identifier}
         child tp             : {Type}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child higherOrder    : {Bool}
         visit 0:
            local maptolocal  : _
            local gathRules   : _
-}
-- cata
sem_Child :: Child  ->
             T_Child 
sem_Child (Child _name _tp _inh _syn _higherOrder )  =
    (sem_Child_Child _name _tp _inh _syn _higherOrder )
-- semantic domain
newtype T_Child  = T_Child (([(Identifier,Type,Bool)]) ->
                            ([Identifier]) ->
                            ([(Identifier,Identifier)]) ->
                            Identifier ->
                            Attributes ->
                            Identifier ->
                            Bool ->
                            Attributes ->
                            ( ([(Identifier,Attributes,Attributes)]),(Map Identifier Attributes ),(Seq Error),((Identifier,Type,Bool)),([AltAttr]),(Seq CRule),(Seq (Identifier,Attributes)),(Seq (Identifier,NontermIdent)),([CRule]),([Identifier])))
data Inh_Child  = Inh_Child {allfields_Inh_Child :: !([(Identifier,Type,Bool)]),allnts_Inh_Child :: !([Identifier]),attrs_Inh_Child :: !([(Identifier,Identifier)]),con_Inh_Child :: !(Identifier),inh_Inh_Child :: !(Attributes),nt_Inh_Child :: !(Identifier),o_unbox_Inh_Child :: !(Bool),syn_Inh_Child :: !(Attributes)}
data Syn_Child  = Syn_Child {attributes_Syn_Child :: !([(Identifier,Attributes,Attributes)]),collectChildrenSyns_Syn_Child :: !(Map Identifier Attributes ),errors_Syn_Child :: !(Seq Error),field_Syn_Child :: !((Identifier,Type,Bool)),gathAltAttrs_Syn_Child :: !([AltAttr]),gathRules_Syn_Child :: !(Seq CRule),inhs_Syn_Child :: !(Seq (Identifier,Attributes)),nts_Syn_Child :: !(Seq (Identifier,NontermIdent)),singlevisits_Syn_Child :: !([CRule]),terminals_Syn_Child :: !([Identifier])}
wrap_Child :: T_Child  ->
              Inh_Child  ->
              Syn_Child 
wrap_Child (T_Child sem ) (Inh_Child _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsInt _lhsIo_unbox _lhsIsyn )  =
    (let ( _lhsOattributes,_lhsOcollectChildrenSyns,_lhsOerrors,_lhsOfield,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinhs,_lhsOnts,_lhsOsinglevisits,_lhsOterminals) =
             (sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsInt _lhsIo_unbox _lhsIsyn )
     in  (Syn_Child _lhsOattributes _lhsOcollectChildrenSyns _lhsOerrors _lhsOfield _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOnts _lhsOsinglevisits _lhsOterminals ))
sem_Child_Child :: Identifier ->
                   Type ->
                   Attributes ->
                   Attributes ->
                   Bool ->
                   T_Child 
sem_Child_Child name_ tp_ inh_ syn_ higherOrder_  =
    (T_Child (\ _lhsIallfields
                _lhsIallnts
                _lhsIattrs
                _lhsIcon
                _lhsIinh
                _lhsInt
                _lhsIo_unbox
                _lhsIsyn ->
                  (let _lhsOgathAltAttrs :: ([AltAttr])
                       _lhsOnts :: (Seq (Identifier,NontermIdent))
                       _lhsOinhs :: (Seq (Identifier,Attributes))
                       _lhsOcollectChildrenSyns :: (Map Identifier Attributes )
                       _lhsOsinglevisits :: ([CRule])
                       _lhsOterminals :: ([Identifier])
                       _lhsOattributes :: ([(Identifier,Attributes,Attributes)])
                       _lhsOfield :: ((Identifier,Type,Bool))
                       _lhsOerrors :: (Seq Error)
                       _lhsOgathRules :: (Seq CRule)
                       -- "Order.ag"(line 157, column 13)
                       _maptolocal =
                           case tp_ of
                             NT nt _ -> Map.null syn_
                             _       -> True
                       -- "Order.ag"(line 160, column 13)
                       _lhsOgathAltAttrs =
                           if  _maptolocal
                               then [ AltAttr _LOC name_ True ]
                               else [ AltAttr name_ syn True | syn <- Map.keys syn_ ]
                       -- "Order.ag"(line 175, column 13)
                       _lhsOnts =
                           Seq.singleton (name_,getNtName tp_)
                       -- "Order.ag"(line 176, column 13)
                       _lhsOinhs =
                           Seq.singleton (name_,inh_)
                       -- "Order.ag"(line 192, column 13)
                       _gathRules =
                           if  _maptolocal
                               then Seq.singleton (cRuleTerminal name_ _lhsInt _lhsIcon tp_)
                               else Seq.fromList [ cRuleRhsSyn syn _lhsInt _lhsIcon tp name_ (getNtName tp_) | (syn,tp) <- Map.assocs syn_]
                       -- "Order.ag"(line 317, column 12)
                       _lhsOcollectChildrenSyns =
                           Map.singleton name_ syn_
                       -- "Order.ag"(line 477, column 11)
                       _lhsOsinglevisits =
                           if  _maptolocal
                               then []
                               else [CChildVisit name_ (getNtName tp_) 0 inh_ syn_ True]
                       -- "Order.ag"(line 498, column 11)
                       _lhsOterminals =
                           if _maptolocal
                           then [name_]
                           else []
                       -- "Order.ag"(line 527, column 11)
                       _lhsOattributes =
                           [(name_, inh_, syn_)]
                       -- "Order.ag"(line 531, column 11)
                       _lhsOfield =
                           (name_, tp_, higherOrder_)
                       -- use rule "Order.ag"(line 64, column 70)
                       _lhsOerrors =
                           Seq.empty
                       -- use rule "Order.ag"(line 186, column 23)
                       _lhsOgathRules =
                           _gathRules
                   in  ( _lhsOattributes,_lhsOcollectChildrenSyns,_lhsOerrors,_lhsOfield,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinhs,_lhsOnts,_lhsOsinglevisits,_lhsOterminals))) )
-- Children ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,Bool)]
         allnts               : [Identifier]
         attrs                : [(Identifier,Identifier)]
         con                  : Identifier
         inh                  : Attributes
         nt                   : Identifier
         o_unbox              : Bool
         syn                  : Attributes
      synthesized attributes:
         attributes           : [(Identifier,Attributes,Attributes)]
         collectChildrenSyns  : Map Identifier Attributes 
         errors               : Seq Error
         fields               : [(Identifier,Type,Bool)]
         gathAltAttrs         : [AltAttr]
         gathRules            : Seq CRule
         inhs                 : Seq (Identifier,Attributes)
         nts                  : Seq (Identifier,NontermIdent)
         singlevisits         : [CRule]
         terminals            : [Identifier]
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
newtype T_Children  = T_Children (([(Identifier,Type,Bool)]) ->
                                  ([Identifier]) ->
                                  ([(Identifier,Identifier)]) ->
                                  Identifier ->
                                  Attributes ->
                                  Identifier ->
                                  Bool ->
                                  Attributes ->
                                  ( ([(Identifier,Attributes,Attributes)]),(Map Identifier Attributes ),(Seq Error),([(Identifier,Type,Bool)]),([AltAttr]),(Seq CRule),(Seq (Identifier,Attributes)),(Seq (Identifier,NontermIdent)),([CRule]),([Identifier])))
data Inh_Children  = Inh_Children {allfields_Inh_Children :: !([(Identifier,Type,Bool)]),allnts_Inh_Children :: !([Identifier]),attrs_Inh_Children :: !([(Identifier,Identifier)]),con_Inh_Children :: !(Identifier),inh_Inh_Children :: !(Attributes),nt_Inh_Children :: !(Identifier),o_unbox_Inh_Children :: !(Bool),syn_Inh_Children :: !(Attributes)}
data Syn_Children  = Syn_Children {attributes_Syn_Children :: !([(Identifier,Attributes,Attributes)]),collectChildrenSyns_Syn_Children :: !(Map Identifier Attributes ),errors_Syn_Children :: !(Seq Error),fields_Syn_Children :: !([(Identifier,Type,Bool)]),gathAltAttrs_Syn_Children :: !([AltAttr]),gathRules_Syn_Children :: !(Seq CRule),inhs_Syn_Children :: !(Seq (Identifier,Attributes)),nts_Syn_Children :: !(Seq (Identifier,NontermIdent)),singlevisits_Syn_Children :: !([CRule]),terminals_Syn_Children :: !([Identifier])}
wrap_Children :: T_Children  ->
                 Inh_Children  ->
                 Syn_Children 
wrap_Children (T_Children sem ) (Inh_Children _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsInt _lhsIo_unbox _lhsIsyn )  =
    (let ( _lhsOattributes,_lhsOcollectChildrenSyns,_lhsOerrors,_lhsOfields,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinhs,_lhsOnts,_lhsOsinglevisits,_lhsOterminals) =
             (sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsInt _lhsIo_unbox _lhsIsyn )
     in  (Syn_Children _lhsOattributes _lhsOcollectChildrenSyns _lhsOerrors _lhsOfields _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOnts _lhsOsinglevisits _lhsOterminals ))
sem_Children_Cons :: T_Child  ->
                     T_Children  ->
                     T_Children 
sem_Children_Cons (T_Child hd_ ) (T_Children tl_ )  =
    (T_Children (\ _lhsIallfields
                   _lhsIallnts
                   _lhsIattrs
                   _lhsIcon
                   _lhsIinh
                   _lhsInt
                   _lhsIo_unbox
                   _lhsIsyn ->
                     (let _lhsOfields :: ([(Identifier,Type,Bool)])
                          _lhsOattributes :: ([(Identifier,Attributes,Attributes)])
                          _lhsOcollectChildrenSyns :: (Map Identifier Attributes )
                          _lhsOerrors :: (Seq Error)
                          _lhsOgathAltAttrs :: ([AltAttr])
                          _lhsOgathRules :: (Seq CRule)
                          _lhsOinhs :: (Seq (Identifier,Attributes))
                          _lhsOnts :: (Seq (Identifier,NontermIdent))
                          _lhsOsinglevisits :: ([CRule])
                          _lhsOterminals :: ([Identifier])
                          _hdOallfields :: ([(Identifier,Type,Bool)])
                          _hdOallnts :: ([Identifier])
                          _hdOattrs :: ([(Identifier,Identifier)])
                          _hdOcon :: Identifier
                          _hdOinh :: Attributes
                          _hdOnt :: Identifier
                          _hdOo_unbox :: Bool
                          _hdOsyn :: Attributes
                          _tlOallfields :: ([(Identifier,Type,Bool)])
                          _tlOallnts :: ([Identifier])
                          _tlOattrs :: ([(Identifier,Identifier)])
                          _tlOcon :: Identifier
                          _tlOinh :: Attributes
                          _tlOnt :: Identifier
                          _tlOo_unbox :: Bool
                          _tlOsyn :: Attributes
                          _hdIattributes :: ([(Identifier,Attributes,Attributes)])
                          _hdIcollectChildrenSyns :: (Map Identifier Attributes )
                          _hdIerrors :: (Seq Error)
                          _hdIfield :: ((Identifier,Type,Bool))
                          _hdIgathAltAttrs :: ([AltAttr])
                          _hdIgathRules :: (Seq CRule)
                          _hdIinhs :: (Seq (Identifier,Attributes))
                          _hdInts :: (Seq (Identifier,NontermIdent))
                          _hdIsinglevisits :: ([CRule])
                          _hdIterminals :: ([Identifier])
                          _tlIattributes :: ([(Identifier,Attributes,Attributes)])
                          _tlIcollectChildrenSyns :: (Map Identifier Attributes )
                          _tlIerrors :: (Seq Error)
                          _tlIfields :: ([(Identifier,Type,Bool)])
                          _tlIgathAltAttrs :: ([AltAttr])
                          _tlIgathRules :: (Seq CRule)
                          _tlIinhs :: (Seq (Identifier,Attributes))
                          _tlInts :: (Seq (Identifier,NontermIdent))
                          _tlIsinglevisits :: ([CRule])
                          _tlIterminals :: ([Identifier])
                          -- "Order.ag"(line 534, column 11)
                          _lhsOfields =
                              _hdIfield : _tlIfields
                          -- use rule "Order.ag"(line 525, column 32)
                          _lhsOattributes =
                              _hdIattributes ++ _tlIattributes
                          -- use rule "Order.ag"(line 315, column 47)
                          _lhsOcollectChildrenSyns =
                              _hdIcollectChildrenSyns `Map.union` _tlIcollectChildrenSyns
                          -- use rule "Order.ag"(line 64, column 70)
                          _lhsOerrors =
                              _hdIerrors Seq.>< _tlIerrors
                          -- use rule "Order.ag"(line 150, column 68)
                          _lhsOgathAltAttrs =
                              _hdIgathAltAttrs ++ _tlIgathAltAttrs
                          -- use rule "Order.ag"(line 186, column 23)
                          _lhsOgathRules =
                              _hdIgathRules Seq.>< _tlIgathRules
                          -- use rule "Order.ag"(line 173, column 20)
                          _lhsOinhs =
                              _hdIinhs Seq.>< _tlIinhs
                          -- use rule "Order.ag"(line 172, column 19)
                          _lhsOnts =
                              _hdInts Seq.>< _tlInts
                          -- use rule "Order.ag"(line 475, column 40)
                          _lhsOsinglevisits =
                              _hdIsinglevisits ++ _tlIsinglevisits
                          -- use rule "Order.ag"(line 496, column 38)
                          _lhsOterminals =
                              _hdIterminals ++ _tlIterminals
                          -- copy rule (down)
                          _hdOallfields =
                              _lhsIallfields
                          -- copy rule (down)
                          _hdOallnts =
                              _lhsIallnts
                          -- copy rule (down)
                          _hdOattrs =
                              _lhsIattrs
                          -- copy rule (down)
                          _hdOcon =
                              _lhsIcon
                          -- copy rule (down)
                          _hdOinh =
                              _lhsIinh
                          -- copy rule (down)
                          _hdOnt =
                              _lhsInt
                          -- copy rule (down)
                          _hdOo_unbox =
                              _lhsIo_unbox
                          -- copy rule (down)
                          _hdOsyn =
                              _lhsIsyn
                          -- copy rule (down)
                          _tlOallfields =
                              _lhsIallfields
                          -- copy rule (down)
                          _tlOallnts =
                              _lhsIallnts
                          -- copy rule (down)
                          _tlOattrs =
                              _lhsIattrs
                          -- copy rule (down)
                          _tlOcon =
                              _lhsIcon
                          -- copy rule (down)
                          _tlOinh =
                              _lhsIinh
                          -- copy rule (down)
                          _tlOnt =
                              _lhsInt
                          -- copy rule (down)
                          _tlOo_unbox =
                              _lhsIo_unbox
                          -- copy rule (down)
                          _tlOsyn =
                              _lhsIsyn
                          ( _hdIattributes,_hdIcollectChildrenSyns,_hdIerrors,_hdIfield,_hdIgathAltAttrs,_hdIgathRules,_hdIinhs,_hdInts,_hdIsinglevisits,_hdIterminals) =
                              (hd_ _hdOallfields _hdOallnts _hdOattrs _hdOcon _hdOinh _hdOnt _hdOo_unbox _hdOsyn )
                          ( _tlIattributes,_tlIcollectChildrenSyns,_tlIerrors,_tlIfields,_tlIgathAltAttrs,_tlIgathRules,_tlIinhs,_tlInts,_tlIsinglevisits,_tlIterminals) =
                              (tl_ _tlOallfields _tlOallnts _tlOattrs _tlOcon _tlOinh _tlOnt _tlOo_unbox _tlOsyn )
                      in  ( _lhsOattributes,_lhsOcollectChildrenSyns,_lhsOerrors,_lhsOfields,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinhs,_lhsOnts,_lhsOsinglevisits,_lhsOterminals))) )
sem_Children_Nil :: T_Children 
sem_Children_Nil  =
    (T_Children (\ _lhsIallfields
                   _lhsIallnts
                   _lhsIattrs
                   _lhsIcon
                   _lhsIinh
                   _lhsInt
                   _lhsIo_unbox
                   _lhsIsyn ->
                     (let _lhsOfields :: ([(Identifier,Type,Bool)])
                          _lhsOattributes :: ([(Identifier,Attributes,Attributes)])
                          _lhsOcollectChildrenSyns :: (Map Identifier Attributes )
                          _lhsOerrors :: (Seq Error)
                          _lhsOgathAltAttrs :: ([AltAttr])
                          _lhsOgathRules :: (Seq CRule)
                          _lhsOinhs :: (Seq (Identifier,Attributes))
                          _lhsOnts :: (Seq (Identifier,NontermIdent))
                          _lhsOsinglevisits :: ([CRule])
                          _lhsOterminals :: ([Identifier])
                          -- "Order.ag"(line 535, column 11)
                          _lhsOfields =
                              []
                          -- use rule "Order.ag"(line 525, column 32)
                          _lhsOattributes =
                              []
                          -- use rule "Order.ag"(line 315, column 47)
                          _lhsOcollectChildrenSyns =
                              Map.empty
                          -- use rule "Order.ag"(line 64, column 70)
                          _lhsOerrors =
                              Seq.empty
                          -- use rule "Order.ag"(line 150, column 68)
                          _lhsOgathAltAttrs =
                              []
                          -- use rule "Order.ag"(line 186, column 23)
                          _lhsOgathRules =
                              Seq.empty
                          -- use rule "Order.ag"(line 173, column 20)
                          _lhsOinhs =
                              Seq.empty
                          -- use rule "Order.ag"(line 172, column 19)
                          _lhsOnts =
                              Seq.empty
                          -- use rule "Order.ag"(line 475, column 40)
                          _lhsOsinglevisits =
                              []
                          -- use rule "Order.ag"(line 496, column 38)
                          _lhsOterminals =
                              []
                      in  ( _lhsOattributes,_lhsOcollectChildrenSyns,_lhsOerrors,_lhsOfields,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinhs,_lhsOnts,_lhsOsinglevisits,_lhsOterminals))) )
-- Expression --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,Bool)]
         allnts               : [Identifier]
         attrs                : [(Identifier,Identifier)]
         con                  : Identifier
         nt                   : Identifier
      synthesized attributes:
         allRhsVars           : Set (Identifier,Identifier)
         copy                 : SELF 
         errors               : Seq Error
         textLines            : [String]
         usedAttrs            : [(Identifier,Identifier)]
         usedFields           : [Identifier]
         usedLocals           : [Identifier]
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
         visit 0:
            local _tup1       : _
            local errors      : _
            local textLines   : _
            local usedAttrs   : _
            local usedLocals  : _
            local usedFields  : _
            local copy        : _
-}
-- cata
sem_Expression :: Expression  ->
                  T_Expression 
sem_Expression (Expression _pos _tks )  =
    (sem_Expression_Expression _pos _tks )
-- semantic domain
newtype T_Expression  = T_Expression (([(Identifier,Type,Bool)]) ->
                                      ([Identifier]) ->
                                      ([(Identifier,Identifier)]) ->
                                      Identifier ->
                                      Identifier ->
                                      ( (Set (Identifier,Identifier)),Expression,(Seq Error),([String]),([(Identifier,Identifier)]),([Identifier]),([Identifier])))
data Inh_Expression  = Inh_Expression {allfields_Inh_Expression :: !([(Identifier,Type,Bool)]),allnts_Inh_Expression :: !([Identifier]),attrs_Inh_Expression :: !([(Identifier,Identifier)]),con_Inh_Expression :: !(Identifier),nt_Inh_Expression :: !(Identifier)}
data Syn_Expression  = Syn_Expression {allRhsVars_Syn_Expression :: !(Set (Identifier,Identifier)),copy_Syn_Expression :: !(Expression),errors_Syn_Expression :: !(Seq Error),textLines_Syn_Expression :: !([String]),usedAttrs_Syn_Expression :: !([(Identifier,Identifier)]),usedFields_Syn_Expression :: !([Identifier]),usedLocals_Syn_Expression :: !([Identifier])}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression (T_Expression sem ) (Inh_Expression _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsInt )  =
    (let ( _lhsOallRhsVars,_lhsOcopy,_lhsOerrors,_lhsOtextLines,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals) =
             (sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsInt )
     in  (Syn_Expression _lhsOallRhsVars _lhsOcopy _lhsOerrors _lhsOtextLines _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals ))
sem_Expression_Expression :: Pos ->
                             ([HsToken]) ->
                             T_Expression 
sem_Expression_Expression pos_ tks_  =
    (T_Expression (\ _lhsIallfields
                     _lhsIallnts
                     _lhsIattrs
                     _lhsIcon
                     _lhsInt ->
                       (let _lhsOallRhsVars :: (Set (Identifier,Identifier))
                            _lhsOcopy :: Expression
                            _lhsOerrors :: (Seq Error)
                            _lhsOtextLines :: ([String])
                            _lhsOusedAttrs :: ([(Identifier,Identifier)])
                            _lhsOusedFields :: ([Identifier])
                            _lhsOusedLocals :: ([Identifier])
                            -- "Order.ag"(line 337, column 21)
                            __tup1 =
                                let inherited = Inh_HsTokensRoot
                                                { attrs_Inh_HsTokensRoot      = _lhsIattrs
                                                , con_Inh_HsTokensRoot        = _lhsIcon
                                                , allfields_Inh_HsTokensRoot  = _lhsIallfields
                                                , allnts_Inh_HsTokensRoot     = _lhsIallnts
                                                , nt_Inh_HsTokensRoot         = _lhsInt
                                                }
                                    synthesized = wrap_HsTokensRoot (sem_HsTokensRoot (HsTokensRoot tks_)) inherited
                                in case synthesized of
                                     Syn_HsTokensRoot
                                      { errors_Syn_HsTokensRoot     = errors
                                      , textLines_Syn_HsTokensRoot  = textLines
                                      , usedAttrs_Syn_HsTokensRoot  = usedAttrs
                                      , usedLocals_Syn_HsTokensRoot = usedLocals
                                      , usedFields_Syn_HsTokensRoot = usedFields
                                      }  -> (errors,textLines,usedAttrs,usedLocals,usedFields)
                            -- "Order.ag"(line 337, column 21)
                            (_errors,_,_,_,_) =
                                __tup1
                            -- "Order.ag"(line 337, column 21)
                            (_,_textLines,_,_,_) =
                                __tup1
                            -- "Order.ag"(line 337, column 21)
                            (_,_,_usedAttrs,_,_) =
                                __tup1
                            -- "Order.ag"(line 337, column 21)
                            (_,_,_,_usedLocals,_) =
                                __tup1
                            -- "Order.ag"(line 337, column 21)
                            (_,_,_,_,_usedFields) =
                                __tup1
                            -- "Order.ag"(line 355, column 17)
                            _lhsOallRhsVars =
                                Set.fromList _usedAttrs
                                `Set.union`
                                Set.fromList [ (_LOC, l) | l <- _usedLocals    ]
                                `Set.union`
                                Set.fromList [ (_FIELD, fld) | fld <- _usedFields    ]
                            -- self rule
                            _copy =
                                Expression pos_ tks_
                            -- self rule
                            _lhsOcopy =
                                _copy
                            -- copy rule (from local)
                            _lhsOerrors =
                                _errors
                            -- copy rule (from local)
                            _lhsOtextLines =
                                _textLines
                            -- copy rule (from local)
                            _lhsOusedAttrs =
                                _usedAttrs
                            -- copy rule (from local)
                            _lhsOusedFields =
                                _usedFields
                            -- copy rule (from local)
                            _lhsOusedLocals =
                                _usedLocals
                        in  ( _lhsOallRhsVars,_lhsOcopy,_lhsOerrors,_lhsOtextLines,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))) )
-- Grammar -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         errors               : Seq Error
         output               : CGrammar
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
            local ruleTable   : _
            local attrTable   : _
            local attrVertex  : _
            local tdpToTds    : _
            local tdsToTdp    : _
            local directDep   : _
            local instDep     : _
            local info        : _
            local _tup2       : _
            local cInterfaceMap : _
            local cVisitsMap  : _
            local cyclesErrors : _
-}
-- cata
sem_Grammar :: Grammar  ->
               T_Grammar 
sem_Grammar (Grammar _typeSyns _useMap _derivings _wrappers _nonts _pragmas _manualAttrOrderMap _paramMap _contextMap _uniqueMap )  =
    (sem_Grammar_Grammar _typeSyns _useMap _derivings _wrappers (sem_Nonterminals _nonts ) _pragmas _manualAttrOrderMap _paramMap _contextMap _uniqueMap )
-- semantic domain
newtype T_Grammar  = T_Grammar (Options ->
                                ( (Seq Error),CGrammar))
data Inh_Grammar  = Inh_Grammar {options_Inh_Grammar :: !(Options)}
data Syn_Grammar  = Syn_Grammar {errors_Syn_Grammar :: !(Seq Error),output_Syn_Grammar :: !(CGrammar)}
wrap_Grammar :: T_Grammar  ->
                Inh_Grammar  ->
                Syn_Grammar 
wrap_Grammar (T_Grammar sem ) (Inh_Grammar _lhsIoptions )  =
    (let ( _lhsOerrors,_lhsOoutput) =
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
sem_Grammar_Grammar typeSyns_ useMap_ derivings_ wrappers_ (T_Nonterminals nonts_ ) pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ uniqueMap_  =
    (T_Grammar (\ _lhsIoptions ->
                    (let _nontsOo_cata :: Bool
                         _nontsOo_data :: Bool
                         _nontsOo_sig :: Bool
                         _nontsOo_sem :: Bool
                         _nontsOo_rename :: Bool
                         _nontsOo_newtypes :: Bool
                         _nontsOo_wantvisit :: Bool
                         _nontsOo_dovisit :: Bool
                         _nontsOo_unbox :: Bool
                         _nontsOo_case :: Bool
                         _nontsOprefix :: String
                         _nontsOvcount :: Int
                         _nontsOmanualAttrDepMap :: AttrOrderMap
                         _nontsOacount :: Int
                         _lhsOerrors :: (Seq Error)
                         _lhsOoutput :: CGrammar
                         _nontsOallnts :: ([Identifier])
                         _nontsOcInterfaceMap :: CInterfaceMap
                         _nontsOcVisitsMap :: CVisitsMap
                         _nontsIacount :: Int
                         _nontsIadditionalDep :: (Seq Edge)
                         _nontsIaranges :: (Seq (Int,Int,Int))
                         _nontsIcNonterminals :: CNonterminals
                         _nontsIdirectDep :: (Seq Edge)
                         _nontsIerrors :: (Seq Error)
                         _nontsIinstDep :: (Seq Edge)
                         _nontsInonts :: ([(NontermIdent,[ConstructorIdent])])
                         _nontsIntattrs :: (Seq (Vertex,NTAttr))
                         _nontsIrules :: (Seq (Vertex,CRule))
                         _nontsIvcount :: Int
                         -- "Order.ag"(line 103, column 17)
                         _nontsOo_cata =
                             folds     _lhsIoptions
                         -- "Order.ag"(line 103, column 17)
                         _nontsOo_data =
                             dataTypes _lhsIoptions
                         -- "Order.ag"(line 103, column 17)
                         _nontsOo_sig =
                             typeSigs  _lhsIoptions
                         -- "Order.ag"(line 103, column 17)
                         _nontsOo_sem =
                             semfuns   _lhsIoptions
                         -- "Order.ag"(line 103, column 17)
                         _nontsOo_rename =
                             rename    _lhsIoptions
                         -- "Order.ag"(line 103, column 17)
                         _nontsOo_newtypes =
                             newtypes  _lhsIoptions
                         -- "Order.ag"(line 103, column 17)
                         _nontsOo_wantvisit =
                             visit   _lhsIoptions
                         -- "Order.ag"(line 103, column 17)
                         _nontsOo_dovisit =
                             visit     _lhsIoptions && null _cyclesErrors
                         -- "Order.ag"(line 103, column 17)
                         _nontsOo_unbox =
                             unbox     _lhsIoptions
                         -- "Order.ag"(line 103, column 17)
                         _nontsOo_case =
                             cases     _lhsIoptions
                         -- "Order.ag"(line 103, column 17)
                         _nontsOprefix =
                             prefix    _lhsIoptions
                         -- "Order.ag"(line 241, column 15)
                         _nontsOvcount =
                             0
                         -- "Order.ag"(line 266, column 7)
                         _nontsOmanualAttrDepMap =
                             manualAttrOrderMap_
                         -- "Order.ag"(line 369, column 13)
                         _nontsOacount =
                             0
                         -- "Order.ag"(line 407, column 13)
                         _ruleTable =
                             Array.array (0,_nontsIvcount-1) (toList _nontsIrules)
                         -- "Order.ag"(line 408, column 13)
                         _attrTable =
                             Array.array (0,_nontsIacount-1) (toList _nontsIntattrs)
                         -- "Order.ag"(line 409, column 13)
                         _attrVertex =
                             Map.fromList (map swap (toList _nontsIntattrs))
                         -- "Order.ag"(line 410, column 13)
                         _tdpToTds =
                             [ (s, maybe (-1) (\v -> findWithErr1 "Grammar.tdpToTds" v _attrVertex) (ntattr cr))
                             | (s,cr) <- toList _nontsIrules]
                         -- "Order.ag"(line 412, column 13)
                         _tdsToTdp =
                             let  eq (_,v) (_,v') = v == v'
                                  conv ((s,v):svs)  | v == -1 = Nothing
                                                    | otherwise = Just (v,s:map fst svs)
                             in mapMaybe conv (eqClasses eq _tdpToTds)
                         -- "Order.ag"(line 416, column 13)
                         _directDep =
                             toList (_nontsIdirectDep Seq.>< _nontsIadditionalDep)
                         -- "Order.ag"(line 417, column 13)
                         _instDep =
                             toList _nontsIinstDep
                         -- "Order.ag"(line 418, column 13)
                         _info =
                             let def [] = -1
                                 def (v:vs) = v
                              in Info { tdsToTdp   = Array.array (0,_nontsIacount-1) _tdsToTdp
                                      , tdpToTds   = Array.array (0,_nontsIvcount-1) _tdpToTds
                                      , attrTable  = _attrTable
                                      , ruleTable  = _ruleTable
                                      , lmh        = toList _nontsIaranges
                                      , nonts      = _nontsInonts
                                      , wraps      = wrappers_
                                      }
                         -- "Order.ag"(line 429, column 17)
                         __tup2 =
                             case computeSequential _info _directDep _instDep of
                                          CycleFree    cim cvm   -> ( cim
                                                                    , cvm
                                                                    , []
                                                                    )
                                          LocalCycle   errs      -> ( error "No interfaces for AG with local cycles"
                                                                    , error "No visit sub-sequences for AG with local cycles"
                                                                    , map (localCycleErr _ruleTable (visit _lhsIoptions)) errs
                                                                    )
                                          InstCycle    errs      -> ( error "No interfaces for AG with cycles through insts"
                                                                    , error "No visit sub-sequences for AG with cycles through insts"
                                                                    , map (instCycleErr _ruleTable (visit _lhsIoptions)) errs
                                                                    )
                                          DirectCycle  errs      -> ( error "No interfaces for AG with direct cycles"
                                                                    , error "No visit sub-sequences for AG with direct cycles"
                                                                    , directCycleErrs _attrTable _ruleTable (visit _lhsIoptions) errs
                                                                    )
                                          InducedCycle cim errs ->  ( cim
                                                                    , error "No visit sub-sequences for AG with induced cycles"
                                                                    , inducedCycleErrs _attrTable _ruleTable cim errs
                                                                    )
                         -- "Order.ag"(line 429, column 17)
                         (_cInterfaceMap,_,_) =
                             __tup2
                         -- "Order.ag"(line 429, column 17)
                         (_,_cVisitsMap,_) =
                             __tup2
                         -- "Order.ag"(line 429, column 17)
                         (_,_,_cyclesErrors) =
                             __tup2
                         -- "Order.ag"(line 451, column 13)
                         _lhsOerrors =
                             (if withCycle _lhsIoptions then Seq.fromList _cyclesErrors else Seq.empty)
                             Seq.>< _nontsIerrors
                         -- "Order.ag"(line 483, column 15)
                         _lhsOoutput =
                             CGrammar typeSyns_ derivings_ wrappers_ _nontsIcNonterminals pragmas_ paramMap_ contextMap_
                         -- "Order.ag"(line 510, column 13)
                         _nontsOallnts =
                             map fst (_nontsInonts)
                         -- copy rule (from local)
                         _nontsOcInterfaceMap =
                             _cInterfaceMap
                         -- copy rule (from local)
                         _nontsOcVisitsMap =
                             _cVisitsMap
                         ( _nontsIacount,_nontsIadditionalDep,_nontsIaranges,_nontsIcNonterminals,_nontsIdirectDep,_nontsIerrors,_nontsIinstDep,_nontsInonts,_nontsIntattrs,_nontsIrules,_nontsIvcount) =
                             (nonts_ _nontsOacount _nontsOallnts _nontsOcInterfaceMap _nontsOcVisitsMap _nontsOmanualAttrDepMap _nontsOo_case _nontsOo_cata _nontsOo_data _nontsOo_dovisit _nontsOo_newtypes _nontsOo_rename _nontsOo_sem _nontsOo_sig _nontsOo_unbox _nontsOo_wantvisit _nontsOprefix _nontsOvcount )
                     in  ( _lhsOerrors,_lhsOoutput))) )
-- Nonterminal -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allnts               : [Identifier]
         cInterfaceMap        : CInterfaceMap
         cVisitsMap           : CVisitsMap
         manualAttrDepMap     : AttrOrderMap
         o_case               : Bool
         o_cata               : Bool
         o_data               : Bool
         o_dovisit            : Bool
         o_newtypes           : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_unbox              : Bool
         o_wantvisit          : Bool
         prefix               : String
      chained attributes:
         acount               : Int
         vcount               : Int
      synthesized attributes:
         additionalDep        : Seq Edge
         aranges              : Seq (Int,Int,Int)
         cNonterminal         : CNonterminal
         directDep            : Seq Edge
         errors               : Seq Error
         instDep              : Seq Edge
         nonts                : [(NontermIdent,[ConstructorIdent])]
         ntattrs              : Seq (Vertex,NTAttr)
         rules                : Seq (Vertex,CRule)
   alternatives:
      alternative Nonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : Productions 
         visit 0:
            local ntattrs     : _
            local cInter      : _
-}
-- cata
sem_Nonterminal :: Nonterminal  ->
                   T_Nonterminal 
sem_Nonterminal (Nonterminal _nt _params _inh _syn _prods )  =
    (sem_Nonterminal_Nonterminal _nt _params _inh _syn (sem_Productions _prods ) )
-- semantic domain
newtype T_Nonterminal  = T_Nonterminal (Int ->
                                        ([Identifier]) ->
                                        CInterfaceMap ->
                                        CVisitsMap ->
                                        AttrOrderMap ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        String ->
                                        Int ->
                                        ( Int,(Seq Edge),(Seq (Int,Int,Int)),CNonterminal,(Seq Edge),(Seq Error),(Seq Edge),([(NontermIdent,[ConstructorIdent])]),(Seq (Vertex,NTAttr)),(Seq (Vertex,CRule)),Int))
data Inh_Nonterminal  = Inh_Nonterminal {acount_Inh_Nonterminal :: !(Int),allnts_Inh_Nonterminal :: !([Identifier]),cInterfaceMap_Inh_Nonterminal :: !(CInterfaceMap),cVisitsMap_Inh_Nonterminal :: !(CVisitsMap),manualAttrDepMap_Inh_Nonterminal :: !(AttrOrderMap),o_case_Inh_Nonterminal :: !(Bool),o_cata_Inh_Nonterminal :: !(Bool),o_data_Inh_Nonterminal :: !(Bool),o_dovisit_Inh_Nonterminal :: !(Bool),o_newtypes_Inh_Nonterminal :: !(Bool),o_rename_Inh_Nonterminal :: !(Bool),o_sem_Inh_Nonterminal :: !(Bool),o_sig_Inh_Nonterminal :: !(Bool),o_unbox_Inh_Nonterminal :: !(Bool),o_wantvisit_Inh_Nonterminal :: !(Bool),prefix_Inh_Nonterminal :: !(String),vcount_Inh_Nonterminal :: !(Int)}
data Syn_Nonterminal  = Syn_Nonterminal {acount_Syn_Nonterminal :: !(Int),additionalDep_Syn_Nonterminal :: !(Seq Edge),aranges_Syn_Nonterminal :: !(Seq (Int,Int,Int)),cNonterminal_Syn_Nonterminal :: !(CNonterminal),directDep_Syn_Nonterminal :: !(Seq Edge),errors_Syn_Nonterminal :: !(Seq Error),instDep_Syn_Nonterminal :: !(Seq Edge),nonts_Syn_Nonterminal :: !([(NontermIdent,[ConstructorIdent])]),ntattrs_Syn_Nonterminal :: !(Seq (Vertex,NTAttr)),rules_Syn_Nonterminal :: !(Seq (Vertex,CRule)),vcount_Syn_Nonterminal :: !(Int)}
wrap_Nonterminal :: T_Nonterminal  ->
                    Inh_Nonterminal  ->
                    Syn_Nonterminal 
wrap_Nonterminal (T_Nonterminal sem ) (Inh_Nonterminal _lhsIacount _lhsIallnts _lhsIcInterfaceMap _lhsIcVisitsMap _lhsImanualAttrDepMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIvcount )  =
    (let ( _lhsOacount,_lhsOadditionalDep,_lhsOaranges,_lhsOcNonterminal,_lhsOdirectDep,_lhsOerrors,_lhsOinstDep,_lhsOnonts,_lhsOntattrs,_lhsOrules,_lhsOvcount) =
             (sem _lhsIacount _lhsIallnts _lhsIcInterfaceMap _lhsIcVisitsMap _lhsImanualAttrDepMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIvcount )
     in  (Syn_Nonterminal _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOcNonterminal _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOnonts _lhsOntattrs _lhsOrules _lhsOvcount ))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions  ->
                               T_Nonterminal 
sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ (T_Productions prods_ )  =
    (T_Nonterminal (\ _lhsIacount
                      _lhsIallnts
                      _lhsIcInterfaceMap
                      _lhsIcVisitsMap
                      _lhsImanualAttrDepMap
                      _lhsIo_case
                      _lhsIo_cata
                      _lhsIo_data
                      _lhsIo_dovisit
                      _lhsIo_newtypes
                      _lhsIo_rename
                      _lhsIo_sem
                      _lhsIo_sig
                      _lhsIo_unbox
                      _lhsIo_wantvisit
                      _lhsIprefix
                      _lhsIvcount ->
                        (let _prodsOnt :: Identifier
                             _prodsOinh :: Attributes
                             _prodsOsyn :: Attributes
                             _lhsOntattrs :: (Seq (Vertex,NTAttr))
                             _lhsOacount :: Int
                             _lhsOaranges :: (Seq (Int,Int,Int))
                             _lhsOnonts :: ([(NontermIdent,[ConstructorIdent])])
                             _lhsOcNonterminal :: CNonterminal
                             _lhsOadditionalDep :: (Seq Edge)
                             _lhsOdirectDep :: (Seq Edge)
                             _lhsOerrors :: (Seq Error)
                             _lhsOinstDep :: (Seq Edge)
                             _lhsOrules :: (Seq (Vertex,CRule))
                             _lhsOvcount :: Int
                             _prodsOallnts :: ([Identifier])
                             _prodsOcVisitsMap :: CVisitsMap
                             _prodsOmanualAttrDepMap :: AttrOrderMap
                             _prodsOo_case :: Bool
                             _prodsOo_cata :: Bool
                             _prodsOo_dovisit :: Bool
                             _prodsOo_newtypes :: Bool
                             _prodsOo_rename :: Bool
                             _prodsOo_sem :: Bool
                             _prodsOo_sig :: Bool
                             _prodsOo_unbox :: Bool
                             _prodsOo_wantvisit :: Bool
                             _prodsOprefix :: String
                             _prodsOvcount :: Int
                             _prodsIadditionalDep :: (Seq Edge)
                             _prodsIcProductions :: CProductions
                             _prodsIcons :: ([ConstructorIdent])
                             _prodsIdirectDep :: (Seq Edge)
                             _prodsIerrors :: (Seq Error)
                             _prodsIinstDep :: (Seq Edge)
                             _prodsIrules :: (Seq (Vertex,CRule))
                             _prodsIvcount :: Int
                             -- "Order.ag"(line 77, column 17)
                             _prodsOnt =
                                 nt_
                             -- "Order.ag"(line 80, column 17)
                             _prodsOinh =
                                 inh_
                             -- "Order.ag"(line 81, column 17)
                             _prodsOsyn =
                                 syn_
                             -- "Order.ag"(line 372, column 17)
                             _ntattrs =
                                 [ NTAInh nt_ inh tp | (inh,tp) <- Map.assocs inh_ ]
                                 ++ [NTASyn nt_ syn tp | (syn,tp) <- Map.assocs syn_ ]
                             -- "Order.ag"(line 374, column 17)
                             _lhsOntattrs =
                                 Seq.fromList (zip [_lhsIacount ..] _ntattrs)
                             -- "Order.ag"(line 375, column 17)
                             _lhsOacount =
                                 _lhsIacount + Map.size inh_ + Map.size syn_
                             -- "Order.ag"(line 376, column 17)
                             _lhsOaranges =
                                 Seq.singleton
                                  (_lhsIacount
                                  ,_lhsIacount + Map.size inh_
                                  ,_lhsIacount + Map.size syn_ + Map.size inh_ - 1)
                             -- "Order.ag"(line 385, column 19)
                             _lhsOnonts =
                                 [(nt_,_prodsIcons)]
                             -- "Order.ag"(line 460, column 19)
                             _cInter =
                                 if  _lhsIo_dovisit
                                        then findWithErr1 "Nonterminal.cInter" nt_ _lhsIcInterfaceMap
                                        else CInterface [CSegment inh_ syn_]
                             -- "Order.ag"(line 488, column 19)
                             _lhsOcNonterminal =
                                 CNonterminal nt_ params_ inh_ syn_ _prodsIcProductions _cInter
                             -- use rule "Order.ag"(line 263, column 71)
                             _lhsOadditionalDep =
                                 _prodsIadditionalDep
                             -- use rule "Order.ag"(line 249, column 33)
                             _lhsOdirectDep =
                                 _prodsIdirectDep
                             -- use rule "Order.ag"(line 64, column 70)
                             _lhsOerrors =
                                 _prodsIerrors
                             -- use rule "Order.ag"(line 296, column 31)
                             _lhsOinstDep =
                                 _prodsIinstDep
                             -- use rule "Order.ag"(line 239, column 18)
                             _lhsOrules =
                                 _prodsIrules
                             -- copy rule (up)
                             _lhsOvcount =
                                 _prodsIvcount
                             -- copy rule (down)
                             _prodsOallnts =
                                 _lhsIallnts
                             -- copy rule (down)
                             _prodsOcVisitsMap =
                                 _lhsIcVisitsMap
                             -- copy rule (down)
                             _prodsOmanualAttrDepMap =
                                 _lhsImanualAttrDepMap
                             -- copy rule (down)
                             _prodsOo_case =
                                 _lhsIo_case
                             -- copy rule (down)
                             _prodsOo_cata =
                                 _lhsIo_cata
                             -- copy rule (down)
                             _prodsOo_dovisit =
                                 _lhsIo_dovisit
                             -- copy rule (down)
                             _prodsOo_newtypes =
                                 _lhsIo_newtypes
                             -- copy rule (down)
                             _prodsOo_rename =
                                 _lhsIo_rename
                             -- copy rule (down)
                             _prodsOo_sem =
                                 _lhsIo_sem
                             -- copy rule (down)
                             _prodsOo_sig =
                                 _lhsIo_sig
                             -- copy rule (down)
                             _prodsOo_unbox =
                                 _lhsIo_unbox
                             -- copy rule (down)
                             _prodsOo_wantvisit =
                                 _lhsIo_wantvisit
                             -- copy rule (down)
                             _prodsOprefix =
                                 _lhsIprefix
                             -- copy rule (down)
                             _prodsOvcount =
                                 _lhsIvcount
                             ( _prodsIadditionalDep,_prodsIcProductions,_prodsIcons,_prodsIdirectDep,_prodsIerrors,_prodsIinstDep,_prodsIrules,_prodsIvcount) =
                                 (prods_ _prodsOallnts _prodsOcVisitsMap _prodsOinh _prodsOmanualAttrDepMap _prodsOnt _prodsOo_case _prodsOo_cata _prodsOo_dovisit _prodsOo_newtypes _prodsOo_rename _prodsOo_sem _prodsOo_sig _prodsOo_unbox _prodsOo_wantvisit _prodsOprefix _prodsOsyn _prodsOvcount )
                         in  ( _lhsOacount,_lhsOadditionalDep,_lhsOaranges,_lhsOcNonterminal,_lhsOdirectDep,_lhsOerrors,_lhsOinstDep,_lhsOnonts,_lhsOntattrs,_lhsOrules,_lhsOvcount))) )
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allnts               : [Identifier]
         cInterfaceMap        : CInterfaceMap
         cVisitsMap           : CVisitsMap
         manualAttrDepMap     : AttrOrderMap
         o_case               : Bool
         o_cata               : Bool
         o_data               : Bool
         o_dovisit            : Bool
         o_newtypes           : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_unbox              : Bool
         o_wantvisit          : Bool
         prefix               : String
      chained attributes:
         acount               : Int
         vcount               : Int
      synthesized attributes:
         additionalDep        : Seq Edge
         aranges              : Seq (Int,Int,Int)
         cNonterminals        : CNonterminals
         directDep            : Seq Edge
         errors               : Seq Error
         instDep              : Seq Edge
         nonts                : [(NontermIdent,[ConstructorIdent])]
         ntattrs              : Seq (Vertex,NTAttr)
         rules                : Seq (Vertex,CRule)
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
newtype T_Nonterminals  = T_Nonterminals (Int ->
                                          ([Identifier]) ->
                                          CInterfaceMap ->
                                          CVisitsMap ->
                                          AttrOrderMap ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          String ->
                                          Int ->
                                          ( Int,(Seq Edge),(Seq (Int,Int,Int)),CNonterminals,(Seq Edge),(Seq Error),(Seq Edge),([(NontermIdent,[ConstructorIdent])]),(Seq (Vertex,NTAttr)),(Seq (Vertex,CRule)),Int))
data Inh_Nonterminals  = Inh_Nonterminals {acount_Inh_Nonterminals :: !(Int),allnts_Inh_Nonterminals :: !([Identifier]),cInterfaceMap_Inh_Nonterminals :: !(CInterfaceMap),cVisitsMap_Inh_Nonterminals :: !(CVisitsMap),manualAttrDepMap_Inh_Nonterminals :: !(AttrOrderMap),o_case_Inh_Nonterminals :: !(Bool),o_cata_Inh_Nonterminals :: !(Bool),o_data_Inh_Nonterminals :: !(Bool),o_dovisit_Inh_Nonterminals :: !(Bool),o_newtypes_Inh_Nonterminals :: !(Bool),o_rename_Inh_Nonterminals :: !(Bool),o_sem_Inh_Nonterminals :: !(Bool),o_sig_Inh_Nonterminals :: !(Bool),o_unbox_Inh_Nonterminals :: !(Bool),o_wantvisit_Inh_Nonterminals :: !(Bool),prefix_Inh_Nonterminals :: !(String),vcount_Inh_Nonterminals :: !(Int)}
data Syn_Nonterminals  = Syn_Nonterminals {acount_Syn_Nonterminals :: !(Int),additionalDep_Syn_Nonterminals :: !(Seq Edge),aranges_Syn_Nonterminals :: !(Seq (Int,Int,Int)),cNonterminals_Syn_Nonterminals :: !(CNonterminals),directDep_Syn_Nonterminals :: !(Seq Edge),errors_Syn_Nonterminals :: !(Seq Error),instDep_Syn_Nonterminals :: !(Seq Edge),nonts_Syn_Nonterminals :: !([(NontermIdent,[ConstructorIdent])]),ntattrs_Syn_Nonterminals :: !(Seq (Vertex,NTAttr)),rules_Syn_Nonterminals :: !(Seq (Vertex,CRule)),vcount_Syn_Nonterminals :: !(Int)}
wrap_Nonterminals :: T_Nonterminals  ->
                     Inh_Nonterminals  ->
                     Syn_Nonterminals 
wrap_Nonterminals (T_Nonterminals sem ) (Inh_Nonterminals _lhsIacount _lhsIallnts _lhsIcInterfaceMap _lhsIcVisitsMap _lhsImanualAttrDepMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIvcount )  =
    (let ( _lhsOacount,_lhsOadditionalDep,_lhsOaranges,_lhsOcNonterminals,_lhsOdirectDep,_lhsOerrors,_lhsOinstDep,_lhsOnonts,_lhsOntattrs,_lhsOrules,_lhsOvcount) =
             (sem _lhsIacount _lhsIallnts _lhsIcInterfaceMap _lhsIcVisitsMap _lhsImanualAttrDepMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIvcount )
     in  (Syn_Nonterminals _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOcNonterminals _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOnonts _lhsOntattrs _lhsOrules _lhsOvcount ))
sem_Nonterminals_Cons :: T_Nonterminal  ->
                         T_Nonterminals  ->
                         T_Nonterminals 
sem_Nonterminals_Cons (T_Nonterminal hd_ ) (T_Nonterminals tl_ )  =
    (T_Nonterminals (\ _lhsIacount
                       _lhsIallnts
                       _lhsIcInterfaceMap
                       _lhsIcVisitsMap
                       _lhsImanualAttrDepMap
                       _lhsIo_case
                       _lhsIo_cata
                       _lhsIo_data
                       _lhsIo_dovisit
                       _lhsIo_newtypes
                       _lhsIo_rename
                       _lhsIo_sem
                       _lhsIo_sig
                       _lhsIo_unbox
                       _lhsIo_wantvisit
                       _lhsIprefix
                       _lhsIvcount ->
                         (let _lhsOcNonterminals :: CNonterminals
                              _lhsOadditionalDep :: (Seq Edge)
                              _lhsOaranges :: (Seq (Int,Int,Int))
                              _lhsOdirectDep :: (Seq Edge)
                              _lhsOerrors :: (Seq Error)
                              _lhsOinstDep :: (Seq Edge)
                              _lhsOnonts :: ([(NontermIdent,[ConstructorIdent])])
                              _lhsOntattrs :: (Seq (Vertex,NTAttr))
                              _lhsOrules :: (Seq (Vertex,CRule))
                              _lhsOacount :: Int
                              _lhsOvcount :: Int
                              _hdOacount :: Int
                              _hdOallnts :: ([Identifier])
                              _hdOcInterfaceMap :: CInterfaceMap
                              _hdOcVisitsMap :: CVisitsMap
                              _hdOmanualAttrDepMap :: AttrOrderMap
                              _hdOo_case :: Bool
                              _hdOo_cata :: Bool
                              _hdOo_data :: Bool
                              _hdOo_dovisit :: Bool
                              _hdOo_newtypes :: Bool
                              _hdOo_rename :: Bool
                              _hdOo_sem :: Bool
                              _hdOo_sig :: Bool
                              _hdOo_unbox :: Bool
                              _hdOo_wantvisit :: Bool
                              _hdOprefix :: String
                              _hdOvcount :: Int
                              _tlOacount :: Int
                              _tlOallnts :: ([Identifier])
                              _tlOcInterfaceMap :: CInterfaceMap
                              _tlOcVisitsMap :: CVisitsMap
                              _tlOmanualAttrDepMap :: AttrOrderMap
                              _tlOo_case :: Bool
                              _tlOo_cata :: Bool
                              _tlOo_data :: Bool
                              _tlOo_dovisit :: Bool
                              _tlOo_newtypes :: Bool
                              _tlOo_rename :: Bool
                              _tlOo_sem :: Bool
                              _tlOo_sig :: Bool
                              _tlOo_unbox :: Bool
                              _tlOo_wantvisit :: Bool
                              _tlOprefix :: String
                              _tlOvcount :: Int
                              _hdIacount :: Int
                              _hdIadditionalDep :: (Seq Edge)
                              _hdIaranges :: (Seq (Int,Int,Int))
                              _hdIcNonterminal :: CNonterminal
                              _hdIdirectDep :: (Seq Edge)
                              _hdIerrors :: (Seq Error)
                              _hdIinstDep :: (Seq Edge)
                              _hdInonts :: ([(NontermIdent,[ConstructorIdent])])
                              _hdIntattrs :: (Seq (Vertex,NTAttr))
                              _hdIrules :: (Seq (Vertex,CRule))
                              _hdIvcount :: Int
                              _tlIacount :: Int
                              _tlIadditionalDep :: (Seq Edge)
                              _tlIaranges :: (Seq (Int,Int,Int))
                              _tlIcNonterminals :: CNonterminals
                              _tlIdirectDep :: (Seq Edge)
                              _tlIerrors :: (Seq Error)
                              _tlIinstDep :: (Seq Edge)
                              _tlInonts :: ([(NontermIdent,[ConstructorIdent])])
                              _tlIntattrs :: (Seq (Vertex,NTAttr))
                              _tlIrules :: (Seq (Vertex,CRule))
                              _tlIvcount :: Int
                              -- "Order.ag"(line 485, column 12)
                              _lhsOcNonterminals =
                                  _hdIcNonterminal : _tlIcNonterminals
                              -- use rule "Order.ag"(line 263, column 71)
                              _lhsOadditionalDep =
                                  _hdIadditionalDep Seq.>< _tlIadditionalDep
                              -- use rule "Order.ag"(line 366, column 36)
                              _lhsOaranges =
                                  _hdIaranges Seq.>< _tlIaranges
                              -- use rule "Order.ag"(line 249, column 33)
                              _lhsOdirectDep =
                                  _hdIdirectDep Seq.>< _tlIdirectDep
                              -- use rule "Order.ag"(line 64, column 70)
                              _lhsOerrors =
                                  _hdIerrors Seq.>< _tlIerrors
                              -- use rule "Order.ag"(line 296, column 31)
                              _lhsOinstDep =
                                  _hdIinstDep Seq.>< _tlIinstDep
                              -- use rule "Order.ag"(line 383, column 43)
                              _lhsOnonts =
                                  _hdInonts ++ _tlInonts
                              -- use rule "Order.ag"(line 365, column 35)
                              _lhsOntattrs =
                                  _hdIntattrs Seq.>< _tlIntattrs
                              -- use rule "Order.ag"(line 239, column 18)
                              _lhsOrules =
                                  _hdIrules Seq.>< _tlIrules
                              -- copy rule (up)
                              _lhsOacount =
                                  _tlIacount
                              -- copy rule (up)
                              _lhsOvcount =
                                  _tlIvcount
                              -- copy rule (down)
                              _hdOacount =
                                  _lhsIacount
                              -- copy rule (down)
                              _hdOallnts =
                                  _lhsIallnts
                              -- copy rule (down)
                              _hdOcInterfaceMap =
                                  _lhsIcInterfaceMap
                              -- copy rule (down)
                              _hdOcVisitsMap =
                                  _lhsIcVisitsMap
                              -- copy rule (down)
                              _hdOmanualAttrDepMap =
                                  _lhsImanualAttrDepMap
                              -- copy rule (down)
                              _hdOo_case =
                                  _lhsIo_case
                              -- copy rule (down)
                              _hdOo_cata =
                                  _lhsIo_cata
                              -- copy rule (down)
                              _hdOo_data =
                                  _lhsIo_data
                              -- copy rule (down)
                              _hdOo_dovisit =
                                  _lhsIo_dovisit
                              -- copy rule (down)
                              _hdOo_newtypes =
                                  _lhsIo_newtypes
                              -- copy rule (down)
                              _hdOo_rename =
                                  _lhsIo_rename
                              -- copy rule (down)
                              _hdOo_sem =
                                  _lhsIo_sem
                              -- copy rule (down)
                              _hdOo_sig =
                                  _lhsIo_sig
                              -- copy rule (down)
                              _hdOo_unbox =
                                  _lhsIo_unbox
                              -- copy rule (down)
                              _hdOo_wantvisit =
                                  _lhsIo_wantvisit
                              -- copy rule (down)
                              _hdOprefix =
                                  _lhsIprefix
                              -- copy rule (down)
                              _hdOvcount =
                                  _lhsIvcount
                              -- copy rule (chain)
                              _tlOacount =
                                  _hdIacount
                              -- copy rule (down)
                              _tlOallnts =
                                  _lhsIallnts
                              -- copy rule (down)
                              _tlOcInterfaceMap =
                                  _lhsIcInterfaceMap
                              -- copy rule (down)
                              _tlOcVisitsMap =
                                  _lhsIcVisitsMap
                              -- copy rule (down)
                              _tlOmanualAttrDepMap =
                                  _lhsImanualAttrDepMap
                              -- copy rule (down)
                              _tlOo_case =
                                  _lhsIo_case
                              -- copy rule (down)
                              _tlOo_cata =
                                  _lhsIo_cata
                              -- copy rule (down)
                              _tlOo_data =
                                  _lhsIo_data
                              -- copy rule (down)
                              _tlOo_dovisit =
                                  _lhsIo_dovisit
                              -- copy rule (down)
                              _tlOo_newtypes =
                                  _lhsIo_newtypes
                              -- copy rule (down)
                              _tlOo_rename =
                                  _lhsIo_rename
                              -- copy rule (down)
                              _tlOo_sem =
                                  _lhsIo_sem
                              -- copy rule (down)
                              _tlOo_sig =
                                  _lhsIo_sig
                              -- copy rule (down)
                              _tlOo_unbox =
                                  _lhsIo_unbox
                              -- copy rule (down)
                              _tlOo_wantvisit =
                                  _lhsIo_wantvisit
                              -- copy rule (down)
                              _tlOprefix =
                                  _lhsIprefix
                              -- copy rule (chain)
                              _tlOvcount =
                                  _hdIvcount
                              ( _hdIacount,_hdIadditionalDep,_hdIaranges,_hdIcNonterminal,_hdIdirectDep,_hdIerrors,_hdIinstDep,_hdInonts,_hdIntattrs,_hdIrules,_hdIvcount) =
                                  (hd_ _hdOacount _hdOallnts _hdOcInterfaceMap _hdOcVisitsMap _hdOmanualAttrDepMap _hdOo_case _hdOo_cata _hdOo_data _hdOo_dovisit _hdOo_newtypes _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_unbox _hdOo_wantvisit _hdOprefix _hdOvcount )
                              ( _tlIacount,_tlIadditionalDep,_tlIaranges,_tlIcNonterminals,_tlIdirectDep,_tlIerrors,_tlIinstDep,_tlInonts,_tlIntattrs,_tlIrules,_tlIvcount) =
                                  (tl_ _tlOacount _tlOallnts _tlOcInterfaceMap _tlOcVisitsMap _tlOmanualAttrDepMap _tlOo_case _tlOo_cata _tlOo_data _tlOo_dovisit _tlOo_newtypes _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_unbox _tlOo_wantvisit _tlOprefix _tlOvcount )
                          in  ( _lhsOacount,_lhsOadditionalDep,_lhsOaranges,_lhsOcNonterminals,_lhsOdirectDep,_lhsOerrors,_lhsOinstDep,_lhsOnonts,_lhsOntattrs,_lhsOrules,_lhsOvcount))) )
sem_Nonterminals_Nil :: T_Nonterminals 
sem_Nonterminals_Nil  =
    (T_Nonterminals (\ _lhsIacount
                       _lhsIallnts
                       _lhsIcInterfaceMap
                       _lhsIcVisitsMap
                       _lhsImanualAttrDepMap
                       _lhsIo_case
                       _lhsIo_cata
                       _lhsIo_data
                       _lhsIo_dovisit
                       _lhsIo_newtypes
                       _lhsIo_rename
                       _lhsIo_sem
                       _lhsIo_sig
                       _lhsIo_unbox
                       _lhsIo_wantvisit
                       _lhsIprefix
                       _lhsIvcount ->
                         (let _lhsOcNonterminals :: CNonterminals
                              _lhsOadditionalDep :: (Seq Edge)
                              _lhsOaranges :: (Seq (Int,Int,Int))
                              _lhsOdirectDep :: (Seq Edge)
                              _lhsOerrors :: (Seq Error)
                              _lhsOinstDep :: (Seq Edge)
                              _lhsOnonts :: ([(NontermIdent,[ConstructorIdent])])
                              _lhsOntattrs :: (Seq (Vertex,NTAttr))
                              _lhsOrules :: (Seq (Vertex,CRule))
                              _lhsOacount :: Int
                              _lhsOvcount :: Int
                              -- "Order.ag"(line 486, column 12)
                              _lhsOcNonterminals =
                                  []
                              -- use rule "Order.ag"(line 263, column 71)
                              _lhsOadditionalDep =
                                  Seq.empty
                              -- use rule "Order.ag"(line 366, column 36)
                              _lhsOaranges =
                                  Seq.empty
                              -- use rule "Order.ag"(line 249, column 33)
                              _lhsOdirectDep =
                                  Seq.empty
                              -- use rule "Order.ag"(line 64, column 70)
                              _lhsOerrors =
                                  Seq.empty
                              -- use rule "Order.ag"(line 296, column 31)
                              _lhsOinstDep =
                                  Seq.empty
                              -- use rule "Order.ag"(line 383, column 43)
                              _lhsOnonts =
                                  []
                              -- use rule "Order.ag"(line 365, column 35)
                              _lhsOntattrs =
                                  Seq.empty
                              -- use rule "Order.ag"(line 239, column 18)
                              _lhsOrules =
                                  Seq.empty
                              -- copy rule (chain)
                              _lhsOacount =
                                  _lhsIacount
                              -- copy rule (chain)
                              _lhsOvcount =
                                  _lhsIvcount
                          in  ( _lhsOacount,_lhsOadditionalDep,_lhsOaranges,_lhsOcNonterminals,_lhsOdirectDep,_lhsOerrors,_lhsOinstDep,_lhsOnonts,_lhsOntattrs,_lhsOrules,_lhsOvcount))) )
-- Pattern -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allTypeSigs          : Map Identifier Type
         altAttrs             : Map AltAttr Vertex
         con                  : Identifier
         inh                  : Attributes
         nt                   : Identifier
         syn                  : Attributes
      synthesized attributes:
         copy                 : SELF 
         errors               : Seq Error
         gathAltAttrs         : [AltAttr]
         instVars             : [Identifier]
         locVars              : [Identifier]
         patternAttrs         : [(Identifier,Identifier,Bool,Patterns)]
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
newtype T_Pattern  = T_Pattern ((Map Identifier Type) ->
                                (Map AltAttr Vertex) ->
                                Identifier ->
                                Attributes ->
                                Identifier ->
                                Attributes ->
                                ( Pattern,(Seq Error),([AltAttr]),([Identifier]),([Identifier]),([(Identifier,Identifier,Bool,Patterns)])))
data Inh_Pattern  = Inh_Pattern {allTypeSigs_Inh_Pattern :: !(Map Identifier Type),altAttrs_Inh_Pattern :: !(Map AltAttr Vertex),con_Inh_Pattern :: !(Identifier),inh_Inh_Pattern :: !(Attributes),nt_Inh_Pattern :: !(Identifier),syn_Inh_Pattern :: !(Attributes)}
data Syn_Pattern  = Syn_Pattern {copy_Syn_Pattern :: !(Pattern),errors_Syn_Pattern :: !(Seq Error),gathAltAttrs_Syn_Pattern :: !([AltAttr]),instVars_Syn_Pattern :: !([Identifier]),locVars_Syn_Pattern :: !([Identifier]),patternAttrs_Syn_Pattern :: !([(Identifier,Identifier,Bool,Patterns)])}
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern (T_Pattern sem ) (Inh_Pattern _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn )  =
    (let ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs) =
             (sem _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn )
     in  (Syn_Pattern _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Alias field_ attr_ (T_Pattern pat_ ) (T_Patterns parts_ )  =
    (T_Pattern (\ _lhsIallTypeSigs
                  _lhsIaltAttrs
                  _lhsIcon
                  _lhsIinh
                  _lhsInt
                  _lhsIsyn ->
                    (let _lhsOgathAltAttrs :: ([AltAttr])
                         _lhsOpatternAttrs :: ([(Identifier,Identifier,Bool,Patterns)])
                         _lhsOlocVars :: ([Identifier])
                         _lhsOinstVars :: ([Identifier])
                         _lhsOerrors :: (Seq Error)
                         _lhsOcopy :: Pattern
                         _patOallTypeSigs :: (Map Identifier Type)
                         _patOaltAttrs :: (Map AltAttr Vertex)
                         _patOcon :: Identifier
                         _patOinh :: Attributes
                         _patOnt :: Identifier
                         _patOsyn :: Attributes
                         _partsOallTypeSigs :: (Map Identifier Type)
                         _partsOaltAttrs :: (Map AltAttr Vertex)
                         _partsOcon :: Identifier
                         _partsOinh :: Attributes
                         _partsOnt :: Identifier
                         _partsOsyn :: Attributes
                         _patIcopy :: Pattern
                         _patIerrors :: (Seq Error)
                         _patIgathAltAttrs :: ([AltAttr])
                         _patIinstVars :: ([Identifier])
                         _patIlocVars :: ([Identifier])
                         _patIpatternAttrs :: ([(Identifier,Identifier,Bool,Patterns)])
                         _partsIcopy :: Patterns
                         _partsIerrors :: (Seq Error)
                         _partsIgathAltAttrs :: ([AltAttr])
                         _partsIinstVars :: ([Identifier])
                         _partsIlocVars :: ([Identifier])
                         _partsIpatternAttrs :: ([(Identifier,Identifier,Bool,Patterns)])
                         -- "Order.ag"(line 164, column 12)
                         _lhsOgathAltAttrs =
                             [AltAttr field_ attr_ (field_ == _LOC || field_ == _INST)]
                         -- "Order.ag"(line 232, column 12)
                         _lhsOpatternAttrs =
                             [(field_,attr_,(field_ == _LOC || field_ == _INST),_partsIcopy)]
                         -- "Order.ag"(line 540, column 14)
                         _lhsOlocVars =
                             if field_ == _LOC
                                then [attr_]
                                else []
                         -- "Order.ag"(line 543, column 14)
                         _lhsOinstVars =
                             if field_ == _INST
                                then [attr_]
                                else []
                         -- use rule "Order.ag"(line 64, column 70)
                         _lhsOerrors =
                             _patIerrors Seq.>< _partsIerrors
                         -- self rule
                         _copy =
                             Alias field_ attr_ _patIcopy _partsIcopy
                         -- self rule
                         _lhsOcopy =
                             _copy
                         -- copy rule (down)
                         _patOallTypeSigs =
                             _lhsIallTypeSigs
                         -- copy rule (down)
                         _patOaltAttrs =
                             _lhsIaltAttrs
                         -- copy rule (down)
                         _patOcon =
                             _lhsIcon
                         -- copy rule (down)
                         _patOinh =
                             _lhsIinh
                         -- copy rule (down)
                         _patOnt =
                             _lhsInt
                         -- copy rule (down)
                         _patOsyn =
                             _lhsIsyn
                         -- copy rule (down)
                         _partsOallTypeSigs =
                             _lhsIallTypeSigs
                         -- copy rule (down)
                         _partsOaltAttrs =
                             _lhsIaltAttrs
                         -- copy rule (down)
                         _partsOcon =
                             _lhsIcon
                         -- copy rule (down)
                         _partsOinh =
                             _lhsIinh
                         -- copy rule (down)
                         _partsOnt =
                             _lhsInt
                         -- copy rule (down)
                         _partsOsyn =
                             _lhsIsyn
                         ( _patIcopy,_patIerrors,_patIgathAltAttrs,_patIinstVars,_patIlocVars,_patIpatternAttrs) =
                             (pat_ _patOallTypeSigs _patOaltAttrs _patOcon _patOinh _patOnt _patOsyn )
                         ( _partsIcopy,_partsIerrors,_partsIgathAltAttrs,_partsIinstVars,_partsIlocVars,_partsIpatternAttrs) =
                             (parts_ _partsOallTypeSigs _partsOaltAttrs _partsOcon _partsOinh _partsOnt _partsOsyn )
                     in  ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs))) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr name_ (T_Patterns pats_ )  =
    (T_Pattern (\ _lhsIallTypeSigs
                  _lhsIaltAttrs
                  _lhsIcon
                  _lhsIinh
                  _lhsInt
                  _lhsIsyn ->
                    (let _lhsOerrors :: (Seq Error)
                         _lhsOgathAltAttrs :: ([AltAttr])
                         _lhsOinstVars :: ([Identifier])
                         _lhsOlocVars :: ([Identifier])
                         _lhsOpatternAttrs :: ([(Identifier,Identifier,Bool,Patterns)])
                         _lhsOcopy :: Pattern
                         _patsOallTypeSigs :: (Map Identifier Type)
                         _patsOaltAttrs :: (Map AltAttr Vertex)
                         _patsOcon :: Identifier
                         _patsOinh :: Attributes
                         _patsOnt :: Identifier
                         _patsOsyn :: Attributes
                         _patsIcopy :: Patterns
                         _patsIerrors :: (Seq Error)
                         _patsIgathAltAttrs :: ([AltAttr])
                         _patsIinstVars :: ([Identifier])
                         _patsIlocVars :: ([Identifier])
                         _patsIpatternAttrs :: ([(Identifier,Identifier,Bool,Patterns)])
                         -- use rule "Order.ag"(line 64, column 70)
                         _lhsOerrors =
                             _patsIerrors
                         -- use rule "Order.ag"(line 150, column 68)
                         _lhsOgathAltAttrs =
                             _patsIgathAltAttrs
                         -- use rule "Order.ag"(line 537, column 86)
                         _lhsOinstVars =
                             _patsIinstVars
                         -- use rule "Order.ag"(line 537, column 48)
                         _lhsOlocVars =
                             _patsIlocVars
                         -- use rule "Order.ag"(line 229, column 42)
                         _lhsOpatternAttrs =
                             _patsIpatternAttrs
                         -- self rule
                         _copy =
                             Constr name_ _patsIcopy
                         -- self rule
                         _lhsOcopy =
                             _copy
                         -- copy rule (down)
                         _patsOallTypeSigs =
                             _lhsIallTypeSigs
                         -- copy rule (down)
                         _patsOaltAttrs =
                             _lhsIaltAttrs
                         -- copy rule (down)
                         _patsOcon =
                             _lhsIcon
                         -- copy rule (down)
                         _patsOinh =
                             _lhsIinh
                         -- copy rule (down)
                         _patsOnt =
                             _lhsInt
                         -- copy rule (down)
                         _patsOsyn =
                             _lhsIsyn
                         ( _patsIcopy,_patsIerrors,_patsIgathAltAttrs,_patsIinstVars,_patsIlocVars,_patsIpatternAttrs) =
                             (pats_ _patsOallTypeSigs _patsOaltAttrs _patsOcon _patsOinh _patsOnt _patsOsyn )
                     in  ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs))) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable (T_Pattern pat_ )  =
    (T_Pattern (\ _lhsIallTypeSigs
                  _lhsIaltAttrs
                  _lhsIcon
                  _lhsIinh
                  _lhsInt
                  _lhsIsyn ->
                    (let _lhsOerrors :: (Seq Error)
                         _lhsOgathAltAttrs :: ([AltAttr])
                         _lhsOinstVars :: ([Identifier])
                         _lhsOlocVars :: ([Identifier])
                         _lhsOpatternAttrs :: ([(Identifier,Identifier,Bool,Patterns)])
                         _lhsOcopy :: Pattern
                         _patOallTypeSigs :: (Map Identifier Type)
                         _patOaltAttrs :: (Map AltAttr Vertex)
                         _patOcon :: Identifier
                         _patOinh :: Attributes
                         _patOnt :: Identifier
                         _patOsyn :: Attributes
                         _patIcopy :: Pattern
                         _patIerrors :: (Seq Error)
                         _patIgathAltAttrs :: ([AltAttr])
                         _patIinstVars :: ([Identifier])
                         _patIlocVars :: ([Identifier])
                         _patIpatternAttrs :: ([(Identifier,Identifier,Bool,Patterns)])
                         -- use rule "Order.ag"(line 64, column 70)
                         _lhsOerrors =
                             _patIerrors
                         -- use rule "Order.ag"(line 150, column 68)
                         _lhsOgathAltAttrs =
                             _patIgathAltAttrs
                         -- use rule "Order.ag"(line 537, column 86)
                         _lhsOinstVars =
                             _patIinstVars
                         -- use rule "Order.ag"(line 537, column 48)
                         _lhsOlocVars =
                             _patIlocVars
                         -- use rule "Order.ag"(line 229, column 42)
                         _lhsOpatternAttrs =
                             _patIpatternAttrs
                         -- self rule
                         _copy =
                             Irrefutable _patIcopy
                         -- self rule
                         _lhsOcopy =
                             _copy
                         -- copy rule (down)
                         _patOallTypeSigs =
                             _lhsIallTypeSigs
                         -- copy rule (down)
                         _patOaltAttrs =
                             _lhsIaltAttrs
                         -- copy rule (down)
                         _patOcon =
                             _lhsIcon
                         -- copy rule (down)
                         _patOinh =
                             _lhsIinh
                         -- copy rule (down)
                         _patOnt =
                             _lhsInt
                         -- copy rule (down)
                         _patOsyn =
                             _lhsIsyn
                         ( _patIcopy,_patIerrors,_patIgathAltAttrs,_patIinstVars,_patIlocVars,_patIpatternAttrs) =
                             (pat_ _patOallTypeSigs _patOaltAttrs _patOcon _patOinh _patOnt _patOsyn )
                     in  ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs))) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product pos_ (T_Patterns pats_ )  =
    (T_Pattern (\ _lhsIallTypeSigs
                  _lhsIaltAttrs
                  _lhsIcon
                  _lhsIinh
                  _lhsInt
                  _lhsIsyn ->
                    (let _lhsOerrors :: (Seq Error)
                         _lhsOgathAltAttrs :: ([AltAttr])
                         _lhsOinstVars :: ([Identifier])
                         _lhsOlocVars :: ([Identifier])
                         _lhsOpatternAttrs :: ([(Identifier,Identifier,Bool,Patterns)])
                         _lhsOcopy :: Pattern
                         _patsOallTypeSigs :: (Map Identifier Type)
                         _patsOaltAttrs :: (Map AltAttr Vertex)
                         _patsOcon :: Identifier
                         _patsOinh :: Attributes
                         _patsOnt :: Identifier
                         _patsOsyn :: Attributes
                         _patsIcopy :: Patterns
                         _patsIerrors :: (Seq Error)
                         _patsIgathAltAttrs :: ([AltAttr])
                         _patsIinstVars :: ([Identifier])
                         _patsIlocVars :: ([Identifier])
                         _patsIpatternAttrs :: ([(Identifier,Identifier,Bool,Patterns)])
                         -- use rule "Order.ag"(line 64, column 70)
                         _lhsOerrors =
                             _patsIerrors
                         -- use rule "Order.ag"(line 150, column 68)
                         _lhsOgathAltAttrs =
                             _patsIgathAltAttrs
                         -- use rule "Order.ag"(line 537, column 86)
                         _lhsOinstVars =
                             _patsIinstVars
                         -- use rule "Order.ag"(line 537, column 48)
                         _lhsOlocVars =
                             _patsIlocVars
                         -- use rule "Order.ag"(line 229, column 42)
                         _lhsOpatternAttrs =
                             _patsIpatternAttrs
                         -- self rule
                         _copy =
                             Product pos_ _patsIcopy
                         -- self rule
                         _lhsOcopy =
                             _copy
                         -- copy rule (down)
                         _patsOallTypeSigs =
                             _lhsIallTypeSigs
                         -- copy rule (down)
                         _patsOaltAttrs =
                             _lhsIaltAttrs
                         -- copy rule (down)
                         _patsOcon =
                             _lhsIcon
                         -- copy rule (down)
                         _patsOinh =
                             _lhsIinh
                         -- copy rule (down)
                         _patsOnt =
                             _lhsInt
                         -- copy rule (down)
                         _patsOsyn =
                             _lhsIsyn
                         ( _patsIcopy,_patsIerrors,_patsIgathAltAttrs,_patsIinstVars,_patsIlocVars,_patsIpatternAttrs) =
                             (pats_ _patsOallTypeSigs _patsOaltAttrs _patsOcon _patsOinh _patsOnt _patsOsyn )
                     in  ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs))) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore pos_  =
    (T_Pattern (\ _lhsIallTypeSigs
                  _lhsIaltAttrs
                  _lhsIcon
                  _lhsIinh
                  _lhsInt
                  _lhsIsyn ->
                    (let _lhsOerrors :: (Seq Error)
                         _lhsOgathAltAttrs :: ([AltAttr])
                         _lhsOinstVars :: ([Identifier])
                         _lhsOlocVars :: ([Identifier])
                         _lhsOpatternAttrs :: ([(Identifier,Identifier,Bool,Patterns)])
                         _lhsOcopy :: Pattern
                         -- use rule "Order.ag"(line 64, column 70)
                         _lhsOerrors =
                             Seq.empty
                         -- use rule "Order.ag"(line 150, column 68)
                         _lhsOgathAltAttrs =
                             []
                         -- use rule "Order.ag"(line 537, column 86)
                         _lhsOinstVars =
                             []
                         -- use rule "Order.ag"(line 537, column 48)
                         _lhsOlocVars =
                             []
                         -- use rule "Order.ag"(line 229, column 42)
                         _lhsOpatternAttrs =
                             []
                         -- self rule
                         _copy =
                             Underscore pos_
                         -- self rule
                         _lhsOcopy =
                             _copy
                     in  ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs))) )
-- Patterns ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allTypeSigs          : Map Identifier Type
         altAttrs             : Map AltAttr Vertex
         con                  : Identifier
         inh                  : Attributes
         nt                   : Identifier
         syn                  : Attributes
      synthesized attributes:
         copy                 : SELF 
         errors               : Seq Error
         gathAltAttrs         : [AltAttr]
         instVars             : [Identifier]
         locVars              : [Identifier]
         patternAttrs         : [(Identifier,Identifier,Bool,Patterns)]
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
newtype T_Patterns  = T_Patterns ((Map Identifier Type) ->
                                  (Map AltAttr Vertex) ->
                                  Identifier ->
                                  Attributes ->
                                  Identifier ->
                                  Attributes ->
                                  ( Patterns,(Seq Error),([AltAttr]),([Identifier]),([Identifier]),([(Identifier,Identifier,Bool,Patterns)])))
data Inh_Patterns  = Inh_Patterns {allTypeSigs_Inh_Patterns :: !(Map Identifier Type),altAttrs_Inh_Patterns :: !(Map AltAttr Vertex),con_Inh_Patterns :: !(Identifier),inh_Inh_Patterns :: !(Attributes),nt_Inh_Patterns :: !(Identifier),syn_Inh_Patterns :: !(Attributes)}
data Syn_Patterns  = Syn_Patterns {copy_Syn_Patterns :: !(Patterns),errors_Syn_Patterns :: !(Seq Error),gathAltAttrs_Syn_Patterns :: !([AltAttr]),instVars_Syn_Patterns :: !([Identifier]),locVars_Syn_Patterns :: !([Identifier]),patternAttrs_Syn_Patterns :: !([(Identifier,Identifier,Bool,Patterns)])}
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns (T_Patterns sem ) (Inh_Patterns _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn )  =
    (let ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs) =
             (sem _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn )
     in  (Syn_Patterns _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons (T_Pattern hd_ ) (T_Patterns tl_ )  =
    (T_Patterns (\ _lhsIallTypeSigs
                   _lhsIaltAttrs
                   _lhsIcon
                   _lhsIinh
                   _lhsInt
                   _lhsIsyn ->
                     (let _lhsOerrors :: (Seq Error)
                          _lhsOgathAltAttrs :: ([AltAttr])
                          _lhsOinstVars :: ([Identifier])
                          _lhsOlocVars :: ([Identifier])
                          _lhsOpatternAttrs :: ([(Identifier,Identifier,Bool,Patterns)])
                          _lhsOcopy :: Patterns
                          _hdOallTypeSigs :: (Map Identifier Type)
                          _hdOaltAttrs :: (Map AltAttr Vertex)
                          _hdOcon :: Identifier
                          _hdOinh :: Attributes
                          _hdOnt :: Identifier
                          _hdOsyn :: Attributes
                          _tlOallTypeSigs :: (Map Identifier Type)
                          _tlOaltAttrs :: (Map AltAttr Vertex)
                          _tlOcon :: Identifier
                          _tlOinh :: Attributes
                          _tlOnt :: Identifier
                          _tlOsyn :: Attributes
                          _hdIcopy :: Pattern
                          _hdIerrors :: (Seq Error)
                          _hdIgathAltAttrs :: ([AltAttr])
                          _hdIinstVars :: ([Identifier])
                          _hdIlocVars :: ([Identifier])
                          _hdIpatternAttrs :: ([(Identifier,Identifier,Bool,Patterns)])
                          _tlIcopy :: Patterns
                          _tlIerrors :: (Seq Error)
                          _tlIgathAltAttrs :: ([AltAttr])
                          _tlIinstVars :: ([Identifier])
                          _tlIlocVars :: ([Identifier])
                          _tlIpatternAttrs :: ([(Identifier,Identifier,Bool,Patterns)])
                          -- use rule "Order.ag"(line 64, column 70)
                          _lhsOerrors =
                              _hdIerrors Seq.>< _tlIerrors
                          -- use rule "Order.ag"(line 150, column 68)
                          _lhsOgathAltAttrs =
                              _hdIgathAltAttrs ++ _tlIgathAltAttrs
                          -- use rule "Order.ag"(line 537, column 86)
                          _lhsOinstVars =
                              _hdIinstVars ++ _tlIinstVars
                          -- use rule "Order.ag"(line 537, column 48)
                          _lhsOlocVars =
                              _hdIlocVars ++ _tlIlocVars
                          -- use rule "Order.ag"(line 229, column 42)
                          _lhsOpatternAttrs =
                              _hdIpatternAttrs ++ _tlIpatternAttrs
                          -- self rule
                          _copy =
                              (:) _hdIcopy _tlIcopy
                          -- self rule
                          _lhsOcopy =
                              _copy
                          -- copy rule (down)
                          _hdOallTypeSigs =
                              _lhsIallTypeSigs
                          -- copy rule (down)
                          _hdOaltAttrs =
                              _lhsIaltAttrs
                          -- copy rule (down)
                          _hdOcon =
                              _lhsIcon
                          -- copy rule (down)
                          _hdOinh =
                              _lhsIinh
                          -- copy rule (down)
                          _hdOnt =
                              _lhsInt
                          -- copy rule (down)
                          _hdOsyn =
                              _lhsIsyn
                          -- copy rule (down)
                          _tlOallTypeSigs =
                              _lhsIallTypeSigs
                          -- copy rule (down)
                          _tlOaltAttrs =
                              _lhsIaltAttrs
                          -- copy rule (down)
                          _tlOcon =
                              _lhsIcon
                          -- copy rule (down)
                          _tlOinh =
                              _lhsIinh
                          -- copy rule (down)
                          _tlOnt =
                              _lhsInt
                          -- copy rule (down)
                          _tlOsyn =
                              _lhsIsyn
                          ( _hdIcopy,_hdIerrors,_hdIgathAltAttrs,_hdIinstVars,_hdIlocVars,_hdIpatternAttrs) =
                              (hd_ _hdOallTypeSigs _hdOaltAttrs _hdOcon _hdOinh _hdOnt _hdOsyn )
                          ( _tlIcopy,_tlIerrors,_tlIgathAltAttrs,_tlIinstVars,_tlIlocVars,_tlIpatternAttrs) =
                              (tl_ _tlOallTypeSigs _tlOaltAttrs _tlOcon _tlOinh _tlOnt _tlOsyn )
                      in  ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs))) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (\ _lhsIallTypeSigs
                   _lhsIaltAttrs
                   _lhsIcon
                   _lhsIinh
                   _lhsInt
                   _lhsIsyn ->
                     (let _lhsOerrors :: (Seq Error)
                          _lhsOgathAltAttrs :: ([AltAttr])
                          _lhsOinstVars :: ([Identifier])
                          _lhsOlocVars :: ([Identifier])
                          _lhsOpatternAttrs :: ([(Identifier,Identifier,Bool,Patterns)])
                          _lhsOcopy :: Patterns
                          -- use rule "Order.ag"(line 64, column 70)
                          _lhsOerrors =
                              Seq.empty
                          -- use rule "Order.ag"(line 150, column 68)
                          _lhsOgathAltAttrs =
                              []
                          -- use rule "Order.ag"(line 537, column 86)
                          _lhsOinstVars =
                              []
                          -- use rule "Order.ag"(line 537, column 48)
                          _lhsOlocVars =
                              []
                          -- use rule "Order.ag"(line 229, column 42)
                          _lhsOpatternAttrs =
                              []
                          -- self rule
                          _copy =
                              []
                          -- self rule
                          _lhsOcopy =
                              _copy
                      in  ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs))) )
-- Production --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allnts               : [Identifier]
         cVisitsMap           : CVisitsMap
         inh                  : Attributes
         manualAttrDepMap     : AttrOrderMap
         nt                   : Identifier
         o_case               : Bool
         o_cata               : Bool
         o_dovisit            : Bool
         o_newtypes           : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_unbox              : Bool
         o_wantvisit          : Bool
         prefix               : String
         syn                  : Attributes
      chained attribute:
         vcount               : Int
      synthesized attributes:
         additionalDep        : Seq Edge
         cProduction          : CProduction
         cons                 : [ConstructorIdent]
         directDep            : Seq Edge
         errors               : Seq Error
         instDep              : Seq Edge
         rules                : Seq (Vertex,CRule)
   alternatives:
      alternative Production:
         child con            : {ConstructorIdent}
         child children       : Children 
         child rules          : Rules 
         child typeSigs       : TypeSigs 
         visit 0:
            local gathAltAttrs : _
            local inhRules    : _
            local gathRules   : _
            local cVisits     : _
            local allfields   : _
            local attrs       : _
            local inhnames    : _
            local synnames    : _
-}
-- cata
sem_Production :: Production  ->
                  T_Production 
sem_Production (Production _con _children _rules _typeSigs )  =
    (sem_Production_Production _con (sem_Children _children ) (sem_Rules _rules ) (sem_TypeSigs _typeSigs ) )
-- semantic domain
newtype T_Production  = T_Production (([Identifier]) ->
                                      CVisitsMap ->
                                      Attributes ->
                                      AttrOrderMap ->
                                      Identifier ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      String ->
                                      Attributes ->
                                      Int ->
                                      ( (Seq Edge),CProduction,([ConstructorIdent]),(Seq Edge),(Seq Error),(Seq Edge),(Seq (Vertex,CRule)),Int))
data Inh_Production  = Inh_Production {allnts_Inh_Production :: !([Identifier]),cVisitsMap_Inh_Production :: !(CVisitsMap),inh_Inh_Production :: !(Attributes),manualAttrDepMap_Inh_Production :: !(AttrOrderMap),nt_Inh_Production :: !(Identifier),o_case_Inh_Production :: !(Bool),o_cata_Inh_Production :: !(Bool),o_dovisit_Inh_Production :: !(Bool),o_newtypes_Inh_Production :: !(Bool),o_rename_Inh_Production :: !(Bool),o_sem_Inh_Production :: !(Bool),o_sig_Inh_Production :: !(Bool),o_unbox_Inh_Production :: !(Bool),o_wantvisit_Inh_Production :: !(Bool),prefix_Inh_Production :: !(String),syn_Inh_Production :: !(Attributes),vcount_Inh_Production :: !(Int)}
data Syn_Production  = Syn_Production {additionalDep_Syn_Production :: !(Seq Edge),cProduction_Syn_Production :: !(CProduction),cons_Syn_Production :: !([ConstructorIdent]),directDep_Syn_Production :: !(Seq Edge),errors_Syn_Production :: !(Seq Error),instDep_Syn_Production :: !(Seq Edge),rules_Syn_Production :: !(Seq (Vertex,CRule)),vcount_Syn_Production :: !(Int)}
wrap_Production :: T_Production  ->
                   Inh_Production  ->
                   Syn_Production 
wrap_Production (T_Production sem ) (Inh_Production _lhsIallnts _lhsIcVisitsMap _lhsIinh _lhsImanualAttrDepMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIvcount )  =
    (let ( _lhsOadditionalDep,_lhsOcProduction,_lhsOcons,_lhsOdirectDep,_lhsOerrors,_lhsOinstDep,_lhsOrules,_lhsOvcount) =
             (sem _lhsIallnts _lhsIcVisitsMap _lhsIinh _lhsImanualAttrDepMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIvcount )
     in  (Syn_Production _lhsOadditionalDep _lhsOcProduction _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOrules _lhsOvcount ))
sem_Production_Production :: ConstructorIdent ->
                             T_Children  ->
                             T_Rules  ->
                             T_TypeSigs  ->
                             T_Production 
sem_Production_Production con_ (T_Children children_ ) (T_Rules rules_ ) (T_TypeSigs typeSigs_ )  =
    (T_Production (\ _lhsIallnts
                     _lhsIcVisitsMap
                     _lhsIinh
                     _lhsImanualAttrDepMap
                     _lhsInt
                     _lhsIo_case
                     _lhsIo_cata
                     _lhsIo_dovisit
                     _lhsIo_newtypes
                     _lhsIo_rename
                     _lhsIo_sem
                     _lhsIo_sig
                     _lhsIo_unbox
                     _lhsIo_wantvisit
                     _lhsIprefix
                     _lhsIsyn
                     _lhsIvcount ->
                       (let _childrenOcon :: Identifier
                            _rulesOcon :: Identifier
                            _rulesOaltAttrs :: (Map AltAttr Vertex)
                            _rulesOchildNts :: (Map Identifier NontermIdent)
                            _rulesOchildInhs :: (Map Identifier Attributes)
                            _lhsOrules :: (Seq (Vertex,CRule))
                            _lhsOvcount :: Int
                            _rulesOsynsOfChildren :: (Map Identifier Attributes)
                            _lhsOcons :: ([ConstructorIdent])
                            _typeSigsOtypeSigs :: (Map Identifier Type)
                            _rulesOallTypeSigs :: (Map Identifier Type)
                            _lhsOcProduction :: CProduction
                            _lhsOadditionalDep :: (Seq Edge)
                            _lhsOdirectDep :: (Seq Edge)
                            _lhsOerrors :: (Seq Error)
                            _lhsOinstDep :: (Seq Edge)
                            _childrenOallfields :: ([(Identifier,Type,Bool)])
                            _childrenOallnts :: ([Identifier])
                            _childrenOattrs :: ([(Identifier,Identifier)])
                            _childrenOinh :: Attributes
                            _childrenOnt :: Identifier
                            _childrenOo_unbox :: Bool
                            _childrenOsyn :: Attributes
                            _rulesOallfields :: ([(Identifier,Type,Bool)])
                            _rulesOallnts :: ([Identifier])
                            _rulesOattrs :: ([(Identifier,Identifier)])
                            _rulesOinh :: Attributes
                            _rulesOmanualAttrDepMap :: AttrOrderMap
                            _rulesOnt :: Identifier
                            _rulesOo_case :: Bool
                            _rulesOo_cata :: Bool
                            _rulesOo_dovisit :: Bool
                            _rulesOo_newtypes :: Bool
                            _rulesOo_rename :: Bool
                            _rulesOo_sem :: Bool
                            _rulesOo_sig :: Bool
                            _rulesOo_wantvisit :: Bool
                            _rulesOprefix :: String
                            _rulesOsyn :: Attributes
                            _childrenIattributes :: ([(Identifier,Attributes,Attributes)])
                            _childrenIcollectChildrenSyns :: (Map Identifier Attributes )
                            _childrenIerrors :: (Seq Error)
                            _childrenIfields :: ([(Identifier,Type,Bool)])
                            _childrenIgathAltAttrs :: ([AltAttr])
                            _childrenIgathRules :: (Seq CRule)
                            _childrenIinhs :: (Seq (Identifier,Attributes))
                            _childrenInts :: (Seq (Identifier,NontermIdent))
                            _childrenIsinglevisits :: ([CRule])
                            _childrenIterminals :: ([Identifier])
                            _rulesIadditionalDep :: (Seq Edge)
                            _rulesIdirectDep :: (Seq Edge)
                            _rulesIerrors :: (Seq Error)
                            _rulesIgathAltAttrs :: ([AltAttr])
                            _rulesIgathRules :: (Seq CRule)
                            _rulesIinstDep :: (Seq Edge)
                            _rulesIinstVars :: ([Identifier])
                            _rulesIlocVars :: ([Identifier])
                            _typeSigsItypeSigs :: (Map Identifier Type)
                            -- "Order.ag"(line 73, column 16)
                            _childrenOcon =
                                con_
                            -- "Order.ag"(line 75, column 16)
                            _rulesOcon =
                                con_
                            -- "Order.ag"(line 152, column 18)
                            _gathAltAttrs =
                                [ AltAttr _LHS inh True | inh <- Map.keys _lhsIinh ]
                                 ++ _childrenIgathAltAttrs
                                 ++ _rulesIgathAltAttrs
                            -- "Order.ag"(line 168, column 17)
                            _rulesOaltAttrs =
                                Map.fromList (zip _gathAltAttrs [_lhsIvcount..])
                            -- "Order.ag"(line 181, column 18)
                            _rulesOchildNts =
                                Map.fromList (toList _childrenInts)
                            -- "Order.ag"(line 182, column 19)
                            _rulesOchildInhs =
                                Map.fromList (toList _childrenIinhs)
                            -- "Order.ag"(line 188, column 18)
                            _inhRules =
                                [ cRuleLhsInh inh _lhsInt con_ tp | (inh,tp) <- Map.assocs _lhsIinh ]
                            -- "Order.ag"(line 189, column 19)
                            _gathRules =
                                _inhRules ++ toList (_childrenIgathRules Seq.>< _rulesIgathRules)
                            -- "Order.ag"(line 243, column 18)
                            _lhsOrules =
                                Seq.fromList (zip [_lhsIvcount..] _gathRules)
                            -- "Order.ag"(line 244, column 19)
                            _lhsOvcount =
                                _lhsIvcount + length _gathRules
                            -- "Order.ag"(line 313, column 17)
                            _rulesOsynsOfChildren =
                                _childrenIcollectChildrenSyns
                            -- "Order.ag"(line 388, column 18)
                            _lhsOcons =
                                [con_]
                            -- "Order.ag"(line 395, column 16)
                            _typeSigsOtypeSigs =
                                Map.empty
                            -- "Order.ag"(line 401, column 17)
                            _rulesOallTypeSigs =
                                _typeSigsItypeSigs
                            -- "Order.ag"(line 467, column 17)
                            _cVisits =
                                if  _lhsIo_dovisit
                                     then let prodsVisitsMap = findWithErr1 "Production.cVisits.nt" _lhsInt _lhsIcVisitsMap
                                              visits = findWithErr1 "Production.cVisits.con" con_ prodsVisitsMap
                                           in visits
                                     else  let  vss = nubBy eqCRuleDefines _gathRules ++ _childrenIsinglevisits
                                           in  [CVisit _lhsIinh _lhsIsyn vss [] False]
                            -- "Order.ag"(line 493, column 18)
                            _lhsOcProduction =
                                CProduction con_ _cVisits _childrenIfields _childrenIterminals
                            -- "Order.ag"(line 517, column 16)
                            _allfields =
                                _childrenIfields
                            -- "Order.ag"(line 517, column 16)
                            _attrs =
                                map ((,) _LOC)  _rulesIlocVars ++
                                map ((,) _INST) _rulesIinstVars ++
                                map ((,) _LHS)  _inhnames ++
                                concat [map ((,) nm) (Map.keys as) | (nm,_,as) <- _childrenIattributes]
                            -- "Order.ag"(line 517, column 16)
                            _inhnames =
                                Map.keys _lhsIinh
                            -- "Order.ag"(line 517, column 16)
                            _synnames =
                                Map.keys _lhsIsyn
                            -- use rule "Order.ag"(line 263, column 71)
                            _lhsOadditionalDep =
                                _rulesIadditionalDep
                            -- use rule "Order.ag"(line 249, column 33)
                            _lhsOdirectDep =
                                _rulesIdirectDep
                            -- use rule "Order.ag"(line 64, column 70)
                            _lhsOerrors =
                                _childrenIerrors Seq.>< _rulesIerrors
                            -- use rule "Order.ag"(line 296, column 31)
                            _lhsOinstDep =
                                _rulesIinstDep
                            -- copy rule (from local)
                            _childrenOallfields =
                                _allfields
                            -- copy rule (down)
                            _childrenOallnts =
                                _lhsIallnts
                            -- copy rule (from local)
                            _childrenOattrs =
                                _attrs
                            -- copy rule (down)
                            _childrenOinh =
                                _lhsIinh
                            -- copy rule (down)
                            _childrenOnt =
                                _lhsInt
                            -- copy rule (down)
                            _childrenOo_unbox =
                                _lhsIo_unbox
                            -- copy rule (down)
                            _childrenOsyn =
                                _lhsIsyn
                            -- copy rule (from local)
                            _rulesOallfields =
                                _allfields
                            -- copy rule (down)
                            _rulesOallnts =
                                _lhsIallnts
                            -- copy rule (from local)
                            _rulesOattrs =
                                _attrs
                            -- copy rule (down)
                            _rulesOinh =
                                _lhsIinh
                            -- copy rule (down)
                            _rulesOmanualAttrDepMap =
                                _lhsImanualAttrDepMap
                            -- copy rule (down)
                            _rulesOnt =
                                _lhsInt
                            -- copy rule (down)
                            _rulesOo_case =
                                _lhsIo_case
                            -- copy rule (down)
                            _rulesOo_cata =
                                _lhsIo_cata
                            -- copy rule (down)
                            _rulesOo_dovisit =
                                _lhsIo_dovisit
                            -- copy rule (down)
                            _rulesOo_newtypes =
                                _lhsIo_newtypes
                            -- copy rule (down)
                            _rulesOo_rename =
                                _lhsIo_rename
                            -- copy rule (down)
                            _rulesOo_sem =
                                _lhsIo_sem
                            -- copy rule (down)
                            _rulesOo_sig =
                                _lhsIo_sig
                            -- copy rule (down)
                            _rulesOo_wantvisit =
                                _lhsIo_wantvisit
                            -- copy rule (down)
                            _rulesOprefix =
                                _lhsIprefix
                            -- copy rule (down)
                            _rulesOsyn =
                                _lhsIsyn
                            ( _childrenIattributes,_childrenIcollectChildrenSyns,_childrenIerrors,_childrenIfields,_childrenIgathAltAttrs,_childrenIgathRules,_childrenIinhs,_childrenInts,_childrenIsinglevisits,_childrenIterminals) =
                                (children_ _childrenOallfields _childrenOallnts _childrenOattrs _childrenOcon _childrenOinh _childrenOnt _childrenOo_unbox _childrenOsyn )
                            ( _rulesIadditionalDep,_rulesIdirectDep,_rulesIerrors,_rulesIgathAltAttrs,_rulesIgathRules,_rulesIinstDep,_rulesIinstVars,_rulesIlocVars) =
                                (rules_ _rulesOallTypeSigs _rulesOallfields _rulesOallnts _rulesOaltAttrs _rulesOattrs _rulesOchildInhs _rulesOchildNts _rulesOcon _rulesOinh _rulesOmanualAttrDepMap _rulesOnt _rulesOo_case _rulesOo_cata _rulesOo_dovisit _rulesOo_newtypes _rulesOo_rename _rulesOo_sem _rulesOo_sig _rulesOo_wantvisit _rulesOprefix _rulesOsyn _rulesOsynsOfChildren )
                            ( _typeSigsItypeSigs) =
                                (typeSigs_ _typeSigsOtypeSigs )
                        in  ( _lhsOadditionalDep,_lhsOcProduction,_lhsOcons,_lhsOdirectDep,_lhsOerrors,_lhsOinstDep,_lhsOrules,_lhsOvcount))) )
-- Productions -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allnts               : [Identifier]
         cVisitsMap           : CVisitsMap
         inh                  : Attributes
         manualAttrDepMap     : AttrOrderMap
         nt                   : Identifier
         o_case               : Bool
         o_cata               : Bool
         o_dovisit            : Bool
         o_newtypes           : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_unbox              : Bool
         o_wantvisit          : Bool
         prefix               : String
         syn                  : Attributes
      chained attribute:
         vcount               : Int
      synthesized attributes:
         additionalDep        : Seq Edge
         cProductions         : CProductions
         cons                 : [ConstructorIdent]
         directDep            : Seq Edge
         errors               : Seq Error
         instDep              : Seq Edge
         rules                : Seq (Vertex,CRule)
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
newtype T_Productions  = T_Productions (([Identifier]) ->
                                        CVisitsMap ->
                                        Attributes ->
                                        AttrOrderMap ->
                                        Identifier ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        String ->
                                        Attributes ->
                                        Int ->
                                        ( (Seq Edge),CProductions,([ConstructorIdent]),(Seq Edge),(Seq Error),(Seq Edge),(Seq (Vertex,CRule)),Int))
data Inh_Productions  = Inh_Productions {allnts_Inh_Productions :: !([Identifier]),cVisitsMap_Inh_Productions :: !(CVisitsMap),inh_Inh_Productions :: !(Attributes),manualAttrDepMap_Inh_Productions :: !(AttrOrderMap),nt_Inh_Productions :: !(Identifier),o_case_Inh_Productions :: !(Bool),o_cata_Inh_Productions :: !(Bool),o_dovisit_Inh_Productions :: !(Bool),o_newtypes_Inh_Productions :: !(Bool),o_rename_Inh_Productions :: !(Bool),o_sem_Inh_Productions :: !(Bool),o_sig_Inh_Productions :: !(Bool),o_unbox_Inh_Productions :: !(Bool),o_wantvisit_Inh_Productions :: !(Bool),prefix_Inh_Productions :: !(String),syn_Inh_Productions :: !(Attributes),vcount_Inh_Productions :: !(Int)}
data Syn_Productions  = Syn_Productions {additionalDep_Syn_Productions :: !(Seq Edge),cProductions_Syn_Productions :: !(CProductions),cons_Syn_Productions :: !([ConstructorIdent]),directDep_Syn_Productions :: !(Seq Edge),errors_Syn_Productions :: !(Seq Error),instDep_Syn_Productions :: !(Seq Edge),rules_Syn_Productions :: !(Seq (Vertex,CRule)),vcount_Syn_Productions :: !(Int)}
wrap_Productions :: T_Productions  ->
                    Inh_Productions  ->
                    Syn_Productions 
wrap_Productions (T_Productions sem ) (Inh_Productions _lhsIallnts _lhsIcVisitsMap _lhsIinh _lhsImanualAttrDepMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIvcount )  =
    (let ( _lhsOadditionalDep,_lhsOcProductions,_lhsOcons,_lhsOdirectDep,_lhsOerrors,_lhsOinstDep,_lhsOrules,_lhsOvcount) =
             (sem _lhsIallnts _lhsIcVisitsMap _lhsIinh _lhsImanualAttrDepMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIvcount )
     in  (Syn_Productions _lhsOadditionalDep _lhsOcProductions _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOrules _lhsOvcount ))
sem_Productions_Cons :: T_Production  ->
                        T_Productions  ->
                        T_Productions 
sem_Productions_Cons (T_Production hd_ ) (T_Productions tl_ )  =
    (T_Productions (\ _lhsIallnts
                      _lhsIcVisitsMap
                      _lhsIinh
                      _lhsImanualAttrDepMap
                      _lhsInt
                      _lhsIo_case
                      _lhsIo_cata
                      _lhsIo_dovisit
                      _lhsIo_newtypes
                      _lhsIo_rename
                      _lhsIo_sem
                      _lhsIo_sig
                      _lhsIo_unbox
                      _lhsIo_wantvisit
                      _lhsIprefix
                      _lhsIsyn
                      _lhsIvcount ->
                        (let _lhsOcProductions :: CProductions
                             _lhsOadditionalDep :: (Seq Edge)
                             _lhsOcons :: ([ConstructorIdent])
                             _lhsOdirectDep :: (Seq Edge)
                             _lhsOerrors :: (Seq Error)
                             _lhsOinstDep :: (Seq Edge)
                             _lhsOrules :: (Seq (Vertex,CRule))
                             _lhsOvcount :: Int
                             _hdOallnts :: ([Identifier])
                             _hdOcVisitsMap :: CVisitsMap
                             _hdOinh :: Attributes
                             _hdOmanualAttrDepMap :: AttrOrderMap
                             _hdOnt :: Identifier
                             _hdOo_case :: Bool
                             _hdOo_cata :: Bool
                             _hdOo_dovisit :: Bool
                             _hdOo_newtypes :: Bool
                             _hdOo_rename :: Bool
                             _hdOo_sem :: Bool
                             _hdOo_sig :: Bool
                             _hdOo_unbox :: Bool
                             _hdOo_wantvisit :: Bool
                             _hdOprefix :: String
                             _hdOsyn :: Attributes
                             _hdOvcount :: Int
                             _tlOallnts :: ([Identifier])
                             _tlOcVisitsMap :: CVisitsMap
                             _tlOinh :: Attributes
                             _tlOmanualAttrDepMap :: AttrOrderMap
                             _tlOnt :: Identifier
                             _tlOo_case :: Bool
                             _tlOo_cata :: Bool
                             _tlOo_dovisit :: Bool
                             _tlOo_newtypes :: Bool
                             _tlOo_rename :: Bool
                             _tlOo_sem :: Bool
                             _tlOo_sig :: Bool
                             _tlOo_unbox :: Bool
                             _tlOo_wantvisit :: Bool
                             _tlOprefix :: String
                             _tlOsyn :: Attributes
                             _tlOvcount :: Int
                             _hdIadditionalDep :: (Seq Edge)
                             _hdIcProduction :: CProduction
                             _hdIcons :: ([ConstructorIdent])
                             _hdIdirectDep :: (Seq Edge)
                             _hdIerrors :: (Seq Error)
                             _hdIinstDep :: (Seq Edge)
                             _hdIrules :: (Seq (Vertex,CRule))
                             _hdIvcount :: Int
                             _tlIadditionalDep :: (Seq Edge)
                             _tlIcProductions :: CProductions
                             _tlIcons :: ([ConstructorIdent])
                             _tlIdirectDep :: (Seq Edge)
                             _tlIerrors :: (Seq Error)
                             _tlIinstDep :: (Seq Edge)
                             _tlIrules :: (Seq (Vertex,CRule))
                             _tlIvcount :: Int
                             -- "Order.ag"(line 490, column 12)
                             _lhsOcProductions =
                                 _hdIcProduction : _tlIcProductions
                             -- use rule "Order.ag"(line 263, column 71)
                             _lhsOadditionalDep =
                                 _hdIadditionalDep Seq.>< _tlIadditionalDep
                             -- use rule "Order.ag"(line 386, column 40)
                             _lhsOcons =
                                 _hdIcons ++ _tlIcons
                             -- use rule "Order.ag"(line 249, column 33)
                             _lhsOdirectDep =
                                 _hdIdirectDep Seq.>< _tlIdirectDep
                             -- use rule "Order.ag"(line 64, column 70)
                             _lhsOerrors =
                                 _hdIerrors Seq.>< _tlIerrors
                             -- use rule "Order.ag"(line 296, column 31)
                             _lhsOinstDep =
                                 _hdIinstDep Seq.>< _tlIinstDep
                             -- use rule "Order.ag"(line 239, column 18)
                             _lhsOrules =
                                 _hdIrules Seq.>< _tlIrules
                             -- copy rule (up)
                             _lhsOvcount =
                                 _tlIvcount
                             -- copy rule (down)
                             _hdOallnts =
                                 _lhsIallnts
                             -- copy rule (down)
                             _hdOcVisitsMap =
                                 _lhsIcVisitsMap
                             -- copy rule (down)
                             _hdOinh =
                                 _lhsIinh
                             -- copy rule (down)
                             _hdOmanualAttrDepMap =
                                 _lhsImanualAttrDepMap
                             -- copy rule (down)
                             _hdOnt =
                                 _lhsInt
                             -- copy rule (down)
                             _hdOo_case =
                                 _lhsIo_case
                             -- copy rule (down)
                             _hdOo_cata =
                                 _lhsIo_cata
                             -- copy rule (down)
                             _hdOo_dovisit =
                                 _lhsIo_dovisit
                             -- copy rule (down)
                             _hdOo_newtypes =
                                 _lhsIo_newtypes
                             -- copy rule (down)
                             _hdOo_rename =
                                 _lhsIo_rename
                             -- copy rule (down)
                             _hdOo_sem =
                                 _lhsIo_sem
                             -- copy rule (down)
                             _hdOo_sig =
                                 _lhsIo_sig
                             -- copy rule (down)
                             _hdOo_unbox =
                                 _lhsIo_unbox
                             -- copy rule (down)
                             _hdOo_wantvisit =
                                 _lhsIo_wantvisit
                             -- copy rule (down)
                             _hdOprefix =
                                 _lhsIprefix
                             -- copy rule (down)
                             _hdOsyn =
                                 _lhsIsyn
                             -- copy rule (down)
                             _hdOvcount =
                                 _lhsIvcount
                             -- copy rule (down)
                             _tlOallnts =
                                 _lhsIallnts
                             -- copy rule (down)
                             _tlOcVisitsMap =
                                 _lhsIcVisitsMap
                             -- copy rule (down)
                             _tlOinh =
                                 _lhsIinh
                             -- copy rule (down)
                             _tlOmanualAttrDepMap =
                                 _lhsImanualAttrDepMap
                             -- copy rule (down)
                             _tlOnt =
                                 _lhsInt
                             -- copy rule (down)
                             _tlOo_case =
                                 _lhsIo_case
                             -- copy rule (down)
                             _tlOo_cata =
                                 _lhsIo_cata
                             -- copy rule (down)
                             _tlOo_dovisit =
                                 _lhsIo_dovisit
                             -- copy rule (down)
                             _tlOo_newtypes =
                                 _lhsIo_newtypes
                             -- copy rule (down)
                             _tlOo_rename =
                                 _lhsIo_rename
                             -- copy rule (down)
                             _tlOo_sem =
                                 _lhsIo_sem
                             -- copy rule (down)
                             _tlOo_sig =
                                 _lhsIo_sig
                             -- copy rule (down)
                             _tlOo_unbox =
                                 _lhsIo_unbox
                             -- copy rule (down)
                             _tlOo_wantvisit =
                                 _lhsIo_wantvisit
                             -- copy rule (down)
                             _tlOprefix =
                                 _lhsIprefix
                             -- copy rule (down)
                             _tlOsyn =
                                 _lhsIsyn
                             -- copy rule (chain)
                             _tlOvcount =
                                 _hdIvcount
                             ( _hdIadditionalDep,_hdIcProduction,_hdIcons,_hdIdirectDep,_hdIerrors,_hdIinstDep,_hdIrules,_hdIvcount) =
                                 (hd_ _hdOallnts _hdOcVisitsMap _hdOinh _hdOmanualAttrDepMap _hdOnt _hdOo_case _hdOo_cata _hdOo_dovisit _hdOo_newtypes _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_unbox _hdOo_wantvisit _hdOprefix _hdOsyn _hdOvcount )
                             ( _tlIadditionalDep,_tlIcProductions,_tlIcons,_tlIdirectDep,_tlIerrors,_tlIinstDep,_tlIrules,_tlIvcount) =
                                 (tl_ _tlOallnts _tlOcVisitsMap _tlOinh _tlOmanualAttrDepMap _tlOnt _tlOo_case _tlOo_cata _tlOo_dovisit _tlOo_newtypes _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_unbox _tlOo_wantvisit _tlOprefix _tlOsyn _tlOvcount )
                         in  ( _lhsOadditionalDep,_lhsOcProductions,_lhsOcons,_lhsOdirectDep,_lhsOerrors,_lhsOinstDep,_lhsOrules,_lhsOvcount))) )
sem_Productions_Nil :: T_Productions 
sem_Productions_Nil  =
    (T_Productions (\ _lhsIallnts
                      _lhsIcVisitsMap
                      _lhsIinh
                      _lhsImanualAttrDepMap
                      _lhsInt
                      _lhsIo_case
                      _lhsIo_cata
                      _lhsIo_dovisit
                      _lhsIo_newtypes
                      _lhsIo_rename
                      _lhsIo_sem
                      _lhsIo_sig
                      _lhsIo_unbox
                      _lhsIo_wantvisit
                      _lhsIprefix
                      _lhsIsyn
                      _lhsIvcount ->
                        (let _lhsOcProductions :: CProductions
                             _lhsOadditionalDep :: (Seq Edge)
                             _lhsOcons :: ([ConstructorIdent])
                             _lhsOdirectDep :: (Seq Edge)
                             _lhsOerrors :: (Seq Error)
                             _lhsOinstDep :: (Seq Edge)
                             _lhsOrules :: (Seq (Vertex,CRule))
                             _lhsOvcount :: Int
                             -- "Order.ag"(line 491, column 12)
                             _lhsOcProductions =
                                 []
                             -- use rule "Order.ag"(line 263, column 71)
                             _lhsOadditionalDep =
                                 Seq.empty
                             -- use rule "Order.ag"(line 386, column 40)
                             _lhsOcons =
                                 []
                             -- use rule "Order.ag"(line 249, column 33)
                             _lhsOdirectDep =
                                 Seq.empty
                             -- use rule "Order.ag"(line 64, column 70)
                             _lhsOerrors =
                                 Seq.empty
                             -- use rule "Order.ag"(line 296, column 31)
                             _lhsOinstDep =
                                 Seq.empty
                             -- use rule "Order.ag"(line 239, column 18)
                             _lhsOrules =
                                 Seq.empty
                             -- copy rule (chain)
                             _lhsOvcount =
                                 _lhsIvcount
                         in  ( _lhsOadditionalDep,_lhsOcProductions,_lhsOcons,_lhsOdirectDep,_lhsOerrors,_lhsOinstDep,_lhsOrules,_lhsOvcount))) )
-- Rule --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allTypeSigs          : Map Identifier Type
         allfields            : [(Identifier,Type,Bool)]
         allnts               : [Identifier]
         altAttrs             : Map AltAttr Vertex
         attrs                : [(Identifier,Identifier)]
         childInhs            : Map Identifier Attributes
         childNts             : Map Identifier NontermIdent
         con                  : Identifier
         inh                  : Attributes
         manualAttrDepMap     : AttrOrderMap
         nt                   : Identifier
         o_case               : Bool
         o_cata               : Bool
         o_dovisit            : Bool
         o_newtypes           : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_wantvisit          : Bool
         prefix               : String
         syn                  : Attributes
         synsOfChildren       : Map Identifier Attributes
      synthesized attributes:
         additionalDep        : Seq Edge
         directDep            : Seq Edge
         errors               : Seq Error
         gathAltAttrs         : [AltAttr]
         gathRules            : Seq CRule
         instDep              : Seq Edge
         instVars             : [Identifier]
         locVars              : [Identifier]
   alternatives:
      alternative Rule:
         child pattern        : Pattern 
         child rhs            : Expression 
         child owrt           : {Bool}
         child origin         : {String}
         visit 0:
            local defines     : _
            local gathRules   : _
            local manualDeps  : _
-}
-- cata
sem_Rule :: Rule  ->
            T_Rule 
sem_Rule (Rule _pattern _rhs _owrt _origin )  =
    (sem_Rule_Rule (sem_Pattern _pattern ) (sem_Expression _rhs ) _owrt _origin )
-- semantic domain
newtype T_Rule  = T_Rule ((Map Identifier Type) ->
                          ([(Identifier,Type,Bool)]) ->
                          ([Identifier]) ->
                          (Map AltAttr Vertex) ->
                          ([(Identifier,Identifier)]) ->
                          (Map Identifier Attributes) ->
                          (Map Identifier NontermIdent) ->
                          Identifier ->
                          Attributes ->
                          AttrOrderMap ->
                          Identifier ->
                          Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          String ->
                          Attributes ->
                          (Map Identifier Attributes) ->
                          ( (Seq Edge),(Seq Edge),(Seq Error),([AltAttr]),(Seq CRule),(Seq Edge),([Identifier]),([Identifier])))
data Inh_Rule  = Inh_Rule {allTypeSigs_Inh_Rule :: !(Map Identifier Type),allfields_Inh_Rule :: !([(Identifier,Type,Bool)]),allnts_Inh_Rule :: !([Identifier]),altAttrs_Inh_Rule :: !(Map AltAttr Vertex),attrs_Inh_Rule :: !([(Identifier,Identifier)]),childInhs_Inh_Rule :: !(Map Identifier Attributes),childNts_Inh_Rule :: !(Map Identifier NontermIdent),con_Inh_Rule :: !(Identifier),inh_Inh_Rule :: !(Attributes),manualAttrDepMap_Inh_Rule :: !(AttrOrderMap),nt_Inh_Rule :: !(Identifier),o_case_Inh_Rule :: !(Bool),o_cata_Inh_Rule :: !(Bool),o_dovisit_Inh_Rule :: !(Bool),o_newtypes_Inh_Rule :: !(Bool),o_rename_Inh_Rule :: !(Bool),o_sem_Inh_Rule :: !(Bool),o_sig_Inh_Rule :: !(Bool),o_wantvisit_Inh_Rule :: !(Bool),prefix_Inh_Rule :: !(String),syn_Inh_Rule :: !(Attributes),synsOfChildren_Inh_Rule :: !(Map Identifier Attributes)}
data Syn_Rule  = Syn_Rule {additionalDep_Syn_Rule :: !(Seq Edge),directDep_Syn_Rule :: !(Seq Edge),errors_Syn_Rule :: !(Seq Error),gathAltAttrs_Syn_Rule :: !([AltAttr]),gathRules_Syn_Rule :: !(Seq CRule),instDep_Syn_Rule :: !(Seq Edge),instVars_Syn_Rule :: !([Identifier]),locVars_Syn_Rule :: !([Identifier])}
wrap_Rule :: T_Rule  ->
             Inh_Rule  ->
             Syn_Rule 
wrap_Rule (T_Rule sem ) (Inh_Rule _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsImanualAttrDepMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynsOfChildren )  =
    (let ( _lhsOadditionalDep,_lhsOdirectDep,_lhsOerrors,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinstDep,_lhsOinstVars,_lhsOlocVars) =
             (sem _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsImanualAttrDepMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynsOfChildren )
     in  (Syn_Rule _lhsOadditionalDep _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars ))
sem_Rule_Rule :: T_Pattern  ->
                 T_Expression  ->
                 Bool ->
                 String ->
                 T_Rule 
sem_Rule_Rule (T_Pattern pattern_ ) (T_Expression rhs_ ) owrt_ origin_  =
    (T_Rule (\ _lhsIallTypeSigs
               _lhsIallfields
               _lhsIallnts
               _lhsIaltAttrs
               _lhsIattrs
               _lhsIchildInhs
               _lhsIchildNts
               _lhsIcon
               _lhsIinh
               _lhsImanualAttrDepMap
               _lhsInt
               _lhsIo_case
               _lhsIo_cata
               _lhsIo_dovisit
               _lhsIo_newtypes
               _lhsIo_rename
               _lhsIo_sem
               _lhsIo_sig
               _lhsIo_wantvisit
               _lhsIprefix
               _lhsIsyn
               _lhsIsynsOfChildren ->
                 (let _lhsOdirectDep :: (Seq Edge)
                      _lhsOadditionalDep :: (Seq Edge)
                      _lhsOinstDep :: (Seq Edge)
                      _lhsOerrors :: (Seq Error)
                      _lhsOgathAltAttrs :: ([AltAttr])
                      _lhsOgathRules :: (Seq CRule)
                      _lhsOinstVars :: ([Identifier])
                      _lhsOlocVars :: ([Identifier])
                      _patternOallTypeSigs :: (Map Identifier Type)
                      _patternOaltAttrs :: (Map AltAttr Vertex)
                      _patternOcon :: Identifier
                      _patternOinh :: Attributes
                      _patternOnt :: Identifier
                      _patternOsyn :: Attributes
                      _rhsOallfields :: ([(Identifier,Type,Bool)])
                      _rhsOallnts :: ([Identifier])
                      _rhsOattrs :: ([(Identifier,Identifier)])
                      _rhsOcon :: Identifier
                      _rhsOnt :: Identifier
                      _patternIcopy :: Pattern
                      _patternIerrors :: (Seq Error)
                      _patternIgathAltAttrs :: ([AltAttr])
                      _patternIinstVars :: ([Identifier])
                      _patternIlocVars :: ([Identifier])
                      _patternIpatternAttrs :: ([(Identifier,Identifier,Bool,Patterns)])
                      _rhsIallRhsVars :: (Set (Identifier,Identifier))
                      _rhsIcopy :: Expression
                      _rhsIerrors :: (Seq Error)
                      _rhsItextLines :: ([String])
                      _rhsIusedAttrs :: ([(Identifier,Identifier)])
                      _rhsIusedFields :: ([Identifier])
                      _rhsIusedLocals :: ([Identifier])
                      -- "Order.ag"(line 197, column 12)
                      _defines =
                          let  tp field attr parts | field == _LOC || field == _INST
                                                                   = case parts of
                                                                       [] -> Map.lookup attr _lhsIallTypeSigs
                                                                       _  -> (sequence (map typ parts)) >>= (haskellTupel . map (substSelf _lhsInt))
                                                   | field == _LHS = Map.lookup attr _lhsIsyn
                                                   | otherwise     = Map.lookup attr (findWithErr1 "Rule.defines.tp" field _lhsIchildInhs)
                               typ :: Pattern -> Maybe Type
                               typ (Alias field attr _ parts) = tp field attr parts
                               typ (Underscore _)             = Nothing
                               typ (Product _ pats)           = tp _LOC undefined pats
                               typ _                          = Nothing
                          in Map.fromList  [ (findWithErr1 "Rule.defines" aa _lhsIaltAttrs, (field,attr,(tp field attr parts)))
                                           | (field,attr,isLocalOrInst,parts) <- _patternIpatternAttrs
                                           , let aa = AltAttr field attr isLocalOrInst
                                           ]
                      -- "Order.ag"(line 213, column 12)
                      _gathRules =
                          let childnt field = Map.lookup field _lhsIchildNts
                          in Seq.fromList [ CRule attr False True _lhsInt _lhsIcon field (childnt field) tp _patternIcopy _rhsItextLines _defines owrt_ origin_ _rhsIallRhsVars
                                          | (field,attr,tp) <- Map.elems _defines
                                          ]
                      -- "Order.ag"(line 251, column 12)
                      _lhsOdirectDep =
                          let  defined = Map.keys _defines
                               used =  [ Map.lookup (AltAttr field attr True) _lhsIaltAttrs | (field,attr) <- _rhsIusedAttrs]
                                       ++ [ Map.lookup (AltAttr _LOC attr True) _lhsIaltAttrs | attr <- _rhsIusedLocals ++ _rhsIusedFields ]
                          in Seq.fromList [ (x,y) | Just x <- used, y <- defined ]
                      -- "Order.ag"(line 269, column 7)
                      _manualDeps =
                          Set.toList $ Map.findWithDefault Set.empty _lhsIcon $ Map.findWithDefault Map.empty _lhsInt _lhsImanualAttrDepMap
                      -- "Order.ag"(line 271, column 7)
                      _lhsOadditionalDep =
                          Seq.fromList [ (vertexA, vertexB)
                                       | Dependency (fldA,nmA) (fldB,nmB) <- _manualDeps
                                       , let vertexA = findWithErr2 (AltAttr fldA nmA True) _lhsIaltAttrs
                                       , vertexB <- lookupVertices fldB nmB _lhsIaltAttrs
                                       ]
                      -- "Order.ag"(line 299, column 7)
                      _lhsOinstDep =
                          Seq.fromList $
                            [ (instVert, synVert)
                            | (field,instNm,_) <- Map.elems _defines
                            , field == _INST
                            , synNm <- Map.keys (findWithErr2 instNm _lhsIsynsOfChildren)
                            , let instAttr = AltAttr _INST instNm True
                                  synAttr  = AltAttr instNm synNm True
                                  instVert = findWithErr2 instAttr _lhsIaltAttrs
                                  synVert  = findWithErr2 synAttr _lhsIaltAttrs
                            ]
                      -- use rule "Order.ag"(line 64, column 70)
                      _lhsOerrors =
                          _patternIerrors Seq.>< _rhsIerrors
                      -- use rule "Order.ag"(line 150, column 68)
                      _lhsOgathAltAttrs =
                          _patternIgathAltAttrs
                      -- use rule "Order.ag"(line 186, column 23)
                      _lhsOgathRules =
                          _gathRules
                      -- use rule "Order.ag"(line 537, column 86)
                      _lhsOinstVars =
                          _patternIinstVars
                      -- use rule "Order.ag"(line 537, column 48)
                      _lhsOlocVars =
                          _patternIlocVars
                      -- copy rule (down)
                      _patternOallTypeSigs =
                          _lhsIallTypeSigs
                      -- copy rule (down)
                      _patternOaltAttrs =
                          _lhsIaltAttrs
                      -- copy rule (down)
                      _patternOcon =
                          _lhsIcon
                      -- copy rule (down)
                      _patternOinh =
                          _lhsIinh
                      -- copy rule (down)
                      _patternOnt =
                          _lhsInt
                      -- copy rule (down)
                      _patternOsyn =
                          _lhsIsyn
                      -- copy rule (down)
                      _rhsOallfields =
                          _lhsIallfields
                      -- copy rule (down)
                      _rhsOallnts =
                          _lhsIallnts
                      -- copy rule (down)
                      _rhsOattrs =
                          _lhsIattrs
                      -- copy rule (down)
                      _rhsOcon =
                          _lhsIcon
                      -- copy rule (down)
                      _rhsOnt =
                          _lhsInt
                      ( _patternIcopy,_patternIerrors,_patternIgathAltAttrs,_patternIinstVars,_patternIlocVars,_patternIpatternAttrs) =
                          (pattern_ _patternOallTypeSigs _patternOaltAttrs _patternOcon _patternOinh _patternOnt _patternOsyn )
                      ( _rhsIallRhsVars,_rhsIcopy,_rhsIerrors,_rhsItextLines,_rhsIusedAttrs,_rhsIusedFields,_rhsIusedLocals) =
                          (rhs_ _rhsOallfields _rhsOallnts _rhsOattrs _rhsOcon _rhsOnt )
                  in  ( _lhsOadditionalDep,_lhsOdirectDep,_lhsOerrors,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinstDep,_lhsOinstVars,_lhsOlocVars))) )
-- Rules -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allTypeSigs          : Map Identifier Type
         allfields            : [(Identifier,Type,Bool)]
         allnts               : [Identifier]
         altAttrs             : Map AltAttr Vertex
         attrs                : [(Identifier,Identifier)]
         childInhs            : Map Identifier Attributes
         childNts             : Map Identifier NontermIdent
         con                  : Identifier
         inh                  : Attributes
         manualAttrDepMap     : AttrOrderMap
         nt                   : Identifier
         o_case               : Bool
         o_cata               : Bool
         o_dovisit            : Bool
         o_newtypes           : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_wantvisit          : Bool
         prefix               : String
         syn                  : Attributes
         synsOfChildren       : Map Identifier Attributes
      synthesized attributes:
         additionalDep        : Seq Edge
         directDep            : Seq Edge
         errors               : Seq Error
         gathAltAttrs         : [AltAttr]
         gathRules            : Seq CRule
         instDep              : Seq Edge
         instVars             : [Identifier]
         locVars              : [Identifier]
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
newtype T_Rules  = T_Rules ((Map Identifier Type) ->
                            ([(Identifier,Type,Bool)]) ->
                            ([Identifier]) ->
                            (Map AltAttr Vertex) ->
                            ([(Identifier,Identifier)]) ->
                            (Map Identifier Attributes) ->
                            (Map Identifier NontermIdent) ->
                            Identifier ->
                            Attributes ->
                            AttrOrderMap ->
                            Identifier ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            String ->
                            Attributes ->
                            (Map Identifier Attributes) ->
                            ( (Seq Edge),(Seq Edge),(Seq Error),([AltAttr]),(Seq CRule),(Seq Edge),([Identifier]),([Identifier])))
data Inh_Rules  = Inh_Rules {allTypeSigs_Inh_Rules :: !(Map Identifier Type),allfields_Inh_Rules :: !([(Identifier,Type,Bool)]),allnts_Inh_Rules :: !([Identifier]),altAttrs_Inh_Rules :: !(Map AltAttr Vertex),attrs_Inh_Rules :: !([(Identifier,Identifier)]),childInhs_Inh_Rules :: !(Map Identifier Attributes),childNts_Inh_Rules :: !(Map Identifier NontermIdent),con_Inh_Rules :: !(Identifier),inh_Inh_Rules :: !(Attributes),manualAttrDepMap_Inh_Rules :: !(AttrOrderMap),nt_Inh_Rules :: !(Identifier),o_case_Inh_Rules :: !(Bool),o_cata_Inh_Rules :: !(Bool),o_dovisit_Inh_Rules :: !(Bool),o_newtypes_Inh_Rules :: !(Bool),o_rename_Inh_Rules :: !(Bool),o_sem_Inh_Rules :: !(Bool),o_sig_Inh_Rules :: !(Bool),o_wantvisit_Inh_Rules :: !(Bool),prefix_Inh_Rules :: !(String),syn_Inh_Rules :: !(Attributes),synsOfChildren_Inh_Rules :: !(Map Identifier Attributes)}
data Syn_Rules  = Syn_Rules {additionalDep_Syn_Rules :: !(Seq Edge),directDep_Syn_Rules :: !(Seq Edge),errors_Syn_Rules :: !(Seq Error),gathAltAttrs_Syn_Rules :: !([AltAttr]),gathRules_Syn_Rules :: !(Seq CRule),instDep_Syn_Rules :: !(Seq Edge),instVars_Syn_Rules :: !([Identifier]),locVars_Syn_Rules :: !([Identifier])}
wrap_Rules :: T_Rules  ->
              Inh_Rules  ->
              Syn_Rules 
wrap_Rules (T_Rules sem ) (Inh_Rules _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsImanualAttrDepMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynsOfChildren )  =
    (let ( _lhsOadditionalDep,_lhsOdirectDep,_lhsOerrors,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinstDep,_lhsOinstVars,_lhsOlocVars) =
             (sem _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsImanualAttrDepMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynsOfChildren )
     in  (Syn_Rules _lhsOadditionalDep _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars ))
sem_Rules_Cons :: T_Rule  ->
                  T_Rules  ->
                  T_Rules 
sem_Rules_Cons (T_Rule hd_ ) (T_Rules tl_ )  =
    (T_Rules (\ _lhsIallTypeSigs
                _lhsIallfields
                _lhsIallnts
                _lhsIaltAttrs
                _lhsIattrs
                _lhsIchildInhs
                _lhsIchildNts
                _lhsIcon
                _lhsIinh
                _lhsImanualAttrDepMap
                _lhsInt
                _lhsIo_case
                _lhsIo_cata
                _lhsIo_dovisit
                _lhsIo_newtypes
                _lhsIo_rename
                _lhsIo_sem
                _lhsIo_sig
                _lhsIo_wantvisit
                _lhsIprefix
                _lhsIsyn
                _lhsIsynsOfChildren ->
                  (let _lhsOadditionalDep :: (Seq Edge)
                       _lhsOdirectDep :: (Seq Edge)
                       _lhsOerrors :: (Seq Error)
                       _lhsOgathAltAttrs :: ([AltAttr])
                       _lhsOgathRules :: (Seq CRule)
                       _lhsOinstDep :: (Seq Edge)
                       _lhsOinstVars :: ([Identifier])
                       _lhsOlocVars :: ([Identifier])
                       _hdOallTypeSigs :: (Map Identifier Type)
                       _hdOallfields :: ([(Identifier,Type,Bool)])
                       _hdOallnts :: ([Identifier])
                       _hdOaltAttrs :: (Map AltAttr Vertex)
                       _hdOattrs :: ([(Identifier,Identifier)])
                       _hdOchildInhs :: (Map Identifier Attributes)
                       _hdOchildNts :: (Map Identifier NontermIdent)
                       _hdOcon :: Identifier
                       _hdOinh :: Attributes
                       _hdOmanualAttrDepMap :: AttrOrderMap
                       _hdOnt :: Identifier
                       _hdOo_case :: Bool
                       _hdOo_cata :: Bool
                       _hdOo_dovisit :: Bool
                       _hdOo_newtypes :: Bool
                       _hdOo_rename :: Bool
                       _hdOo_sem :: Bool
                       _hdOo_sig :: Bool
                       _hdOo_wantvisit :: Bool
                       _hdOprefix :: String
                       _hdOsyn :: Attributes
                       _hdOsynsOfChildren :: (Map Identifier Attributes)
                       _tlOallTypeSigs :: (Map Identifier Type)
                       _tlOallfields :: ([(Identifier,Type,Bool)])
                       _tlOallnts :: ([Identifier])
                       _tlOaltAttrs :: (Map AltAttr Vertex)
                       _tlOattrs :: ([(Identifier,Identifier)])
                       _tlOchildInhs :: (Map Identifier Attributes)
                       _tlOchildNts :: (Map Identifier NontermIdent)
                       _tlOcon :: Identifier
                       _tlOinh :: Attributes
                       _tlOmanualAttrDepMap :: AttrOrderMap
                       _tlOnt :: Identifier
                       _tlOo_case :: Bool
                       _tlOo_cata :: Bool
                       _tlOo_dovisit :: Bool
                       _tlOo_newtypes :: Bool
                       _tlOo_rename :: Bool
                       _tlOo_sem :: Bool
                       _tlOo_sig :: Bool
                       _tlOo_wantvisit :: Bool
                       _tlOprefix :: String
                       _tlOsyn :: Attributes
                       _tlOsynsOfChildren :: (Map Identifier Attributes)
                       _hdIadditionalDep :: (Seq Edge)
                       _hdIdirectDep :: (Seq Edge)
                       _hdIerrors :: (Seq Error)
                       _hdIgathAltAttrs :: ([AltAttr])
                       _hdIgathRules :: (Seq CRule)
                       _hdIinstDep :: (Seq Edge)
                       _hdIinstVars :: ([Identifier])
                       _hdIlocVars :: ([Identifier])
                       _tlIadditionalDep :: (Seq Edge)
                       _tlIdirectDep :: (Seq Edge)
                       _tlIerrors :: (Seq Error)
                       _tlIgathAltAttrs :: ([AltAttr])
                       _tlIgathRules :: (Seq CRule)
                       _tlIinstDep :: (Seq Edge)
                       _tlIinstVars :: ([Identifier])
                       _tlIlocVars :: ([Identifier])
                       -- use rule "Order.ag"(line 263, column 71)
                       _lhsOadditionalDep =
                           _hdIadditionalDep Seq.>< _tlIadditionalDep
                       -- use rule "Order.ag"(line 249, column 33)
                       _lhsOdirectDep =
                           _hdIdirectDep Seq.>< _tlIdirectDep
                       -- use rule "Order.ag"(line 64, column 70)
                       _lhsOerrors =
                           _hdIerrors Seq.>< _tlIerrors
                       -- use rule "Order.ag"(line 150, column 68)
                       _lhsOgathAltAttrs =
                           _hdIgathAltAttrs ++ _tlIgathAltAttrs
                       -- use rule "Order.ag"(line 186, column 23)
                       _lhsOgathRules =
                           _hdIgathRules Seq.>< _tlIgathRules
                       -- use rule "Order.ag"(line 296, column 31)
                       _lhsOinstDep =
                           _hdIinstDep Seq.>< _tlIinstDep
                       -- use rule "Order.ag"(line 537, column 86)
                       _lhsOinstVars =
                           _hdIinstVars ++ _tlIinstVars
                       -- use rule "Order.ag"(line 537, column 48)
                       _lhsOlocVars =
                           _hdIlocVars ++ _tlIlocVars
                       -- copy rule (down)
                       _hdOallTypeSigs =
                           _lhsIallTypeSigs
                       -- copy rule (down)
                       _hdOallfields =
                           _lhsIallfields
                       -- copy rule (down)
                       _hdOallnts =
                           _lhsIallnts
                       -- copy rule (down)
                       _hdOaltAttrs =
                           _lhsIaltAttrs
                       -- copy rule (down)
                       _hdOattrs =
                           _lhsIattrs
                       -- copy rule (down)
                       _hdOchildInhs =
                           _lhsIchildInhs
                       -- copy rule (down)
                       _hdOchildNts =
                           _lhsIchildNts
                       -- copy rule (down)
                       _hdOcon =
                           _lhsIcon
                       -- copy rule (down)
                       _hdOinh =
                           _lhsIinh
                       -- copy rule (down)
                       _hdOmanualAttrDepMap =
                           _lhsImanualAttrDepMap
                       -- copy rule (down)
                       _hdOnt =
                           _lhsInt
                       -- copy rule (down)
                       _hdOo_case =
                           _lhsIo_case
                       -- copy rule (down)
                       _hdOo_cata =
                           _lhsIo_cata
                       -- copy rule (down)
                       _hdOo_dovisit =
                           _lhsIo_dovisit
                       -- copy rule (down)
                       _hdOo_newtypes =
                           _lhsIo_newtypes
                       -- copy rule (down)
                       _hdOo_rename =
                           _lhsIo_rename
                       -- copy rule (down)
                       _hdOo_sem =
                           _lhsIo_sem
                       -- copy rule (down)
                       _hdOo_sig =
                           _lhsIo_sig
                       -- copy rule (down)
                       _hdOo_wantvisit =
                           _lhsIo_wantvisit
                       -- copy rule (down)
                       _hdOprefix =
                           _lhsIprefix
                       -- copy rule (down)
                       _hdOsyn =
                           _lhsIsyn
                       -- copy rule (down)
                       _hdOsynsOfChildren =
                           _lhsIsynsOfChildren
                       -- copy rule (down)
                       _tlOallTypeSigs =
                           _lhsIallTypeSigs
                       -- copy rule (down)
                       _tlOallfields =
                           _lhsIallfields
                       -- copy rule (down)
                       _tlOallnts =
                           _lhsIallnts
                       -- copy rule (down)
                       _tlOaltAttrs =
                           _lhsIaltAttrs
                       -- copy rule (down)
                       _tlOattrs =
                           _lhsIattrs
                       -- copy rule (down)
                       _tlOchildInhs =
                           _lhsIchildInhs
                       -- copy rule (down)
                       _tlOchildNts =
                           _lhsIchildNts
                       -- copy rule (down)
                       _tlOcon =
                           _lhsIcon
                       -- copy rule (down)
                       _tlOinh =
                           _lhsIinh
                       -- copy rule (down)
                       _tlOmanualAttrDepMap =
                           _lhsImanualAttrDepMap
                       -- copy rule (down)
                       _tlOnt =
                           _lhsInt
                       -- copy rule (down)
                       _tlOo_case =
                           _lhsIo_case
                       -- copy rule (down)
                       _tlOo_cata =
                           _lhsIo_cata
                       -- copy rule (down)
                       _tlOo_dovisit =
                           _lhsIo_dovisit
                       -- copy rule (down)
                       _tlOo_newtypes =
                           _lhsIo_newtypes
                       -- copy rule (down)
                       _tlOo_rename =
                           _lhsIo_rename
                       -- copy rule (down)
                       _tlOo_sem =
                           _lhsIo_sem
                       -- copy rule (down)
                       _tlOo_sig =
                           _lhsIo_sig
                       -- copy rule (down)
                       _tlOo_wantvisit =
                           _lhsIo_wantvisit
                       -- copy rule (down)
                       _tlOprefix =
                           _lhsIprefix
                       -- copy rule (down)
                       _tlOsyn =
                           _lhsIsyn
                       -- copy rule (down)
                       _tlOsynsOfChildren =
                           _lhsIsynsOfChildren
                       ( _hdIadditionalDep,_hdIdirectDep,_hdIerrors,_hdIgathAltAttrs,_hdIgathRules,_hdIinstDep,_hdIinstVars,_hdIlocVars) =
                           (hd_ _hdOallTypeSigs _hdOallfields _hdOallnts _hdOaltAttrs _hdOattrs _hdOchildInhs _hdOchildNts _hdOcon _hdOinh _hdOmanualAttrDepMap _hdOnt _hdOo_case _hdOo_cata _hdOo_dovisit _hdOo_newtypes _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_wantvisit _hdOprefix _hdOsyn _hdOsynsOfChildren )
                       ( _tlIadditionalDep,_tlIdirectDep,_tlIerrors,_tlIgathAltAttrs,_tlIgathRules,_tlIinstDep,_tlIinstVars,_tlIlocVars) =
                           (tl_ _tlOallTypeSigs _tlOallfields _tlOallnts _tlOaltAttrs _tlOattrs _tlOchildInhs _tlOchildNts _tlOcon _tlOinh _tlOmanualAttrDepMap _tlOnt _tlOo_case _tlOo_cata _tlOo_dovisit _tlOo_newtypes _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_wantvisit _tlOprefix _tlOsyn _tlOsynsOfChildren )
                   in  ( _lhsOadditionalDep,_lhsOdirectDep,_lhsOerrors,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinstDep,_lhsOinstVars,_lhsOlocVars))) )
sem_Rules_Nil :: T_Rules 
sem_Rules_Nil  =
    (T_Rules (\ _lhsIallTypeSigs
                _lhsIallfields
                _lhsIallnts
                _lhsIaltAttrs
                _lhsIattrs
                _lhsIchildInhs
                _lhsIchildNts
                _lhsIcon
                _lhsIinh
                _lhsImanualAttrDepMap
                _lhsInt
                _lhsIo_case
                _lhsIo_cata
                _lhsIo_dovisit
                _lhsIo_newtypes
                _lhsIo_rename
                _lhsIo_sem
                _lhsIo_sig
                _lhsIo_wantvisit
                _lhsIprefix
                _lhsIsyn
                _lhsIsynsOfChildren ->
                  (let _lhsOadditionalDep :: (Seq Edge)
                       _lhsOdirectDep :: (Seq Edge)
                       _lhsOerrors :: (Seq Error)
                       _lhsOgathAltAttrs :: ([AltAttr])
                       _lhsOgathRules :: (Seq CRule)
                       _lhsOinstDep :: (Seq Edge)
                       _lhsOinstVars :: ([Identifier])
                       _lhsOlocVars :: ([Identifier])
                       -- use rule "Order.ag"(line 263, column 71)
                       _lhsOadditionalDep =
                           Seq.empty
                       -- use rule "Order.ag"(line 249, column 33)
                       _lhsOdirectDep =
                           Seq.empty
                       -- use rule "Order.ag"(line 64, column 70)
                       _lhsOerrors =
                           Seq.empty
                       -- use rule "Order.ag"(line 150, column 68)
                       _lhsOgathAltAttrs =
                           []
                       -- use rule "Order.ag"(line 186, column 23)
                       _lhsOgathRules =
                           Seq.empty
                       -- use rule "Order.ag"(line 296, column 31)
                       _lhsOinstDep =
                           Seq.empty
                       -- use rule "Order.ag"(line 537, column 86)
                       _lhsOinstVars =
                           []
                       -- use rule "Order.ag"(line 537, column 48)
                       _lhsOlocVars =
                           []
                   in  ( _lhsOadditionalDep,_lhsOdirectDep,_lhsOerrors,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinstDep,_lhsOinstVars,_lhsOlocVars))) )
-- TypeSig -----------------------------------------------------
{-
   visit 0:
      chained attribute:
         typeSigs             : Map Identifier Type
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
newtype T_TypeSig  = T_TypeSig ((Map Identifier Type) ->
                                ( (Map Identifier Type)))
data Inh_TypeSig  = Inh_TypeSig {typeSigs_Inh_TypeSig :: !(Map Identifier Type)}
data Syn_TypeSig  = Syn_TypeSig {typeSigs_Syn_TypeSig :: !(Map Identifier Type)}
wrap_TypeSig :: T_TypeSig  ->
                Inh_TypeSig  ->
                Syn_TypeSig 
wrap_TypeSig (T_TypeSig sem ) (Inh_TypeSig _lhsItypeSigs )  =
    (let ( _lhsOtypeSigs) =
             (sem _lhsItypeSigs )
     in  (Syn_TypeSig _lhsOtypeSigs ))
sem_TypeSig_TypeSig :: Identifier ->
                       Type ->
                       T_TypeSig 
sem_TypeSig_TypeSig name_ tp_  =
    (T_TypeSig (\ _lhsItypeSigs ->
                    (let _lhsOtypeSigs :: (Map Identifier Type)
                         -- "Order.ag"(line 397, column 13)
                         _lhsOtypeSigs =
                             Map.insert name_ tp_ _lhsItypeSigs
                     in  ( _lhsOtypeSigs))) )
-- TypeSigs ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         typeSigs             : Map Identifier Type
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
newtype T_TypeSigs  = T_TypeSigs ((Map Identifier Type) ->
                                  ( (Map Identifier Type)))
data Inh_TypeSigs  = Inh_TypeSigs {typeSigs_Inh_TypeSigs :: !(Map Identifier Type)}
data Syn_TypeSigs  = Syn_TypeSigs {typeSigs_Syn_TypeSigs :: !(Map Identifier Type)}
wrap_TypeSigs :: T_TypeSigs  ->
                 Inh_TypeSigs  ->
                 Syn_TypeSigs 
wrap_TypeSigs (T_TypeSigs sem ) (Inh_TypeSigs _lhsItypeSigs )  =
    (let ( _lhsOtypeSigs) =
             (sem _lhsItypeSigs )
     in  (Syn_TypeSigs _lhsOtypeSigs ))
sem_TypeSigs_Cons :: T_TypeSig  ->
                     T_TypeSigs  ->
                     T_TypeSigs 
sem_TypeSigs_Cons (T_TypeSig hd_ ) (T_TypeSigs tl_ )  =
    (T_TypeSigs (\ _lhsItypeSigs ->
                     (let _lhsOtypeSigs :: (Map Identifier Type)
                          _hdOtypeSigs :: (Map Identifier Type)
                          _tlOtypeSigs :: (Map Identifier Type)
                          _hdItypeSigs :: (Map Identifier Type)
                          _tlItypeSigs :: (Map Identifier Type)
                          -- copy rule (up)
                          _lhsOtypeSigs =
                              _tlItypeSigs
                          -- copy rule (down)
                          _hdOtypeSigs =
                              _lhsItypeSigs
                          -- copy rule (chain)
                          _tlOtypeSigs =
                              _hdItypeSigs
                          ( _hdItypeSigs) =
                              (hd_ _hdOtypeSigs )
                          ( _tlItypeSigs) =
                              (tl_ _tlOtypeSigs )
                      in  ( _lhsOtypeSigs))) )
sem_TypeSigs_Nil :: T_TypeSigs 
sem_TypeSigs_Nil  =
    (T_TypeSigs (\ _lhsItypeSigs ->
                     (let _lhsOtypeSigs :: (Map Identifier Type)
                          -- copy rule (chain)
                          _lhsOtypeSigs =
                              _lhsItypeSigs
                      in  ( _lhsOtypeSigs))) )