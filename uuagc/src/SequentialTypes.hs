module SequentialTypes where

import CodeSyntax
import CommonTypes
import Data.Array(Array)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe(fromJust)
import Data.List(partition)
import UU.Pretty


type Vertex    = Int
data PathStep  = AttrStep Vertex Vertex
               | AtOcStep Vertex Vertex
               | AttrIndu Vertex Vertex
               deriving (Show, Eq)
               
type Path      = [PathStep]
type Route     = [Vertex]
            
type Edge      = (Int,Int)
type EdgePath  = (Edge,Path)
type EdgePaths = (Edge,Path,Path)
type EdgeRoute = (Edge,Route)
type EdgeRoutes= (Edge,Route,Route)

type Table a   = Array     Vertex a


data ChildVisit = ChildVisit Identifier Identifier Int [Vertex] [Vertex] deriving (Eq,Show) -- field, rhs nt, visit nr., inh, syn
data NTAttr = NTAInh NontermIdent Identifier Type -- nt, attribute, type
            | NTASyn NontermIdent Identifier Type -- nt, attribute, type
               deriving Show

getNtaNameType (NTAInh nt name tp) = (name,tp)
getNtaNameType (NTASyn nt name tp) = (name,tp)

getAttr     (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin uses) = name
getIsIn     (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin uses) = ii
getHasCode  (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin uses) = hc
getLhsNt    (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin uses) = nt
getCon      (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin uses) = con
getField    (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin uses) = field
getRhsNt    (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin uses) = childnt
getType     (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin uses) = tp
getDefines  (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin uses) = defines
getUses     (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin uses) = uses

isLocal = (_LOC==) . getField
isInst = (_INST==) . getField
isLhs = (_LHS==) . getField
isRhs cr = not (isLhs cr || isLocal cr)
isSyn cr | isLocal cr  = False
         | getIsIn cr  = isRhs cr
         | otherwise   = isLhs cr
isInh = not . isSyn
hasCode cr = isLocal cr || (isLhs cr && isInh cr) || (isRhs cr && isSyn cr)

isEqualField      a b = isEqualCon a b && getField a == getField b
isDifferentField  a b = isEqualCon a b && getField a /= getField b 
isEqualCon        a b = getLhsNt a == getLhsNt b && getCon a == getCon b
isRhsOfSameCon    a b = isEqualCon a b && isRhs a && isRhs b

isSynAttr (NTAInh _ _ _) = False
isSynAttr (NTASyn _ _ _) = True
isInhAttr = not . isSynAttr

ntattr :: CRule -> Maybe NTAttr
ntattr cr  | isLocal cr =  Nothing
           | isInst  cr =  Nothing -- an inst definition is just considered as a local attribute definition
           | otherwise  =  let  at = if isSyn cr then NTASyn else NTAInh
                                getNt cr = if isRhs cr then fromJust (getRhsNt cr) else getLhsNt cr
                           in Just (at (getNt cr) (getAttr cr) (fromJust (getType cr)))

cRuleLhsInh :: Identifier -> NontermIdent -> ConstructorIdent -> Type -> CRule
cRuleLhsInh attr nt con tp = CRule attr True False nt con _LHS Nothing (Just tp) (error "cRuleLhsInh") [] Map.empty False "" Set.empty
cRuleTerminal :: Identifier -> NontermIdent -> ConstructorIdent -> Type -> CRule
cRuleTerminal attr nt con tp = CRule attr True False nt con _LOC Nothing (Just tp) (error ("cRuleTerminal: " ++ show (attr, nt, con, tp))) [] Map.empty False "" Set.empty
cRuleRhsSyn :: Identifier -> NontermIdent -> ConstructorIdent -> Type -> Identifier -> NontermIdent -> CRule
cRuleRhsSyn attr nt con tp field childnt = CRule attr True False nt con field (Just childnt) (Just tp) (error ("cRuleRhsSyn: " ++ show (attr, nt, con, tp, field))) [] Map.empty False "" Set.empty

defaultRule :: Identifier -> NontermIdent -> ConstructorIdent -> Identifier -> CRule
defaultRule attr nt con field =  CRule attr (er 1) (er 2) nt con field (er 3) (er 4) (er 5) (er 6) (er 7) (er 8) (er 9) (er 10)
                                 where er i = error ("Default rule has no code " ++ show i)

instance Eq CRule where
  a == b = getAttr a == getAttr b && isEqualField a b
instance Ord CRule where
  compare a b =  compare (getLhsNt a) (getLhsNt b) 
                 >/< compare (getCon a) (getCon b)
                 >/< compare (getField a) (getField b)
                 >/< compare (getAttr a) (getAttr b)
instance Eq NTAttr where
  (NTAInh nt name _) == (NTASyn nt' name' _) = False
  (NTASyn nt name _) == (NTAInh nt' name' _) = False
  (NTAInh nt name _) == (NTAInh nt' name' _) = nt == nt' && name == name'
  (NTASyn nt name _) == (NTASyn nt' name' _) = nt == nt' && name == name'
instance Ord NTAttr where
  compare (NTAInh _ _ _) (NTASyn _ _ _) = LT
  compare (NTASyn _ _ _) (NTAInh _ _ _) = GT
  compare (NTAInh nt name _) (NTAInh nt' name' _) = compare nt nt' >/< compare name name'
  compare (NTASyn nt name _) (NTASyn nt' name' _) = compare nt nt' >/< compare name name'

eqCRuleDefines :: CRule -> CRule -> Bool
eqCRuleDefines a b
  = Map.keys (getDefines a) == Map.keys (getDefines b)

(>/<) :: Ordering -> Ordering -> Ordering
EQ >/< b = b
a >/< _ = a


eqClasses :: (a -> a -> Bool) -> [a] -> [[a]]
eqClasses p [] = []
eqClasses p (a:as) = let (isA,rest) = partition (p a) as
                     in (a:isA):eqClasses p rest

lhsshow (NTAInh field attr _) = lhsname True attr
lhsshow (NTASyn field attr _) = lhsname False attr 

rhsshow :: Identifier -> NTAttr -> String
rhsshow field (NTAInh _ attr _) = attrname False field attr
rhsshow field (NTASyn _ attr _) = attrname True field attr 

prettyCRule :: CRule -> String
prettyCRule cr 
   =  let descr | isLocal cr = "local attribute " ++ show (getAttr cr)
                | otherwise =     (if isSyn cr then "synthesized " else "inherited ")
                               ++ "attribute "
                               ++ (if isRhs cr then show (getField cr) ++ "." else "")
                               ++ (if isLhs cr then "lhs." else "")
                               ++ (show (getAttr cr))
      in show (getLhsNt cr) ++ "." ++ show (getCon cr) ++ ", " ++ descr

