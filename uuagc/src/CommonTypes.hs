module CommonTypes where

import Pretty
import UU.Scanner.Position(Pos,noPos)
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set as Set


type Blocks = Map BlockInfo [([String], Pos)]
type BlockInfo = (BlockType, Maybe NontermIdent)
data BlockType
  = BlockImport
  | BlockPragma
  | BlockOther
  deriving (Eq, Ord, Show)

data Identifier = Ident { getName::String, getPos::Pos }

instance Eq Identifier where
 Ident x _ == Ident y _ = x == y

instance Ord Identifier where 
 compare (Ident x _) (Ident y _) = compare x y

instance Show Identifier where
  show ident = getName ident

instance PP Identifier where
  pp = text . getName

data Type = Haskell String
          | NT Identifier [String]

data ComplexType = List Type
                 | Tuple [(Identifier, Type)]
                 | Maybe Type
                 | Either Type Type
                 | Map Type Type
                 | IntMap Type

instance Show ComplexType where
  show (List  t )     = "[" ++ show t ++ "]"
  show (Tuple ts)     = "(" ++ showList [ show n ++ ": " ++ show t | (n,t) <- ts ] "" ++ ")"
  show (Maybe t )     = "Maybe " ++ show t
  show (Either t1 t2) = "Either " ++ show t1 ++ " " ++ show t2
  show (Map t1 t2)    = "Map " ++ show t1 ++ " " ++ show t2
  show (IntMap t1)    = "IntMap " ++ show t1

instance Show Type where
  show = typeToHaskellString Nothing []

type Attributes  = Map Identifier Type
type TypeSyns    = [(NontermIdent,ComplexType)]
type ParamMap    = Map NontermIdent [Identifier]
type AttrNames   = [(Identifier,Type,(String,String,String))]
type UseMap      = Map NontermIdent (Map Identifier (String,String,String))
type PragmaMap   = Map NontermIdent (Map ConstructorIdent (Set Identifier))
type AttrMap     = Map NontermIdent (Map ConstructorIdent (Set (Identifier,Identifier)))
type UniqueMap   = Map NontermIdent (Map ConstructorIdent (Map Identifier Identifier))
type Fields      = [(Identifier,Type)]
type Derivings   = Map NontermIdent (Set Identifier)
type ClassContext = [(Identifier, [String])]
type ContextMap  = Map NontermIdent ClassContext
type Strings     = [String]
type NontermIdent     = Identifier
type ConstructorIdent = Identifier
type AttrOrderMap = Map NontermIdent (Map ConstructorIdent (Set Dependency))
data Dependency = Dependency (Identifier,Identifier) (Identifier,Identifier) deriving (Eq,Ord,Show)

type AttrEnv = ( [Identifier]
               , [(Identifier,Identifier)]
               )

identifier x   = Ident x noPos               
nullIdent = identifier ""
_LHS   = identifier "lhs" 
_SELF  = identifier "SELF" 
_LOC   = identifier "loc" 
_INST  = identifier "inst"
_INST' = identifier "inst'"
_FIELD = identifier "field"
_FIRST = identifier "first__"
_LAST  = identifier "last__"

sdtype :: NontermIdent -> String
sdtype nt = "T_"++getName nt

cataname ::  String -> Identifier -> String
cataname pre name = pre++getName name

conname :: Bool -> NontermIdent -> ConstructorIdent -> String
conname rename nt con | rename =  getName nt ++ "_" ++ getName con
                      | otherwise = getName con

semname  ::  String -> NontermIdent -> ConstructorIdent -> String
semname pre nt con =  pre ++ (getName nt ++ "_" ++ getName con)

lhsname :: Bool -> Identifier -> String
lhsname isIn = attrname isIn _LHS

attrname :: Bool -> Identifier -> Identifier -> String
attrname isIn field attr | field == _LOC   = locname attr 
                         | field == _INST  = instname attr
                         | field == _INST' = inst'name attr
                         | field == _FIELD = fieldname attr
                         | otherwise       = let direction | isIn      = "I" 
                                                           | otherwise = "O"
                                             in '_' : getName field ++ direction ++ getName attr
                               
locname v   = '_' : getName v
instname v  = getName v ++ "_val_"
inst'name v = getName v ++ "_"
fieldname v =  getName v++"_"

typeToAGString :: Type -> String
typeToAGString tp
  = case tp of
      Haskell t -> t
      NT nt tps -> formatNonterminalToHaskell (getName nt) (map (\s -> "{" ++ s ++ "}") tps)

typeToHaskellString :: Maybe NontermIdent -> [String] -> Type -> String
typeToHaskellString mbNt params tp
  = case tp of
      Haskell t -> t
      NT nt tps | nt == _SELF -> formatNonterminalToHaskell (maybe "Unknown" getName mbNt) params
                | otherwise   -> formatNonterminalToHaskell (getName nt) tps

formatNonterminalToHaskell :: String -> [String] -> String
formatNonterminalToHaskell nt tps
  = unwords (nt:tps)

ind :: String -> String
ind s = replicate 3 ' ' ++ s

_NOCASE :: Identifier
_NOCASE = identifier "nocase"

hasPragma :: PragmaMap -> NontermIdent -> ConstructorIdent -> Identifier -> Bool
hasPragma mp nt con nm
  = nm `Set.member` Map.findWithDefault Set.empty con (Map.findWithDefault Map.empty nt mp)
  
isNonterminal :: Type -> Bool
isNonterminal (NT _ _) = True
isNonterminal _        = False

isSELFNonterminal :: Type -> Bool
isSELFNonterminal (NT nt _) | nt == _SELF = True
isSELFNonterminal _                       = False

extractNonterminal :: Type -> NontermIdent
extractNonterminal (NT n _) = n

nontermArgs :: Type -> [String]
nontermArgs tp
  = case tp of
      NT _ args -> args
      _         -> [] 

