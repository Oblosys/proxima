-----------------------------------------------------------------------------------------
{-| Module      : Types
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module TypesUtils where

import System
import Char
import List

delimiterLine = "----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----"

primTypes = map primDecl ["Bool", "Int", "String", "Float"]
 where primDecl tpe = Decl Basic tpe []
 
documentDecl = Decl Basic "Document" [Prod ExplicitProd "RootDoc" [] [Field "root" (BasicType "Root")]]

type DocumentType = [Decl]

data Decl = Decl { declKind :: DeclKind, declTypeName :: TypeName, productions :: [Prod] } deriving Show

data DeclKind = Basic | List | ConsList deriving Show
-- encodes the origin of a declaration. Basic comes from the document type definition or the implicit Document decl.

data Prod = Prod { prodKind :: ProdKind, constructorName :: ConstructorName, idpFields :: [Field], fields :: [Field] } deriving Show

data ProdKind = ExplicitProd | ListProd | ConsProd | NilProd | HoleProd | ParseErrProd deriving Show
-- encodes the origin of a production. ExplicitProd comes from the document type definition or the implicit Document decl.

data Field = Field { fieldName :: FieldName, fieldType :: Type } deriving Show

data Type = BasicType     { typeName :: TypeName }
          | ListType      { typeName :: TypeName }
          | CompositeType { typeName :: TypeName } deriving (Show, Eq)
          
type TypeName = String

type ConstructorName = String

type FieldName = String

isListType (ListType _) = True
isListType _            = False

getAllDeclaredTypeNames :: DocumentType -> [TypeName]
getAllDeclaredTypeNames decls = [ name | Decl _ name _ <- decls ]

getAllUsedTypes :: DocumentType -> [Type]
getAllUsedTypes decls = nub 
  [ tpe | Decl _ _ prods <- decls, Prod _ name _ fields <- prods, Field _ tpe <- fields  ]

-- return the list types appearing in right-hand sides
getAllUsedListTypes :: DocumentType -> [Type]
getAllUsedListTypes decls = filter isListType $ getAllUsedTypes decls

getAllProductions :: DocumentType -> [Prod]
getAllProductions decls = [ prod | Decl _ _ prods <- decls, prod <- prods ]

getAllConstructorNames :: DocumentType -> [ConstructorName]
getAllConstructorNames decls = [ name | Decl _ _ prods <- decls, Prod _ name _ _ <- prods ]

getArity :: Prod -> Int
getArity (Prod _ _ idpFields fields) = length idpFields + length fields

fieldNameFromType (CompositeType typeName) = [ if c == ' ' then '_' else c | c <- typeName ]
fieldNameFromType tpe = case typeName tpe of
                          []     -> error "Types.genFieldName: empty typeName"
                          (c:cs) -> toLower c : cs ++ if isListType tpe then "s" else ""

genIDPType (BasicType typeName)     = typeName
genIDPType (ListType typeName)      = "["++typeName++"]"
genIDPType (CompositeType typeName) = "("++typeName++")"

genIDPTypeAG (BasicType typeName)     = typeName
genIDPTypeAG (ListType typeName)      = "{["++typeName++"]}"
genIDPTypeAG (CompositeType typeName) = "{"++typeName++"}"

genType (BasicType typeName)     = typeName
genType (ListType typeName)      = "List_"++typeName
genType (CompositeType typeName) = "("++typeName++")"
 
genTypeAG (BasicType typeName)     = typeName
genTypeAG (ListType typeName)      = "List_"++typeName
genTypeAG (CompositeType typeName) = "{("++typeName++")}"
 
genNoIDP (Field _ tpe) = if isListType tpe then "[]" else "NoIDP"         


genPattern (Prod _ cnstrName idpFields fields) = 
  "(%1%2%3)" <~ 
  [cnstrName, concat $ replicate (length idpFields) " _", concatMap ((" "++) . fieldName) fields]



genListDecls decls = map genListDecl $ getAllUsedListTypes decls
 where genListDecl tpe = Decl List (genType tpe) 
                           [ Prod ListProd ("List_"++typeName tpe) [] [Field "elts" (BasicType ("ConsList_"++typeName tpe))] ]


genConsListDecls decls = map genConsListDecl $ getAllUsedListTypes decls
 where genConsListDecl tpe = Decl ConsList ("ConsList_"++typeName tpe)
                           [ Prod ConsProd ("Cons_"++typeName tpe) [] 
                               [ Field "head" (BasicType (typeName tpe))
                               , Field "tail" (BasicType ("ConsList_"++typeName tpe))
                               ]
                           , Prod NilProd ("Nil_"++typeName tpe) [] []
                           ]
                           
addHolesParseErrs :: DocumentType -> DocumentType
addHolesParseErrs decls = [ Decl declKind typeName $ prods ++ holeParseErr typeName 
                          | Decl declKind typeName prods <- decls ]
 where holeParseErr typeName = [ Prod HoleProd ("Hole"++typeName) [] [] 
                               , Prod ParseErrProd ("ParseErr"++typeName) [] 
                                   [ Field "presentation" (CompositeType "Presentation Document Node ClipDoc UserToken") ]
                               ]
  
addBanner str lines = 
 [""
 ,"--------------------------------------------------------------------------"
 ,"-- " ++ str ++  replicate (69 - length str) ' ' ++ "--"
 ,"--------------------------------------------------------------------------"
 ,""
 ] ++ lines ++ ["",""]
 
appendToLastLine str [] = [str]
appendToLastLine str lines = init lines ++ [last lines ++ str]

separateBy sep strs = concat $ intersperse sep strs

prefixBy pre strs = concatMap (pre++) strs

suffixBy suf strs = concatMap (++suf) strs

surroundBy pre suf strs = concatMap (\str -> pre ++ str ++ suf) strs
 
str <~ args = substitute str
 where substitute "" = ""
       substitute [c] = [c]
       substitute ('%':d:cs) = if not $ isDigit d 
                          then error $ "subs: incorrect format: "++show str
                          else let i = ord d - ord '0'
                               in  if i > length args 
                                   then "subs: not enough arguments: "++show str
                                   else args !! (i-1) ++ substitute cs
       substitute (c:cs) = c : substitute cs 
       
subst strs args = map (<~ args) strs

stop err =
 do { putStrLn "\n\nCode generation failed."
    ; putStrLn err
    ; exitWith (ExitFailure 1)
    }
