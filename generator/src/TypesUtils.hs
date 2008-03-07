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
 where primDecl tpe = Decl tpe []
 
documentDecl = Decl "Document" [Prod "RootDoc" [] [Field "root" (BasicType "Root")]]

type DocumentType = [Decl]

data Decl = Decl { declTypeName :: TypeName, productions :: [Prod] } deriving Show

data Prod = Prod { constructorName :: ConstructorName, idpFields :: [Field], fields :: [Field] } deriving Show

data Field = Field { fieldName :: FieldName, fieldType :: Type } deriving Show

data Type = BasicType { typeName :: TypeName }
          | ListType  { typeName :: TypeName } deriving (Show, Eq)
          
type TypeName = String

type ConstructorName = String

type FieldName = String

isListType (ListType _) = True
isListType _            = False

getAllDeclaredTypeNames :: DocumentType -> [TypeName]
getAllDeclaredTypeNames decls = [ name | Decl name _ <- decls ]

getAllUsedTypes :: DocumentType -> [Type]
getAllUsedTypes decls = nub 
  [ tpe | Decl _ prods <- decls, Prod name _ fields <- prods, Field _ tpe <- fields  ]

-- return the list types appearing in right-hand sides
getAllUsedListTypes :: DocumentType -> [Type]
getAllUsedListTypes decls = filter isListType $ getAllUsedTypes decls

getAllConstructorNames :: DocumentType -> [ConstructorName]
getAllConstructorNames decls = [ name | Decl _ prods <- decls, Prod name _ _ <- prods ]

getArity :: Prod -> Int
getArity (Prod _ idpFields fields) = length idpFields + length fields

fieldNameFromType tpe = case genTypeName tpe of
                          []     -> error "Types.genFieldName: empty typeName"
                          (c:cs) -> toLower c : cs ++ if isListType tpe then "s" else ""



genTypeName (BasicType typeName) = typeName
genTypeName (ListType typeName)  = "List_"++typeName


genListDecls decls = map genListDecl $ getAllUsedListTypes decls
 where genListDecl tpe = Decl (genTypeName tpe) 
                           [ Prod ("List_"++typeName tpe) [] [Field "elts" (BasicType ("ConsList_"++typeName tpe))] ]


genConsListDecls decls = map genConsListDecl $ getAllUsedListTypes decls
 where genConsListDecl tpe = Decl ("ConsList_"++typeName tpe)
                           [ Prod ("Cons_"++typeName tpe) [] 
                               [ Field "head" (BasicType (typeName tpe))
                               , Field "tail" (BasicType ("ConsList_"++typeName tpe))
                               ]
                           , Prod ("Nil_"++typeName tpe) [] []
                           ]
                           
addHolesParseErrs :: DocumentType -> DocumentType
addHolesParseErrs decls = [ Decl typeName $ prods ++ holeParseErr typeName 
                          | Decl typeName prods <- decls ]
 where holeParseErr typeName = [ Prod ("Hole"++typeName) [] [] 
                               , Prod ("ParseErr"++typeName) [] 
                                   [ Field "presentation" (BasicType "(Presentation Document Node ClipDoc UserToken)") ]
                               ]
  
addBanner str lines = 
 [""
 ,"--------------------------------------------------------------------------------"
 ,"-- " ++ str ++  replicate (75 - length str) ' ' ++ "--"
 ,"--------------------------------------------------------------------------------"
 ,""
 ] ++ lines ++ ["",""]
 
appendToLastLine str [] = [str]
appendToLastLine str lines = init lines ++ [last lines ++ str]

separateBy sep strs = concat $ intersperse sep strs

subs str args = subs' str
 where subs' "" = ""
       subs' [c] = [c]
       subs' ('%':d:cs) = if not $ isDigit d 
                          then error $ "subs: incorrect format: "++show str
                          else let i = ord d - ord '0'
                               in  if i > length args 
                                   then "subs: not enough arguments: "++show str
                                   else args !! (i-1) ++ subs' cs
       subs' (c:cs) = c : subs' cs 
       
stop err =
 do { putStrLn "\n\nCode generation failed."
    ; putStrLn err
    ; getChar
    ; exitWith (ExitFailure 1)
    }
