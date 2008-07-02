-----------------------------------------------------------------------------------------
{-| Module      : Gen_DocUtils
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Gen_DocTypes where

import Char

import TypesUtils

generate :: DocumentType -> [String]
generate docType = genDataType (addHolesParseErrs (addConsListDecls (docTypeWithLists)))
                ++ genClipDoc                     (docTypeWithLists ++ primTypeDecls)
                ++ genNode     (addHolesParseErrs (docTypeWithLists))
                ++ genShowNode (addHolesParseErrs (docTypeWithLists))
  where docTypeWithLists = addListDecls (addEnrichedDocDecl docType)  -- all are with Document


genDataType decls = genBanner "Proxima data type" $
  concatMap genDataDecl decls
 where genDataDecl (Decl lhsType prods) =
         let typeName = genTypeName lhsType
         in  zipWith (++) ("data %1 = " <~ [typeName] : repeat (replicate (length typeName + 6) ' ' ++ "| ")) 
                          (map genProd prods) ++
             -- [ replicate (length typeName + 10) ' ' ++ "deriving (Show, Data, Typeable)", "" ]
             [ replicate (length typeName + 10) ' ' ++ "deriving Show", "" ]
       genProd (Prod _ cnstrName idpFields fields) = 
         cnstrName ++ (prefixBy " " $ map (genIDPType . fieldType) idpFields ++
                                      map (genType' decls . fieldType) fields)

genClipDoc decls = genBanner "ClipDoc" $
  zipWith (++) ("data ClipDoc = " : repeat "             | ")
               [ "Clip_%1 %1" <~ [name] | name <- getAllDeclaredTypeNames decls ] ++
  [ "             | Clip_Nothing deriving Show" ]
  -- [ "             | Clip_Nothing deriving (Show, Typeable)" ]
    
genNode decls = genBanner "Node" $
  "data Node = NoNode" :
  [ "          | Node_%1 %2 Path" <~ [cnstrName, genTypeName lhsType]
  | Decl lhsType prods <- decls, Prod _ cnstrName _ _ <- prods 
  ] -- ++
  -- [ "            deriving Typeable" ]

genShowNode decls = genBanner "Show instance for Node" $
  "instance Show Node where" :
  "  show NoNode = \"NoNode\"" :
  [ "  show (Node_%1 _ _) = \"Node_%1\" " <~ [cnstrName]
  | cnstrName <- getAllConstructorNames decls
  ]


