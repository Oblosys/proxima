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
generate docType = genDataType (addHolesParseErrs docTypeWithLists ++ genConsListDecls docType)
                ++ genClipDoc  (documentDecl : docTypeWithLists ++ primTypes)
                ++ genNode     (addHolesParseErrs (documentDecl : docTypeWithLists))
                ++ genShowNode (addHolesParseErrs (documentDecl : docTypeWithLists))
  where docTypeWithLists = docType ++ genListDecls docType

                

genDataType decls = addBanner "Proxima data type" $
  concatMap genDataDecl decls
 where genDataDecl (Decl _ typeName prods) = 
         zipWith (++) ("data %1 = " <~ [typeName] : repeat (replicate (length typeName + 6) ' ' ++ "| ")) 
                      (map genProd prods) ++
         [ replicate (length typeName + 10) ' ' ++ "deriving Show", "" ]
       genProd (Prod cnstrName idpFields fields) = 
         cnstrName ++ (concatMap ((" "++) . genTypeName . fieldType) $ idpFields ++ fields)

genClipDoc decls = addBanner "ClipDoc" $
  zipWith (++) ("data ClipDoc = " : repeat "             | ")
               [ "Clip_%1 %1" <~ [name] | name <- getAllDeclaredTypeNames decls ] ++
  ["             | Clip_Nothing deriving Show"]
    
genNode decls = addBanner "Node" $
  "data Node = NoNode" :
  [ "          | %1Node %2 Path" <~ [cnstrName, typeName]
  | Decl _ typeName prods <- decls, Prod cnstrName _ _ <- prods 
  ]

genShowNode decls = addBanner "Show instance for Node" $
  "instance Show Node where" :
    "  show NoNode = \"NoNode\"" :
  [ "  show (%1Node _ _) = \"%1Node\" " <~ [cnstrName]
  | Decl _ _ prods <- decls, prod@(Prod cnstrName _ _) <- prods
  ]


