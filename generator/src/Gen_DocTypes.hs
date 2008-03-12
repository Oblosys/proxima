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
  where docTypeWithLists = addListDecls docType

                
genDataType decls = genBanner "Proxima data type" $
  [ "data Document = RootDoc Root deriving Show"
  , "" 
  , "data EnrichedDoc = RootEnr RootE Document"
  , "                 | HoleEnr deriving Show"
  , "" 
  ] ++
  concatMap genDataDecl decls
 where genDataDecl (Decl lhsType prods) =
         let typeName = genTypeName lhsType
         in  zipWith (++) ("data %1 = " <~ [typeName] : repeat (replicate (length typeName + 6) ' ' ++ "| ")) 
                          (map genProd prods) ++
             [ replicate (length typeName + 10) ' ' ++ "deriving Show", "" ]
       genProd (Prod _ cnstrName idpFields fields) = 
         cnstrName ++ (prefixBy " " $ map (genIDPType . fieldType) idpFields ++
                                      map (genType . fieldType) fields)

genClipDoc decls = genBanner "ClipDoc" $
  zipWith (++) ("data ClipDoc = " : repeat "             | ")
               [ "Clip_%1 %1" <~ [name] | name <- getAllDeclaredTypeNames decls ] ++
  ["             | Clip_Nothing deriving Show"]
    
genNode decls = genBanner "Node" $
  "data Node = NoNode" :
  "          | Node_RootDoc Document Path" :  -- TODO: get rid of this one after popup hack is solved
  [ "          | Node_%1 %2 Path" <~ [cnstrName, genTypeName lhsType]
  | Decl lhsType prods <- decls, Prod _ cnstrName _ _ <- prods 
  ]

genShowNode decls = genBanner "Show instance for Node" $
  "instance Show Node where" :
   "  show NoNode = \"NoNode\"" :
  [ "  show (Node_%1 _ _) = \"Node_%1\" " <~ [cnstrName]
  | cnstrName <- getAllConstructorNames decls
  ]


