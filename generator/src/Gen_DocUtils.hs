-----------------------------------------------------------------------------------------
{-| Module      : Gen_DocUtils
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Gen_DocUtils where

import List

import TypesUtils


generate :: DocumentType -> [String]
generate docType = genRankNode (addHolesParseErrs (documentDecl : docTypeWithLists))
                ++ genPathNode (addHolesParseErrs (documentDecl : docTypeWithLists))
                ++ genToXML    (addHolesParseErrs docTypeWithLists ++ genConsListDecls docType)  
                ++ genParseXML (docTypeWithLists)  
  where docTypeWithLists = docType ++ genListDecls docType

genRankNode decls = addBanner "rankNode" $
  "rankNode :: Node -> Int" :
  "rankNode NoNode = 0" :
  zipWith (++) (map genRankNodeCnstr (getAllConstructorNames decls))
               (map show [1..])
  where genRankNodeCnstr cnstrName = "rankNode (%1Node _ _) = " <~ [cnstrName]
               

genPathNode decls = addBanner "HasPath instance for Node" $
  "instance HasPath Node where" :
  "  pathNode NoNode            = NoPathD" :
  map genPathNodeCnstr (getAllConstructorNames decls)
  where genPathNodeCnstr cnstrName = "  pathNode (%1Node _ pth) = PathD pth" <~ [cnstrName]

genToXML decls = addBanner "toXML functions" $ concatMap genToXMLDecl decls
  where genToXMLDecl (Decl Basic typeName prods) = 
         [ if "ParseErr" `isPrefixOf` cnstrName 
         then "toXML%1 %2 = Elt \"%3\" [] []" <~ [typeName, genPattern prod, cnstrName] 
         else "toXML%1 %2 = Elt \"%3\" [] $ " <~ [typeName, genPattern prod, cnstrName] ++
             genToXMLFields fields
         | prod@(Prod cnstrName _ fields) <- prods 
         ]
        genToXMLDecl (Decl List typeName prods) = 
          [ "toXMLList_%1 (List_%1 xs) = toXMLConsList_%1 xs"
          , "toXMLList_%1 HoleList_%1 = []"
          , "toXMLList_%1 (ParseErrList_%1 _) = []"
          ] `subst` [ drop 5 typeName ]
        genToXMLDecl (Decl ConsList typeName prods) = 
          [ "toXMLConsList_%1 (Cons_%1 x xs) = toXML%1 x : toXMLConsList_%1 xs"
          , "toXMLConsList_%1 Nil_%1             = []"  
          ] `subst` [ drop 9 typeName ]
     
        genToXMLFields [] = "[]"
        genToXMLFields fields = separateBy " ++ "
          [ if isListType . fieldType $ field 
            then "toXML%1 %2" <~ [genTypeName (fieldType field), fieldName field] 
            else "[toXML%1 %2]" <~ [genTypeName (fieldType field), fieldName field] 
          | field <- fields ]

genParseXML decls = addBanner "parseXML functions" $ concatMap genParseXMLType decls
 where genParseXMLType (Decl Basic typeName prods) =
         "parzeXML_%1 = %2parseHoleAndParseErr \"%1\" Hole%1" <~ 
           [ typeName
           , concat [ "parseXMLCns_%1 <?|> " <~ [cnstrName] | Prod cnstrName _ _ <- prods ]
           ] :
           map genParseXMLProd prods
       genParseXMLType (Decl List typeName prods) =
         ["parseXML_List_%1 = mkList List_%1 Cons_%1 Nil_%1 <$> many parseXML_%1" <~ 
           [ drop 5 typeName ]
         ]
       genParseXMLProd (Prod cnstrName _ fields) =
         ("parseXMLCns_%1 = %1%2 <$ " ++
          if null fields 
          then "emptyTag \"%1\""
          else "startTag \"%1\"" ++  concatMap ((" <*> parseXML_"++) . genTypeName . fieldType) fields ++ "<* endTag \"%1\""
         ) <~ [cnstrName, prefixBy " " $ map genNoID fields ]
         