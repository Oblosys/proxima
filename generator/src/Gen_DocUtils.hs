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
generate docType = genRankNode (addHolesParseErrs (docTypeWithLists)) -- with Document
                ++ genDocNode (addHolesParseErrs (docTypeWithLists)) -- with Document
                ++ genToXML    (addHolesParseErrs (addConsListDecls docTypeWithLists))
                ++ genParseXML  (docTypeWithLists)
                ++ genListUtils docTypeWithLists
                ++ genMisc
  where docTypeWithLists = addListDecls (addEnrichedDocDecl docType)

genRankNode decls = genBanner "rankNode" $
  "rankNode :: Node -> Int" :
  "rankNode NoNode = 0" :
  zipWith (++) (map genRankNodeCnstr (getAllConstructorNames decls))
               (map show [1..])
  where genRankNodeCnstr cnstrName = "rankNode (Node_%1 _ _) = " <~ [cnstrName]
               

genDocNode decls = genBanner "DocNode instance for Node" $
  [ "instance DocNode Node where"
  , "  noNode = NoNode"
  , "  pathNode NoNode            = NoPathD" 
  ] ++
  map genPathNodeCnstr (getAllConstructorNames decls)
  where genPathNodeCnstr cnstrName = "  pathNode (Node_%1 _ pth) = PathD pth" <~ [cnstrName]

genToXML decls = genBanner "toXML functions" $ concatMap genToXMLDecl decls
  where genToXMLDecl (Decl (LHSBasicType typeName) prods) = 
         [ case prodKind of 
             ParseErrProd -> "toXML%1 %2 = Elt \"%3\" [] []" <~ [typeName, genPattern prod, cnstrName] 
             _            -> "toXML%1 %2 = Elt \"%3\" [] $ " <~ [typeName, genPattern prod, cnstrName] ++
                                               genToXMLFields fields
         | prod@(Prod prodKind cnstrName _ fields) <- prods 
         ]
        genToXMLDecl (Decl (LHSListType typeName) prods) = 
          [ "toXMLList_%1 (List_%1 xs) = toXMLConsList_%1 xs"
          , "toXMLList_%1 HoleList_%1 = []"
          , "toXMLList_%1 (ParseErrList_%1 _ _ _) = []"
          ] <~ [ typeName ]
        genToXMLDecl (Decl (LHSConsListType typeName) prods) = 
          [ "toXMLConsList_%1 (Cons_%1 x xs) = toXML%1 x : toXMLConsList_%1 xs"
          , "toXMLConsList_%1 Nil_%1             = []"  
          ] <~ [ typeName ]
     
        genToXMLFields [] = "[]"
        genToXMLFields fields = separateBy " ++ "
          [ if isListType . fieldType $ field 
            then "toXML%1 %2" <~ [genType (fieldType field), fieldName field] 
            else "[toXML%1 %2]" <~ [genType (fieldType field), fieldName field] 
          | field <- fields ]

genParseXML decls = genBanner "parseXML functions" $ concatMap genParseXMLType decls
 where genParseXMLType (Decl (LHSBasicType typeName) prods) =
         "parseXML_%1 = %2parseHoleAndParseErr \"%1\" Hole%1" <~ 
           [ typeName
           , concat [ "parseXMLCns_%1 <?|> " <~ [cnstrName] | Prod _ cnstrName _ _ <- prods ]
           ] :
           map genParseXMLProd prods
       genParseXMLType (Decl (LHSListType typeName) prods) =
         [ "parseXML_List_%1 = mkList List_%1 Cons_%1 Nil_%1 <$> many parseXML_%1" <~ 
           [ typeName ]
         ]
         
       genParseXMLProd (Prod _ cnstrName idpFields fields) =
         ("parseXMLCns_%1 = %1%2 <$ " ++
          if null fields 
          then "emptyTag \"%1\""
          else "startTag \"%1\"" ++  concatMap ((" <*> parseXML_"++) . genType . fieldType) fields ++ "<* endTag \"%1\""
         ) <~ [cnstrName, prefixBy " " $ map genNoIDP idpFields ]

genListUtils decls = genBanner "List utility functions" $ concat
  [ [ "toList_%1 vs = List_%1 (toConsList_%1 vs)"
    , ""
    , "fromList_%1 (List_%1 vs) = fromConsList_%1 vs"
    , "fromList_%1 _ = []"
    , ""
    , "toConsList_%1 [] = Nil_%1"
    , "toConsList_%1 (x:xs) = Cons_%1 x (toConsList_%1 xs)"
    , ""
    , "fromConsList_%1 Nil_%1 = []"
    , "fromConsList_%1 (Cons_%1 x xs) = x: fromConsList_%1 xs"
    , ""
    , "replaceList_%1 _ x Nil_%1 = Nil_%1  -- replace beyond end of list"
    , "replaceList_%1 0 x (Cons_%1 cx cxs) = Cons_%1 x cxs"
    , "replaceList_%1 n x (Cons_%1 cx cxs) = Cons_%1 cx (replaceList_%1 (n-1) x cxs)"
    , ""
    , "insertList_%1 0 x cxs = Cons_%1 x cxs"
    , "insertList_%1 _ x Nil_%1  = Nil_%1  -- insert beyond end of list"
    , "insertList_%1 n x (Cons_%1 cx cxs) = Cons_%1 cx (insertList_%1 (n-1) x cxs)"
    , ""
    , "removeList_%1 _ Nil_%1  = Nil_%1  -- remove beyond end of list"
    , "removeList_%1 0 (Cons_%1 cx cxs) = cxs"
    , "removeList_%1 n (Cons_%1 cx cxs) = Cons_%1 cx (removeList_%1 (n-1) cxs)"
    , ""
    ] <~ [typeName tpe]
  | tpe <- getAllUsedListTypes decls
  ]

genMisc = genBanner "Miscellaneous" $
  [ "type Presentation_Doc_Node_Clip_Token = Presentation Document Node ClipDoc UserToken"
  , ""
  , "instance Doc Document where"
  , "  initialDoc = initialDocument"
  , "  toXML = toXMLDocument"
  , "  parseXML = parseXML_Document"
  , ""
  , "instance Eq Node where"
  , "  nd1 == nd2 = rankNode nd1 == rankNode nd2"
  , "  "
  , "instance Ord Node where"
  , "  nd1 <= nd2 = rankNode nd1 <= rankNode nd2"
  , ""
  , "instance PopupMenuHack Node Document where" -- this one will disappear when popups are implemented in
  , "  mkDocNode doc = Node_RootDoc doc []"      -- a better way
  , ""
  , ""
  , "-- toXML for primitive types"
  , ""
 {-
  , "-- we don't put a \"RootDoc\" element in the XML, because this type is not visible to the user."
  , "toXMLDocument (RootDoc root) = toXMLRoot root"
  , "toXMLDocument _              = debug Err \"DocUtils_Generated.toXMLDocument: malformed Document\" $"
  , "                                 Elt \"Root\" [] [] -- this does not occur"
  , ""
 -}
  , "toXMLInt i = Elt \"Integer\" [(\"val\", show i)] []"
  , ""
  , "toXMLInt f = Elt \"Float\" [(\"val\", show f)] []"
  , ""
  , "toXMLBool b = Elt \"Bool\" [(\"val\", show b)] []"
  , ""
  , "toXMLString str = Elt \"String\" [] [PCData str] "
  , ""
  , ""
  , "-- parseXML for primitive types"
  , ""
--  , "parseXML_Document = RootDoc <$> parseXML_Root"
--  , ""
  , "parseXML_Int :: Parser Int"
  , "parseXML_Int  ="
  , " do { spaces"
  , "    ; string \"<Integer val=\\\"\""
  , "    ; str <- many (satisfy (/='\"')) "
  , "    ; string \"\\\"/>\""
  , "    ; return $ read str"
  , "    } "
  , ""
  , "parseXML_Float :: Parser Float"
  , "parseXML_Float  ="
  , " do { spaces"
  , "    ; string \"<Float val=\\\"\""
  , "    ; str <- many (satisfy (/='\"')) "
  , "    ; string \"\\\"/>\""
  , "    ; return $ read str"
  , "    } "
  , ""
  , "parseXML_Bool :: Parser Bool"
  , "parseXML_Bool ="
  , " do { spaces"
  , "    ; string \"<Bool val=\\\"\""
  , "    ; str <- many (satisfy (/='\"')) "
  , "    ; string \"\\\"/>\""
  , "    ; return $ read str"
  , "    }"
  , ""
  , "parseXML_String :: Parser String"
  , "parseXML_String ="
  , " do { spaces"
  , "    ; string \"<String>\""
  , "    ; str <- many (satisfy (/='<')) "
  , "    ; string \"</String>\""
  , "    ; return str"
  , "    }"
  , " "
  , ""
  , "-- Xprez XML presentation for primitive types"
  , ""
  , "presentPrimXMLInt :: Int -> Presentation_Doc_Node_Clip_Token"
  , "presentPrimXMLInt x = text $ \"<Int>\"++show x++\"<Int/>\""
  , ""
  , "presentPrimXMLFloat :: String -> Presentation_Doc_Node_Clip_Token"
  , "presentPrimXMLFloat x = text $ \"<Float>\"++x++\"<Float>\""
  , ""
  , "presentPrimXMLBool :: Bool -> Presentation doc node clip token"
  , "presentPrimXMLBool x = text $ \"<Bool>\"++show x++\"<Bool/>\""
  , ""
  , "presentPrimXMLString :: String -> Presentation_Doc_Node_Clip_Token"
  , "presentPrimXMLString x = text $ \"<String>\"++x++\"<String>\""
  , ""
  , ""
  , "-- Xprez tree presentation for primitive types"
  , ""
  , "presentPrimTreeInt :: Int -> Presentation_Doc_Node_Clip_Token"
  , "presentPrimTreeInt x =  mkTreeLeaf False $ text $ \"Int: \"++show x"
  , ""
  , "presentPrimTreeFloat :: Float -> Presentation_Doc_Node_Clip_Token"
  , "presentPrimTreeFloat x =  mkTreeLeaf False $ text $ \"Float: \"++show x"
  , ""
  , "presentPrimTreeBool :: Bool -> Presentation_Doc_Node_Clip_Token"
  , "presentPrimTreeBool x =  mkTreeLeaf False $ text $ \"Bool: \"++show x"
  , ""
  , "presentPrimTreeString :: String -> Presentation_Doc_Node_Clip_Token"
  , "presentPrimTreeString x =  mkTreeLeaf False $ text $ \"String: \"++x"
  ]