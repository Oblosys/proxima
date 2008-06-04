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
generate docType = genRankNode (addHolesParseErrs docTypeWithLists) -- with Document
                ++ genDocNode  (addHolesParseErrs docTypeWithLists) -- with Document
                ++ genToXML    (addHolesParseErrs (addConsListDecls docTypeWithLists))
                ++ genParseXML  docTypeWithLists
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
  [ "  pathNode (Node_%1 _ pth) = PathD pth" <~ [cnstrName] 
  | cnstrName <-  getAllConstructorNames decls 
  ]

genToXML decls = genBanner "toXML functions" $ concatMap genToXMLDecl decls
  where genToXMLDecl (Decl (LHSBasicType typeName) prods) = 
         [ case prodKind of 
             ParseErrProd -> "toXML%1 %2 = EmptyElt \"%3\" []" <~ [typeName, genPattern prod, cnstrName] 
             _            -> if null fields 
                             then "toXML%1 %2 = EmptyElt \"%3\" [] " <~ [typeName, genPattern prod, cnstrName]
                             else "toXML%1 %2 = Elt \"%3\" [] $ " <~ [typeName, genPattern prod, cnstrName]
                                   ++ genToXMLFields fields
         | prod@(Prod prodKind cnstrName _ fields) <- prods 
         ]
        genToXMLDecl (Decl (LHSListType typeName) prods) = 
          [ "toXMLList_%1 (List_%1 xs) = toXMLConsList_%1 xs"
          , "toXMLList_%1 HoleList_%1 = []"
          , "toXMLList_%1 (ParseErrList_%1 _) = []"
          ] <~ [ typeName ]
        genToXMLDecl (Decl (LHSConsListType typeName) prods) = 
          [ "toXMLConsList_%1 (Cons_%1 x xs) = toXML%1 x : toXMLConsList_%1 xs"
          , "toXMLConsList_%1 Nil_%1             = []"  
          ] <~ [ typeName ]
     
        genToXMLFields [] = "[]"
        genToXMLFields fields = separateBy " ++ "
          [ if (isDeclaredOrPrimType decls $ fieldType field)
            then if isListType . fieldType $ field 
                 then "toXML%1 %2" <~ [genType (fieldType field), fieldName field] -- can be genType'
                 else "[toXML%1 %2]" <~ [{-genType-} typeName (fieldType field), fieldName field] -- can NOT be genType' --gerbo
            else "[toXMLNonDeclared \"%1\" %2]" <~ [genType' decls (fieldType field), fieldName field]
                   -- gerbo. Bit hacky, should be different
                   -- Like: what happens with LFoo? Non declared? or some Located Foo?
                   -- And: the list should be different
          | field <- fields ]

genParseXML decls = genBanner "parseXML functions" $ concatMap genParseXMLType decls
 where genParseXMLType (Decl (LHSBasicType typeName) prods) =
         "parseXML_%1 = %2parseHoleAndParseErr \"%1\" Hole%1" <~ 
           [ typeName
           , concat [ "parseXMLCns_%1 <|> " <~ [cnstrName] | Prod _ cnstrName _ _ <- prods ]
           ] :
           map genParseXMLProd prods
       genParseXMLType (Decl (LHSListType typeName) prods) =
         [ "parseXML_List_%1 = mkList List_%1 Cons_%1 Nil_%1 <$> pList_ng parseXML_%1" <~ 
           [ typeName ]
         ]
         
       genParseXMLProd (Prod _ cnstrName idpFields fields) =
         ("parseXMLCns_%1 = %1%2 <$ " ++
          if null fields 
          then "emptyTag \"%1\""
          -- else "startTag \"%1\"" ++  concatMap ((" <*> parseXML_"++) . genType . fieldType) fields ++ "<* endTag \"%1\""
          else "startTag \"%1\"" ++  concatMap (addParseXML . fieldType) fields ++ " <* endTag \"%1\""
         ) <~ [cnstrName, prefixBy " " $ map genNoIDP idpFields ]
           where addParseXML fld = if (isDeclaredOrPrimType decls fld) 
                                   then ((" <*> parseXML_"++) . genType) fld -- can be genType'
                                   else ((" <*> (parseXMLNonDeclared \""++) . genType) fld ++ "\")" -- can NOT be genType'.

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
  , "  parseXML = parseXML_Document <* pCharSpaces"
  , ""
  , "instance Eq Node where"
  , "  nd1 == nd2 = rankNode nd1 == rankNode nd2"
  , ""
  , "instance Ord Node where"
  , "  nd1 <= nd2 = rankNode nd1 <= rankNode nd2"
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
  , "toXMLInt i = EmptyElt \"Integer\" [(\"val\", show i)]"
  , ""
  , "toXMLFloat f = EmptyElt \"Float\" [(\"val\", show f)]"
  , ""
  , "toXMLBool b = EmptyElt \"Bool\" [(\"val\", show b)]"
  , ""
  , "toXMLString str = Elt \"String\" [] [PCData str] "
  , ""
  , "toXMLNonDeclared s x = Elt s [] []" -- gerbo, barf could contain some data with 'show x'.
  , ""
  , "-- parseXML for primitive types"
  , ""
--  , "parseXML_Document = RootDoc <$> parseXML_Root"
--  , ""
  , "parseXML_Int :: CharParser Int"
  , "parseXML_Int  ="
  , "      read "
  , "  <$  pCharSpaces"
  , "  <*  pCharString \"<Integer val=\\\"\""
  , "  <*> pList (pExcept ('\\0','\\255','x') \"\\\"\") "
  , "  <*  pCharString \"\\\"/>\""
  , ""
  , "parseXML_Float :: CharParser Float"
  , "parseXML_Float  ="
  , "      read "
  , "  <$  pCharSpaces"
  , "  <*  pCharString \"<Float val=\\\"\""
  , "  <*> pList (pExcept ('\\0','\\255','x') \"\\\"\") "
  , "  <*  pCharString \"\\\"/>\""
  , ""
  , "parseXML_Bool :: CharParser Bool"
  , "parseXML_Bool  ="
  , "      read "
  , "  <$  pCharSpaces"
  , "  <*  pCharString \"<Bool val=\\\"\""
  , "  <*> pList (pExcept ('\\0','\\255','x') \"\\\"\") "
  , "  <*  pCharString \"\\\"/>\""
  , ""
  , "parseXML_String :: CharParser String"
  , "parseXML_String  ="
  , "      id"
  , "  <$  pCharSpaces"
  , "  <*  pCharString \"<String>\""
  , "  <*> pList (pExcept ('\\0','\\255','x') \"<\") "
  , "  <*  pCharString \"</String>\""
  , ""
  , "parseXMLNonDeclared :: {- (Editable a Document Node ClipDoc UserToken) => -} String -> CharParser a"
  , "parseXMLNonDeclared s ="
  , "  (error \"docutils\") <$ emptyTag s" {-
  , "      undefined" -- read" gerbo HACK Need read instances
  , "  <$  pCharSpaces"
  , "  <*  pCharString (\"<\" ++ s ++ \">\")"
  , "  <*> pList (pExcept ('\\0','\\255','x') \"<\") "
  , "  <*  pCharString (\"</\" ++ s ++ \">\")" -}
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
  , "presentNonDeclaredXML :: String -> a -> Presentation_Doc_Node_Clip_Token"
  , "presentNonDeclaredXML s x = text $ \"<\"++s++\"/>\"" --gerbo
--  , "presentNonDeclaredXML s x = text $ \"<\" ++ s ++ \">\" ++ {-show x-} ++ " ++ \"</\"++s++\">\"" -- gerbo
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
  , ""
  , "presentNonDeclaredTree :: String -> a -> Presentation_Doc_Node_Clip_Token"
  , "presentNonDeclaredTree s x = mkTreeLeaf False $ text $ s" -- ++ \": \" ++ \"barf\"" --show x" --gerbo
  ]