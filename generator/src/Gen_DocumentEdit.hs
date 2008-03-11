-----------------------------------------------------------------------------------------
{-| Module      : Gen_DocumentEdit
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Gen_DocumentEdit where

import TypesUtils

{-
TODO: 
- move functions to docutils

-}
generate :: DocumentType -> [String]
generate docType = genClip (docTypeWithLists ++ documentDecl : primTypeDecls) ++
                   genEditable (removeEnrichedDocDecl docTypeWithLists)
 where docTypeWithLists = addListDecls docType


genClip decls = genBanner "Clip instance" $
    "instance Clip ClipDoc where" : 
  [ "  arityClip Clip_Nothing = -1" ] ++
  [ "  arityClip (Clip_%1 x) = arity x" <~ [typeName] | typeName <- typeNames ] ++ 
  [ "" ] ++
  [ "  alternativesClip Clip_Nothing = []" ] ++
  [ "  alternativesClip (Clip_%1 x) = alternatives x" <~ [typeName] | typeName <- typeNames ] ++ 
  [ "" ] ++
  [ "  holeClip Clip_Nothing = Clip_Nothing" ] ++
  [ "  holeClip (Clip_%1 x) = Clip_%1 hole" <~ [typeName] | typeName <- typeNames ] ++ 
  [ "" ] ++
  [ "  isListClip Clip_Nothing = False" ] ++
  [ "  isListClip (Clip_%1 x) = isList x" <~ [typeName] | typeName <- typeNames ] ++ 
  [ "" ] ++
  [ "  insertListClip i c Clip_Nothing = Clip_Nothing" ] ++
  [ "  insertListClip i c (Clip_%1 x) = insertList i c x" <~ [typeName] | typeName <- typeNames ] ++
  [ "" ] ++
  [ "  removeListClip i Clip_Nothing = Clip_Nothing" ] ++
  [ "  removeListClip i (Clip_%1 x) = removeList i x" <~ [typeName] | typeName <- typeNames ] ++ 
  [ "" ]
 where typeNames = getAllDeclaredTypeNames decls

genEditable decls = genBanner "Editable instances" $
  concatMap genEditableDecl decls

genEditableDecl (Decl (LHSBasicType typeName) prods) = 
  ["instance Editable %1 Document Node ClipDoc UserToken where"
  , "  select [] x = Clip_%1 x" 
  ] <~ [typeName] ++
  concatMap genSelect prods ++
  [ "  select _ _ = Clip_Nothing" 
  , ""
  , "  paste [] (Clip_%1 c) _ = c"
  , "  paste [] c x = debug Err (\"Type error: pasting \"++show c++\" on %1\") x"
  ] <~ [typeName] ++
  concatMap genPaste prods ++
  [ "  paste _ _ x = x" 
  , ""
  ] ++
  zipWith (++) ("  alternatives _ = [" : repeat "                   ,")
               (map genAlternatives prods) ++
  [ "                   ,(\"{%1}\", Clip_%1 hole)"
  , "                   ]" 
  ] <~ [typeName] ++
  [ ""
  ] ++
  map genArity prods ++
  [ "  arity _                        = 0"
  , ""
  , "  parseErr = ParseErr%1"
  , ""
  , "  hole = Hole%1"
  , ""
  , "  isList _ = False"
  , "  insertList _ _ _ = Clip_Nothing"
  , "  removeList _ _ = Clip_Nothing"
  , ""
  ] <~ [typeName]
  where genSelect prod@(Prod _ cnstrName idpFields fields) = 
          [ "  select (%1:p) %2 = select p x%3"
            <~ [ show i, genXPattern prod, show $ i] 
          | i <- [ 0..length fields -1 ]]
        genPaste prod@(Prod _ cnstrName idpFields fields) =
          [ "  paste (%1:p) c %2 = %3"
            <~ [ show i, genIXPattern prod
               , cnstrName ++ prefixBy " i" (map show [0..length idpFields-1]) ++ concat
                   [ if j == i then " (paste p c x" ++ show j ++ ")" else " x"++show j | j <- [0..length fields-1] ]
               ] 
          | i <- [ 0..length fields -1 ]]
        genAlternatives prod@(Prod _ cnstrName idpFields fields) =
          " (\"%1%2 \"  , Clip_%3 $ %1%4%5)"
          <~ [ cnstrName
             , surroundBy " {" "}" $ map (genType . fieldType) fields
             , typeName
             , prefixBy " " $ map genNoIDP idpFields
             , concat $ replicate (length fields) " hole"
             ]
        genArity prod@(Prod _ cnstrName idpFields fields) = 
          "  arity %1 = %2" <~ [ genXPattern prod, show $ length fields ]

genEditableDecl (Decl (LHSListType typeName) prods) = 
  [ "toList_%1 vs = List_%1 (toConsList_%1 vs)"
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
  , "instance Editable List_%1 Document Node ClipDoc UserToken where"
  , "  select [] x = Clip_List_%1 x"
  , "  select (n:p) (List_%1 cxs) ="
  , "    let xs = fromConsList_%1 cxs"
  , "    in  if n < length xs "
  , "        then select p (xs !! n)"
  , "        else Clip_Nothing"
  , "  select _ _ = Clip_Nothing"
  , ""
  , "  paste [] (Clip_List_%1 c) _ = c"
  , "  paste [] c x = debug Err (\"Type error: pasting \"++show c++\" on List_%1\")   x"
  , "  paste (n:p) c (List_%1 cxs) ="
  , "    let xs = fromConsList_%1 cxs"
  , "    in  if n < length xs"
  , "        then let x  = xs!!n"
  , "                 x' = paste p c x"
  , "             in  List_%1 (replaceList_%1 n x' cxs)"
  , "        else List_%1 cxs -- paste beyond end of list"
  , "  paste _ _ x = x"
  , ""
  , "  alternatives _ = [(\"{List_%1}\", Clip_List_%1 hole)"
  , "                   ]"
  , ""
  , "  arity (List_%1 x1) = length (fromConsList_%1 x1)"
  , "  arity _ = 0"
  , ""
  , "  parseErr = ParseErrList_%1"
  , ""
  , "  hole = List_%1 Nil_%1"
  , ""
  , "  isList _ = True"
  , ""
  , "  insertList n (Clip_%1 c) (List_%1 cxs) = Clip_List_%1 $ List_%1 (insertList_%1 n c cxs)"
  , "  insertList _ _ xs = debug Err \"Type error, no paste\" $ Clip_List_%1 xs"
  , "  insertList _ c xs = Clip_List_%1 xs"
  , ""
  , "  removeList n (List_%1 cxs) = Clip_List_%1 $ List_%1 (removeList_%1 n cxs)"
  , "  removeList _ xs = Clip_List_%1 $ xs"
  , ""
  ] <~ [typeName]
