module GenCommon where

import Char (toLower)
import List

defaultLimit = "-- don't edit this line or below !!!"
--- maybe use something a bit more visible like: ----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----"


---------------------------------------------------------------------
--       D A T A                                                   --
---------------------------------------------------------------------

type ModuleName = String

data File  = File  ModuleName [Decl]       deriving Show
data Decl  = Decl  String [Prod] DeclType  deriving Show

data Prod  = Prod  String [Field]          deriving Show
data Field = Field String String Type      deriving Show

data Type = List 
          | Prim 
          | Data
          | Id                      
          | Ids                            deriving (Show, Eq) 

data DeclType = DeclList
              | DeclConsList            --- 
              | DeclDef                    deriving (Show, Eq) --- Eq

{- L I B -}

getLists (File _ decls) = removeRepeat (concatMap getF decls) where
         getF (Decl _ l _) = concatMap f l
         f (Prod _ l) = map (\(Field _ f _)->f) (filter (isListNotIDs) l)

getFields (File _ decls) = removeRepeat (concatMap getF decls) where
           getF (Decl _ l _) = concatMap f l
           f (Prod _ l) = map (\(Field _ f _)->f) (filter (not.isID) l)
       
---
getFields' (File _ decls) = removeRepeat (concatMap getF decls) where
           getF (Decl _ l _) = concatMap f l
           f (Prod _ l) = (filter (not.isID) l)

---
instance Eq Field where
 Field _ tp1 _ == Field _ tp2 _ = tp1 == tp2 --- this equality on types is not what we want,
                                             --- but I added it because fields are used in the
                                             --- generation, and we need to nub them (removeRepeat)
---

                 
isID (Field _ _ a) = (a==Id) || (a==Ids)

isPrim (Field _ _ a) = a==Prim

isList (Field _ _ a) = a==List

notIDPrim a = (not(isID a)) && (not(isPrim a)) 

isListNotIDs a = (isList a) && not(isID a) 

isNotListNotIDs a = not(isList a) && not(isID a)


-- 
showAsList = printList "[]" "[" "]" ", "

printList e o c s xs = case xs of
  []     -> [e]
  (x:xs) -> (o ++ x) : map (s ++) xs  ++ [c]


--- 'nub' from List removes double elements

removeRepeat l = reverse(removeRepeat' [] l) where
        removeRepeat' l []     = l
        removeRepeat' l (x:xs) = if (elem x l) then removeRepeat' l xs 
                                               else removeRepeat' (x:l) xs


decapitalize e = toLower(head e):(tail e)


---
fieldType (Field _ tp List) = "List_"++init tp
fieldType (Field _ _ Ids)   = "[IDP] "
fieldType (Field _ tp _)    = tp
---


{-  Generate Types         -}    --- was "Genereate"
genListTypes :: File -> [Decl]
genListTypes parsedFile = concatMap genListType (getLists parsedFile) --- Extra nonterminal added

---
--- type name is no longer the type ++ "s", so the parser should be updated as well
--- local defs for the different names (rather than init, decap. etc.) will make the function more readable
genListType e = [ Decl ("List_" ++ init e) 
                    [ Prod ("List_"++ init e) [ Field "idd" "IDD" Id
                                              , Field ("elts") ("ConsList_"++init e) Id]
                    ] DeclList
                , Decl ("ConsList_" ++ init e) 
                    [prodCons
                    ,prodNil] DeclConsList
                ] where
                   prodNil     = Prod ("Nil_" ++init e)     []
                   prodCons    = Prod ("Cons_"++init e)     [(Field ("head") (init e) Data)
                                                            ,(Field ("tail")  ("ConsList_"++init e)  Data)]
---                



indent n = map (sp++) where
 sp = replicate n ' '


prListLine = foldl (\a b->a++" "++b) ""