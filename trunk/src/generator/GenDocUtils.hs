module GenDocUtils where

import GenCommon

-------------------------------------------------------------------
--           G E N E R A T E    D O C    U T I L S               --
-------------------------------------------------------------------

genDocUtils :: [String] -> File -> [String]
genDocUtils include parsedFile =  
                 include ++ 
                 [  defaultLimit 
                 ,  "\n{- ------------------------------------"
                 ,  "\n generated part"
                 ,  "\n-------------------------------------- -}"
                 ]
                 ++ genPathNode         (addedTypes parsedFile)
                 ++ genFuncIDD          parsedFile
                 ++ genShallowShow      (addedTypes parsedFile)
                 ++ genToXML            (addedTypes parsedFile)
                 where
                 addedTypes parsedFile@(File m d) = (File m (d++(genListTypes parsedFile)))

{- Path Node -} 
genPathNode parsedFile = [ "pathNode :: Node -> PathDoc","pathNode NoNode            = NoPathD"]
                         ++ (map (\e->"pathNode ("++e ++"Node _ pth)  = PathD pth") fields)  where
                         fields = removeRepeat(getFields parsedFile)


{- Function IDD -} 
genFuncIDD :: File -> [String]
genFuncIDD (File _ decls) = concatMap printDeclIDD decls

printDeclIDD (Decl d prods _) =  concatMap (printProdIDD d) prods

printProdIDD d (Prod s fields) 
          = let name = decapitalize s
            in
                if null fields then [name++"IDD _                                   = Nothing\n\n"]
                else    [ name++"IDD :: Node -> Maybe IDD"
                        , name++"IDD ("++d++"Node ("++s++ " iDP"++concat(replicate ((length fields)-1) " _")++") _) = Just iDP"
                        , name++"IDD _                                   = Nothing\n\n"]


{-
printProdIDD d (Prod s fields) 
          = let name = decapitalize s
                idName = if null fields then idD else (\(Field n _ _)->n) (head fields)
            in  [ name++"IDD :: Node -> Maybe IDD"
                , name++"IDD ("++d++"Node ("++s++ " "++idName ++concat(replicate ((length fields)-1) " _")++") _) = Just "++idName
                , name++"IDD _                                   = Nothing\n\n"]

-}


{- Function XML -} 

{- Function IDD -} 
genShallowShow :: File -> [String]
genShallowShow (File _ decls) = concatMap printDeclSS decls

printDeclSS (Decl d prods _) =  map (printProdSS d) prods

printProdSS d (Prod s fields) 
              = "shallowShow"++d++"1 ("++s++" "++concat(replicate ((length fields)) " _") ++") = "++show s


{-
"shallowShow"++d++"1 ("++concat(replicate ((length fields)-1) " _") ++") = "++s



shallowShowExp1 (PlusExp id pid _ _)  = "PlusExp"
shallowShowExp1 (TimesExp id pid _ _) = "TimesExp"
shallowShowExp1 (DivExp id pid _ _)   = "DivExp"
shallowShowExp1 (PowerExp id pid _ _) = "PowerExp"
shallowShowExp1 (IntExp id pid i)     = "IntExp"
shallowShowExp1 HoleExp               = "HoleExp"
shallowShowExp1 (ParseErrExp _ _)     = "ParseErrExp"

shallowShowExp1 _                     = "<EXP>"
-}



genToXML :: File -> [String]
genToXML (File _ decls) = concatMap printDeclXML decls


printDeclXML (Decl d prods DeclDef)  =  map (printProdXML d) prods
printDeclXML (Decl d _ DeclList) =  
            [ "toXML"++d++" (Cons"++d++" _ x xs) = toXML"++(init d)++" x : toXML"++d++" xs" --not so nice
            , "toXML"++d++" (Nil"++d++" _)       = []"
            , "toXML"++d++" _                    = []"]
printDeclXML _  =  ["???????"]

printProdXML d (Prod p fields) =
             let fieldsNotListNotIDs = filter isNotListNotIDs  fields
                 fieldsList = (filter (isList) fields)
                 listXML =  if   null fieldsList
                            then []
                            else typeList (head fieldsList)                    
--                 argsXML = if ((length fieldsNotListNotIDs)==1) then singleArg (head fieldsNotListNotIDs) else listArg  d fieldsNotListNotIDs
                 argsXML = foldr  (\a b ->a++" ++ "++b) "[]" (map singleArg fieldsNotListNotIDs)
                 finalXML = if (null listXML) then argsXML else (if null argsXML then listXML ++" ++ "++argsXML else listXML)
             in  "toXML"++d++" ("++ p ++ (concatMap printVar fields)++") = Elt " ++ (show p)++" [] $ " ++ finalXML


printVar f@(Field var _ varType) =if (isID f) then " _" else " "++var

getVar fields = map (\(Field var _ _)->var) (filter (not.isID) fields)



typeList (Field varName varType _) = "toXML"++varType++" "++varName 
listArg  d fields   = "map toXML"++d++" "++concat(showAsList(getVar fields))  

singleArg :: Field -> String
singleArg (Field varName varType _)= "[toXML"++varType++" "++varName++"]"


