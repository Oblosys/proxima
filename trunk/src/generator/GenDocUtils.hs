module GenDocUtils where

import GenCommon

-- Doesn't generate for hole or ParseErr
-- Fix: Either add a String doing it or extend the type with it

-------------------------------------------------------------------
--           G E N E R A T E    D O C    U T I L S               --
-------------------------------------------------------------------

genDocUtils :: [String] -> File -> [String]
genDocUtils include parsedFile@(File m d) =  
                 include ++ 
                 [  defaultLimit 
                 ,  "\n{- ------------------------------------"
                 ,  "\n generated part"
                 ,  "\n-------------------------------------- -}"
                 ]
                 ++ genRankNode    allConstructors
                 ++ ["\n\n"]
                 ++ genPathNode    allConstructors
                 ++ ["\n\n"]
                 ++ genFuncIDD     parsedFile
                 ++ ["\n\n"]
                 ++ genShallowShow extendedTypes 
                 ++ ["\n\n"]
                 ++ genToXML       extendedTypes 
 where extendedTypes@(File m d') = (File m (d++(genListTypes parsedFile)))
       allConstructors = [ (tp, cnstr, map fieldType cs, decltp) 
                         | Decl tp prods decltp <- d'
                         , decltp /= DeclConsList
                         , Prod cnstr cs <- prods ++[Prod ("Hole"++tp) []] ]  --- *** hack

-- hole support is nasty, it should be in the decls already
-- so we need a function that adds holes and a function that adds parse errs, then the 
-- generator parts can use the datatype with holes & parse errors, or without.       

genRankNode constrs = [ "rankNode :: Node -> Int"
                      , "rankNode NoNode            = 0"
                      , "rankNode (RootDocNode _ _) = 1"  
                      , "rankNode (HoleDocumentNode _ _) = 2"]
                      ++ (map makeNodeAlt $ zip constrs [3..]) 
 where makeNodeAlt ((_, cnstr, _, _),i) = "rankNode ("++ cnstr ++"Node _ _)  = "++show i

{- Path Node -} 
genPathNode constrs = [ "pathNode :: Node -> PathDoc"
                      , "pathNode NoNode            = NoPathD"
                      , "pathNode (RootDocNode _ pth) = PathD pth"
                      , "pathNode (HoleDocumentNode _ pth) = PathD pth"]  -- RUI: added this line
                      ++ (map makeNodeAlt constrs) 
 where makeNodeAlt (_, cnstr, _, _) = "pathNode ("++ cnstr ++"Node _ pth)  = PathD pth"


--- this one doesn't work for holes, but it will be obsolete soon anyway
{- Function IDD -} 
genFuncIDD :: File -> [String]
genFuncIDD (File _ decls) = concatMap printDeclIDD decls

printDeclIDD (Decl d prods _) =  concatMap (printProdIDD d) prods

printProdIDD d (Prod s fields) 
          = let name = decapitalize s
            in  if null fields then [name++"IDD _                                   = Nothing\n"]
                else    [ name++"IDD :: Node -> Maybe IDD"
                        , name++"IDD ("++s++"Node ("++s++ " iDP"++concat(replicate ((length fields)-1) " _")++") _) = Just iDP"
                        , name++"IDD _                                   = Nothing\n"]






{- Function Shallow -} 
genShallowShow :: File -> [String]
genShallowShow (File _ decls) = concatMap printDeclSS decls

printDeclSS (Decl d prods _) =  map (printProdSS d) prods

printProdSS d (Prod s fields)
              = "shallowShow"++d++"1 ("++s++" "++concat(replicate ((length fields)) " _") ++") = "++show s



{- Function XML -} 

genToXML :: File -> [String]
genToXML (File _ decls) = concatMap printDeclXML decls

printDeclXML (Decl d _     DeclList)     =  []
printDeclXML (Decl d prods DeclDef)      =  map (printProdXML d) prods
printDeclXML (Decl d prods DeclConsList) =  
          let   decl =  drop 9 d
          in    [ "toXMLList_"++decl++" (List_"++decl++" _ "++(decapitalize decl)++"s) = toXMLConsList_"++decl++" "++(decapitalize decl)++"s"
                , "toXMLConsList_"++decl++" (Cons_"++decl++" "++(decapitalize decl)++" "++(decapitalize decl)++"s) = toXML"++decl++" "++(decapitalize decl)++" : toXML"++ decl++"s "++(decapitalize decl)++"s"
                , "toXMLConsList_"++decl++" Nil_"++decl++"             = []"
                , "toXML"++decl++"s _                           = []"
                ]





printProdXML d (Prod p fields) =
             let fieldsNotListNotIDs = filter isNotListNotIDs  fields
                 fieldsList = (filter (isList) fields)
                 listXML =  if   null fieldsList
                            then []
                            else typeList (head fieldsList)                    
                 argsXML = foldr  (\a b ->a++" ++ "++b) "[]" (map singleArg fieldsNotListNotIDs)
                 finalXML = if (null listXML) then argsXML else (if null argsXML then listXML ++" ++ "++argsXML else listXML)
             in  "toXML"++d++" ("++ p ++ (concatMap printVar fields)++") = Elt " ++ (show p)++" [] $ " ++ finalXML



printVar f@(Field var _ varType) =if (isID f) then " _" else " "++var

getVar fields = map (\(Field var _ _)->var) (filter (not.isID) fields)

typeList (Field varName varType _) = "toXML"++varType++" "++varName 

listArg  d fields   = "map toXML"++d++" "++concat(showAsList(getVar fields))  

singleArg :: Field -> String
singleArg (Field varName varType _)= "[toXML"++varType++" "++varName++"]"




--            [ "toXML"++d++" (Cons"++d++" _ x xs) = toXML"++(init d)++" x : toXML"++d++" xs" --not so nice
--            , "toXML"++d++" (Nil"++d++" _)       = []"
--            , "toXML"++d++" _                    = []"]
{-
genNode parsedFile    = [    "data Node = NoNode "]
                        ++ [ "          | EnrichedDocNode EnrichedDoc Path" ] --- does not appear as field, but should be in Node
                        ++ indent 10 (map makeNodeAlt fields)  where
                        fields = removeRepeat(getFields' parsedFile)
                        makeNodeAlt (Field _ ('C':'o':'n':'s':'L':'i':'s':'t':'_':_) _) = ""
                        makeNodeAlt e = "| "++fieldType e ++"Node "++fieldType e++" Path "
-}
--                 argsXML = if ((length fieldsNotListNotIDs)==1) then singleArg (head fieldsNotListNotIDs) else listArg  d fieldsNotListNotIDs

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


toXMLConsList_Slide-- ??????????


toXMLConsList_Decl (Cons_Decl decl decls) = toXMLDecl decl : toXMLDecls decls
toXMLConsList_Decl Nil_Decl             = []
toXMLDecls _                           = []

toXMLConsList_Exp (Cons_Exp exp exps) = toXMLExp exp : toXMLExps exps
toXMLConsList_Exp Nil_Exp             = []
toXMLExps _                           = []


toXMLConsList_Alt (Cons_Alt alt alts) = toXMLAlt alt : toXMLAlts alts
toXMLConsList_Alt Nil_Alt             = []
toXMLAlts _                           = []
-}
