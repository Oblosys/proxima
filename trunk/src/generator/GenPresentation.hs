module Main where
import List
import UU.Parsing
import UU.Parsing.CharParser
import System
import Char

---------------------------------------------------------------------
--       M A I N                                                   --
---------------------------------------------------------------------

main = do [fname]      <- getArgs
          generateFiles fname


generateFiles fname  
     = do putStr $ "Parsing File: "++(show fname)++" ..."
          parsedFile <- parseFile show pDataDefs fname
          putStr $ " done\nDocumentTypes_Generated.hs..."
          genDocumentTypes parsedFile
          putStr $ " done\nDocumentEdit_Generated.hs..."
          genDocumentEdit parsedFile
          putStr $ " done\nPresentationAG_Generated.ag..."
          genPresentationAG parsedFile
          putStr $ " done\n"


--          putStrLn $ show parsedFile
--          putStrLn $ "*********************"
--          putStrLn $ show (genEditableInstances parsedFile)
--          genAg modName datas



genDocumentTypes = writeFile ("DocumentTypes_Generated.hs") . unlines . documentTypes 
genDocumentEdit  = writeFile ("DocumentEdit_Generated.hs")  . unlines . documentEdit 
genPresentationAG= writeFile ("PresentationAG_Generated.ag"). unlines . presentationAG

---------------------------------------------------------------------
--       D A T A                                                   --
---------------------------------------------------------------------

type ModuleName = String

data File  = File  ModuleName [Decl]        deriving Show
data Decl  = Decl  String [Prod]           deriving Show

data Prod  = Prod  String [Field]          deriving Show
data Field = Field String String Type      deriving Show

data Type = List 
          | Prim 
          | Data
          | Id                      
          | Ids                            deriving (Show, Eq) 

-------------------------------------------------------------------
--           G E N E R A T E    A G    F I L E S                 --
-------------------------------------------------------------------


presentationAG parsedFile =  
                 [  "\n{- ------------------------------------"
                 ,  "\n Generated File"
                 ,  "\n------------------------------------- -}"
                 ]
                 ++ ["\n\n-- Constant  Types --\n"]
--                 ++ boardTypes
                 ++ ["\n\n-- Generated Types --\n"]
                 ++ genAgTypes (addedTypes parsedFile)
                 ++ genSem     (addedTypes parsedFile)
                 ++ genAtt     (addedTypes parsedFile)
                 where
                 addedTypes parsedFile@(File m d) = (File m (d++(genListTypes parsedFile)))

{-
                 ++ ["\n\n-- Attr --\n"]
                 ++ genATTR (defTypes++listTypes)
                 ++ genATTRPress listTypes
                 ++ genATTRPres (tail defTypes)
                 ++ ["\n\n-- Generated Gen Data Sem --\n"]
                 ++ genDataSem ds
                 ++ ["\n\n-- Generated Unique IDs --\n"]
                 ++ genSem ds
-}


-------------------------------------------------------------------
--        D A T A    T Y P E     D E C L A R A T I O N           --
-------------------------------------------------------------------


genAgTypes (File _ decls) = concatMap printAgDecl decls where
    printAgDecl (Decl e (p:prods)) = ["\n\nDATA "++e++" |" ++ (tail(printAgProd  p)) ]  
                                     ++ indent (6+(length e)) ((map printAgProd prods)
                                     ++ ["| Hole"++e
                                        ,"| ParseErr"++e++" Node Presentation"])
    printAgProd (Prod s fields)= "| "++s++" "++ concatMap (\(Field n s _) -> (n++":"++s++" ")) fields 



-------------------------------------------------------------------
--        S E M A N T I C S                                      --
-------------------------------------------------------------------



genSem (File _ decls) = concatMap genSem' decls where
         genSem' d@(Decl e l) = if   isDeclList d
                                then semList e ++ (genHoles e "s") 
                                     ++ [("\n\nSem "++e)] ++ indent 2 (concatMap doIdC  l)
                                else [("\n\nSem "++e)] ++ indent 2 (concatMap doIdC  l) ++ (genHoles  e "") ++
                                     [("\n\nSem "++e)] ++ indent 2 (concatMap doPath l) 
                                     ++ genIx d


         doIdC  (Prod e ls) = let l =  takeFieldsNames ls
                              in  if null l then []
                                  else lines ("| "++e++" "++(head l)++".idC = @lhs.idC" ++ " + " 
                                       ++ (show (length (filter (not.isID) ls)))) 
                                       ++ indent (3+(length e)) (linkAttrIdC l)

-- linkAttrIdC ["a","b","c"] = ["c.idC = @b.idC","b.idC = @a.idC","lhs.idC = @c.idC"]   
         linkAttrIdC :: [String] -> [String]
         linkAttrIdC l    = aux (l, (length l)-1)
         aux (l,0) = lines $ "lhs.idC = @"++ (last l)++ ".idC"
         aux (l,n) = lines (l!!n++".idC = @" ++ l!!(n-1)++ ".idC") ++ (aux (l,n-1))

         doPath (Prod e ls) = let l =  takeFieldsNames ls
                              in  if null l then []
                                  else lines ("| "++e++" "++ (head l)++".path  = @lhs.path++[0]")
                                       ++ indent (3+(length e)) (path 1 (tail l))
         path _ [] = []
         path n (e:es)= [(e++".path  = @lhs.path++["++(show n)++"]")] ++ (path (n+1) es)

         takeFieldsNames l = map (\(Field s _ _)->s) (filter (not.isID) l)
         
         takeFieldsListNames l = map (\(Field s _ List)->s) (filter (isList) l)


         genIx (Decl e prods) = let l = concatMap (\(Prod _ f)->takeFieldsListNames f) prods
                                in if null l then []
                                   else [("\n\nSem "++e)] ++ indent 2 (concatMap doIx  prods) 
         doIx (Prod e ls) = let l =  takeFieldsListNames ls
                            in  if null l then []
                                   else lines ("| "++e++" "++ (head l)++".ix = 0")
                            ++ indent (3+(length e)) (map (\a->a++".ix = 0") (tail l))



         semList e =  [ "\nSEM "++e
                      , "  | Cons"++e++" "++(map toLower (init e))++".path  = @lhs.path++[@lhs.ix]"
                      , concat(replicate (9+(length e)) " ") ++ (map toLower e)++".path = @lhs.path"
                      ,"  | Cons"++e++"     lhs.press = @"++(map toLower (init e))++".pres : @"++(map toLower e)++".press"
                      ,"  | Nil"++e++"      lhs.press = []"                     
                      ,"\n\nSEM "++e++" [ ix : Int | | path : {[Int]} ]"
                      ,"  | Cons"++e++"     alts.ix  = @lhs.ix + 1"
                      ,"                 lhs.path = @lhs.path"
                      ,"  | Nil"++e++"      lhs.path = @lhs.path"
                      ,"  | Hole"++e++"     lhs.path = @lhs.path"
                      ,"  | ParseErr"++e++" lhs.path = @lhs.path"
                      ]
         genHoles e s =  let sl= if s=="s" then "[" else ""
                             fl= if s=="s" then "]" else ""
                         in ["  | Hole"++e++"     lhs.pres"++s++" = "++sl++"presHole @lhs.focusD \""++e++"\" ("++e++"Node @self @lhs.path) @lhs.path"++fl
                            ,"  | ParseErr"++e++" lhs.pres"++s++" = "++sl++"presParseErr @node @presentation"++fl
                            ]


         -------------------------------------------------------------------------------------------

         -------------------------------------------------------------------------------------------


-------------------------------------------------------------------
--        A T T R I B U T E   D E C L                            --
-------------------------------------------------------------------

genAtt (File _ decls) = let all    = (map (\(Decl n _ )-> n) decls) 
                            lists  = (map (\(Decl n _ )-> n) (filter isDeclList decls)) 
                            noList = (map (\(Decl n _ )-> n) (filter (not.isDeclList) decls)) 
                        in   ["\n\nATTR " ++ prListLine (filter (/="Document") all)
                             ,"     Board BoardRow BoardSquare [ path : {[Int]} |  idC : Int  | ]"
                             ,"\n\nATTR " ++ prListLine all
                             ,"     Board BoardRow BoardSquare [ focusD : FocusDoc | | ]"  
                             ,"\n\nATTR " ++ prListLine lists ++ " [ | | press : {[Presentation]} ]"
                             ,"\n\nATTR " ++ prListLine noList ++ " [ | | pres : Presentation ]"
                             ,"\n\nATTR " ++ prListLine (filter (/="Document") noList) ++ " [ ix : Int || ]"
                             ]









{-  U T I L  -}

-- Generated Unique IDs --

{-
-- enumRepData ["Exp","Exp","Exp","Alt"] = ["Exp1","Exp2","Exp3","Alt"]
enumRepData :: [String] -> [String]
enumRepData l = c l l
      where
         c []     l = l
         c (x:xs) l = c xs (chg x l)
         chg v l =  if (length(filter (==v) l)>1) then chg' (v,1,l) else l
         chg' (_,_,[])     = []
         chg' (v,n,(x:xs)) = if (v == x) then (x++(show n)):(chg' (v,n+1,xs)) else x:(chg' (v,n,xs))
-}
isDeclList (Decl e ((Prod cons _):(Prod nil _):xs)) = 
                    (startsWith "Cons" cons) &&  (startsWith "Nil" nil) 
isDeclList _ = False

startsWith [] _ = True
startsWith (x:xs) (y:ys) = x==y && (startsWith xs ys)
startsWith _ _ = False







---------------------------------------------------------------------
--       E D I T A B L E     C L A S S                             --
---------------------------------------------------------------------

documentTypes :: File -> [String]
documentTypes parsedFile =
                 [  "module DocumentTypes_Generated where "
                 ,  "import CommonTypes"
                 ,  "import {-# SOURCE #-} DocumentTypes"
                 ,  "import PresentationTypes"
                 ,  "import List"
                 ,  "import Char"
                 ,  "\n{- ------------------------------------"
                 ,  "\n Generated File"
                 ,  "\n-------------------------------------- -}"
                 ]
                 ++ ["-- Constant  Types --"]
                 ++ constTypes 
                 ++ ["\n\n-- Generated Types --\n"]
                 ++ genDataTypes (addedTypes parsedFile)
                 ++ ["\n\n-- Generated Types --\n"]
                 ++ genClipDoc (addedTypes parsedFile)
                 ++ genNode (addedTypes parsedFile)   where
                addedTypes parsedFile@(File m d) = (File m (d++(genListTypes parsedFile)))
                primTypes parsedFile = ["Int", "Bool", "String"]

documentEdit :: File -> [String]
documentEdit parsedFile =
                 [  "module DocumentEdit_Generated where "
                 ,  "import CommonTypes"
                 ,  "import DocumentTypes"
                 ,  "import DocumentUtils"
                 ,  "import PresentationTypes"
                 ,  "import IOExts"
                 ,  "\n{- ------------------------------------"
                 ,  "\n Generated File"
                 ,  "\n-------------------------------------- -}"
                 ]
                 ++ ["\n\n-- Generated clipfunctions  --\n"]
                 ++ clipfunctions (addedTypes parsedFile)                              
                 ++ ["\n\n-- Editable Class --\n"]
                 ++ editableClass
                 ++ ["\n\n-- Editable Instances --\n"]
                 ++ genEditableInstances (addedTypes parsedFile) 
                 ++ (concatMap primitiveEdit primTypes) where
                addedTypes parsedFile@(File m d) = (File m (d++(genListTypes parsedFile)))
                primTypes = ["Int", "Bool", "String"]



genNode parsedFile =    [ "data Node = NoNode "
                        , "          | DocNode Document Path    -- Path will always be []"
                        , "          | EnrNode EnrichedDoc Path -- Path will always be []"]
                        ++ indent 10 (map (\e->"| "++e ++"Node "++e) fields)  where
                        fields = removeRepeat(getFields parsedFile)

genClipDoc parsedFile = ("data ClipDoc = Clip_" ++ (head fields) ++" "++(head fields)):  
                        indent 13 ((map (\e->"| Clip_" ++ e ++" "++e) (tail fields)) 
                        ++ ["| Clip_Nothing deriving Show\n"]) where
                        fields = removeRepeat(getFields parsedFile)


clipfunctions parsedFile = genArityClip ++ genAlternativesClip ++ genHoleClip ++ genIsListClip ++ genInsertListClip ++ genRemoveListClip where
         fields = removeRepeat(getFields parsedFile)
         {- arityClip -}
         genArityClip = genClip "\narityClip :: ClipDoc -> Int" "arity" "-1" 

         {- alternativesClip -}
         genAlternativesClip = genClip "\nalternativesClip :: ClipDoc -> [ (String, ClipDoc) ]" "alternatives"  "[]" 

         {- isListClip -}
         genIsListClip = genClip "\nisListClip :: ClipDoc -> Bool" "isList"  "False" 

         {- holeClip -}
         genHoleClip = ["\nholeClip :: ClipDoc -> ClipDoc"] 
                    ++ map (\e->"holeClip (Clip_"++e++" x) = Clip_"++e++" hole" ) fields

         {- insertListClip -}
         genInsertListClip = ["\ninsertListClip :: Int -> ClipDoc -> ClipDoc -> ClipDoc"] 
                          ++ map (\e->"insertListClip i c (Clip_"++e++" x) = insertList i c x") fields
                          ++ ["insertListClip i c (Clip_Nothing)   = Clip_Nothing"]

         {- removeListClip -}
         genRemoveListClip = ["\nremoveListClip :: Int -> ClipDoc -> ClipDoc"]
                          ++ map (\e->"removeListClip i (Clip_"++e++" x) = removeList i x") fields
                          ++ ["removeListClip i (Clip_Nothing)   = Clip_Nothing"]

         {- Lib -}
         genClip sign name nothVal = 
                 sign:(map (genF name) fields) ++ [name++"Clip (Clip_Nothing)   = "++nothVal]
         genF name e = name++"Clip (Clip_"++e++" x) = "++name++" x"



{-  Genereate Types         -}
genListTypes :: File -> [Decl]
genListTypes parsedFile = map genListType (getLists parsedFile)

genListType e= Decl e [prod,prodNil] where
                   prodNil     = Prod ("Nil"++e)      [(Field "idd" "IDD" Id)]
                   prod        = Prod ("Cons"++e)     [(Field "idd" "IDD" Id)
                                                      ,(Field (toLower(head e):(tail(init e))) (init e) Data)
                                                      ,(Field (toLower(head e):(tail(e)))  e List)]

genDataTypes :: File -> [String]
genDataTypes (File _ decls) = concatMap printDecl decls

printDecl (Decl e prods)=   ["\n\ndata "++e++" =" ++ (tail(printProd  (head prods))) ]
                            ++ indent (6+(length e)) (map printProd (tail prods))
                            ++ indent (6+(length e)) ["| Hole"++e]
                            ++ indent (6+(length e)) ["| ParseErr"++e++" Node Presentation"]
                            ++ indent (8+(length e)) [" deriving Show"]                               


printProd :: Prod -> String
printProd (Prod s fields)= "| "++s++" "++ concatMap (\(Field _ s _) -> (s++" ")) fields -- (foldl (\(Field _ a _) (Field _ b _)->a++" "++b) "" fields)

count :: [Field] -> (Int,Int)
count f = (,) (length(filter isID f)) (length(filter (not.isID) f))

{-  i n s t a n c e      E d i t a b l e  -}
genEditableInstances f@(File mn decls) = concatMap instances decls where
         instances d@(Decl e prods) = ["\n\ninstance Editable " ++ e ++ " where"] 
                                    ++ (if isList(d) then select' (e,prods) else select (e,prods))
                                    ++ paste (e,prods) 
                                    ++ alternatives (d) ++ arity (e,prods) ++ hole e  
                                    ++ (if (isList d) then genInsertList e else [])
                                    ++ (if (isList d) then genRemoveList e else [])
         
         isList (Decl e ((Prod s _):prods)) = ("Cons"++e)== s
         {- select -}
         select'   (e,prods) = (emptySelect e)++(bodySelect' (head prods)) ++ noSelect   
         select    (e,prods) = (emptySelect e)++(concatMap bodySelect prods) ++ noSelect   
         bodySelect (Prod s fields) = genSelect s (count fields) 

         bodySelect' (Prod s fields) = genSelectL s (length(filter isID fields))

         genSelect  e (v,v')   = genSelect' v' e (v,v')
         genSelect' 0 e (v,v') = []
         genSelect' n e (v,v') = genSelect'  (n-1) e (v,v') 
                               ++ ["  select ("++ (show (n-1)) ++":p) ("++ e ++ concat(replicate v " _") 
                               ++ (genVar v' " x") ++") = select p x" ++ show n] 

         genSelectL e v = ["  select (0:p) ("++ e ++ concat(replicate v " _") ++ " x _) = select p x" ]
                        ++["  select (n:p) ("++ e ++ concat(replicate v " _") ++ " _ xs) = select p xs"]
 

         emptySelect tp  = ["  select []    x                  = Clip_"++ tp ++" x"]
         noSelect        = ["  select _     _                  = Clip_Nothing\n"]

  
         {- Paste -}
         paste (e,l)    = (emptyPaste e) ++ concatMap bodyPaste l++ noPaste

         bodyPaste (Prod e l)=   genPaste e (count l)

         genPaste    e (v,v') = genPaste' v' e (v,v')   
         genPaste' 0 e (v,v') = []         
         genPaste' n e (v,v') = genPaste'  (n-1) e (v,v') 
                                ++ ["  paste ("++ (show (n-1)) ++":p) c ("++ e 
                                ++ (genVar v " i") ++ (genVar v' " x") ++") = " 
                                ++ e ++ (genVar v " i") ++ (genVar' v' n)]

         emptyPaste  e  = [ "  paste [] (Clip_"++e++" c) _      = c"
                          , "  paste [] c  x                    = trace (\"Type error: pasting \"++show c++\" on "++e++"\")   x"]
         noPaste        = [ "  paste _  _  x                    = x",""]

         genVar   0 _            = ""
         genVar   n v            = (genVar (n-1) v)++v++(show n)

         genVar'   0 _             = ""
         genVar'   i n             = if (i==n) then (genVar' (i-1) n)++ " (paste p c x"++(show i)++")"
                                               else (genVar' (i-1) n)++ " x"++(show i)


         {- alternatives -}
         alternatives d@(Decl e l) = if (isList d)
                                     then [altList e]
                                     else [startAlt ++ concatMap (bodyAlt e) l ++ (endAlt e)]

         startAlt           = "  alternatives _ = ["
         endAlt  e          = "(\"{"++e++"}\", Clip_"++e++" hole)\n                   ]\n"
        
         bodyAlt ep (Prod e l)   = let (s,r) = func2 ("",l)
                              in  genAlt ep s (e,l)
         genAlt ep s (e,l)  = "(\""++e++" "++(braces (map (\(Field _ o _)->o) (filter (not.isID) l)))++
                              "\"  , Clip_"++ep++" $ "++e++s++")\n                   ,"


         altList e = "  alternatives _ = [ (\"add {"++(init e)++"}\", Clip_"++(init e)++" $ hole)]\n"

         func2 (s,[])     = (s,[])
         func2 (s,((Field _ l t):ls)) = 
                            if (t == Id) then func2 (s++" No"++l{-(show t)-},ls) 
                            else if (t==List) then func2 (s++" []",ls) else func2 (s++" hole",ls)

         braces l = concat ["{"++x++"} "|x<-l] --(printList "" "{" "}" "} {")
   

         {- arity -}
         arity (e,l) = map bodyArity l ++ endArity
         endArity          = ["  arity _                        = 0\n"]
         bodyArity (Prod e l)   = genArity e (count l) 

         genArity e (v,v') = "  arity ("++e++concat(replicate v " _")++ (genVar v' " x") ++") = " ++ show v'


         {- hole  -}
         hole e = ["  hole = Hole" ++ e]

         {- insertList -}
         genInsertList e = [ "  isList _ = True\n"
                           , "  insertList 0 (Clip_"++(init e)++" x )  xs = Clip_"++e++" $ Cons"++e++" NoIDD x xs"
                           , "  insertList 0 _                     xs = trace \"Type error, no paste\" $ Clip_"++e++" $ xs"
                           , "  insertList n c (Cons"++e++" i1 x xs) = let (Clip_"++e++" xs') = insertList (n-1) c xs"
                           , "                                           in   Clip_"++e++" $ Cons"++e++" i1 x xs'"
                           , "  insertList _ c xs                   = Clip_"++e++" xs\n"
                           ]

         {- removeList -}
         genRemoveList e = [ "  removeList 0 (Cons"++e++" i1 x xs) = Clip_"++e++" $ xs"
                           , "  removeList n (Cons"++e++" i1 x xs) = let (Clip_"++e++" xs') = removeList (n-1) xs"
                           , "                                        in   Clip_"++e++" $ Cons"++e++" i1 x xs'"
                           , "  removeList _ xs                        = Clip_"++e++" xs\n"
                           ]

-- ---------------------------------------------------------------------------------------------------------------



{- L I B -}

getLists (File _ decls) = removeRepeat (concatMap getF decls) where
                           getF (Decl _ l) = concatMap f l
                           f (Prod _ l) = map (\(Field _ f _)->f) (filter (isListNotIDs) l)

isListNotIDs a = (isList a) && not(isID a) 

isList (Field _ _ a) = a==List


--getDecls  (File _ decls) = (filter (/="Document")) $ map (\(Decl s _)->s) decls

getFields (File _ decls) = removeRepeat (concatMap getF decls) where
                           getF (Decl _ l) = concatMap f l
                           f (Prod _ l) = map (\(Field _ f _)->f) (filter (not.isID) l)
                        
isID (Field _ _ a) = (a==Id) || (a==Ids)



removeRepeat l = reverse(removeRepeat' [] l) where
        removeRepeat' l []     = l
        removeRepeat' l (x:xs) = if (elem x l) then removeRepeat' l xs 
                                               else removeRepeat' (x:l) xs


{-

type ModuleName = String

data File  = File  ModuleName [Decl]        deriving Show
data Decl  = Decl  String [Prod]           deriving Show

data Prod  = Prod  String [Field]          deriving Show
data Field = Field String String Type      deriving Show

data Type = List 
          | Prim 
          | Data
          | Id                      
          | Ids                            deriving (Show, Eq) 

-}


---------------------------------------------------------------------
--       P A R S E R                                               --
---------------------------------------------------------------------

pDataDefs :: CharParser  File --(String,DataDefs)
pDataDefs = (File "")  <$> (pSpaces *> pList_ng (pDecl)) where -- pToken "module" <*> pConid <* pToken "where"                  
 pDecl = Decl <$ pToken "data"
              <*> pConid
              <*  pSpec '='
              <*> pList1Sep (pSpec '|') pProd
              <*  (pDeriving <|> pSucceed ())


 pProd  = Prod <$> pConid <*> (pField <|> (const [] <$> pSpaces))
 
 pField :: CharParser [Field]
 pField =  flip (++) <$> pList (makeField <$> opt (Just <$> pVarid <* (pToken ":")) Nothing
                                                                 <*> (((\a->(a,Prim))<$>pConid) <|> ((\a->(a++"s",List))<$>pDataList'))) 
                     <*> pComments


 makeField Nothing     (tpName@(c:cs), tp) = Field  (toLower c:cs) tpName tp
 makeField (Just name) (tpName, tp) = Field name tpName tp

 --(pConid <|> pDataList' <|> pParens pConid )
 --makeField :: Maybe String -> (String, String) -


 pComments :: CharParser [Field]
 pComments =  (pToken "{") *> pList (makeField 
                                      <$> opt (Just <$> pVarid <* (pToken ":")) Nothing -- pList ((\a b ->Field a (fst b) (snd b)) <$> pVarid <* (pToken ":") 
                                      <*> (  ((\a->(a, Id))<$>pConid) 
                                      <|> ((\a->(a++"s", Ids))
                                      <$> pDataList')))  
           <* (pToken "}")

 pDataList :: CharParser String
 pDataList  = (\a b c ->a++b++c) <$> (pToken "[") <*> pConid <*> (pToken "]")

 pDataList' :: CharParser String
 pDataList' = (\a ->a++"s") <$> (pToken "[") *> pConid <* (pToken "]")

 


 pDeriving = () <$ pToken "deriving" <* (() <$ pConid <|> () <$ pParens (pList1Sep (pSpec ',') pConid))



indent n = map (sp++) where
 sp = replicate n ' '



prListLine = foldl (\a b->a++" "++b) ""
{-

showAsList = printList "[]" "[" "]" ", "

printList e o c s xs = case xs of
  []     -> [e]
  (x:xs) -> (o ++ x) : map (s ++) xs  ++ [c]
-}

---------------------------------------------------------------------
--       C O N S T A N T        V A L U E S                        --
---------------------------------------------------------------------







boardTypes  =   [""
                ,"DATA Board       | Board    idD: IDD r1, r2, r3, r4, r5, r6, r7, r8 : BoardRow"
                ,"DATA BoardRow    | BoardRow idD: IDD ca, cb, cc, cd, ce, cf, cg, ch : BoardSquare"
                ,"DATA BoardSquare | Queen idD : IDD color : Bool"
                ,"                 | King  idD : IDD color : Bool"
                ,"                 | Bishop idD : IDD color : Bool"
                ,"                 | Knight idD : IDD color : Bool"
                ,"                 | Rook idD : IDD color : Bool"
                ,"                 | Pawn idD : IDD color : Bool"
                ,"                 | Empty "
                ]







editableClass :: [String]
editableClass = [""
                ,"class Editable a where"
                ,"  select :: PathD -> a -> ClipDoc"
                ,"  paste :: PathD -> ClipDoc -> a -> a"
                ,"  alternatives :: a -> [ (String, ClipDoc) ]"               
                ,"  arity :: a -> Int"
                ,"  hole :: a"
                ,"  "
                ,"  isList :: a -> Bool"
                ,"  isList _ = False"
                ,"  insertList :: Int -> ClipDoc -> a -> ClipDoc"
                ,"  insertList _ _ _ = Clip_Nothing"
                ,"  removeList :: Int -> a -> ClipDoc"
                ,"  removeList _ _ = Clip_Nothing"
               ]


constTypes  = 
                [" "
                ,"data DocumentLevel = DocumentLevel Document FocusDoc ClipDoc"
                ,"data PathDoc = NoPathD | PathD PathD deriving Show"                
                ,"data ID = NoID | ID Int deriving Show"
                ," "
                ,"type Presentation = Int"
                ,"type Node = Int"
                ,"type FocusDoc = PathDoc"
                ,"type PathD = [Int]"
                ]





primitiveEdit f 
        = case f of
             "Bool" ->  [ "instance Editable Bool where                         "
                        , "  select [] x = Clip_Bool x                            "
                        , "  select _  _ = Clip_Nothing                           "
                        , "  paste [] (Clip_Bool c) x = c                         "
                        , "  paste [] c             x =  trace (\"Type error: pasting \"++show c++\" on Bool\") x"
                        , "  alternatives _ = [ (\"True\", Clip_Bool True)        "
                        , "                   , (\"False\", Clip_Bool False)      "
                        , "                   , (\"{Bool}\", Clip_Bool hole) ]    "
                        , "  arity _ = 0                                          "
                        , "  hole = False\n" 
                        ]

             "Int"  ->  [ "instance Editable Int where"
                        , "  select [] x = Clip_Int x"
                        , "  select _  _ = Clip_Nothing"
                        , "  paste [] (Clip_Int c) x = c"
                        , "  paste [] c            x =  trace (\"Type error: pasting \"++show c++\" on Int\") x"
                        , "  "
                        , "  alternatives _ = [ (\"0\", Clip_Int 0)"
                        , "                   , (\"1\", Clip_Int 1)"
                        , "                   , (\"{Int}\", Clip_Int hole) ]"
                        , "  "
                        , "  arity _ = 0"
                        , "  hole = -1\n" 
                        ]

             "String" -> [ "instance Editable String where"
                         , "  select [] x = Clip_String x" 
                         , "  select _  _ = Clip_Nothing"
                         , "  paste [] (Clip_String c) x = c"
                         , "  paste [] c             x =  trace (\"Type error: pasting \"++show c++\" on String\") x"
                         , ""
                         , "  alternatives _ = [ (\"a\", Clip_String \"a\")"
                         , "                   , (\"ab\", Clip_String \"ab\")"
                         , "                   , (\"{String}\", Clip_String hole) ] "
                         , " "
                         , "  arity _ = 0"
                         , "  hole = \"{String}\"\n"
                         ]

             x      -> [""] -- "--Error: no Editable instance of " ++ x ++"\n\n" 









-- CharParser ----------------------------------------------

lexeme :: IsParser p Char => p a -> p a
lexeme p = p <* pSpaces

pSpaces :: IsParser p Char => p ()
pSpaces = () <$ pList_ng (pSpace <|> pComment)  <?> "whitespace"

pSpace :: IsParser p Char => p ()
pSpace = () <$ pAnySym (nub " \LF\CR\n\r\t\v\f") <?> "a space"

pComment :: IsParser p Char => p ()
pComment = () <$ pToks "--" <* pList pAnyChar <* pSym '\n'
  where pAnyChar = ' ' <..> '~' <|> pSym '\t' <?> "any character"

{-
pComment2 :: IsParser p Char => p ()
--pComment2 = undefined 
pComment2 = () <$ pToks "{-" <* contents 
               <* pToks "-}"

contents :: IsParser p Char => p ()
contents =   (  pAnySym <|> pSym '}') <*> contents 
                 <|> pSym '-' *> complex
                 <|> pSym '{' *> complex2 where
        complex  = pSym '}' <|> (pAnyChar <|> (() <$ pSym '{')) <*> contents <|>  (pSym '-' <*> complex)

        complex2 = <*> contents <|> (pAnyChar <|> pSym '}') <*> contents <|>  (pSym '{' <*> complex2)

        pAnyChar =  () <$ ( '!' <..> ','
                 <|> '.' <..> 'z'
                 <|> pSym '~' <|> pSym '|' )
                 <|> pSpace <?> "any character"


pSym' :: IsParser p Char => p ()
pSym' = (() <$ pSym '-') 
-}

pUpper,pLower,pIdentLetter :: IsParser p Char => p Char
pUpper = 'A' <..> 'Z'
pLower = 'a' <..> 'z'
pIdentLetter = pUpper <|> pLower <|> '0' <..> '9' <|> pSym '_'

pInteger :: IsParser p Char => p Int
pInteger = lexeme ((\s v -> s (read v)) <$>  pSign <*> pList1 ('0' <..> '9')) <?> "an integer"
 where pSign = (id <$ pSym '+' <|> negate <$ pSym '-') `opt` id

pSpec :: IsParser p Char => Char -> p Char
pSpec x = lexeme ( pSym x)              <?> "symbol " ++ show x

pToken :: IsParser p Char => String -> p String
pToken t = lexeme (pToks t)              <?> "symbol " ++ show t

pComma :: IsParser p Char => p Char
pComma = pSpec ','

pParens :: IsParser p Char => p a -> p a
pParens p = pPacked (pSpec '(') (pSpec ')') p

pConid, pVarid, pString :: IsParser p Char => p String
pConid  = lexeme ((:) <$> pUpper <*> pList pIdentLetter) <?> "an uppercase identifier"

pVarid  = lexeme ((:) <$> pLower <*> pList pIdentLetter) <?> "an identifier"
pString = lexeme (pSym '"' *> pList pAnyChar <* pSym '"')
  where pAnyChar = ('#' <..> '~' <|> pSym '!'<|> pSym ' ') <?> "any character"

------------------------------------------------------------