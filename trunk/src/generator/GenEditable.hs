module GenEditable where

import GenCommon
import List ---
---------------------------------------------------------------------
--       E D I T A B L E     C L A S S                             --
---------------------------------------------------------------------

genDocumentEdit :: [String] -> File -> [String]
genDocumentEdit include parsedFile =  
                 include ++ 
                 [  defaultLimit 
                 ,  "\n{- ------------------------------------"
                 ,  "\n generated part"
                 ,  "\n-------------------------------------- -}"
                 ]
                 ++ ["\n\n-- Generated clipfunctions  --\n"]
                 ++ clipfunctions (listFields extendedTypes)
                 ++ ["\n\n-- Editable Instances --\n"]
                 ++ genEditableInstances extendedTypes
                 ++ concatMap primitiveEdit primTypes
                 ++ ["\n\n\n\n\n\n-- ProxParser_Generated --\n"] ---
                 ++ genProxParser        extendedTypes
                 where
                 extendedTypes = extendTypes parsedFile
                 extendTypes parsedFile@(File m d) = (File m (d++(genListTypes parsedFile)))
                 primTypes = ["Int", "Bool", "String"]  --- This doesn't belong here!


{- Clip Functions -}


---
clipfunctions fields =  genArityClip        fields
                     ++ genAlternativesClip fields
                     ++ genHoleClip         fields
                     ++ genIsListClip       fields
                     ++ genInsertListClip   fields
                     ++ genRemoveListClip   fields
                     
 --- no generation for conslists (unsafe, should be done in a different way)
listFields parsedFile = map fieldType . filter (\(Field _ tp _)-> not $ isPrefixOf "ConsList_" tp) $ removeRepeat(getFields' parsedFile) 
--- getFields already does a removeRepeat

{- arityClip -}
genArityClip fields = genClip fields "\narityClip :: ClipDoc -> Int" "arity" "-1" 

{- alternativesClip -}
genAlternativesClip fields = genClip fields "\nalternativesClip :: ClipDoc -> [ (String, ClipDoc) ]" "alternatives"  "[]" 

{- isListClip -}
genIsListClip fields = genClip fields "\nisListClip :: ClipDoc -> Bool" "isList"  "False" 

---
{- holeClip -}
genHoleClip fields = ["\nholeClip :: ClipDoc -> ClipDoc"] 
        ++ map (\e->"holeClip (Clip_"++e++" x) = Clip_"++e++" hole" ) fields
        ++ ["holeClip Clip_Nothing   = Clip_Nothing"]   --- hole also needs a Nothing case (was missing in my source)

{- insertListClip -}
genInsertListClip fields = ["\ninsertListClip :: Int -> ClipDoc -> ClipDoc -> ClipDoc"] 
              ++ map (\e->"insertListClip i c (Clip_"++e++" x) = insertList i c x") fields
              ++ ["insertListClip i c (Clip_Nothing)   = Clip_Nothing"]

{- removeListClip -}
genRemoveListClip fields = ["\nremoveListClip :: Int -> ClipDoc -> ClipDoc"]
              ++ map (\e->"removeListClip i (Clip_"++e++" x) = removeList i x") fields
              ++ ["removeListClip i (Clip_Nothing)   = Clip_Nothing"]

{- Lib -}
genClip fields sign name nothVal = 
        sign:(map (genF name) fields) ++ [name++"Clip (Clip_Nothing)   = "++nothVal] 
        where
        genF name e = name++"Clip (Clip_"++e++" x) = "++name++" x"







{-  i n s t a n c e      E d i t a b l e  -}

genEditableInstances f@(File mn decls) = concatMap instances decls 
instances (Decl "EnrichedDoc" _ _)  = []  --- don't generate for EnrichedDoc (the root)
{- Lists -} ---
instances d@(Decl e prods DeclConsList) =  []
instances d@(Decl e prods DeclList) =  instanceList d ---

{- NON Lists -}
instances d@(Decl e prods DeclDef)  = ["\n\ninstance Editable " ++ e ++ " where"]
                                    ++ select        d 
                                    ++ paste         d
                                    ++ alternatives  d 
                                    ++ arity (e,prods)
                                    ++ hole e


{- select -}
selectList (Decl e prods _) = (emptySelect e)++(bodySelectList (head prods)) ++ noSelect   
select     (Decl e prods _) = (emptySelect e)++(concatMap bodySelect prods) ++ noSelect   

bodySelect (Prod s fields)  = genSelect s (count fields) 
bodySelectList (Prod s fields) = genSelectList s (length(filter isID fields))

genSelect  e (v,v')       = genSelect' v' e (v,v') where
    genSelect' 0 e (v,v') = []
    genSelect' n e (v,v') = genSelect'  (n-1) e (v,v') 
                       ++ ["  select ("++ (show (n-1)) ++":p) ("++ e ++ concat(replicate v " _") 
                       ++ (genVar v' " x") ++") = select p x" ++ show n] 


genSelectList e v = ["  select (0:p) ("++ e ++ concat(replicate v " _") ++ " x _) = select p x" ]
                  ++["  select (n:p) ("++ e ++ concat(replicate v " _") ++ " _ xs) = select (n-1:p) xs"]


emptySelect tp  = ["  select []    x                  = Clip_"++ tp ++" x"]
noSelect        = ["  select _     _                  = Clip_Nothing\n"]



{- Paste -}
paste  (Decl e prods _)    = (emptyPaste e) ++ concatMap bodyPaste prods  ++ noPaste
pasteList (Decl e prods _) = (emptyPaste e) ++ bodyPasteList (head prods) ++ noPaste

bodyPasteList (Prod e l) = genPasteList e (count l) 

bodyPaste (Prod e l)=   genPaste e (count l)

genPasteList e (v,v')
    = ["  paste (0:p) c ("++e++(genVar v " i") ++ " x xs) = "++e++(genVar v " i") ++ " (paste p c x) xs"
      ,"  paste (n:p) c ("++e++(genVar v " i") ++ " x xs) = "++e++(genVar v " i") ++ " x (paste (n-1:p) c xs)" 
      ]

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
alternatives (Decl e l DeclList) =  [altList e]
alternatives (Decl e l DeclDef)  =  [startAlt ++ concatMap (bodyAlt e) l ++ (endAlt e)]
         
startAlt           = "  alternatives _ = ["
endAlt  e          = "(\"{"++e++"}\", Clip_"++e++" hole)\n                   ]\n"

bodyAlt ep (Prod e l)   = let (s,r) = func2 ("",l)
                  in  genAlt ep s (e,l)
genAlt ep s (e,l)  = "(\""++e++" "++(braces (map (\(Field _ o _)->o) (filter (notIDPrim) l)))++
                  "\"  , Clip_"++ep++" $ "++e++s++")\n                   ,"


altList e = "  alternatives _ = [ (\"add {"++(init e)++"}\", Clip_"++(init e)++" $ hole)]\n"

func2 (s,[])     = (s,[])
func2 (s,((Field _ l t):ls)) = 
                if (t == Id) then func2 (s++" No"++l{-(show t)-},ls) 
                else if (t==Ids) then func2 (s++" []",ls) else func2 (s++" hole",ls)

braces l = concat ["{"++x++"} "|x<-l] --(printList "" "{" "}" "} {")

{- arity -}
arityList (Decl tpName prods _) = [ "  arity (Cons"++tpName++" _ _ xs) = 1 + arity xs"
                               , "  arity _ = 0\n" ]

arity (e,l) = map bodyArity l ++ endArity
endArity          = ["  arity _                        = 0\n"]
bodyArity (Prod e l)   = genArity e (count l) 

genArity e (v,v') = "  arity ("++e++concat(replicate v " _")++ (genVar v' " x") ++") = " ++ show v'


{- hole  -}
holeList e = ["  hole = Nil" ++ init e ++ "\n" ]
hole e = ["  hole = Hole" ++ e ++"\n"]

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




count :: [Field] -> (Int,Int)
count f = (,) (length(filter isID f)) (length(filter (not.isID) f))

---------------------------------------------------------------------
--       C O N S T A N T        V A L U E S                        --
---------------------------------------------------------------------


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




primitiveEdit f 
        = case f of
             "Bool" ->  [ "instance Editable Bool where                         "
                        , "  select [] x = Clip_Bool x                            "
                        , "  select _  _ = Clip_Nothing                           "
                        , "  paste [] (Clip_Bool c) x = c                         "
                        , "  paste [] c             x =  trace (\"Type error: pasting \"++show c++\" on Bool\") x"
                        , "  paste _  _             x = x"  ---
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
                        , "  paste _  _             x = x"  ---
                        , "  "
                        , "  alternatives _ = [ (\"0\", Clip_Int 0)"
                        , "                   , (\"1\", Clip_Int 1)"
                        , "                   , (\"{Int}\", Clip_Int hole) ]"
                        , "  "
                        , "  arity _ = 0"
                        , "  hole = 0\n" 
                        ]

             "String" -> [ "instance Editable String where"
                         , "  select [] x = Clip_String x" 
                         , "  select _  _ = Clip_Nothing"
                         , "  paste [] (Clip_String c) x = c"
                         , "  paste [] c             x =  trace (\"Type error: pasting \"++show c++\" on String\") x"
                         , "  paste _  _             x = x"  ---
                         , ""
                         , "  alternatives _ = [ (\"a\", Clip_String \"a\")"
                         , "                   , (\"ab\", Clip_String \"ab\")"
                         , "                   , (\"{String}\", Clip_String hole) ] "
                         , " "
                         , "  arity _ = 0"
                         , "  hole = \"{String}\"\n"
                         ]

             x      -> [""] -- "--Error: no Editable instance of " ++ x ++"\n\n" 







{-
instances d@(Decl e prods _) = ["\n\ninstance Editable " ++ e ++ " where"] 
                        ++ (if isDeclList d then select' (e,prods) else select (e,prods)) --- why use tuples when you have your Decl type?
                        ++ (if isDeclList d then pasteList (e,prods) else paste (e,prods) )
                        ++ alternatives d 
                        ++ (if isDeclList d then arityList d else arity (e,prods))  --- arity was also different for lists
                        ++ (if isDeclList d then holeList e else hole e)           --- hole as well
                        ++ (if isDeclList d then genInsertList e else [])
                        ++ (if isDeclList d then genRemoveList e else [])
                        --- if isList appears almost everywhere now, so put it higher up.
                        
isDeclList (Decl _ _ DeclList) = True
isDeclList (Decl _ _ _) = False
-}

-- e should not be List_Exp, since we need the type "Exp" in the generated code
instanceList (Decl e prods _) =
  [
--- some of this code still needs to be moved
--- these two belong in docUtils
    "toConsList_"++listTp++" [] = Nil_"++listTp++""
  , "toConsList_"++listTp++" (x:xs) = Cons_"++listTp++" x (toConsList_"++listTp++" xs)"
  , ""
  , "fromConsList_"++listTp++" Nil_"++listTp++" = []"
  , "fromConsList_"++listTp++" (Cons_"++listTp++" x xs) = x: fromConsList_"++listTp++" xs"
  , ""
--these three belong somewhere else in DocumentEdit_Generated, not between the instance declaration
  , "replaceList_"++listTp++" _ x Nil_"++listTp++" = Nil_"++listTp++" -- replace beyond end of list"
  , "replaceList_"++listTp++" 0 x (Cons_"++listTp++" cx cxs) = Cons_"++listTp++" x cxs"
  , "replaceList_"++listTp++" n x (Cons_"++listTp++" cx cxs) = Cons_"++listTp++" cx (replaceList_"++listTp++" (n-1) x cxs)"
  , ""
  , "insertList_"++listTp++" 0 x cxs = Cons_"++listTp++" x cxs"
  , "insertList_"++listTp++" _ x Nil_"++listTp++"  = Nil_"++listTp++"   -- insert beyond end of list"
  , "insertList_"++listTp++" n x (Cons_"++listTp++" cx cxs) = Cons_"++listTp++" cx (insertList_"++listTp++" (n-1) x cxs)"
  , ""
  , "removeList_"++listTp++" _ Nil_"++listTp++"  = Nil_"++listTp++" -- remove beyond end of list"
  , "removeList_"++listTp++" 0 (Cons_"++listTp++" cx cxs) = cxs"
  , "removeList_"++listTp++" n (Cons_"++listTp++" cx cxs) = Cons_"++listTp++" cx (removeList_"++listTp++" (n-1) cxs)"
  , ""
-- here's the actual instance declaration
  , "instance Editable List_"++listTp++" where"
  , "  select []    x                  = Clip_List_"++listTp++" x"
  , "  select (n:p) (List_"++listTp++" _ cxs) = let xs = fromConsList_"++listTp++" cxs"
  , "                                  in  if n < length xs "
  , "                                      then select p (xs !! n)"
  , "                                      else Clip_Nothing"
  , "  select _     _                  = Clip_Nothing"
  , ""
  , "  paste [] (Clip_List_"++listTp++" c) _   = c"
  , "  paste [] c  x                  = trace (\"Type error: pasting \"++show c++\" on List_"++listTp++"\")   x"
  , "  paste (n:p) c (List_"++listTp++" i1 cxs) = let xs = fromConsList_"++listTp++" cxs"
  , "                                    in  if n < length xs"
  , "                                        then let x  = xs!!n"
  , "                                                 x' = paste p c x"
  , "                                             in  List_"++listTp++" i1 (replaceList_"++listTp++" n x' cxs)"
  , "                                        else List_"++listTp++" i1 cxs -- paste beyond end of list"
  , "  paste _  _  x                  = x"
  , ""
  , "  alternatives _ = [(\"{List_"++listTp++"}\", Clip_List_"++listTp++" hole)"
  , "                   ]"
  , ""
  , "  arity (List_"++listTp++" _ x1) = length (fromConsList_"++listTp++" x1)"
  , "  arity _                        = 0"
  , ""
  , "  hole = List_"++listTp++" NoIDD Nil_"++listTp++""
  , ""
  , "  isList _ = True"
  , ""
  , "  insertList n (Clip_"++listTp++" c) (List_"++listTp++" idd cxs) = Clip_List_"++listTp++" $ List_"++listTp++" idd (insertList_"++listTp++" n c cxs)"
  , "  insertList _ _             xs = trace \"Type error, no paste\" $ Clip_List_"++listTp++" xs"
  , "  insertList _ c xs                 = Clip_List_"++listTp++" xs"
  , ""
  , "  removeList n (List_"++listTp++" idd cxs) = Clip_List_"++listTp++" $ List_"++listTp++" idd (removeList_"++listTp++" n cxs)"
  , "  removeList _ xs                        = Clip_List_"++listTp++" $ xs"
  , ""
  ]
 where listTp = drop 5 e
 
 
 
 
 
 
 
 
 
 
 
 
       
       
--- below are some generated functions for a new module "../src/presentation/ProxParser_Generated"
--- this module should be similar to the other generated modules (with a "DON'T EDIT.." line)
--- it was added to a very old generator version


genProxParser (File _ decls) = genDocUtils' decls

allConstructors ds = [ (tp, cnstr, map fieldType cs, decltp) | Decl tp prods decltp <- ds, decltp /= DeclConsList, Prod cnstr cs <-prods]
       
genDocUtils' ds = ["-- Type specific"] ++
                  map genReuse    (allConstructors ds) ++
                  map genExtract  (allConstructors ds) ++
                  map genDefault  (allConstructors ds) ++
                 ["-- General"] ++
                 genExtractFromNodes ++
                 map genReuseN arities
 where arities = nub [ length cs | Decl tp prods _ <- ds, Prod _ cs <- prods ]


------- 

{-

reuseRootEnr :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe Decls -> Maybe Decls -> Maybe HeliumTypeInfo -> Maybe Document  -> EnrichedDoc
reuseRootEnr nodes ma0 ma1 ma2 ma3 ma4 ma5
  = case extractFromNodes extractRootEnr defaultRootEnr nodes of
           (RootEnr a0 a1 a2 a3 a4 a5) -> reuse6 RootEnr a0 a1 a2 a3 a4 a5 ma0 ma1 ma2 ma3 ma4 ma5
           _ -> error "System error:<module>.reuseRootEnr"

-}

genReuse (tp,cnstr,cs,_) =
  "reuse"++cnstr++" :: [Maybe Node] ->"++concatMap tpsig cs++" "++tp++"\n"++
  "reuse"++cnstr++" nodes "++mas++"\n" ++ --(Just ("++tp++"Node x@("++cnstr++(concat $ replicate arity " _")++") _)) = Just x\n"++
  "  = case extractFromNodes extract"++cnstr++" default"++cnstr++" nodes of\n"++
  "           ("++cnstr++as++") -> reuse"++show arity++" "++cnstr++as++mas++"\n"++
  "           _ -> error \"System error:<module>.reuse"++cnstr++"\"\n"
 where arity = length cs
       tpsig c = " Maybe "++c++" ->"
       mas = concat [" ma"++show i | i <- [0..arity-1]]
       as = concat [" a"++show i | i <- [0..arity-1]]

genExtract (tp,cnstr,cs,_) =
  "extract"++cnstr++" :: Maybe Node -> Maybe "++tp++"\n"++
  "extract"++cnstr++" (Just ("++tp++"Node x@("++cnstr++(concat $ replicate arity " _")++") _)) = Just x\n"++
  "extract"++cnstr++" _ = Nothing\n"
 where arity = length cs


genDefault (tp,cnstr,cs,DeclList) =
  "default"++cnstr++" :: "++tp++"\n"++
  "default"++cnstr++" = "++cnstr ++ " NoIDD Nil_" ++ drop 5 tp ++"\n"
genDefault (tp,cnstr,cs,_) =
  "default"++cnstr++" :: "++tp++"\n"++
  "default"++cnstr++" = "++cnstr ++ (concat $ map def cs) ++"\n"
 where def id@('I':'D':_) = " No"++id    --
       def lst@('[':_)    = " []"        -- Should use an abstract data type to distinguish between
       def c              = " hole"      -- id's, lists, and types

genExtractFromNodes = -- generate this one as well, because we don't want DocUtils to import yet another module
  [ "-- return result of the first extraction application in the list that is not Nothing\n"++
    "--extractFromNodes ::(Node -> Maybe a) -> a -> [Node] -> a\n"++
    "extractFromNodes extr def []     = def\n"++
    "extractFromNodes extr def (n:ns) = maybe (extractFromNodes extr def ns) id (extr n)\n" ]

{-
reuse6 :: (a0->a1->a2->a3->a4->a5->r) ->
          a0 -> a1 -> a2 -> a3 -> a4 -> a5->
          Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> r
reuse6 f a0 a1 a2 a3 a4 a5 ma0 ma1 ma2 ma3 ma4 ma5 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) (maybe a5 id ma5)

-}
genReuseN 0     = "reuse0 :: r -> r\nreuse0 f = f\n" 
genReuseN arity = 
  "reuse"++show arity ++" :: ("++tas++"r) -> \n"++
  "     "++spaces     ++"    "++tas++"\n"++
  "     "++spaces     ++"    "++tmaybeas++"r\n"++
  "reuse"++show arity ++" f "++as++mas++" =\n"++
  "  f "++ maybes ++"\n"
 where spaces = take (length (show arity)) (repeat ' ')
       tas = concat ["a"++show i++" -> " | i <- [0..arity-1]]
       tmaybeas = concat ["Maybe a"++show i++" -> " | i <- [0..arity-1]]
       mas = concat [" ma"++show i | i <- [0..arity-1]]
       as = concat [" a"++show i | i <- [0..arity-1]]
       maybes = concat ["(maybe a"++show i++" id ma"++show i++") " | i <- [0..arity-1]]

---------
       
       
       
       