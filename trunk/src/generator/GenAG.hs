module GenAG where

import GenCommon

--- This file needs major restructuring, also the output

-------------------------------------------------------------------
--           G E N E R A T E    A G    F I L E S                 --
-------------------------------------------------------------------

genPresentationAG :: [String] -> File -> [String]
genPresentationAG include parsedFile =  
                 include ++ 
                 [  defaultLimit
                 ,  "\n{- ------------------------------------"
                 ,  "\n generated part"
                 ,  "\n------------------------------------- -}"
                 ]                                                  --- always the same, use abstraction!
                 ++ genAgTypes extendedTypes
                 ++ genSem     extendedTypes
                 ++ genAtt     extendedTypes
                 where
                 extendedTypes = extendTypes parsedFile
                 extendTypes parsedFile@(File m d) = (File m (d++(genListTypes parsedFile)))



-------------------------------------------------------------------
--        D A T A    T Y P E     D E C L A R A T I O N           --
-------------------------------------------------------------------

---
genAgTypes (File _ decls) = concatMap printAgDecl decls where
    printAgDecl (Decl e (p:prods) decltp) = ["\n\nDATA "++e++" |" ++ (tail(printAgProd  p)) ]  
                                     ++ indent (6+(length e)) ((map printAgProd prods)
                                     ++ (if decltp /= DeclConsList --- no hole/parseErrs for consLists
                                         then ["| Hole"++e
                                              ,"| ParseErr"++e++" Node Presentation"] 
                                         else [])
                                     )
    printAgProd (Prod s fields)= "| "++s++" "++ concatMap printField fields 

printField (Field n s Ids) = (n++":{["++(init s)++"]} ") 
printField (Field n s List) = (n++":"++"List_"++init s++" ") ---
printField (Field n s _) = (n++":"++s++" ")

-------------------------------------------------------------------
--        S E M A N T I C S                                      --
-------------------------------------------------------------------



genSem (File _ decls) = concatMap genSem' decls 

genSem' d@(Decl "EnrichedDoc" l _) = []     --- No SEM should be created for EnrichedDoc

genSem' d@(Decl e l DeclConsList) = semConsList e 
genSem' d@(Decl e l DeclList) = semList e 
genSem' d@(Decl e l _)   = [("\n\nSEM "++e)] ++ indent 2 (concatMap doIdC  l) ++ --- SEM
                           [ "  | Hole"++e++"     lhs.pres = presHole @lhs.focusD \""++e++"\" (Hole"++e++"Node @self @lhs.path) @lhs.path"
                            ,"  | ParseErr"++e++" lhs.pres = presParseErr @node @presentation"]++
                         --- quick dirty fix so no SEM is generated if no path rules are generated
                         let pathRules = concatMap doPath l  ---
                         in (if null pathRules then [] else [("\n\nSEM "++e)])    ---
                         ++ indent 2 (pathRules) ---  SEM
                      ---   ++ genIx d

--- !!!!!!!
doIdC  (Prod e ls) = let l =  takeFieldsNamesNoPrim ls
                  in  if null l then []
                      else lines ("| "++e++" "++(head l)++".pIdC = @lhs.pIdC" ++ " + " 
                           ++ (show ((length (filter (isID) ls))-1))) --- !!! fails if there is no IDD
                           ++ indent (3+(length e)) (linkAttrIdC l)

-- linkAttrIdC ["a","b","c"] = ["c.idC = @b.idC","b.idC = @a.idC","lhs.idC = @c.idC"]   
linkAttrIdC :: [String] -> [String]
linkAttrIdC l    = aux (l, (length l)-1)
aux (l,0) = lines $ "lhs.pIdC = @"++ (last l)++ ".pIdC"
aux (l,n) = lines (l!!n++".pIdC = @" ++ l!!(n-1)++ ".pIdC") ++ (aux (l,n-1))

doPath (Prod e ls) = let l =  takeFieldsNamesNoPrim' (numerateFields (filter (not.isID) ls))
                  in  if null l then []  
                      else ("| "++e++" "++(head(map path l))) :(indent (3+(length e)) (tail (map path l))) --- added the constructor
                      
path (n,e)= e++".path  = @lhs.path++["++(show n)++"]"

takeFieldsNamesNoPrim l = map (\(Field s _ _)->s) (filter (notIDPrim) l)
takeFieldsNamesNoPrim' l = map (\(n,(Field s _ _))->(n,s)) (filter (notIDPrim.snd) l)

numerateFields = numerateFields' 0 where
numerateFields' _ []     = []
numerateFields' n (e:es) = (n,e):(numerateFields' (n+1) es)


takeFieldsListNames l = map (\(Field s _ List)->s) (filter (isList) l)
{-
--- gen? do?
genIx (Decl e prods _) = let l = concatMap (\(Prod _ f)->takeFieldsListNames f) prods
                    in if null l then []
                       else [("\n\nSEM "++e)] ++ indent 2 (concatMap doIx  prods)  --- SEM
doIx (Prod e ls) = let l =  takeFieldsListNames ls
                in  if null l then []
                       else lines ("| "++e++" "++ (head l)++".ix = 0")
                ++ indent (3+(length e)) (map (\a->a ++ ".ix = 0") (tail l))
-}

semList e =  [ "\nSEM "++e
             , "  | "++e
             , "      lhs.press = map ( loc ("++e++"Node @self @lhs.path) "
             , "                      . presentFocus @lhs.focusD @lhs.path ) "
             , "                      @elts.press"
             , "                      -- parent is reponsible for setting parsing/structural"
             , "      elts.pIdC = @lhs.pIdC + 100 -- NOT RIGHT, should be taken from document type def."
             , "      lhs.pIdC = @elts.pIdC"
             , "      elts.path = @lhs.path"
             , "      elts.ix = 0"
             , "  | Hole"++e++"     lhs.press = []"
             , "  | ParseErr"++e++" lhs.press = [ presParseErr @node @presentation ]" ------ ??? ok?
             ]
 where  listTp = drop (length "List_") e --- !!! need to access the type name here in an safe way


---
semConsList e =  [ "\nSEM "++e
          , "  | Cons_"++listTp++" "++"head.path  = @lhs.path++[@lhs.ix]"
          , concat(replicate (9+(length listTp)) " ") ++ "tail.path = @lhs.path"
          ,"                 lhs.press = @head.pres : @tail.press"
          ,"                 head.pIdC = @lhs.pIdC + 30 -- NOT RIGHT, should be taken from document type def."
          ,"                 tail.pIdC = @head.pIdC"
          ,"                 lhs.pIdC = @tail.pIdC"

          ,"  | Nil_"++listTp++"      lhs.press = []"                     
          ,"\n\nSEM "++e++" [ ix : Int | | ]" --- path : {[Int]} ]"
          ,"  | Cons_"++listTp++"     tail.ix  = @lhs.ix + 1" --- was always alts
---          ,"                 lhs.path = @lhs.path"
---          ,"  | Nil"++e++"      lhs.path = @lhs.path"
---          ,"  | Hole"++e++"     lhs.path = @lhs.path"
          ]
 where listTp = drop (length "ConsList_") e --- !!! need to access the type name here in an safe way

--- genHoles suggests that holes are generated. However, it is pres and press rules that are generated


genHoles e DeclConsList =
    []





-------------------------------------------------------------------
--        A T T R I B U T E   D E C L                            --
-------------------------------------------------------------------


--- path is not an attribute of EnrichedDoc, but pIDc is.
genAtt (File _ decls  ) = let all    = (map (\(Decl n _ _)-> n) decls) 
                              lists  = (map (\(Decl n _ _)-> n) (filter (\d -> isDeclList d || isDeclConsList d) decls)) 
                              noList = (map (\(Decl n _ _)-> n) (filter (\d -> not (isDeclList d || isDeclConsList d)) decls)) 
                          in   [---"\n\nATTR " ++ prListLine (filter (/="EnrichedDoc") all)
                              --- ,"       [ path : {[Int]} | | ]"  --- removed synthesize path attr
                                "\n\nATTR " ++ prListLine all
                               ,"       [ |  pIdC : Int  | ]"
                               ,"\n\nATTR " ++ prListLine all
                               ,"       [ focusD : FocusDoc | | ]"   
                               ,"\n\nATTR " ++ prListLine (filter (/="EnrichedDoc") all)
                               ,"       [ path : {[Int]}  | | ]"   
                               ,"\n\nATTR " ++ prListLine lists ++ " [ | | press : {[Presentation]} ]"
                               ,"\n\nATTR " ++ prListLine noList ++ " [ | | pres : Presentation ]"
                               ,"\n\nATTR " ++ prListLine (filter (/="EnrichedDoc") noList) ++ " [ ix : Int || ]"
                                      ---  ix should only be defined for list children
                               ]


isDeclList (Decl e _ DeclList) = True 
isDeclList _ = False

---
isDeclConsList (Decl e _ DeclConsList) = True 
isDeclConsList _ = False


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



{-
genHoles e DeclDef  = 
    ["  | Hole"++e++"     lhs.pres = presHole @lhs.focusD \""++e++"\" ("++e++"Node @self @lhs.path) @lhs.path"
    ,"  | ParseErr"++e++" lhs.pres = presParseErr @node @presentation"
    ]
genHoles e DeclList =
    [
    ]

SEM ConsList_Exp
  | Cons_Exp head.pIdC = @lhs.pIdC + 2
             tail.pIdC = @head.pIdC
             lhs.pIdC = @tail.pIdC


---          ,"                 lhs.path = @lhs.path"
---          ,"  | Nil"++e++"      lhs.path = @lhs.path"
---          ,"  | Hole"++e++"     lhs.path = @lhs.path"
--          ,"  | ParseErr"++e++" lhs.path = @lhs.path"
-}
