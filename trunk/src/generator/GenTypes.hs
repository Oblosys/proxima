module GenTypes where

import GenCommon
import List
---------------------------------------------------------------------
--       D O C U M E N T   T Y P E S                             --
---------------------------------------------------------------------

genDocumentTypes :: [String] -> File -> [String]
genDocumentTypes include parsedFile@(File m d) =  
                 include ++    
                 [  defaultLimit
                 ,  "\n{- ------------------------------------"
                 ,  "\n generated part"
                 ,  "\n-------------------------------------- -}"
                 ]
                 ++ ["\n\n-- Generated Types --\n"]
                 ++ genDataTypes extendedTypes 
                 ++ ["\n\n-- Generated Types --\n"]
                 ++ genClipDoc   extendedTypes 
                 ++ ["\n\n"]
                 ++ genNode      allConstructors
                 ++ ["\n\n"]
                 ++ genShowNode  allConstructors
                 
{-
                 ++ ["{- !!!!!!!!!!!!!"]
                 ++ (removeRepeat(getFields extendedTypes))
                 ++ ["-- !!!!!!!!!!!!! extended types"]
                 ++ [(show extendedTypes)]     
                 ++ ["!!!!!!!!!!!!! -}"]
-}
 where extendedTypes@(File _ d') = (File m (d++(genListTypes parsedFile)))
       allConstructors = [ (tp, cnstr, map fieldType cs, decltp) 
                         | Decl tp prods decltp <- d'
                         , decltp /= DeclConsList
                         , Prod cnstr cs <- prods ++[Prod ("Hole"++tp) []] ]  --- *** hack


genShowNode constrs = [ "instance Show Node where"
                      , "  show NoNode            = \"NoNode\""
                      , "  show (RootDocNode _ _) = \"RootDocNode\""  
                      , "  show (HoleDocumentNode _ _) = \"HoleDocumentNode\""]
                      ++ (map makeNodeAlt $ zip constrs [3..]) 
 where makeNodeAlt ((_, cnstr, _, _),i) = "  show ("++ cnstr ++"Node _ _)  = \""++cnstr ++"Node\""


--- generation of ParseErr and Hole should be done more abstractely, now it is hard to generate for them

--- no node for conslist, is unsafe string compare hack now, should be done nicely!!!
--- it also produces an empty line
genNode cnstrs = [    "data Node = NoNode "]
              ++ [ "          | RootDocNode Document Path" ] 
              ++ [ "          | HoleDocumentNode Document Path" ] --- does not appear as field, but should be in Node
              ++ indent 10 (map makeNodeAlt cnstrs)
 where makeNodeAlt (tp, cnstr, _, _) = "| "++ cnstr ++"Node "++ tp ++" Path "
---
       
--- no clip for conslist, is unsafe string compare hack now, should be done nicely!!!
--- a special ConsList label in fields is not a good idea. Instead of from getFields, these
--- types should come from the lefthand sides of the declarations
genClipDoc parsedFile = ("data ClipDoc = Clip_" ++ fieldType (head fields) ++" "++ fieldType (head fields)):  
                        indent 13 ((map makeClipAlt (tail fields)) 
                        ++ ["| Clip_Nothing deriving Show\n"]) where
                        fields = removeRepeat(getFields' parsedFile)
                        makeClipAlt (Field _ ('C':'o':'n':'s':'L':'i':'s':'t':'_':_) _) = ""
                        makeClipAlt e = "| Clip_" ++ fieldType e ++" "++ fieldType e


                        
genDataTypes :: File -> [String]
genDataTypes (File _ decls) = concatMap printDecl decls

printDecl (Decl e prods decltp) =["\n\ndata "++e++" =" ++ (tail(printProd  (head prods))) ]
                                 ++ indent (6+(length e)) (map printProd (tail prods))
                                 ++ (if decltp /= DeclConsList then indent (6+(length e)) ["| Hole"++e] else [])
                                 ++ (if decltp /= DeclConsList then indent (6+(length e)) ["| ParseErr"++e++" Node (Presentation Node)"] else [])
                                 ++ indent (8+(length e)) [" deriving Show"]                               

---
printProd :: Prod -> String
printProd (Prod s fields)= "| "++s++" "++ concatMap printField fields 
 where printField (Field _ _ Ids) = "[IDP] "
       printField f = fieldType f++" " ---
       
       
       