module GenTypes where

import GenCommon
import List
---------------------------------------------------------------------
--       D O C U M E N T   T Y P E S                             --
---------------------------------------------------------------------

genDocumentTypes :: [String] -> File -> [String]
genDocumentTypes include parsedFile =  
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
                 ++ genNode      extendedTypes 
                 where
                 extendedTypes = extendTypes parsedFile
                 extendTypes parsedFile@(File m d) = (File m (d++(genListTypes parsedFile)))


--- no node for conslist, is unsafe string compare hack now, should be done nicely!!!
--- it also produces an empty line
genNode parsedFile    = [    "data Node = NoNode "]
                        ++ [ "          | EnrichedDocNode EnrichedDoc Path" ] --- does not appear as field, but should be in Node
                        ++ indent 10 (map makeNodeAlt fields)  where
                        fields = removeRepeat(getFields' parsedFile)
                        makeNodeAlt (Field _ ('C':'o':'n':'s':'L':'i':'s':'t':'_':_) _) = ""
                        makeNodeAlt e = "| "++fieldType e ++"Node "++fieldType e++" Path "
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
                                 ++ (if decltp /= DeclConsList then indent (6+(length e)) ["| ParseErr"++e++" Node Presentation"] else [])
                                 ++ indent (8+(length e)) [" deriving Show"]                               

---
printProd :: Prod -> String
printProd (Prod s fields)= "| "++s++" "++ concatMap printField fields 
 where printField (Field _ _ Ids) = "[IDP] "
       printField f = fieldType f++" " ---
       
       
       