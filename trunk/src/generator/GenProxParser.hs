module GenProxParser where

import GenCommon
import List

---------------------------------------------------------------------
--       P R O X    P A R S E R      G E N E R A T E D             --
---------------------------------------------------------------------

genProxParser :: [String] -> File -> [String]
genProxParser include parsedFile =  
                 include ++ 
                 [  defaultLimit 
                 ,  "\n{- ------------------------------------"
                 ,  "\n generated part"
                 ,  "\n-------------------------------------- -}"
                 ]
                 ++ ["\n\n\n\n\n\n-- ProxParser_Generated --\n"] ---
                 ++ genProxParser'        extendedTypes
                 where
                 extendedTypes = extendTypes parsedFile
                 extendTypes parsedFile@(File m d) = (File m (d++(genListTypes parsedFile)))

      
       
--- below are some generated functions for a new module "../src/presentation/ProxParser_Generated"
--- this module should be similar to the other generated modules (with a "DON'T EDIT.." line)
--- it was added to a very old generator version


genProxParser' (File _ decls) = genDocUtils' decls

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
       
       
       
--                 primTypes = ["Int", "Bool", "String"]  --- This doesn't belong here!
{-
                 ++ ["\n\n-- Generated clipfunctions  --\n"]
                 ++ clipfunctions (listFields extendedTypes)
                 ++ ["\n\n-- Editable Instances --\n"]
                 ++ genEditableInstances extendedTypes
                 ++ concatMap primitiveEdit primTypes
-}
{-
reuse6 :: (a0->a1->a2->a3->a4->a5->r) ->
          a0 -> a1 -> a2 -> a3 -> a4 -> a5->
          Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> r
reuse6 f a0 a1 a2 a3 a4 a5 ma0 ma1 ma2 ma3 ma4 ma5 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) (maybe a5 id ma5)

-}

       
{-

reuseRootEnr :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe Decls -> Maybe Decls -> Maybe HeliumTypeInfo -> Maybe Document  -> EnrichedDoc
reuseRootEnr nodes ma0 ma1 ma2 ma3 ma4 ma5
  = case extractFromNodes extractRootEnr defaultRootEnr nodes of
           (RootEnr a0 a1 a2 a3 a4 a5) -> reuse6 RootEnr a0 a1 a2 a3 a4 a5 ma0 ma1 ma2 ma3 ma4 ma5
           _ -> error "System error:<module>.reuseRootEnr"

-}
