module ProxParser (parsePres) where

import CommonTypes
import PresLayerTypes
import PresLayerUtils
import PresentationParsing
import XprezLib

import UU_Parsing hiding (Exp, parse)

import List hiding (delete)


import ProxParser_Generated
import DocumentEdit
import DocumentEdit_Generated

import qualified UU_Parsing
import Char

import DocTypes_Generated
import DocUtils_Generated

-- TODO: move to PresentationParsing
reuse = Nothing
set = Just

parsePres pres = let tokens = postScanStr keywords Nothing pres
                     (enr,errs) = runParser recognizeRootEnr tokens
                 in -- showDebug' Err ("Parsing:\n"++concatMap (deepShowTks 0) (tokens)++"with errs"{-++show errs-}++"\nhas result:") $
                     (if null errs then Just enr else Nothing)
       
deepShowTks i tok = case tok of
                      (StructuralTk _ _ cs _) -> indent i ++ show tok ++ "\n"
                                               ++ indent (i+1)++"[\n"
                                               ++ concatMap (deepShowTks (i+1)) cs 
                                               ++ indent (i+1)++" ]\n"
                      (ParsingTk _ cs _) -> indent i ++ show tok ++ "\n"
                                               ++ indent (i+1)++"[\n"
                                               ++ concatMap (deepShowTks (i+1)) cs 
                                               ++ indent (i+1)++" ]\n"
                      _                     -> indent i ++ show tok ++ "\n" 
 where indent i = take i (repeat ' ')
       





-------------------- Proxima Parser/Structure Recognizer -------------------- 

recognizeRootEnr :: ListParser Document Node ClipDoc EnrichedDoc
recognizeRootEnr = pStr $ 
          (\str decls-> reuseRootEnr [tokenNode str] Nothing (Just decls) Nothing)
      <$> pSym (StructuralTk (Just $ RootEnrNode HoleEnrichedDoc []) empty [] NoIDP) -- EnrichedDoc is not instance of Editable
      <*> recognizeRoot
  <|>    RootEnr NoIDD (error "doc hole was parsed") (error "doc hole was parsed")
     <$ pStructural HoleEnrichedDocNode
-- TODO: Why do we need this hole parser here?


recognizeRoot :: ListParser Document Node ClipDoc Root
recognizeRoot = pStr $
          (\str tree -> reuseRoot [tokenNode str] Nothing (Just tree) Nothing)
      <$> pStructural RootNode
      <*> pPrs parseTree {- recognizeTree -}

parseTree :: ListParser Document Node ClipDoc Tree
parseTree = 
          (\po t1 b t2 pc -> reuseBin [tokenNode po, tokenNode b, tokenNode pc] Nothing (Just t1) (Just t2))
      <$> pKey "("
      <*> parseTree
      <*> pKey "Bin"
      <*> parseTree
      <*> pKey ")"
  <|>     (\l -> reuseLeaf [tokenNode l] Nothing)
      <$> pKey "Leaf"

recognizeTree :: ListParser Document Node ClipDoc Tree
recognizeTree = pStr $
          (\str t1 t2 -> reuseBin [tokenNode str] Nothing (Just t1) (Just t2))
      <$> pStructural BinNode
      <*> recognizeTree
      <*> recognizeTree
  <|>     (\str -> reuseLeaf [tokenNode str] Nothing)
      <$> pStructural LeafNode



keywords :: [String]
keywords = 
  [ ":-"
  , ":+"
  , "_|_"
  , "#"
  , "<"
  , ">"
  , "L"
  , "R"
  , "<-"
  , "->"
  , "<+"
  , "+>"
  , "\""
  , "</"
  , "/>"
  , "," --
  , "(" --
  , ")" --
  , "{" --
  , "}" --
  , ";" --
  , "[" --
  , "]" --
  , "="
  , "%"
  , "+"
  , "-"
  , "*"
  , "/"
  , "^"
  , "\174"
  , "\\" 
--  , "l"      -- not very nice, just for demonstrating lambdas
  , "False"
  , "True"
  , "if"
  , "then"
  , "else"
  , "let"
  , "in"
  , "case"
  , "of"
  , "Chess"
  , "board"
  , "Slides"
  , "pres"
  , "Inv"
  , "inv"
  , ":"
  , "..."
  , "Form"
  , "what"
  , "Leaf"
  , "Bin"
  ]





-- don't even have to use reuse now, since the IDD is never used. String_ NoIDD would be sufficient
mkString_ :: Show node => Token doc node clip (Maybe node) -> String_
mkString_ = (\strTk -> reuseString_ [] Nothing (Just $ strValTk strTk)) 

mkInt_ :: Show node => Token doc node clip (Maybe node) -> Int_
mkInt_ = (\intTk -> reuseInt_ [] Nothing (Just $ intVal intTk)) 

-- Extracting the value from the token is not necessary, since true and false have different
-- parsers, which can give the value as an argument
mkBool_ :: Bool -> Bool_
mkBool_ = (\bool -> reuseBool_ [] Nothing (Just bool)) 


