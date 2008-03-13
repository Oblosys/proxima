module ProxParser (recognizeRootEnr) where

import Common.CommonTypes hiding (Dirty (..))
import qualified Common.CommonTypes
import Presentation.PresLayerTypes
import Presentation.PresLayerUtils hiding ((<*),(<*>),(<$),(<$>))
import Presentation.PresentationParsing
import Presentation.XprezLib

import Common.UU_Parsing hiding (Exp, parse)

import List hiding (delete)


import ProxParser_Generated
import Evaluation.DocumentEdit
import DocUtils_Generated

import qualified Common.UU_Parsing as UU_Parsing
import Char

import DocTypes_Generated
import DocUtils_Generated
              
-------------------- Proxima Parser/Structure Recognizer -------------------- 

recognizeRootEnr :: ListParser Document Node ClipDoc UserToken EnrichedDoc
recognizeRootEnr = pStr $ 
          (\str root-> reuseRootEnr [str] (Just root))
      <$> pStructural Node_RootEnr
      <*> recognizeTree

recognizeTree :: ListParser Document Node ClipDoc UserToken Tree
recognizeTree = pStr $
          (\str left right -> reuseBin [str] (Just left) (Just right))
      <$> pStructural Node_Bin
      <*> recognizeTree
      <*> recognizeTree
  <|>     (\str -> reuseLeaf [str])
      <$> pStructural Node_Leaf

parseTree :: ListParser Document Node ClipDoc UserToken Tree
parseTree = addHoleParser $
          (\token left right -> reuseBin [token] (Just left) (Just right))
      <$> pToken BinToken
      <*  pToken (SymToken "(")
      <*> parseTree
      <*  pToken (SymToken ")")      
      <*  pToken (SymToken "(")
      <*> parseTree
      <*  pToken (SymToken ")")      
  <|>     (\str -> reuseLeaf [str])
      <$> pToken LeafToken

