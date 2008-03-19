module ProxParser where

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

{-
recognizeEnrichedDoc :: ListParser Document Node ClipDoc UserToken EnrichedDoc
recognizeEnrichedDoc = pStr $ 
          (\str trees trees2-> reuseRootEnr [str] (Just trees) (Just trees2))
      <$> pStructural Node_RootEnr
      <*> recognizeList_Tree
      <*> recognizeList_Tree
--      <*> pPrs parseTree
-}

recognizeEnrichedDoc :: ListParser Document Node ClipDoc UserToken EnrichedDoc
recognizeEnrichedDoc = pStructural


leafParser =
          (\str t -> reuseLeaf [str] (Just $ read $ tokenString t))
      <$> pToken LeafToken
      <*> pToken IntToken

     

recognizeList_Tree :: ListParser Document Node ClipDoc UserToken List_Tree
recognizeList_Tree = pStr $
          (\str trees -> reuseList_Tree [str] (Just $ toConsList_Tree trees))
      <$> pStructuralTk Node_List_Tree
      <*> pList recognizeTree

recognizeTree :: ListParser Document Node ClipDoc UserToken Tree
recognizeTree = pStr $
          (\str left right -> reuseBin [str] (Just left) (Just right))
      <$> pStructuralTk Node_Bin
      <*> recognizeTree
      <*> recognizeTree
--      <*  recognizeTree
  <|>     (\str -> reuseLeaf [str] Nothing)
      <$> pStructuralTk Node_Leaf

parseTree :: ListParser Document Node ClipDoc UserToken Tree
parseTree = 
          (\token left right -> reuseBin [token] (Just left) (Just right))
      <$> pToken BinToken
      <*  pToken (SymToken "(")
      <*> parseTree
      <*  pToken (SymToken ")")      
      <*  pToken (SymToken "(")
      <*> parseTree
      <*  pToken (SymToken ")")      
  <|>     (\str -> reuseLeaf [str] Nothing)
      <$> pToken LeafToken

