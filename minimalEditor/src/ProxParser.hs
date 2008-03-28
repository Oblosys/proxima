module ProxParser where

import Common.CommonTypes hiding (Dirty (..))
import qualified Common.CommonTypes
import Presentation.PresLayerTypes
import Presentation.PresLayerUtils
import Presentation.PresentationParsing
import Presentation.XprezLib

import List hiding (delete)


import ProxParser_Generated
import Evaluation.DocumentEdit
import DocUtils_Generated

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

intParser :: ListParser Document Node ClipDoc UserToken Int
intParser = 0 
      <$ pToken LeafToken
      <* pToken IntToken


pTree = pLeaf <|> pBin

pLeaf = pStructuralConstr Node_Leaf
{-
          (\str t -> reuseLeaf [str] (Just $ read $ tokenString t))
      <$> pToken LeafToken
      <*> pToken IntToken
-}
     
pBin :: ListParser Document Node ClipDoc UserToken Tree
pBin = 
          (\bin open1 left close1 open2 right close2 -> 
              reuseBin [bin] (Just $ getTokenIDP bin) (Just $ getTokenIDP open1) (Just $ getTokenIDP close1) (Just $ getTokenIDP open2) (Just $ getTokenIDP close2) (Just left) (Just right))
      <$> pToken BinToken
      <*> pToken (SymToken "(")
      <*> pTree
      <*> pToken (SymToken ")")      
      <*> pToken (SymToken "(")
      <*> pTree
      <*> pToken (SymToken ")")      


recognizeList_Tree :: ListParser Document Node ClipDoc UserToken List_Tree
recognizeList_Tree = pStr $
          (\str trees -> reuseList_Tree [str] (Just $ toConsList_Tree trees))
      <$> pStructuralTk Node_List_Tree
      <*> pList recognizeTree


recognizeTree :: ListParser Document Node ClipDoc UserToken Tree
recognizeTree = pStr $
          (\str left right -> reuseBin [str] Nothing Nothing Nothing Nothing Nothing (Just left) (Just right))
      <$> pStructuralTk Node_Bin
      <*> recognizeTree
      <*> recognizeTree
--      <*  recognizeTree
  <|>     (\str -> reuseLeaf [str] Nothing Nothing Nothing)
      <$> pStructuralTk Node_Leaf
