module ProxParser (recognizeEnrichedDoc) where

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
--      <*> pPrs parseTree

recognizeEnrichedDoc :: ListParser Document Node ClipDoc UserToken EnrichedDoc
recognizeEnrichedDoc =  
          (\str -> let clip = recognize str
                       in  case fromClip clip of 
                              Just enr -> enr
                              Nothing   -> error $ "Error"++show clip )
      <$> pStructural Node_RootEnr
      
recognizeTree :: ListParser Document Node ClipDoc UserToken Tree
recognizeTree = pStr $
          (\str left right -> reuseBin [str] (Just left) (Just right))
      <$> pStructural Node_Bin
      <*> recognizeTree
      <*> recognizeTree
      <*  recognizeTree
  <|>     (\str -> reuseLeaf [str])
      <$> pStructural Node_Leaf

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
  <|>     (\str -> reuseLeaf [str])
      <$> pToken LeafToken

recognize :: Token Document Node ClipDoc UserToken -> ClipDoc
recognize (StructuralTk _ (Just node) _ childTokens _) = 
  let thisPath = pathNode node
      parsedChildren = map recognize childTokens
      result = construct node parsedChildren
  in  debug Prs ("\nThis path"++ show thisPath ++"\nChildren: " ++ show (map tokenPath childTokens)) $
      result


tokenPath (StructuralTk _ (Just node) _ _ _) = pathNode node
-- ParsingTk does not have a node! We have to add it.

construct (Node_Bin _ _) = construct_Tree_Bin 
construct (Node_Leaf _ _) = construct_Tree_Leaf
construct (Node_RootEnr _ _) = construct_EnrichedDoc_RootEnr
construct x = error $ show x

construct_EnrichedDoc_RootEnr :: [ ClipDoc ] -> ClipDoc
construct_EnrichedDoc_RootEnr [clip1] = let Just c1 = fromClip clip1
                                        in  Clip_EnrichedDoc $ RootEnr c1

construct_Tree_Bin :: [ ClipDoc ] -> ClipDoc
construct_Tree_Bin (clip1: clip2:_) = let Just c1 = fromClip clip1
                                          Just c2 = fromClip clip2
                                    in  Clip_Tree $ Bin c1 c2
construct_Tree_Leaf [] = Clip_Tree Leaf
  


class Clipable a where 
  toClip :: a -> ClipDoc
  fromClip :: ClipDoc -> Maybe a
  
instance Clipable Tree where
  toClip t = Clip_Tree t
  fromClip (Clip_Tree t) = Just t
  fromClip _             = Nothing
instance Clipable EnrichedDoc where
  toClip t = Clip_EnrichedDoc t
  fromClip (Clip_EnrichedDoc t) = Just t
  fromClip _             = Nothing