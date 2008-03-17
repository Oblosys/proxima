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

{-
TODO
handle wrong order
handle missing children (reuse)
handle parsing add not to parsingTk and add parser too (parser to clip?)
-}

{- recognize groups the children according to their paths. 
-}
recognize :: (Clip clip, Construct doc node clip token, DocNode node, Show token) =>
             Token doc node clip token -> clip
recognize strTk@(StructuralTk _ (Just node) _ childTokens _) = 
  let thisPath = case pathNode node of
                   PathD path -> path
                   NoPathD    -> error $ "recognize: Encountered StructuralTk that has node without path:" ++ show strTk
      numberedChildTokens = map (tokenPath thisPath) childTokens
      constructorArity = arityClip result -- no problem, since the constructor can be evaluated lazily
      initChildTokenGroups = replicate constructorArity []
      childTokenGroups = addChildTokens initChildTokenGroups numberedChildTokens 
      clipGroups       = map (map recognize) childTokenGroups
      reuseArgs = [ case group of
                      (clip:_) -> Just clip -- cannot handle multiple occurrences yet. Now we just take
                                            -- the first, in the future, use dirty bits to take the updated one
                      []       -> Nothing
                  | group <- clipGroups
                  ]
      parsedChildren = map recognize childTokens
      result = construct node strTk reuseArgs
  in  debug Prs ("\nThis path"++ show thisPath ++"\nChildren (max "++show constructorArity++"):\n"++show numberedChildTokens++"\n" ++ show childTokenGroups) $
      result
recognize strTk@(StructuralTk _ Nothing _ childTokens _) =
  error $ "recognize: Encountered StructuralTk without node: " ++ show strTk

-- ParsingTk does not have a node! We have to add it.
tokenPath :: (Construct doc node clip token, DocNode node, Show token) => Path -> Token doc node clip token -> (Int, Token doc node clip token)
tokenPath parentPath strTk@(StructuralTk _ (Just node) _ _ _) = 
  case pathNode node of
    PathD path -> if parentPath `isPrefixOf` path 
                  then case drop (length parentPath) path of
                         [childNr] -> (childNr, strTk)
                         _ -> error $ "encountered token that is not a child: tokenPath=" ++show path ++ 
                                      " parentPath=" ++ show parentPath ++ " token=" ++ show strTk
                  else error $ "encountered token that is not a child: tokenPath=" ++show path ++ 
                                      " parentPath=" ++ show parentPath ++ " token=" ++ show strTk
    NoPathD    -> error $ "tokenPath: childToken has node without path: " ++ show strTk
tokenPath parentPath strTk@(StructuralTk _ Nothing _ _ _) = 
  error $ "tokenPath: childToken without node" ++ show strTk

addChildTokens childTokenGroups []  = childTokenGroups
addChildTokens childTokenGroups (childToken: childTokens) =
  addChildTokens (addChildToken childTokenGroups childToken) childTokens

addChildToken childTokenGroups (nr, childToken) =
  case splitAt nr childTokenGroups of
    (left, group:right) -> left ++ (group ++ [childToken]) : right
    _                   -> error $ "addChildToken: encountered child with number larger than parents arity: nr="++show nr ++ " token="++show childToken 

      
-- if the argument is nothing, return nothing (reuse), otherwise apply fromClip to the clip
retrieveArg (Just clip) = case fromClip clip of
                  Just x  -> Just x
                  Nothing -> error "retrieveArg: Type error"
retrieveArg Nothing     = Nothing

construct_EnrichedDoc_RootEnr :: Token Document Node ClipDoc UserToken -> [ Maybe ClipDoc ] -> ClipDoc
construct_EnrichedDoc_RootEnr tk ~[mclip1] = Clip_EnrichedDoc $ reuseRootEnr [tk] (retrieveArg mclip1)

construct_Tree_Bin :: Token Document Node ClipDoc UserToken -> [Maybe ClipDoc ] -> ClipDoc
construct_Tree_Bin  tk ~(clip1: clip2:_) = Clip_Tree $ reuseBin [tk] (retrieveArg clip1) (retrieveArg clip2)
construct_Tree_Leaf tk ~[] = Clip_Tree $ reuseLeaf [tk] 
  



class Construct doc node clip token where
  construct :: node -> (Token doc node clip token) -> [Maybe clip] -> clip

instance Construct Document Node ClipDoc UserToken where
  construct (Node_Bin _ _) = construct_Tree_Bin 
  construct (Node_Leaf _ _) = construct_Tree_Leaf
  construct (Node_RootEnr _ _) = construct_EnrichedDoc_RootEnr
  construct x = error $ show x

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