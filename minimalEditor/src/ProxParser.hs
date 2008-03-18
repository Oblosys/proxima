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
recognizeEnrichedDoc =  
          (\str -> let clip = recognize str
                       in  case fromClip clip of 
                              Just enr -> enr
                              Nothing   -> error $ "Error"++show clip )
      <$> pStructural Node_RootEnr
     

recognizeList_Tree :: ListParser Document Node ClipDoc UserToken List_Tree
recognizeList_Tree = pStr $
          (\str trees -> reuseList_Tree [str] (Just $ toConsList_Tree trees))
      <$> pStructural Node_List_Tree
      <*> pList recognizeTree

recognizeTree :: ListParser Document Node ClipDoc UserToken Tree
recognizeTree = pStr $
          (\str left right -> reuseBin [str] (Just left) (Just right))
      <$> pStructural Node_Bin
      <*> recognizeTree
      <*> recognizeTree
--      <*  recognizeTree
  <|>     (\str -> reuseLeaf [str] Nothing)
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
  <|>     (\str -> reuseLeaf [str] Nothing)
      <$> pToken LeafToken


leafParser = mkClipParser $
          (\str t -> reuseLeaf [str] (Just $ read $ tokenString t))
      <$> pToken LeafToken
      <*> pToken IntToken

mkClipParser p = 
 \tokens ->
         let (res, errs) = runParser p tokens
         in  toClip $ if null errs then res else debug Err ("ERROR: Parse error"++(show errs)) $ parseErr (ParsingParseErr (mkErr errs) tokens)

{-
TODO
handle parsing add not to parsingTk and add parser too (parser to clip?)
typeArg: more info

what about parseerr hole for constr? will they occur?
what about String etc.?
duplicates!
-}

{- recognize groups the children according to their paths. 
-}
recognize :: (Clip clip, Construct doc node clip token, DocNode node, Show token, Ord token) =>
             Token doc node clip token -> clip
recognize strTk@(StructuralTk _ (Just node) _ childTokens _) = 
  if isListClip (construct node strTk []) 
  then 
  let thisPath = case pathNode node of
                   PathD path -> path
                   NoPathD    -> error $ "recognize: Encountered StructuralTk that has node without path:" ++ show strTk
      eltTokens = map (snd . tokenPath thisPath) childTokens -- we do the checks, but discard the numbers
      eltClips = map (Just. recognize) eltTokens
      result = construct node strTk eltClips -- for lists construct does not call reuse
  in  result
  else    
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
recognize tk@(StructuralTk _ Nothing _ childTokens _) =
  error $ "recognize: Encountered StructuralTk without node: " ++ show tk
recognize tk@(ParsingTk (Just parser) _ _ childTokens _) = 
  parser childTokens
recognize tk@(ParsingTk Nothing _ _ childTokens _) = 
  error $ "recognize: Encountered ParsingTk without parser: " ++ show tk


tokenPath :: (Construct doc node clip token, DocNode node, Show token) => Path -> Token doc node clip token -> (Int, Token doc node clip token)
tokenPath parentPath tk =
  let node = case tk of 
               (StructuralTk _ (Just node) _ _ _) -> node
               (StructuralTk _ Nothing _ _ _)     -> error $ "tokenPath: childToken without node" ++ show tk
               (ParsingTk _ (Just node) _ _ _) -> node
               (ParsingTk _ Nothing _ _ _)     -> error $ "tokenPath: childToken without node" ++ show tk
  in case pathNode node of
       PathD path -> if parentPath `isPrefixOf` path 
                     then case drop (length parentPath) path of
                            [childNr] -> (childNr, tk)
                            _ -> error $ "encountered token that is not a child: tokenPath=" ++show path ++ 
                                         " parentPath=" ++ show parentPath ++ " token=" ++ show tk
                     else error $ "encountered token that is not a child: tokenPath=" ++show path ++ 
                                  " parentPath=" ++ show parentPath ++ " token=" ++ show tk
       NoPathD    -> error $ "tokenPath: childToken has node without path: " ++ show tk

addChildTokens childTokenGroups []  = childTokenGroups
addChildTokens childTokenGroups (childToken: childTokens) =
  addChildTokens (addChildToken childTokenGroups childToken) childTokens

addChildToken childTokenGroups (nr, childToken) =
  case splitAt nr childTokenGroups of
    (left, group:right) -> left ++ (group ++ [childToken]) : right
    _                   -> error $ "addChildToken: encountered child with number larger than parent's arity: nr="++show nr ++ " token="++show childToken 

      
-- if the argument is nothing, return nothing (reuse), otherwise apply fromClip to the clip
retrieveArg :: Clipable a => Maybe ClipDoc -> Maybe a
retrieveArg (Just clip) = case fromClip clip of
                  Just x  -> Just x
                  Nothing -> error "retrieveArg: Type error"
retrieveArg Nothing     = Nothing

-- lazy pattern is necessary because recognize uses arityClip on the result
construct_EnrichedDoc_RootEnr :: Token Document Node ClipDoc UserToken -> [ Maybe ClipDoc ] -> ClipDoc
construct_EnrichedDoc_RootEnr tk ~[mclip1, mclip2] = Clip_EnrichedDoc $ reuseRootEnr [tk] (retrieveArg mclip1)  (retrieveArg mclip2)

construct_Tree_Bin :: Token Document Node ClipDoc UserToken -> [Maybe ClipDoc ] -> ClipDoc
construct_Tree_Bin  tk ~(clip1: clip2:_) = Clip_Tree $ reuseBin [tk] (retrieveArg clip1) (retrieveArg clip2)
construct_Tree_Leaf tk ~[clip1] = Clip_Tree $ reuseLeaf [tk] (retrieveArg clip1)
  
construct_List_Tree :: Token Document Node ClipDoc UserToken -> [Maybe ClipDoc ] -> ClipDoc
construct_List_Tree tk clips = 
  Clip_List_Tree $ toList_Tree 
                                 [ x | clip <- clips, let Just x = retrieveArg clip]



class Construct doc node clip token where
  construct :: node -> (Token doc node clip token) -> [Maybe clip] -> clip

instance Construct Document Node ClipDoc UserToken where
  construct (Node_Bin _ _) = construct_Tree_Bin 
  construct (Node_Leaf _ _) = construct_Tree_Leaf
  construct (Node_RootEnr _ _) = construct_EnrichedDoc_RootEnr
  construct (Node_List_Tree _ _) = construct_List_Tree
  construct x = error $ "Construct"++show x

class Clipable a where 
  toClip :: a -> ClipDoc
  fromClip :: ClipDoc -> Maybe a
  
instance Clipable Tree where
  toClip t = Clip_Tree t
  fromClip (Clip_Tree t) = Just t
  fromClip _             = Nothing

instance Clipable Int where
  toClip t = Clip_Int t
  fromClip (Clip_Int t) = Just t
  fromClip _             = Nothing


instance Clipable List_Tree where
  toClip t = Clip_List_Tree t
  fromClip (Clip_List_Tree t) = Just t
  fromClip _             = Nothing
  
instance Clipable EnrichedDoc where
  toClip t = Clip_EnrichedDoc t
  fromClip (Clip_EnrichedDoc t) = Just t
  fromClip _             = Nothing
