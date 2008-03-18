module ProxParser_Generated where

import Common.CommonTypes hiding (Dirty (..))
import Presentation.PresLayerTypes
import Presentation.PresLayerUtils

import Evaluation.DocumentEdit
import DocumentEdit_Generated
import DocUtils_Generated
import Evaluation.DocTypes
import DocTypes_Generated
import Presentation.PresentationParsing

-- if the argument is nothing, return nothing (reuse), otherwise apply fromClip to the clip
retrieveArg :: Editable a doc node clip token => Maybe clip -> Maybe a
retrieveArg (Just clip) = case fromClip clip of
                  Just x  -> Just x
                  Nothing -> error "retrieveArg: Type error"
retrieveArg Nothing     = Nothing

instance Construct Document Node ClipDoc UserToken where
  construct (Node_Bin _ _) = construct_Tree_Bin 
  construct (Node_Leaf _ _) = construct_Tree_Leaf
  construct (Node_HoleTree _ _) = construct_Tree_HoleTree
  construct (Node_RootEnr _ _) = construct_EnrichedDoc_RootEnr
  construct (Node_List_Tree _ _) = construct_List_Tree
  construct x = error $ "Construct"++show x


-- lazy pattern is necessary because recognize uses arityClip on the result
construct_EnrichedDoc_RootEnr :: Token Document Node ClipDoc UserToken -> [ Maybe ClipDoc ] -> ClipDoc
construct_EnrichedDoc_RootEnr tk ~[mclip1, mclip2] = Clip_EnrichedDoc $ reuseRootEnr [tk] (retrieveArg mclip1)  (retrieveArg mclip2)

construct_Tree_Bin :: Token Document Node ClipDoc UserToken -> [Maybe ClipDoc ] -> ClipDoc
construct_Tree_Bin  tk ~(clip1: clip2:_) = Clip_Tree $ reuseBin [tk] (retrieveArg clip1) (retrieveArg clip2)
construct_Tree_Leaf tk ~[clip1] = Clip_Tree $ reuseLeaf [tk] (retrieveArg clip1)
construct_Tree_HoleTree tk ~[] = Clip_Tree $ hole
  
construct_List_Tree :: Token Document Node ClipDoc UserToken -> [Maybe ClipDoc ] -> ClipDoc
construct_List_Tree tk clips = 
  Clip_List_Tree $ toList_Tree 
                                 [ x | clip <- clips, let Just x = retrieveArg clip]



----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- reuse functions                                                      --
--------------------------------------------------------------------------

reuseRootEnr :: [Token doc Node clip token] -> Maybe List_Tree -> Maybe List_Tree -> EnrichedDoc
reuseRootEnr nodes ma0 ma1
  = case extractFromTokens extractRootEnr defaultRootEnr nodes of
           (RootEnr a0 a1) -> genericReuse2 RootEnr a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseRootEnr"

reuseRootDoc :: [Token doc Node clip token] -> Maybe List_Tree -> Maybe List_Tree -> Document
reuseRootDoc nodes ma0 ma1
  = case extractFromTokens extractRootDoc defaultRootDoc nodes of
           (RootDoc a0 a1) -> genericReuse2 RootDoc a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseRootDoc"

reuseBin :: [Token doc Node clip token] -> Maybe Tree -> Maybe Tree -> Tree
reuseBin nodes ma0 ma1
  = case extractFromTokens extractBin defaultBin nodes of
           (Bin a0 a1) -> genericReuse2 Bin a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseBin"

reuseLeaf :: [Token doc Node clip token] -> Maybe Int -> Tree
reuseLeaf nodes ma0
  = case extractFromTokens extractLeaf defaultLeaf nodes of
           (Leaf a0) -> genericReuse1 Leaf a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseLeaf"

reuseList_Tree :: [Token doc Node clip token] -> Maybe ConsList_Tree -> List_Tree
reuseList_Tree nodes ma0
  = case extractFromTokens extractList_Tree defaultList_Tree nodes of
           (List_Tree a0) -> genericReuse1 List_Tree a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Tree"




--------------------------------------------------------------------------
-- extract functions                                                    --
--------------------------------------------------------------------------

extractRootEnr :: Maybe Node -> Maybe EnrichedDoc
extractRootEnr (Just (Node_RootEnr x@(RootEnr _ _) _)) = Just x
extractRootEnr _ = Nothing

extractRootDoc :: Maybe Node -> Maybe Document
extractRootDoc (Just (Node_RootDoc x@(RootDoc _ _) _)) = Just x
extractRootDoc _ = Nothing

extractBin :: Maybe Node -> Maybe Tree
extractBin (Just (Node_Bin x@(Bin _ _) _)) = Just x
extractBin _ = Nothing

extractLeaf :: Maybe Node -> Maybe Tree
extractLeaf (Just (Node_Leaf x@(Leaf _) _)) = Just x
extractLeaf _ = Nothing

extractList_Tree :: Maybe Node -> Maybe List_Tree
extractList_Tree (Just (Node_List_Tree x@(List_Tree _) _)) = Just x
extractList_Tree _ = Nothing




--------------------------------------------------------------------------
-- default functions                                                    --
--------------------------------------------------------------------------

defaultRootEnr :: EnrichedDoc
defaultRootEnr = RootEnr hole hole

defaultRootDoc :: Document
defaultRootDoc = RootDoc hole hole

defaultBin :: Tree
defaultBin = Bin hole hole

defaultLeaf :: Tree
defaultLeaf = Leaf hole

defaultList_Tree :: List_Tree
defaultList_Tree = List_Tree Nil_Tree




--------------------------------------------------------------------------
-- extractFromTokens                                                    --
--------------------------------------------------------------------------

-- return result of the first extraction application in the list that is not Nothing
extractFromTokens :: (Maybe Node -> Maybe a) -> a -> [Token doc Node clip token] -> a
extractFromTokens extr def []     = def
extractFromTokens extr def (t:ts) = maybe (extractFromTokens extr def ts) id (extr (tokenNode t))



--------------------------------------------------------------------------
-- genericReuse functions                                               --
--------------------------------------------------------------------------

genericReuse0 :: (r) ->
                 
                 r
genericReuse0 f =
  f

genericReuse1 :: (a0 -> r) ->
                 a0 -> 
                 Maybe a0 -> r
genericReuse1 f a0 ma0 =
  f (maybe a0 id ma0)

genericReuse2 :: (a0 -> a1 -> r) ->
                 a0 -> a1 -> 
                 Maybe a0 -> Maybe a1 -> r
genericReuse2 f a0 a1 ma0 ma1 =
  f (maybe a0 id ma0) (maybe a1 id ma1)



