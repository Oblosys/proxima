module ProxParser_Generated where

import Common.CommonTypes hiding (Dirty (..))
import Presentation.PresLayerTypes
import Presentation.PresLayerUtils

import Evaluation.DocumentEdit
import DocumentEdit_Generated
import Evaluation.DocTypes
import DocTypes_Generated

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



