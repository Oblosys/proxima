module ProxParser_Generated where

import CommonTypes
import PresLayerTypes
import PresLayerUtils

import DocumentEdit
import DocumentEdit_Generated
import DocTypes
import DocTypes_Generated

----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

{- ------------------------------------

 generated part

-------------------------------------- -}






-- ProxParser_Generated --

-- Type specific
reuseRootEnr :: [Maybe Node] -> Maybe IDD -> Maybe Root -> Maybe Document -> EnrichedDoc
reuseRootEnr nodes  ma0 ma1 ma2
  = case extractFromNodes extractRootEnr defaultRootEnr nodes of
           (RootEnr a0 a1 a2) -> reuse3 RootEnr a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseRootEnr"

reuseString_ :: [Maybe Node] -> Maybe IDD -> Maybe String -> String_
reuseString_ nodes  ma0 ma1
  = case extractFromNodes extractString_ defaultString_ nodes of
           (String_ a0 a1) -> reuse2 String_ a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseString_"

reuseBool_ :: [Maybe Node] -> Maybe IDD -> Maybe Bool -> Bool_
reuseBool_ nodes  ma0 ma1
  = case extractFromNodes extractBool_ defaultBool_ nodes of
           (Bool_ a0 a1) -> reuse2 Bool_ a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseBool_"

reuseInt_ :: [Maybe Node] -> Maybe IDD -> Maybe Int -> Int_
reuseInt_ nodes  ma0 ma1
  = case extractFromNodes extractInt_ defaultInt_ nodes of
           (Int_ a0 a1) -> reuse2 Int_ a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseInt_"

reuseDummy :: [Maybe Node] -> Maybe IDD -> Maybe List_Dummy -> Maybe String_ -> Maybe Bool_ -> Maybe Int_ -> Dummy
reuseDummy nodes  ma0 ma1 ma2 ma3 ma4
  = case extractFromNodes extractDummy defaultDummy nodes of
           (Dummy a0 a1 a2 a3 a4) -> reuse5 Dummy a0 a1 a2 a3 a4 ma0 ma1 ma2 ma3 ma4
           _ -> error "System error:<module>.reuseDummy"

reuseRoot :: [Maybe Node] -> Maybe IDD -> Maybe Tree -> Root
reuseRoot nodes  ma0 ma1
  = case extractFromNodes extractRoot defaultRoot nodes of
           (Root a0 a1) -> reuse2 Root a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseRoot"

reuseBin :: [Maybe Node] -> Maybe IDD -> Maybe Tree -> Maybe Tree -> Tree
reuseBin nodes  ma0 ma1 ma2
  = case extractFromNodes extractBin defaultBin nodes of
           (Bin a0 a1 a2) -> reuse3 Bin a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseBin"

reuseLeaf :: [Maybe Node] -> Maybe IDD -> Tree
reuseLeaf nodes  ma0
  = case extractFromNodes extractLeaf defaultLeaf nodes of
           (Leaf a0) -> reuse1 Leaf a0 ma0
           _ -> error "System error:<module>.reuseLeaf"

reuseList_Dummy :: [Maybe Node] -> Maybe IDD -> Maybe ConsList_Dummy -> List_Dummy
reuseList_Dummy nodes  ma0 ma1
  = case extractFromNodes extractList_Dummy defaultList_Dummy nodes of
           (List_Dummy a0 a1) -> reuse2 List_Dummy a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Dummy"

extractRootEnr :: Maybe Node -> Maybe EnrichedDoc
extractRootEnr (Just (RootEnrNode x@(RootEnr _ _ _) _)) = Just x
extractRootEnr _ = Nothing

extractString_ :: Maybe Node -> Maybe String_
extractString_ (Just (String_Node x@(String_ _ _) _)) = Just x
extractString_ _ = Nothing

extractBool_ :: Maybe Node -> Maybe Bool_
extractBool_ (Just (Bool_Node x@(Bool_ _ _) _)) = Just x
extractBool_ _ = Nothing

extractInt_ :: Maybe Node -> Maybe Int_
extractInt_ (Just (Int_Node x@(Int_ _ _) _)) = Just x
extractInt_ _ = Nothing

extractDummy :: Maybe Node -> Maybe Dummy
extractDummy (Just (DummyNode x@(Dummy _ _ _ _ _) _)) = Just x
extractDummy _ = Nothing

extractRoot :: Maybe Node -> Maybe Root
extractRoot (Just (RootNode x@(Root _ _) _)) = Just x
extractRoot _ = Nothing

extractBin :: Maybe Node -> Maybe Tree
extractBin (Just (BinNode x@(Bin _ _ _) _)) = Just x
extractBin _ = Nothing

extractLeaf :: Maybe Node -> Maybe Tree
extractLeaf (Just (LeafNode x@(Leaf _) _)) = Just x
extractLeaf _ = Nothing

extractList_Dummy :: Maybe Node -> Maybe List_Dummy
extractList_Dummy (Just (List_DummyNode x@(List_Dummy _ _) _)) = Just x
extractList_Dummy _ = Nothing

defaultRootEnr :: EnrichedDoc
defaultRootEnr = RootEnr NoIDD hole hole

defaultString_ :: String_
defaultString_ = String_ NoIDD hole

defaultBool_ :: Bool_
defaultBool_ = Bool_ NoIDD hole

defaultInt_ :: Int_
defaultInt_ = Int_ NoIDD hole

defaultDummy :: Dummy
defaultDummy = Dummy NoIDD hole hole hole hole

defaultRoot :: Root
defaultRoot = Root NoIDD hole

defaultBin :: Tree
defaultBin = Bin NoIDD hole hole

defaultLeaf :: Tree
defaultLeaf = Leaf NoIDD

defaultList_Dummy :: List_Dummy
defaultList_Dummy = List_Dummy NoIDD Nil_Dummy

-- General
-- return result of the first extraction application in the list that is not Nothing
--extractFromNodes ::(Node -> Maybe a) -> a -> [Node] -> a
extractFromNodes extr def []     = def
extractFromNodes extr def (n:ns) = maybe (extractFromNodes extr def ns) id (extr n)

reuse3 :: (a0 -> a1 -> a2 -> r) -> 
          a0 -> a1 -> a2 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> r
reuse3 f  a0 a1 a2 ma0 ma1 ma2 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) 

reuse2 :: (a0 -> a1 -> r) -> 
          a0 -> a1 -> 
          Maybe a0 -> Maybe a1 -> r
reuse2 f  a0 a1 ma0 ma1 =
  f (maybe a0 id ma0) (maybe a1 id ma1) 

reuse5 :: (a0 -> a1 -> a2 -> a3 -> a4 -> r) -> 
          a0 -> a1 -> a2 -> a3 -> a4 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> r
reuse5 f  a0 a1 a2 a3 a4 ma0 ma1 ma2 ma3 ma4 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) 

reuse1 :: (a0 -> r) -> 
          a0 -> 
          Maybe a0 -> r
reuse1 f  a0 ma0 =
  f (maybe a0 id ma0) 

reuse0 :: r -> r
reuse0 f = f

