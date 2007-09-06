module ProxParser_Generated where

import CommonTypes hiding (Dirty (..))
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

reuseRoot :: [Maybe Node] -> Maybe IDD -> Maybe Tree -> Maybe Graph -> Maybe List_Section -> Root
reuseRoot nodes  ma0 ma1 ma2 ma3
  = case extractFromNodes extractRoot defaultRoot nodes of
           (Root a0 a1 a2 a3) -> reuse4 Root a0 a1 a2 a3 ma0 ma1 ma2 ma3
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

reuseSection :: [Maybe Node] -> Maybe IDD -> Maybe List_Paragraph -> Maybe Subgraph -> Section
reuseSection nodes  ma0 ma1 ma2
  = case extractFromNodes extractSection defaultSection nodes of
           (Section a0 a1 a2) -> reuse3 Section a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseSection"

reuseParagraph :: [Maybe Node] -> Maybe IDD -> Maybe List_Word -> Paragraph
reuseParagraph nodes  ma0 ma1
  = case extractFromNodes extractParagraph defaultParagraph nodes of
           (Paragraph a0 a1) -> reuse2 Paragraph a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseParagraph"

reuseWord :: [Maybe Node] -> Maybe IDD -> Maybe String_ -> Word
reuseWord nodes  ma0 ma1
  = case extractFromNodes extractWord defaultWord nodes of
           (Word a0 a1) -> reuse2 Word a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseWord"

reuseGraph :: [Maybe Node] -> Maybe IDD -> Maybe Dirty -> Maybe List_Vertex -> Maybe List_Edge -> Graph
reuseGraph nodes  ma0 ma1 ma2 ma3
  = case extractFromNodes extractGraph defaultGraph nodes of
           (Graph a0 a1 a2 a3) -> reuse4 Graph a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseGraph"

reuseVertex :: [Maybe Node] -> Maybe IDD -> Maybe String_ -> Maybe Shape -> Maybe Int_ -> Maybe Int_ -> Maybe Int_ -> Vertex
reuseVertex nodes  ma0 ma1 ma2 ma3 ma4 ma5
  = case extractFromNodes extractVertex defaultVertex nodes of
           (Vertex a0 a1 a2 a3 a4 a5) -> reuse6 Vertex a0 a1 a2 a3 a4 a5 ma0 ma1 ma2 ma3 ma4 ma5
           _ -> error "System error:<module>.reuseVertex"

reuseCircle :: [Maybe Node] -> Maybe IDD -> Shape
reuseCircle nodes  ma0
  = case extractFromNodes extractCircle defaultCircle nodes of
           (Circle a0) -> reuse1 Circle a0 ma0
           _ -> error "System error:<module>.reuseCircle"

reuseSquare :: [Maybe Node] -> Maybe IDD -> Shape
reuseSquare nodes  ma0
  = case extractFromNodes extractSquare defaultSquare nodes of
           (Square a0) -> reuse1 Square a0 ma0
           _ -> error "System error:<module>.reuseSquare"

reuseEdge :: [Maybe Node] -> Maybe IDD -> Maybe Int_ -> Maybe Int_ -> Edge
reuseEdge nodes  ma0 ma1 ma2
  = case extractFromNodes extractEdge defaultEdge nodes of
           (Edge a0 a1 a2) -> reuse3 Edge a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseEdge"

reuseSubgraph :: [Maybe Node] -> Maybe IDD -> Maybe Dirty -> Maybe List_Vertex -> Maybe List_Edge -> Subgraph
reuseSubgraph nodes  ma0 ma1 ma2 ma3
  = case extractFromNodes extractSubgraph defaultSubgraph nodes of
           (Subgraph a0 a1 a2 a3) -> reuse4 Subgraph a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseSubgraph"

reuseDirty :: [Maybe Node] -> Maybe IDD -> Dirty
reuseDirty nodes  ma0
  = case extractFromNodes extractDirty defaultDirty nodes of
           (Dirty a0) -> reuse1 Dirty a0 ma0
           _ -> error "System error:<module>.reuseDirty"

reuseClean :: [Maybe Node] -> Maybe IDD -> Dirty
reuseClean nodes  ma0
  = case extractFromNodes extractClean defaultClean nodes of
           (Clean a0) -> reuse1 Clean a0 ma0
           _ -> error "System error:<module>.reuseClean"

reuseList_Dummy :: [Maybe Node] -> Maybe IDD -> Maybe ConsList_Dummy -> List_Dummy
reuseList_Dummy nodes  ma0 ma1
  = case extractFromNodes extractList_Dummy defaultList_Dummy nodes of
           (List_Dummy a0 a1) -> reuse2 List_Dummy a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Dummy"

reuseList_Section :: [Maybe Node] -> Maybe IDD -> Maybe ConsList_Section -> List_Section
reuseList_Section nodes  ma0 ma1
  = case extractFromNodes extractList_Section defaultList_Section nodes of
           (List_Section a0 a1) -> reuse2 List_Section a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Section"

reuseList_Paragraph :: [Maybe Node] -> Maybe IDD -> Maybe ConsList_Paragraph -> List_Paragraph
reuseList_Paragraph nodes  ma0 ma1
  = case extractFromNodes extractList_Paragraph defaultList_Paragraph nodes of
           (List_Paragraph a0 a1) -> reuse2 List_Paragraph a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Paragraph"

reuseList_Word :: [Maybe Node] -> Maybe IDD -> Maybe ConsList_Word -> List_Word
reuseList_Word nodes  ma0 ma1
  = case extractFromNodes extractList_Word defaultList_Word nodes of
           (List_Word a0 a1) -> reuse2 List_Word a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Word"

reuseList_Vertex :: [Maybe Node] -> Maybe IDD -> Maybe ConsList_Vertex -> List_Vertex
reuseList_Vertex nodes  ma0 ma1
  = case extractFromNodes extractList_Vertex defaultList_Vertex nodes of
           (List_Vertex a0 a1) -> reuse2 List_Vertex a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Vertex"

reuseList_Edge :: [Maybe Node] -> Maybe IDD -> Maybe ConsList_Edge -> List_Edge
reuseList_Edge nodes  ma0 ma1
  = case extractFromNodes extractList_Edge defaultList_Edge nodes of
           (List_Edge a0 a1) -> reuse2 List_Edge a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Edge"

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
extractRoot (Just (RootNode x@(Root _ _ _ _) _)) = Just x
extractRoot _ = Nothing

extractBin :: Maybe Node -> Maybe Tree
extractBin (Just (BinNode x@(Bin _ _ _) _)) = Just x
extractBin _ = Nothing

extractLeaf :: Maybe Node -> Maybe Tree
extractLeaf (Just (LeafNode x@(Leaf _) _)) = Just x
extractLeaf _ = Nothing

extractSection :: Maybe Node -> Maybe Section
extractSection (Just (SectionNode x@(Section _ _ _) _)) = Just x
extractSection _ = Nothing

extractParagraph :: Maybe Node -> Maybe Paragraph
extractParagraph (Just (ParagraphNode x@(Paragraph _ _) _)) = Just x
extractParagraph _ = Nothing

extractWord :: Maybe Node -> Maybe Word
extractWord (Just (WordNode x@(Word _ _) _)) = Just x
extractWord _ = Nothing

extractGraph :: Maybe Node -> Maybe Graph
extractGraph (Just (GraphNode x@(Graph _ _ _ _) _)) = Just x
extractGraph _ = Nothing

extractVertex :: Maybe Node -> Maybe Vertex
extractVertex (Just (VertexNode x@(Vertex _ _ _ _ _ _) _)) = Just x
extractVertex _ = Nothing

extractCircle :: Maybe Node -> Maybe Shape
extractCircle (Just (CircleNode x@(Circle _) _)) = Just x
extractCircle _ = Nothing

extractSquare :: Maybe Node -> Maybe Shape
extractSquare (Just (SquareNode x@(Square _) _)) = Just x
extractSquare _ = Nothing

extractEdge :: Maybe Node -> Maybe Edge
extractEdge (Just (EdgeNode x@(Edge _ _ _) _)) = Just x
extractEdge _ = Nothing

extractSubgraph :: Maybe Node -> Maybe Subgraph
extractSubgraph (Just (SubgraphNode x@(Subgraph _ _ _ _) _)) = Just x
extractSubgraph _ = Nothing

extractDirty :: Maybe Node -> Maybe Dirty
extractDirty (Just (DirtyNode x@(Dirty _) _)) = Just x
extractDirty _ = Nothing

extractClean :: Maybe Node -> Maybe Dirty
extractClean (Just (CleanNode x@(Clean _) _)) = Just x
extractClean _ = Nothing

extractList_Dummy :: Maybe Node -> Maybe List_Dummy
extractList_Dummy (Just (List_DummyNode x@(List_Dummy _ _) _)) = Just x
extractList_Dummy _ = Nothing

extractList_Section :: Maybe Node -> Maybe List_Section
extractList_Section (Just (List_SectionNode x@(List_Section _ _) _)) = Just x
extractList_Section _ = Nothing

extractList_Paragraph :: Maybe Node -> Maybe List_Paragraph
extractList_Paragraph (Just (List_ParagraphNode x@(List_Paragraph _ _) _)) = Just x
extractList_Paragraph _ = Nothing

extractList_Word :: Maybe Node -> Maybe List_Word
extractList_Word (Just (List_WordNode x@(List_Word _ _) _)) = Just x
extractList_Word _ = Nothing

extractList_Vertex :: Maybe Node -> Maybe List_Vertex
extractList_Vertex (Just (List_VertexNode x@(List_Vertex _ _) _)) = Just x
extractList_Vertex _ = Nothing

extractList_Edge :: Maybe Node -> Maybe List_Edge
extractList_Edge (Just (List_EdgeNode x@(List_Edge _ _) _)) = Just x
extractList_Edge _ = Nothing

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
defaultRoot = Root NoIDD hole hole hole

defaultBin :: Tree
defaultBin = Bin NoIDD hole hole

defaultLeaf :: Tree
defaultLeaf = Leaf NoIDD

defaultSection :: Section
defaultSection = Section NoIDD hole hole

defaultParagraph :: Paragraph
defaultParagraph = Paragraph NoIDD hole

defaultWord :: Word
defaultWord = Word NoIDD hole

defaultGraph :: Graph
defaultGraph = Graph NoIDD hole hole hole

defaultVertex :: Vertex
defaultVertex = Vertex NoIDD hole hole hole hole hole

defaultCircle :: Shape
defaultCircle = Circle NoIDD

defaultSquare :: Shape
defaultSquare = Square NoIDD

defaultEdge :: Edge
defaultEdge = Edge NoIDD hole hole

defaultSubgraph :: Subgraph
defaultSubgraph = Subgraph NoIDD hole hole hole

defaultDirty :: Dirty
defaultDirty = Dirty NoIDD

defaultClean :: Dirty
defaultClean = Clean NoIDD

defaultList_Dummy :: List_Dummy
defaultList_Dummy = List_Dummy NoIDD Nil_Dummy

defaultList_Section :: List_Section
defaultList_Section = List_Section NoIDD Nil_Section

defaultList_Paragraph :: List_Paragraph
defaultList_Paragraph = List_Paragraph NoIDD Nil_Paragraph

defaultList_Word :: List_Word
defaultList_Word = List_Word NoIDD Nil_Word

defaultList_Vertex :: List_Vertex
defaultList_Vertex = List_Vertex NoIDD Nil_Vertex

defaultList_Edge :: List_Edge
defaultList_Edge = List_Edge NoIDD Nil_Edge

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

reuse4 :: (a0 -> a1 -> a2 -> a3 -> r) -> 
          a0 -> a1 -> a2 -> a3 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> r
reuse4 f  a0 a1 a2 a3 ma0 ma1 ma2 ma3 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) 

reuse1 :: (a0 -> r) -> 
          a0 -> 
          Maybe a0 -> r
reuse1 f  a0 ma0 =
  f (maybe a0 id ma0) 

reuse6 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> r) -> 
          a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> r
reuse6 f  a0 a1 a2 a3 a4 a5 ma0 ma1 ma2 ma3 ma4 ma5 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) (maybe a5 id ma5) 

reuse0 :: r -> r
reuse0 f = f

