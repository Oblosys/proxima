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
reuseRootEnr :: [Token doc Node clip token] -> Maybe IDD -> Maybe Root -> Maybe Document -> EnrichedDoc
reuseRootEnr nodes  ma0 ma1 ma2
  = case extractFromTokens extractRootEnr defaultRootEnr nodes of
           (RootEnr a0 a1 a2) -> reuse3 RootEnr a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseRootEnr"

reuseDummy :: [Token doc Node clip token] -> Maybe IDD -> Maybe List_Dummy -> Maybe String -> Maybe Bool -> Maybe Int -> Dummy
reuseDummy nodes  ma0 ma1 ma2 ma3 ma4
  = case extractFromTokens extractDummy defaultDummy nodes of
           (Dummy a0 a1 a2 a3 a4) -> reuse5 Dummy a0 a1 a2 a3 a4 ma0 ma1 ma2 ma3 ma4
           _ -> error "System error:<module>.reuseDummy"

reuseRoot :: [Token doc Node clip token] -> Maybe IDD -> Maybe Graph -> Maybe String -> Maybe List_Section -> Root
reuseRoot nodes  ma0 ma1 ma2 ma3
  = case extractFromTokens extractRoot defaultRoot nodes of
           (Root a0 a1 a2 a3) -> reuse4 Root a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseRoot"

reuseSection :: [Token doc Node clip token] -> Maybe IDD -> Maybe String -> Maybe List_Paragraph -> Maybe List_Subsection -> Section
reuseSection nodes  ma0 ma1 ma2 ma3
  = case extractFromTokens extractSection defaultSection nodes of
           (Section a0 a1 a2 a3) -> reuse4 Section a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseSection"

reuseSubsection :: [Token doc Node clip token] -> Maybe IDD -> Maybe String -> Maybe List_Paragraph -> Maybe List_Subsubsection -> Subsection
reuseSubsection nodes  ma0 ma1 ma2 ma3
  = case extractFromTokens extractSubsection defaultSubsection nodes of
           (Subsection a0 a1 a2 a3) -> reuse4 Subsection a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseSubsection"

reuseSubsubsection :: [Token doc Node clip token] -> Maybe IDD -> Maybe String -> Maybe List_Paragraph -> Subsubsection
reuseSubsubsection nodes  ma0 ma1 ma2
  = case extractFromTokens extractSubsubsection defaultSubsubsection nodes of
           (Subsubsection a0 a1 a2) -> reuse3 Subsubsection a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseSubsubsection"

reuseParagraph :: [Token doc Node clip token] -> Maybe IDD -> Maybe List_Word -> Paragraph
reuseParagraph nodes  ma0 ma1
  = case extractFromTokens extractParagraph defaultParagraph nodes of
           (Paragraph a0 a1) -> reuse2 Paragraph a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseParagraph"

reuseSubgraphPara :: [Token doc Node clip token] -> Maybe IDD -> Maybe Subgraph -> Paragraph
reuseSubgraphPara nodes  ma0 ma1
  = case extractFromTokens extractSubgraphPara defaultSubgraphPara nodes of
           (SubgraphPara a0 a1) -> reuse2 SubgraphPara a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseSubgraphPara"

reuseWord :: [Token doc Node clip token] -> Maybe IDD -> Maybe String -> Word
reuseWord nodes  ma0 ma1
  = case extractFromTokens extractWord defaultWord nodes of
           (Word a0 a1) -> reuse2 Word a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseWord"

reuseNodeRef :: [Token doc Node clip token] -> Maybe IDD -> Maybe String -> Word
reuseNodeRef nodes  ma0 ma1
  = case extractFromTokens extractNodeRef defaultNodeRef nodes of
           (NodeRef a0 a1) -> reuse2 NodeRef a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseNodeRef"

reuseGraph :: [Token doc Node clip token] -> Maybe IDD -> Maybe Dirty -> Maybe List_Vertex -> Maybe List_Edge -> Graph
reuseGraph nodes  ma0 ma1 ma2 ma3
  = case extractFromTokens extractGraph defaultGraph nodes of
           (Graph a0 a1 a2 a3) -> reuse4 Graph a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseGraph"

reuseVertex :: [Token doc Node clip token] -> Maybe IDD -> Maybe String -> Maybe Shape -> Maybe Int -> Maybe Int -> Maybe Int -> Vertex
reuseVertex nodes  ma0 ma1 ma2 ma3 ma4 ma5
  = case extractFromTokens extractVertex defaultVertex nodes of
           (Vertex a0 a1 a2 a3 a4 a5) -> reuse6 Vertex a0 a1 a2 a3 a4 a5 ma0 ma1 ma2 ma3 ma4 ma5
           _ -> error "System error:<module>.reuseVertex"

reuseCircle :: [Token doc Node clip token] -> Maybe IDD -> Shape
reuseCircle nodes  ma0
  = case extractFromTokens extractCircle defaultCircle nodes of
           (Circle a0) -> reuse1 Circle a0 ma0
           _ -> error "System error:<module>.reuseCircle"

reuseSquare :: [Token doc Node clip token] -> Maybe IDD -> Shape
reuseSquare nodes  ma0
  = case extractFromTokens extractSquare defaultSquare nodes of
           (Square a0) -> reuse1 Square a0 ma0
           _ -> error "System error:<module>.reuseSquare"

reuseEdge :: [Token doc Node clip token] -> Maybe IDD -> Maybe Int -> Maybe Int -> Edge
reuseEdge nodes  ma0 ma1 ma2
  = case extractFromTokens extractEdge defaultEdge nodes of
           (Edge a0 a1 a2) -> reuse3 Edge a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseEdge"

reuseSubgraph :: [Token doc Node clip token] -> Maybe IDD -> Maybe Dirty -> Maybe List_Vertex -> Maybe List_Edge -> Subgraph
reuseSubgraph nodes  ma0 ma1 ma2 ma3
  = case extractFromTokens extractSubgraph defaultSubgraph nodes of
           (Subgraph a0 a1 a2 a3) -> reuse4 Subgraph a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseSubgraph"

reuseDirty :: [Token doc Node clip token] -> Maybe IDD -> Dirty
reuseDirty nodes  ma0
  = case extractFromTokens extractDirty defaultDirty nodes of
           (Dirty a0) -> reuse1 Dirty a0 ma0
           _ -> error "System error:<module>.reuseDirty"

reuseClean :: [Token doc Node clip token] -> Maybe IDD -> Dirty
reuseClean nodes  ma0
  = case extractFromTokens extractClean defaultClean nodes of
           (Clean a0) -> reuse1 Clean a0 ma0
           _ -> error "System error:<module>.reuseClean"

reuseList_Dummy :: [Token doc Node clip token] -> Maybe IDD -> Maybe ConsList_Dummy -> List_Dummy
reuseList_Dummy nodes  ma0 ma1
  = case extractFromTokens extractList_Dummy defaultList_Dummy nodes of
           (List_Dummy a0 a1) -> reuse2 List_Dummy a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Dummy"

reuseList_Section :: [Token doc Node clip token] -> Maybe IDD -> Maybe ConsList_Section -> List_Section
reuseList_Section nodes  ma0 ma1
  = case extractFromTokens extractList_Section defaultList_Section nodes of
           (List_Section a0 a1) -> reuse2 List_Section a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Section"

reuseList_Paragraph :: [Token doc Node clip token] -> Maybe IDD -> Maybe ConsList_Paragraph -> List_Paragraph
reuseList_Paragraph nodes  ma0 ma1
  = case extractFromTokens extractList_Paragraph defaultList_Paragraph nodes of
           (List_Paragraph a0 a1) -> reuse2 List_Paragraph a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Paragraph"

reuseList_Subsection :: [Token doc Node clip token] -> Maybe IDD -> Maybe ConsList_Subsection -> List_Subsection
reuseList_Subsection nodes  ma0 ma1
  = case extractFromTokens extractList_Subsection defaultList_Subsection nodes of
           (List_Subsection a0 a1) -> reuse2 List_Subsection a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Subsection"

reuseList_Subsubsection :: [Token doc Node clip token] -> Maybe IDD -> Maybe ConsList_Subsubsection -> List_Subsubsection
reuseList_Subsubsection nodes  ma0 ma1
  = case extractFromTokens extractList_Subsubsection defaultList_Subsubsection nodes of
           (List_Subsubsection a0 a1) -> reuse2 List_Subsubsection a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Subsubsection"

reuseList_Word :: [Token doc Node clip token] -> Maybe IDD -> Maybe ConsList_Word -> List_Word
reuseList_Word nodes  ma0 ma1
  = case extractFromTokens extractList_Word defaultList_Word nodes of
           (List_Word a0 a1) -> reuse2 List_Word a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Word"

reuseList_Vertex :: [Token doc Node clip token] -> Maybe IDD -> Maybe ConsList_Vertex -> List_Vertex
reuseList_Vertex nodes  ma0 ma1
  = case extractFromTokens extractList_Vertex defaultList_Vertex nodes of
           (List_Vertex a0 a1) -> reuse2 List_Vertex a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Vertex"

reuseList_Edge :: [Token doc Node clip token] -> Maybe IDD -> Maybe ConsList_Edge -> List_Edge
reuseList_Edge nodes  ma0 ma1
  = case extractFromTokens extractList_Edge defaultList_Edge nodes of
           (List_Edge a0 a1) -> reuse2 List_Edge a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Edge"

extractRootEnr :: Maybe Node -> Maybe EnrichedDoc
extractRootEnr (Just (RootEnrNode x@(RootEnr _ _ _) _)) = Just x
extractRootEnr _ = Nothing

extractDummy :: Maybe Node -> Maybe Dummy
extractDummy (Just (DummyNode x@(Dummy _ _ _ _ _) _)) = Just x
extractDummy _ = Nothing

extractRoot :: Maybe Node -> Maybe Root
extractRoot (Just (RootNode x@(Root _ _ _ _) _)) = Just x
extractRoot _ = Nothing

extractSection :: Maybe Node -> Maybe Section
extractSection (Just (SectionNode x@(Section _ _ _ _) _)) = Just x
extractSection _ = Nothing

extractSubsection :: Maybe Node -> Maybe Subsection
extractSubsection (Just (SubsectionNode x@(Subsection _ _ _ _) _)) = Just x
extractSubsection _ = Nothing

extractSubsubsection :: Maybe Node -> Maybe Subsubsection
extractSubsubsection (Just (SubsubsectionNode x@(Subsubsection _ _ _) _)) = Just x
extractSubsubsection _ = Nothing

extractParagraph :: Maybe Node -> Maybe Paragraph
extractParagraph (Just (ParagraphNode x@(Paragraph _ _) _)) = Just x
extractParagraph _ = Nothing

extractSubgraphPara :: Maybe Node -> Maybe Paragraph
extractSubgraphPara (Just (SubgraphParaNode x@(SubgraphPara _ _) _)) = Just x
extractSubgraphPara _ = Nothing

extractWord :: Maybe Node -> Maybe Word
extractWord (Just (WordNode x@(Word _ _) _)) = Just x
extractWord _ = Nothing

extractNodeRef :: Maybe Node -> Maybe Word
extractNodeRef (Just (NodeRefNode x@(NodeRef _ _) _)) = Just x
extractNodeRef _ = Nothing

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

extractList_Subsection :: Maybe Node -> Maybe List_Subsection
extractList_Subsection (Just (List_SubsectionNode x@(List_Subsection _ _) _)) = Just x
extractList_Subsection _ = Nothing

extractList_Subsubsection :: Maybe Node -> Maybe List_Subsubsection
extractList_Subsubsection (Just (List_SubsubsectionNode x@(List_Subsubsection _ _) _)) = Just x
extractList_Subsubsection _ = Nothing

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

defaultDummy :: Dummy
defaultDummy = Dummy NoIDD hole hole hole hole

defaultRoot :: Root
defaultRoot = Root NoIDD hole hole hole

defaultSection :: Section
defaultSection = Section NoIDD hole hole hole

defaultSubsection :: Subsection
defaultSubsection = Subsection NoIDD hole hole hole

defaultSubsubsection :: Subsubsection
defaultSubsubsection = Subsubsection NoIDD hole hole

defaultParagraph :: Paragraph
defaultParagraph = Paragraph NoIDD hole

defaultSubgraphPara :: Paragraph
defaultSubgraphPara = SubgraphPara NoIDD hole

defaultWord :: Word
defaultWord = Word NoIDD hole

defaultNodeRef :: Word
defaultNodeRef = NodeRef NoIDD hole

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

defaultList_Subsection :: List_Subsection
defaultList_Subsection = List_Subsection NoIDD Nil_Subsection

defaultList_Subsubsection :: List_Subsubsection
defaultList_Subsubsection = List_Subsubsection NoIDD Nil_Subsubsection

defaultList_Word :: List_Word
defaultList_Word = List_Word NoIDD Nil_Word

defaultList_Vertex :: List_Vertex
defaultList_Vertex = List_Vertex NoIDD Nil_Vertex

defaultList_Edge :: List_Edge
defaultList_Edge = List_Edge NoIDD Nil_Edge

-- General
-- return result of the first extraction application in the list that is not Nothing
extractFromTokens :: (Maybe Node -> Maybe a) -> a -> [Token doc Node clip token] -> a
extractFromTokens extr def []     = def
extractFromTokens extr def (t:ts) = maybe (extractFromTokens extr def ts) id (extr (tokenNode t))

reuse3 :: (a0 -> a1 -> a2 -> r) -> 
          a0 -> a1 -> a2 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> r
reuse3 f  a0 a1 a2 ma0 ma1 ma2 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) 

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

reuse2 :: (a0 -> a1 -> r) -> 
          a0 -> a1 -> 
          Maybe a0 -> Maybe a1 -> r
reuse2 f  a0 a1 ma0 ma1 =
  f (maybe a0 id ma0) (maybe a1 id ma1) 

reuse6 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> r) -> 
          a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> r
reuse6 f  a0 a1 a2 a3 a4 a5 ma0 ma1 ma2 ma3 ma4 ma5 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) (maybe a5 id ma5) 

reuse1 :: (a0 -> r) -> 
          a0 -> 
          Maybe a0 -> r
reuse1 f  a0 ma0 =
  f (maybe a0 id ma0) 

reuse0 :: r -> r
reuse0 f = f

