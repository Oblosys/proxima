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

reuseRootEnr :: [Token doc Node clip token] -> Maybe Root -> Maybe Document -> EnrichedDoc
reuseRootEnr nodes ma0 ma1
  = case extractFromTokens extractRootEnr defaultRootEnr nodes of
           (RootEnr a0 a1) -> reuse2 RootEnr a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseRootEnr"

reuseDummy :: [Token doc Node clip token] -> Maybe Dummy -> Maybe Bool -> Dummy
reuseDummy nodes ma0 ma1
  = case extractFromTokens extractDummy defaultDummy nodes of
           (Dummy a0 a1) -> reuse2 Dummy a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseDummy"

reuseRoot :: [Token doc Node clip token] -> Maybe Graph -> Maybe String -> Maybe List_Section -> Root
reuseRoot nodes ma0 ma1 ma2
  = case extractFromTokens extractRoot defaultRoot nodes of
           (Root a0 a1 a2) -> reuse3 Root a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reuseRoot"

reuseSection :: [Token doc Node clip token] -> Maybe String -> Maybe List_Paragraph -> Maybe List_Subsection -> Section
reuseSection nodes ma0 ma1 ma2
  = case extractFromTokens extractSection defaultSection nodes of
           (Section a0 a1 a2) -> reuse3 Section a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reuseSection"

reuseSubsection :: [Token doc Node clip token] -> Maybe String -> Maybe List_Paragraph -> Maybe List_Subsubsection -> Subsection
reuseSubsection nodes ma0 ma1 ma2
  = case extractFromTokens extractSubsection defaultSubsection nodes of
           (Subsection a0 a1 a2) -> reuse3 Subsection a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reuseSubsection"

reuseSubsubsection :: [Token doc Node clip token] -> Maybe String -> Maybe List_Paragraph -> Subsubsection
reuseSubsubsection nodes ma0 ma1
  = case extractFromTokens extractSubsubsection defaultSubsubsection nodes of
           (Subsubsection a0 a1) -> reuse2 Subsubsection a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseSubsubsection"

reuseParagraph :: [Token doc Node clip token] -> Maybe List_Word -> Paragraph
reuseParagraph nodes ma0
  = case extractFromTokens extractParagraph defaultParagraph nodes of
           (Paragraph a0) -> reuse1 Paragraph a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseParagraph"

reuseSubgraphPara :: [Token doc Node clip token] -> Maybe Subgraph -> Paragraph
reuseSubgraphPara nodes ma0
  = case extractFromTokens extractSubgraphPara defaultSubgraphPara nodes of
           (SubgraphPara a0) -> reuse1 SubgraphPara a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseSubgraphPara"

reuseWord :: [Token doc Node clip token] -> Maybe String -> Word
reuseWord nodes ma0
  = case extractFromTokens extractWord defaultWord nodes of
           (Word a0) -> reuse1 Word a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseWord"

reuseNodeRef :: [Token doc Node clip token] -> Maybe String -> Word
reuseNodeRef nodes ma0
  = case extractFromTokens extractNodeRef defaultNodeRef nodes of
           (NodeRef a0) -> reuse1 NodeRef a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseNodeRef"

reuseLabel :: [Token doc Node clip token] -> Maybe String -> Word
reuseLabel nodes ma0
  = case extractFromTokens extractLabel defaultLabel nodes of
           (Label a0) -> reuse1 Label a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseLabel"

reuseLabelRef :: [Token doc Node clip token] -> Maybe String -> Word
reuseLabelRef nodes ma0
  = case extractFromTokens extractLabelRef defaultLabelRef nodes of
           (LabelRef a0) -> reuse1 LabelRef a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseLabelRef"

reuseGraph :: [Token doc Node clip token] -> Maybe Dirty -> Maybe List_Vertex -> Maybe List_Edge -> Graph
reuseGraph nodes ma0 ma1 ma2
  = case extractFromTokens extractGraph defaultGraph nodes of
           (Graph a0 a1 a2) -> reuse3 Graph a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reuseGraph"

reuseVertex :: [Token doc Node clip token] -> Maybe String -> Maybe Shape -> Maybe Int -> Maybe Int -> Maybe Int -> Vertex
reuseVertex nodes ma0 ma1 ma2 ma3 ma4
  = case extractFromTokens extractVertex defaultVertex nodes of
           (Vertex a0 a1 a2 a3 a4) -> reuse5 Vertex a0 a1 a2 a3 a4 ma0 ma1 ma2 ma3 ma4
           _ -> error "Internal error:ProxParser_Generated.reuseVertex"

reuseCircle :: [Token doc Node clip token] -> Shape
reuseCircle nodes
  = case extractFromTokens extractCircle defaultCircle nodes of
           (Circle) -> reuse0 Circle
           _ -> error "Internal error:ProxParser_Generated.reuseCircle"

reuseSquare :: [Token doc Node clip token] -> Shape
reuseSquare nodes
  = case extractFromTokens extractSquare defaultSquare nodes of
           (Square) -> reuse0 Square
           _ -> error "Internal error:ProxParser_Generated.reuseSquare"

reuseEdge :: [Token doc Node clip token] -> Maybe Int -> Maybe Int -> Edge
reuseEdge nodes ma0 ma1
  = case extractFromTokens extractEdge defaultEdge nodes of
           (Edge a0 a1) -> reuse2 Edge a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseEdge"

reuseSubgraph :: [Token doc Node clip token] -> Maybe Dirty -> Maybe List_Vertex -> Maybe List_Edge -> Subgraph
reuseSubgraph nodes ma0 ma1 ma2
  = case extractFromTokens extractSubgraph defaultSubgraph nodes of
           (Subgraph a0 a1 a2) -> reuse3 Subgraph a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reuseSubgraph"

reuseDirty :: [Token doc Node clip token] -> Dirty
reuseDirty nodes
  = case extractFromTokens extractDirty defaultDirty nodes of
           (Dirty) -> reuse0 Dirty
           _ -> error "Internal error:ProxParser_Generated.reuseDirty"

reuseClean :: [Token doc Node clip token] -> Dirty
reuseClean nodes
  = case extractFromTokens extractClean defaultClean nodes of
           (Clean) -> reuse0 Clean
           _ -> error "Internal error:ProxParser_Generated.reuseClean"

reuseList_Section :: [Token doc Node clip token] -> Maybe ConsList_Section -> List_Section
reuseList_Section nodes ma0
  = case extractFromTokens extractList_Section defaultList_Section nodes of
           (List_Section a0) -> reuse1 List_Section a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Section"

reuseList_Paragraph :: [Token doc Node clip token] -> Maybe ConsList_Paragraph -> List_Paragraph
reuseList_Paragraph nodes ma0
  = case extractFromTokens extractList_Paragraph defaultList_Paragraph nodes of
           (List_Paragraph a0) -> reuse1 List_Paragraph a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Paragraph"

reuseList_Subsection :: [Token doc Node clip token] -> Maybe ConsList_Subsection -> List_Subsection
reuseList_Subsection nodes ma0
  = case extractFromTokens extractList_Subsection defaultList_Subsection nodes of
           (List_Subsection a0) -> reuse1 List_Subsection a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Subsection"

reuseList_Subsubsection :: [Token doc Node clip token] -> Maybe ConsList_Subsubsection -> List_Subsubsection
reuseList_Subsubsection nodes ma0
  = case extractFromTokens extractList_Subsubsection defaultList_Subsubsection nodes of
           (List_Subsubsection a0) -> reuse1 List_Subsubsection a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Subsubsection"

reuseList_Word :: [Token doc Node clip token] -> Maybe ConsList_Word -> List_Word
reuseList_Word nodes ma0
  = case extractFromTokens extractList_Word defaultList_Word nodes of
           (List_Word a0) -> reuse1 List_Word a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Word"

reuseList_Vertex :: [Token doc Node clip token] -> Maybe ConsList_Vertex -> List_Vertex
reuseList_Vertex nodes ma0
  = case extractFromTokens extractList_Vertex defaultList_Vertex nodes of
           (List_Vertex a0) -> reuse1 List_Vertex a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Vertex"

reuseList_Edge :: [Token doc Node clip token] -> Maybe ConsList_Edge -> List_Edge
reuseList_Edge nodes ma0
  = case extractFromTokens extractList_Edge defaultList_Edge nodes of
           (List_Edge a0) -> reuse1 List_Edge a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Edge"




--------------------------------------------------------------------------
-- extract functions                                                    --
--------------------------------------------------------------------------

extractRootEnr :: Maybe Node -> Maybe EnrichedDoc
extractRootEnr (Just (RootEnrNode x@(RootEnr _ _) _)) = Just x
extractRootEnr _ = Nothing

extractDummy :: Maybe Node -> Maybe Dummy
extractDummy (Just (DummyNode x@(Dummy _ _) _)) = Just x
extractDummy _ = Nothing

extractRoot :: Maybe Node -> Maybe Root
extractRoot (Just (RootNode x@(Root _ _ _) _)) = Just x
extractRoot _ = Nothing

extractSection :: Maybe Node -> Maybe Section
extractSection (Just (SectionNode x@(Section _ _ _) _)) = Just x
extractSection _ = Nothing

extractSubsection :: Maybe Node -> Maybe Subsection
extractSubsection (Just (SubsectionNode x@(Subsection _ _ _) _)) = Just x
extractSubsection _ = Nothing

extractSubsubsection :: Maybe Node -> Maybe Subsubsection
extractSubsubsection (Just (SubsubsectionNode x@(Subsubsection _ _) _)) = Just x
extractSubsubsection _ = Nothing

extractParagraph :: Maybe Node -> Maybe Paragraph
extractParagraph (Just (ParagraphNode x@(Paragraph _) _)) = Just x
extractParagraph _ = Nothing

extractSubgraphPara :: Maybe Node -> Maybe Paragraph
extractSubgraphPara (Just (SubgraphParaNode x@(SubgraphPara _) _)) = Just x
extractSubgraphPara _ = Nothing

extractWord :: Maybe Node -> Maybe Word
extractWord (Just (WordNode x@(Word _) _)) = Just x
extractWord _ = Nothing

extractNodeRef :: Maybe Node -> Maybe Word
extractNodeRef (Just (NodeRefNode x@(NodeRef _) _)) = Just x
extractNodeRef _ = Nothing

extractLabel :: Maybe Node -> Maybe Word
extractLabel (Just (LabelNode x@(Label _) _)) = Just x
extractLabel _ = Nothing

extractLabelRef :: Maybe Node -> Maybe Word
extractLabelRef (Just (LabelRefNode x@(LabelRef _) _)) = Just x
extractLabelRef _ = Nothing

extractGraph :: Maybe Node -> Maybe Graph
extractGraph (Just (GraphNode x@(Graph _ _ _) _)) = Just x
extractGraph _ = Nothing

extractVertex :: Maybe Node -> Maybe Vertex
extractVertex (Just (VertexNode x@(Vertex _ _ _ _ _) _)) = Just x
extractVertex _ = Nothing

extractCircle :: Maybe Node -> Maybe Shape
extractCircle (Just (CircleNode x@(Circle) _)) = Just x
extractCircle _ = Nothing

extractSquare :: Maybe Node -> Maybe Shape
extractSquare (Just (SquareNode x@(Square) _)) = Just x
extractSquare _ = Nothing

extractEdge :: Maybe Node -> Maybe Edge
extractEdge (Just (EdgeNode x@(Edge _ _) _)) = Just x
extractEdge _ = Nothing

extractSubgraph :: Maybe Node -> Maybe Subgraph
extractSubgraph (Just (SubgraphNode x@(Subgraph _ _ _) _)) = Just x
extractSubgraph _ = Nothing

extractDirty :: Maybe Node -> Maybe Dirty
extractDirty (Just (DirtyNode x@(Dirty) _)) = Just x
extractDirty _ = Nothing

extractClean :: Maybe Node -> Maybe Dirty
extractClean (Just (CleanNode x@(Clean) _)) = Just x
extractClean _ = Nothing

extractList_Section :: Maybe Node -> Maybe List_Section
extractList_Section (Just (List_SectionNode x@(List_Section _) _)) = Just x
extractList_Section _ = Nothing

extractList_Paragraph :: Maybe Node -> Maybe List_Paragraph
extractList_Paragraph (Just (List_ParagraphNode x@(List_Paragraph _) _)) = Just x
extractList_Paragraph _ = Nothing

extractList_Subsection :: Maybe Node -> Maybe List_Subsection
extractList_Subsection (Just (List_SubsectionNode x@(List_Subsection _) _)) = Just x
extractList_Subsection _ = Nothing

extractList_Subsubsection :: Maybe Node -> Maybe List_Subsubsection
extractList_Subsubsection (Just (List_SubsubsectionNode x@(List_Subsubsection _) _)) = Just x
extractList_Subsubsection _ = Nothing

extractList_Word :: Maybe Node -> Maybe List_Word
extractList_Word (Just (List_WordNode x@(List_Word _) _)) = Just x
extractList_Word _ = Nothing

extractList_Vertex :: Maybe Node -> Maybe List_Vertex
extractList_Vertex (Just (List_VertexNode x@(List_Vertex _) _)) = Just x
extractList_Vertex _ = Nothing

extractList_Edge :: Maybe Node -> Maybe List_Edge
extractList_Edge (Just (List_EdgeNode x@(List_Edge _) _)) = Just x
extractList_Edge _ = Nothing




--------------------------------------------------------------------------
-- default functions                                                    --
--------------------------------------------------------------------------

defaultRootEnr :: EnrichedDoc
defaultRootEnr = RootEnr hole hole

defaultDummy :: Dummy
defaultDummy = Dummy hole hole

defaultRoot :: Root
defaultRoot = Root hole hole hole

defaultSection :: Section
defaultSection = Section hole hole hole

defaultSubsection :: Subsection
defaultSubsection = Subsection hole hole hole

defaultSubsubsection :: Subsubsection
defaultSubsubsection = Subsubsection hole hole

defaultParagraph :: Paragraph
defaultParagraph = Paragraph hole

defaultSubgraphPara :: Paragraph
defaultSubgraphPara = SubgraphPara hole

defaultWord :: Word
defaultWord = Word hole

defaultNodeRef :: Word
defaultNodeRef = NodeRef hole

defaultLabel :: Word
defaultLabel = Label hole

defaultLabelRef :: Word
defaultLabelRef = LabelRef hole

defaultGraph :: Graph
defaultGraph = Graph hole hole hole

defaultVertex :: Vertex
defaultVertex = Vertex hole hole hole hole hole

defaultCircle :: Shape
defaultCircle = Circle

defaultSquare :: Shape
defaultSquare = Square

defaultEdge :: Edge
defaultEdge = Edge hole hole

defaultSubgraph :: Subgraph
defaultSubgraph = Subgraph hole hole hole

defaultDirty :: Dirty
defaultDirty = Dirty

defaultClean :: Dirty
defaultClean = Clean

defaultList_Section :: List_Section
defaultList_Section = List_Section Nil_Section

defaultList_Paragraph :: List_Paragraph
defaultList_Paragraph = List_Paragraph Nil_Paragraph

defaultList_Subsection :: List_Subsection
defaultList_Subsection = List_Subsection Nil_Subsection

defaultList_Subsubsection :: List_Subsubsection
defaultList_Subsubsection = List_Subsubsection Nil_Subsubsection

defaultList_Word :: List_Word
defaultList_Word = List_Word Nil_Word

defaultList_Vertex :: List_Vertex
defaultList_Vertex = List_Vertex Nil_Vertex

defaultList_Edge :: List_Edge
defaultList_Edge = List_Edge Nil_Edge




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

reuse0 :: (r) ->
          
          r
reuse0 f =
  f

reuse1 :: (a0 -> r) ->
          a0 -> 
          Maybe a0 -> r
reuse1 f a0 ma0 =
  f (maybe a0 id ma0)

reuse2 :: (a0 -> a1 -> r) ->
          a0 -> a1 -> 
          Maybe a0 -> Maybe a1 -> r
reuse2 f a0 a1 ma0 ma1 =
  f (maybe a0 id ma0) (maybe a1 id ma1)

reuse3 :: (a0 -> a1 -> a2 -> r) ->
          a0 -> a1 -> a2 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> r
reuse3 f a0 a1 a2 ma0 ma1 ma2 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2)

reuse4 :: (a0 -> a1 -> a2 -> a3 -> r) ->
          a0 -> a1 -> a2 -> a3 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> r
reuse4 f a0 a1 a2 a3 ma0 ma1 ma2 ma3 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3)

reuse5 :: (a0 -> a1 -> a2 -> a3 -> a4 -> r) ->
          a0 -> a1 -> a2 -> a3 -> a4 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> r
reuse5 f a0 a1 a2 a3 a4 ma0 ma1 ma2 ma3 ma4 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4)



