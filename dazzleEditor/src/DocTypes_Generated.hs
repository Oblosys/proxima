module DocTypes_Generated where

import Common.CommonTypes hiding (Dirty (..))
import Evaluation.DocTypes

import Presentation.PresTypes
import List
import Char


data Document = RootDoc Root 
              | HoleDocument
              | ParseErrDocument (Presentation Document Node ClipDoc UserToken)
                 deriving Show

data UserToken = KeyTk String
               | WordTk 
               | NodeRefTk
               | LabelTk
               | LabelRefTk deriving (Show, Eq, Ord)


----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

{- ------------------------------------

 generated part

-------------------------------------- -}


-- Generated Types --



data EnrichedDoc = RootEnr Root Document 
                 | HoleEnrichedDoc
                 | ParseErrEnrichedDoc (Presentation Document Node ClipDoc UserToken)
                    deriving Show


data Dummy = Dummy Dummy Bool 
           | HoleDummy
           | ParseErrDummy (Presentation Document Node ClipDoc UserToken)
              deriving Show


data Root = Root Graph String List_Section 
          | HoleRoot
          | ParseErrRoot (Presentation Document Node ClipDoc UserToken)
             deriving Show


data Section = Section String List_Paragraph List_Subsection 
             | HoleSection
             | ParseErrSection (Presentation Document Node ClipDoc UserToken)
                deriving Show


data Subsection = Subsection String List_Paragraph List_Subsubsection 
                | HoleSubsection
                | ParseErrSubsection (Presentation Document Node ClipDoc UserToken)
                   deriving Show


data Subsubsection = Subsubsection String List_Paragraph 
                   | HoleSubsubsection
                   | ParseErrSubsubsection (Presentation Document Node ClipDoc UserToken)
                      deriving Show


data Paragraph = Paragraph List_Word 
               | SubgraphPara Subgraph 
               | HoleParagraph
               | ParseErrParagraph (Presentation Document Node ClipDoc UserToken)
                  deriving Show


data Word = Word String 
          | NodeRef String 
          | Label String 
          | LabelRef String 
          | HoleWord
          | ParseErrWord (Presentation Document Node ClipDoc UserToken)
             deriving Show


data Graph = Graph Dirty List_Vertex List_Edge 
           | HoleGraph
           | ParseErrGraph (Presentation Document Node ClipDoc UserToken)
              deriving Show


data Vertex = Vertex String Shape Int Int Int 
            | HoleVertex
            | ParseErrVertex (Presentation Document Node ClipDoc UserToken)
               deriving Show


data Shape = Circle 
           | Square 
           | HoleShape
           | ParseErrShape (Presentation Document Node ClipDoc UserToken)
              deriving Show


data Edge = Edge Int Int 
          | HoleEdge
          | ParseErrEdge (Presentation Document Node ClipDoc UserToken)
             deriving Show


data Subgraph = Subgraph Dirty List_Vertex List_Edge 
              | HoleSubgraph
              | ParseErrSubgraph (Presentation Document Node ClipDoc UserToken)
                 deriving Show


data Dirty = Dirty 
           | Clean 
           | HoleDirty
           | ParseErrDirty (Presentation Document Node ClipDoc UserToken)
              deriving Show


data List_Section = List_Section ConsList_Section 
                  | HoleList_Section
                  | ParseErrList_Section (Presentation Document Node ClipDoc UserToken)
                     deriving Show


data ConsList_Section = Cons_Section Section ConsList_Section 
                      | Nil_Section 
                         deriving Show


data List_Paragraph = List_Paragraph ConsList_Paragraph 
                    | HoleList_Paragraph
                    | ParseErrList_Paragraph (Presentation Document Node ClipDoc UserToken)
                       deriving Show


data ConsList_Paragraph = Cons_Paragraph Paragraph ConsList_Paragraph 
                        | Nil_Paragraph 
                           deriving Show


data List_Subsection = List_Subsection ConsList_Subsection 
                     | HoleList_Subsection
                     | ParseErrList_Subsection (Presentation Document Node ClipDoc UserToken)
                        deriving Show


data ConsList_Subsection = Cons_Subsection Subsection ConsList_Subsection 
                         | Nil_Subsection 
                            deriving Show


data List_Subsubsection = List_Subsubsection ConsList_Subsubsection 
                        | HoleList_Subsubsection
                        | ParseErrList_Subsubsection (Presentation Document Node ClipDoc UserToken)
                           deriving Show


data ConsList_Subsubsection = Cons_Subsubsection Subsubsection ConsList_Subsubsection 
                            | Nil_Subsubsection 
                               deriving Show


data List_Word = List_Word ConsList_Word 
               | HoleList_Word
               | ParseErrList_Word (Presentation Document Node ClipDoc UserToken)
                  deriving Show


data ConsList_Word = Cons_Word Word ConsList_Word 
                   | Nil_Word 
                      deriving Show


data List_Vertex = List_Vertex ConsList_Vertex 
                 | HoleList_Vertex
                 | ParseErrList_Vertex (Presentation Document Node ClipDoc UserToken)
                    deriving Show


data ConsList_Vertex = Cons_Vertex Vertex ConsList_Vertex 
                     | Nil_Vertex 
                        deriving Show


data List_Edge = List_Edge ConsList_Edge 
               | HoleList_Edge
               | ParseErrList_Edge (Presentation Document Node ClipDoc UserToken)
                  deriving Show


data ConsList_Edge = Cons_Edge Edge ConsList_Edge 
                   | Nil_Edge 
                      deriving Show


-- Generated Types --

data ClipDoc = Clip_Root Root
             | Clip_Document Document
             | Clip_Dummy Dummy
             | Clip_Bool Bool
             | Clip_Graph Graph
             | Clip_String String
             | Clip_List_Section List_Section
             | Clip_List_Paragraph List_Paragraph
             | Clip_List_Subsection List_Subsection
             | Clip_List_Subsubsection List_Subsubsection
             | Clip_List_Word List_Word
             | Clip_Subgraph Subgraph
             | Clip_Dirty Dirty
             | Clip_List_Vertex List_Vertex
             | Clip_List_Edge List_Edge
             | Clip_Shape Shape
             | Clip_Int Int
             | Clip_Section Section
             
             | Clip_Paragraph Paragraph
             
             | Clip_Subsection Subsection
             
             | Clip_Subsubsection Subsubsection
             
             | Clip_Word Word
             
             | Clip_Vertex Vertex
             
             | Clip_Edge Edge
             
             | Clip_Nothing deriving Show




data Node = NoNode 
          | RootDocNode Document Path
          | HoleDocumentNode Document Path
          | RootEnrNode EnrichedDoc Path 
          | HoleEnrichedDocNode EnrichedDoc Path 
          | DummyNode Dummy Path 
          | HoleDummyNode Dummy Path 
          | RootNode Root Path 
          | HoleRootNode Root Path 
          | SectionNode Section Path 
          | HoleSectionNode Section Path 
          | SubsectionNode Subsection Path 
          | HoleSubsectionNode Subsection Path 
          | SubsubsectionNode Subsubsection Path 
          | HoleSubsubsectionNode Subsubsection Path 
          | ParagraphNode Paragraph Path 
          | SubgraphParaNode Paragraph Path 
          | HoleParagraphNode Paragraph Path 
          | WordNode Word Path 
          | NodeRefNode Word Path 
          | LabelNode Word Path 
          | LabelRefNode Word Path 
          | HoleWordNode Word Path 
          | GraphNode Graph Path 
          | HoleGraphNode Graph Path 
          | VertexNode Vertex Path 
          | HoleVertexNode Vertex Path 
          | CircleNode Shape Path 
          | SquareNode Shape Path 
          | HoleShapeNode Shape Path 
          | EdgeNode Edge Path 
          | HoleEdgeNode Edge Path 
          | SubgraphNode Subgraph Path 
          | HoleSubgraphNode Subgraph Path 
          | DirtyNode Dirty Path 
          | CleanNode Dirty Path 
          | HoleDirtyNode Dirty Path 
          | List_SectionNode List_Section Path 
          | HoleList_SectionNode List_Section Path 
          | List_ParagraphNode List_Paragraph Path 
          | HoleList_ParagraphNode List_Paragraph Path 
          | List_SubsectionNode List_Subsection Path 
          | HoleList_SubsectionNode List_Subsection Path 
          | List_SubsubsectionNode List_Subsubsection Path 
          | HoleList_SubsubsectionNode List_Subsubsection Path 
          | List_WordNode List_Word Path 
          | HoleList_WordNode List_Word Path 
          | List_VertexNode List_Vertex Path 
          | HoleList_VertexNode List_Vertex Path 
          | List_EdgeNode List_Edge Path 
          | HoleList_EdgeNode List_Edge Path 



instance Show Node where
  show NoNode            = "NoNode"
  show (RootDocNode _ _) = "RootDocNode"
  show (HoleDocumentNode _ _) = "HoleDocumentNode"
  show (RootEnrNode _ _)  = "RootEnrNode"
  show (HoleEnrichedDocNode _ _)  = "HoleEnrichedDocNode"
  show (DummyNode _ _)  = "DummyNode"
  show (HoleDummyNode _ _)  = "HoleDummyNode"
  show (RootNode _ _)  = "RootNode"
  show (HoleRootNode _ _)  = "HoleRootNode"
  show (SectionNode _ _)  = "SectionNode"
  show (HoleSectionNode _ _)  = "HoleSectionNode"
  show (SubsectionNode _ _)  = "SubsectionNode"
  show (HoleSubsectionNode _ _)  = "HoleSubsectionNode"
  show (SubsubsectionNode _ _)  = "SubsubsectionNode"
  show (HoleSubsubsectionNode _ _)  = "HoleSubsubsectionNode"
  show (ParagraphNode _ _)  = "ParagraphNode"
  show (SubgraphParaNode _ _)  = "SubgraphParaNode"
  show (HoleParagraphNode _ _)  = "HoleParagraphNode"
  show (WordNode _ _)  = "WordNode"
  show (NodeRefNode _ _)  = "NodeRefNode"
  show (LabelNode _ _)  = "LabelNode"
  show (LabelRefNode _ _)  = "LabelRefNode"
  show (HoleWordNode _ _)  = "HoleWordNode"
  show (GraphNode _ _)  = "GraphNode"
  show (HoleGraphNode _ _)  = "HoleGraphNode"
  show (VertexNode _ _)  = "VertexNode"
  show (HoleVertexNode _ _)  = "HoleVertexNode"
  show (CircleNode _ _)  = "CircleNode"
  show (SquareNode _ _)  = "SquareNode"
  show (HoleShapeNode _ _)  = "HoleShapeNode"
  show (EdgeNode _ _)  = "EdgeNode"
  show (HoleEdgeNode _ _)  = "HoleEdgeNode"
  show (SubgraphNode _ _)  = "SubgraphNode"
  show (HoleSubgraphNode _ _)  = "HoleSubgraphNode"
  show (DirtyNode _ _)  = "DirtyNode"
  show (CleanNode _ _)  = "CleanNode"
  show (HoleDirtyNode _ _)  = "HoleDirtyNode"
  show (List_SectionNode _ _)  = "List_SectionNode"
  show (HoleList_SectionNode _ _)  = "HoleList_SectionNode"
  show (List_ParagraphNode _ _)  = "List_ParagraphNode"
  show (HoleList_ParagraphNode _ _)  = "HoleList_ParagraphNode"
  show (List_SubsectionNode _ _)  = "List_SubsectionNode"
  show (HoleList_SubsectionNode _ _)  = "HoleList_SubsectionNode"
  show (List_SubsubsectionNode _ _)  = "List_SubsubsectionNode"
  show (HoleList_SubsubsectionNode _ _)  = "HoleList_SubsubsectionNode"
  show (List_WordNode _ _)  = "List_WordNode"
  show (HoleList_WordNode _ _)  = "HoleList_WordNode"
  show (List_VertexNode _ _)  = "List_VertexNode"
  show (HoleList_VertexNode _ _)  = "HoleList_VertexNode"
  show (List_EdgeNode _ _)  = "List_EdgeNode"
  show (HoleList_EdgeNode _ _)  = "HoleList_EdgeNode"
