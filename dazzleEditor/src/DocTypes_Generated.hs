module DocTypes_Generated where

import CommonTypes hiding (Dirty (..))
import DocTypes

import PresTypes
import List
import Char


data Document = RootDoc IDD Root 
              | HoleDocument
              | ParseErrDocument (Presentation Document Node ClipDoc)
                 deriving Show


----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

{- ------------------------------------

 generated part

-------------------------------------- -}


-- Generated Types --



data EnrichedDoc = RootEnr IDD Root Document 
                 | HoleEnrichedDoc
                 | ParseErrEnrichedDoc (Presentation Document Node ClipDoc)
                    deriving Show


data String_ = String_ IDD String 
             | HoleString_
             | ParseErrString_ (Presentation Document Node ClipDoc)
                deriving Show


data Bool_ = Bool_ IDD Bool 
           | HoleBool_
           | ParseErrBool_ (Presentation Document Node ClipDoc)
              deriving Show


data Int_ = Int_ IDD Int 
          | HoleInt_
          | ParseErrInt_ (Presentation Document Node ClipDoc)
             deriving Show


data Dummy = Dummy IDD List_Dummy String_ Bool_ Int_ 
           | HoleDummy
           | ParseErrDummy (Presentation Document Node ClipDoc)
              deriving Show


data Root = Root IDD Graph String_ List_Section 
          | HoleRoot
          | ParseErrRoot (Presentation Document Node ClipDoc)
             deriving Show


data Section = Section IDD String_ List_Paragraph List_Subsection 
             | HoleSection
             | ParseErrSection (Presentation Document Node ClipDoc)
                deriving Show


data Subsection = Subsection IDD String_ List_Paragraph List_Subsubsection 
                | HoleSubsection
                | ParseErrSubsection (Presentation Document Node ClipDoc)
                   deriving Show


data Subsubsection = Subsubsection IDD String_ List_Paragraph 
                   | HoleSubsubsection
                   | ParseErrSubsubsection (Presentation Document Node ClipDoc)
                      deriving Show


data Paragraph = Paragraph IDD List_Word 
               | SubgraphPara IDD Subgraph 
               | HoleParagraph
               | ParseErrParagraph (Presentation Document Node ClipDoc)
                  deriving Show


data Word = Word IDD String_ 
          | HoleWord
          | ParseErrWord (Presentation Document Node ClipDoc)
             deriving Show


data Graph = Graph IDD Dirty List_Vertex List_Edge 
           | HoleGraph
           | ParseErrGraph (Presentation Document Node ClipDoc)
              deriving Show


data Vertex = Vertex IDD String_ Shape Int_ Int_ Int_ 
            | HoleVertex
            | ParseErrVertex (Presentation Document Node ClipDoc)
               deriving Show


data Shape = Circle IDD 
           | Square IDD 
           | HoleShape
           | ParseErrShape (Presentation Document Node ClipDoc)
              deriving Show


data Edge = Edge IDD Int_ Int_ 
          | HoleEdge
          | ParseErrEdge (Presentation Document Node ClipDoc)
             deriving Show


data Subgraph = Subgraph IDD Dirty List_Vertex List_Edge 
              | HoleSubgraph
              | ParseErrSubgraph (Presentation Document Node ClipDoc)
                 deriving Show


data Dirty = Dirty IDD 
           | Clean IDD 
           | HoleDirty
           | ParseErrDirty (Presentation Document Node ClipDoc)
              deriving Show


data List_Dummy = List_Dummy IDD ConsList_Dummy 
                | HoleList_Dummy
                | ParseErrList_Dummy (Presentation Document Node ClipDoc)
                   deriving Show


data ConsList_Dummy = Cons_Dummy Dummy ConsList_Dummy 
                    | Nil_Dummy 
                       deriving Show


data List_Section = List_Section IDD ConsList_Section 
                  | HoleList_Section
                  | ParseErrList_Section (Presentation Document Node ClipDoc)
                     deriving Show


data ConsList_Section = Cons_Section Section ConsList_Section 
                      | Nil_Section 
                         deriving Show


data List_Paragraph = List_Paragraph IDD ConsList_Paragraph 
                    | HoleList_Paragraph
                    | ParseErrList_Paragraph (Presentation Document Node ClipDoc)
                       deriving Show


data ConsList_Paragraph = Cons_Paragraph Paragraph ConsList_Paragraph 
                        | Nil_Paragraph 
                           deriving Show


data List_Subsection = List_Subsection IDD ConsList_Subsection 
                     | HoleList_Subsection
                     | ParseErrList_Subsection (Presentation Document Node ClipDoc)
                        deriving Show


data ConsList_Subsection = Cons_Subsection Subsection ConsList_Subsection 
                         | Nil_Subsection 
                            deriving Show


data List_Subsubsection = List_Subsubsection IDD ConsList_Subsubsection 
                        | HoleList_Subsubsection
                        | ParseErrList_Subsubsection (Presentation Document Node ClipDoc)
                           deriving Show


data ConsList_Subsubsection = Cons_Subsubsection Subsubsection ConsList_Subsubsection 
                            | Nil_Subsubsection 
                               deriving Show


data List_Word = List_Word IDD ConsList_Word 
               | HoleList_Word
               | ParseErrList_Word (Presentation Document Node ClipDoc)
                  deriving Show


data ConsList_Word = Cons_Word Word ConsList_Word 
                   | Nil_Word 
                      deriving Show


data List_Vertex = List_Vertex IDD ConsList_Vertex 
                 | HoleList_Vertex
                 | ParseErrList_Vertex (Presentation Document Node ClipDoc)
                    deriving Show


data ConsList_Vertex = Cons_Vertex Vertex ConsList_Vertex 
                     | Nil_Vertex 
                        deriving Show


data List_Edge = List_Edge IDD ConsList_Edge 
               | HoleList_Edge
               | ParseErrList_Edge (Presentation Document Node ClipDoc)
                  deriving Show


data ConsList_Edge = Cons_Edge Edge ConsList_Edge 
                   | Nil_Edge 
                      deriving Show


-- Generated Types --

data ClipDoc = Clip_Root Root
             | Clip_Document Document
             | Clip_String String
             | Clip_Bool Bool
             | Clip_Int Int
             | Clip_List_Dummy List_Dummy
             | Clip_String_ String_
             | Clip_Bool_ Bool_
             | Clip_Int_ Int_
             | Clip_Graph Graph
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
             | Clip_Dummy Dummy
             
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
          | String_Node String_ Path 
          | HoleString_Node String_ Path 
          | Bool_Node Bool_ Path 
          | HoleBool_Node Bool_ Path 
          | Int_Node Int_ Path 
          | HoleInt_Node Int_ Path 
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
          | List_DummyNode List_Dummy Path 
          | HoleList_DummyNode List_Dummy Path 
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
  show (String_Node _ _)  = "String_Node"
  show (HoleString_Node _ _)  = "HoleString_Node"
  show (Bool_Node _ _)  = "Bool_Node"
  show (HoleBool_Node _ _)  = "HoleBool_Node"
  show (Int_Node _ _)  = "Int_Node"
  show (HoleInt_Node _ _)  = "HoleInt_Node"
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
  show (List_DummyNode _ _)  = "List_DummyNode"
  show (HoleList_DummyNode _ _)  = "HoleList_DummyNode"
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
