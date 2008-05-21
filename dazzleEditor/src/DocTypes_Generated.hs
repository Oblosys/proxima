module DocTypes_Generated where

import Common.CommonTypes hiding (Dirty (..))
import Evaluation.DocTypes

import Presentation.PresTypes hiding (Edge)
import List
import Char

data UserToken = KeyTk String
               | WordTk 
               | NodeRefTk
               | LabelTk
               | LabelRefTk
               | IntTk deriving (Show, Eq, Ord)

type List_Int = [Int]

----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- Proxima data type                                                    --
--------------------------------------------------------------------------

data EnrichedDoc = RootEnr Root
                 | HoleEnrichedDoc
                 | ParseErrEnrichedDoc (ParseError Document Node ClipDoc UserToken)
                     deriving Show

data Document = RootDoc Root
              | HoleDocument
              | ParseErrDocument (ParseError Document Node ClipDoc UserToken)
                  deriving Show

data Root = Root Graph List_Probtable String List_Section
          | HoleRoot
          | ParseErrRoot (ParseError Document Node ClipDoc UserToken)
              deriving Show

data Section = Section String List_Paragraph List_Subsection
             | HoleSection
             | ParseErrSection (ParseError Document Node ClipDoc UserToken)
                 deriving Show

data Subsection = Subsection String List_Paragraph List_Subsubsection
                | HoleSubsection
                | ParseErrSubsection (ParseError Document Node ClipDoc UserToken)
                    deriving Show

data Subsubsection = Subsubsection String List_Paragraph
                   | HoleSubsubsection
                   | ParseErrSubsubsection (ParseError Document Node ClipDoc UserToken)
                       deriving Show

data Paragraph = Paragraph List_Word
               | SubgraphPara Subgraph
               | ProbtablePara Probtable
               | HoleParagraph
               | ParseErrParagraph (ParseError Document Node ClipDoc UserToken)
                   deriving Show

data Word = Word String
          | NodeRef String
          | Label String
          | LabelRef String
          | HoleWord
          | ParseErrWord (ParseError Document Node ClipDoc UserToken)
              deriving Show

data Graph = Graph Dirty List_Vertex List_Edge
           | HoleGraph
           | ParseErrGraph (ParseError Document Node ClipDoc UserToken)
               deriving Show

data Vertex = Vertex String Shape Int Int Int
            | HoleVertex
            | ParseErrVertex (ParseError Document Node ClipDoc UserToken)
                deriving Show

data Shape = Circle
           | Square
           | HoleShape
           | ParseErrShape (ParseError Document Node ClipDoc UserToken)
               deriving Show

data Edge = Edge Int Int
          | HoleEdge
          | ParseErrEdge (ParseError Document Node ClipDoc UserToken)
              deriving Show

data Subgraph = Subgraph Dirty List_Vertex List_Edge
              | HoleSubgraph
              | ParseErrSubgraph (ParseError Document Node ClipDoc UserToken)
                  deriving Show

data Dirty = Dirty
           | Clean
           | HoleDirty
           | ParseErrDirty (ParseError Document Node ClipDoc UserToken)
               deriving Show

data Probtable = Probtable Int List_Value Table
               | HoleProbtable
               | ParseErrProbtable (ParseError Document Node ClipDoc UserToken)
                   deriving Show

data Value = Value String
           | HoleValue
           | ParseErrValue (ParseError Document Node ClipDoc UserToken)
               deriving Show

data Table = Table List_Axis List_Probability
           | HoleTable
           | ParseErrTable (ParseError Document Node ClipDoc UserToken)
               deriving Show

data Axis = Axis List_Value
          | HoleAxis
          | ParseErrAxis (ParseError Document Node ClipDoc UserToken)
              deriving Show

data Probability = Probability String
                 | HoleProbability
                 | ParseErrProbability (ParseError Document Node ClipDoc UserToken)
                     deriving Show

data List_Probtable = List_Probtable ConsList_Probtable
                    | HoleList_Probtable
                    | ParseErrList_Probtable (ParseError Document Node ClipDoc UserToken)
                        deriving Show

data List_Section = List_Section ConsList_Section
                  | HoleList_Section
                  | ParseErrList_Section (ParseError Document Node ClipDoc UserToken)
                      deriving Show

data List_Paragraph = List_Paragraph ConsList_Paragraph
                    | HoleList_Paragraph
                    | ParseErrList_Paragraph (ParseError Document Node ClipDoc UserToken)
                        deriving Show

data List_Subsection = List_Subsection ConsList_Subsection
                     | HoleList_Subsection
                     | ParseErrList_Subsection (ParseError Document Node ClipDoc UserToken)
                         deriving Show

data List_Subsubsection = List_Subsubsection ConsList_Subsubsection
                        | HoleList_Subsubsection
                        | ParseErrList_Subsubsection (ParseError Document Node ClipDoc UserToken)
                            deriving Show

data List_Word = List_Word ConsList_Word
               | HoleList_Word
               | ParseErrList_Word (ParseError Document Node ClipDoc UserToken)
                   deriving Show

data List_Vertex = List_Vertex ConsList_Vertex
                 | HoleList_Vertex
                 | ParseErrList_Vertex (ParseError Document Node ClipDoc UserToken)
                     deriving Show

data List_Edge = List_Edge ConsList_Edge
               | HoleList_Edge
               | ParseErrList_Edge (ParseError Document Node ClipDoc UserToken)
                   deriving Show

data List_Value = List_Value ConsList_Value
                | HoleList_Value
                | ParseErrList_Value (ParseError Document Node ClipDoc UserToken)
                    deriving Show

data List_Axis = List_Axis ConsList_Axis
               | HoleList_Axis
               | ParseErrList_Axis (ParseError Document Node ClipDoc UserToken)
                   deriving Show

data List_Probability = List_Probability ConsList_Probability
                      | HoleList_Probability
                      | ParseErrList_Probability (ParseError Document Node ClipDoc UserToken)
                          deriving Show

data ConsList_Probtable = Cons_Probtable Probtable ConsList_Probtable
                        | Nil_Probtable
                            deriving Show

data ConsList_Section = Cons_Section Section ConsList_Section
                      | Nil_Section
                          deriving Show

data ConsList_Paragraph = Cons_Paragraph Paragraph ConsList_Paragraph
                        | Nil_Paragraph
                            deriving Show

data ConsList_Subsection = Cons_Subsection Subsection ConsList_Subsection
                         | Nil_Subsection
                             deriving Show

data ConsList_Subsubsection = Cons_Subsubsection Subsubsection ConsList_Subsubsection
                            | Nil_Subsubsection
                                deriving Show

data ConsList_Word = Cons_Word Word ConsList_Word
                   | Nil_Word
                       deriving Show

data ConsList_Vertex = Cons_Vertex Vertex ConsList_Vertex
                     | Nil_Vertex
                         deriving Show

data ConsList_Edge = Cons_Edge Edge ConsList_Edge
                   | Nil_Edge
                       deriving Show

data ConsList_Value = Cons_Value Value ConsList_Value
                    | Nil_Value
                        deriving Show

data ConsList_Axis = Cons_Axis Axis ConsList_Axis
                   | Nil_Axis
                       deriving Show

data ConsList_Probability = Cons_Probability Probability ConsList_Probability
                          | Nil_Probability
                              deriving Show




--------------------------------------------------------------------------
-- ClipDoc                                                              --
--------------------------------------------------------------------------

data ClipDoc = Clip_EnrichedDoc EnrichedDoc
             | Clip_Document Document
             | Clip_Root Root
             | Clip_Section Section
             | Clip_Subsection Subsection
             | Clip_Subsubsection Subsubsection
             | Clip_Paragraph Paragraph
             | Clip_Word Word
             | Clip_Graph Graph
             | Clip_Vertex Vertex
             | Clip_Shape Shape
             | Clip_Edge Edge
             | Clip_Subgraph Subgraph
             | Clip_Dirty Dirty
             | Clip_Probtable Probtable
             | Clip_Value Value
             | Clip_Table Table
             | Clip_Axis Axis
             | Clip_Probability Probability
             | Clip_List_Probtable List_Probtable
             | Clip_List_Section List_Section
             | Clip_List_Paragraph List_Paragraph
             | Clip_List_Subsection List_Subsection
             | Clip_List_Subsubsection List_Subsubsection
             | Clip_List_Word List_Word
             | Clip_List_Vertex List_Vertex
             | Clip_List_Edge List_Edge
             | Clip_List_Value List_Value
             | Clip_List_Axis List_Axis
             | Clip_List_Probability List_Probability
             | Clip_Bool Bool
             | Clip_Int Int
             | Clip_String String
             | Clip_Float Float
             | Clip_Nothing deriving Show



--------------------------------------------------------------------------
-- Node                                                                 --
--------------------------------------------------------------------------

data Node = NoNode
          | Node_RootEnr EnrichedDoc Path
          | Node_HoleEnrichedDoc EnrichedDoc Path
          | Node_ParseErrEnrichedDoc EnrichedDoc Path
          | Node_RootDoc Document Path
          | Node_HoleDocument Document Path
          | Node_ParseErrDocument Document Path
          | Node_Root Root Path
          | Node_HoleRoot Root Path
          | Node_ParseErrRoot Root Path
          | Node_Section Section Path
          | Node_HoleSection Section Path
          | Node_ParseErrSection Section Path
          | Node_Subsection Subsection Path
          | Node_HoleSubsection Subsection Path
          | Node_ParseErrSubsection Subsection Path
          | Node_Subsubsection Subsubsection Path
          | Node_HoleSubsubsection Subsubsection Path
          | Node_ParseErrSubsubsection Subsubsection Path
          | Node_Paragraph Paragraph Path
          | Node_SubgraphPara Paragraph Path
          | Node_ProbtablePara Paragraph Path
          | Node_HoleParagraph Paragraph Path
          | Node_ParseErrParagraph Paragraph Path
          | Node_Word Word Path
          | Node_NodeRef Word Path
          | Node_Label Word Path
          | Node_LabelRef Word Path
          | Node_HoleWord Word Path
          | Node_ParseErrWord Word Path
          | Node_Graph Graph Path
          | Node_HoleGraph Graph Path
          | Node_ParseErrGraph Graph Path
          | Node_Vertex Vertex Path
          | Node_HoleVertex Vertex Path
          | Node_ParseErrVertex Vertex Path
          | Node_Circle Shape Path
          | Node_Square Shape Path
          | Node_HoleShape Shape Path
          | Node_ParseErrShape Shape Path
          | Node_Edge Edge Path
          | Node_HoleEdge Edge Path
          | Node_ParseErrEdge Edge Path
          | Node_Subgraph Subgraph Path
          | Node_HoleSubgraph Subgraph Path
          | Node_ParseErrSubgraph Subgraph Path
          | Node_Dirty Dirty Path
          | Node_Clean Dirty Path
          | Node_HoleDirty Dirty Path
          | Node_ParseErrDirty Dirty Path
          | Node_Probtable Probtable Path
          | Node_HoleProbtable Probtable Path
          | Node_ParseErrProbtable Probtable Path
          | Node_Value Value Path
          | Node_HoleValue Value Path
          | Node_ParseErrValue Value Path
          | Node_Table Table Path
          | Node_HoleTable Table Path
          | Node_ParseErrTable Table Path
          | Node_Axis Axis Path
          | Node_HoleAxis Axis Path
          | Node_ParseErrAxis Axis Path
          | Node_Probability Probability Path
          | Node_HoleProbability Probability Path
          | Node_ParseErrProbability Probability Path
          | Node_List_Probtable List_Probtable Path
          | Node_HoleList_Probtable List_Probtable Path
          | Node_ParseErrList_Probtable List_Probtable Path
          | Node_List_Section List_Section Path
          | Node_HoleList_Section List_Section Path
          | Node_ParseErrList_Section List_Section Path
          | Node_List_Paragraph List_Paragraph Path
          | Node_HoleList_Paragraph List_Paragraph Path
          | Node_ParseErrList_Paragraph List_Paragraph Path
          | Node_List_Subsection List_Subsection Path
          | Node_HoleList_Subsection List_Subsection Path
          | Node_ParseErrList_Subsection List_Subsection Path
          | Node_List_Subsubsection List_Subsubsection Path
          | Node_HoleList_Subsubsection List_Subsubsection Path
          | Node_ParseErrList_Subsubsection List_Subsubsection Path
          | Node_List_Word List_Word Path
          | Node_HoleList_Word List_Word Path
          | Node_ParseErrList_Word List_Word Path
          | Node_List_Vertex List_Vertex Path
          | Node_HoleList_Vertex List_Vertex Path
          | Node_ParseErrList_Vertex List_Vertex Path
          | Node_List_Edge List_Edge Path
          | Node_HoleList_Edge List_Edge Path
          | Node_ParseErrList_Edge List_Edge Path
          | Node_List_Value List_Value Path
          | Node_HoleList_Value List_Value Path
          | Node_ParseErrList_Value List_Value Path
          | Node_List_Axis List_Axis Path
          | Node_HoleList_Axis List_Axis Path
          | Node_ParseErrList_Axis List_Axis Path
          | Node_List_Probability List_Probability Path
          | Node_HoleList_Probability List_Probability Path
          | Node_ParseErrList_Probability List_Probability Path



--------------------------------------------------------------------------
-- Show instance for Node                                               --
--------------------------------------------------------------------------

instance Show Node where
  show NoNode = "NoNode"
  show (Node_RootEnr _ _) = "Node_RootEnr" 
  show (Node_HoleEnrichedDoc _ _) = "Node_HoleEnrichedDoc" 
  show (Node_ParseErrEnrichedDoc _ _) = "Node_ParseErrEnrichedDoc" 
  show (Node_RootDoc _ _) = "Node_RootDoc" 
  show (Node_HoleDocument _ _) = "Node_HoleDocument" 
  show (Node_ParseErrDocument _ _) = "Node_ParseErrDocument" 
  show (Node_Root _ _) = "Node_Root" 
  show (Node_HoleRoot _ _) = "Node_HoleRoot" 
  show (Node_ParseErrRoot _ _) = "Node_ParseErrRoot" 
  show (Node_Section _ _) = "Node_Section" 
  show (Node_HoleSection _ _) = "Node_HoleSection" 
  show (Node_ParseErrSection _ _) = "Node_ParseErrSection" 
  show (Node_Subsection _ _) = "Node_Subsection" 
  show (Node_HoleSubsection _ _) = "Node_HoleSubsection" 
  show (Node_ParseErrSubsection _ _) = "Node_ParseErrSubsection" 
  show (Node_Subsubsection _ _) = "Node_Subsubsection" 
  show (Node_HoleSubsubsection _ _) = "Node_HoleSubsubsection" 
  show (Node_ParseErrSubsubsection _ _) = "Node_ParseErrSubsubsection" 
  show (Node_Paragraph _ _) = "Node_Paragraph" 
  show (Node_SubgraphPara _ _) = "Node_SubgraphPara" 
  show (Node_ProbtablePara _ _) = "Node_ProbtablePara" 
  show (Node_HoleParagraph _ _) = "Node_HoleParagraph" 
  show (Node_ParseErrParagraph _ _) = "Node_ParseErrParagraph" 
  show (Node_Word _ _) = "Node_Word" 
  show (Node_NodeRef _ _) = "Node_NodeRef" 
  show (Node_Label _ _) = "Node_Label" 
  show (Node_LabelRef _ _) = "Node_LabelRef" 
  show (Node_HoleWord _ _) = "Node_HoleWord" 
  show (Node_ParseErrWord _ _) = "Node_ParseErrWord" 
  show (Node_Graph _ _) = "Node_Graph" 
  show (Node_HoleGraph _ _) = "Node_HoleGraph" 
  show (Node_ParseErrGraph _ _) = "Node_ParseErrGraph" 
  show (Node_Vertex _ _) = "Node_Vertex" 
  show (Node_HoleVertex _ _) = "Node_HoleVertex" 
  show (Node_ParseErrVertex _ _) = "Node_ParseErrVertex" 
  show (Node_Circle _ _) = "Node_Circle" 
  show (Node_Square _ _) = "Node_Square" 
  show (Node_HoleShape _ _) = "Node_HoleShape" 
  show (Node_ParseErrShape _ _) = "Node_ParseErrShape" 
  show (Node_Edge _ _) = "Node_Edge" 
  show (Node_HoleEdge _ _) = "Node_HoleEdge" 
  show (Node_ParseErrEdge _ _) = "Node_ParseErrEdge" 
  show (Node_Subgraph _ _) = "Node_Subgraph" 
  show (Node_HoleSubgraph _ _) = "Node_HoleSubgraph" 
  show (Node_ParseErrSubgraph _ _) = "Node_ParseErrSubgraph" 
  show (Node_Dirty _ _) = "Node_Dirty" 
  show (Node_Clean _ _) = "Node_Clean" 
  show (Node_HoleDirty _ _) = "Node_HoleDirty" 
  show (Node_ParseErrDirty _ _) = "Node_ParseErrDirty" 
  show (Node_Probtable _ _) = "Node_Probtable" 
  show (Node_HoleProbtable _ _) = "Node_HoleProbtable" 
  show (Node_ParseErrProbtable _ _) = "Node_ParseErrProbtable" 
  show (Node_Value _ _) = "Node_Value" 
  show (Node_HoleValue _ _) = "Node_HoleValue" 
  show (Node_ParseErrValue _ _) = "Node_ParseErrValue" 
  show (Node_Table _ _) = "Node_Table" 
  show (Node_HoleTable _ _) = "Node_HoleTable" 
  show (Node_ParseErrTable _ _) = "Node_ParseErrTable" 
  show (Node_Axis _ _) = "Node_Axis" 
  show (Node_HoleAxis _ _) = "Node_HoleAxis" 
  show (Node_ParseErrAxis _ _) = "Node_ParseErrAxis" 
  show (Node_Probability _ _) = "Node_Probability" 
  show (Node_HoleProbability _ _) = "Node_HoleProbability" 
  show (Node_ParseErrProbability _ _) = "Node_ParseErrProbability" 
  show (Node_List_Probtable _ _) = "Node_List_Probtable" 
  show (Node_HoleList_Probtable _ _) = "Node_HoleList_Probtable" 
  show (Node_ParseErrList_Probtable _ _) = "Node_ParseErrList_Probtable" 
  show (Node_List_Section _ _) = "Node_List_Section" 
  show (Node_HoleList_Section _ _) = "Node_HoleList_Section" 
  show (Node_ParseErrList_Section _ _) = "Node_ParseErrList_Section" 
  show (Node_List_Paragraph _ _) = "Node_List_Paragraph" 
  show (Node_HoleList_Paragraph _ _) = "Node_HoleList_Paragraph" 
  show (Node_ParseErrList_Paragraph _ _) = "Node_ParseErrList_Paragraph" 
  show (Node_List_Subsection _ _) = "Node_List_Subsection" 
  show (Node_HoleList_Subsection _ _) = "Node_HoleList_Subsection" 
  show (Node_ParseErrList_Subsection _ _) = "Node_ParseErrList_Subsection" 
  show (Node_List_Subsubsection _ _) = "Node_List_Subsubsection" 
  show (Node_HoleList_Subsubsection _ _) = "Node_HoleList_Subsubsection" 
  show (Node_ParseErrList_Subsubsection _ _) = "Node_ParseErrList_Subsubsection" 
  show (Node_List_Word _ _) = "Node_List_Word" 
  show (Node_HoleList_Word _ _) = "Node_HoleList_Word" 
  show (Node_ParseErrList_Word _ _) = "Node_ParseErrList_Word" 
  show (Node_List_Vertex _ _) = "Node_List_Vertex" 
  show (Node_HoleList_Vertex _ _) = "Node_HoleList_Vertex" 
  show (Node_ParseErrList_Vertex _ _) = "Node_ParseErrList_Vertex" 
  show (Node_List_Edge _ _) = "Node_List_Edge" 
  show (Node_HoleList_Edge _ _) = "Node_HoleList_Edge" 
  show (Node_ParseErrList_Edge _ _) = "Node_ParseErrList_Edge" 
  show (Node_List_Value _ _) = "Node_List_Value" 
  show (Node_HoleList_Value _ _) = "Node_HoleList_Value" 
  show (Node_ParseErrList_Value _ _) = "Node_ParseErrList_Value" 
  show (Node_List_Axis _ _) = "Node_List_Axis" 
  show (Node_HoleList_Axis _ _) = "Node_HoleList_Axis" 
  show (Node_ParseErrList_Axis _ _) = "Node_ParseErrList_Axis" 
  show (Node_List_Probability _ _) = "Node_List_Probability" 
  show (Node_HoleList_Probability _ _) = "Node_HoleList_Probability" 
  show (Node_ParseErrList_Probability _ _) = "Node_ParseErrList_Probability" 


