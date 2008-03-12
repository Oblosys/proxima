module DocumentEdit_Generated where

import Common.CommonTypes hiding (Dirty (..))
import Evaluation.DocTypes
import DocTypes_Generated
import DocUtils_Generated
import Evaluation.DocumentEdit
import Evaluation.DocUtils
import Presentation.PresTypes

----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- Clip instance                                                        --
--------------------------------------------------------------------------

instance Clip ClipDoc where
  arityClip Clip_Nothing = -1
  arityClip (Clip_Document x) = arity x
  arityClip (Clip_EnrichedDoc x) = arity x
  arityClip (Clip_Root x) = arity x
  arityClip (Clip_Section x) = arity x
  arityClip (Clip_Subsection x) = arity x
  arityClip (Clip_Subsubsection x) = arity x
  arityClip (Clip_Paragraph x) = arity x
  arityClip (Clip_Word x) = arity x
  arityClip (Clip_Graph x) = arity x
  arityClip (Clip_Vertex x) = arity x
  arityClip (Clip_Shape x) = arity x
  arityClip (Clip_Edge x) = arity x
  arityClip (Clip_Subgraph x) = arity x
  arityClip (Clip_Dirty x) = arity x
  arityClip (Clip_List_Section x) = arity x
  arityClip (Clip_List_Paragraph x) = arity x
  arityClip (Clip_List_Subsection x) = arity x
  arityClip (Clip_List_Subsubsection x) = arity x
  arityClip (Clip_List_Word x) = arity x
  arityClip (Clip_List_Vertex x) = arity x
  arityClip (Clip_List_Edge x) = arity x
  arityClip (Clip_Bool x) = arity x
  arityClip (Clip_Int x) = arity x
  arityClip (Clip_String x) = arity x
  arityClip (Clip_Float x) = arity x

  alternativesClip Clip_Nothing = []
  alternativesClip (Clip_Document x) = alternatives x
  alternativesClip (Clip_EnrichedDoc x) = alternatives x
  alternativesClip (Clip_Root x) = alternatives x
  alternativesClip (Clip_Section x) = alternatives x
  alternativesClip (Clip_Subsection x) = alternatives x
  alternativesClip (Clip_Subsubsection x) = alternatives x
  alternativesClip (Clip_Paragraph x) = alternatives x
  alternativesClip (Clip_Word x) = alternatives x
  alternativesClip (Clip_Graph x) = alternatives x
  alternativesClip (Clip_Vertex x) = alternatives x
  alternativesClip (Clip_Shape x) = alternatives x
  alternativesClip (Clip_Edge x) = alternatives x
  alternativesClip (Clip_Subgraph x) = alternatives x
  alternativesClip (Clip_Dirty x) = alternatives x
  alternativesClip (Clip_List_Section x) = alternatives x
  alternativesClip (Clip_List_Paragraph x) = alternatives x
  alternativesClip (Clip_List_Subsection x) = alternatives x
  alternativesClip (Clip_List_Subsubsection x) = alternatives x
  alternativesClip (Clip_List_Word x) = alternatives x
  alternativesClip (Clip_List_Vertex x) = alternatives x
  alternativesClip (Clip_List_Edge x) = alternatives x
  alternativesClip (Clip_Bool x) = alternatives x
  alternativesClip (Clip_Int x) = alternatives x
  alternativesClip (Clip_String x) = alternatives x
  alternativesClip (Clip_Float x) = alternatives x

  holeClip Clip_Nothing = Clip_Nothing
  holeClip (Clip_Document x) = Clip_Document hole
  holeClip (Clip_EnrichedDoc x) = Clip_EnrichedDoc hole
  holeClip (Clip_Root x) = Clip_Root hole
  holeClip (Clip_Section x) = Clip_Section hole
  holeClip (Clip_Subsection x) = Clip_Subsection hole
  holeClip (Clip_Subsubsection x) = Clip_Subsubsection hole
  holeClip (Clip_Paragraph x) = Clip_Paragraph hole
  holeClip (Clip_Word x) = Clip_Word hole
  holeClip (Clip_Graph x) = Clip_Graph hole
  holeClip (Clip_Vertex x) = Clip_Vertex hole
  holeClip (Clip_Shape x) = Clip_Shape hole
  holeClip (Clip_Edge x) = Clip_Edge hole
  holeClip (Clip_Subgraph x) = Clip_Subgraph hole
  holeClip (Clip_Dirty x) = Clip_Dirty hole
  holeClip (Clip_List_Section x) = Clip_List_Section hole
  holeClip (Clip_List_Paragraph x) = Clip_List_Paragraph hole
  holeClip (Clip_List_Subsection x) = Clip_List_Subsection hole
  holeClip (Clip_List_Subsubsection x) = Clip_List_Subsubsection hole
  holeClip (Clip_List_Word x) = Clip_List_Word hole
  holeClip (Clip_List_Vertex x) = Clip_List_Vertex hole
  holeClip (Clip_List_Edge x) = Clip_List_Edge hole
  holeClip (Clip_Bool x) = Clip_Bool hole
  holeClip (Clip_Int x) = Clip_Int hole
  holeClip (Clip_String x) = Clip_String hole
  holeClip (Clip_Float x) = Clip_Float hole

  isListClip Clip_Nothing = False
  isListClip (Clip_Document x) = isList x
  isListClip (Clip_EnrichedDoc x) = isList x
  isListClip (Clip_Root x) = isList x
  isListClip (Clip_Section x) = isList x
  isListClip (Clip_Subsection x) = isList x
  isListClip (Clip_Subsubsection x) = isList x
  isListClip (Clip_Paragraph x) = isList x
  isListClip (Clip_Word x) = isList x
  isListClip (Clip_Graph x) = isList x
  isListClip (Clip_Vertex x) = isList x
  isListClip (Clip_Shape x) = isList x
  isListClip (Clip_Edge x) = isList x
  isListClip (Clip_Subgraph x) = isList x
  isListClip (Clip_Dirty x) = isList x
  isListClip (Clip_List_Section x) = isList x
  isListClip (Clip_List_Paragraph x) = isList x
  isListClip (Clip_List_Subsection x) = isList x
  isListClip (Clip_List_Subsubsection x) = isList x
  isListClip (Clip_List_Word x) = isList x
  isListClip (Clip_List_Vertex x) = isList x
  isListClip (Clip_List_Edge x) = isList x
  isListClip (Clip_Bool x) = isList x
  isListClip (Clip_Int x) = isList x
  isListClip (Clip_String x) = isList x
  isListClip (Clip_Float x) = isList x

  insertListClip i c Clip_Nothing = Clip_Nothing
  insertListClip i c (Clip_Document x) = insertList i c x
  insertListClip i c (Clip_EnrichedDoc x) = insertList i c x
  insertListClip i c (Clip_Root x) = insertList i c x
  insertListClip i c (Clip_Section x) = insertList i c x
  insertListClip i c (Clip_Subsection x) = insertList i c x
  insertListClip i c (Clip_Subsubsection x) = insertList i c x
  insertListClip i c (Clip_Paragraph x) = insertList i c x
  insertListClip i c (Clip_Word x) = insertList i c x
  insertListClip i c (Clip_Graph x) = insertList i c x
  insertListClip i c (Clip_Vertex x) = insertList i c x
  insertListClip i c (Clip_Shape x) = insertList i c x
  insertListClip i c (Clip_Edge x) = insertList i c x
  insertListClip i c (Clip_Subgraph x) = insertList i c x
  insertListClip i c (Clip_Dirty x) = insertList i c x
  insertListClip i c (Clip_List_Section x) = insertList i c x
  insertListClip i c (Clip_List_Paragraph x) = insertList i c x
  insertListClip i c (Clip_List_Subsection x) = insertList i c x
  insertListClip i c (Clip_List_Subsubsection x) = insertList i c x
  insertListClip i c (Clip_List_Word x) = insertList i c x
  insertListClip i c (Clip_List_Vertex x) = insertList i c x
  insertListClip i c (Clip_List_Edge x) = insertList i c x
  insertListClip i c (Clip_Bool x) = insertList i c x
  insertListClip i c (Clip_Int x) = insertList i c x
  insertListClip i c (Clip_String x) = insertList i c x
  insertListClip i c (Clip_Float x) = insertList i c x

  removeListClip i Clip_Nothing = Clip_Nothing
  removeListClip i (Clip_Document x) = removeList i x
  removeListClip i (Clip_EnrichedDoc x) = removeList i x
  removeListClip i (Clip_Root x) = removeList i x
  removeListClip i (Clip_Section x) = removeList i x
  removeListClip i (Clip_Subsection x) = removeList i x
  removeListClip i (Clip_Subsubsection x) = removeList i x
  removeListClip i (Clip_Paragraph x) = removeList i x
  removeListClip i (Clip_Word x) = removeList i x
  removeListClip i (Clip_Graph x) = removeList i x
  removeListClip i (Clip_Vertex x) = removeList i x
  removeListClip i (Clip_Shape x) = removeList i x
  removeListClip i (Clip_Edge x) = removeList i x
  removeListClip i (Clip_Subgraph x) = removeList i x
  removeListClip i (Clip_Dirty x) = removeList i x
  removeListClip i (Clip_List_Section x) = removeList i x
  removeListClip i (Clip_List_Paragraph x) = removeList i x
  removeListClip i (Clip_List_Subsection x) = removeList i x
  removeListClip i (Clip_List_Subsubsection x) = removeList i x
  removeListClip i (Clip_List_Word x) = removeList i x
  removeListClip i (Clip_List_Vertex x) = removeList i x
  removeListClip i (Clip_List_Edge x) = removeList i x
  removeListClip i (Clip_Bool x) = removeList i x
  removeListClip i (Clip_Int x) = removeList i x
  removeListClip i (Clip_String x) = removeList i x
  removeListClip i (Clip_Float x) = removeList i x




--------------------------------------------------------------------------
-- Editable instances                                                   --
--------------------------------------------------------------------------

instance Editable Document Document Node ClipDoc UserToken where
  select [] x = Clip_Document x
  select (0:p) (RootDoc x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Document c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Document") x
  paste (0:p) c (RootDoc x0) = RootDoc (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("RootDoc {Root} "  , Clip_Document $ RootDoc hole)
                   ,("{Document}", Clip_Document hole)
                   ]

  arity (RootDoc x0) = 1
  arity _                        = 0

  parseErr = ParseErrDocument

  hole = HoleDocument

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable EnrichedDoc Document Node ClipDoc UserToken where
  select [] x = Clip_EnrichedDoc x
  select (0:p) (RootEnr x0 x1) = select p x0
  select (1:p) (RootEnr x0 x1) = select p x1
  select _ _ = Clip_Nothing

  paste [] (Clip_EnrichedDoc c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on EnrichedDoc") x
  paste (0:p) c (RootEnr x0 x1) = RootEnr (paste p c x0) x1
  paste (1:p) c (RootEnr x0 x1) = RootEnr x0 (paste p c x1)
  paste _ _ x = x

  alternatives _ = [ ("RootEnr {Root} {Document} "  , Clip_EnrichedDoc $ RootEnr hole hole)
                   ,("{EnrichedDoc}", Clip_EnrichedDoc hole)
                   ]

  arity (RootEnr x0 x1) = 2
  arity _                        = 0

  parseErr = ParseErrEnrichedDoc

  hole = HoleEnrichedDoc

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Root Document Node ClipDoc UserToken where
  select [] x = Clip_Root x
  select (0:p) (Root x0 x1 x2) = select p x0
  select (1:p) (Root x0 x1 x2) = select p x1
  select (2:p) (Root x0 x1 x2) = select p x2
  select _ _ = Clip_Nothing

  paste [] (Clip_Root c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Root") x
  paste (0:p) c (Root x0 x1 x2) = Root (paste p c x0) x1 x2
  paste (1:p) c (Root x0 x1 x2) = Root x0 (paste p c x1) x2
  paste (2:p) c (Root x0 x1 x2) = Root x0 x1 (paste p c x2)
  paste _ _ x = x

  alternatives _ = [ ("Root {Graph} {String} {List_Section} "  , Clip_Root $ Root hole hole hole)
                   ,("{Root}", Clip_Root hole)
                   ]

  arity (Root x0 x1 x2) = 3
  arity _                        = 0

  parseErr = ParseErrRoot

  hole = HoleRoot

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Section Document Node ClipDoc UserToken where
  select [] x = Clip_Section x
  select (0:p) (Section x0 x1 x2) = select p x0
  select (1:p) (Section x0 x1 x2) = select p x1
  select (2:p) (Section x0 x1 x2) = select p x2
  select _ _ = Clip_Nothing

  paste [] (Clip_Section c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Section") x
  paste (0:p) c (Section x0 x1 x2) = Section (paste p c x0) x1 x2
  paste (1:p) c (Section x0 x1 x2) = Section x0 (paste p c x1) x2
  paste (2:p) c (Section x0 x1 x2) = Section x0 x1 (paste p c x2)
  paste _ _ x = x

  alternatives _ = [ ("Section {String} {List_Paragraph} {List_Subsection} "  , Clip_Section $ Section hole hole hole)
                   ,("{Section}", Clip_Section hole)
                   ]

  arity (Section x0 x1 x2) = 3
  arity _                        = 0

  parseErr = ParseErrSection

  hole = HoleSection

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Subsection Document Node ClipDoc UserToken where
  select [] x = Clip_Subsection x
  select (0:p) (Subsection x0 x1 x2) = select p x0
  select (1:p) (Subsection x0 x1 x2) = select p x1
  select (2:p) (Subsection x0 x1 x2) = select p x2
  select _ _ = Clip_Nothing

  paste [] (Clip_Subsection c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Subsection") x
  paste (0:p) c (Subsection x0 x1 x2) = Subsection (paste p c x0) x1 x2
  paste (1:p) c (Subsection x0 x1 x2) = Subsection x0 (paste p c x1) x2
  paste (2:p) c (Subsection x0 x1 x2) = Subsection x0 x1 (paste p c x2)
  paste _ _ x = x

  alternatives _ = [ ("Subsection {String} {List_Paragraph} {List_Subsubsection} "  , Clip_Subsection $ Subsection hole hole hole)
                   ,("{Subsection}", Clip_Subsection hole)
                   ]

  arity (Subsection x0 x1 x2) = 3
  arity _                        = 0

  parseErr = ParseErrSubsection

  hole = HoleSubsection

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Subsubsection Document Node ClipDoc UserToken where
  select [] x = Clip_Subsubsection x
  select (0:p) (Subsubsection x0 x1) = select p x0
  select (1:p) (Subsubsection x0 x1) = select p x1
  select _ _ = Clip_Nothing

  paste [] (Clip_Subsubsection c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Subsubsection") x
  paste (0:p) c (Subsubsection x0 x1) = Subsubsection (paste p c x0) x1
  paste (1:p) c (Subsubsection x0 x1) = Subsubsection x0 (paste p c x1)
  paste _ _ x = x

  alternatives _ = [ ("Subsubsection {String} {List_Paragraph} "  , Clip_Subsubsection $ Subsubsection hole hole)
                   ,("{Subsubsection}", Clip_Subsubsection hole)
                   ]

  arity (Subsubsection x0 x1) = 2
  arity _                        = 0

  parseErr = ParseErrSubsubsection

  hole = HoleSubsubsection

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Paragraph Document Node ClipDoc UserToken where
  select [] x = Clip_Paragraph x
  select (0:p) (Paragraph x0) = select p x0
  select (0:p) (SubgraphPara x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Paragraph c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Paragraph") x
  paste (0:p) c (Paragraph x0) = Paragraph (paste p c x0)
  paste (0:p) c (SubgraphPara x0) = SubgraphPara (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("Paragraph {List_Word} "  , Clip_Paragraph $ Paragraph hole)
                   , ("SubgraphPara {Subgraph} "  , Clip_Paragraph $ SubgraphPara hole)
                   ,("{Paragraph}", Clip_Paragraph hole)
                   ]

  arity (Paragraph x0) = 1
  arity (SubgraphPara x0) = 1
  arity _                        = 0

  parseErr = ParseErrParagraph

  hole = HoleParagraph

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Word Document Node ClipDoc UserToken where
  select [] x = Clip_Word x
  select (0:p) (Word x0) = select p x0
  select (0:p) (NodeRef x0) = select p x0
  select (0:p) (Label x0) = select p x0
  select (0:p) (LabelRef x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Word c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Word") x
  paste (0:p) c (Word x0) = Word (paste p c x0)
  paste (0:p) c (NodeRef x0) = NodeRef (paste p c x0)
  paste (0:p) c (Label x0) = Label (paste p c x0)
  paste (0:p) c (LabelRef x0) = LabelRef (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("Word {String} "  , Clip_Word $ Word hole)
                   , ("NodeRef {String} "  , Clip_Word $ NodeRef hole)
                   , ("Label {String} "  , Clip_Word $ Label hole)
                   , ("LabelRef {String} "  , Clip_Word $ LabelRef hole)
                   ,("{Word}", Clip_Word hole)
                   ]

  arity (Word x0) = 1
  arity (NodeRef x0) = 1
  arity (Label x0) = 1
  arity (LabelRef x0) = 1
  arity _                        = 0

  parseErr = ParseErrWord

  hole = HoleWord

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Graph Document Node ClipDoc UserToken where
  select [] x = Clip_Graph x
  select (0:p) (Graph x0 x1 x2) = select p x0
  select (1:p) (Graph x0 x1 x2) = select p x1
  select (2:p) (Graph x0 x1 x2) = select p x2
  select _ _ = Clip_Nothing

  paste [] (Clip_Graph c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Graph") x
  paste (0:p) c (Graph x0 x1 x2) = Graph (paste p c x0) x1 x2
  paste (1:p) c (Graph x0 x1 x2) = Graph x0 (paste p c x1) x2
  paste (2:p) c (Graph x0 x1 x2) = Graph x0 x1 (paste p c x2)
  paste _ _ x = x

  alternatives _ = [ ("Graph {Dirty} {List_Vertex} {List_Edge} "  , Clip_Graph $ Graph hole hole hole)
                   ,("{Graph}", Clip_Graph hole)
                   ]

  arity (Graph x0 x1 x2) = 3
  arity _                        = 0

  parseErr = ParseErrGraph

  hole = HoleGraph

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Vertex Document Node ClipDoc UserToken where
  select [] x = Clip_Vertex x
  select (0:p) (Vertex x0 x1 x2 x3 x4) = select p x0
  select (1:p) (Vertex x0 x1 x2 x3 x4) = select p x1
  select (2:p) (Vertex x0 x1 x2 x3 x4) = select p x2
  select (3:p) (Vertex x0 x1 x2 x3 x4) = select p x3
  select (4:p) (Vertex x0 x1 x2 x3 x4) = select p x4
  select _ _ = Clip_Nothing

  paste [] (Clip_Vertex c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Vertex") x
  paste (0:p) c (Vertex x0 x1 x2 x3 x4) = Vertex (paste p c x0) x1 x2 x3 x4
  paste (1:p) c (Vertex x0 x1 x2 x3 x4) = Vertex x0 (paste p c x1) x2 x3 x4
  paste (2:p) c (Vertex x0 x1 x2 x3 x4) = Vertex x0 x1 (paste p c x2) x3 x4
  paste (3:p) c (Vertex x0 x1 x2 x3 x4) = Vertex x0 x1 x2 (paste p c x3) x4
  paste (4:p) c (Vertex x0 x1 x2 x3 x4) = Vertex x0 x1 x2 x3 (paste p c x4)
  paste _ _ x = x

  alternatives _ = [ ("Vertex {String} {Shape} {Int} {Int} {Int} "  , Clip_Vertex $ Vertex hole hole hole hole hole)
                   ,("{Vertex}", Clip_Vertex hole)
                   ]

  arity (Vertex x0 x1 x2 x3 x4) = 5
  arity _                        = 0

  parseErr = ParseErrVertex

  hole = HoleVertex

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Shape Document Node ClipDoc UserToken where
  select [] x = Clip_Shape x
  select _ _ = Clip_Nothing

  paste [] (Clip_Shape c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Shape") x
  paste _ _ x = x

  alternatives _ = [ ("Circle "  , Clip_Shape $ Circle)
                   , ("Square "  , Clip_Shape $ Square)
                   ,("{Shape}", Clip_Shape hole)
                   ]

  arity (Circle) = 0
  arity (Square) = 0
  arity _                        = 0

  parseErr = ParseErrShape

  hole = HoleShape

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Edge Document Node ClipDoc UserToken where
  select [] x = Clip_Edge x
  select (0:p) (Edge x0 x1) = select p x0
  select (1:p) (Edge x0 x1) = select p x1
  select _ _ = Clip_Nothing

  paste [] (Clip_Edge c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Edge") x
  paste (0:p) c (Edge x0 x1) = Edge (paste p c x0) x1
  paste (1:p) c (Edge x0 x1) = Edge x0 (paste p c x1)
  paste _ _ x = x

  alternatives _ = [ ("Edge {Int} {Int} "  , Clip_Edge $ Edge hole hole)
                   ,("{Edge}", Clip_Edge hole)
                   ]

  arity (Edge x0 x1) = 2
  arity _                        = 0

  parseErr = ParseErrEdge

  hole = HoleEdge

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Subgraph Document Node ClipDoc UserToken where
  select [] x = Clip_Subgraph x
  select (0:p) (Subgraph x0 x1 x2) = select p x0
  select (1:p) (Subgraph x0 x1 x2) = select p x1
  select (2:p) (Subgraph x0 x1 x2) = select p x2
  select _ _ = Clip_Nothing

  paste [] (Clip_Subgraph c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Subgraph") x
  paste (0:p) c (Subgraph x0 x1 x2) = Subgraph (paste p c x0) x1 x2
  paste (1:p) c (Subgraph x0 x1 x2) = Subgraph x0 (paste p c x1) x2
  paste (2:p) c (Subgraph x0 x1 x2) = Subgraph x0 x1 (paste p c x2)
  paste _ _ x = x

  alternatives _ = [ ("Subgraph {Dirty} {List_Vertex} {List_Edge} "  , Clip_Subgraph $ Subgraph hole hole hole)
                   ,("{Subgraph}", Clip_Subgraph hole)
                   ]

  arity (Subgraph x0 x1 x2) = 3
  arity _                        = 0

  parseErr = ParseErrSubgraph

  hole = HoleSubgraph

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Dirty Document Node ClipDoc UserToken where
  select [] x = Clip_Dirty x
  select _ _ = Clip_Nothing

  paste [] (Clip_Dirty c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Dirty") x
  paste _ _ x = x

  alternatives _ = [ ("Dirty "  , Clip_Dirty $ Dirty)
                   , ("Clean "  , Clip_Dirty $ Clean)
                   ,("{Dirty}", Clip_Dirty hole)
                   ]

  arity (Dirty) = 0
  arity (Clean) = 0
  arity _                        = 0

  parseErr = ParseErrDirty

  hole = HoleDirty

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable List_Section Document Node ClipDoc UserToken where
  select [] x = Clip_List_Section x
  select (n:p) (List_Section cxs) =
    let xs = fromConsList_Section cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Section c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Section")   x
  paste (n:p) c (List_Section cxs) =
    let xs = fromConsList_Section cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Section (replaceList_Section n x' cxs)
        else List_Section cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Section}", Clip_List_Section hole)
                   ]

  arity (List_Section x1) = length (fromConsList_Section x1)
  arity _ = 0

  parseErr = ParseErrList_Section

  hole = List_Section Nil_Section

  isList _ = True

  insertList n (Clip_Section c) (List_Section cxs) = Clip_List_Section $ List_Section (insertList_Section n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Section xs
  insertList _ c xs = Clip_List_Section xs

  removeList n (List_Section cxs) = Clip_List_Section $ List_Section (removeList_Section n cxs)
  removeList _ xs = Clip_List_Section $ xs

instance Editable List_Paragraph Document Node ClipDoc UserToken where
  select [] x = Clip_List_Paragraph x
  select (n:p) (List_Paragraph cxs) =
    let xs = fromConsList_Paragraph cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Paragraph c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Paragraph")   x
  paste (n:p) c (List_Paragraph cxs) =
    let xs = fromConsList_Paragraph cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Paragraph (replaceList_Paragraph n x' cxs)
        else List_Paragraph cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Paragraph}", Clip_List_Paragraph hole)
                   ]

  arity (List_Paragraph x1) = length (fromConsList_Paragraph x1)
  arity _ = 0

  parseErr = ParseErrList_Paragraph

  hole = List_Paragraph Nil_Paragraph

  isList _ = True

  insertList n (Clip_Paragraph c) (List_Paragraph cxs) = Clip_List_Paragraph $ List_Paragraph (insertList_Paragraph n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Paragraph xs
  insertList _ c xs = Clip_List_Paragraph xs

  removeList n (List_Paragraph cxs) = Clip_List_Paragraph $ List_Paragraph (removeList_Paragraph n cxs)
  removeList _ xs = Clip_List_Paragraph $ xs

instance Editable List_Subsection Document Node ClipDoc UserToken where
  select [] x = Clip_List_Subsection x
  select (n:p) (List_Subsection cxs) =
    let xs = fromConsList_Subsection cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Subsection c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Subsection")   x
  paste (n:p) c (List_Subsection cxs) =
    let xs = fromConsList_Subsection cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Subsection (replaceList_Subsection n x' cxs)
        else List_Subsection cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Subsection}", Clip_List_Subsection hole)
                   ]

  arity (List_Subsection x1) = length (fromConsList_Subsection x1)
  arity _ = 0

  parseErr = ParseErrList_Subsection

  hole = List_Subsection Nil_Subsection

  isList _ = True

  insertList n (Clip_Subsection c) (List_Subsection cxs) = Clip_List_Subsection $ List_Subsection (insertList_Subsection n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Subsection xs
  insertList _ c xs = Clip_List_Subsection xs

  removeList n (List_Subsection cxs) = Clip_List_Subsection $ List_Subsection (removeList_Subsection n cxs)
  removeList _ xs = Clip_List_Subsection $ xs

instance Editable List_Subsubsection Document Node ClipDoc UserToken where
  select [] x = Clip_List_Subsubsection x
  select (n:p) (List_Subsubsection cxs) =
    let xs = fromConsList_Subsubsection cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Subsubsection c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Subsubsection")   x
  paste (n:p) c (List_Subsubsection cxs) =
    let xs = fromConsList_Subsubsection cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Subsubsection (replaceList_Subsubsection n x' cxs)
        else List_Subsubsection cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Subsubsection}", Clip_List_Subsubsection hole)
                   ]

  arity (List_Subsubsection x1) = length (fromConsList_Subsubsection x1)
  arity _ = 0

  parseErr = ParseErrList_Subsubsection

  hole = List_Subsubsection Nil_Subsubsection

  isList _ = True

  insertList n (Clip_Subsubsection c) (List_Subsubsection cxs) = Clip_List_Subsubsection $ List_Subsubsection (insertList_Subsubsection n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Subsubsection xs
  insertList _ c xs = Clip_List_Subsubsection xs

  removeList n (List_Subsubsection cxs) = Clip_List_Subsubsection $ List_Subsubsection (removeList_Subsubsection n cxs)
  removeList _ xs = Clip_List_Subsubsection $ xs

instance Editable List_Word Document Node ClipDoc UserToken where
  select [] x = Clip_List_Word x
  select (n:p) (List_Word cxs) =
    let xs = fromConsList_Word cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Word c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Word")   x
  paste (n:p) c (List_Word cxs) =
    let xs = fromConsList_Word cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Word (replaceList_Word n x' cxs)
        else List_Word cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Word}", Clip_List_Word hole)
                   ]

  arity (List_Word x1) = length (fromConsList_Word x1)
  arity _ = 0

  parseErr = ParseErrList_Word

  hole = List_Word Nil_Word

  isList _ = True

  insertList n (Clip_Word c) (List_Word cxs) = Clip_List_Word $ List_Word (insertList_Word n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Word xs
  insertList _ c xs = Clip_List_Word xs

  removeList n (List_Word cxs) = Clip_List_Word $ List_Word (removeList_Word n cxs)
  removeList _ xs = Clip_List_Word $ xs

instance Editable List_Vertex Document Node ClipDoc UserToken where
  select [] x = Clip_List_Vertex x
  select (n:p) (List_Vertex cxs) =
    let xs = fromConsList_Vertex cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Vertex c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Vertex")   x
  paste (n:p) c (List_Vertex cxs) =
    let xs = fromConsList_Vertex cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Vertex (replaceList_Vertex n x' cxs)
        else List_Vertex cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Vertex}", Clip_List_Vertex hole)
                   ]

  arity (List_Vertex x1) = length (fromConsList_Vertex x1)
  arity _ = 0

  parseErr = ParseErrList_Vertex

  hole = List_Vertex Nil_Vertex

  isList _ = True

  insertList n (Clip_Vertex c) (List_Vertex cxs) = Clip_List_Vertex $ List_Vertex (insertList_Vertex n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Vertex xs
  insertList _ c xs = Clip_List_Vertex xs

  removeList n (List_Vertex cxs) = Clip_List_Vertex $ List_Vertex (removeList_Vertex n cxs)
  removeList _ xs = Clip_List_Vertex $ xs

instance Editable List_Edge Document Node ClipDoc UserToken where
  select [] x = Clip_List_Edge x
  select (n:p) (List_Edge cxs) =
    let xs = fromConsList_Edge cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Edge c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Edge")   x
  paste (n:p) c (List_Edge cxs) =
    let xs = fromConsList_Edge cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Edge (replaceList_Edge n x' cxs)
        else List_Edge cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Edge}", Clip_List_Edge hole)
                   ]

  arity (List_Edge x1) = length (fromConsList_Edge x1)
  arity _ = 0

  parseErr = ParseErrList_Edge

  hole = List_Edge Nil_Edge

  isList _ = True

  insertList n (Clip_Edge c) (List_Edge cxs) = Clip_List_Edge $ List_Edge (insertList_Edge n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Edge xs
  insertList _ c xs = Clip_List_Edge xs

  removeList n (List_Edge cxs) = Clip_List_Edge $ List_Edge (removeList_Edge n cxs)
  removeList _ xs = Clip_List_Edge $ xs




--------------------------------------------------------------------------
-- Editable instances for Document, EnrichedDoc and primitive types     --
--------------------------------------------------------------------------

instance Editable Int Document Node ClipDoc UserToken where
  select [] x = Clip_Int x
  select _  _ = Clip_Nothing
  paste [] (Clip_Int c) x = c
  paste [] c            x =  debug Err ("Type error: pasting "++show c++" on Int") x
  paste _  _             x = x
  
  alternatives _ = [ ("0", Clip_Int 0) ]
  
  arity _ = 0
  parseErr _ = 0

  hole = 0

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Float Document Node ClipDoc UserToken where
  select [] x = Clip_Float x
  select _  _ = Clip_Nothing
  paste [] (Clip_Float c) x = c
  paste [] c              x =  debug Err ("Type error: pasting "++show c++" on Float") x
  paste _  _              x = x
  
  alternatives _ = [ ("0.0", Clip_Float 0.0) ]
  
  arity _ = 0
  parseErr _ = 0

  hole = 0

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Bool Document Node ClipDoc UserToken where
  select [] x = Clip_Bool x                            
  select _  _ = Clip_Nothing                           
  paste [] (Clip_Bool c) x = c                         
  paste [] c             x =  debug Err ("Type error: pasting "++show c++" on Bool") x
  paste _  _             x = x
  alternatives _ = [ ("True", Clip_Bool True)        
                   , ("False", Clip_Bool False)      
                   ]    
  arity _ = 0                                          
  parseErr _ = False

  hole = False

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable String Document Node ClipDoc UserToken where
  select [] x = Clip_String x
  select _  _ = Clip_Nothing
  paste [] (Clip_String c) x = c
  paste [] c             x =  debug Err ("Type error: pasting "++show c++" on String") x
  paste _  _             x = x

  alternatives _ = [ ("string", Clip_String "string")
                   ] 
 
  arity _ = 0
  parseErr _ = "{ParseErr}"

  hole = "{String}"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


