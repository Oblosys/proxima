module DocumentEdit_Generated where

import CommonTypes hiding (Dirty (..))
import DocTypes
import DocTypes_Generated
import DocumentEdit
import DocUtils
import PresTypes

import Debug.Trace


-- not entirely generated: hole is special for chessboard and pppresentation, because hole is initialized
-- This is not good. hole must be just a hole for cut operations.
-- an initialized value must be specified separately class member initialValue?


-- Constructor for HoleClip can be put in class as toClip or inject


-- paths start below RootDoc, so on traversing the RootDoc constructor p is not modified
instance Editable Document Document Node ClipDoc UserToken where
  select p (RootDoc id x) = select p x
  paste p c (RootDoc id  x) = RootDoc id $ paste p c x
  hole = HoleDocument
  parseErr = ParseErrDocument
  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

-- from Editable, only the member parseErr is used for EnrichedDoc
instance Editable EnrichedDoc Document Node ClipDoc UserToken where
--  hole = HoleEnrichedDoc
  parseErr = ParseErrEnrichedDoc
  
instance Editable Int Document Node ClipDoc UserToken where
  select [] x = Clip_Int x
  select _  _ = Clip_Nothing
  paste [] (Clip_Int c) x = c
  paste [] c            x =  trace ("Type error: pasting "++show c++" on Int") x
  paste _  _             x = x
  
  alternatives _ = [ ("0", Clip_Int 0)
                   , ("1", Clip_Int 1)
                   , ("{Int}", Clip_Int hole) ]
  
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
  paste [] c             x =  trace ("Type error: pasting "++show c++" on Bool") x
  paste _  _             x = x
  alternatives _ = [ ("True", Clip_Bool True)        
                   , ("False", Clip_Bool False)      
                   , ("{Bool}", Clip_Bool hole) ]    
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
  paste [] c             x =  trace ("Type error: pasting "++show c++" on String") x
  paste _  _             x = x

  alternatives _ = [ ("a", Clip_String "a")
                   , ("ab", Clip_String "ab")
                   , ("{String}", Clip_String hole) ] 
 
  arity _ = 0
  parseErr _= "{ParseErr}"

  hole = "{String}"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing



----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

{- ------------------------------------

 generated part

-------------------------------------- -}


-- Generated clipfunctions  --

instance Clip ClipDoc where
  arityClip (Clip_Root x) = arity x
  arityClip (Clip_Document x) = arity x
  arityClip (Clip_List_Dummy x) = arity x
  arityClip (Clip_String x) = arity x
  arityClip (Clip_Bool x) = arity x
  arityClip (Clip_Int x) = arity x
  arityClip (Clip_Graph x) = arity x
  arityClip (Clip_List_Section x) = arity x
  arityClip (Clip_List_Paragraph x) = arity x
  arityClip (Clip_List_Subsection x) = arity x
  arityClip (Clip_List_Subsubsection x) = arity x
  arityClip (Clip_List_Word x) = arity x
  arityClip (Clip_Subgraph x) = arity x
  arityClip (Clip_Dirty x) = arity x
  arityClip (Clip_List_Vertex x) = arity x
  arityClip (Clip_List_Edge x) = arity x
  arityClip (Clip_Shape x) = arity x
  arityClip (Clip_Dummy x) = arity x
  arityClip (Clip_Section x) = arity x
  arityClip (Clip_Paragraph x) = arity x
  arityClip (Clip_Subsection x) = arity x
  arityClip (Clip_Subsubsection x) = arity x
  arityClip (Clip_Word x) = arity x
  arityClip (Clip_Vertex x) = arity x
  arityClip (Clip_Edge x) = arity x
  arityClip (Clip_Nothing)   = -1
  alternativesClip (Clip_Root x) = alternatives x
  alternativesClip (Clip_Document x) = alternatives x
  alternativesClip (Clip_List_Dummy x) = alternatives x
  alternativesClip (Clip_String x) = alternatives x
  alternativesClip (Clip_Bool x) = alternatives x
  alternativesClip (Clip_Int x) = alternatives x
  alternativesClip (Clip_Graph x) = alternatives x
  alternativesClip (Clip_List_Section x) = alternatives x
  alternativesClip (Clip_List_Paragraph x) = alternatives x
  alternativesClip (Clip_List_Subsection x) = alternatives x
  alternativesClip (Clip_List_Subsubsection x) = alternatives x
  alternativesClip (Clip_List_Word x) = alternatives x
  alternativesClip (Clip_Subgraph x) = alternatives x
  alternativesClip (Clip_Dirty x) = alternatives x
  alternativesClip (Clip_List_Vertex x) = alternatives x
  alternativesClip (Clip_List_Edge x) = alternatives x
  alternativesClip (Clip_Shape x) = alternatives x
  alternativesClip (Clip_Dummy x) = alternatives x
  alternativesClip (Clip_Section x) = alternatives x
  alternativesClip (Clip_Paragraph x) = alternatives x
  alternativesClip (Clip_Subsection x) = alternatives x
  alternativesClip (Clip_Subsubsection x) = alternatives x
  alternativesClip (Clip_Word x) = alternatives x
  alternativesClip (Clip_Vertex x) = alternatives x
  alternativesClip (Clip_Edge x) = alternatives x
  alternativesClip (Clip_Nothing)   = []

  holeClip (Clip_Root x) = Clip_Root hole
  holeClip (Clip_Document x) = Clip_Document hole
  holeClip (Clip_List_Dummy x) = Clip_List_Dummy hole
  holeClip (Clip_String x) = Clip_String hole
  holeClip (Clip_Bool x) = Clip_Bool hole
  holeClip (Clip_Int x) = Clip_Int hole
  holeClip (Clip_Graph x) = Clip_Graph hole
  holeClip (Clip_List_Section x) = Clip_List_Section hole
  holeClip (Clip_List_Paragraph x) = Clip_List_Paragraph hole
  holeClip (Clip_List_Subsection x) = Clip_List_Subsection hole
  holeClip (Clip_List_Subsubsection x) = Clip_List_Subsubsection hole
  holeClip (Clip_List_Word x) = Clip_List_Word hole
  holeClip (Clip_Subgraph x) = Clip_Subgraph hole
  holeClip (Clip_Dirty x) = Clip_Dirty hole
  holeClip (Clip_List_Vertex x) = Clip_List_Vertex hole
  holeClip (Clip_List_Edge x) = Clip_List_Edge hole
  holeClip (Clip_Shape x) = Clip_Shape hole
  holeClip (Clip_Dummy x) = Clip_Dummy hole
  holeClip (Clip_Section x) = Clip_Section hole
  holeClip (Clip_Paragraph x) = Clip_Paragraph hole
  holeClip (Clip_Subsection x) = Clip_Subsection hole
  holeClip (Clip_Subsubsection x) = Clip_Subsubsection hole
  holeClip (Clip_Word x) = Clip_Word hole
  holeClip (Clip_Vertex x) = Clip_Vertex hole
  holeClip (Clip_Edge x) = Clip_Edge hole
  holeClip Clip_Nothing   = Clip_Nothing

  isListClip (Clip_Root x) = isList x
  isListClip (Clip_Document x) = isList x
  isListClip (Clip_List_Dummy x) = isList x
  isListClip (Clip_String x) = isList x
  isListClip (Clip_Bool x) = isList x
  isListClip (Clip_Int x) = isList x
  isListClip (Clip_Graph x) = isList x
  isListClip (Clip_List_Section x) = isList x
  isListClip (Clip_List_Paragraph x) = isList x
  isListClip (Clip_List_Subsection x) = isList x
  isListClip (Clip_List_Subsubsection x) = isList x
  isListClip (Clip_List_Word x) = isList x
  isListClip (Clip_Subgraph x) = isList x
  isListClip (Clip_Dirty x) = isList x
  isListClip (Clip_List_Vertex x) = isList x
  isListClip (Clip_List_Edge x) = isList x
  isListClip (Clip_Shape x) = isList x
  isListClip (Clip_Dummy x) = isList x
  isListClip (Clip_Section x) = isList x
  isListClip (Clip_Paragraph x) = isList x
  isListClip (Clip_Subsection x) = isList x
  isListClip (Clip_Subsubsection x) = isList x
  isListClip (Clip_Word x) = isList x
  isListClip (Clip_Vertex x) = isList x
  isListClip (Clip_Edge x) = isList x
  isListClip (Clip_Nothing)   = False

  insertListClip i c (Clip_Root x) = insertList i c x
  insertListClip i c (Clip_Document x) = insertList i c x
  insertListClip i c (Clip_List_Dummy x) = insertList i c x
  insertListClip i c (Clip_String x) = insertList i c x
  insertListClip i c (Clip_Bool x) = insertList i c x
  insertListClip i c (Clip_Int x) = insertList i c x
  insertListClip i c (Clip_Graph x) = insertList i c x
  insertListClip i c (Clip_List_Section x) = insertList i c x
  insertListClip i c (Clip_List_Paragraph x) = insertList i c x
  insertListClip i c (Clip_List_Subsection x) = insertList i c x
  insertListClip i c (Clip_List_Subsubsection x) = insertList i c x
  insertListClip i c (Clip_List_Word x) = insertList i c x
  insertListClip i c (Clip_Subgraph x) = insertList i c x
  insertListClip i c (Clip_Dirty x) = insertList i c x
  insertListClip i c (Clip_List_Vertex x) = insertList i c x
  insertListClip i c (Clip_List_Edge x) = insertList i c x
  insertListClip i c (Clip_Shape x) = insertList i c x
  insertListClip i c (Clip_Dummy x) = insertList i c x
  insertListClip i c (Clip_Section x) = insertList i c x
  insertListClip i c (Clip_Paragraph x) = insertList i c x
  insertListClip i c (Clip_Subsection x) = insertList i c x
  insertListClip i c (Clip_Subsubsection x) = insertList i c x
  insertListClip i c (Clip_Word x) = insertList i c x
  insertListClip i c (Clip_Vertex x) = insertList i c x
  insertListClip i c (Clip_Edge x) = insertList i c x
  insertListClip i c (Clip_Nothing)   = Clip_Nothing

  removeListClip i (Clip_Root x) = removeList i x
  removeListClip i (Clip_Document x) = removeList i x
  removeListClip i (Clip_List_Dummy x) = removeList i x
  removeListClip i (Clip_String x) = removeList i x
  removeListClip i (Clip_Bool x) = removeList i x
  removeListClip i (Clip_Int x) = removeList i x
  removeListClip i (Clip_Graph x) = removeList i x
  removeListClip i (Clip_List_Section x) = removeList i x
  removeListClip i (Clip_List_Paragraph x) = removeList i x
  removeListClip i (Clip_List_Subsection x) = removeList i x
  removeListClip i (Clip_List_Subsubsection x) = removeList i x
  removeListClip i (Clip_List_Word x) = removeList i x
  removeListClip i (Clip_Subgraph x) = removeList i x
  removeListClip i (Clip_Dirty x) = removeList i x
  removeListClip i (Clip_List_Vertex x) = removeList i x
  removeListClip i (Clip_List_Edge x) = removeList i x
  removeListClip i (Clip_Shape x) = removeList i x
  removeListClip i (Clip_Dummy x) = removeList i x
  removeListClip i (Clip_Section x) = removeList i x
  removeListClip i (Clip_Paragraph x) = removeList i x
  removeListClip i (Clip_Subsection x) = removeList i x
  removeListClip i (Clip_Subsubsection x) = removeList i x
  removeListClip i (Clip_Word x) = removeList i x
  removeListClip i (Clip_Vertex x) = removeList i x
  removeListClip i (Clip_Edge x) = removeList i x
  removeListClip i (Clip_Nothing)   = Clip_Nothing


-- Editable Instances --



instance Editable Dummy Document Node ClipDoc UserToken where
  select []    x                  = Clip_Dummy x
  select (0:p) (Dummy _ x1 x2 x3 x4) = select p x1
  select (1:p) (Dummy _ x1 x2 x3 x4) = select p x2
  select (2:p) (Dummy _ x1 x2 x3 x4) = select p x3
  select (3:p) (Dummy _ x1 x2 x3 x4) = select p x4
  select _     _                  = Clip_Nothing

  paste [] (Clip_Dummy c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Dummy")   x
  paste (0:p) c (Dummy i1 x1 x2 x3 x4) = Dummy i1 (paste p c x1) x2 x3 x4
  paste (1:p) c (Dummy i1 x1 x2 x3 x4) = Dummy i1 x1 (paste p c x2) x3 x4
  paste (2:p) c (Dummy i1 x1 x2 x3 x4) = Dummy i1 x1 x2 (paste p c x3) x4
  paste (3:p) c (Dummy i1 x1 x2 x3 x4) = Dummy i1 x1 x2 x3 (paste p c x4)
  paste _  _  x                    = x

  alternatives _ = [("Dummy {Dummys} "  , Clip_Dummy $ Dummy NoIDD hole hole hole hole)
                   ,("{Dummy}", Clip_Dummy hole)
                   ]

  arity (Dummy _ x1 x2 x3 x4) = 4
  arity _                        = 0

  parseErr = ParseErrDummy

  hole = HoleDummy


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Root Document Node ClipDoc UserToken where
  select []    x                  = Clip_Root x
  select (0:p) (Root _ x1 x2 x3) = select p x1
  select (1:p) (Root _ x1 x2 x3) = select p x2
  select (2:p) (Root _ x1 x2 x3) = select p x3
  select _     _                  = Clip_Nothing

  paste [] (Clip_Root c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Root")   x
  paste (0:p) c (Root i1 x1 x2 x3) = Root i1 (paste p c x1) x2 x3
  paste (1:p) c (Root i1 x1 x2 x3) = Root i1 x1 (paste p c x2) x3
  paste (2:p) c (Root i1 x1 x2 x3) = Root i1 x1 x2 (paste p c x3)
  paste _  _  x                    = x

  alternatives _ = [("Root {Graph} {Sections} "  , Clip_Root $ Root NoIDD hole hole hole)
                   ,("{Root}", Clip_Root hole)
                   ]

  arity (Root _ x1 x2 x3) = 3
  arity _                        = 0

  parseErr = ParseErrRoot

  hole = HoleRoot


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Section Document Node ClipDoc UserToken where
  select []    x                  = Clip_Section x
  select (0:p) (Section _ x1 x2 x3) = select p x1
  select (1:p) (Section _ x1 x2 x3) = select p x2
  select (2:p) (Section _ x1 x2 x3) = select p x3
  select _     _                  = Clip_Nothing

  paste [] (Clip_Section c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Section")   x
  paste (0:p) c (Section i1 x1 x2 x3) = Section i1 (paste p c x1) x2 x3
  paste (1:p) c (Section i1 x1 x2 x3) = Section i1 x1 (paste p c x2) x3
  paste (2:p) c (Section i1 x1 x2 x3) = Section i1 x1 x2 (paste p c x3)
  paste _  _  x                    = x

  alternatives _ = [("Section {Paragraphs} {Subsections} "  , Clip_Section $ Section NoIDD hole hole hole)
                   ,("{Section}", Clip_Section hole)
                   ]

  arity (Section _ x1 x2 x3) = 3
  arity _                        = 0

  parseErr = ParseErrSection

  hole = HoleSection


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Subsection Document Node ClipDoc UserToken where
  select []    x                  = Clip_Subsection x
  select (0:p) (Subsection _ x1 x2 x3) = select p x1
  select (1:p) (Subsection _ x1 x2 x3) = select p x2
  select (2:p) (Subsection _ x1 x2 x3) = select p x3
  select _     _                  = Clip_Nothing

  paste [] (Clip_Subsection c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Subsection")   x
  paste (0:p) c (Subsection i1 x1 x2 x3) = Subsection i1 (paste p c x1) x2 x3
  paste (1:p) c (Subsection i1 x1 x2 x3) = Subsection i1 x1 (paste p c x2) x3
  paste (2:p) c (Subsection i1 x1 x2 x3) = Subsection i1 x1 x2 (paste p c x3)
  paste _  _  x                    = x

  alternatives _ = [("Subsection {Paragraphs} {Subsubsections} "  , Clip_Subsection $ Subsection NoIDD hole hole hole)
                   ,("{Subsection}", Clip_Subsection hole)
                   ]

  arity (Subsection _ x1 x2 x3) = 3
  arity _                        = 0

  parseErr = ParseErrSubsection

  hole = HoleSubsection


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Subsubsection Document Node ClipDoc UserToken where
  select []    x                  = Clip_Subsubsection x
  select (0:p) (Subsubsection _ x1 x2) = select p x1
  select (1:p) (Subsubsection _ x1 x2) = select p x2
  select _     _                  = Clip_Nothing

  paste [] (Clip_Subsubsection c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Subsubsection")   x
  paste (0:p) c (Subsubsection i1 x1 x2) = Subsubsection i1 (paste p c x1) x2
  paste (1:p) c (Subsubsection i1 x1 x2) = Subsubsection i1 x1 (paste p c x2)
  paste _  _  x                    = x

  alternatives _ = [("Subsubsection {Paragraphs} "  , Clip_Subsubsection $ Subsubsection NoIDD hole hole)
                   ,("{Subsubsection}", Clip_Subsubsection hole)
                   ]

  arity (Subsubsection _ x1 x2) = 2
  arity _                        = 0

  parseErr = ParseErrSubsubsection

  hole = HoleSubsubsection


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Paragraph Document Node ClipDoc UserToken where
  select []    x                  = Clip_Paragraph x
  select (0:p) (Paragraph _ x1) = select p x1
  select (0:p) (SubgraphPara _ x1) = select p x1
  select _     _                  = Clip_Nothing

  paste [] (Clip_Paragraph c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Paragraph")   x
  paste (0:p) c (Paragraph i1 x1) = Paragraph i1 (paste p c x1)
  paste (0:p) c (SubgraphPara i1 x1) = SubgraphPara i1 (paste p c x1)
  paste _  _  x                    = x

  alternatives _ = [("Paragraph {Words} "  , Clip_Paragraph $ Paragraph NoIDD hole)
                   ,("SubgraphPara {Subgraph} "  , Clip_Paragraph $ SubgraphPara NoIDD hole)
                   ,("{Paragraph}", Clip_Paragraph hole)
                   ]

  arity (Paragraph _ x1) = 1
  arity (SubgraphPara _ x1) = 1
  arity _                        = 0

  parseErr = ParseErrParagraph

  hole = HoleParagraph


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Word Document Node ClipDoc UserToken where
  select []    x                  = Clip_Word x
  select (0:p) (Word _ x1) = select p x1
  select (0:p) (NodeRef _ x1) = select p x1
  select _     _                  = Clip_Nothing

  paste [] (Clip_Word c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Word")   x
  paste (0:p) c (Word i1 x1) = Word i1 (paste p c x1)
  paste (0:p) c (NodeRef i1 x1) = NodeRef i1 (paste p c x1)
  paste _  _  x                    = x

  alternatives _ = [("Word "  , Clip_Word $ Word NoIDD hole)
                   ,("NodeRef "  , Clip_Word $ NodeRef NoIDD hole)
                   ,("{Word}", Clip_Word hole)
                   ]

  arity (Word _ x1) = 1
  arity (NodeRef _ x1) = 1
  arity _                        = 0

  parseErr = ParseErrWord

  hole = HoleWord


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Graph Document Node ClipDoc UserToken where
  select []    x                  = Clip_Graph x
  select (0:p) (Graph _ x1 x2 x3) = select p x1
  select (1:p) (Graph _ x1 x2 x3) = select p x2
  select (2:p) (Graph _ x1 x2 x3) = select p x3
  select _     _                  = Clip_Nothing

  paste [] (Clip_Graph c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Graph")   x
  paste (0:p) c (Graph i1 x1 x2 x3) = Graph i1 (paste p c x1) x2 x3
  paste (1:p) c (Graph i1 x1 x2 x3) = Graph i1 x1 (paste p c x2) x3
  paste (2:p) c (Graph i1 x1 x2 x3) = Graph i1 x1 x2 (paste p c x3)
  paste _  _  x                    = x

  alternatives _ = [("Graph {Dirty} {Vertexs} {Edges} "  , Clip_Graph $ Graph NoIDD hole hole hole)
                   ,("{Graph}", Clip_Graph hole)
                   ]

  arity (Graph _ x1 x2 x3) = 3
  arity _                        = 0

  parseErr = ParseErrGraph

  hole = HoleGraph


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Vertex Document Node ClipDoc UserToken where
  select []    x                  = Clip_Vertex x
  select (0:p) (Vertex _ x1 x2 x3 x4 x5) = select p x1
  select (1:p) (Vertex _ x1 x2 x3 x4 x5) = select p x2
  select (2:p) (Vertex _ x1 x2 x3 x4 x5) = select p x3
  select (3:p) (Vertex _ x1 x2 x3 x4 x5) = select p x4
  select (4:p) (Vertex _ x1 x2 x3 x4 x5) = select p x5
  select _     _                  = Clip_Nothing

  paste [] (Clip_Vertex c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Vertex")   x
  paste (0:p) c (Vertex i1 x1 x2 x3 x4 x5) = Vertex i1 (paste p c x1) x2 x3 x4 x5
  paste (1:p) c (Vertex i1 x1 x2 x3 x4 x5) = Vertex i1 x1 (paste p c x2) x3 x4 x5
  paste (2:p) c (Vertex i1 x1 x2 x3 x4 x5) = Vertex i1 x1 x2 (paste p c x3) x4 x5
  paste (3:p) c (Vertex i1 x1 x2 x3 x4 x5) = Vertex i1 x1 x2 x3 (paste p c x4) x5
  paste (4:p) c (Vertex i1 x1 x2 x3 x4 x5) = Vertex i1 x1 x2 x3 x4 (paste p c x5)
  paste _  _  x                    = x

  alternatives _ = [("Vertex {Shape} "  , Clip_Vertex $ Vertex NoIDD hole hole hole hole hole)
                   ,("{Vertex}", Clip_Vertex hole)
                   ]

  arity (Vertex _ x1 x2 x3 x4 x5) = 5
  arity _                        = 0

  parseErr = ParseErrVertex

  hole = HoleVertex


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Shape Document Node ClipDoc UserToken where
  select []    x                  = Clip_Shape x
  select _     _                  = Clip_Nothing

  paste [] (Clip_Shape c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Shape")   x
  paste _  _  x                    = x

  alternatives _ = [("Circle "  , Clip_Shape $ Circle NoIDD)
                   ,("Square "  , Clip_Shape $ Square NoIDD)
                   ,("{Shape}", Clip_Shape hole)
                   ]

  arity (Circle _) = 0
  arity (Square _) = 0
  arity _                        = 0

  parseErr = ParseErrShape

  hole = HoleShape


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Edge Document Node ClipDoc UserToken where
  select []    x                  = Clip_Edge x
  select (0:p) (Edge _ x1 x2) = select p x1
  select (1:p) (Edge _ x1 x2) = select p x2
  select _     _                  = Clip_Nothing

  paste [] (Clip_Edge c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Edge")   x
  paste (0:p) c (Edge i1 x1 x2) = Edge i1 (paste p c x1) x2
  paste (1:p) c (Edge i1 x1 x2) = Edge i1 x1 (paste p c x2)
  paste _  _  x                    = x

  alternatives _ = [("Edge "  , Clip_Edge $ Edge NoIDD hole hole)
                   ,("{Edge}", Clip_Edge hole)
                   ]

  arity (Edge _ x1 x2) = 2
  arity _                        = 0

  parseErr = ParseErrEdge

  hole = HoleEdge


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Subgraph Document Node ClipDoc UserToken where
  select []    x                  = Clip_Subgraph x
  select (0:p) (Subgraph _ x1 x2 x3) = select p x1
  select (1:p) (Subgraph _ x1 x2 x3) = select p x2
  select (2:p) (Subgraph _ x1 x2 x3) = select p x3
  select _     _                  = Clip_Nothing

  paste [] (Clip_Subgraph c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Subgraph")   x
  paste (0:p) c (Subgraph i1 x1 x2 x3) = Subgraph i1 (paste p c x1) x2 x3
  paste (1:p) c (Subgraph i1 x1 x2 x3) = Subgraph i1 x1 (paste p c x2) x3
  paste (2:p) c (Subgraph i1 x1 x2 x3) = Subgraph i1 x1 x2 (paste p c x3)
  paste _  _  x                    = x

  alternatives _ = [("Subgraph {Dirty} {Vertexs} {Edges} "  , Clip_Subgraph $ Subgraph NoIDD hole hole hole)
                   ,("{Subgraph}", Clip_Subgraph hole)
                   ]

  arity (Subgraph _ x1 x2 x3) = 3
  arity _                        = 0

  parseErr = ParseErrSubgraph

  hole = HoleSubgraph


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Dirty Document Node ClipDoc UserToken where
  select []    x                  = Clip_Dirty x
  select _     _                  = Clip_Nothing

  paste [] (Clip_Dirty c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Dirty")   x
  paste _  _  x                    = x

  alternatives _ = [("Dirty "  , Clip_Dirty $ Dirty NoIDD)
                   ,("Clean "  , Clip_Dirty $ Clean NoIDD)
                   ,("{Dirty}", Clip_Dirty hole)
                   ]

  arity (Dirty _) = 0
  arity (Clean _) = 0
  arity _                        = 0

  parseErr = ParseErrDirty

  hole = HoleDirty


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing
toList_Dummy vs = List_Dummy NoIDD (toConsList_Dummy vs)

fromList_Dummy (List_Dummy _ vs) = fromConsList_Dummy vs
fromList_Dummy _                  = []

toConsList_Dummy [] = Nil_Dummy
toConsList_Dummy (x:xs) = Cons_Dummy x (toConsList_Dummy xs)

fromConsList_Dummy Nil_Dummy = []
fromConsList_Dummy (Cons_Dummy x xs) = x: fromConsList_Dummy xs

replaceList_Dummy _ x Nil_Dummy = Nil_Dummy -- replace beyond end of list
replaceList_Dummy 0 x (Cons_Dummy cx cxs) = Cons_Dummy x cxs
replaceList_Dummy n x (Cons_Dummy cx cxs) = Cons_Dummy cx (replaceList_Dummy (n-1) x cxs)

insertList_Dummy 0 x cxs = Cons_Dummy x cxs
insertList_Dummy _ x Nil_Dummy  = Nil_Dummy   -- insert beyond end of list
insertList_Dummy n x (Cons_Dummy cx cxs) = Cons_Dummy cx (insertList_Dummy (n-1) x cxs)

removeList_Dummy _ Nil_Dummy  = Nil_Dummy -- remove beyond end of list
removeList_Dummy 0 (Cons_Dummy cx cxs) = cxs
removeList_Dummy n (Cons_Dummy cx cxs) = Cons_Dummy cx (removeList_Dummy (n-1) cxs)

instance Editable List_Dummy Document Node ClipDoc UserToken where
  select []    x                  = Clip_List_Dummy x
  select (n:p) (List_Dummy _ cxs) = let xs = fromConsList_Dummy cxs
                                  in  if n < length xs 
                                      then select p (xs !! n)
                                      else Clip_Nothing
  select _     _                  = Clip_Nothing

  paste [] (Clip_List_Dummy c) _   = c
  paste [] c  x                  = trace ("Type error: pasting "++show c++" on List_Dummy")   x
  paste (n:p) c (List_Dummy i1 cxs) = let xs = fromConsList_Dummy cxs
                                    in  if n < length xs
                                        then let x  = xs!!n
                                                 x' = paste p c x
                                             in  List_Dummy i1 (replaceList_Dummy n x' cxs)
                                        else List_Dummy i1 cxs -- paste beyond end of list
  paste _  _  x                  = x

  alternatives _ = [("{List_Dummy}", Clip_List_Dummy hole)
                   ]

  arity (List_Dummy _ x1) = length (fromConsList_Dummy x1)
  arity _                        = 0

  parseErr = ParseErrList_Dummy

  hole = List_Dummy NoIDD Nil_Dummy

  isList _ = True

  insertList n (Clip_Dummy c) (List_Dummy idd cxs) = Clip_List_Dummy $ List_Dummy idd (insertList_Dummy n c cxs)
  insertList _ _             xs = trace "Type error, no paste" $ Clip_List_Dummy xs
  insertList _ c xs                 = Clip_List_Dummy xs

  removeList n (List_Dummy idd cxs) = Clip_List_Dummy $ List_Dummy idd (removeList_Dummy n cxs)
  removeList _ xs                        = Clip_List_Dummy $ xs

toList_Section vs = List_Section NoIDD (toConsList_Section vs)

fromList_Section (List_Section _ vs) = fromConsList_Section vs
fromList_Section _                  = []

toConsList_Section [] = Nil_Section
toConsList_Section (x:xs) = Cons_Section x (toConsList_Section xs)

fromConsList_Section Nil_Section = []
fromConsList_Section (Cons_Section x xs) = x: fromConsList_Section xs

replaceList_Section _ x Nil_Section = Nil_Section -- replace beyond end of list
replaceList_Section 0 x (Cons_Section cx cxs) = Cons_Section x cxs
replaceList_Section n x (Cons_Section cx cxs) = Cons_Section cx (replaceList_Section (n-1) x cxs)

insertList_Section 0 x cxs = Cons_Section x cxs
insertList_Section _ x Nil_Section  = Nil_Section   -- insert beyond end of list
insertList_Section n x (Cons_Section cx cxs) = Cons_Section cx (insertList_Section (n-1) x cxs)

removeList_Section _ Nil_Section  = Nil_Section -- remove beyond end of list
removeList_Section 0 (Cons_Section cx cxs) = cxs
removeList_Section n (Cons_Section cx cxs) = Cons_Section cx (removeList_Section (n-1) cxs)

instance Editable List_Section Document Node ClipDoc UserToken where
  select []    x                  = Clip_List_Section x
  select (n:p) (List_Section _ cxs) = let xs = fromConsList_Section cxs
                                  in  if n < length xs 
                                      then select p (xs !! n)
                                      else Clip_Nothing
  select _     _                  = Clip_Nothing

  paste [] (Clip_List_Section c) _   = c
  paste [] c  x                  = trace ("Type error: pasting "++show c++" on List_Section")   x
  paste (n:p) c (List_Section i1 cxs) = let xs = fromConsList_Section cxs
                                    in  if n < length xs
                                        then let x  = xs!!n
                                                 x' = paste p c x
                                             in  List_Section i1 (replaceList_Section n x' cxs)
                                        else List_Section i1 cxs -- paste beyond end of list
  paste _  _  x                  = x

  alternatives _ = [("{List_Section}", Clip_List_Section hole)
                   ]

  arity (List_Section _ x1) = length (fromConsList_Section x1)
  arity _                        = 0

  parseErr = ParseErrList_Section

  hole = List_Section NoIDD Nil_Section

  isList _ = True

  insertList n (Clip_Section c) (List_Section idd cxs) = Clip_List_Section $ List_Section idd (insertList_Section n c cxs)
  insertList _ _             xs = trace "Type error, no paste" $ Clip_List_Section xs
  insertList _ c xs                 = Clip_List_Section xs

  removeList n (List_Section idd cxs) = Clip_List_Section $ List_Section idd (removeList_Section n cxs)
  removeList _ xs                        = Clip_List_Section $ xs

toList_Paragraph vs = List_Paragraph NoIDD (toConsList_Paragraph vs)

fromList_Paragraph (List_Paragraph _ vs) = fromConsList_Paragraph vs
fromList_Paragraph _                  = []

toConsList_Paragraph [] = Nil_Paragraph
toConsList_Paragraph (x:xs) = Cons_Paragraph x (toConsList_Paragraph xs)

fromConsList_Paragraph Nil_Paragraph = []
fromConsList_Paragraph (Cons_Paragraph x xs) = x: fromConsList_Paragraph xs

replaceList_Paragraph _ x Nil_Paragraph = Nil_Paragraph -- replace beyond end of list
replaceList_Paragraph 0 x (Cons_Paragraph cx cxs) = Cons_Paragraph x cxs
replaceList_Paragraph n x (Cons_Paragraph cx cxs) = Cons_Paragraph cx (replaceList_Paragraph (n-1) x cxs)

insertList_Paragraph 0 x cxs = Cons_Paragraph x cxs
insertList_Paragraph _ x Nil_Paragraph  = Nil_Paragraph   -- insert beyond end of list
insertList_Paragraph n x (Cons_Paragraph cx cxs) = Cons_Paragraph cx (insertList_Paragraph (n-1) x cxs)

removeList_Paragraph _ Nil_Paragraph  = Nil_Paragraph -- remove beyond end of list
removeList_Paragraph 0 (Cons_Paragraph cx cxs) = cxs
removeList_Paragraph n (Cons_Paragraph cx cxs) = Cons_Paragraph cx (removeList_Paragraph (n-1) cxs)

instance Editable List_Paragraph Document Node ClipDoc UserToken where
  select []    x                  = Clip_List_Paragraph x
  select (n:p) (List_Paragraph _ cxs) = let xs = fromConsList_Paragraph cxs
                                  in  if n < length xs 
                                      then select p (xs !! n)
                                      else Clip_Nothing
  select _     _                  = Clip_Nothing

  paste [] (Clip_List_Paragraph c) _   = c
  paste [] c  x                  = trace ("Type error: pasting "++show c++" on List_Paragraph")   x
  paste (n:p) c (List_Paragraph i1 cxs) = let xs = fromConsList_Paragraph cxs
                                    in  if n < length xs
                                        then let x  = xs!!n
                                                 x' = paste p c x
                                             in  List_Paragraph i1 (replaceList_Paragraph n x' cxs)
                                        else List_Paragraph i1 cxs -- paste beyond end of list
  paste _  _  x                  = x

  alternatives _ = [("{List_Paragraph}", Clip_List_Paragraph hole)
                   ]

  arity (List_Paragraph _ x1) = length (fromConsList_Paragraph x1)
  arity _                        = 0

  parseErr = ParseErrList_Paragraph

  hole = List_Paragraph NoIDD Nil_Paragraph

  isList _ = True

  insertList n (Clip_Paragraph c) (List_Paragraph idd cxs) = Clip_List_Paragraph $ List_Paragraph idd (insertList_Paragraph n c cxs)
  insertList _ _             xs = trace "Type error, no paste" $ Clip_List_Paragraph xs
  insertList _ c xs                 = Clip_List_Paragraph xs

  removeList n (List_Paragraph idd cxs) = Clip_List_Paragraph $ List_Paragraph idd (removeList_Paragraph n cxs)
  removeList _ xs                        = Clip_List_Paragraph $ xs

toList_Subsection vs = List_Subsection NoIDD (toConsList_Subsection vs)

fromList_Subsection (List_Subsection _ vs) = fromConsList_Subsection vs
fromList_Subsection _                  = []

toConsList_Subsection [] = Nil_Subsection
toConsList_Subsection (x:xs) = Cons_Subsection x (toConsList_Subsection xs)

fromConsList_Subsection Nil_Subsection = []
fromConsList_Subsection (Cons_Subsection x xs) = x: fromConsList_Subsection xs

replaceList_Subsection _ x Nil_Subsection = Nil_Subsection -- replace beyond end of list
replaceList_Subsection 0 x (Cons_Subsection cx cxs) = Cons_Subsection x cxs
replaceList_Subsection n x (Cons_Subsection cx cxs) = Cons_Subsection cx (replaceList_Subsection (n-1) x cxs)

insertList_Subsection 0 x cxs = Cons_Subsection x cxs
insertList_Subsection _ x Nil_Subsection  = Nil_Subsection   -- insert beyond end of list
insertList_Subsection n x (Cons_Subsection cx cxs) = Cons_Subsection cx (insertList_Subsection (n-1) x cxs)

removeList_Subsection _ Nil_Subsection  = Nil_Subsection -- remove beyond end of list
removeList_Subsection 0 (Cons_Subsection cx cxs) = cxs
removeList_Subsection n (Cons_Subsection cx cxs) = Cons_Subsection cx (removeList_Subsection (n-1) cxs)

instance Editable List_Subsection Document Node ClipDoc UserToken where
  select []    x                  = Clip_List_Subsection x
  select (n:p) (List_Subsection _ cxs) = let xs = fromConsList_Subsection cxs
                                  in  if n < length xs 
                                      then select p (xs !! n)
                                      else Clip_Nothing
  select _     _                  = Clip_Nothing

  paste [] (Clip_List_Subsection c) _   = c
  paste [] c  x                  = trace ("Type error: pasting "++show c++" on List_Subsection")   x
  paste (n:p) c (List_Subsection i1 cxs) = let xs = fromConsList_Subsection cxs
                                    in  if n < length xs
                                        then let x  = xs!!n
                                                 x' = paste p c x
                                             in  List_Subsection i1 (replaceList_Subsection n x' cxs)
                                        else List_Subsection i1 cxs -- paste beyond end of list
  paste _  _  x                  = x

  alternatives _ = [("{List_Subsection}", Clip_List_Subsection hole)
                   ]

  arity (List_Subsection _ x1) = length (fromConsList_Subsection x1)
  arity _                        = 0

  parseErr = ParseErrList_Subsection

  hole = List_Subsection NoIDD Nil_Subsection

  isList _ = True

  insertList n (Clip_Subsection c) (List_Subsection idd cxs) = Clip_List_Subsection $ List_Subsection idd (insertList_Subsection n c cxs)
  insertList _ _             xs = trace "Type error, no paste" $ Clip_List_Subsection xs
  insertList _ c xs                 = Clip_List_Subsection xs

  removeList n (List_Subsection idd cxs) = Clip_List_Subsection $ List_Subsection idd (removeList_Subsection n cxs)
  removeList _ xs                        = Clip_List_Subsection $ xs

toList_Subsubsection vs = List_Subsubsection NoIDD (toConsList_Subsubsection vs)

fromList_Subsubsection (List_Subsubsection _ vs) = fromConsList_Subsubsection vs
fromList_Subsubsection _                  = []

toConsList_Subsubsection [] = Nil_Subsubsection
toConsList_Subsubsection (x:xs) = Cons_Subsubsection x (toConsList_Subsubsection xs)

fromConsList_Subsubsection Nil_Subsubsection = []
fromConsList_Subsubsection (Cons_Subsubsection x xs) = x: fromConsList_Subsubsection xs

replaceList_Subsubsection _ x Nil_Subsubsection = Nil_Subsubsection -- replace beyond end of list
replaceList_Subsubsection 0 x (Cons_Subsubsection cx cxs) = Cons_Subsubsection x cxs
replaceList_Subsubsection n x (Cons_Subsubsection cx cxs) = Cons_Subsubsection cx (replaceList_Subsubsection (n-1) x cxs)

insertList_Subsubsection 0 x cxs = Cons_Subsubsection x cxs
insertList_Subsubsection _ x Nil_Subsubsection  = Nil_Subsubsection   -- insert beyond end of list
insertList_Subsubsection n x (Cons_Subsubsection cx cxs) = Cons_Subsubsection cx (insertList_Subsubsection (n-1) x cxs)

removeList_Subsubsection _ Nil_Subsubsection  = Nil_Subsubsection -- remove beyond end of list
removeList_Subsubsection 0 (Cons_Subsubsection cx cxs) = cxs
removeList_Subsubsection n (Cons_Subsubsection cx cxs) = Cons_Subsubsection cx (removeList_Subsubsection (n-1) cxs)

instance Editable List_Subsubsection Document Node ClipDoc UserToken where
  select []    x                  = Clip_List_Subsubsection x
  select (n:p) (List_Subsubsection _ cxs) = let xs = fromConsList_Subsubsection cxs
                                  in  if n < length xs 
                                      then select p (xs !! n)
                                      else Clip_Nothing
  select _     _                  = Clip_Nothing

  paste [] (Clip_List_Subsubsection c) _   = c
  paste [] c  x                  = trace ("Type error: pasting "++show c++" on List_Subsubsection")   x
  paste (n:p) c (List_Subsubsection i1 cxs) = let xs = fromConsList_Subsubsection cxs
                                    in  if n < length xs
                                        then let x  = xs!!n
                                                 x' = paste p c x
                                             in  List_Subsubsection i1 (replaceList_Subsubsection n x' cxs)
                                        else List_Subsubsection i1 cxs -- paste beyond end of list
  paste _  _  x                  = x

  alternatives _ = [("{List_Subsubsection}", Clip_List_Subsubsection hole)
                   ]

  arity (List_Subsubsection _ x1) = length (fromConsList_Subsubsection x1)
  arity _                        = 0

  parseErr = ParseErrList_Subsubsection

  hole = List_Subsubsection NoIDD Nil_Subsubsection

  isList _ = True

  insertList n (Clip_Subsubsection c) (List_Subsubsection idd cxs) = Clip_List_Subsubsection $ List_Subsubsection idd (insertList_Subsubsection n c cxs)
  insertList _ _             xs = trace "Type error, no paste" $ Clip_List_Subsubsection xs
  insertList _ c xs                 = Clip_List_Subsubsection xs

  removeList n (List_Subsubsection idd cxs) = Clip_List_Subsubsection $ List_Subsubsection idd (removeList_Subsubsection n cxs)
  removeList _ xs                        = Clip_List_Subsubsection $ xs

toList_Word vs = List_Word NoIDD (toConsList_Word vs)

fromList_Word (List_Word _ vs) = fromConsList_Word vs
fromList_Word _                  = []

toConsList_Word [] = Nil_Word
toConsList_Word (x:xs) = Cons_Word x (toConsList_Word xs)

fromConsList_Word Nil_Word = []
fromConsList_Word (Cons_Word x xs) = x: fromConsList_Word xs

replaceList_Word _ x Nil_Word = Nil_Word -- replace beyond end of list
replaceList_Word 0 x (Cons_Word cx cxs) = Cons_Word x cxs
replaceList_Word n x (Cons_Word cx cxs) = Cons_Word cx (replaceList_Word (n-1) x cxs)

insertList_Word 0 x cxs = Cons_Word x cxs
insertList_Word _ x Nil_Word  = Nil_Word   -- insert beyond end of list
insertList_Word n x (Cons_Word cx cxs) = Cons_Word cx (insertList_Word (n-1) x cxs)

removeList_Word _ Nil_Word  = Nil_Word -- remove beyond end of list
removeList_Word 0 (Cons_Word cx cxs) = cxs
removeList_Word n (Cons_Word cx cxs) = Cons_Word cx (removeList_Word (n-1) cxs)

instance Editable List_Word Document Node ClipDoc UserToken where
  select []    x                  = Clip_List_Word x
  select (n:p) (List_Word _ cxs) = let xs = fromConsList_Word cxs
                                  in  if n < length xs 
                                      then select p (xs !! n)
                                      else Clip_Nothing
  select _     _                  = Clip_Nothing

  paste [] (Clip_List_Word c) _   = c
  paste [] c  x                  = trace ("Type error: pasting "++show c++" on List_Word")   x
  paste (n:p) c (List_Word i1 cxs) = let xs = fromConsList_Word cxs
                                    in  if n < length xs
                                        then let x  = xs!!n
                                                 x' = paste p c x
                                             in  List_Word i1 (replaceList_Word n x' cxs)
                                        else List_Word i1 cxs -- paste beyond end of list
  paste _  _  x                  = x

  alternatives _ = [("{List_Word}", Clip_List_Word hole)
                   ]

  arity (List_Word _ x1) = length (fromConsList_Word x1)
  arity _                        = 0

  parseErr = ParseErrList_Word

  hole = List_Word NoIDD Nil_Word

  isList _ = True

  insertList n (Clip_Word c) (List_Word idd cxs) = Clip_List_Word $ List_Word idd (insertList_Word n c cxs)
  insertList _ _             xs = trace "Type error, no paste" $ Clip_List_Word xs
  insertList _ c xs                 = Clip_List_Word xs

  removeList n (List_Word idd cxs) = Clip_List_Word $ List_Word idd (removeList_Word n cxs)
  removeList _ xs                        = Clip_List_Word $ xs

toList_Vertex vs = List_Vertex NoIDD (toConsList_Vertex vs)

fromList_Vertex (List_Vertex _ vs) = fromConsList_Vertex vs
fromList_Vertex _                  = []

toConsList_Vertex [] = Nil_Vertex
toConsList_Vertex (x:xs) = Cons_Vertex x (toConsList_Vertex xs)

fromConsList_Vertex Nil_Vertex = []
fromConsList_Vertex (Cons_Vertex x xs) = x: fromConsList_Vertex xs

replaceList_Vertex _ x Nil_Vertex = Nil_Vertex -- replace beyond end of list
replaceList_Vertex 0 x (Cons_Vertex cx cxs) = Cons_Vertex x cxs
replaceList_Vertex n x (Cons_Vertex cx cxs) = Cons_Vertex cx (replaceList_Vertex (n-1) x cxs)

insertList_Vertex 0 x cxs = Cons_Vertex x cxs
insertList_Vertex _ x Nil_Vertex  = Nil_Vertex   -- insert beyond end of list
insertList_Vertex n x (Cons_Vertex cx cxs) = Cons_Vertex cx (insertList_Vertex (n-1) x cxs)

removeList_Vertex _ Nil_Vertex  = Nil_Vertex -- remove beyond end of list
removeList_Vertex 0 (Cons_Vertex cx cxs) = cxs
removeList_Vertex n (Cons_Vertex cx cxs) = Cons_Vertex cx (removeList_Vertex (n-1) cxs)

instance Editable List_Vertex Document Node ClipDoc UserToken where
  select []    x                  = Clip_List_Vertex x
  select (n:p) (List_Vertex _ cxs) = let xs = fromConsList_Vertex cxs
                                  in  if n < length xs 
                                      then select p (xs !! n)
                                      else Clip_Nothing
  select _     _                  = Clip_Nothing

  paste [] (Clip_List_Vertex c) _   = c
  paste [] c  x                  = trace ("Type error: pasting "++show c++" on List_Vertex")   x
  paste (n:p) c (List_Vertex i1 cxs) = let xs = fromConsList_Vertex cxs
                                    in  if n < length xs
                                        then let x  = xs!!n
                                                 x' = paste p c x
                                             in  List_Vertex i1 (replaceList_Vertex n x' cxs)
                                        else List_Vertex i1 cxs -- paste beyond end of list
  paste _  _  x                  = x

  alternatives _ = [("{List_Vertex}", Clip_List_Vertex hole)
                   ]

  arity (List_Vertex _ x1) = length (fromConsList_Vertex x1)
  arity _                        = 0

  parseErr = ParseErrList_Vertex

  hole = List_Vertex NoIDD Nil_Vertex

  isList _ = True

  insertList n (Clip_Vertex c) (List_Vertex idd cxs) = Clip_List_Vertex $ List_Vertex idd (insertList_Vertex n c cxs)
  insertList _ _             xs = trace "Type error, no paste" $ Clip_List_Vertex xs
  insertList _ c xs                 = Clip_List_Vertex xs

  removeList n (List_Vertex idd cxs) = Clip_List_Vertex $ List_Vertex idd (removeList_Vertex n cxs)
  removeList _ xs                        = Clip_List_Vertex $ xs

toList_Edge vs = List_Edge NoIDD (toConsList_Edge vs)

fromList_Edge (List_Edge _ vs) = fromConsList_Edge vs
fromList_Edge _                  = []

toConsList_Edge [] = Nil_Edge
toConsList_Edge (x:xs) = Cons_Edge x (toConsList_Edge xs)

fromConsList_Edge Nil_Edge = []
fromConsList_Edge (Cons_Edge x xs) = x: fromConsList_Edge xs

replaceList_Edge _ x Nil_Edge = Nil_Edge -- replace beyond end of list
replaceList_Edge 0 x (Cons_Edge cx cxs) = Cons_Edge x cxs
replaceList_Edge n x (Cons_Edge cx cxs) = Cons_Edge cx (replaceList_Edge (n-1) x cxs)

insertList_Edge 0 x cxs = Cons_Edge x cxs
insertList_Edge _ x Nil_Edge  = Nil_Edge   -- insert beyond end of list
insertList_Edge n x (Cons_Edge cx cxs) = Cons_Edge cx (insertList_Edge (n-1) x cxs)

removeList_Edge _ Nil_Edge  = Nil_Edge -- remove beyond end of list
removeList_Edge 0 (Cons_Edge cx cxs) = cxs
removeList_Edge n (Cons_Edge cx cxs) = Cons_Edge cx (removeList_Edge (n-1) cxs)

instance Editable List_Edge Document Node ClipDoc UserToken where
  select []    x                  = Clip_List_Edge x
  select (n:p) (List_Edge _ cxs) = let xs = fromConsList_Edge cxs
                                  in  if n < length xs 
                                      then select p (xs !! n)
                                      else Clip_Nothing
  select _     _                  = Clip_Nothing

  paste [] (Clip_List_Edge c) _   = c
  paste [] c  x                  = trace ("Type error: pasting "++show c++" on List_Edge")   x
  paste (n:p) c (List_Edge i1 cxs) = let xs = fromConsList_Edge cxs
                                    in  if n < length xs
                                        then let x  = xs!!n
                                                 x' = paste p c x
                                             in  List_Edge i1 (replaceList_Edge n x' cxs)
                                        else List_Edge i1 cxs -- paste beyond end of list
  paste _  _  x                  = x

  alternatives _ = [("{List_Edge}", Clip_List_Edge hole)
                   ]

  arity (List_Edge _ x1) = length (fromConsList_Edge x1)
  arity _                        = 0

  parseErr = ParseErrList_Edge

  hole = List_Edge NoIDD Nil_Edge

  isList _ = True

  insertList n (Clip_Edge c) (List_Edge idd cxs) = Clip_List_Edge $ List_Edge idd (insertList_Edge n c cxs)
  insertList _ _             xs = trace "Type error, no paste" $ Clip_List_Edge xs
  insertList _ c xs                 = Clip_List_Edge xs

  removeList n (List_Edge idd cxs) = Clip_List_Edge $ List_Edge idd (removeList_Edge n cxs)
  removeList _ xs                        = Clip_List_Edge $ xs

