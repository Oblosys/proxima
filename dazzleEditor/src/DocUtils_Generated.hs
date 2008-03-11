module DocUtils_Generated where

import Evaluation.DocTypes
import DocTypes_Generated
import Evaluation.DocUtils
import Evaluation.DocumentEdit
import Presentation.PresTypes
import Common.DebugLevels
import Text.ParserCombinators.Parsec

import Common.CommonTypes hiding (Clean, Dirty)

--instance Show Node where
--  show NoNode = "<>"
--  show (DocNode doc pth) = "<"++shallowShowDoc1 doc++","++show pth++">"
--  show (ExpNode exp pth) = "<"++shallowShowExp1 exp++","++show pth++">"
--  show _ = "<NODE>"

instance DocNode Node where
  noNode = NoNode

instance Eq Node where
  nd1 == nd2 = rankNode nd1 == rankNode nd2
  
instance Ord Node where
  nd1 <= nd2 = rankNode nd1 <= rankNode nd2

instance Doc Document where
  initialDoc = return (RootDoc (Root (Graph Clean (toList_Vertex []) (toList_Edge [])) 
                                     "" 
                                     (toList_Section [])))
  toXML = toXMLDocument
  parseXML = parseXML_Document

-- XML

-- we don't put a "RootDoc" element in the XML, because this type is not visible to the user.
toXMLDocument (RootDoc root) = toXMLRoot root
toXMLDocument _              = debug Err "DocUtils_Generated.toXMLDocument: malformed Document" $
                                 Elt "Root" [] [] -- this does not occur

parseXML_Document = RootDoc <$> parseXML_Root


-- String, Int, and Bool are unboxed types in the Document, so they can't be holes or parseErrs

toXMLBool b = Elt "Bool" [("val", show b)] []

toXMLInt i = Elt "Integer" [("val", show i)] []

toXMLString str = Elt "String" [] [PCData str] 


parseXML_String :: Parser String
parseXML_String =
 do { spaces
    ; string "<String>"
    ; str <- many (satisfy (/='<')) 
    ; string "</String>"
    ; return str
    }

parseXML_Int :: Parser Int
parseXML_Int  =
 do { spaces
    ; string "<Integer val=\""
    ; str <- many (satisfy (/='"')) 
    ; string "\"/>"
    ; return $ read str
    } 

parseXML_Bool :: Parser Bool
parseXML_Bool =
 do { spaces
    ; string "<Bool val=\""
    ; str <- many (satisfy (/='"')) 
    ; string "\"/>"
    ; return $ read str
    }





----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- rankNode                                                             --
--------------------------------------------------------------------------

rankNode :: Node -> Int
rankNode NoNode = 0
rankNode (RootDocNode _ _) = 1
rankNode (HoleDocumentNode _ _) = 2
rankNode (ParseErrDocumentNode _ _) = 3
rankNode (RootEnrNode _ _) = 4
rankNode (HoleEnrichedDocNode _ _) = 5
rankNode (ParseErrEnrichedDocNode _ _) = 6
rankNode (DummyNode _ _) = 7
rankNode (HoleDummyNode _ _) = 8
rankNode (ParseErrDummyNode _ _) = 9
rankNode (RootNode _ _) = 10
rankNode (HoleRootNode _ _) = 11
rankNode (ParseErrRootNode _ _) = 12
rankNode (SectionNode _ _) = 13
rankNode (HoleSectionNode _ _) = 14
rankNode (ParseErrSectionNode _ _) = 15
rankNode (SubsectionNode _ _) = 16
rankNode (HoleSubsectionNode _ _) = 17
rankNode (ParseErrSubsectionNode _ _) = 18
rankNode (SubsubsectionNode _ _) = 19
rankNode (HoleSubsubsectionNode _ _) = 20
rankNode (ParseErrSubsubsectionNode _ _) = 21
rankNode (ParagraphNode _ _) = 22
rankNode (SubgraphParaNode _ _) = 23
rankNode (HoleParagraphNode _ _) = 24
rankNode (ParseErrParagraphNode _ _) = 25
rankNode (WordNode _ _) = 26
rankNode (NodeRefNode _ _) = 27
rankNode (LabelNode _ _) = 28
rankNode (LabelRefNode _ _) = 29
rankNode (HoleWordNode _ _) = 30
rankNode (ParseErrWordNode _ _) = 31
rankNode (GraphNode _ _) = 32
rankNode (HoleGraphNode _ _) = 33
rankNode (ParseErrGraphNode _ _) = 34
rankNode (VertexNode _ _) = 35
rankNode (HoleVertexNode _ _) = 36
rankNode (ParseErrVertexNode _ _) = 37
rankNode (CircleNode _ _) = 38
rankNode (SquareNode _ _) = 39
rankNode (HoleShapeNode _ _) = 40
rankNode (ParseErrShapeNode _ _) = 41
rankNode (EdgeNode _ _) = 42
rankNode (HoleEdgeNode _ _) = 43
rankNode (ParseErrEdgeNode _ _) = 44
rankNode (SubgraphNode _ _) = 45
rankNode (HoleSubgraphNode _ _) = 46
rankNode (ParseErrSubgraphNode _ _) = 47
rankNode (DirtyNode _ _) = 48
rankNode (CleanNode _ _) = 49
rankNode (HoleDirtyNode _ _) = 50
rankNode (ParseErrDirtyNode _ _) = 51
rankNode (List_SectionNode _ _) = 52
rankNode (HoleList_SectionNode _ _) = 53
rankNode (ParseErrList_SectionNode _ _) = 54
rankNode (List_ParagraphNode _ _) = 55
rankNode (HoleList_ParagraphNode _ _) = 56
rankNode (ParseErrList_ParagraphNode _ _) = 57
rankNode (List_SubsectionNode _ _) = 58
rankNode (HoleList_SubsectionNode _ _) = 59
rankNode (ParseErrList_SubsectionNode _ _) = 60
rankNode (List_SubsubsectionNode _ _) = 61
rankNode (HoleList_SubsubsectionNode _ _) = 62
rankNode (ParseErrList_SubsubsectionNode _ _) = 63
rankNode (List_WordNode _ _) = 64
rankNode (HoleList_WordNode _ _) = 65
rankNode (ParseErrList_WordNode _ _) = 66
rankNode (List_VertexNode _ _) = 67
rankNode (HoleList_VertexNode _ _) = 68
rankNode (ParseErrList_VertexNode _ _) = 69
rankNode (List_EdgeNode _ _) = 70
rankNode (HoleList_EdgeNode _ _) = 71
rankNode (ParseErrList_EdgeNode _ _) = 72



--------------------------------------------------------------------------
-- HasPath instance for Node                                            --
--------------------------------------------------------------------------

instance HasPath Node where
  pathNode NoNode            = NoPathD
  pathNode (RootDocNode _ pth) = PathD pth
  pathNode (HoleDocumentNode _ pth) = PathD pth
  pathNode (ParseErrDocumentNode _ pth) = PathD pth
  pathNode (RootEnrNode _ pth) = PathD pth
  pathNode (HoleEnrichedDocNode _ pth) = PathD pth
  pathNode (ParseErrEnrichedDocNode _ pth) = PathD pth
  pathNode (DummyNode _ pth) = PathD pth
  pathNode (HoleDummyNode _ pth) = PathD pth
  pathNode (ParseErrDummyNode _ pth) = PathD pth
  pathNode (RootNode _ pth) = PathD pth
  pathNode (HoleRootNode _ pth) = PathD pth
  pathNode (ParseErrRootNode _ pth) = PathD pth
  pathNode (SectionNode _ pth) = PathD pth
  pathNode (HoleSectionNode _ pth) = PathD pth
  pathNode (ParseErrSectionNode _ pth) = PathD pth
  pathNode (SubsectionNode _ pth) = PathD pth
  pathNode (HoleSubsectionNode _ pth) = PathD pth
  pathNode (ParseErrSubsectionNode _ pth) = PathD pth
  pathNode (SubsubsectionNode _ pth) = PathD pth
  pathNode (HoleSubsubsectionNode _ pth) = PathD pth
  pathNode (ParseErrSubsubsectionNode _ pth) = PathD pth
  pathNode (ParagraphNode _ pth) = PathD pth
  pathNode (SubgraphParaNode _ pth) = PathD pth
  pathNode (HoleParagraphNode _ pth) = PathD pth
  pathNode (ParseErrParagraphNode _ pth) = PathD pth
  pathNode (WordNode _ pth) = PathD pth
  pathNode (NodeRefNode _ pth) = PathD pth
  pathNode (LabelNode _ pth) = PathD pth
  pathNode (LabelRefNode _ pth) = PathD pth
  pathNode (HoleWordNode _ pth) = PathD pth
  pathNode (ParseErrWordNode _ pth) = PathD pth
  pathNode (GraphNode _ pth) = PathD pth
  pathNode (HoleGraphNode _ pth) = PathD pth
  pathNode (ParseErrGraphNode _ pth) = PathD pth
  pathNode (VertexNode _ pth) = PathD pth
  pathNode (HoleVertexNode _ pth) = PathD pth
  pathNode (ParseErrVertexNode _ pth) = PathD pth
  pathNode (CircleNode _ pth) = PathD pth
  pathNode (SquareNode _ pth) = PathD pth
  pathNode (HoleShapeNode _ pth) = PathD pth
  pathNode (ParseErrShapeNode _ pth) = PathD pth
  pathNode (EdgeNode _ pth) = PathD pth
  pathNode (HoleEdgeNode _ pth) = PathD pth
  pathNode (ParseErrEdgeNode _ pth) = PathD pth
  pathNode (SubgraphNode _ pth) = PathD pth
  pathNode (HoleSubgraphNode _ pth) = PathD pth
  pathNode (ParseErrSubgraphNode _ pth) = PathD pth
  pathNode (DirtyNode _ pth) = PathD pth
  pathNode (CleanNode _ pth) = PathD pth
  pathNode (HoleDirtyNode _ pth) = PathD pth
  pathNode (ParseErrDirtyNode _ pth) = PathD pth
  pathNode (List_SectionNode _ pth) = PathD pth
  pathNode (HoleList_SectionNode _ pth) = PathD pth
  pathNode (ParseErrList_SectionNode _ pth) = PathD pth
  pathNode (List_ParagraphNode _ pth) = PathD pth
  pathNode (HoleList_ParagraphNode _ pth) = PathD pth
  pathNode (ParseErrList_ParagraphNode _ pth) = PathD pth
  pathNode (List_SubsectionNode _ pth) = PathD pth
  pathNode (HoleList_SubsectionNode _ pth) = PathD pth
  pathNode (ParseErrList_SubsectionNode _ pth) = PathD pth
  pathNode (List_SubsubsectionNode _ pth) = PathD pth
  pathNode (HoleList_SubsubsectionNode _ pth) = PathD pth
  pathNode (ParseErrList_SubsubsectionNode _ pth) = PathD pth
  pathNode (List_WordNode _ pth) = PathD pth
  pathNode (HoleList_WordNode _ pth) = PathD pth
  pathNode (ParseErrList_WordNode _ pth) = PathD pth
  pathNode (List_VertexNode _ pth) = PathD pth
  pathNode (HoleList_VertexNode _ pth) = PathD pth
  pathNode (ParseErrList_VertexNode _ pth) = PathD pth
  pathNode (List_EdgeNode _ pth) = PathD pth
  pathNode (HoleList_EdgeNode _ pth) = PathD pth
  pathNode (ParseErrList_EdgeNode _ pth) = PathD pth



--------------------------------------------------------------------------
-- toXML functions                                                      --
--------------------------------------------------------------------------

toXMLEnrichedDoc (RootEnr root document) = Elt "RootEnr" [] $ [toXMLRoot root] ++ [toXMLDocument document]
toXMLEnrichedDoc (HoleEnrichedDoc) = Elt "HoleEnrichedDoc" [] $ []
toXMLEnrichedDoc (ParseErrEnrichedDoc presentation) = Elt "ParseErrEnrichedDoc" [] []
toXMLDummy (Dummy dummy bool) = Elt "Dummy" [] $ [toXMLDummy dummy] ++ [toXMLBool bool]
toXMLDummy (HoleDummy) = Elt "HoleDummy" [] $ []
toXMLDummy (ParseErrDummy presentation) = Elt "ParseErrDummy" [] []
toXMLRoot (Root graph title sections) = Elt "Root" [] $ [toXMLGraph graph] ++ [toXMLString title] ++ toXMLList_Section sections
toXMLRoot (HoleRoot) = Elt "HoleRoot" [] $ []
toXMLRoot (ParseErrRoot presentation) = Elt "ParseErrRoot" [] []
toXMLSection (Section title paragraphs subsections) = Elt "Section" [] $ [toXMLString title] ++ toXMLList_Paragraph paragraphs ++ toXMLList_Subsection subsections
toXMLSection (HoleSection) = Elt "HoleSection" [] $ []
toXMLSection (ParseErrSection presentation) = Elt "ParseErrSection" [] []
toXMLSubsection (Subsection title paragraphs subsubsections) = Elt "Subsection" [] $ [toXMLString title] ++ toXMLList_Paragraph paragraphs ++ toXMLList_Subsubsection subsubsections
toXMLSubsection (HoleSubsection) = Elt "HoleSubsection" [] $ []
toXMLSubsection (ParseErrSubsection presentation) = Elt "ParseErrSubsection" [] []
toXMLSubsubsection (Subsubsection title paragraphs) = Elt "Subsubsection" [] $ [toXMLString title] ++ toXMLList_Paragraph paragraphs
toXMLSubsubsection (HoleSubsubsection) = Elt "HoleSubsubsection" [] $ []
toXMLSubsubsection (ParseErrSubsubsection presentation) = Elt "ParseErrSubsubsection" [] []
toXMLParagraph (Paragraph words) = Elt "Paragraph" [] $ toXMLList_Word words
toXMLParagraph (SubgraphPara subgraph) = Elt "SubgraphPara" [] $ [toXMLSubgraph subgraph]
toXMLParagraph (HoleParagraph) = Elt "HoleParagraph" [] $ []
toXMLParagraph (ParseErrParagraph presentation) = Elt "ParseErrParagraph" [] []
toXMLWord (Word word) = Elt "Word" [] $ [toXMLString word]
toXMLWord (NodeRef nodeName) = Elt "NodeRef" [] $ [toXMLString nodeName]
toXMLWord (Label label) = Elt "Label" [] $ [toXMLString label]
toXMLWord (LabelRef label) = Elt "LabelRef" [] $ [toXMLString label]
toXMLWord (HoleWord) = Elt "HoleWord" [] $ []
toXMLWord (ParseErrWord presentation) = Elt "ParseErrWord" [] []
toXMLGraph (Graph dirty vertices edges) = Elt "Graph" [] $ [toXMLDirty dirty] ++ toXMLList_Vertex vertices ++ toXMLList_Edge edges
toXMLGraph (HoleGraph) = Elt "HoleGraph" [] $ []
toXMLGraph (ParseErrGraph presentation) = Elt "ParseErrGraph" [] []
toXMLVertex (Vertex name shape id x y) = Elt "Vertex" [] $ [toXMLString name] ++ [toXMLShape shape] ++ [toXMLInt id] ++ [toXMLInt x] ++ [toXMLInt y]
toXMLVertex (HoleVertex) = Elt "HoleVertex" [] $ []
toXMLVertex (ParseErrVertex presentation) = Elt "ParseErrVertex" [] []
toXMLShape (Circle) = Elt "Circle" [] $ []
toXMLShape (Square) = Elt "Square" [] $ []
toXMLShape (HoleShape) = Elt "HoleShape" [] $ []
toXMLShape (ParseErrShape presentation) = Elt "ParseErrShape" [] []
toXMLEdge (Edge from to) = Elt "Edge" [] $ [toXMLInt from] ++ [toXMLInt to]
toXMLEdge (HoleEdge) = Elt "HoleEdge" [] $ []
toXMLEdge (ParseErrEdge presentation) = Elt "ParseErrEdge" [] []
toXMLSubgraph (Subgraph dirty vertices edges) = Elt "Subgraph" [] $ [toXMLDirty dirty] ++ toXMLList_Vertex vertices ++ toXMLList_Edge edges
toXMLSubgraph (HoleSubgraph) = Elt "HoleSubgraph" [] $ []
toXMLSubgraph (ParseErrSubgraph presentation) = Elt "ParseErrSubgraph" [] []
toXMLDirty (Dirty) = Elt "Dirty" [] $ []
toXMLDirty (Clean) = Elt "Clean" [] $ []
toXMLDirty (HoleDirty) = Elt "HoleDirty" [] $ []
toXMLDirty (ParseErrDirty presentation) = Elt "ParseErrDirty" [] []
toXMLList_Section (List_Section xs) = toXMLConsList_Section xs
toXMLList_Section HoleList_Section = []
toXMLList_Section (ParseErrList_Section _) = []
toXMLList_Paragraph (List_Paragraph xs) = toXMLConsList_Paragraph xs
toXMLList_Paragraph HoleList_Paragraph = []
toXMLList_Paragraph (ParseErrList_Paragraph _) = []
toXMLList_Subsection (List_Subsection xs) = toXMLConsList_Subsection xs
toXMLList_Subsection HoleList_Subsection = []
toXMLList_Subsection (ParseErrList_Subsection _) = []
toXMLList_Subsubsection (List_Subsubsection xs) = toXMLConsList_Subsubsection xs
toXMLList_Subsubsection HoleList_Subsubsection = []
toXMLList_Subsubsection (ParseErrList_Subsubsection _) = []
toXMLList_Word (List_Word xs) = toXMLConsList_Word xs
toXMLList_Word HoleList_Word = []
toXMLList_Word (ParseErrList_Word _) = []
toXMLList_Vertex (List_Vertex xs) = toXMLConsList_Vertex xs
toXMLList_Vertex HoleList_Vertex = []
toXMLList_Vertex (ParseErrList_Vertex _) = []
toXMLList_Edge (List_Edge xs) = toXMLConsList_Edge xs
toXMLList_Edge HoleList_Edge = []
toXMLList_Edge (ParseErrList_Edge _) = []
toXMLConsList_Section (Cons_Section x xs) = toXMLSection x : toXMLConsList_Section xs
toXMLConsList_Section Nil_Section             = []
toXMLConsList_Paragraph (Cons_Paragraph x xs) = toXMLParagraph x : toXMLConsList_Paragraph xs
toXMLConsList_Paragraph Nil_Paragraph             = []
toXMLConsList_Subsection (Cons_Subsection x xs) = toXMLSubsection x : toXMLConsList_Subsection xs
toXMLConsList_Subsection Nil_Subsection             = []
toXMLConsList_Subsubsection (Cons_Subsubsection x xs) = toXMLSubsubsection x : toXMLConsList_Subsubsection xs
toXMLConsList_Subsubsection Nil_Subsubsection             = []
toXMLConsList_Word (Cons_Word x xs) = toXMLWord x : toXMLConsList_Word xs
toXMLConsList_Word Nil_Word             = []
toXMLConsList_Vertex (Cons_Vertex x xs) = toXMLVertex x : toXMLConsList_Vertex xs
toXMLConsList_Vertex Nil_Vertex             = []
toXMLConsList_Edge (Cons_Edge x xs) = toXMLEdge x : toXMLConsList_Edge xs
toXMLConsList_Edge Nil_Edge             = []



--------------------------------------------------------------------------
-- parseXML functions                                                   --
--------------------------------------------------------------------------

parseXML_EnrichedDoc = parseXMLCns_RootEnr <?|> parseHoleAndParseErr "EnrichedDoc" HoleEnrichedDoc
parseXMLCns_RootEnr = RootEnr <$ startTag "RootEnr" <*> parseXML_Root <*> parseXML_Document<* endTag "RootEnr"
parseXML_Dummy = parseXMLCns_Dummy <?|> parseHoleAndParseErr "Dummy" HoleDummy
parseXMLCns_Dummy = Dummy <$ startTag "Dummy" <*> parseXML_Dummy <*> parseXML_Bool<* endTag "Dummy"
parseXML_Root = parseXMLCns_Root <?|> parseHoleAndParseErr "Root" HoleRoot
parseXMLCns_Root = Root <$ startTag "Root" <*> parseXML_Graph <*> parseXML_String <*> parseXML_List_Section<* endTag "Root"
parseXML_Section = parseXMLCns_Section <?|> parseHoleAndParseErr "Section" HoleSection
parseXMLCns_Section = Section <$ startTag "Section" <*> parseXML_String <*> parseXML_List_Paragraph <*> parseXML_List_Subsection<* endTag "Section"
parseXML_Subsection = parseXMLCns_Subsection <?|> parseHoleAndParseErr "Subsection" HoleSubsection
parseXMLCns_Subsection = Subsection <$ startTag "Subsection" <*> parseXML_String <*> parseXML_List_Paragraph <*> parseXML_List_Subsubsection<* endTag "Subsection"
parseXML_Subsubsection = parseXMLCns_Subsubsection <?|> parseHoleAndParseErr "Subsubsection" HoleSubsubsection
parseXMLCns_Subsubsection = Subsubsection <$ startTag "Subsubsection" <*> parseXML_String <*> parseXML_List_Paragraph<* endTag "Subsubsection"
parseXML_Paragraph = parseXMLCns_Paragraph <?|> parseXMLCns_SubgraphPara <?|> parseHoleAndParseErr "Paragraph" HoleParagraph
parseXMLCns_Paragraph = Paragraph <$ startTag "Paragraph" <*> parseXML_List_Word<* endTag "Paragraph"
parseXMLCns_SubgraphPara = SubgraphPara <$ startTag "SubgraphPara" <*> parseXML_Subgraph<* endTag "SubgraphPara"
parseXML_Word = parseXMLCns_Word <?|> parseXMLCns_NodeRef <?|> parseXMLCns_Label <?|> parseXMLCns_LabelRef <?|> parseHoleAndParseErr "Word" HoleWord
parseXMLCns_Word = Word <$ startTag "Word" <*> parseXML_String<* endTag "Word"
parseXMLCns_NodeRef = NodeRef <$ startTag "NodeRef" <*> parseXML_String<* endTag "NodeRef"
parseXMLCns_Label = Label <$ startTag "Label" <*> parseXML_String<* endTag "Label"
parseXMLCns_LabelRef = LabelRef <$ startTag "LabelRef" <*> parseXML_String<* endTag "LabelRef"
parseXML_Graph = parseXMLCns_Graph <?|> parseHoleAndParseErr "Graph" HoleGraph
parseXMLCns_Graph = Graph <$ startTag "Graph" <*> parseXML_Dirty <*> parseXML_List_Vertex <*> parseXML_List_Edge<* endTag "Graph"
parseXML_Vertex = parseXMLCns_Vertex <?|> parseHoleAndParseErr "Vertex" HoleVertex
parseXMLCns_Vertex = Vertex <$ startTag "Vertex" <*> parseXML_String <*> parseXML_Shape <*> parseXML_Int <*> parseXML_Int <*> parseXML_Int<* endTag "Vertex"
parseXML_Shape = parseXMLCns_Circle <?|> parseXMLCns_Square <?|> parseHoleAndParseErr "Shape" HoleShape
parseXMLCns_Circle = Circle <$ emptyTag "Circle"
parseXMLCns_Square = Square <$ emptyTag "Square"
parseXML_Edge = parseXMLCns_Edge <?|> parseHoleAndParseErr "Edge" HoleEdge
parseXMLCns_Edge = Edge <$ startTag "Edge" <*> parseXML_Int <*> parseXML_Int<* endTag "Edge"
parseXML_Subgraph = parseXMLCns_Subgraph <?|> parseHoleAndParseErr "Subgraph" HoleSubgraph
parseXMLCns_Subgraph = Subgraph <$ startTag "Subgraph" <*> parseXML_Dirty <*> parseXML_List_Vertex <*> parseXML_List_Edge<* endTag "Subgraph"
parseXML_Dirty = parseXMLCns_Dirty <?|> parseXMLCns_Clean <?|> parseHoleAndParseErr "Dirty" HoleDirty
parseXMLCns_Dirty = Dirty <$ emptyTag "Dirty"
parseXMLCns_Clean = Clean <$ emptyTag "Clean"
parseXML_List_Section = mkList List_Section Cons_Section Nil_Section <$> many parseXML_Section
parseXML_List_Paragraph = mkList List_Paragraph Cons_Paragraph Nil_Paragraph <$> many parseXML_Paragraph
parseXML_List_Subsection = mkList List_Subsection Cons_Subsection Nil_Subsection <$> many parseXML_Subsection
parseXML_List_Subsubsection = mkList List_Subsubsection Cons_Subsubsection Nil_Subsubsection <$> many parseXML_Subsubsection
parseXML_List_Word = mkList List_Word Cons_Word Nil_Word <$> many parseXML_Word
parseXML_List_Vertex = mkList List_Vertex Cons_Vertex Nil_Vertex <$> many parseXML_Vertex
parseXML_List_Edge = mkList List_Edge Cons_Edge Nil_Edge <$> many parseXML_Edge


toList_Section vs = List_Section (toConsList_Section vs)

fromList_Section (List_Section vs) = fromConsList_Section vs
fromList_Section _ = []

toConsList_Section [] = Nil_Section
toConsList_Section (x:xs) = Cons_Section x (toConsList_Section xs)

fromConsList_Section Nil_Section = []
fromConsList_Section (Cons_Section x xs) = x: fromConsList_Section xs

replaceList_Section _ x Nil_Section = Nil_Section  -- replace beyond end of list
replaceList_Section 0 x (Cons_Section cx cxs) = Cons_Section x cxs
replaceList_Section n x (Cons_Section cx cxs) = Cons_Section cx (replaceList_Section (n-1) x cxs)

insertList_Section 0 x cxs = Cons_Section x cxs
insertList_Section _ x Nil_Section  = Nil_Section  -- insert beyond end of list
insertList_Section n x (Cons_Section cx cxs) = Cons_Section cx (insertList_Section (n-1) x cxs)

removeList_Section _ Nil_Section  = Nil_Section  -- remove beyond end of list
removeList_Section 0 (Cons_Section cx cxs) = cxs
removeList_Section n (Cons_Section cx cxs) = Cons_Section cx (removeList_Section (n-1) cxs)

toList_Paragraph vs = List_Paragraph (toConsList_Paragraph vs)

fromList_Paragraph (List_Paragraph vs) = fromConsList_Paragraph vs
fromList_Paragraph _ = []

toConsList_Paragraph [] = Nil_Paragraph
toConsList_Paragraph (x:xs) = Cons_Paragraph x (toConsList_Paragraph xs)

fromConsList_Paragraph Nil_Paragraph = []
fromConsList_Paragraph (Cons_Paragraph x xs) = x: fromConsList_Paragraph xs

replaceList_Paragraph _ x Nil_Paragraph = Nil_Paragraph  -- replace beyond end of list
replaceList_Paragraph 0 x (Cons_Paragraph cx cxs) = Cons_Paragraph x cxs
replaceList_Paragraph n x (Cons_Paragraph cx cxs) = Cons_Paragraph cx (replaceList_Paragraph (n-1) x cxs)

insertList_Paragraph 0 x cxs = Cons_Paragraph x cxs
insertList_Paragraph _ x Nil_Paragraph  = Nil_Paragraph  -- insert beyond end of list
insertList_Paragraph n x (Cons_Paragraph cx cxs) = Cons_Paragraph cx (insertList_Paragraph (n-1) x cxs)

removeList_Paragraph _ Nil_Paragraph  = Nil_Paragraph  -- remove beyond end of list
removeList_Paragraph 0 (Cons_Paragraph cx cxs) = cxs
removeList_Paragraph n (Cons_Paragraph cx cxs) = Cons_Paragraph cx (removeList_Paragraph (n-1) cxs)

toList_Subsection vs = List_Subsection (toConsList_Subsection vs)

fromList_Subsection (List_Subsection vs) = fromConsList_Subsection vs
fromList_Subsection _ = []

toConsList_Subsection [] = Nil_Subsection
toConsList_Subsection (x:xs) = Cons_Subsection x (toConsList_Subsection xs)

fromConsList_Subsection Nil_Subsection = []
fromConsList_Subsection (Cons_Subsection x xs) = x: fromConsList_Subsection xs

replaceList_Subsection _ x Nil_Subsection = Nil_Subsection  -- replace beyond end of list
replaceList_Subsection 0 x (Cons_Subsection cx cxs) = Cons_Subsection x cxs
replaceList_Subsection n x (Cons_Subsection cx cxs) = Cons_Subsection cx (replaceList_Subsection (n-1) x cxs)

insertList_Subsection 0 x cxs = Cons_Subsection x cxs
insertList_Subsection _ x Nil_Subsection  = Nil_Subsection  -- insert beyond end of list
insertList_Subsection n x (Cons_Subsection cx cxs) = Cons_Subsection cx (insertList_Subsection (n-1) x cxs)

removeList_Subsection _ Nil_Subsection  = Nil_Subsection  -- remove beyond end of list
removeList_Subsection 0 (Cons_Subsection cx cxs) = cxs
removeList_Subsection n (Cons_Subsection cx cxs) = Cons_Subsection cx (removeList_Subsection (n-1) cxs)

toList_Subsubsection vs = List_Subsubsection (toConsList_Subsubsection vs)

fromList_Subsubsection (List_Subsubsection vs) = fromConsList_Subsubsection vs
fromList_Subsubsection _ = []

toConsList_Subsubsection [] = Nil_Subsubsection
toConsList_Subsubsection (x:xs) = Cons_Subsubsection x (toConsList_Subsubsection xs)

fromConsList_Subsubsection Nil_Subsubsection = []
fromConsList_Subsubsection (Cons_Subsubsection x xs) = x: fromConsList_Subsubsection xs

replaceList_Subsubsection _ x Nil_Subsubsection = Nil_Subsubsection  -- replace beyond end of list
replaceList_Subsubsection 0 x (Cons_Subsubsection cx cxs) = Cons_Subsubsection x cxs
replaceList_Subsubsection n x (Cons_Subsubsection cx cxs) = Cons_Subsubsection cx (replaceList_Subsubsection (n-1) x cxs)

insertList_Subsubsection 0 x cxs = Cons_Subsubsection x cxs
insertList_Subsubsection _ x Nil_Subsubsection  = Nil_Subsubsection  -- insert beyond end of list
insertList_Subsubsection n x (Cons_Subsubsection cx cxs) = Cons_Subsubsection cx (insertList_Subsubsection (n-1) x cxs)

removeList_Subsubsection _ Nil_Subsubsection  = Nil_Subsubsection  -- remove beyond end of list
removeList_Subsubsection 0 (Cons_Subsubsection cx cxs) = cxs
removeList_Subsubsection n (Cons_Subsubsection cx cxs) = Cons_Subsubsection cx (removeList_Subsubsection (n-1) cxs)

toList_Word vs = List_Word (toConsList_Word vs)

fromList_Word (List_Word vs) = fromConsList_Word vs
fromList_Word _ = []

toConsList_Word [] = Nil_Word
toConsList_Word (x:xs) = Cons_Word x (toConsList_Word xs)

fromConsList_Word Nil_Word = []
fromConsList_Word (Cons_Word x xs) = x: fromConsList_Word xs

replaceList_Word _ x Nil_Word = Nil_Word  -- replace beyond end of list
replaceList_Word 0 x (Cons_Word cx cxs) = Cons_Word x cxs
replaceList_Word n x (Cons_Word cx cxs) = Cons_Word cx (replaceList_Word (n-1) x cxs)

insertList_Word 0 x cxs = Cons_Word x cxs
insertList_Word _ x Nil_Word  = Nil_Word  -- insert beyond end of list
insertList_Word n x (Cons_Word cx cxs) = Cons_Word cx (insertList_Word (n-1) x cxs)

removeList_Word _ Nil_Word  = Nil_Word  -- remove beyond end of list
removeList_Word 0 (Cons_Word cx cxs) = cxs
removeList_Word n (Cons_Word cx cxs) = Cons_Word cx (removeList_Word (n-1) cxs)

toList_Vertex vs = List_Vertex (toConsList_Vertex vs)

fromList_Vertex (List_Vertex vs) = fromConsList_Vertex vs
fromList_Vertex _ = []

toConsList_Vertex [] = Nil_Vertex
toConsList_Vertex (x:xs) = Cons_Vertex x (toConsList_Vertex xs)

fromConsList_Vertex Nil_Vertex = []
fromConsList_Vertex (Cons_Vertex x xs) = x: fromConsList_Vertex xs

replaceList_Vertex _ x Nil_Vertex = Nil_Vertex  -- replace beyond end of list
replaceList_Vertex 0 x (Cons_Vertex cx cxs) = Cons_Vertex x cxs
replaceList_Vertex n x (Cons_Vertex cx cxs) = Cons_Vertex cx (replaceList_Vertex (n-1) x cxs)

insertList_Vertex 0 x cxs = Cons_Vertex x cxs
insertList_Vertex _ x Nil_Vertex  = Nil_Vertex  -- insert beyond end of list
insertList_Vertex n x (Cons_Vertex cx cxs) = Cons_Vertex cx (insertList_Vertex (n-1) x cxs)

removeList_Vertex _ Nil_Vertex  = Nil_Vertex  -- remove beyond end of list
removeList_Vertex 0 (Cons_Vertex cx cxs) = cxs
removeList_Vertex n (Cons_Vertex cx cxs) = Cons_Vertex cx (removeList_Vertex (n-1) cxs)

toList_Edge vs = List_Edge (toConsList_Edge vs)

fromList_Edge (List_Edge vs) = fromConsList_Edge vs
fromList_Edge _ = []

toConsList_Edge [] = Nil_Edge
toConsList_Edge (x:xs) = Cons_Edge x (toConsList_Edge xs)

fromConsList_Edge Nil_Edge = []
fromConsList_Edge (Cons_Edge x xs) = x: fromConsList_Edge xs

replaceList_Edge _ x Nil_Edge = Nil_Edge  -- replace beyond end of list
replaceList_Edge 0 x (Cons_Edge cx cxs) = Cons_Edge x cxs
replaceList_Edge n x (Cons_Edge cx cxs) = Cons_Edge cx (replaceList_Edge (n-1) x cxs)

insertList_Edge 0 x cxs = Cons_Edge x cxs
insertList_Edge _ x Nil_Edge  = Nil_Edge  -- insert beyond end of list
insertList_Edge n x (Cons_Edge cx cxs) = Cons_Edge cx (insertList_Edge (n-1) x cxs)

removeList_Edge _ Nil_Edge  = Nil_Edge  -- remove beyond end of list
removeList_Edge 0 (Cons_Edge cx cxs) = cxs
removeList_Edge n (Cons_Edge cx cxs) = Cons_Edge cx (removeList_Edge (n-1) cxs)

