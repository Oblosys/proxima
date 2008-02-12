module DocUtils_Generated where

import DocTypes
import DocTypes_Generated
import DocUtils
import DocumentEdit
import DocumentEdit_Generated
import PresTypes
import DebugLevels
import Text.ParserCombinators.Parsec

import CommonTypes hiding (Clean, Dirty)

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

{- ------------------------------------

 generated part

-------------------------------------- -}
rankNode :: Node -> Int
rankNode NoNode            = 0
rankNode (RootDocNode _ _) = 1
rankNode (HoleDocumentNode _ _) = 2
rankNode (RootEnrNode _ _)  = 3
rankNode (HoleEnrichedDocNode _ _)  = 4
rankNode (DummyNode _ _)  = 5
rankNode (HoleDummyNode _ _)  = 6
rankNode (RootNode _ _)  = 7
rankNode (HoleRootNode _ _)  = 8
rankNode (SectionNode _ _)  = 9
rankNode (HoleSectionNode _ _)  = 10
rankNode (SubsectionNode _ _)  = 11
rankNode (HoleSubsectionNode _ _)  = 12
rankNode (SubsubsectionNode _ _)  = 13
rankNode (HoleSubsubsectionNode _ _)  = 14
rankNode (ParagraphNode _ _)  = 15
rankNode (SubgraphParaNode _ _)  = 16
rankNode (HoleParagraphNode _ _)  = 17
rankNode (WordNode _ _)  = 18
rankNode (NodeRefNode _ _)  = 19
rankNode (LabelNode _ _)  = 20
rankNode (LabelRefNode _ _)  = 21
rankNode (HoleWordNode _ _)  = 22
rankNode (GraphNode _ _)  = 23
rankNode (HoleGraphNode _ _)  = 24
rankNode (VertexNode _ _)  = 25
rankNode (HoleVertexNode _ _)  = 26
rankNode (CircleNode _ _)  = 27
rankNode (SquareNode _ _)  = 28
rankNode (HoleShapeNode _ _)  = 29
rankNode (EdgeNode _ _)  = 30
rankNode (HoleEdgeNode _ _)  = 31
rankNode (SubgraphNode _ _)  = 32
rankNode (HoleSubgraphNode _ _)  = 33
rankNode (DirtyNode _ _)  = 34
rankNode (CleanNode _ _)  = 35
rankNode (HoleDirtyNode _ _)  = 36
rankNode (List_DummyNode _ _)  = 37
rankNode (HoleList_DummyNode _ _)  = 38
rankNode (List_SectionNode _ _)  = 39
rankNode (HoleList_SectionNode _ _)  = 40
rankNode (List_ParagraphNode _ _)  = 41
rankNode (HoleList_ParagraphNode _ _)  = 42
rankNode (List_SubsectionNode _ _)  = 43
rankNode (HoleList_SubsectionNode _ _)  = 44
rankNode (List_SubsubsectionNode _ _)  = 45
rankNode (HoleList_SubsubsectionNode _ _)  = 46
rankNode (List_WordNode _ _)  = 47
rankNode (HoleList_WordNode _ _)  = 48
rankNode (List_VertexNode _ _)  = 49
rankNode (HoleList_VertexNode _ _)  = 50
rankNode (List_EdgeNode _ _)  = 51
rankNode (HoleList_EdgeNode _ _)  = 52



instance HasPath Node where
  pathNode NoNode            = NoPathD
  pathNode (RootDocNode _ pth) = PathD pth
  pathNode (HoleDocumentNode _ pth) = PathD pth
  pathNode (RootEnrNode _ pth)  = PathD pth
  pathNode (HoleEnrichedDocNode _ pth)  = PathD pth
  pathNode (DummyNode _ pth)  = PathD pth
  pathNode (HoleDummyNode _ pth)  = PathD pth
  pathNode (RootNode _ pth)  = PathD pth
  pathNode (HoleRootNode _ pth)  = PathD pth
  pathNode (SectionNode _ pth)  = PathD pth
  pathNode (HoleSectionNode _ pth)  = PathD pth
  pathNode (SubsectionNode _ pth)  = PathD pth
  pathNode (HoleSubsectionNode _ pth)  = PathD pth
  pathNode (SubsubsectionNode _ pth)  = PathD pth
  pathNode (HoleSubsubsectionNode _ pth)  = PathD pth
  pathNode (ParagraphNode _ pth)  = PathD pth
  pathNode (SubgraphParaNode _ pth)  = PathD pth
  pathNode (HoleParagraphNode _ pth)  = PathD pth
  pathNode (WordNode _ pth)  = PathD pth
  pathNode (NodeRefNode _ pth)  = PathD pth
  pathNode (LabelNode _ pth)  = PathD pth
  pathNode (LabelRefNode _ pth)  = PathD pth
  pathNode (HoleWordNode _ pth)  = PathD pth
  pathNode (GraphNode _ pth)  = PathD pth
  pathNode (HoleGraphNode _ pth)  = PathD pth
  pathNode (VertexNode _ pth)  = PathD pth
  pathNode (HoleVertexNode _ pth)  = PathD pth
  pathNode (CircleNode _ pth)  = PathD pth
  pathNode (SquareNode _ pth)  = PathD pth
  pathNode (HoleShapeNode _ pth)  = PathD pth
  pathNode (EdgeNode _ pth)  = PathD pth
  pathNode (HoleEdgeNode _ pth)  = PathD pth
  pathNode (SubgraphNode _ pth)  = PathD pth
  pathNode (HoleSubgraphNode _ pth)  = PathD pth
  pathNode (DirtyNode _ pth)  = PathD pth
  pathNode (CleanNode _ pth)  = PathD pth
  pathNode (HoleDirtyNode _ pth)  = PathD pth
  pathNode (List_DummyNode _ pth)  = PathD pth
  pathNode (HoleList_DummyNode _ pth)  = PathD pth
  pathNode (List_SectionNode _ pth)  = PathD pth
  pathNode (HoleList_SectionNode _ pth)  = PathD pth
  pathNode (List_ParagraphNode _ pth)  = PathD pth
  pathNode (HoleList_ParagraphNode _ pth)  = PathD pth
  pathNode (List_SubsectionNode _ pth)  = PathD pth
  pathNode (HoleList_SubsectionNode _ pth)  = PathD pth
  pathNode (List_SubsubsectionNode _ pth)  = PathD pth
  pathNode (HoleList_SubsubsectionNode _ pth)  = PathD pth
  pathNode (List_WordNode _ pth)  = PathD pth
  pathNode (HoleList_WordNode _ pth)  = PathD pth
  pathNode (List_VertexNode _ pth)  = PathD pth
  pathNode (HoleList_VertexNode _ pth)  = PathD pth
  pathNode (List_EdgeNode _ pth)  = PathD pth
  pathNode (HoleList_EdgeNode _ pth)  = PathD pth



shallowShowEnrichedDoc1 (RootEnr  _ _) = "RootEnr"
shallowShowDummy1 (Dummy  _ _ _ _) = "Dummy"
shallowShowRoot1 (Root  _ _ _) = "Root"
shallowShowSection1 (Section  _ _ _) = "Section"
shallowShowSubsection1 (Subsection  _ _ _) = "Subsection"
shallowShowSubsubsection1 (Subsubsection  _ _) = "Subsubsection"
shallowShowParagraph1 (Paragraph  _) = "Paragraph"
shallowShowParagraph1 (SubgraphPara  _) = "SubgraphPara"
shallowShowWord1 (Word  _) = "Word"
shallowShowWord1 (NodeRef  _) = "NodeRef"
shallowShowWord1 (Label  _) = "Label"
shallowShowWord1 (LabelRef  _) = "LabelRef"
shallowShowGraph1 (Graph  _ _ _) = "Graph"
shallowShowVertex1 (Vertex  _ _ _ _ _) = "Vertex"
shallowShowShape1 (Circle ) = "Circle"
shallowShowShape1 (Square ) = "Square"
shallowShowEdge1 (Edge  _ _) = "Edge"
shallowShowSubgraph1 (Subgraph  _ _ _) = "Subgraph"
shallowShowDirty1 (Dirty ) = "Dirty"
shallowShowDirty1 (Clean ) = "Clean"
shallowShowList_Dummy1 (List_Dummy  _) = "List_Dummy"
shallowShowConsList_Dummy1 (Cons_Dummy  _ _) = "Cons_Dummy"
shallowShowConsList_Dummy1 (Nil_Dummy ) = "Nil_Dummy"
shallowShowList_Section1 (List_Section  _) = "List_Section"
shallowShowConsList_Section1 (Cons_Section  _ _) = "Cons_Section"
shallowShowConsList_Section1 (Nil_Section ) = "Nil_Section"
shallowShowList_Paragraph1 (List_Paragraph  _) = "List_Paragraph"
shallowShowConsList_Paragraph1 (Cons_Paragraph  _ _) = "Cons_Paragraph"
shallowShowConsList_Paragraph1 (Nil_Paragraph ) = "Nil_Paragraph"
shallowShowList_Subsection1 (List_Subsection  _) = "List_Subsection"
shallowShowConsList_Subsection1 (Cons_Subsection  _ _) = "Cons_Subsection"
shallowShowConsList_Subsection1 (Nil_Subsection ) = "Nil_Subsection"
shallowShowList_Subsubsection1 (List_Subsubsection  _) = "List_Subsubsection"
shallowShowConsList_Subsubsection1 (Cons_Subsubsection  _ _) = "Cons_Subsubsection"
shallowShowConsList_Subsubsection1 (Nil_Subsubsection ) = "Nil_Subsubsection"
shallowShowList_Word1 (List_Word  _) = "List_Word"
shallowShowConsList_Word1 (Cons_Word  _ _) = "Cons_Word"
shallowShowConsList_Word1 (Nil_Word ) = "Nil_Word"
shallowShowList_Vertex1 (List_Vertex  _) = "List_Vertex"
shallowShowConsList_Vertex1 (Cons_Vertex  _ _) = "Cons_Vertex"
shallowShowConsList_Vertex1 (Nil_Vertex ) = "Nil_Vertex"
shallowShowList_Edge1 (List_Edge  _) = "List_Edge"
shallowShowConsList_Edge1 (Cons_Edge  _ _) = "Cons_Edge"
shallowShowConsList_Edge1 (Nil_Edge ) = "Nil_Edge"



toXMLEnrichedDoc (RootEnr root document) = Elt "RootEnr" [] $ [toXMLRoot root] ++ [toXMLDocument document] ++ []
toXMLEnrichedDoc HoleEnrichedDoc = Elt "HoleEnrichedDoc" [] []
toXMLEnrichedDoc (ParseErrEnrichedDoc _) = Elt "ParseErrEnrichedDoc" [] []
toXMLDummy (Dummy dummys string bool int) = Elt "Dummy" [] $ toXMLList_Dummy dummys ++ [toXMLString string] ++ [toXMLBool bool] ++ [toXMLInt int] ++ []
toXMLDummy HoleDummy = Elt "HoleDummy" [] []
toXMLDummy (ParseErrDummy _) = Elt "ParseErrDummy" [] []
toXMLRoot (Root graph title sections) = Elt "Root" [] $ [toXMLGraph graph] ++ [toXMLString title] ++ toXMLList_Section sections ++ []
toXMLRoot HoleRoot = Elt "HoleRoot" [] []
toXMLRoot (ParseErrRoot _) = Elt "ParseErrRoot" [] []
toXMLSection (Section title paragraphs subsections) = Elt "Section" [] $ [toXMLString title] ++ toXMLList_Paragraph paragraphs ++ toXMLList_Subsection subsections ++ []
toXMLSection HoleSection = Elt "HoleSection" [] []
toXMLSection (ParseErrSection _) = Elt "ParseErrSection" [] []
toXMLSubsection (Subsection title paragraphs subsubsections) = Elt "Subsection" [] $ [toXMLString title] ++ toXMLList_Paragraph paragraphs ++ toXMLList_Subsubsection subsubsections ++ []
toXMLSubsection HoleSubsection = Elt "HoleSubsection" [] []
toXMLSubsection (ParseErrSubsection _) = Elt "ParseErrSubsection" [] []
toXMLSubsubsection (Subsubsection title paragraphs) = Elt "Subsubsection" [] $ [toXMLString title] ++ toXMLList_Paragraph paragraphs ++ []
toXMLSubsubsection HoleSubsubsection = Elt "HoleSubsubsection" [] []
toXMLSubsubsection (ParseErrSubsubsection _) = Elt "ParseErrSubsubsection" [] []
toXMLParagraph (Paragraph words) = Elt "Paragraph" [] $ toXMLList_Word words ++ []
toXMLParagraph (SubgraphPara subgraph) = Elt "SubgraphPara" [] $ [toXMLSubgraph subgraph] ++ []
toXMLParagraph HoleParagraph = Elt "HoleParagraph" [] []
toXMLParagraph (ParseErrParagraph _) = Elt "ParseErrParagraph" [] []
toXMLWord (Word word) = Elt "Word" [] $ [toXMLString word] ++ []
toXMLWord (NodeRef nodeName) = Elt "NodeRef" [] $ [toXMLString nodeName] ++ []
toXMLWord (Label label) = Elt "Label" [] $ [toXMLString label] ++ []
toXMLWord (LabelRef label) = Elt "LabelRef" [] $ [toXMLString label] ++ []
toXMLWord HoleWord = Elt "HoleWord" [] []
toXMLWord (ParseErrWord _) = Elt "ParseErrWord" [] []
toXMLGraph (Graph dirty vertices edges) = Elt "Graph" [] $ [toXMLDirty dirty] ++ toXMLList_Vertex vertices ++ toXMLList_Edge edges ++ []
toXMLGraph HoleGraph = Elt "HoleGraph" [] []
toXMLGraph (ParseErrGraph _) = Elt "ParseErrGraph" [] []
toXMLVertex (Vertex name shape id x y) = Elt "Vertex" [] $ [toXMLString name] ++ [toXMLShape shape] ++ [toXMLInt id] ++ [toXMLInt x] ++ [toXMLInt y] ++ []
toXMLVertex HoleVertex = Elt "HoleVertex" [] []
toXMLVertex (ParseErrVertex _) = Elt "ParseErrVertex" [] []
toXMLShape (Circle) = Elt "Circle" [] $ []
toXMLShape (Square) = Elt "Square" [] $ []
toXMLShape HoleShape = Elt "HoleShape" [] []
toXMLShape (ParseErrShape _) = Elt "ParseErrShape" [] []
toXMLEdge (Edge from to) = Elt "Edge" [] $ [toXMLInt from] ++ [toXMLInt to] ++ []
toXMLEdge HoleEdge = Elt "HoleEdge" [] []
toXMLEdge (ParseErrEdge _) = Elt "ParseErrEdge" [] []
toXMLSubgraph (Subgraph dirty vertices edges) = Elt "Subgraph" [] $ [toXMLDirty dirty] ++ toXMLList_Vertex vertices ++ toXMLList_Edge edges ++ []
toXMLSubgraph HoleSubgraph = Elt "HoleSubgraph" [] []
toXMLSubgraph (ParseErrSubgraph _) = Elt "ParseErrSubgraph" [] []
toXMLDirty (Dirty) = Elt "Dirty" [] $ []
toXMLDirty (Clean) = Elt "Clean" [] $ []
toXMLDirty HoleDirty = Elt "HoleDirty" [] []
toXMLDirty (ParseErrDirty _) = Elt "ParseErrDirty" [] []
toXMLList_Dummy (List_Dummy dummys) = toXMLConsList_Dummy dummys
toXMLList_Dummy HoleList_Dummy = []
toXMLList_Dummy (ParseErrList_Dummy _) = []
toXMLConsList_Dummy (Cons_Dummy dummy dummys) = toXMLDummy dummy : toXMLConsList_Dummy dummys
toXMLConsList_Dummy Nil_Dummy             = []
toXMLList_Section (List_Section sections) = toXMLConsList_Section sections
toXMLList_Section HoleList_Section = []
toXMLList_Section (ParseErrList_Section _) = []
toXMLConsList_Section (Cons_Section section sections) = toXMLSection section : toXMLConsList_Section sections
toXMLConsList_Section Nil_Section             = []
toXMLList_Paragraph (List_Paragraph paragraphs) = toXMLConsList_Paragraph paragraphs
toXMLList_Paragraph HoleList_Paragraph = []
toXMLList_Paragraph (ParseErrList_Paragraph _) = []
toXMLConsList_Paragraph (Cons_Paragraph paragraph paragraphs) = toXMLParagraph paragraph : toXMLConsList_Paragraph paragraphs
toXMLConsList_Paragraph Nil_Paragraph             = []
toXMLList_Subsection (List_Subsection subsections) = toXMLConsList_Subsection subsections
toXMLList_Subsection HoleList_Subsection = []
toXMLList_Subsection (ParseErrList_Subsection _) = []
toXMLConsList_Subsection (Cons_Subsection subsection subsections) = toXMLSubsection subsection : toXMLConsList_Subsection subsections
toXMLConsList_Subsection Nil_Subsection             = []
toXMLList_Subsubsection (List_Subsubsection subsubsections) = toXMLConsList_Subsubsection subsubsections
toXMLList_Subsubsection HoleList_Subsubsection = []
toXMLList_Subsubsection (ParseErrList_Subsubsection _) = []
toXMLConsList_Subsubsection (Cons_Subsubsection subsubsection subsubsections) = toXMLSubsubsection subsubsection : toXMLConsList_Subsubsection subsubsections
toXMLConsList_Subsubsection Nil_Subsubsection             = []
toXMLList_Word (List_Word words) = toXMLConsList_Word words
toXMLList_Word HoleList_Word = []
toXMLList_Word (ParseErrList_Word _) = []
toXMLConsList_Word (Cons_Word word words) = toXMLWord word : toXMLConsList_Word words
toXMLConsList_Word Nil_Word             = []
toXMLList_Vertex (List_Vertex vertexs) = toXMLConsList_Vertex vertexs
toXMLList_Vertex HoleList_Vertex = []
toXMLList_Vertex (ParseErrList_Vertex _) = []
toXMLConsList_Vertex (Cons_Vertex vertex vertexs) = toXMLVertex vertex : toXMLConsList_Vertex vertexs
toXMLConsList_Vertex Nil_Vertex             = []
toXMLList_Edge (List_Edge edges) = toXMLConsList_Edge edges
toXMLList_Edge HoleList_Edge = []
toXMLList_Edge (ParseErrList_Edge _) = []
toXMLConsList_Edge (Cons_Edge edge edges) = toXMLEdge edge : toXMLConsList_Edge edges
toXMLConsList_Edge Nil_Edge             = []



parseXML_EnrichedDoc = parseXMLCns_RootEnr <?|> parseHoleAndParseErr "EnrichedDoc" HoleEnrichedDoc
parseXMLCns_RootEnr = RootEnr <$ startTag "RootEnr" <*> parseXML_Root <*> parseXML_Document <* endTag "RootEnr"
parseXML_Dummy = parseXMLCns_Dummy <?|> parseHoleAndParseErr "Dummy" HoleDummy
parseXMLCns_Dummy = Dummy <$ startTag "Dummy" <*> parseXML_List_Dummy <*> parseXML_String <*> parseXML_Bool <*> parseXML_Int <* endTag "Dummy"
parseXML_Root = parseXMLCns_Root <?|> parseHoleAndParseErr "Root" HoleRoot
parseXMLCns_Root = Root <$ startTag "Root" <*> parseXML_Graph <*> parseXML_String <*> parseXML_List_Section <* endTag "Root"
parseXML_Section = parseXMLCns_Section <?|> parseHoleAndParseErr "Section" HoleSection
parseXMLCns_Section = Section <$ startTag "Section" <*> parseXML_String <*> parseXML_List_Paragraph <*> parseXML_List_Subsection <* endTag "Section"
parseXML_Subsection = parseXMLCns_Subsection <?|> parseHoleAndParseErr "Subsection" HoleSubsection
parseXMLCns_Subsection = Subsection <$ startTag "Subsection" <*> parseXML_String <*> parseXML_List_Paragraph <*> parseXML_List_Subsubsection <* endTag "Subsection"
parseXML_Subsubsection = parseXMLCns_Subsubsection <?|> parseHoleAndParseErr "Subsubsection" HoleSubsubsection
parseXMLCns_Subsubsection = Subsubsection <$ startTag "Subsubsection" <*> parseXML_String <*> parseXML_List_Paragraph <* endTag "Subsubsection"
parseXML_Paragraph = parseXMLCns_Paragraph <?|> parseXMLCns_SubgraphPara <?|> parseHoleAndParseErr "Paragraph" HoleParagraph
parseXMLCns_Paragraph = Paragraph <$ startTag "Paragraph" <*> parseXML_List_Word <* endTag "Paragraph"
parseXMLCns_SubgraphPara = SubgraphPara <$ startTag "SubgraphPara" <*> parseXML_Subgraph <* endTag "SubgraphPara"
parseXML_Word = parseXMLCns_Word <?|> parseXMLCns_NodeRef <?|> parseXMLCns_Label <?|> parseXMLCns_LabelRef <?|> parseHoleAndParseErr "Word" HoleWord
parseXMLCns_Word = Word <$ startTag "Word" <*> parseXML_String <* endTag "Word"
parseXMLCns_NodeRef = NodeRef <$ startTag "NodeRef" <*> parseXML_String <* endTag "NodeRef"
parseXMLCns_Label = Label <$ startTag "Label" <*> parseXML_String <* endTag "Label"
parseXMLCns_LabelRef = LabelRef <$ startTag "LabelRef" <*> parseXML_String <* endTag "LabelRef"
parseXML_Graph = parseXMLCns_Graph <?|> parseHoleAndParseErr "Graph" HoleGraph
parseXMLCns_Graph = Graph <$ startTag "Graph" <*> parseXML_Dirty <*> parseXML_List_Vertex <*> parseXML_List_Edge <* endTag "Graph"
parseXML_Vertex = parseXMLCns_Vertex <?|> parseHoleAndParseErr "Vertex" HoleVertex
parseXMLCns_Vertex = Vertex <$ startTag "Vertex" <*> parseXML_String <*> parseXML_Shape <*> parseXML_Int <*> parseXML_Int <*> parseXML_Int <* endTag "Vertex"
parseXML_Shape = parseXMLCns_Circle <?|> parseXMLCns_Square <?|> parseHoleAndParseErr "Shape" HoleShape
parseXMLCns_Circle = Circle <$ emptyTag "Circle"
parseXMLCns_Square = Square <$ emptyTag "Square"
parseXML_Edge = parseXMLCns_Edge <?|> parseHoleAndParseErr "Edge" HoleEdge
parseXMLCns_Edge = Edge <$ startTag "Edge" <*> parseXML_Int <*> parseXML_Int <* endTag "Edge"
parseXML_Subgraph = parseXMLCns_Subgraph <?|> parseHoleAndParseErr "Subgraph" HoleSubgraph
parseXMLCns_Subgraph = Subgraph <$ startTag "Subgraph" <*> parseXML_Dirty <*> parseXML_List_Vertex <*> parseXML_List_Edge <* endTag "Subgraph"
parseXML_Dirty = parseXMLCns_Dirty <?|> parseXMLCns_Clean <?|> parseHoleAndParseErr "Dirty" HoleDirty
parseXMLCns_Dirty = Dirty <$ emptyTag "Dirty"
parseXMLCns_Clean = Clean <$ emptyTag "Clean"
parseXML_List_Dummy = mkList List_Dummy Cons_Dummy Nil_Dummy <$> many parseXML_Dummy
parseXML_List_Section = mkList List_Section Cons_Section Nil_Section <$> many parseXML_Section
parseXML_List_Paragraph = mkList List_Paragraph Cons_Paragraph Nil_Paragraph <$> many parseXML_Paragraph
parseXML_List_Subsection = mkList List_Subsection Cons_Subsection Nil_Subsection <$> many parseXML_Subsection
parseXML_List_Subsubsection = mkList List_Subsubsection Cons_Subsubsection Nil_Subsubsection <$> many parseXML_Subsubsection
parseXML_List_Word = mkList List_Word Cons_Word Nil_Word <$> many parseXML_Word
parseXML_List_Vertex = mkList List_Vertex Cons_Vertex Nil_Vertex <$> many parseXML_Vertex
parseXML_List_Edge = mkList List_Edge Cons_Edge Nil_Edge <$> many parseXML_Edge
