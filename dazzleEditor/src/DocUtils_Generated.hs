module DocUtils_Generated where

import DocTypes
import DocTypes_Generated
import DocUtils
import PresTypes
import Text.ParserCombinators.Parsec

--instance Show Node where
--  show NoNode = "<>"
--  show (DocNode doc pth) = "<"++shallowShowDoc1 doc++","++show pth++">"
--  show (ExpNode exp pth) = "<"++shallowShowExp1 exp++","++show pth++">"
--  show _ = "<NODE>"

instance Eq Node where
  nd1 == nd2 = rankNode nd1 == rankNode nd2
  
instance Ord Node where
  nd1 <= nd2 = rankNode nd1 <= rankNode nd2


-- XML

toXMLRootDoc (RootDoc _ root)  = toXMLRoot root
toXMLRootDoc _                 = Elt "ErrRoot" [] []

  

---- deconstructors for boxed primitive types

string_ :: String_ -> String
string_ (String_ _ str) = str
string_ _ = ""

bool_ :: Bool_ -> Bool
bool_ (Bool_ _ b) = b
bool_ _ = False

int_ :: Int_ -> Int
int_ (Int_ _ i) = i
int_ _ = 0




-- completely unclear why this one is not necessary in heliumEditor
-- somehow the generator generates different code there.
toXMLDocument document = Elt "Document" [] []

parseXML_Document = RootDoc NoIDD <$ startTag "RootEnr" <*> parseXML_Root  <* endTag "RootEnr"



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
rankNode (String_Node _ _)  = 5
rankNode (HoleString_Node _ _)  = 6
rankNode (Bool_Node _ _)  = 7
rankNode (HoleBool_Node _ _)  = 8
rankNode (Int_Node _ _)  = 9
rankNode (HoleInt_Node _ _)  = 10
rankNode (DummyNode _ _)  = 11
rankNode (HoleDummyNode _ _)  = 12
rankNode (RootNode _ _)  = 13
rankNode (HoleRootNode _ _)  = 14
rankNode (BinNode _ _)  = 15
rankNode (LeafNode _ _)  = 16
rankNode (HoleTreeNode _ _)  = 17
rankNode (SectionNode _ _)  = 18
rankNode (HoleSectionNode _ _)  = 19
rankNode (SubsectionNode _ _)  = 20
rankNode (HoleSubsectionNode _ _)  = 21
rankNode (SubsubsectionNode _ _)  = 22
rankNode (HoleSubsubsectionNode _ _)  = 23
rankNode (ParagraphNode _ _)  = 24
rankNode (HoleParagraphNode _ _)  = 25
rankNode (WordNode _ _)  = 26
rankNode (HoleWordNode _ _)  = 27
rankNode (GraphNode _ _)  = 28
rankNode (HoleGraphNode _ _)  = 29
rankNode (VertexNode _ _)  = 30
rankNode (HoleVertexNode _ _)  = 31
rankNode (CircleNode _ _)  = 32
rankNode (SquareNode _ _)  = 33
rankNode (HoleShapeNode _ _)  = 34
rankNode (EdgeNode _ _)  = 35
rankNode (HoleEdgeNode _ _)  = 36
rankNode (SubgraphNode _ _)  = 37
rankNode (HoleSubgraphNode _ _)  = 38
rankNode (DirtyNode _ _)  = 39
rankNode (CleanNode _ _)  = 40
rankNode (HoleDirtyNode _ _)  = 41
rankNode (List_DummyNode _ _)  = 42
rankNode (HoleList_DummyNode _ _)  = 43
rankNode (List_SectionNode _ _)  = 44
rankNode (HoleList_SectionNode _ _)  = 45
rankNode (List_ParagraphNode _ _)  = 46
rankNode (HoleList_ParagraphNode _ _)  = 47
rankNode (List_SubsectionNode _ _)  = 48
rankNode (HoleList_SubsectionNode _ _)  = 49
rankNode (List_SubsubsectionNode _ _)  = 50
rankNode (HoleList_SubsubsectionNode _ _)  = 51
rankNode (List_WordNode _ _)  = 52
rankNode (HoleList_WordNode _ _)  = 53
rankNode (List_VertexNode _ _)  = 54
rankNode (HoleList_VertexNode _ _)  = 55
rankNode (List_EdgeNode _ _)  = 56
rankNode (HoleList_EdgeNode _ _)  = 57



instance HasPath Node where
  pathNode NoNode            = NoPathD
  pathNode (RootDocNode _ pth) = PathD pth
  pathNode (HoleDocumentNode _ pth) = PathD pth
  pathNode (RootEnrNode _ pth)  = PathD pth
  pathNode (HoleEnrichedDocNode _ pth)  = PathD pth
  pathNode (String_Node _ pth)  = PathD pth
  pathNode (HoleString_Node _ pth)  = PathD pth
  pathNode (Bool_Node _ pth)  = PathD pth
  pathNode (HoleBool_Node _ pth)  = PathD pth
  pathNode (Int_Node _ pth)  = PathD pth
  pathNode (HoleInt_Node _ pth)  = PathD pth
  pathNode (DummyNode _ pth)  = PathD pth
  pathNode (HoleDummyNode _ pth)  = PathD pth
  pathNode (RootNode _ pth)  = PathD pth
  pathNode (HoleRootNode _ pth)  = PathD pth
  pathNode (BinNode _ pth)  = PathD pth
  pathNode (LeafNode _ pth)  = PathD pth
  pathNode (HoleTreeNode _ pth)  = PathD pth
  pathNode (SectionNode _ pth)  = PathD pth
  pathNode (HoleSectionNode _ pth)  = PathD pth
  pathNode (SubsectionNode _ pth)  = PathD pth
  pathNode (HoleSubsectionNode _ pth)  = PathD pth
  pathNode (SubsubsectionNode _ pth)  = PathD pth
  pathNode (HoleSubsubsectionNode _ pth)  = PathD pth
  pathNode (ParagraphNode _ pth)  = PathD pth
  pathNode (HoleParagraphNode _ pth)  = PathD pth
  pathNode (WordNode _ pth)  = PathD pth
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



rootEnrIDD :: Node -> Maybe IDD
rootEnrIDD (RootEnrNode (RootEnr iDD _ _) _) = Just iDD
rootEnrIDD _                                   = Nothing

string_IDD :: Node -> Maybe IDD
string_IDD (String_Node (String_ iDD _) _) = Just iDD
string_IDD _                                   = Nothing

bool_IDD :: Node -> Maybe IDD
bool_IDD (Bool_Node (Bool_ iDD _) _) = Just iDD
bool_IDD _                                   = Nothing

int_IDD :: Node -> Maybe IDD
int_IDD (Int_Node (Int_ iDD _) _) = Just iDD
int_IDD _                                   = Nothing

dummyIDD :: Node -> Maybe IDD
dummyIDD (DummyNode (Dummy iDD _ _ _ _) _) = Just iDD
dummyIDD _                                   = Nothing

rootIDD :: Node -> Maybe IDD
rootIDD (RootNode (Root iDD _ _ _) _) = Just iDD
rootIDD _                                   = Nothing

binIDD :: Node -> Maybe IDD
binIDD (BinNode (Bin iDD _ _) _) = Just iDD
binIDD _                                   = Nothing

leafIDD :: Node -> Maybe IDD
leafIDD (LeafNode (Leaf iDD) _) = Just iDD
leafIDD _                                   = Nothing

sectionIDD :: Node -> Maybe IDD
sectionIDD (SectionNode (Section iDD _ _ _ _) _) = Just iDD
sectionIDD _                                   = Nothing

subsectionIDD :: Node -> Maybe IDD
subsectionIDD (SubsectionNode (Subsection iDD _ _ _) _) = Just iDD
subsectionIDD _                                   = Nothing

subsubsectionIDD :: Node -> Maybe IDD
subsubsectionIDD (SubsubsectionNode (Subsubsection iDD _ _) _) = Just iDD
subsubsectionIDD _                                   = Nothing

paragraphIDD :: Node -> Maybe IDD
paragraphIDD (ParagraphNode (Paragraph iDD _) _) = Just iDD
paragraphIDD _                                   = Nothing

wordIDD :: Node -> Maybe IDD
wordIDD (WordNode (Word iDD _) _) = Just iDD
wordIDD _                                   = Nothing

graphIDD :: Node -> Maybe IDD
graphIDD (GraphNode (Graph iDD _ _ _) _) = Just iDD
graphIDD _                                   = Nothing

vertexIDD :: Node -> Maybe IDD
vertexIDD (VertexNode (Vertex iDD _ _ _ _ _) _) = Just iDD
vertexIDD _                                   = Nothing

circleIDD :: Node -> Maybe IDD
circleIDD (CircleNode (Circle iDD) _) = Just iDD
circleIDD _                                   = Nothing

squareIDD :: Node -> Maybe IDD
squareIDD (SquareNode (Square iDD) _) = Just iDD
squareIDD _                                   = Nothing

edgeIDD :: Node -> Maybe IDD
edgeIDD (EdgeNode (Edge iDD _ _) _) = Just iDD
edgeIDD _                                   = Nothing

subgraphIDD :: Node -> Maybe IDD
subgraphIDD (SubgraphNode (Subgraph iDD _ _ _) _) = Just iDD
subgraphIDD _                                   = Nothing

dirtyIDD :: Node -> Maybe IDD
dirtyIDD (DirtyNode (Dirty iDD) _) = Just iDD
dirtyIDD _                                   = Nothing

cleanIDD :: Node -> Maybe IDD
cleanIDD (CleanNode (Clean iDD) _) = Just iDD
cleanIDD _                                   = Nothing




shallowShowEnrichedDoc1 (RootEnr  _ _ _) = "RootEnr"
shallowShowString_1 (String_  _ _) = "String_"
shallowShowBool_1 (Bool_  _ _) = "Bool_"
shallowShowInt_1 (Int_  _ _) = "Int_"
shallowShowDummy1 (Dummy  _ _ _ _ _) = "Dummy"
shallowShowRoot1 (Root  _ _ _ _) = "Root"
shallowShowTree1 (Bin  _ _ _) = "Bin"
shallowShowTree1 (Leaf  _) = "Leaf"
shallowShowSection1 (Section  _ _ _ _ _) = "Section"
shallowShowSubsection1 (Subsection  _ _ _ _) = "Subsection"
shallowShowSubsubsection1 (Subsubsection  _ _ _) = "Subsubsection"
shallowShowParagraph1 (Paragraph  _ _) = "Paragraph"
shallowShowWord1 (Word  _ _) = "Word"
shallowShowGraph1 (Graph  _ _ _ _) = "Graph"
shallowShowVertex1 (Vertex  _ _ _ _ _ _) = "Vertex"
shallowShowShape1 (Circle  _) = "Circle"
shallowShowShape1 (Square  _) = "Square"
shallowShowEdge1 (Edge  _ _ _) = "Edge"
shallowShowSubgraph1 (Subgraph  _ _ _ _) = "Subgraph"
shallowShowDirty1 (Dirty  _) = "Dirty"
shallowShowDirty1 (Clean  _) = "Clean"
shallowShowList_Dummy1 (List_Dummy  _ _) = "List_Dummy"
shallowShowConsList_Dummy1 (Cons_Dummy  _ _) = "Cons_Dummy"
shallowShowConsList_Dummy1 (Nil_Dummy ) = "Nil_Dummy"
shallowShowList_Section1 (List_Section  _ _) = "List_Section"
shallowShowConsList_Section1 (Cons_Section  _ _) = "Cons_Section"
shallowShowConsList_Section1 (Nil_Section ) = "Nil_Section"
shallowShowList_Paragraph1 (List_Paragraph  _ _) = "List_Paragraph"
shallowShowConsList_Paragraph1 (Cons_Paragraph  _ _) = "Cons_Paragraph"
shallowShowConsList_Paragraph1 (Nil_Paragraph ) = "Nil_Paragraph"
shallowShowList_Subsection1 (List_Subsection  _ _) = "List_Subsection"
shallowShowConsList_Subsection1 (Cons_Subsection  _ _) = "Cons_Subsection"
shallowShowConsList_Subsection1 (Nil_Subsection ) = "Nil_Subsection"
shallowShowList_Subsubsection1 (List_Subsubsection  _ _) = "List_Subsubsection"
shallowShowConsList_Subsubsection1 (Cons_Subsubsection  _ _) = "Cons_Subsubsection"
shallowShowConsList_Subsubsection1 (Nil_Subsubsection ) = "Nil_Subsubsection"
shallowShowList_Word1 (List_Word  _ _) = "List_Word"
shallowShowConsList_Word1 (Cons_Word  _ _) = "Cons_Word"
shallowShowConsList_Word1 (Nil_Word ) = "Nil_Word"
shallowShowList_Vertex1 (List_Vertex  _ _) = "List_Vertex"
shallowShowConsList_Vertex1 (Cons_Vertex  _ _) = "Cons_Vertex"
shallowShowConsList_Vertex1 (Nil_Vertex ) = "Nil_Vertex"
shallowShowList_Edge1 (List_Edge  _ _) = "List_Edge"
shallowShowConsList_Edge1 (Cons_Edge  _ _) = "Cons_Edge"
shallowShowConsList_Edge1 (Nil_Edge ) = "Nil_Edge"



toXMLEnrichedDoc (RootEnr _ root document) = Elt "RootEnr" [] $ [toXMLRoot root] ++ [toXMLDocument document] ++ []
toXMLString_ (String_ _ string) = Elt "String_" [] $ [toXMLString string] ++ []
toXMLBool_ (Bool_ _ bool) = Elt "Bool_" [] $ [toXMLBool bool] ++ []
toXMLInt_ (Int_ _ int) = Elt "Int_" [] $ [toXMLInt int] ++ []
toXMLDummy (Dummy _ dummys string_ bool_ int_) = Elt "Dummy" [] $ toXMLList_Dummy dummys ++ [toXMLString_ string_] ++ [toXMLBool_ bool_] ++ [toXMLInt_ int_] ++ []
toXMLRoot (Root _ tree graph sections) = Elt "Root" [] $ [toXMLTree tree] ++ [toXMLGraph graph] ++ toXMLList_Section sections ++ []
toXMLTree (Bin _ left right) = Elt "Bin" [] $ [toXMLTree left] ++ [toXMLTree right] ++ []
toXMLTree (Leaf _) = Elt "Leaf" [] $ []
toXMLSection (Section _ title paragraphs subsections subgraph) = Elt "Section" [] $ [toXMLString_ title] ++ toXMLList_Paragraph paragraphs ++ toXMLList_Subsection subsections ++ [toXMLSubgraph subgraph] ++ []
toXMLSubsection (Subsection _ title paragraphs subsubsections) = Elt "Subsection" [] $ [toXMLString_ title] ++ toXMLList_Paragraph paragraphs ++ toXMLList_Subsubsection subsubsections ++ []
toXMLSubsubsection (Subsubsection _ title paragraphs) = Elt "Subsubsection" [] $ [toXMLString_ title] ++ toXMLList_Paragraph paragraphs ++ []
toXMLParagraph (Paragraph _ words) = Elt "Paragraph" [] $ toXMLList_Word words ++ []
toXMLWord (Word _ word) = Elt "Word" [] $ [toXMLString_ word] ++ []
toXMLGraph (Graph _ dirty vertices edges) = Elt "Graph" [] $ [toXMLDirty dirty] ++ toXMLList_Vertex vertices ++ toXMLList_Edge edges ++ []
toXMLVertex (Vertex _ name shape id x y) = Elt "Vertex" [] $ [toXMLString_ name] ++ [toXMLShape shape] ++ [toXMLInt_ id] ++ [toXMLInt_ x] ++ [toXMLInt_ y] ++ []
toXMLShape (Circle _) = Elt "Circle" [] $ []
toXMLShape (Square _) = Elt "Square" [] $ []
toXMLEdge (Edge _ from to) = Elt "Edge" [] $ [toXMLInt_ from] ++ [toXMLInt_ to] ++ []
toXMLSubgraph (Subgraph _ dirty vertices edges) = Elt "Subgraph" [] $ [toXMLDirty dirty] ++ toXMLList_Vertex vertices ++ toXMLList_Edge edges ++ []
toXMLDirty (Dirty _) = Elt "Dirty" [] $ []
toXMLDirty (Clean _) = Elt "Clean" [] $ []
toXMLList_Dummy (List_Dummy _ dummys) = toXMLConsList_Dummy dummys
toXMLConsList_Dummy (Cons_Dummy dummy dummys) = toXMLDummy dummy : toXMLConsList_Dummy dummys
toXMLConsList_Dummy Nil_Dummy             = []
toXMLList_Section (List_Section _ sections) = toXMLConsList_Section sections
toXMLConsList_Section (Cons_Section section sections) = toXMLSection section : toXMLConsList_Section sections
toXMLConsList_Section Nil_Section             = []
toXMLList_Paragraph (List_Paragraph _ paragraphs) = toXMLConsList_Paragraph paragraphs
toXMLConsList_Paragraph (Cons_Paragraph paragraph paragraphs) = toXMLParagraph paragraph : toXMLConsList_Paragraph paragraphs
toXMLConsList_Paragraph Nil_Paragraph             = []
toXMLList_Subsection (List_Subsection _ subsections) = toXMLConsList_Subsection subsections
toXMLConsList_Subsection (Cons_Subsection subsection subsections) = toXMLSubsection subsection : toXMLConsList_Subsection subsections
toXMLConsList_Subsection Nil_Subsection             = []
toXMLList_Subsubsection (List_Subsubsection _ subsubsections) = toXMLConsList_Subsubsection subsubsections
toXMLConsList_Subsubsection (Cons_Subsubsection subsubsection subsubsections) = toXMLSubsubsection subsubsection : toXMLConsList_Subsubsection subsubsections
toXMLConsList_Subsubsection Nil_Subsubsection             = []
toXMLList_Word (List_Word _ words) = toXMLConsList_Word words
toXMLConsList_Word (Cons_Word word words) = toXMLWord word : toXMLConsList_Word words
toXMLConsList_Word Nil_Word             = []
toXMLList_Vertex (List_Vertex _ vertexs) = toXMLConsList_Vertex vertexs
toXMLConsList_Vertex (Cons_Vertex vertex vertexs) = toXMLVertex vertex : toXMLConsList_Vertex vertexs
toXMLConsList_Vertex Nil_Vertex             = []
toXMLList_Edge (List_Edge _ edges) = toXMLConsList_Edge edges
toXMLConsList_Edge (Cons_Edge edge edges) = toXMLEdge edge : toXMLConsList_Edge edges
toXMLConsList_Edge Nil_Edge             = []



parseXML_EnrichedDoc = parseXMLCns_RootEnr
parseXMLCns_RootEnr = RootEnr NoIDD <$ startTag "RootEnr" <*> parseXML_Root <*> parseXML_Document <* endTag "RootEnr"
parseXML_String_ = parseXMLCns_String_
parseXMLCns_String_ = String_ NoIDD <$ startTag "String_" <*> parseXML_String <* endTag "String_"
parseXML_Bool_ = parseXMLCns_Bool_
parseXMLCns_Bool_ = Bool_ NoIDD <$ startTag "Bool_" <*> parseXML_Bool <* endTag "Bool_"
parseXML_Int_ = parseXMLCns_Int_
parseXMLCns_Int_ = Int_ NoIDD <$ startTag "Int_" <*> parseXML_Int <* endTag "Int_"
parseXML_Dummy = parseXMLCns_Dummy
parseXMLCns_Dummy = Dummy NoIDD <$ startTag "Dummy" <*> parseXML_List_Dummy <*> parseXML_String_ <*> parseXML_Bool_ <*> parseXML_Int_ <* endTag "Dummy"
parseXML_Root = parseXMLCns_Root
parseXMLCns_Root = Root NoIDD <$ startTag "Root" <*> parseXML_Tree <*> parseXML_Graph <*> parseXML_List_Section <* endTag "Root"
parseXML_Tree = parseXMLCns_Bin <?|> parseXMLCns_Leaf
parseXMLCns_Bin = Bin NoIDD <$ startTag "Bin" <*> parseXML_Tree <*> parseXML_Tree <* endTag "Bin"
parseXMLCns_Leaf = Leaf NoIDD <$ emptyTag "Leaf"
parseXML_Section = parseXMLCns_Section
parseXMLCns_Section = Section NoIDD <$ startTag "Section" <*> parseXML_String_ <*> parseXML_List_Paragraph <*> parseXML_List_Subsection <*> parseXML_Subgraph <* endTag "Section"
parseXML_Subsection = parseXMLCns_Subsection
parseXMLCns_Subsection = Subsection NoIDD <$ startTag "Subsection" <*> parseXML_String_ <*> parseXML_List_Paragraph <*> parseXML_List_Subsubsection <* endTag "Subsection"
parseXML_Subsubsection = parseXMLCns_Subsubsection
parseXMLCns_Subsubsection = Subsubsection NoIDD <$ startTag "Subsubsection" <*> parseXML_String_ <*> parseXML_List_Paragraph <* endTag "Subsubsection"
parseXML_Paragraph = parseXMLCns_Paragraph
parseXMLCns_Paragraph = Paragraph NoIDD <$ startTag "Paragraph" <*> parseXML_List_Word <* endTag "Paragraph"
parseXML_Word = parseXMLCns_Word
parseXMLCns_Word = Word NoIDD <$ startTag "Word" <*> parseXML_String_ <* endTag "Word"
parseXML_Graph = parseXMLCns_Graph
parseXMLCns_Graph = Graph NoIDD <$ startTag "Graph" <*> parseXML_Dirty <*> parseXML_List_Vertex <*> parseXML_List_Edge <* endTag "Graph"
parseXML_Vertex = parseXMLCns_Vertex
parseXMLCns_Vertex = Vertex NoIDD <$ startTag "Vertex" <*> parseXML_String_ <*> parseXML_Shape <*> parseXML_Int_ <*> parseXML_Int_ <*> parseXML_Int_ <* endTag "Vertex"
parseXML_Shape = parseXMLCns_Circle <?|> parseXMLCns_Square
parseXMLCns_Circle = Circle NoIDD <$ emptyTag "Circle"
parseXMLCns_Square = Square NoIDD <$ emptyTag "Square"
parseXML_Edge = parseXMLCns_Edge
parseXMLCns_Edge = Edge NoIDD <$ startTag "Edge" <*> parseXML_Int_ <*> parseXML_Int_ <* endTag "Edge"
parseXML_Subgraph = parseXMLCns_Subgraph
parseXMLCns_Subgraph = Subgraph NoIDD <$ startTag "Subgraph" <*> parseXML_Dirty <*> parseXML_List_Vertex <*> parseXML_List_Edge <* endTag "Subgraph"
parseXML_Dirty = parseXMLCns_Dirty <?|> parseXMLCns_Clean
parseXMLCns_Dirty = Dirty NoIDD <$ emptyTag "Dirty"
parseXMLCns_Clean = Clean NoIDD <$ emptyTag "Clean"
parseXML_List_Dummy = mkList List_Dummy Cons_Dummy Nil_Dummy <$> many parseXML_Dummy
parseXML_List_Section = mkList List_Section Cons_Section Nil_Section <$> many parseXML_Section
parseXML_List_Paragraph = mkList List_Paragraph Cons_Paragraph Nil_Paragraph <$> many parseXML_Paragraph
parseXML_List_Subsection = mkList List_Subsection Cons_Subsection Nil_Subsection <$> many parseXML_Subsection
parseXML_List_Subsubsection = mkList List_Subsubsection Cons_Subsubsection Nil_Subsubsection <$> many parseXML_Subsubsection
parseXML_List_Word = mkList List_Word Cons_Word Nil_Word <$> many parseXML_Word
parseXML_List_Vertex = mkList List_Vertex Cons_Vertex Nil_Vertex <$> many parseXML_Vertex
parseXML_List_Edge = mkList List_Edge Cons_Edge Nil_Edge <$> many parseXML_Edge
