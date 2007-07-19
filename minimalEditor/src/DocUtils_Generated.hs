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
rankNode (GraphNode _ _)  = 18
rankNode (HoleGraphNode _ _)  = 19
rankNode (VertexNode _ _)  = 20
rankNode (HoleVertexNode _ _)  = 21
rankNode (EdgeNode _ _)  = 22
rankNode (HoleEdgeNode _ _)  = 23
rankNode (SubGraphNode _ _)  = 24
rankNode (HoleSubGraphNode _ _)  = 25
rankNode (List_DummyNode _ _)  = 26
rankNode (HoleList_DummyNode _ _)  = 27
rankNode (List_VertexNode _ _)  = 28
rankNode (HoleList_VertexNode _ _)  = 29
rankNode (List_EdgeNode _ _)  = 30
rankNode (HoleList_EdgeNode _ _)  = 31



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
  pathNode (GraphNode _ pth)  = PathD pth
  pathNode (HoleGraphNode _ pth)  = PathD pth
  pathNode (VertexNode _ pth)  = PathD pth
  pathNode (HoleVertexNode _ pth)  = PathD pth
  pathNode (EdgeNode _ pth)  = PathD pth
  pathNode (HoleEdgeNode _ pth)  = PathD pth
  pathNode (SubGraphNode _ pth)  = PathD pth
  pathNode (HoleSubGraphNode _ pth)  = PathD pth
  pathNode (List_DummyNode _ pth)  = PathD pth
  pathNode (HoleList_DummyNode _ pth)  = PathD pth
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

graphIDD :: Node -> Maybe IDD
graphIDD (GraphNode (Graph iDD _ _) _) = Just iDD
graphIDD _                                   = Nothing

vertexIDD :: Node -> Maybe IDD
vertexIDD (VertexNode (Vertex iDD _ _ _ _) _) = Just iDD
vertexIDD _                                   = Nothing

edgeIDD :: Node -> Maybe IDD
edgeIDD (EdgeNode (Edge iDD _ _) _) = Just iDD
edgeIDD _                                   = Nothing

subGraphIDD :: Node -> Maybe IDD
subGraphIDD (SubGraphNode (SubGraph iDD _ _) _) = Just iDD
subGraphIDD _                                   = Nothing




shallowShowEnrichedDoc1 (RootEnr  _ _ _) = "RootEnr"
shallowShowString_1 (String_  _ _) = "String_"
shallowShowBool_1 (Bool_  _ _) = "Bool_"
shallowShowInt_1 (Int_  _ _) = "Int_"
shallowShowDummy1 (Dummy  _ _ _ _ _) = "Dummy"
shallowShowRoot1 (Root  _ _ _ _) = "Root"
shallowShowTree1 (Bin  _ _ _) = "Bin"
shallowShowTree1 (Leaf  _) = "Leaf"
shallowShowGraph1 (Graph  _ _ _) = "Graph"
shallowShowVertex1 (Vertex  _ _ _ _ _) = "Vertex"
shallowShowEdge1 (Edge  _ _ _) = "Edge"
shallowShowSubGraph1 (SubGraph  _ _ _) = "SubGraph"
shallowShowList_Dummy1 (List_Dummy  _ _) = "List_Dummy"
shallowShowConsList_Dummy1 (Cons_Dummy  _ _) = "Cons_Dummy"
shallowShowConsList_Dummy1 (Nil_Dummy ) = "Nil_Dummy"
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
toXMLRoot (Root _ tree graph subGraph) = Elt "Root" [] $ [toXMLTree tree] ++ [toXMLGraph graph] ++ [toXMLSubGraph subGraph] ++ []
toXMLTree (Bin _ left right) = Elt "Bin" [] $ [toXMLTree left] ++ [toXMLTree right] ++ []
toXMLTree (Leaf _) = Elt "Leaf" [] $ []
toXMLGraph (Graph _ vertices edges) = Elt "Graph" [] $ toXMLList_Vertex vertices ++ toXMLList_Edge edges ++ []
toXMLVertex (Vertex _ name id x y) = Elt "Vertex" [] $ [toXMLString_ name] ++ [toXMLInt_ id] ++ [toXMLInt_ x] ++ [toXMLInt_ y] ++ []
toXMLEdge (Edge _ from to) = Elt "Edge" [] $ [toXMLInt_ from] ++ [toXMLInt_ to] ++ []
toXMLSubGraph (SubGraph _ vertices edges) = Elt "SubGraph" [] $ toXMLList_Vertex vertices ++ toXMLList_Edge edges ++ []
toXMLList_Dummy (List_Dummy _ dummys) = toXMLConsList_Dummy dummys
toXMLConsList_Dummy (Cons_Dummy dummy dummys) = toXMLDummy dummy : toXMLConsList_Dummy dummys
toXMLConsList_Dummy Nil_Dummy             = []
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
parseXMLCns_Root = Root NoIDD <$ startTag "Root" <*> parseXML_Tree <*> parseXML_Graph <*> parseXML_SubGraph <* endTag "Root"
parseXML_Tree = parseXMLCns_Bin <?|> parseXMLCns_Leaf
parseXMLCns_Bin = Bin NoIDD <$ startTag "Bin" <*> parseXML_Tree <*> parseXML_Tree <* endTag "Bin"
parseXMLCns_Leaf = Leaf NoIDD <$ emptyTag "Leaf"
parseXML_Graph = parseXMLCns_Graph
parseXMLCns_Graph = Graph NoIDD <$ startTag "Graph" <*> parseXML_List_Vertex <*> parseXML_List_Edge <* endTag "Graph"
parseXML_Vertex = parseXMLCns_Vertex
parseXMLCns_Vertex = Vertex NoIDD <$ startTag "Vertex" <*> parseXML_String_ <*> parseXML_Int_ <*> parseXML_Int_ <*> parseXML_Int_ <* endTag "Vertex"
parseXML_Edge = parseXMLCns_Edge
parseXMLCns_Edge = Edge NoIDD <$ startTag "Edge" <*> parseXML_Int_ <*> parseXML_Int_ <* endTag "Edge"
parseXML_SubGraph = parseXMLCns_SubGraph
parseXMLCns_SubGraph = SubGraph NoIDD <$ startTag "SubGraph" <*> parseXML_List_Vertex <*> parseXML_List_Edge <* endTag "SubGraph"
parseXML_List_Dummy = mkList List_Dummy Cons_Dummy Nil_Dummy <$> many (try parseXML_Dummy)
parseXML_List_Vertex = mkList List_Vertex Cons_Vertex Nil_Vertex <$> many (try parseXML_Vertex)
parseXML_List_Edge = mkList List_Edge Cons_Edge Nil_Edge <$> many (try parseXML_Edge)
