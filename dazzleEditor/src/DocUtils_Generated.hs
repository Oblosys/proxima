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
rankNode (Node_RootDoc _ _) = 1
rankNode (Node_HoleDocument _ _) = 2
rankNode (Node_ParseErrDocument _ _) = 3
rankNode (Node_RootEnr _ _) = 4
rankNode (Node_HoleEnrichedDoc _ _) = 5
rankNode (Node_ParseErrEnrichedDoc _ _) = 6
rankNode (Node_Dummy _ _) = 7
rankNode (Node_HoleDummy _ _) = 8
rankNode (Node_ParseErrDummy _ _) = 9
rankNode (Node_Root _ _) = 10
rankNode (Node_HoleRoot _ _) = 11
rankNode (Node_ParseErrRoot _ _) = 12
rankNode (Node_Section _ _) = 13
rankNode (Node_HoleSection _ _) = 14
rankNode (Node_ParseErrSection _ _) = 15
rankNode (Node_Subsection _ _) = 16
rankNode (Node_HoleSubsection _ _) = 17
rankNode (Node_ParseErrSubsection _ _) = 18
rankNode (Node_Subsubsection _ _) = 19
rankNode (Node_HoleSubsubsection _ _) = 20
rankNode (Node_ParseErrSubsubsection _ _) = 21
rankNode (Node_Paragraph _ _) = 22
rankNode (Node_SubgraphPara _ _) = 23
rankNode (Node_HoleParagraph _ _) = 24
rankNode (Node_ParseErrParagraph _ _) = 25
rankNode (Node_Word _ _) = 26
rankNode (Node_NodeRef _ _) = 27
rankNode (Node_Label _ _) = 28
rankNode (Node_LabelRef _ _) = 29
rankNode (Node_HoleWord _ _) = 30
rankNode (Node_ParseErrWord _ _) = 31
rankNode (Node_Graph _ _) = 32
rankNode (Node_HoleGraph _ _) = 33
rankNode (Node_ParseErrGraph _ _) = 34
rankNode (Node_Vertex _ _) = 35
rankNode (Node_HoleVertex _ _) = 36
rankNode (Node_ParseErrVertex _ _) = 37
rankNode (Node_Circle _ _) = 38
rankNode (Node_Square _ _) = 39
rankNode (Node_HoleShape _ _) = 40
rankNode (Node_ParseErrShape _ _) = 41
rankNode (Node_Edge _ _) = 42
rankNode (Node_HoleEdge _ _) = 43
rankNode (Node_ParseErrEdge _ _) = 44
rankNode (Node_Subgraph _ _) = 45
rankNode (Node_HoleSubgraph _ _) = 46
rankNode (Node_ParseErrSubgraph _ _) = 47
rankNode (Node_Dirty _ _) = 48
rankNode (Node_Clean _ _) = 49
rankNode (Node_HoleDirty _ _) = 50
rankNode (Node_ParseErrDirty _ _) = 51
rankNode (Node_List_Section _ _) = 52
rankNode (Node_HoleList_Section _ _) = 53
rankNode (Node_ParseErrList_Section _ _) = 54
rankNode (Node_List_Paragraph _ _) = 55
rankNode (Node_HoleList_Paragraph _ _) = 56
rankNode (Node_ParseErrList_Paragraph _ _) = 57
rankNode (Node_List_Subsection _ _) = 58
rankNode (Node_HoleList_Subsection _ _) = 59
rankNode (Node_ParseErrList_Subsection _ _) = 60
rankNode (Node_List_Subsubsection _ _) = 61
rankNode (Node_HoleList_Subsubsection _ _) = 62
rankNode (Node_ParseErrList_Subsubsection _ _) = 63
rankNode (Node_List_Word _ _) = 64
rankNode (Node_HoleList_Word _ _) = 65
rankNode (Node_ParseErrList_Word _ _) = 66
rankNode (Node_List_Vertex _ _) = 67
rankNode (Node_HoleList_Vertex _ _) = 68
rankNode (Node_ParseErrList_Vertex _ _) = 69
rankNode (Node_List_Edge _ _) = 70
rankNode (Node_HoleList_Edge _ _) = 71
rankNode (Node_ParseErrList_Edge _ _) = 72



--------------------------------------------------------------------------
-- HasPath instance for Node                                            --
--------------------------------------------------------------------------

instance HasPath Node where
  pathNode NoNode            = NoPathD
  pathNode (Node_RootDoc _ pth) = PathD pth
  pathNode (Node_HoleDocument _ pth) = PathD pth
  pathNode (Node_ParseErrDocument _ pth) = PathD pth
  pathNode (Node_RootEnr _ pth) = PathD pth
  pathNode (Node_HoleEnrichedDoc _ pth) = PathD pth
  pathNode (Node_ParseErrEnrichedDoc _ pth) = PathD pth
  pathNode (Node_Dummy _ pth) = PathD pth
  pathNode (Node_HoleDummy _ pth) = PathD pth
  pathNode (Node_ParseErrDummy _ pth) = PathD pth
  pathNode (Node_Root _ pth) = PathD pth
  pathNode (Node_HoleRoot _ pth) = PathD pth
  pathNode (Node_ParseErrRoot _ pth) = PathD pth
  pathNode (Node_Section _ pth) = PathD pth
  pathNode (Node_HoleSection _ pth) = PathD pth
  pathNode (Node_ParseErrSection _ pth) = PathD pth
  pathNode (Node_Subsection _ pth) = PathD pth
  pathNode (Node_HoleSubsection _ pth) = PathD pth
  pathNode (Node_ParseErrSubsection _ pth) = PathD pth
  pathNode (Node_Subsubsection _ pth) = PathD pth
  pathNode (Node_HoleSubsubsection _ pth) = PathD pth
  pathNode (Node_ParseErrSubsubsection _ pth) = PathD pth
  pathNode (Node_Paragraph _ pth) = PathD pth
  pathNode (Node_SubgraphPara _ pth) = PathD pth
  pathNode (Node_HoleParagraph _ pth) = PathD pth
  pathNode (Node_ParseErrParagraph _ pth) = PathD pth
  pathNode (Node_Word _ pth) = PathD pth
  pathNode (Node_NodeRef _ pth) = PathD pth
  pathNode (Node_Label _ pth) = PathD pth
  pathNode (Node_LabelRef _ pth) = PathD pth
  pathNode (Node_HoleWord _ pth) = PathD pth
  pathNode (Node_ParseErrWord _ pth) = PathD pth
  pathNode (Node_Graph _ pth) = PathD pth
  pathNode (Node_HoleGraph _ pth) = PathD pth
  pathNode (Node_ParseErrGraph _ pth) = PathD pth
  pathNode (Node_Vertex _ pth) = PathD pth
  pathNode (Node_HoleVertex _ pth) = PathD pth
  pathNode (Node_ParseErrVertex _ pth) = PathD pth
  pathNode (Node_Circle _ pth) = PathD pth
  pathNode (Node_Square _ pth) = PathD pth
  pathNode (Node_HoleShape _ pth) = PathD pth
  pathNode (Node_ParseErrShape _ pth) = PathD pth
  pathNode (Node_Edge _ pth) = PathD pth
  pathNode (Node_HoleEdge _ pth) = PathD pth
  pathNode (Node_ParseErrEdge _ pth) = PathD pth
  pathNode (Node_Subgraph _ pth) = PathD pth
  pathNode (Node_HoleSubgraph _ pth) = PathD pth
  pathNode (Node_ParseErrSubgraph _ pth) = PathD pth
  pathNode (Node_Dirty _ pth) = PathD pth
  pathNode (Node_Clean _ pth) = PathD pth
  pathNode (Node_HoleDirty _ pth) = PathD pth
  pathNode (Node_ParseErrDirty _ pth) = PathD pth
  pathNode (Node_List_Section _ pth) = PathD pth
  pathNode (Node_HoleList_Section _ pth) = PathD pth
  pathNode (Node_ParseErrList_Section _ pth) = PathD pth
  pathNode (Node_List_Paragraph _ pth) = PathD pth
  pathNode (Node_HoleList_Paragraph _ pth) = PathD pth
  pathNode (Node_ParseErrList_Paragraph _ pth) = PathD pth
  pathNode (Node_List_Subsection _ pth) = PathD pth
  pathNode (Node_HoleList_Subsection _ pth) = PathD pth
  pathNode (Node_ParseErrList_Subsection _ pth) = PathD pth
  pathNode (Node_List_Subsubsection _ pth) = PathD pth
  pathNode (Node_HoleList_Subsubsection _ pth) = PathD pth
  pathNode (Node_ParseErrList_Subsubsection _ pth) = PathD pth
  pathNode (Node_List_Word _ pth) = PathD pth
  pathNode (Node_HoleList_Word _ pth) = PathD pth
  pathNode (Node_ParseErrList_Word _ pth) = PathD pth
  pathNode (Node_List_Vertex _ pth) = PathD pth
  pathNode (Node_HoleList_Vertex _ pth) = PathD pth
  pathNode (Node_ParseErrList_Vertex _ pth) = PathD pth
  pathNode (Node_List_Edge _ pth) = PathD pth
  pathNode (Node_HoleList_Edge _ pth) = PathD pth
  pathNode (Node_ParseErrList_Edge _ pth) = PathD pth



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

