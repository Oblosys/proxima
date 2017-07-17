module DocUtils_Generated where

import Evaluation.DocTypes
import DocTypes_Generated
import Evaluation.DocUtils
import Evaluation.DocumentEdit
import Presentation.PresTypes
import Presentation.XprezLib
import Common.DebugLevels
import UU.Parsing
import UU.Parsing.CharParser

import Common.CommonTypes hiding (Clean, Dirty)

initialDocument :: IO Document
initialDocument = return (RootDoc (Root (Graph Clean (toList_Vertex []) (toList_Edge [])) 
                                        "" "" 
                                        (toList_Probtable [])
                                        "Initial document" 
                                        (toList_Section [])))

parseXML_List_Int = pList_ng parseXML_Int
toXMLList_Int = map toXMLInt

presentPrimXMLList_Int x = error "DocUtils_Generated.presentPrimXMLInt not implemented"

presentPrimTreeList_Int x = error "DocUtils_Generated.presentPrimTreeInt not implemented"

----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- rankNode                                                             --
--------------------------------------------------------------------------

rankNode :: Node -> Int
rankNode NoNode = 0
rankNode (Node_RootEnr _ _) = 1
rankNode (Node_HoleEnrichedDoc _ _) = 2
rankNode (Node_ParseErrEnrichedDoc _ _) = 3
rankNode (Node_RootDoc _ _) = 4
rankNode (Node_HoleDocument _ _) = 5
rankNode (Node_ParseErrDocument _ _) = 6
rankNode (Node_Root _ _) = 7
rankNode (Node_HoleRoot _ _) = 8
rankNode (Node_ParseErrRoot _ _) = 9
rankNode (Node_Section _ _) = 10
rankNode (Node_HoleSection _ _) = 11
rankNode (Node_ParseErrSection _ _) = 12
rankNode (Node_Subsection _ _) = 13
rankNode (Node_HoleSubsection _ _) = 14
rankNode (Node_ParseErrSubsection _ _) = 15
rankNode (Node_Subsubsection _ _) = 16
rankNode (Node_HoleSubsubsection _ _) = 17
rankNode (Node_ParseErrSubsubsection _ _) = 18
rankNode (Node_Paragraph _ _) = 19
rankNode (Node_SubgraphPara _ _) = 20
rankNode (Node_ProbtablePara _ _) = 21
rankNode (Node_HoleParagraph _ _) = 22
rankNode (Node_ParseErrParagraph _ _) = 23
rankNode (Node_Word _ _) = 24
rankNode (Node_NodeRef _ _) = 25
rankNode (Node_Label _ _) = 26
rankNode (Node_LabelRef _ _) = 27
rankNode (Node_HoleWord _ _) = 28
rankNode (Node_ParseErrWord _ _) = 29
rankNode (Node_NodeName _ _) = 30
rankNode (Node_HoleNodeName _ _) = 31
rankNode (Node_ParseErrNodeName _ _) = 32
rankNode (Node_Graph _ _) = 33
rankNode (Node_HoleGraph _ _) = 34
rankNode (Node_ParseErrGraph _ _) = 35
rankNode (Node_Vertex _ _) = 36
rankNode (Node_HoleVertex _ _) = 37
rankNode (Node_ParseErrVertex _ _) = 38
rankNode (Node_Circle _ _) = 39
rankNode (Node_Square _ _) = 40
rankNode (Node_HoleShape _ _) = 41
rankNode (Node_ParseErrShape _ _) = 42
rankNode (Node_Edge _ _) = 43
rankNode (Node_HoleEdge _ _) = 44
rankNode (Node_ParseErrEdge _ _) = 45
rankNode (Node_Subgraph _ _) = 46
rankNode (Node_HoleSubgraph _ _) = 47
rankNode (Node_ParseErrSubgraph _ _) = 48
rankNode (Node_Dirty _ _) = 49
rankNode (Node_Clean _ _) = 50
rankNode (Node_HoleDirty _ _) = 51
rankNode (Node_ParseErrDirty _ _) = 52
rankNode (Node_Probtable _ _) = 53
rankNode (Node_HoleProbtable _ _) = 54
rankNode (Node_ParseErrProbtable _ _) = 55
rankNode (Node_Value _ _) = 56
rankNode (Node_HoleValue _ _) = 57
rankNode (Node_ParseErrValue _ _) = 58
rankNode (Node_Table _ _) = 59
rankNode (Node_HoleTable _ _) = 60
rankNode (Node_ParseErrTable _ _) = 61
rankNode (Node_Axis _ _) = 62
rankNode (Node_HoleAxis _ _) = 63
rankNode (Node_ParseErrAxis _ _) = 64
rankNode (Node_Probability _ _) = 65
rankNode (Node_HoleProbability _ _) = 66
rankNode (Node_ParseErrProbability _ _) = 67
rankNode (Node_List_Probtable _ _) = 68
rankNode (Node_HoleList_Probtable _ _) = 69
rankNode (Node_ParseErrList_Probtable _ _) = 70
rankNode (Node_List_Section _ _) = 71
rankNode (Node_HoleList_Section _ _) = 72
rankNode (Node_ParseErrList_Section _ _) = 73
rankNode (Node_List_Paragraph _ _) = 74
rankNode (Node_HoleList_Paragraph _ _) = 75
rankNode (Node_ParseErrList_Paragraph _ _) = 76
rankNode (Node_List_Subsection _ _) = 77
rankNode (Node_HoleList_Subsection _ _) = 78
rankNode (Node_ParseErrList_Subsection _ _) = 79
rankNode (Node_List_Subsubsection _ _) = 80
rankNode (Node_HoleList_Subsubsection _ _) = 81
rankNode (Node_ParseErrList_Subsubsection _ _) = 82
rankNode (Node_List_Word _ _) = 83
rankNode (Node_HoleList_Word _ _) = 84
rankNode (Node_ParseErrList_Word _ _) = 85
rankNode (Node_List_Vertex _ _) = 86
rankNode (Node_HoleList_Vertex _ _) = 87
rankNode (Node_ParseErrList_Vertex _ _) = 88
rankNode (Node_List_Edge _ _) = 89
rankNode (Node_HoleList_Edge _ _) = 90
rankNode (Node_ParseErrList_Edge _ _) = 91
rankNode (Node_List_Value _ _) = 92
rankNode (Node_HoleList_Value _ _) = 93
rankNode (Node_ParseErrList_Value _ _) = 94
rankNode (Node_List_Axis _ _) = 95
rankNode (Node_HoleList_Axis _ _) = 96
rankNode (Node_ParseErrList_Axis _ _) = 97
rankNode (Node_List_Probability _ _) = 98
rankNode (Node_HoleList_Probability _ _) = 99
rankNode (Node_ParseErrList_Probability _ _) = 100



--------------------------------------------------------------------------
-- DocNode instance for Node                                            --
--------------------------------------------------------------------------

instance DocNode Node where
  noNode = NoNode

  pathNode NoNode            = NoPathD
  pathNode (Node_RootEnr _ pth) = PathD pth
  pathNode (Node_HoleEnrichedDoc _ pth) = PathD pth
  pathNode (Node_ParseErrEnrichedDoc _ pth) = PathD pth
  pathNode (Node_RootDoc _ pth) = PathD pth
  pathNode (Node_HoleDocument _ pth) = PathD pth
  pathNode (Node_ParseErrDocument _ pth) = PathD pth
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
  pathNode (Node_ProbtablePara _ pth) = PathD pth
  pathNode (Node_HoleParagraph _ pth) = PathD pth
  pathNode (Node_ParseErrParagraph _ pth) = PathD pth
  pathNode (Node_Word _ pth) = PathD pth
  pathNode (Node_NodeRef _ pth) = PathD pth
  pathNode (Node_Label _ pth) = PathD pth
  pathNode (Node_LabelRef _ pth) = PathD pth
  pathNode (Node_HoleWord _ pth) = PathD pth
  pathNode (Node_ParseErrWord _ pth) = PathD pth
  pathNode (Node_NodeName _ pth) = PathD pth
  pathNode (Node_HoleNodeName _ pth) = PathD pth
  pathNode (Node_ParseErrNodeName _ pth) = PathD pth
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
  pathNode (Node_Probtable _ pth) = PathD pth
  pathNode (Node_HoleProbtable _ pth) = PathD pth
  pathNode (Node_ParseErrProbtable _ pth) = PathD pth
  pathNode (Node_Value _ pth) = PathD pth
  pathNode (Node_HoleValue _ pth) = PathD pth
  pathNode (Node_ParseErrValue _ pth) = PathD pth
  pathNode (Node_Table _ pth) = PathD pth
  pathNode (Node_HoleTable _ pth) = PathD pth
  pathNode (Node_ParseErrTable _ pth) = PathD pth
  pathNode (Node_Axis _ pth) = PathD pth
  pathNode (Node_HoleAxis _ pth) = PathD pth
  pathNode (Node_ParseErrAxis _ pth) = PathD pth
  pathNode (Node_Probability _ pth) = PathD pth
  pathNode (Node_HoleProbability _ pth) = PathD pth
  pathNode (Node_ParseErrProbability _ pth) = PathD pth
  pathNode (Node_List_Probtable _ pth) = PathD pth
  pathNode (Node_HoleList_Probtable _ pth) = PathD pth
  pathNode (Node_ParseErrList_Probtable _ pth) = PathD pth
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
  pathNode (Node_List_Value _ pth) = PathD pth
  pathNode (Node_HoleList_Value _ pth) = PathD pth
  pathNode (Node_ParseErrList_Value _ pth) = PathD pth
  pathNode (Node_List_Axis _ pth) = PathD pth
  pathNode (Node_HoleList_Axis _ pth) = PathD pth
  pathNode (Node_ParseErrList_Axis _ pth) = PathD pth
  pathNode (Node_List_Probability _ pth) = PathD pth
  pathNode (Node_HoleList_Probability _ pth) = PathD pth
  pathNode (Node_ParseErrList_Probability _ pth) = PathD pth

  typeOfNode (Node_RootEnr _ _) = BasicType "EnrichedDoc"
  typeOfNode (Node_HoleEnrichedDoc _ _) = BasicType "EnrichedDoc"
  typeOfNode (Node_ParseErrEnrichedDoc _ _) = BasicType "EnrichedDoc"
  typeOfNode (Node_RootDoc _ _) = BasicType "Document"
  typeOfNode (Node_HoleDocument _ _) = BasicType "Document"
  typeOfNode (Node_ParseErrDocument _ _) = BasicType "Document"
  typeOfNode (Node_Root _ _) = BasicType "Root"
  typeOfNode (Node_HoleRoot _ _) = BasicType "Root"
  typeOfNode (Node_ParseErrRoot _ _) = BasicType "Root"
  typeOfNode (Node_Section _ _) = BasicType "Section"
  typeOfNode (Node_HoleSection _ _) = BasicType "Section"
  typeOfNode (Node_ParseErrSection _ _) = BasicType "Section"
  typeOfNode (Node_Subsection _ _) = BasicType "Subsection"
  typeOfNode (Node_HoleSubsection _ _) = BasicType "Subsection"
  typeOfNode (Node_ParseErrSubsection _ _) = BasicType "Subsection"
  typeOfNode (Node_Subsubsection _ _) = BasicType "Subsubsection"
  typeOfNode (Node_HoleSubsubsection _ _) = BasicType "Subsubsection"
  typeOfNode (Node_ParseErrSubsubsection _ _) = BasicType "Subsubsection"
  typeOfNode (Node_Paragraph _ _) = BasicType "Paragraph"
  typeOfNode (Node_SubgraphPara _ _) = BasicType "Paragraph"
  typeOfNode (Node_ProbtablePara _ _) = BasicType "Paragraph"
  typeOfNode (Node_HoleParagraph _ _) = BasicType "Paragraph"
  typeOfNode (Node_ParseErrParagraph _ _) = BasicType "Paragraph"
  typeOfNode (Node_Word _ _) = BasicType "Word"
  typeOfNode (Node_NodeRef _ _) = BasicType "Word"
  typeOfNode (Node_Label _ _) = BasicType "Word"
  typeOfNode (Node_LabelRef _ _) = BasicType "Word"
  typeOfNode (Node_HoleWord _ _) = BasicType "Word"
  typeOfNode (Node_ParseErrWord _ _) = BasicType "Word"
  typeOfNode (Node_NodeName _ _) = BasicType "NodeName"
  typeOfNode (Node_HoleNodeName _ _) = BasicType "NodeName"
  typeOfNode (Node_ParseErrNodeName _ _) = BasicType "NodeName"
  typeOfNode (Node_Graph _ _) = BasicType "Graph"
  typeOfNode (Node_HoleGraph _ _) = BasicType "Graph"
  typeOfNode (Node_ParseErrGraph _ _) = BasicType "Graph"
  typeOfNode (Node_Vertex _ _) = BasicType "Vertex"
  typeOfNode (Node_HoleVertex _ _) = BasicType "Vertex"
  typeOfNode (Node_ParseErrVertex _ _) = BasicType "Vertex"
  typeOfNode (Node_Circle _ _) = BasicType "Shape"
  typeOfNode (Node_Square _ _) = BasicType "Shape"
  typeOfNode (Node_HoleShape _ _) = BasicType "Shape"
  typeOfNode (Node_ParseErrShape _ _) = BasicType "Shape"
  typeOfNode (Node_Edge _ _) = BasicType "Edge"
  typeOfNode (Node_HoleEdge _ _) = BasicType "Edge"
  typeOfNode (Node_ParseErrEdge _ _) = BasicType "Edge"
  typeOfNode (Node_Subgraph _ _) = BasicType "Subgraph"
  typeOfNode (Node_HoleSubgraph _ _) = BasicType "Subgraph"
  typeOfNode (Node_ParseErrSubgraph _ _) = BasicType "Subgraph"
  typeOfNode (Node_Dirty _ _) = BasicType "Dirty"
  typeOfNode (Node_Clean _ _) = BasicType "Dirty"
  typeOfNode (Node_HoleDirty _ _) = BasicType "Dirty"
  typeOfNode (Node_ParseErrDirty _ _) = BasicType "Dirty"
  typeOfNode (Node_Probtable _ _) = BasicType "Probtable"
  typeOfNode (Node_HoleProbtable _ _) = BasicType "Probtable"
  typeOfNode (Node_ParseErrProbtable _ _) = BasicType "Probtable"
  typeOfNode (Node_Value _ _) = BasicType "Value"
  typeOfNode (Node_HoleValue _ _) = BasicType "Value"
  typeOfNode (Node_ParseErrValue _ _) = BasicType "Value"
  typeOfNode (Node_Table _ _) = BasicType "Table"
  typeOfNode (Node_HoleTable _ _) = BasicType "Table"
  typeOfNode (Node_ParseErrTable _ _) = BasicType "Table"
  typeOfNode (Node_Axis _ _) = BasicType "Axis"
  typeOfNode (Node_HoleAxis _ _) = BasicType "Axis"
  typeOfNode (Node_ParseErrAxis _ _) = BasicType "Axis"
  typeOfNode (Node_Probability _ _) = BasicType "Probability"
  typeOfNode (Node_HoleProbability _ _) = BasicType "Probability"
  typeOfNode (Node_ParseErrProbability _ _) = BasicType "Probability"
  typeOfNode (Node_List_Probtable _ _) = ListType "Probtable"
  typeOfNode (Node_HoleList_Probtable _ _) = ListType "Probtable"
  typeOfNode (Node_ParseErrList_Probtable _ _) = ListType "Probtable"
  typeOfNode (Node_List_Section _ _) = ListType "Section"
  typeOfNode (Node_HoleList_Section _ _) = ListType "Section"
  typeOfNode (Node_ParseErrList_Section _ _) = ListType "Section"
  typeOfNode (Node_List_Paragraph _ _) = ListType "Paragraph"
  typeOfNode (Node_HoleList_Paragraph _ _) = ListType "Paragraph"
  typeOfNode (Node_ParseErrList_Paragraph _ _) = ListType "Paragraph"
  typeOfNode (Node_List_Subsection _ _) = ListType "Subsection"
  typeOfNode (Node_HoleList_Subsection _ _) = ListType "Subsection"
  typeOfNode (Node_ParseErrList_Subsection _ _) = ListType "Subsection"
  typeOfNode (Node_List_Subsubsection _ _) = ListType "Subsubsection"
  typeOfNode (Node_HoleList_Subsubsection _ _) = ListType "Subsubsection"
  typeOfNode (Node_ParseErrList_Subsubsection _ _) = ListType "Subsubsection"
  typeOfNode (Node_List_Word _ _) = ListType "Word"
  typeOfNode (Node_HoleList_Word _ _) = ListType "Word"
  typeOfNode (Node_ParseErrList_Word _ _) = ListType "Word"
  typeOfNode (Node_List_Vertex _ _) = ListType "Vertex"
  typeOfNode (Node_HoleList_Vertex _ _) = ListType "Vertex"
  typeOfNode (Node_ParseErrList_Vertex _ _) = ListType "Vertex"
  typeOfNode (Node_List_Edge _ _) = ListType "Edge"
  typeOfNode (Node_HoleList_Edge _ _) = ListType "Edge"
  typeOfNode (Node_ParseErrList_Edge _ _) = ListType "Edge"
  typeOfNode (Node_List_Value _ _) = ListType "Value"
  typeOfNode (Node_HoleList_Value _ _) = ListType "Value"
  typeOfNode (Node_ParseErrList_Value _ _) = ListType "Value"
  typeOfNode (Node_List_Axis _ _) = ListType "Axis"
  typeOfNode (Node_HoleList_Axis _ _) = ListType "Axis"
  typeOfNode (Node_ParseErrList_Axis _ _) = ListType "Axis"
  typeOfNode (Node_List_Probability _ _) = ListType "Probability"
  typeOfNode (Node_HoleList_Probability _ _) = ListType "Probability"
  typeOfNode (Node_ParseErrList_Probability _ _) = ListType "Probability"



--------------------------------------------------------------------------
-- toXML functions                                                      --
--------------------------------------------------------------------------

toXMLEnrichedDoc (RootEnr root) = Elt "RootEnr" [] $ [toXMLRoot root]
toXMLEnrichedDoc (HoleEnrichedDoc) = EmptyElt "HoleEnrichedDoc" [] 
toXMLEnrichedDoc (ParseErrEnrichedDoc error) = EmptyElt "ParseErrEnrichedDoc" []
toXMLDocument (RootDoc root) = Elt "RootDoc" [] $ [toXMLRoot root]
toXMLDocument (HoleDocument) = EmptyElt "HoleDocument" [] 
toXMLDocument (ParseErrDocument error) = EmptyElt "ParseErrDocument" []
toXMLRoot (Root graph caption label probtables title sections) = Elt "Root" [] $ [toXMLGraph graph] ++ [toXMLString caption] ++ [toXMLString label] ++ toXMLList_Probtable probtables ++ [toXMLString title] ++ toXMLList_Section sections
toXMLRoot (HoleRoot) = EmptyElt "HoleRoot" [] 
toXMLRoot (ParseErrRoot error) = EmptyElt "ParseErrRoot" []
toXMLSection (Section title paragraphs subsections) = Elt "Section" [] $ [toXMLString title] ++ toXMLList_Paragraph paragraphs ++ toXMLList_Subsection subsections
toXMLSection (HoleSection) = EmptyElt "HoleSection" [] 
toXMLSection (ParseErrSection error) = EmptyElt "ParseErrSection" []
toXMLSubsection (Subsection title paragraphs subsubsections) = Elt "Subsection" [] $ [toXMLString title] ++ toXMLList_Paragraph paragraphs ++ toXMLList_Subsubsection subsubsections
toXMLSubsection (HoleSubsection) = EmptyElt "HoleSubsection" [] 
toXMLSubsection (ParseErrSubsection error) = EmptyElt "ParseErrSubsection" []
toXMLSubsubsection (Subsubsection title paragraphs) = Elt "Subsubsection" [] $ [toXMLString title] ++ toXMLList_Paragraph paragraphs
toXMLSubsubsection (HoleSubsubsection) = EmptyElt "HoleSubsubsection" [] 
toXMLSubsubsection (ParseErrSubsubsection error) = EmptyElt "ParseErrSubsubsection" []
toXMLParagraph (Paragraph words) = Elt "Paragraph" [] $ toXMLList_Word words
toXMLParagraph (SubgraphPara subgraph caption label) = Elt "SubgraphPara" [] $ [toXMLSubgraph subgraph] ++ [toXMLString caption] ++ [toXMLString label]
toXMLParagraph (ProbtablePara probtable) = Elt "ProbtablePara" [] $ [toXMLProbtable probtable]
toXMLParagraph (HoleParagraph) = EmptyElt "HoleParagraph" [] 
toXMLParagraph (ParseErrParagraph error) = EmptyElt "ParseErrParagraph" []
toXMLWord (Word _ word) = Elt "Word" [] $ [toXMLString word]
toXMLWord (NodeRef nodeName) = Elt "NodeRef" [] $ [toXMLNodeName nodeName]
toXMLWord (Label label) = Elt "Label" [] $ [toXMLString label]
toXMLWord (LabelRef label) = Elt "LabelRef" [] $ [toXMLString label]
toXMLWord (HoleWord) = EmptyElt "HoleWord" [] 
toXMLWord (ParseErrWord error) = EmptyElt "ParseErrWord" []
toXMLNodeName (NodeName name) = Elt "NodeName" [] $ [toXMLString name]
toXMLNodeName (HoleNodeName) = EmptyElt "HoleNodeName" [] 
toXMLNodeName (ParseErrNodeName error) = EmptyElt "ParseErrNodeName" []
toXMLGraph (Graph dirty vertices edges) = Elt "Graph" [] $ [toXMLDirty dirty] ++ toXMLList_Vertex vertices ++ toXMLList_Edge edges
toXMLGraph (HoleGraph) = EmptyElt "HoleGraph" [] 
toXMLGraph (ParseErrGraph error) = EmptyElt "ParseErrGraph" []
toXMLVertex (Vertex name shape id x y) = Elt "Vertex" [] $ [toXMLString name] ++ [toXMLShape shape] ++ [toXMLInt id] ++ [toXMLInt x] ++ [toXMLInt y]
toXMLVertex (HoleVertex) = EmptyElt "HoleVertex" [] 
toXMLVertex (ParseErrVertex error) = EmptyElt "ParseErrVertex" []
toXMLShape (Circle) = EmptyElt "Circle" [] 
toXMLShape (Square) = EmptyElt "Square" [] 
toXMLShape (HoleShape) = EmptyElt "HoleShape" [] 
toXMLShape (ParseErrShape error) = EmptyElt "ParseErrShape" []
toXMLEdge (Edge from to) = Elt "Edge" [] $ [toXMLInt from] ++ [toXMLInt to]
toXMLEdge (HoleEdge) = EmptyElt "HoleEdge" [] 
toXMLEdge (ParseErrEdge error) = EmptyElt "ParseErrEdge" []
toXMLSubgraph (Subgraph dirty vertices edges) = Elt "Subgraph" [] $ [toXMLDirty dirty] ++ toXMLList_Vertex vertices ++ toXMLList_Edge edges
toXMLSubgraph (HoleSubgraph) = EmptyElt "HoleSubgraph" [] 
toXMLSubgraph (ParseErrSubgraph error) = EmptyElt "ParseErrSubgraph" []
toXMLDirty (Dirty) = EmptyElt "Dirty" [] 
toXMLDirty (Clean) = EmptyElt "Clean" [] 
toXMLDirty (HoleDirty) = EmptyElt "HoleDirty" [] 
toXMLDirty (ParseErrDirty error) = EmptyElt "ParseErrDirty" []
toXMLProbtable (Probtable id values table) = Elt "Probtable" [] $ [toXMLInt id] ++ toXMLList_Value values ++ [toXMLTable table]
toXMLProbtable (HoleProbtable) = EmptyElt "HoleProbtable" [] 
toXMLProbtable (ParseErrProbtable error) = EmptyElt "ParseErrProbtable" []
toXMLValue (Value val) = Elt "Value" [] $ [toXMLString val]
toXMLValue (HoleValue) = EmptyElt "HoleValue" [] 
toXMLValue (ParseErrValue error) = EmptyElt "ParseErrValue" []
toXMLTable (Table parents axes probs) = Elt "Table" [] $ toXMLList_Int parents ++ toXMLList_Axis axes ++ toXMLList_Probability probs
toXMLTable (HoleTable) = EmptyElt "HoleTable" [] 
toXMLTable (ParseErrTable error) = EmptyElt "ParseErrTable" []
toXMLAxis (Axis values) = Elt "Axis" [] $ toXMLList_Value values
toXMLAxis (HoleAxis) = EmptyElt "HoleAxis" [] 
toXMLAxis (ParseErrAxis error) = EmptyElt "ParseErrAxis" []
toXMLProbability (Probability prob) = Elt "Probability" [] $ [toXMLString prob]
toXMLProbability (HoleProbability) = EmptyElt "HoleProbability" [] 
toXMLProbability (ParseErrProbability error) = EmptyElt "ParseErrProbability" []
toXMLList_Probtable (List_Probtable xs) = toXMLConsList_Probtable xs
toXMLList_Probtable HoleList_Probtable = []
toXMLList_Probtable (ParseErrList_Probtable _) = []
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
toXMLList_Value (List_Value xs) = toXMLConsList_Value xs
toXMLList_Value HoleList_Value = []
toXMLList_Value (ParseErrList_Value _) = []
toXMLList_Axis (List_Axis xs) = toXMLConsList_Axis xs
toXMLList_Axis HoleList_Axis = []
toXMLList_Axis (ParseErrList_Axis _) = []
toXMLList_Probability (List_Probability xs) = toXMLConsList_Probability xs
toXMLList_Probability HoleList_Probability = []
toXMLList_Probability (ParseErrList_Probability _) = []
toXMLConsList_Probtable (Cons_Probtable x xs) = toXMLProbtable x : toXMLConsList_Probtable xs
toXMLConsList_Probtable Nil_Probtable             = []
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
toXMLConsList_Value (Cons_Value x xs) = toXMLValue x : toXMLConsList_Value xs
toXMLConsList_Value Nil_Value             = []
toXMLConsList_Axis (Cons_Axis x xs) = toXMLAxis x : toXMLConsList_Axis xs
toXMLConsList_Axis Nil_Axis             = []
toXMLConsList_Probability (Cons_Probability x xs) = toXMLProbability x : toXMLConsList_Probability xs
toXMLConsList_Probability Nil_Probability             = []



--------------------------------------------------------------------------
-- parseXML functions                                                   --
--------------------------------------------------------------------------

parseXML_EnrichedDoc = parseXMLCns_RootEnr <|> parseHoleAndParseErr "EnrichedDoc" HoleEnrichedDoc
parseXMLCns_RootEnr = RootEnr <$ startTag "RootEnr" <*> parseXML_Root<* endTag "RootEnr"
parseXML_Document = parseXMLCns_RootDoc <|> parseHoleAndParseErr "Document" HoleDocument
parseXMLCns_RootDoc = RootDoc <$ startTag "RootDoc" <*> parseXML_Root<* endTag "RootDoc"
parseXML_Root = parseXMLCns_Root <|> parseHoleAndParseErr "Root" HoleRoot
parseXMLCns_Root = Root <$ startTag "Root" <*> parseXML_Graph <*> parseXML_String <*> parseXML_String <*> parseXML_List_Probtable <*> parseXML_String <*> parseXML_List_Section<* endTag "Root"
parseXML_Section = parseXMLCns_Section <|> parseHoleAndParseErr "Section" HoleSection
parseXMLCns_Section = Section <$ startTag "Section" <*> parseXML_String <*> parseXML_List_Paragraph <*> parseXML_List_Subsection<* endTag "Section"
parseXML_Subsection = parseXMLCns_Subsection <|> parseHoleAndParseErr "Subsection" HoleSubsection
parseXMLCns_Subsection = Subsection <$ startTag "Subsection" <*> parseXML_String <*> parseXML_List_Paragraph <*> parseXML_List_Subsubsection<* endTag "Subsection"
parseXML_Subsubsection = parseXMLCns_Subsubsection <|> parseHoleAndParseErr "Subsubsection" HoleSubsubsection
parseXMLCns_Subsubsection = Subsubsection <$ startTag "Subsubsection" <*> parseXML_String <*> parseXML_List_Paragraph<* endTag "Subsubsection"
parseXML_Paragraph = parseXMLCns_Paragraph <|> parseXMLCns_SubgraphPara <|> parseXMLCns_ProbtablePara <|> parseHoleAndParseErr "Paragraph" HoleParagraph
parseXMLCns_Paragraph = Paragraph <$ startTag "Paragraph" <*> parseXML_List_Word<* endTag "Paragraph"
parseXMLCns_SubgraphPara = SubgraphPara <$ startTag "SubgraphPara" <*> parseXML_Subgraph <*> parseXML_String <*> parseXML_String<* endTag "SubgraphPara"
parseXMLCns_ProbtablePara = ProbtablePara <$ startTag "ProbtablePara" <*> parseXML_Probtable<* endTag "ProbtablePara"
parseXML_Word = parseXMLCns_Word <|> parseXMLCns_NodeRef <|> parseXMLCns_Label <|> parseXMLCns_LabelRef <|> parseHoleAndParseErr "Word" HoleWord
parseXMLCns_Word = Word NoIDP <$ startTag "Word" <*> parseXML_String<* endTag "Word"
parseXMLCns_NodeRef = NodeRef <$ startTag "NodeRef" <*> parseXML_NodeName<* endTag "NodeRef"
parseXMLCns_Label = Label <$ startTag "Label" <*> parseXML_String<* endTag "Label"
parseXMLCns_LabelRef = LabelRef <$ startTag "LabelRef" <*> parseXML_String<* endTag "LabelRef"
parseXML_NodeName = parseXMLCns_NodeName <|> parseHoleAndParseErr "NodeName" HoleNodeName
parseXMLCns_NodeName = NodeName <$ startTag "NodeName" <*> parseXML_String<* endTag "NodeName"
parseXML_Graph = parseXMLCns_Graph <|> parseHoleAndParseErr "Graph" HoleGraph
parseXMLCns_Graph = Graph <$ startTag "Graph" <*> parseXML_Dirty <*> parseXML_List_Vertex <*> parseXML_List_Edge<* endTag "Graph"
parseXML_Vertex = parseXMLCns_Vertex <|> parseHoleAndParseErr "Vertex" HoleVertex
parseXMLCns_Vertex = Vertex <$ startTag "Vertex" <*> parseXML_String <*> parseXML_Shape <*> parseXML_Int <*> parseXML_Int <*> parseXML_Int<* endTag "Vertex"
parseXML_Shape = parseXMLCns_Circle <|> parseXMLCns_Square <|> parseHoleAndParseErr "Shape" HoleShape
parseXMLCns_Circle = Circle <$ emptyTag "Circle"
parseXMLCns_Square = Square <$ emptyTag "Square"
parseXML_Edge = parseXMLCns_Edge <|> parseHoleAndParseErr "Edge" HoleEdge
parseXMLCns_Edge = Edge <$ startTag "Edge" <*> parseXML_Int <*> parseXML_Int<* endTag "Edge"
parseXML_Subgraph = parseXMLCns_Subgraph <|> parseHoleAndParseErr "Subgraph" HoleSubgraph
parseXMLCns_Subgraph = Subgraph <$ startTag "Subgraph" <*> parseXML_Dirty <*> parseXML_List_Vertex <*> parseXML_List_Edge<* endTag "Subgraph"
parseXML_Dirty = parseXMLCns_Dirty <|> parseXMLCns_Clean <|> parseHoleAndParseErr "Dirty" HoleDirty
parseXMLCns_Dirty = Dirty <$ emptyTag "Dirty"
parseXMLCns_Clean = Clean <$ emptyTag "Clean"
parseXML_Probtable = parseXMLCns_Probtable <|> parseHoleAndParseErr "Probtable" HoleProbtable
parseXMLCns_Probtable = Probtable <$ startTag "Probtable" <*> parseXML_Int <*> parseXML_List_Value <*> parseXML_Table<* endTag "Probtable"
parseXML_Value = parseXMLCns_Value <|> parseHoleAndParseErr "Value" HoleValue
parseXMLCns_Value = Value <$ startTag "Value" <*> parseXML_String<* endTag "Value"
parseXML_Table = parseXMLCns_Table <|> parseHoleAndParseErr "Table" HoleTable
parseXMLCns_Table = Table <$ startTag "Table" <*> parseXML_List_Int <*> parseXML_List_Axis <*> parseXML_List_Probability<* endTag "Table"
parseXML_Axis = parseXMLCns_Axis <|> parseHoleAndParseErr "Axis" HoleAxis
parseXMLCns_Axis = Axis <$ startTag "Axis" <*> parseXML_List_Value<* endTag "Axis"
parseXML_Probability = parseXMLCns_Probability <|> parseHoleAndParseErr "Probability" HoleProbability
parseXMLCns_Probability = Probability <$ startTag "Probability" <*> parseXML_String<* endTag "Probability"
parseXML_List_Probtable = mkList List_Probtable Cons_Probtable Nil_Probtable <$> pList_ng parseXML_Probtable
parseXML_List_Section = mkList List_Section Cons_Section Nil_Section <$> pList_ng parseXML_Section
parseXML_List_Paragraph = mkList List_Paragraph Cons_Paragraph Nil_Paragraph <$> pList_ng parseXML_Paragraph
parseXML_List_Subsection = mkList List_Subsection Cons_Subsection Nil_Subsection <$> pList_ng parseXML_Subsection
parseXML_List_Subsubsection = mkList List_Subsubsection Cons_Subsubsection Nil_Subsubsection <$> pList_ng parseXML_Subsubsection
parseXML_List_Word = mkList List_Word Cons_Word Nil_Word <$> pList_ng parseXML_Word
parseXML_List_Vertex = mkList List_Vertex Cons_Vertex Nil_Vertex <$> pList_ng parseXML_Vertex
parseXML_List_Edge = mkList List_Edge Cons_Edge Nil_Edge <$> pList_ng parseXML_Edge
parseXML_List_Value = mkList List_Value Cons_Value Nil_Value <$> pList_ng parseXML_Value
parseXML_List_Axis = mkList List_Axis Cons_Axis Nil_Axis <$> pList_ng parseXML_Axis
parseXML_List_Probability = mkList List_Probability Cons_Probability Nil_Probability <$> pList_ng parseXML_Probability



--------------------------------------------------------------------------
-- List utility functions                                               --
--------------------------------------------------------------------------

toList_Probtable vs = List_Probtable (toConsList_Probtable vs)

fromList_Probtable (List_Probtable vs) = fromConsList_Probtable vs
fromList_Probtable _ = []

toConsList_Probtable [] = Nil_Probtable
toConsList_Probtable (x:xs) = Cons_Probtable x (toConsList_Probtable xs)

fromConsList_Probtable Nil_Probtable = []
fromConsList_Probtable (Cons_Probtable x xs) = x: fromConsList_Probtable xs

replaceList_Probtable _ x Nil_Probtable = Nil_Probtable  -- replace beyond end of list
replaceList_Probtable 0 x (Cons_Probtable cx cxs) = Cons_Probtable x cxs
replaceList_Probtable n x (Cons_Probtable cx cxs) = Cons_Probtable cx (replaceList_Probtable (n-1) x cxs)

insertList_Probtable 0 x cxs = Cons_Probtable x cxs
insertList_Probtable _ x Nil_Probtable  = Nil_Probtable  -- insert beyond end of list
insertList_Probtable n x (Cons_Probtable cx cxs) = Cons_Probtable cx (insertList_Probtable (n-1) x cxs)

removeList_Probtable _ Nil_Probtable  = Nil_Probtable  -- remove beyond end of list
removeList_Probtable 0 (Cons_Probtable cx cxs) = cxs
removeList_Probtable n (Cons_Probtable cx cxs) = Cons_Probtable cx (removeList_Probtable (n-1) cxs)

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

toList_Value vs = List_Value (toConsList_Value vs)

fromList_Value (List_Value vs) = fromConsList_Value vs
fromList_Value _ = []

toConsList_Value [] = Nil_Value
toConsList_Value (x:xs) = Cons_Value x (toConsList_Value xs)

fromConsList_Value Nil_Value = []
fromConsList_Value (Cons_Value x xs) = x: fromConsList_Value xs

replaceList_Value _ x Nil_Value = Nil_Value  -- replace beyond end of list
replaceList_Value 0 x (Cons_Value cx cxs) = Cons_Value x cxs
replaceList_Value n x (Cons_Value cx cxs) = Cons_Value cx (replaceList_Value (n-1) x cxs)

insertList_Value 0 x cxs = Cons_Value x cxs
insertList_Value _ x Nil_Value  = Nil_Value  -- insert beyond end of list
insertList_Value n x (Cons_Value cx cxs) = Cons_Value cx (insertList_Value (n-1) x cxs)

removeList_Value _ Nil_Value  = Nil_Value  -- remove beyond end of list
removeList_Value 0 (Cons_Value cx cxs) = cxs
removeList_Value n (Cons_Value cx cxs) = Cons_Value cx (removeList_Value (n-1) cxs)

toList_Axis vs = List_Axis (toConsList_Axis vs)

fromList_Axis (List_Axis vs) = fromConsList_Axis vs
fromList_Axis _ = []

toConsList_Axis [] = Nil_Axis
toConsList_Axis (x:xs) = Cons_Axis x (toConsList_Axis xs)

fromConsList_Axis Nil_Axis = []
fromConsList_Axis (Cons_Axis x xs) = x: fromConsList_Axis xs

replaceList_Axis _ x Nil_Axis = Nil_Axis  -- replace beyond end of list
replaceList_Axis 0 x (Cons_Axis cx cxs) = Cons_Axis x cxs
replaceList_Axis n x (Cons_Axis cx cxs) = Cons_Axis cx (replaceList_Axis (n-1) x cxs)

insertList_Axis 0 x cxs = Cons_Axis x cxs
insertList_Axis _ x Nil_Axis  = Nil_Axis  -- insert beyond end of list
insertList_Axis n x (Cons_Axis cx cxs) = Cons_Axis cx (insertList_Axis (n-1) x cxs)

removeList_Axis _ Nil_Axis  = Nil_Axis  -- remove beyond end of list
removeList_Axis 0 (Cons_Axis cx cxs) = cxs
removeList_Axis n (Cons_Axis cx cxs) = Cons_Axis cx (removeList_Axis (n-1) cxs)

toList_Probability vs = List_Probability (toConsList_Probability vs)

fromList_Probability (List_Probability vs) = fromConsList_Probability vs
fromList_Probability _ = []

toConsList_Probability [] = Nil_Probability
toConsList_Probability (x:xs) = Cons_Probability x (toConsList_Probability xs)

fromConsList_Probability Nil_Probability = []
fromConsList_Probability (Cons_Probability x xs) = x: fromConsList_Probability xs

replaceList_Probability _ x Nil_Probability = Nil_Probability  -- replace beyond end of list
replaceList_Probability 0 x (Cons_Probability cx cxs) = Cons_Probability x cxs
replaceList_Probability n x (Cons_Probability cx cxs) = Cons_Probability cx (replaceList_Probability (n-1) x cxs)

insertList_Probability 0 x cxs = Cons_Probability x cxs
insertList_Probability _ x Nil_Probability  = Nil_Probability  -- insert beyond end of list
insertList_Probability n x (Cons_Probability cx cxs) = Cons_Probability cx (insertList_Probability (n-1) x cxs)

removeList_Probability _ Nil_Probability  = Nil_Probability  -- remove beyond end of list
removeList_Probability 0 (Cons_Probability cx cxs) = cxs
removeList_Probability n (Cons_Probability cx cxs) = Cons_Probability cx (removeList_Probability (n-1) cxs)




--------------------------------------------------------------------------
-- Miscellaneous                                                        --
--------------------------------------------------------------------------

type Presentation_ = Presentation Document EnrichedDoc Node ClipDoc UserToken

instance Doc Document where
  initialDoc = initialDocument
  toXML = toXMLDocument
  parseXML = parseXML_Document <* pCharSpaces

instance Eq Node where
  nd1 == nd2 = rankNode nd1 == rankNode nd2

instance Ord Node where
  nd1 <= nd2 = rankNode nd1 <= rankNode nd2


-- toXML for primitive types

toXMLInt i = EmptyElt "Integer" [("val", show i)]

toXMLFloat f = EmptyElt "Float" [("val", show f)]

toXMLBool b = EmptyElt "Bool" [("val", show b)]

toXMLString str = Elt "String" [] [PCData str] 


-- parseXML for primitive types

parseXML_Int :: CharParser Int
parseXML_Int  =
      read 
  <$  pCharSpaces
  <*  pCharString "<Integer val=\""
  <*> pList (pExcept ('\0','\255','x') "\"") 
  <*  pCharString "\"/>"

parseXML_Float :: CharParser Float
parseXML_Float  =
      read 
  <$  pCharSpaces
  <*  pCharString "<Float val=\""
  <*> pList (pExcept ('\0','\255','x') "\"") 
  <*  pCharString "\"/>"

parseXML_Bool :: CharParser Bool
parseXML_Bool  =
      read 
  <$  pCharSpaces
  <*  pCharString "<Bool val=\""
  <*> pList (pExcept ('\0','\255','x') "\"") 
  <*  pCharString "\"/>"

parseXML_String :: CharParser String
parseXML_String  =
      id
  <$  pCharSpaces
  <*  pCharString "<String>"
  <*> pList (pExcept ('\0','\255','x') "<") 
  <*  pCharString "</String>"
 

-- Xprez XML presentation for primitive types

presentPrimXMLInt :: Int -> Presentation_
presentPrimXMLInt x = text $ "<Int>"++show x++"<Int/>"

presentPrimXMLFloat :: Float -> Presentation_
presentPrimXMLFloat x = text $ "<Float>"++show x++"<Float>"

presentPrimXMLBool :: Bool -> Presentation_
presentPrimXMLBool x = text $ "<Bool>"++show x++"<Bool/>"

presentPrimXMLString :: String -> Presentation_
presentPrimXMLString x = text $ "<String>"++x++"<String>"


-- Xprez tree presentation for primitive types

presentPrimTreeInt :: Int -> Presentation_
presentPrimTreeInt x =  mkTreeLeaf False $ text $ "Int: "++show x

presentPrimTreeFloat :: Float -> Presentation_
presentPrimTreeFloat x =  mkTreeLeaf False $ text $ "Float: "++show x

presentPrimTreeBool :: Bool -> Presentation_
presentPrimTreeBool x =  mkTreeLeaf False $ text $ "Bool: "++show x

presentPrimTreeString :: String -> Presentation_
presentPrimTreeString x =  mkTreeLeaf False $ text $ "String: "++x


