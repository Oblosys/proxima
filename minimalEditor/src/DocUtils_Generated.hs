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
initialDocument = return (RootDoc (toList_Tree [Bin NoIDP NoIDP NoIDP NoIDP NoIDP 
                                                  (Leaf NoIDP NoIDP 0) (Leaf NoIDP NoIDP 1)]) 
                                  (toList_Tree [Leaf NoIDP NoIDP 100,Leaf NoIDP NoIDP 200]))

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
rankNode (Node_Bin _ _) = 7
rankNode (Node_Leaf _ _) = 8
rankNode (Node_HoleTree _ _) = 9
rankNode (Node_ParseErrTree _ _) = 10
rankNode (Node_List_Tree _ _) = 11
rankNode (Node_HoleList_Tree _ _) = 12
rankNode (Node_ParseErrList_Tree _ _) = 13



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
  pathNode (Node_Bin _ pth) = PathD pth
  pathNode (Node_Leaf _ pth) = PathD pth
  pathNode (Node_HoleTree _ pth) = PathD pth
  pathNode (Node_ParseErrTree _ pth) = PathD pth
  pathNode (Node_List_Tree _ pth) = PathD pth
  pathNode (Node_HoleList_Tree _ pth) = PathD pth
  pathNode (Node_ParseErrList_Tree _ pth) = PathD pth



--------------------------------------------------------------------------
-- toXML functions                                                      --
--------------------------------------------------------------------------

toXMLEnrichedDoc (RootEnr trees trees2) = Elt "RootEnr" [] $ toXMLList_Tree trees ++ toXMLList_Tree trees2
toXMLEnrichedDoc (HoleEnrichedDoc) = Elt "HoleEnrichedDoc" [] $ []
toXMLEnrichedDoc (ParseErrEnrichedDoc error) = Elt "ParseErrEnrichedDoc" [] []
toXMLDocument (RootDoc trees trees2) = Elt "RootDoc" [] $ toXMLList_Tree trees ++ toXMLList_Tree trees2
toXMLDocument (HoleDocument) = Elt "HoleDocument" [] $ []
toXMLDocument (ParseErrDocument error) = Elt "ParseErrDocument" [] []
toXMLTree (Bin _ _ _ _ _ left right) = Elt "Bin" [] $ [toXMLTree left] ++ [toXMLTree right]
toXMLTree (Leaf _ _ int) = Elt "Leaf" [] $ [toXMLInt int]
toXMLTree (HoleTree) = Elt "HoleTree" [] $ []
toXMLTree (ParseErrTree error) = Elt "ParseErrTree" [] []
toXMLList_Tree (List_Tree xs) = toXMLConsList_Tree xs
toXMLList_Tree HoleList_Tree = []
toXMLList_Tree (ParseErrList_Tree _) = []
toXMLConsList_Tree (Cons_Tree x xs) = toXMLTree x : toXMLConsList_Tree xs
toXMLConsList_Tree Nil_Tree             = []



--------------------------------------------------------------------------
-- parseXML functions                                                   --
--------------------------------------------------------------------------

parseXML_EnrichedDoc = parseXMLCns_RootEnr <|> parseHoleAndParseErr "EnrichedDoc" HoleEnrichedDoc
parseXMLCns_RootEnr = RootEnr <$ startTag "RootEnr" <*> parseXML_List_Tree <*> parseXML_List_Tree<* endTag "RootEnr"
parseXML_Document = parseXMLCns_RootDoc <|> parseHoleAndParseErr "Document" HoleDocument
parseXMLCns_RootDoc = RootDoc <$ startTag "RootDoc" <*> parseXML_List_Tree <*> parseXML_List_Tree<* endTag "RootDoc"
parseXML_Tree = parseXMLCns_Bin <|> parseXMLCns_Leaf <|> parseHoleAndParseErr "Tree" HoleTree
parseXMLCns_Bin = Bin NoIDP NoIDP NoIDP NoIDP NoIDP <$ startTag "Bin" <*> parseXML_Tree <*> parseXML_Tree<* endTag "Bin"
parseXMLCns_Leaf = Leaf NoIDP NoIDP <$ startTag "Leaf" <*> parseXML_Int<* endTag "Leaf"
parseXML_List_Tree = mkList List_Tree Cons_Tree Nil_Tree <$> pList_ng parseXML_Tree



--------------------------------------------------------------------------
-- List utility functions                                               --
--------------------------------------------------------------------------

toList_Tree vs = List_Tree (toConsList_Tree vs)

fromList_Tree (List_Tree vs) = fromConsList_Tree vs
fromList_Tree _ = []

toConsList_Tree [] = Nil_Tree
toConsList_Tree (x:xs) = Cons_Tree x (toConsList_Tree xs)

fromConsList_Tree Nil_Tree = []
fromConsList_Tree (Cons_Tree x xs) = x: fromConsList_Tree xs

replaceList_Tree _ x Nil_Tree = Nil_Tree  -- replace beyond end of list
replaceList_Tree 0 x (Cons_Tree cx cxs) = Cons_Tree x cxs
replaceList_Tree n x (Cons_Tree cx cxs) = Cons_Tree cx (replaceList_Tree (n-1) x cxs)

insertList_Tree 0 x cxs = Cons_Tree x cxs
insertList_Tree _ x Nil_Tree  = Nil_Tree  -- insert beyond end of list
insertList_Tree n x (Cons_Tree cx cxs) = Cons_Tree cx (insertList_Tree (n-1) x cxs)

removeList_Tree _ Nil_Tree  = Nil_Tree  -- remove beyond end of list
removeList_Tree 0 (Cons_Tree cx cxs) = cxs
removeList_Tree n (Cons_Tree cx cxs) = Cons_Tree cx (removeList_Tree (n-1) cxs)




--------------------------------------------------------------------------
-- Miscellaneous                                                        --
--------------------------------------------------------------------------

type Presentation_Doc_Node_Clip_Token = Presentation Document Node ClipDoc UserToken

instance Doc Document where
  initialDoc = initialDocument
  toXML = toXMLDocument
  parseXML = parseXML_Document

instance Eq Node where
  nd1 == nd2 = rankNode nd1 == rankNode nd2
  
instance Ord Node where
  nd1 <= nd2 = rankNode nd1 <= rankNode nd2

instance PopupMenuHack Node Document where
  mkDocNode doc = Node_RootDoc doc []


-- toXML for primitive types

toXMLInt i = Elt "Integer" [("val", show i)] []

toXMLInt f = Elt "Float" [("val", show f)] []

toXMLBool b = Elt "Bool" [("val", show b)] []

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

presentPrimXMLInt :: Int -> Presentation_Doc_Node_Clip_Token
presentPrimXMLInt x = text $ "<Int>"++show x++"<Int/>"

presentPrimXMLFloat :: String -> Presentation_Doc_Node_Clip_Token
presentPrimXMLFloat x = text $ "<Float>"++x++"<Float>"

presentPrimXMLBool :: Bool -> Presentation doc node clip token
presentPrimXMLBool x = text $ "<Bool>"++show x++"<Bool/>"

presentPrimXMLString :: String -> Presentation_Doc_Node_Clip_Token
presentPrimXMLString x = text $ "<String>"++x++"<String>"


-- Xprez tree presentation for primitive types

presentPrimTreeInt :: Int -> Presentation_Doc_Node_Clip_Token
presentPrimTreeInt x =  mkTreeLeaf False $ text $ "Int: "++show x

presentPrimTreeFloat :: Float -> Presentation_Doc_Node_Clip_Token
presentPrimTreeFloat x =  mkTreeLeaf False $ text $ "Float: "++show x

presentPrimTreeBool :: Bool -> Presentation_Doc_Node_Clip_Token
presentPrimTreeBool x =  mkTreeLeaf False $ text $ "Bool: "++show x

presentPrimTreeString :: String -> Presentation_Doc_Node_Clip_Token
presentPrimTreeString x =  mkTreeLeaf False $ text $ "String: "++x


