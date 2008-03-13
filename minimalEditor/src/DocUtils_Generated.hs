module DocUtils_Generated where

import Evaluation.DocTypes
import DocTypes_Generated
import Evaluation.DocUtils
import Evaluation.DocumentEdit
import Presentation.PresTypes
import Presentation.XprezLib
import Common.DebugLevels
import Text.ParserCombinators.Parsec

import Common.CommonTypes hiding (Clean, Dirty)

initialDocument :: IO Document
initialDocument = return (RootDoc Leaf)

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



--------------------------------------------------------------------------
-- toXML functions                                                      --
--------------------------------------------------------------------------

toXMLEnrichedDoc (RootEnr tree) = Elt "RootEnr" [] $ [toXMLTree tree]
toXMLEnrichedDoc (HoleEnrichedDoc) = Elt "HoleEnrichedDoc" [] $ []
toXMLEnrichedDoc (ParseErrEnrichedDoc presentation) = Elt "ParseErrEnrichedDoc" [] []
toXMLDocument (RootDoc tree) = Elt "RootDoc" [] $ [toXMLTree tree]
toXMLDocument (HoleDocument) = Elt "HoleDocument" [] $ []
toXMLDocument (ParseErrDocument presentation) = Elt "ParseErrDocument" [] []
toXMLTree (Bin left right) = Elt "Bin" [] $ [toXMLTree left] ++ [toXMLTree right]
toXMLTree (Leaf) = Elt "Leaf" [] $ []
toXMLTree (HoleTree) = Elt "HoleTree" [] $ []
toXMLTree (ParseErrTree presentation) = Elt "ParseErrTree" [] []



--------------------------------------------------------------------------
-- parseXML functions                                                   --
--------------------------------------------------------------------------

parseXML_EnrichedDoc = parseXMLCns_RootEnr <?|> parseHoleAndParseErr "EnrichedDoc" HoleEnrichedDoc
parseXMLCns_RootEnr = RootEnr <$ startTag "RootEnr" <*> parseXML_Tree<* endTag "RootEnr"
parseXML_Document = parseXMLCns_RootDoc <?|> parseHoleAndParseErr "Document" HoleDocument
parseXMLCns_RootDoc = RootDoc <$ startTag "RootDoc" <*> parseXML_Tree<* endTag "RootDoc"
parseXML_Tree = parseXMLCns_Bin <?|> parseXMLCns_Leaf <?|> parseHoleAndParseErr "Tree" HoleTree
parseXMLCns_Bin = Bin <$ startTag "Bin" <*> parseXML_Tree <*> parseXML_Tree<* endTag "Bin"
parseXMLCns_Leaf = Leaf <$ emptyTag "Leaf"



--------------------------------------------------------------------------
-- List utility functions                                               --
--------------------------------------------------------------------------




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

parseXML_Int :: Parser Int
parseXML_Int  =
 do { spaces
    ; string "<Integer val=\""
    ; str <- many (satisfy (/='"')) 
    ; string "\"/>"
    ; return $ read str
    } 

parseXML_Float :: Parser Float
parseXML_Float  =
 do { spaces
    ; string "<Float val=\""
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

parseXML_String :: Parser String
parseXML_String =
 do { spaces
    ; string "<String>"
    ; str <- many (satisfy (/='<')) 
    ; string "</String>"
    ; return str
    }
 

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


