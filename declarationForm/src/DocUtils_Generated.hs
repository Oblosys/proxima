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
initialDocument = return $ RootDoc $ SudokuDoc $ Sudoku (Float_ 1) (Fload_ 1)


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
rankNode (Node_SudokuDoc _ _) = 7
rankNode (Node_HoleChoiceDoc _ _) = 8
rankNode (Node_ParseErrChoiceDoc _ _) = 9
rankNode (Node_Sudoku _ _) = 10
rankNode (Node_HoleSudoku _ _) = 11
rankNode (Node_ParseErrSudoku _ _) = 12
rankNode (Node_Float_ _ _) = 13
rankNode (Node_HoleFloat_ _ _) = 14
rankNode (Node_ParseErrFloat_ _ _) = 15
rankNode (Node_Fload_ _ _) = 16
rankNode (Node_HoleFload_ _ _) = 17
rankNode (Node_ParseErrFload_ _ _) = 18



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
  pathNode (Node_SudokuDoc _ pth) = PathD pth
  pathNode (Node_HoleChoiceDoc _ pth) = PathD pth
  pathNode (Node_ParseErrChoiceDoc _ pth) = PathD pth
  pathNode (Node_Sudoku _ pth) = PathD pth
  pathNode (Node_HoleSudoku _ pth) = PathD pth
  pathNode (Node_ParseErrSudoku _ pth) = PathD pth
  pathNode (Node_Float_ _ pth) = PathD pth
  pathNode (Node_HoleFloat_ _ pth) = PathD pth
  pathNode (Node_ParseErrFloat_ _ pth) = PathD pth
  pathNode (Node_Fload_ _ pth) = PathD pth
  pathNode (Node_HoleFload_ _ pth) = PathD pth
  pathNode (Node_ParseErrFload_ _ pth) = PathD pth

  typeOfNode (Node_RootEnr _ _) = BasicType "EnrichedDoc"
  typeOfNode (Node_HoleEnrichedDoc _ _) = BasicType "EnrichedDoc"
  typeOfNode (Node_ParseErrEnrichedDoc _ _) = BasicType "EnrichedDoc"
  typeOfNode (Node_RootDoc _ _) = BasicType "Document"
  typeOfNode (Node_HoleDocument _ _) = BasicType "Document"
  typeOfNode (Node_ParseErrDocument _ _) = BasicType "Document"
  typeOfNode (Node_SudokuDoc _ _) = BasicType "ChoiceDoc"
  typeOfNode (Node_HoleChoiceDoc _ _) = BasicType "ChoiceDoc"
  typeOfNode (Node_ParseErrChoiceDoc _ _) = BasicType "ChoiceDoc"
  typeOfNode (Node_Sudoku _ _) = BasicType "Sudoku"
  typeOfNode (Node_HoleSudoku _ _) = BasicType "Sudoku"
  typeOfNode (Node_ParseErrSudoku _ _) = BasicType "Sudoku"
  typeOfNode (Node_Float_ _ _) = BasicType "Float_"
  typeOfNode (Node_HoleFloat_ _ _) = BasicType "Float_"
  typeOfNode (Node_ParseErrFloat_ _ _) = BasicType "Float_"
  typeOfNode (Node_Fload_ _ _) = BasicType "Fload_"
  typeOfNode (Node_HoleFload_ _ _) = BasicType "Fload_"
  typeOfNode (Node_ParseErrFload_ _ _) = BasicType "Fload_"



--------------------------------------------------------------------------
-- toXML functions                                                      --
--------------------------------------------------------------------------

toXMLEnrichedDoc (RootEnr choiceDoc) = Elt "RootEnr" [] $ [toXMLChoiceDoc choiceDoc]
toXMLEnrichedDoc (HoleEnrichedDoc) = EmptyElt "HoleEnrichedDoc" [] 
toXMLEnrichedDoc (ParseErrEnrichedDoc error) = EmptyElt "ParseErrEnrichedDoc" []
toXMLDocument (RootDoc choiceDoc) = Elt "RootDoc" [] $ [toXMLChoiceDoc choiceDoc]
toXMLDocument (HoleDocument) = EmptyElt "HoleDocument" [] 
toXMLDocument (ParseErrDocument error) = EmptyElt "ParseErrDocument" []
toXMLChoiceDoc (SudokuDoc sudoku) = Elt "SudokuDoc" [] $ [toXMLSudoku sudoku]
toXMLChoiceDoc (HoleChoiceDoc) = EmptyElt "HoleChoiceDoc" [] 
toXMLChoiceDoc (ParseErrChoiceDoc error) = EmptyElt "ParseErrChoiceDoc" []
toXMLSudoku (Sudoku f i) = Elt "Sudoku" [] $ [toXMLFloat_ f] ++ [toXMLFload_ i]
toXMLSudoku (HoleSudoku) = EmptyElt "HoleSudoku" [] 
toXMLSudoku (ParseErrSudoku error) = EmptyElt "ParseErrSudoku" []
toXMLFloat_ (Float_ value) = Elt "Float_" [] $ [toXMLFloat value]
toXMLFloat_ (HoleFloat_) = EmptyElt "HoleFloat_" [] 
toXMLFloat_ (ParseErrFloat_ error) = EmptyElt "ParseErrFloat_" []
toXMLFload_ (Fload_ value) = Elt "Fload_" [] $ [toXMLInt value]
toXMLFload_ (HoleFload_) = EmptyElt "HoleFload_" [] 
toXMLFload_ (ParseErrFload_ error) = EmptyElt "ParseErrFload_" []



--------------------------------------------------------------------------
-- parseXML functions                                                   --
--------------------------------------------------------------------------

parseXML_EnrichedDoc = parseXMLCns_RootEnr <|> parseHoleAndParseErr "EnrichedDoc" HoleEnrichedDoc
parseXMLCns_RootEnr = RootEnr <$ startTag "RootEnr" <*> parseXML_ChoiceDoc<* endTag "RootEnr"
parseXML_Document = parseXMLCns_RootDoc <|> parseHoleAndParseErr "Document" HoleDocument
parseXMLCns_RootDoc = RootDoc <$ startTag "RootDoc" <*> parseXML_ChoiceDoc<* endTag "RootDoc"
parseXML_ChoiceDoc = parseXMLCns_SudokuDoc <|> parseHoleAndParseErr "ChoiceDoc" HoleChoiceDoc
parseXMLCns_SudokuDoc = SudokuDoc <$ startTag "SudokuDoc" <*> parseXML_Sudoku<* endTag "SudokuDoc"
parseXML_Sudoku = parseXMLCns_Sudoku <|> parseHoleAndParseErr "Sudoku" HoleSudoku
parseXMLCns_Sudoku = Sudoku <$ startTag "Sudoku" <*> parseXML_Float_ <*> parseXML_Fload_<* endTag "Sudoku"
parseXML_Float_ = parseXMLCns_Float_ <|> parseHoleAndParseErr "Float_" HoleFloat_
parseXMLCns_Float_ = Float_ <$ startTag "Float_" <*> parseXML_Float<* endTag "Float_"
parseXML_Fload_ = parseXMLCns_Fload_ <|> parseHoleAndParseErr "Fload_" HoleFload_
parseXMLCns_Fload_ = Fload_ <$ startTag "Fload_" <*> parseXML_Int<* endTag "Fload_"



--------------------------------------------------------------------------
-- List utility functions                                               --
--------------------------------------------------------------------------




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


