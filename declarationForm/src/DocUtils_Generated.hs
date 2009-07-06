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
initialDocument = return (RootDoc 
                           (Form "Martijn" "Informatica" 
                             (toList_Expense 
                               [ Expense "Koffie" 1 0
                               , Expense "Caipirinha" 3 1
                               ])
                             (toList_Currency
                               [ Currency "Real"   0.345715
                               , Currency "Dollar" 0.790938
                               ])
                           )
                         )

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
rankNode (Node_Form _ _) = 7
rankNode (Node_HoleForm _ _) = 8
rankNode (Node_ParseErrForm _ _) = 9
rankNode (Node_Expense _ _) = 10
rankNode (Node_HoleExpense _ _) = 11
rankNode (Node_ParseErrExpense _ _) = 12
rankNode (Node_Currency _ _) = 13
rankNode (Node_HoleCurrency _ _) = 14
rankNode (Node_ParseErrCurrency _ _) = 15
rankNode (Node_List_Expense _ _) = 16
rankNode (Node_HoleList_Expense _ _) = 17
rankNode (Node_ParseErrList_Expense _ _) = 18
rankNode (Node_List_Currency _ _) = 19
rankNode (Node_HoleList_Currency _ _) = 20
rankNode (Node_ParseErrList_Currency _ _) = 21



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
  pathNode (Node_Form _ pth) = PathD pth
  pathNode (Node_HoleForm _ pth) = PathD pth
  pathNode (Node_ParseErrForm _ pth) = PathD pth
  pathNode (Node_Expense _ pth) = PathD pth
  pathNode (Node_HoleExpense _ pth) = PathD pth
  pathNode (Node_ParseErrExpense _ pth) = PathD pth
  pathNode (Node_Currency _ pth) = PathD pth
  pathNode (Node_HoleCurrency _ pth) = PathD pth
  pathNode (Node_ParseErrCurrency _ pth) = PathD pth
  pathNode (Node_List_Expense _ pth) = PathD pth
  pathNode (Node_HoleList_Expense _ pth) = PathD pth
  pathNode (Node_ParseErrList_Expense _ pth) = PathD pth
  pathNode (Node_List_Currency _ pth) = PathD pth
  pathNode (Node_HoleList_Currency _ pth) = PathD pth
  pathNode (Node_ParseErrList_Currency _ pth) = PathD pth



--------------------------------------------------------------------------
-- toXML functions                                                      --
--------------------------------------------------------------------------

toXMLEnrichedDoc (RootEnr form) = Elt "RootEnr" [] $ [toXMLForm form]
toXMLEnrichedDoc (HoleEnrichedDoc) = EmptyElt "HoleEnrichedDoc" [] 
toXMLEnrichedDoc (ParseErrEnrichedDoc error) = EmptyElt "ParseErrEnrichedDoc" []
toXMLDocument (RootDoc form) = Elt "RootDoc" [] $ [toXMLForm form]
toXMLDocument (HoleDocument) = EmptyElt "HoleDocument" [] 
toXMLDocument (ParseErrDocument error) = EmptyElt "ParseErrDocument" []
toXMLForm (Form name faculty expenses currencies) = Elt "Form" [] $ [toXMLString name] ++ [toXMLString faculty] ++ toXMLList_Expense expenses ++ toXMLList_Currency currencies
toXMLForm (HoleForm) = EmptyElt "HoleForm" [] 
toXMLForm (ParseErrForm error) = EmptyElt "ParseErrForm" []
toXMLExpense (Expense description amount currencyIx) = Elt "Expense" [] $ [toXMLString description] ++ [toXMLFloat amount] ++ [toXMLInt currencyIx]
toXMLExpense (HoleExpense) = EmptyElt "HoleExpense" [] 
toXMLExpense (ParseErrExpense error) = EmptyElt "ParseErrExpense" []
toXMLCurrency (Currency name euroRate) = Elt "Currency" [] $ [toXMLString name] ++ [toXMLFloat euroRate]
toXMLCurrency (HoleCurrency) = EmptyElt "HoleCurrency" [] 
toXMLCurrency (ParseErrCurrency error) = EmptyElt "ParseErrCurrency" []
toXMLList_Expense (List_Expense xs) = toXMLConsList_Expense xs
toXMLList_Expense HoleList_Expense = []
toXMLList_Expense (ParseErrList_Expense _) = []
toXMLList_Currency (List_Currency xs) = toXMLConsList_Currency xs
toXMLList_Currency HoleList_Currency = []
toXMLList_Currency (ParseErrList_Currency _) = []
toXMLConsList_Expense (Cons_Expense x xs) = toXMLExpense x : toXMLConsList_Expense xs
toXMLConsList_Expense Nil_Expense             = []
toXMLConsList_Currency (Cons_Currency x xs) = toXMLCurrency x : toXMLConsList_Currency xs
toXMLConsList_Currency Nil_Currency             = []



--------------------------------------------------------------------------
-- parseXML functions                                                   --
--------------------------------------------------------------------------

parseXML_EnrichedDoc = parseXMLCns_RootEnr <|> parseHoleAndParseErr "EnrichedDoc" HoleEnrichedDoc
parseXMLCns_RootEnr = RootEnr <$ startTag "RootEnr" <*> parseXML_Form<* endTag "RootEnr"
parseXML_Document = parseXMLCns_RootDoc <|> parseHoleAndParseErr "Document" HoleDocument
parseXMLCns_RootDoc = RootDoc <$ startTag "RootDoc" <*> parseXML_Form<* endTag "RootDoc"
parseXML_Form = parseXMLCns_Form <|> parseHoleAndParseErr "Form" HoleForm
parseXMLCns_Form = Form <$ startTag "Form" <*> parseXML_String <*> parseXML_String <*> parseXML_List_Expense <*> parseXML_List_Currency<* endTag "Form"
parseXML_Expense = parseXMLCns_Expense <|> parseHoleAndParseErr "Expense" HoleExpense
parseXMLCns_Expense = Expense <$ startTag "Expense" <*> parseXML_String <*> parseXML_Float <*> parseXML_Int<* endTag "Expense"
parseXML_Currency = parseXMLCns_Currency <|> parseHoleAndParseErr "Currency" HoleCurrency
parseXMLCns_Currency = Currency <$ startTag "Currency" <*> parseXML_String <*> parseXML_Float<* endTag "Currency"
parseXML_List_Expense = mkList List_Expense Cons_Expense Nil_Expense <$> pList_ng parseXML_Expense
parseXML_List_Currency = mkList List_Currency Cons_Currency Nil_Currency <$> pList_ng parseXML_Currency



--------------------------------------------------------------------------
-- List utility functions                                               --
--------------------------------------------------------------------------

toList_Expense vs = List_Expense (toConsList_Expense vs)

fromList_Expense (List_Expense vs) = fromConsList_Expense vs
fromList_Expense _ = []

toConsList_Expense [] = Nil_Expense
toConsList_Expense (x:xs) = Cons_Expense x (toConsList_Expense xs)

fromConsList_Expense Nil_Expense = []
fromConsList_Expense (Cons_Expense x xs) = x: fromConsList_Expense xs

replaceList_Expense _ x Nil_Expense = Nil_Expense  -- replace beyond end of list
replaceList_Expense 0 x (Cons_Expense cx cxs) = Cons_Expense x cxs
replaceList_Expense n x (Cons_Expense cx cxs) = Cons_Expense cx (replaceList_Expense (n-1) x cxs)

insertList_Expense 0 x cxs = Cons_Expense x cxs
insertList_Expense _ x Nil_Expense  = Nil_Expense  -- insert beyond end of list
insertList_Expense n x (Cons_Expense cx cxs) = Cons_Expense cx (insertList_Expense (n-1) x cxs)

removeList_Expense _ Nil_Expense  = Nil_Expense  -- remove beyond end of list
removeList_Expense 0 (Cons_Expense cx cxs) = cxs
removeList_Expense n (Cons_Expense cx cxs) = Cons_Expense cx (removeList_Expense (n-1) cxs)

toList_Currency vs = List_Currency (toConsList_Currency vs)

fromList_Currency (List_Currency vs) = fromConsList_Currency vs
fromList_Currency _ = []

toConsList_Currency [] = Nil_Currency
toConsList_Currency (x:xs) = Cons_Currency x (toConsList_Currency xs)

fromConsList_Currency Nil_Currency = []
fromConsList_Currency (Cons_Currency x xs) = x: fromConsList_Currency xs

replaceList_Currency _ x Nil_Currency = Nil_Currency  -- replace beyond end of list
replaceList_Currency 0 x (Cons_Currency cx cxs) = Cons_Currency x cxs
replaceList_Currency n x (Cons_Currency cx cxs) = Cons_Currency cx (replaceList_Currency (n-1) x cxs)

insertList_Currency 0 x cxs = Cons_Currency x cxs
insertList_Currency _ x Nil_Currency  = Nil_Currency  -- insert beyond end of list
insertList_Currency n x (Cons_Currency cx cxs) = Cons_Currency cx (insertList_Currency (n-1) x cxs)

removeList_Currency _ Nil_Currency  = Nil_Currency  -- remove beyond end of list
removeList_Currency 0 (Cons_Currency cx cxs) = cxs
removeList_Currency n (Cons_Currency cx cxs) = Cons_Currency cx (removeList_Currency (n-1) cxs)




--------------------------------------------------------------------------
-- Miscellaneous                                                        --
--------------------------------------------------------------------------

type Presentation_Doc_Node_Clip_Token = Presentation Document Node ClipDoc UserToken

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

presentPrimXMLInt :: Int -> Presentation_Doc_Node_Clip_Token
presentPrimXMLInt x = text $ "<Int>"++show x++"<Int/>"

presentPrimXMLFloat :: Float -> Presentation_Doc_Node_Clip_Token
presentPrimXMLFloat x = text $ "<Float>"++show x++"<Float>"

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


