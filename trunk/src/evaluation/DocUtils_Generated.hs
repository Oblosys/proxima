module DocUtils_Generated where

import DocTypes
import PresTypes

--instance Show Node where
--  show NoNode = "<>"
--  show (DocNode doc pth) = "<"++shallowShowDoc1 doc++","++show pth++">"
--  show (ExpNode exp pth) = "<"++shallowShowExp1 exp++","++show pth++">"
--  show _ = "<NODE>"


data XML = Elt String [(String, String)] [XML] | PCData String | EmptyElt


showXML xml = 
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  ++ case xml of (Elt tg _ _) -> "<!DOCTYPE "++tg++" SYSTEM \""++tg++".dtd\" >\n"
                 _            -> ""
  ++ showXML' 0 xml
 where showXML' i (Elt tag ps []) = replicate i ' ' ++"<"++tag++showProperties ps++"/>\n"
       showXML' i (Elt tag ps [PCData str]) = replicate i ' ' ++"<"++tag++showProperties ps++">"++str++"</"++tag++">\n" 
       showXML' i (Elt tag ps cs) = replicate i ' ' ++"<"++tag++showProperties ps++">\n"
                              ++ concatMap (showXML' (i+2)) cs
                              ++ replicate i ' ' ++"</"++tag++">\n" 
       showXML' i (EmptyElt)     = replicate i ' ' ++"Empty\n"
       showXML' i (PCData str)   = replicate i ' ' ++str++"\n"
       showProperties [] = ""
       showProperties ((p,v):ps) = " "++p++"="++show v++ showProperties ps
-- element with one child PCDATA is displayed on one line. For more than one child it's too much of a hassle
-- because this function is only temporary

toXMLBool True = Elt "True" [] [] 
toXMLBool False = Elt "False" [] [] 

toXMLInt i = Elt "Integer" [("val", show i)] []

toXMLString str = Elt "String" [] [PCData str] 

toXMLRoot (RootDoc _ _ dcls)  = Elt "Module" [] $ toXMLList_Decl dcls
toXMLRoot _                   = Elt "ErrRoot" [] []



-- id functions: return (Just id) if node is the appropriate production, otherwise Nothing

----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

{- ------------------------------------

 generated part

-------------------------------------- -}
pathNode :: Node -> PathDoc
pathNode NoNode            = NoPathD
pathNode (EnrichedDocNode _ pth) = PathD pth
pathNode (List_DeclNode _ pth)  = PathD pth
pathNode (HeliumTypeInfoNode _ pth)  = PathD pth
pathNode (DocumentNode _ pth)  = PathD pth
pathNode (BoolNode _ pth)  = PathD pth
pathNode (IdentNode _ pth)  = PathD pth
pathNode (ExpNode _ pth)  = PathD pth
pathNode (BoardNode _ pth)  = PathD pth
pathNode (PPPresentationNode _ pth)  = PathD pth
pathNode (StringNode _ pth)  = PathD pth
pathNode (IntNode _ pth)  = PathD pth
pathNode (List_AltNode _ pth)  = PathD pth
pathNode (List_ExpNode _ pth)  = PathD pth
pathNode (BoardRowNode _ pth)  = PathD pth
pathNode (BoardSquareNode _ pth)  = PathD pth
pathNode (List_SlideNode _ pth)  = PathD pth
pathNode (String_Node _ pth)  = PathD pth
pathNode (ItemListNode _ pth)  = PathD pth
pathNode (ListTypeNode _ pth)  = PathD pth
pathNode (List_ItemNode _ pth)  = PathD pth
pathNode (DeclNode _ pth)  = PathD pth

pathNode (AltNode _ pth)  = PathD pth


pathNode (SlideNode _ pth)  = PathD pth

pathNode (ItemNode _ pth)  = PathD pth




rootEnrIDD :: Node -> Maybe IDD
rootEnrIDD (EnrichedDocNode (RootEnr iDP _ _ _ _ _) _) = Just iDP
rootEnrIDD _                                   = Nothing

declIDD :: Node -> Maybe IDD
declIDD (DeclNode (Decl iDP _ _ _ _ _ _ _ _) _) = Just iDP
declIDD _                                   = Nothing

boardDeclIDD :: Node -> Maybe IDD
boardDeclIDD (DeclNode (BoardDecl iDP _ _ _) _) = Just iDP
boardDeclIDD _                                   = Nothing

pPPresentationDeclIDD :: Node -> Maybe IDD
pPPresentationDeclIDD (DeclNode (PPPresentationDecl iDP _ _ _) _) = Just iDP
pPPresentationDeclIDD _                                   = Nothing

identIDD :: Node -> Maybe IDD
identIDD (IdentNode (Ident iDP _ _ _) _) = Just iDP
identIDD _                                   = Nothing

plusExpIDD :: Node -> Maybe IDD
plusExpIDD (ExpNode (PlusExp iDP _ _ _) _) = Just iDP
plusExpIDD _                                   = Nothing

timesExpIDD :: Node -> Maybe IDD
timesExpIDD (ExpNode (TimesExp iDP _ _ _) _) = Just iDP
timesExpIDD _                                   = Nothing

divExpIDD :: Node -> Maybe IDD
divExpIDD (ExpNode (DivExp iDP _ _ _) _) = Just iDP
divExpIDD _                                   = Nothing

powerExpIDD :: Node -> Maybe IDD
powerExpIDD (ExpNode (PowerExp iDP _ _ _) _) = Just iDP
powerExpIDD _                                   = Nothing

boolExpIDD :: Node -> Maybe IDD
boolExpIDD (ExpNode (BoolExp iDP _ _) _) = Just iDP
boolExpIDD _                                   = Nothing

intExpIDD :: Node -> Maybe IDD
intExpIDD (ExpNode (IntExp iDP _ _) _) = Just iDP
intExpIDD _                                   = Nothing

lamExpIDD :: Node -> Maybe IDD
lamExpIDD (ExpNode (LamExp iDP _ _ _ _) _) = Just iDP
lamExpIDD _                                   = Nothing

appExpIDD :: Node -> Maybe IDD
appExpIDD (ExpNode (AppExp iDP _ _) _) = Just iDP
appExpIDD _                                   = Nothing

caseExpIDD :: Node -> Maybe IDD
caseExpIDD (ExpNode (CaseExp iDP _ _ _ _) _) = Just iDP
caseExpIDD _                                   = Nothing

letExpIDD :: Node -> Maybe IDD
letExpIDD (ExpNode (LetExp iDP _ _ _ _) _) = Just iDP
letExpIDD _                                   = Nothing

identExpIDD :: Node -> Maybe IDD
identExpIDD (ExpNode (IdentExp iDP _) _) = Just iDP
identExpIDD _                                   = Nothing

ifExpIDD :: Node -> Maybe IDD
ifExpIDD (ExpNode (IfExp iDP _ _ _ _ _ _) _) = Just iDP
ifExpIDD _                                   = Nothing

parenExpIDD :: Node -> Maybe IDD
parenExpIDD (ExpNode (ParenExp iDP _ _ _) _) = Just iDP
parenExpIDD _                                   = Nothing

listExpIDD :: Node -> Maybe IDD
listExpIDD (ExpNode (ListExp iDP _ _ _ _) _) = Just iDP
listExpIDD _                                   = Nothing

productExpIDD :: Node -> Maybe IDD
productExpIDD (ExpNode (ProductExp iDP _ _ _ _) _) = Just iDP
productExpIDD _                                   = Nothing

altIDD :: Node -> Maybe IDD
altIDD (AltNode (Alt iDP _ _ _ _) _) = Just iDP
altIDD _                                   = Nothing

boardIDD :: Node -> Maybe IDD
boardIDD (BoardNode (Board iDP _ _ _ _ _ _ _ _) _) = Just iDP
boardIDD _                                   = Nothing

boardRowIDD :: Node -> Maybe IDD
boardRowIDD (BoardRowNode (BoardRow iDP _ _ _ _ _ _ _ _) _) = Just iDP
boardRowIDD _                                   = Nothing

queenIDD :: Node -> Maybe IDD
queenIDD (BoardSquareNode (Queen iDP _) _) = Just iDP
queenIDD _                                   = Nothing

kingIDD :: Node -> Maybe IDD
kingIDD (BoardSquareNode (King iDP _) _) = Just iDP
kingIDD _                                   = Nothing

bishopIDD :: Node -> Maybe IDD
bishopIDD (BoardSquareNode (Bishop iDP _) _) = Just iDP
bishopIDD _                                   = Nothing

knightIDD :: Node -> Maybe IDD
knightIDD (BoardSquareNode (Knight iDP _) _) = Just iDP
knightIDD _                                   = Nothing

rookIDD :: Node -> Maybe IDD
rookIDD (BoardSquareNode (Rook iDP _) _) = Just iDP
rookIDD _                                   = Nothing

pawnIDD :: Node -> Maybe IDD
pawnIDD (BoardSquareNode (Pawn iDP _) _) = Just iDP
pawnIDD _                                   = Nothing

emptyIDD _                                   = Nothing

pPPresentationIDD :: Node -> Maybe IDD
pPPresentationIDD (PPPresentationNode (PPPresentation iDP _ _) _) = Just iDP
pPPresentationIDD _                                   = Nothing

slideIDD :: Node -> Maybe IDD
slideIDD (SlideNode (Slide iDP _ _) _) = Just iDP
slideIDD _                                   = Nothing

itemListIDD :: Node -> Maybe IDD
itemListIDD (ItemListNode (ItemList iDP _ _) _) = Just iDP
itemListIDD _                                   = Nothing

bulletIDD :: Node -> Maybe IDD
bulletIDD (ListTypeNode (Bullet iDP) _) = Just iDP
bulletIDD _                                   = Nothing

numberIDD :: Node -> Maybe IDD
numberIDD (ListTypeNode (Number iDP) _) = Just iDP
numberIDD _                                   = Nothing

alphaIDD :: Node -> Maybe IDD
alphaIDD (ListTypeNode (Alpha iDP) _) = Just iDP
alphaIDD _                                   = Nothing

stringItemIDD :: Node -> Maybe IDD
stringItemIDD (ItemNode (StringItem iDP _) _) = Just iDP
stringItemIDD _                                   = Nothing

heliumItemIDD :: Node -> Maybe IDD
heliumItemIDD (ItemNode (HeliumItem iDP _) _) = Just iDP
heliumItemIDD _                                   = Nothing

listItemIDD :: Node -> Maybe IDD
listItemIDD (ItemNode (ListItem iDP _) _) = Just iDP
listItemIDD _                                   = Nothing

string_IDD :: Node -> Maybe IDD
string_IDD (String_Node (String_ iDP _) _) = Just iDP
string_IDD _                                   = Nothing




shallowShowEnrichedDoc1 (RootEnr  _ _ _ _ _ _) = "RootEnr"
shallowShowDecl1 (Decl  _ _ _ _ _ _ _ _ _) = "Decl"
shallowShowDecl1 (BoardDecl  _ _ _ _) = "BoardDecl"
shallowShowDecl1 (PPPresentationDecl  _ _ _ _) = "PPPresentationDecl"
shallowShowIdent1 (Ident  _ _ _ _) = "Ident"
shallowShowExp1 (PlusExp  _ _ _ _) = "PlusExp"
shallowShowExp1 (TimesExp  _ _ _ _) = "TimesExp"
shallowShowExp1 (DivExp  _ _ _ _) = "DivExp"
shallowShowExp1 (PowerExp  _ _ _ _) = "PowerExp"
shallowShowExp1 (BoolExp  _ _ _) = "BoolExp"
shallowShowExp1 (IntExp  _ _ _) = "IntExp"
shallowShowExp1 (LamExp  _ _ _ _ _) = "LamExp"
shallowShowExp1 (AppExp  _ _ _) = "AppExp"
shallowShowExp1 (CaseExp  _ _ _ _ _) = "CaseExp"
shallowShowExp1 (LetExp  _ _ _ _ _) = "LetExp"
shallowShowExp1 (IdentExp  _ _) = "IdentExp"
shallowShowExp1 (IfExp  _ _ _ _ _ _ _) = "IfExp"
shallowShowExp1 (ParenExp  _ _ _ _) = "ParenExp"
shallowShowExp1 (ListExp  _ _ _ _ _) = "ListExp"
shallowShowExp1 (ProductExp  _ _ _ _ _) = "ProductExp"
shallowShowAlt1 (Alt  _ _ _ _ _) = "Alt"
shallowShowBoard1 (Board  _ _ _ _ _ _ _ _ _) = "Board"
shallowShowBoardRow1 (BoardRow  _ _ _ _ _ _ _ _ _) = "BoardRow"
shallowShowBoardSquare1 (Queen  _ _) = "Queen"
shallowShowBoardSquare1 (King  _ _) = "King"
shallowShowBoardSquare1 (Bishop  _ _) = "Bishop"
shallowShowBoardSquare1 (Knight  _ _) = "Knight"
shallowShowBoardSquare1 (Rook  _ _) = "Rook"
shallowShowBoardSquare1 (Pawn  _ _) = "Pawn"
shallowShowBoardSquare1 (Empty ) = "Empty"
shallowShowPPPresentation1 (PPPresentation  _ _ _) = "PPPresentation"
shallowShowSlide1 (Slide  _ _ _) = "Slide"
shallowShowItemList1 (ItemList  _ _ _) = "ItemList"
shallowShowListType1 (Bullet  _) = "Bullet"
shallowShowListType1 (Number  _) = "Number"
shallowShowListType1 (Alpha  _) = "Alpha"
shallowShowItem1 (StringItem  _ _) = "StringItem"
shallowShowItem1 (HeliumItem  _ _) = "HeliumItem"
shallowShowItem1 (ListItem  _ _) = "ListItem"
shallowShowString_1 (String_  _ _) = "String_"
shallowShowList_Decl1 (List_Decl  _ _) = "List_Decl"
shallowShowConsList_Decl1 (Cons_Decl  _ _) = "Cons_Decl"
shallowShowConsList_Decl1 (Nil_Decl ) = "Nil_Decl"
shallowShowList_Alt1 (List_Alt  _ _) = "List_Alt"
shallowShowConsList_Alt1 (Cons_Alt  _ _) = "Cons_Alt"
shallowShowConsList_Alt1 (Nil_Alt ) = "Nil_Alt"
shallowShowList_Exp1 (List_Exp  _ _) = "List_Exp"
shallowShowConsList_Exp1 (Cons_Exp  _ _) = "Cons_Exp"
shallowShowConsList_Exp1 (Nil_Exp ) = "Nil_Exp"
shallowShowList_Slide1 (List_Slide  _ _) = "List_Slide"
shallowShowConsList_Slide1 (Cons_Slide  _ _) = "Cons_Slide"
shallowShowConsList_Slide1 (Nil_Slide ) = "Nil_Slide"
shallowShowList_Item1 (List_Item  _ _) = "List_Item"
shallowShowConsList_Item1 (Cons_Item  _ _) = "Cons_Item"
shallowShowConsList_Item1 (Nil_Item ) = "Nil_Item"



toXMLEnrichedDoc (RootEnr _ _ idListDecls decls heliumTypeInfo document) = Elt "RootEnr" [] $ toXMLDecls idListDecls
toXMLDecl (Decl _ _ _ _ _ expanded autoLayout ident exp) = Elt "Decl" [] $ [toXMLBool expanded] ++ [toXMLBool autoLayout] ++ [toXMLIdent ident] ++ [toXMLExp exp] ++ []
toXMLDecl (BoardDecl _ _ _ board) = Elt "BoardDecl" [] $ [toXMLBoard board] ++ []
toXMLDecl (PPPresentationDecl _ _ _ pPPresentation) = Elt "PPPresentationDecl" [] $ [toXMLPPPresentation pPPresentation] ++ []
toXMLIdent (Ident _ _ _ string) = Elt "Ident" [] $ [toXMLString string] ++ []
toXMLExp (PlusExp _ _ exp1 exp2) = Elt "PlusExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ []
toXMLExp (TimesExp _ _ exp1 exp2) = Elt "TimesExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ []
toXMLExp (DivExp _ _ exp1 exp2) = Elt "DivExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ []
toXMLExp (PowerExp _ _ exp1 exp2) = Elt "PowerExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ []
toXMLExp (BoolExp _ _ bool) = Elt "BoolExp" [] $ [toXMLBool bool] ++ []
toXMLExp (IntExp _ _ int) = Elt "IntExp" [] $ [toXMLInt int] ++ []
toXMLExp (LamExp _ _ _ ident exp) = Elt "LamExp" [] $ [toXMLIdent ident] ++ [toXMLExp exp] ++ []
toXMLExp (AppExp _ exp1 exp2) = Elt "AppExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ []
toXMLExp (CaseExp _ _ _ exp alts) = Elt "CaseExp" [] $ toXMLAlts alts
toXMLExp (LetExp _ _ _ decls exp) = Elt "LetExp" [] $ toXMLDecls decls
toXMLExp (IdentExp _ ident) = Elt "IdentExp" [] $ [toXMLIdent ident] ++ []
toXMLExp (IfExp _ _ _ _ exp1 exp2 exp3) = Elt "IfExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ [toXMLExp exp3] ++ []
toXMLExp (ParenExp _ _ _ exp) = Elt "ParenExp" [] $ [toXMLExp exp] ++ []
toXMLExp (ListExp _ _ _ _ exps) = Elt "ListExp" [] $ toXMLExps exps
toXMLExp (ProductExp _ _ _ _ exps) = Elt "ProductExp" [] $ toXMLExps exps
toXMLAlt (Alt _ _ _ ident exp) = Elt "Alt" [] $ [toXMLIdent ident] ++ [toXMLExp exp] ++ []
toXMLBoard (Board _ r1 r2 r3 r4 r5 r6 r7 r8) = Elt "Board" [] $ [toXMLBoardRow r1] ++ [toXMLBoardRow r2] ++ [toXMLBoardRow r3] ++ [toXMLBoardRow r4] ++ [toXMLBoardRow r5] ++ [toXMLBoardRow r6] ++ [toXMLBoardRow r7] ++ [toXMLBoardRow r8] ++ []
toXMLBoardRow (BoardRow _ ca cb cc cd ce cf cg ch) = Elt "BoardRow" [] $ [toXMLBoardSquare ca] ++ [toXMLBoardSquare cb] ++ [toXMLBoardSquare cc] ++ [toXMLBoardSquare cd] ++ [toXMLBoardSquare ce] ++ [toXMLBoardSquare cf] ++ [toXMLBoardSquare cg] ++ [toXMLBoardSquare ch] ++ []
toXMLBoardSquare (Queen _ color) = Elt "Queen" [] $ [toXMLBool color] ++ []
toXMLBoardSquare (King _ color) = Elt "King" [] $ [toXMLBool color] ++ []
toXMLBoardSquare (Bishop _ color) = Elt "Bishop" [] $ [toXMLBool color] ++ []
toXMLBoardSquare (Knight _ color) = Elt "Knight" [] $ [toXMLBool color] ++ []
toXMLBoardSquare (Rook _ color) = Elt "Rook" [] $ [toXMLBool color] ++ []
toXMLBoardSquare (Pawn _ color) = Elt "Pawn" [] $ [toXMLBool color] ++ []
toXMLBoardSquare (Empty) = Elt "Empty" [] $ []
toXMLPPPresentation (PPPresentation _ viewType slides) = Elt "PPPresentation" [] $ toXMLSlides slides
toXMLSlide (Slide _ title itemList) = Elt "Slide" [] $ [toXMLString_ title] ++ [toXMLItemList itemList] ++ []
toXMLItemList (ItemList _ listType items) = Elt "ItemList" [] $ toXMLItems items
toXMLListType (Bullet _) = Elt "Bullet" [] $ []
toXMLListType (Number _) = Elt "Number" [] $ []
toXMLListType (Alpha _) = Elt "Alpha" [] $ []
toXMLItem (StringItem _ string) = Elt "StringItem" [] $ [toXMLString_ string] ++ []
toXMLItem (HeliumItem _ exp) = Elt "HeliumItem" [] $ [toXMLExp exp] ++ []
toXMLItem (ListItem _ itemList) = Elt "ListItem" [] $ [toXMLItemList itemList] ++ []
toXMLString_ (String_ _ string) = Elt "String_" [] $ [toXMLString string] ++ []
toXMLList_Decl (List_Decl _ decls) = toXMLConsList_Decl decls
toXMLConsList_Decl (Cons_Decl decl decls) = toXMLDecl decl : toXMLDecls decls
toXMLConsList_Decl Nil_Decl             = []
toXMLDecls _                           = []
toXMLList_Alt (List_Alt _ alts) = toXMLConsList_Alt alts
toXMLConsList_Alt (Cons_Alt alt alts) = toXMLAlt alt : toXMLAlts alts
toXMLConsList_Alt Nil_Alt             = []
toXMLAlts _                           = []
toXMLList_Exp (List_Exp _ exps) = toXMLConsList_Exp exps
toXMLConsList_Exp (Cons_Exp exp exps) = toXMLExp exp : toXMLExps exps
toXMLConsList_Exp Nil_Exp             = []
toXMLExps _                           = []
toXMLList_Slide (List_Slide _ slides) = toXMLConsList_Slide slides
toXMLConsList_Slide (Cons_Slide slide slides) = toXMLSlide slide : toXMLSlides slides
toXMLConsList_Slide Nil_Slide             = []
toXMLSlides _                           = []
toXMLList_Item (List_Item _ items) = toXMLConsList_Item items
toXMLConsList_Item (Cons_Item item items) = toXMLItem item : toXMLItems items
toXMLConsList_Item Nil_Item             = []
toXMLItems _                           = []
