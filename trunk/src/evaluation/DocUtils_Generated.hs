module DocUtils_Generated where

import DocTypes
import PresTypes

--instance Show Node where
--  show NoNode = "<>"
--  show (DocNode doc pth) = "<"++shallowShowDoc1 doc++","++show pth++">"
--  show (ExpNode exp pth) = "<"++shallowShowExp1 exp++","++show pth++">"
--  show _ = "<NODE>"

instance Eq Node where
  nd1 == nd2 = rankNode nd1 == rankNode nd2
  
instance Ord Node where
  nd1 <= nd2 = rankNode nd1 <= rankNode nd2


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
rankNode :: Node -> Int
rankNode NoNode            = 0
rankNode (RootDocNode _ _) = 1
rankNode (HoleDocumentNode _ _) = 2
rankNode (RootEnrNode _ _)  = 3
rankNode (HoleEnrichedDocNode _ _)  = 4
rankNode (DeclNode _ _)  = 5
rankNode (BoardDeclNode _ _)  = 6
rankNode (PPPresentationDeclNode _ _)  = 7
rankNode (HoleDeclNode _ _)  = 8
rankNode (IdentNode _ _)  = 9
rankNode (HoleIdentNode _ _)  = 10
rankNode (PlusExpNode _ _)  = 11
rankNode (TimesExpNode _ _)  = 12
rankNode (DivExpNode _ _)  = 13
rankNode (PowerExpNode _ _)  = 14
rankNode (BoolExpNode _ _)  = 15
rankNode (IntExpNode _ _)  = 16
rankNode (LamExpNode _ _)  = 17
rankNode (AppExpNode _ _)  = 18
rankNode (CaseExpNode _ _)  = 19
rankNode (LetExpNode _ _)  = 20
rankNode (IdentExpNode _ _)  = 21
rankNode (IfExpNode _ _)  = 22
rankNode (ParenExpNode _ _)  = 23
rankNode (ListExpNode _ _)  = 24
rankNode (ProductExpNode _ _)  = 25
rankNode (HoleExpNode _ _)  = 26
rankNode (AltNode _ _)  = 27
rankNode (HoleAltNode _ _)  = 28
rankNode (BoardNode _ _)  = 29
rankNode (HoleBoardNode _ _)  = 30
rankNode (BoardRowNode _ _)  = 31
rankNode (HoleBoardRowNode _ _)  = 32
rankNode (QueenNode _ _)  = 33
rankNode (KingNode _ _)  = 34
rankNode (BishopNode _ _)  = 35
rankNode (KnightNode _ _)  = 36
rankNode (RookNode _ _)  = 37
rankNode (PawnNode _ _)  = 38
rankNode (EmptyNode _ _)  = 39
rankNode (HoleBoardSquareNode _ _)  = 40
rankNode (PPPresentationNode _ _)  = 41
rankNode (HolePPPresentationNode _ _)  = 42
rankNode (SlideNode _ _)  = 43
rankNode (HoleSlideNode _ _)  = 44
rankNode (ItemListNode _ _)  = 45
rankNode (HoleItemListNode _ _)  = 46
rankNode (BulletNode _ _)  = 47
rankNode (NumberNode _ _)  = 48
rankNode (AlphaNode _ _)  = 49
rankNode (HoleListTypeNode _ _)  = 50
rankNode (StringItemNode _ _)  = 51
rankNode (HeliumItemNode _ _)  = 52
rankNode (ListItemNode _ _)  = 53
rankNode (HoleItemNode _ _)  = 54
rankNode (String_Node _ _)  = 55
rankNode (HoleString_Node _ _)  = 56
rankNode (List_DeclNode _ _)  = 57
rankNode (HoleList_DeclNode _ _)  = 58
rankNode (List_AltNode _ _)  = 59
rankNode (HoleList_AltNode _ _)  = 60
rankNode (List_ExpNode _ _)  = 61
rankNode (HoleList_ExpNode _ _)  = 62
rankNode (List_SlideNode _ _)  = 63
rankNode (HoleList_SlideNode _ _)  = 64
rankNode (List_ItemNode _ _)  = 65
rankNode (HoleList_ItemNode _ _)  = 66



pathNode :: Node -> PathDoc
pathNode NoNode            = NoPathD
pathNode (RootDocNode _ pth) = PathD pth
pathNode (HoleDocumentNode _ pth) = PathD pth
pathNode (RootEnrNode _ pth)  = PathD pth
pathNode (HoleEnrichedDocNode _ pth)  = PathD pth
pathNode (DeclNode _ pth)  = PathD pth
pathNode (BoardDeclNode _ pth)  = PathD pth
pathNode (PPPresentationDeclNode _ pth)  = PathD pth
pathNode (HoleDeclNode _ pth)  = PathD pth
pathNode (IdentNode _ pth)  = PathD pth
pathNode (HoleIdentNode _ pth)  = PathD pth
pathNode (PlusExpNode _ pth)  = PathD pth
pathNode (TimesExpNode _ pth)  = PathD pth
pathNode (DivExpNode _ pth)  = PathD pth
pathNode (PowerExpNode _ pth)  = PathD pth
pathNode (BoolExpNode _ pth)  = PathD pth
pathNode (IntExpNode _ pth)  = PathD pth
pathNode (LamExpNode _ pth)  = PathD pth
pathNode (AppExpNode _ pth)  = PathD pth
pathNode (CaseExpNode _ pth)  = PathD pth
pathNode (LetExpNode _ pth)  = PathD pth
pathNode (IdentExpNode _ pth)  = PathD pth
pathNode (IfExpNode _ pth)  = PathD pth
pathNode (ParenExpNode _ pth)  = PathD pth
pathNode (ListExpNode _ pth)  = PathD pth
pathNode (ProductExpNode _ pth)  = PathD pth
pathNode (HoleExpNode _ pth)  = PathD pth
pathNode (AltNode _ pth)  = PathD pth
pathNode (HoleAltNode _ pth)  = PathD pth
pathNode (BoardNode _ pth)  = PathD pth
pathNode (HoleBoardNode _ pth)  = PathD pth
pathNode (BoardRowNode _ pth)  = PathD pth
pathNode (HoleBoardRowNode _ pth)  = PathD pth
pathNode (QueenNode _ pth)  = PathD pth
pathNode (KingNode _ pth)  = PathD pth
pathNode (BishopNode _ pth)  = PathD pth
pathNode (KnightNode _ pth)  = PathD pth
pathNode (RookNode _ pth)  = PathD pth
pathNode (PawnNode _ pth)  = PathD pth
pathNode (EmptyNode _ pth)  = PathD pth
pathNode (HoleBoardSquareNode _ pth)  = PathD pth
pathNode (PPPresentationNode _ pth)  = PathD pth
pathNode (HolePPPresentationNode _ pth)  = PathD pth
pathNode (SlideNode _ pth)  = PathD pth
pathNode (HoleSlideNode _ pth)  = PathD pth
pathNode (ItemListNode _ pth)  = PathD pth
pathNode (HoleItemListNode _ pth)  = PathD pth
pathNode (BulletNode _ pth)  = PathD pth
pathNode (NumberNode _ pth)  = PathD pth
pathNode (AlphaNode _ pth)  = PathD pth
pathNode (HoleListTypeNode _ pth)  = PathD pth
pathNode (StringItemNode _ pth)  = PathD pth
pathNode (HeliumItemNode _ pth)  = PathD pth
pathNode (ListItemNode _ pth)  = PathD pth
pathNode (HoleItemNode _ pth)  = PathD pth
pathNode (String_Node _ pth)  = PathD pth
pathNode (HoleString_Node _ pth)  = PathD pth
pathNode (List_DeclNode _ pth)  = PathD pth
pathNode (HoleList_DeclNode _ pth)  = PathD pth
pathNode (List_AltNode _ pth)  = PathD pth
pathNode (HoleList_AltNode _ pth)  = PathD pth
pathNode (List_ExpNode _ pth)  = PathD pth
pathNode (HoleList_ExpNode _ pth)  = PathD pth
pathNode (List_SlideNode _ pth)  = PathD pth
pathNode (HoleList_SlideNode _ pth)  = PathD pth
pathNode (List_ItemNode _ pth)  = PathD pth
pathNode (HoleList_ItemNode _ pth)  = PathD pth



rootEnrIDD :: Node -> Maybe IDD
rootEnrIDD (RootEnrNode (RootEnr iDP _ _ _ _ _) _) = Just iDP
rootEnrIDD _                                   = Nothing

declIDD :: Node -> Maybe IDD
declIDD (DeclNode (Decl iDP _ _ _ _ _ _ _ _) _) = Just iDP
declIDD _                                   = Nothing

boardDeclIDD :: Node -> Maybe IDD
boardDeclIDD (BoardDeclNode (BoardDecl iDP _ _ _) _) = Just iDP
boardDeclIDD _                                   = Nothing

pPPresentationDeclIDD :: Node -> Maybe IDD
pPPresentationDeclIDD (PPPresentationDeclNode (PPPresentationDecl iDP _ _ _) _) = Just iDP
pPPresentationDeclIDD _                                   = Nothing

identIDD :: Node -> Maybe IDD
identIDD (IdentNode (Ident iDP _ _ _) _) = Just iDP
identIDD _                                   = Nothing

plusExpIDD :: Node -> Maybe IDD
plusExpIDD (PlusExpNode (PlusExp iDP _ _ _) _) = Just iDP
plusExpIDD _                                   = Nothing

timesExpIDD :: Node -> Maybe IDD
timesExpIDD (TimesExpNode (TimesExp iDP _ _ _) _) = Just iDP
timesExpIDD _                                   = Nothing

divExpIDD :: Node -> Maybe IDD
divExpIDD (DivExpNode (DivExp iDP _ _ _) _) = Just iDP
divExpIDD _                                   = Nothing

powerExpIDD :: Node -> Maybe IDD
powerExpIDD (PowerExpNode (PowerExp iDP _ _ _) _) = Just iDP
powerExpIDD _                                   = Nothing

boolExpIDD :: Node -> Maybe IDD
boolExpIDD (BoolExpNode (BoolExp iDP _ _) _) = Just iDP
boolExpIDD _                                   = Nothing

intExpIDD :: Node -> Maybe IDD
intExpIDD (IntExpNode (IntExp iDP _ _) _) = Just iDP
intExpIDD _                                   = Nothing

lamExpIDD :: Node -> Maybe IDD
lamExpIDD (LamExpNode (LamExp iDP _ _ _ _) _) = Just iDP
lamExpIDD _                                   = Nothing

appExpIDD :: Node -> Maybe IDD
appExpIDD (AppExpNode (AppExp iDP _ _) _) = Just iDP
appExpIDD _                                   = Nothing

caseExpIDD :: Node -> Maybe IDD
caseExpIDD (CaseExpNode (CaseExp iDP _ _ _ _) _) = Just iDP
caseExpIDD _                                   = Nothing

letExpIDD :: Node -> Maybe IDD
letExpIDD (LetExpNode (LetExp iDP _ _ _ _) _) = Just iDP
letExpIDD _                                   = Nothing

identExpIDD :: Node -> Maybe IDD
identExpIDD (IdentExpNode (IdentExp iDP _) _) = Just iDP
identExpIDD _                                   = Nothing

ifExpIDD :: Node -> Maybe IDD
ifExpIDD (IfExpNode (IfExp iDP _ _ _ _ _ _) _) = Just iDP
ifExpIDD _                                   = Nothing

parenExpIDD :: Node -> Maybe IDD
parenExpIDD (ParenExpNode (ParenExp iDP _ _ _) _) = Just iDP
parenExpIDD _                                   = Nothing

listExpIDD :: Node -> Maybe IDD
listExpIDD (ListExpNode (ListExp iDP _ _ _ _) _) = Just iDP
listExpIDD _                                   = Nothing

productExpIDD :: Node -> Maybe IDD
productExpIDD (ProductExpNode (ProductExp iDP _ _ _ _) _) = Just iDP
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
queenIDD (QueenNode (Queen iDP _) _) = Just iDP
queenIDD _                                   = Nothing

kingIDD :: Node -> Maybe IDD
kingIDD (KingNode (King iDP _) _) = Just iDP
kingIDD _                                   = Nothing

bishopIDD :: Node -> Maybe IDD
bishopIDD (BishopNode (Bishop iDP _) _) = Just iDP
bishopIDD _                                   = Nothing

knightIDD :: Node -> Maybe IDD
knightIDD (KnightNode (Knight iDP _) _) = Just iDP
knightIDD _                                   = Nothing

rookIDD :: Node -> Maybe IDD
rookIDD (RookNode (Rook iDP _) _) = Just iDP
rookIDD _                                   = Nothing

pawnIDD :: Node -> Maybe IDD
pawnIDD (PawnNode (Pawn iDP _) _) = Just iDP
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
bulletIDD (BulletNode (Bullet iDP) _) = Just iDP
bulletIDD _                                   = Nothing

numberIDD :: Node -> Maybe IDD
numberIDD (NumberNode (Number iDP) _) = Just iDP
numberIDD _                                   = Nothing

alphaIDD :: Node -> Maybe IDD
alphaIDD (AlphaNode (Alpha iDP) _) = Just iDP
alphaIDD _                                   = Nothing

stringItemIDD :: Node -> Maybe IDD
stringItemIDD (StringItemNode (StringItem iDP _) _) = Just iDP
stringItemIDD _                                   = Nothing

heliumItemIDD :: Node -> Maybe IDD
heliumItemIDD (HeliumItemNode (HeliumItem iDP _) _) = Just iDP
heliumItemIDD _                                   = Nothing

listItemIDD :: Node -> Maybe IDD
listItemIDD (ListItemNode (ListItem iDP _) _) = Just iDP
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
