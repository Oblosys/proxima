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
rankNode (InvDeclNode _ _)  = 8
rankNode (HoleDeclNode _ _)  = 9
rankNode (IdentNode _ _)  = 10
rankNode (HoleIdentNode _ _)  = 11
rankNode (PlusExpNode _ _)  = 12
rankNode (TimesExpNode _ _)  = 13
rankNode (DivExpNode _ _)  = 14
rankNode (PowerExpNode _ _)  = 15
rankNode (BoolExpNode _ _)  = 16
rankNode (IntExpNode _ _)  = 17
rankNode (LamExpNode _ _)  = 18
rankNode (AppExpNode _ _)  = 19
rankNode (CaseExpNode _ _)  = 20
rankNode (LetExpNode _ _)  = 21
rankNode (IdentExpNode _ _)  = 22
rankNode (IfExpNode _ _)  = 23
rankNode (ParenExpNode _ _)  = 24
rankNode (ListExpNode _ _)  = 25
rankNode (ProductExpNode _ _)  = 26
rankNode (HoleExpNode _ _)  = 27
rankNode (AltNode _ _)  = 28
rankNode (HoleAltNode _ _)  = 29
rankNode (BoardNode _ _)  = 30
rankNode (HoleBoardNode _ _)  = 31
rankNode (BoardRowNode _ _)  = 32
rankNode (HoleBoardRowNode _ _)  = 33
rankNode (QueenNode _ _)  = 34
rankNode (KingNode _ _)  = 35
rankNode (BishopNode _ _)  = 36
rankNode (KnightNode _ _)  = 37
rankNode (RookNode _ _)  = 38
rankNode (PawnNode _ _)  = 39
rankNode (EmptyNode _ _)  = 40
rankNode (HoleBoardSquareNode _ _)  = 41
rankNode (PPPresentationNode _ _)  = 42
rankNode (HolePPPresentationNode _ _)  = 43
rankNode (SlideNode _ _)  = 44
rankNode (HoleSlideNode _ _)  = 45
rankNode (ItemListNode _ _)  = 46
rankNode (HoleItemListNode _ _)  = 47
rankNode (BulletNode _ _)  = 48
rankNode (NumberNode _ _)  = 49
rankNode (AlphaNode _ _)  = 50
rankNode (HoleListTypeNode _ _)  = 51
rankNode (StringItemNode _ _)  = 52
rankNode (HeliumItemNode _ _)  = 53
rankNode (ListItemNode _ _)  = 54
rankNode (HoleItemNode _ _)  = 55
rankNode (InvNode _ _)  = 56
rankNode (HoleInvNode _ _)  = 57
rankNode (ReEvaluate1Node _ _)  = 58
rankNode (ReEvaluate2Node _ _)  = 59
rankNode (SkipNode _ _)  = 60
rankNode (HoleEvalButtonNode _ _)  = 61
rankNode (LeftDocViewNode _ _)  = 62
rankNode (RightDocViewNode _ _)  = 63
rankNode (HoleEitherDocViewNode _ _)  = 64
rankNode (ANilNode _ _)  = 65
rankNode (ANNode _ _)  = 66
rankNode (ASNode _ _)  = 67
rankNode (PrNode _ _)  = 68
rankNode (LsNode _ _)  = 69
rankNode (TrNode _ _)  = 70
rankNode (LNode _ _)  = 71
rankNode (RNode _ _)  = 72
rankNode (MarkNode _ _)  = 73
rankNode (DelLNode _ _)  = 74
rankNode (InsLNode _ _)  = 75
rankNode (SndPNode _ _)  = 76
rankNode (FstPNode _ _)  = 77
rankNode (IfNilNode _ _)  = 78
rankNode (UndefNode _ _)  = 79
rankNode (UnitNode _ _)  = 80
rankNode (HoleViewNode _ _)  = 81
rankNode (String_Node _ _)  = 82
rankNode (HoleString_Node _ _)  = 83
rankNode (Bool_Node _ _)  = 84
rankNode (HoleBool_Node _ _)  = 85
rankNode (Int_Node _ _)  = 86
rankNode (HoleInt_Node _ _)  = 87
rankNode (List_DeclNode _ _)  = 88
rankNode (HoleList_DeclNode _ _)  = 89
rankNode (List_AltNode _ _)  = 90
rankNode (HoleList_AltNode _ _)  = 91
rankNode (List_ExpNode _ _)  = 92
rankNode (HoleList_ExpNode _ _)  = 93
rankNode (List_SlideNode _ _)  = 94
rankNode (HoleList_SlideNode _ _)  = 95
rankNode (List_ItemNode _ _)  = 96
rankNode (HoleList_ItemNode _ _)  = 97



pathNode :: Node -> PathDoc
pathNode NoNode            = NoPathD
pathNode (RootDocNode _ pth) = PathD pth
pathNode (HoleDocumentNode _ pth) = PathD pth
pathNode (RootEnrNode _ pth)  = PathD pth
pathNode (HoleEnrichedDocNode _ pth)  = PathD pth
pathNode (DeclNode _ pth)  = PathD pth
pathNode (BoardDeclNode _ pth)  = PathD pth
pathNode (PPPresentationDeclNode _ pth)  = PathD pth
pathNode (InvDeclNode _ pth)  = PathD pth
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
pathNode (InvNode _ pth)  = PathD pth
pathNode (HoleInvNode _ pth)  = PathD pth
pathNode (ReEvaluate1Node _ pth)  = PathD pth
pathNode (ReEvaluate2Node _ pth)  = PathD pth
pathNode (SkipNode _ pth)  = PathD pth
pathNode (HoleEvalButtonNode _ pth)  = PathD pth
pathNode (LeftDocViewNode _ pth)  = PathD pth
pathNode (RightDocViewNode _ pth)  = PathD pth
pathNode (HoleEitherDocViewNode _ pth)  = PathD pth
pathNode (ANilNode _ pth)  = PathD pth
pathNode (ANNode _ pth)  = PathD pth
pathNode (ASNode _ pth)  = PathD pth
pathNode (PrNode _ pth)  = PathD pth
pathNode (LsNode _ pth)  = PathD pth
pathNode (TrNode _ pth)  = PathD pth
pathNode (LNode _ pth)  = PathD pth
pathNode (RNode _ pth)  = PathD pth
pathNode (MarkNode _ pth)  = PathD pth
pathNode (DelLNode _ pth)  = PathD pth
pathNode (InsLNode _ pth)  = PathD pth
pathNode (SndPNode _ pth)  = PathD pth
pathNode (FstPNode _ pth)  = PathD pth
pathNode (IfNilNode _ pth)  = PathD pth
pathNode (UndefNode _ pth)  = PathD pth
pathNode (UnitNode _ pth)  = PathD pth
pathNode (HoleViewNode _ pth)  = PathD pth
pathNode (String_Node _ pth)  = PathD pth
pathNode (HoleString_Node _ pth)  = PathD pth
pathNode (Bool_Node _ pth)  = PathD pth
pathNode (HoleBool_Node _ pth)  = PathD pth
pathNode (Int_Node _ pth)  = PathD pth
pathNode (HoleInt_Node _ pth)  = PathD pth
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

invDeclIDD :: Node -> Maybe IDD
invDeclIDD (InvDeclNode (InvDecl iDP _ _ _) _) = Just iDP
invDeclIDD _                                   = Nothing

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

invIDD :: Node -> Maybe IDD
invIDD (InvNode (Inv iDP _ _ _ _) _) = Just iDP
invIDD _                                   = Nothing

reEvaluate1IDD :: Node -> Maybe IDD
reEvaluate1IDD (ReEvaluate1Node (ReEvaluate1 iDP) _) = Just iDP
reEvaluate1IDD _                                   = Nothing

reEvaluate2IDD :: Node -> Maybe IDD
reEvaluate2IDD (ReEvaluate2Node (ReEvaluate2 iDP) _) = Just iDP
reEvaluate2IDD _                                   = Nothing

skipIDD :: Node -> Maybe IDD
skipIDD (SkipNode (Skip iDP) _) = Just iDP
skipIDD _                                   = Nothing

leftDocViewIDD :: Node -> Maybe IDD
leftDocViewIDD (LeftDocViewNode (LeftDocView iDP _) _) = Just iDP
leftDocViewIDD _                                   = Nothing

rightDocViewIDD :: Node -> Maybe IDD
rightDocViewIDD (RightDocViewNode (RightDocView iDP _) _) = Just iDP
rightDocViewIDD _                                   = Nothing

aNilIDD :: Node -> Maybe IDD
aNilIDD (ANilNode (ANil iDP) _) = Just iDP
aNilIDD _                                   = Nothing

aNIDD :: Node -> Maybe IDD
aNIDD (ANNode (AN iDP _) _) = Just iDP
aNIDD _                                   = Nothing

aSIDD :: Node -> Maybe IDD
aSIDD (ASNode (AS iDP _) _) = Just iDP
aSIDD _                                   = Nothing

prIDD :: Node -> Maybe IDD
prIDD (PrNode (Pr iDP _ _) _) = Just iDP
prIDD _                                   = Nothing

lsIDD :: Node -> Maybe IDD
lsIDD (LsNode (Ls iDP _ _) _) = Just iDP
lsIDD _                                   = Nothing

trIDD :: Node -> Maybe IDD
trIDD (TrNode (Tr iDP _ _) _) = Just iDP
trIDD _                                   = Nothing

lIDD :: Node -> Maybe IDD
lIDD (LNode (L iDP _) _) = Just iDP
lIDD _                                   = Nothing

rIDD :: Node -> Maybe IDD
rIDD (RNode (R iDP _) _) = Just iDP
rIDD _                                   = Nothing

markIDD :: Node -> Maybe IDD
markIDD (MarkNode (Mark iDP _) _) = Just iDP
markIDD _                                   = Nothing

delLIDD :: Node -> Maybe IDD
delLIDD (DelLNode (DelL iDP _ _) _) = Just iDP
delLIDD _                                   = Nothing

insLIDD :: Node -> Maybe IDD
insLIDD (InsLNode (InsL iDP _ _) _) = Just iDP
insLIDD _                                   = Nothing

sndPIDD :: Node -> Maybe IDD
sndPIDD (SndPNode (SndP iDP _ _ _) _) = Just iDP
sndPIDD _                                   = Nothing

fstPIDD :: Node -> Maybe IDD
fstPIDD (FstPNode (FstP iDP _ _ _) _) = Just iDP
fstPIDD _                                   = Nothing

ifNilIDD :: Node -> Maybe IDD
ifNilIDD (IfNilNode (IfNil iDP _ _) _) = Just iDP
ifNilIDD _                                   = Nothing

undefIDD :: Node -> Maybe IDD
undefIDD (UndefNode (Undef iDP) _) = Just iDP
undefIDD _                                   = Nothing

unitIDD :: Node -> Maybe IDD
unitIDD (UnitNode (Unit iDP) _) = Just iDP
unitIDD _                                   = Nothing

string_IDD :: Node -> Maybe IDD
string_IDD (String_Node (String_ iDP _) _) = Just iDP
string_IDD _                                   = Nothing

bool_IDD :: Node -> Maybe IDD
bool_IDD (Bool_Node (Bool_ iDP _) _) = Just iDP
bool_IDD _                                   = Nothing

int_IDD :: Node -> Maybe IDD
int_IDD (Int_Node (Int_ iDP _) _) = Just iDP
int_IDD _                                   = Nothing




shallowShowEnrichedDoc1 (RootEnr  _ _ _ _ _ _) = "RootEnr"
shallowShowDecl1 (Decl  _ _ _ _ _ _ _ _ _) = "Decl"
shallowShowDecl1 (BoardDecl  _ _ _ _) = "BoardDecl"
shallowShowDecl1 (PPPresentationDecl  _ _ _ _) = "PPPresentationDecl"
shallowShowDecl1 (InvDecl  _ _ _ _) = "InvDecl"
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
shallowShowInv1 (Inv  _ _ _ _ _) = "Inv"
shallowShowEvalButton1 (ReEvaluate1  _) = "ReEvaluate1"
shallowShowEvalButton1 (ReEvaluate2  _) = "ReEvaluate2"
shallowShowEvalButton1 (Skip  _) = "Skip"
shallowShowEitherDocView1 (LeftDocView  _ _) = "LeftDocView"
shallowShowEitherDocView1 (RightDocView  _ _) = "RightDocView"
shallowShowView1 (ANil  _) = "ANil"
shallowShowView1 (AN  _ _) = "AN"
shallowShowView1 (AS  _ _) = "AS"
shallowShowView1 (Pr  _ _ _) = "Pr"
shallowShowView1 (Ls  _ _ _) = "Ls"
shallowShowView1 (Tr  _ _ _) = "Tr"
shallowShowView1 (L  _ _) = "L"
shallowShowView1 (R  _ _) = "R"
shallowShowView1 (Mark  _ _) = "Mark"
shallowShowView1 (DelL  _ _ _) = "DelL"
shallowShowView1 (InsL  _ _ _) = "InsL"
shallowShowView1 (SndP  _ _ _ _) = "SndP"
shallowShowView1 (FstP  _ _ _ _) = "FstP"
shallowShowView1 (IfNil  _ _ _) = "IfNil"
shallowShowView1 (Undef  _) = "Undef"
shallowShowView1 (Unit  _) = "Unit"
shallowShowString_1 (String_  _ _) = "String_"
shallowShowBool_1 (Bool_  _ _) = "Bool_"
shallowShowInt_1 (Int_  _ _) = "Int_"
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
toXMLDecl (Decl _ _ _ _ _ expanded autoLayout ident exp) = Elt "Decl" [] $ [toXMLBool_ expanded] ++ [toXMLBool_ autoLayout] ++ [toXMLIdent ident] ++ [toXMLExp exp] ++ []
toXMLDecl (BoardDecl _ _ _ board) = Elt "BoardDecl" [] $ [toXMLBoard board] ++ []
toXMLDecl (PPPresentationDecl _ _ _ pPPresentation) = Elt "PPPresentationDecl" [] $ [toXMLPPPresentation pPPresentation] ++ []
toXMLDecl (InvDecl _ _ _ inv) = Elt "InvDecl" [] $ [toXMLInv inv] ++ []
toXMLIdent (Ident _ _ _ string_) = Elt "Ident" [] $ [toXMLString_ string_] ++ []
toXMLExp (PlusExp _ _ exp1 exp2) = Elt "PlusExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ []
toXMLExp (TimesExp _ _ exp1 exp2) = Elt "TimesExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ []
toXMLExp (DivExp _ _ exp1 exp2) = Elt "DivExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ []
toXMLExp (PowerExp _ _ exp1 exp2) = Elt "PowerExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ []
toXMLExp (BoolExp _ _ bool_) = Elt "BoolExp" [] $ [toXMLBool_ bool_] ++ []
toXMLExp (IntExp _ _ int_) = Elt "IntExp" [] $ [toXMLInt_ int_] ++ []
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
toXMLBoardSquare (Queen _ color) = Elt "Queen" [] $ [toXMLBool_ color] ++ []
toXMLBoardSquare (King _ color) = Elt "King" [] $ [toXMLBool_ color] ++ []
toXMLBoardSquare (Bishop _ color) = Elt "Bishop" [] $ [toXMLBool_ color] ++ []
toXMLBoardSquare (Knight _ color) = Elt "Knight" [] $ [toXMLBool_ color] ++ []
toXMLBoardSquare (Rook _ color) = Elt "Rook" [] $ [toXMLBool_ color] ++ []
toXMLBoardSquare (Pawn _ color) = Elt "Pawn" [] $ [toXMLBool_ color] ++ []
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
toXMLInv (Inv _ doc enr eval evalButton) = Elt "Inv" [] $ [toXMLEitherDocView doc] ++ [toXMLView enr] ++ [toXMLString_ eval] ++ [toXMLEvalButton evalButton] ++ []
toXMLEvalButton (ReEvaluate1 _) = Elt "ReEvaluate1" [] $ []
toXMLEvalButton (ReEvaluate2 _) = Elt "ReEvaluate2" [] $ []
toXMLEvalButton (Skip _) = Elt "Skip" [] $ []
toXMLEitherDocView (LeftDocView _ error) = Elt "LeftDocView" [] $ [toXMLString_ error] ++ []
toXMLEitherDocView (RightDocView _ doc) = Elt "RightDocView" [] $ [toXMLView doc] ++ []
toXMLView (ANil _) = Elt "ANil" [] $ []
toXMLView (AN _ int_) = Elt "AN" [] $ [toXMLInt_ int_] ++ []
toXMLView (AS _ string_) = Elt "AS" [] $ [toXMLString_ string_] ++ []
toXMLView (Pr _ view1 view2) = Elt "Pr" [] $ [toXMLView view1] ++ [toXMLView view2] ++ []
toXMLView (Ls _ view1 view2) = Elt "Ls" [] $ [toXMLView view1] ++ [toXMLView view2] ++ []
toXMLView (Tr _ view1 view2) = Elt "Tr" [] $ [toXMLView view1] ++ [toXMLView view2] ++ []
toXMLView (L _ view) = Elt "L" [] $ [toXMLView view] ++ []
toXMLView (R _ view) = Elt "R" [] $ [toXMLView view] ++ []
toXMLView (Mark _ view) = Elt "Mark" [] $ [toXMLView view] ++ []
toXMLView (DelL _ view1 view2) = Elt "DelL" [] $ [toXMLView view1] ++ [toXMLView view2] ++ []
toXMLView (InsL _ view1 view2) = Elt "InsL" [] $ [toXMLView view1] ++ [toXMLView view2] ++ []
toXMLView (SndP _ bool_ view1 view2) = Elt "SndP" [] $ [toXMLBool_ bool_] ++ [toXMLView view1] ++ [toXMLView view2] ++ []
toXMLView (FstP _ bool_ view1 view2) = Elt "FstP" [] $ [toXMLBool_ bool_] ++ [toXMLView view1] ++ [toXMLView view2] ++ []
toXMLView (IfNil _ bool_ view) = Elt "IfNil" [] $ [toXMLBool_ bool_] ++ [toXMLView view] ++ []
toXMLView (Undef _) = Elt "Undef" [] $ []
toXMLView (Unit _) = Elt "Unit" [] $ []
toXMLString_ (String_ _ string) = Elt "String_" [] $ [toXMLString string] ++ []
toXMLBool_ (Bool_ _ bool) = Elt "Bool_" [] $ [toXMLBool bool] ++ []
toXMLInt_ (Int_ _ int) = Elt "Int_" [] $ [toXMLInt int] ++ []
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
