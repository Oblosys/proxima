module DocUtils_Generated where

import DocTypes
import DocTypes_Generated
import DocumentEdit
import DocumentEdit_Generated
import PresTypes
import Text.ParserCombinators.Parsec
import DocUtils
import CommonTypes
import CommonUtils
import System.Directory

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
  initialDoc = initDoc
  toXML = toXMLDocument
  parseXML = parseXML_Document

initDoc :: IO Document
initDoc = 
 do { let filePath = "Heliumfile.hs"
    ; dir <- getCurrentDirectory
    ; debugLnIO Prs $ "InitDoc: opening file: "++"Proxima.hs"++ " at " ++dir  
    ; fileContents <- readFile filePath
    ; return $ RootDoc NoIDD $ Root NoIDD NoIDP $ ParseErrList_Decl (ColP NoIDP 0 NF . map (StringP NoIDP). lines' $ fileContents) {- [] -}
    }
    -- by putting the text in a parse error node, we don't need to specify a textual parser. Instead,
    -- the proxima parser is used when the presented document is parsed.

-- XML

-- we don't put a "RootDoc" element in the XML, because this type is not visible to the user.
toXMLDocument (RootDoc _ root) = toXMLRoot root
toXMLDocument _                = debug Err "DocUtils_Generated.toXMLDocument: malformed Document" $
                                   Elt "Root" [] [] -- this does not occur

parseXML_Document = RootDoc NoIDD <$> parseXML_Root

toXMLHeliumTypeInfo _ = Elt "HeliumTypeInfo" [] []
parseXML_HeliumTypeInfo = ([],[],[]) <$ emptyTag "HeliumTypeInfo"

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

----


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





initBoard = demoBoard -- Board NoIDD (backRow (Bool_ NoIDD True)) (pawnRow (Bool_ NoIDD True)) emptyRow emptyRow emptyRow emptyRow (pawnRow (Bool_ NoIDD False)) (backRow (Bool_ NoIDD False))

demoBoard = Board NoIDD r8 r7 r6 r5 r4 r3 r2 r1
 where r1 = BoardRow NoIDD e  e  bb e  e  e  br bk
       r2 = BoardRow NoIDD e  e  e  e  bn bp e  bp
       r3 = BoardRow NoIDD bp e  e  e  e  e  e  wp
       r4 = BoardRow NoIDD e  e  br e  bq bp wq wr
       r5 = BoardRow NoIDD e  bp wp e  e  e  e  e
       r6 = BoardRow NoIDD e  e  e  e  wn e  e  e
       r7 = BoardRow NoIDD wp wp wb e  e  wp wp e
       r8 = BoardRow NoIDD e  wk e  wr e  e  e  e
       [e,wp,wr,wn,wb,wq,wk,bp,br,bn,bb,bq,bk] = Empty : pieces (Bool_ NoIDD True) ++ pieces (Bool_ NoIDD False)
       pieces c = [Pawn NoIDD c, Rook NoIDD c, Knight NoIDD c, Bishop NoIDD c, Queen NoIDD c, King NoIDD c]
      
  
-- Kasparov,G - Lautier,J Moscow, 1995 1.Ng4!! Qe6 [1...Rxg5 2.Nxe5 Rxh5 3.Rd8+ Ng8 4.Nxf7#] 2.Rd8 Qg6 3.Qxe7 1-0

emptyBoard = Board NoIDD emptyRow emptyRow emptyRow emptyRow emptyRow emptyRow emptyRow emptyRow

emptyRow = BoardRow NoIDD Empty Empty Empty Empty Empty Empty Empty Empty

pawnRow c = BoardRow NoIDD (Pawn NoIDD c) (Pawn NoIDD c) (Pawn NoIDD c) (Pawn NoIDD c) 
                          (Pawn NoIDD c) (Pawn NoIDD c) (Pawn NoIDD c) (Pawn NoIDD c)
backRow c = BoardRow NoIDD (Rook NoIDD c) (Knight NoIDD c) (Bishop NoIDD c) (Queen NoIDD c) 
                          (King NoIDD c) (Bishop NoIDD c) (Knight NoIDD c) (Rook NoIDD c)




initPPPresentation = 
  PPPresentation NoIDD (Bool_ NoIDD True) $ List_Slide NoIDD $
    mkSlides
      [ Slide NoIDD (mkString_ "slide_1") $
          ItemList NoIDD (Bullet NoIDD) $ List_Item NoIDD $
                         mkItems [ StringItem NoIDD (mkString_ "item_1")
                                 , HeliumItem NoIDD -- simple trick to use parser: Needs an additional parse (F1) though!
                                     --(ident "\\ x -> increaze x")
                                     (ident "\\b -> \\x -> if b then ink x else x")
                                 , StringItem NoIDD (mkString_ "item_2")
                                 , ListItem NoIDD listItem
                                 ]
       , Slide NoIDD (mkString_ "slide_2") $
          ItemList NoIDD (Alpha NoIDD) $  List_Item NoIDD $
                         mkItems [ StringItem NoIDD (mkString_ "item_a")
                                 , StringItem NoIDD (mkString_ "item_b")
                                 , StringItem NoIDD (mkString_ "item_c")
                                 ]
      ]
 where listItem = ItemList NoIDD (Number NoIDD) $  List_Item NoIDD $
                    mkItems [ StringItem NoIDD (mkString_ "nested_item_1")
                            , ListItem NoIDD listItem'
                            , StringItem NoIDD (mkString_ "nested_item_2")
                            , StringItem NoIDD (mkString_ "nested_item_3")
                            ]
       listItem' = ItemList NoIDD (Bullet NoIDD) $  List_Item NoIDD $
                    mkItems [ StringItem NoIDD (mkString_ "nested_nested_item")
                            , StringItem NoIDD (mkString_ "nested_nested_item")
                            , StringItem NoIDD (mkString_ "nested_nested_item")
                            ]
       dv e1 e2 = DivExp NoIDD NoIDP e1 e2 
       lam str body = LamExp NoIDD NoIDP NoIDP (Ident NoIDD NoIDP NoIDP (mkString_ str)) body
       ifxp c t e = IfExp NoIDD NoIDP NoIDP NoIDP c t e 
       int i = IntExp NoIDD NoIDP (Int_ NoIDD i)
       bool b = BoolExp NoIDD NoIDP (Bool_ NoIDD b)
       ident str = IdentExp NoIDD (Ident NoIDD NoIDP NoIDP (mkString_ str))
       

       mkString_ str = String_ NoIDD str

mkSlides []     = Nil_Slide
mkSlides (s:ss) = Cons_Slide s (mkSlides ss)

mkItems []     = Nil_Item 
mkItems (s:ss) = Cons_Item s (mkItems ss)






----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

{- ------------------------------------

 generated part

-------------------------------------- -}
rankNode :: Node -> Int
rankNode NoNode            = 0
rankNode (RootDocNode _ _) = 1
rankNode (HoleDocumentNode _ _) = 2
rankNode (String_Node _ _)  = 3
rankNode (HoleString_Node _ _)  = 4
rankNode (Bool_Node _ _)  = 5
rankNode (HoleBool_Node _ _)  = 6
rankNode (Int_Node _ _)  = 7
rankNode (HoleInt_Node _ _)  = 8
rankNode (DummyNode _ _)  = 9
rankNode (HoleDummyNode _ _)  = 10
rankNode (RootEnrNode _ _)  = 11
rankNode (HoleEnrichedDocNode _ _)  = 12
rankNode (RootNode _ _)  = 13
rankNode (HoleRootNode _ _)  = 14
rankNode (RootENode _ _)  = 15
rankNode (HoleRootENode _ _)  = 16
rankNode (DeclNode _ _)  = 17
rankNode (BoardDeclNode _ _)  = 18
rankNode (PPPresentationDeclNode _ _)  = 19
rankNode (HoleDeclNode _ _)  = 20
rankNode (IdentNode _ _)  = 21
rankNode (HoleIdentNode _ _)  = 22
rankNode (PlusExpNode _ _)  = 23
rankNode (TimesExpNode _ _)  = 24
rankNode (DivExpNode _ _)  = 25
rankNode (PowerExpNode _ _)  = 26
rankNode (BoolExpNode _ _)  = 27
rankNode (IntExpNode _ _)  = 28
rankNode (LamExpNode _ _)  = 29
rankNode (AppExpNode _ _)  = 30
rankNode (CaseExpNode _ _)  = 31
rankNode (LetExpNode _ _)  = 32
rankNode (IdentExpNode _ _)  = 33
rankNode (IfExpNode _ _)  = 34
rankNode (ParenExpNode _ _)  = 35
rankNode (ListExpNode _ _)  = 36
rankNode (ProductExpNode _ _)  = 37
rankNode (HoleExpNode _ _)  = 38
rankNode (AltNode _ _)  = 39
rankNode (HoleAltNode _ _)  = 40
rankNode (BoardNode _ _)  = 41
rankNode (HoleBoardNode _ _)  = 42
rankNode (BoardRowNode _ _)  = 43
rankNode (HoleBoardRowNode _ _)  = 44
rankNode (QueenNode _ _)  = 45
rankNode (KingNode _ _)  = 46
rankNode (BishopNode _ _)  = 47
rankNode (KnightNode _ _)  = 48
rankNode (RookNode _ _)  = 49
rankNode (PawnNode _ _)  = 50
rankNode (EmptyNode _ _)  = 51
rankNode (HoleBoardSquareNode _ _)  = 52
rankNode (PPPresentationNode _ _)  = 53
rankNode (HolePPPresentationNode _ _)  = 54
rankNode (SlideNode _ _)  = 55
rankNode (HoleSlideNode _ _)  = 56
rankNode (ItemListNode _ _)  = 57
rankNode (HoleItemListNode _ _)  = 58
rankNode (BulletNode _ _)  = 59
rankNode (NumberNode _ _)  = 60
rankNode (AlphaNode _ _)  = 61
rankNode (HoleListTypeNode _ _)  = 62
rankNode (StringItemNode _ _)  = 63
rankNode (HeliumItemNode _ _)  = 64
rankNode (ListItemNode _ _)  = 65
rankNode (HoleItemNode _ _)  = 66
rankNode (List_DummyNode _ _)  = 67
rankNode (HoleList_DummyNode _ _)  = 68
rankNode (List_DeclNode _ _)  = 69
rankNode (HoleList_DeclNode _ _)  = 70
rankNode (List_AltNode _ _)  = 71
rankNode (HoleList_AltNode _ _)  = 72
rankNode (List_ExpNode _ _)  = 73
rankNode (HoleList_ExpNode _ _)  = 74
rankNode (List_SlideNode _ _)  = 75
rankNode (HoleList_SlideNode _ _)  = 76
rankNode (List_ItemNode _ _)  = 77
rankNode (HoleList_ItemNode _ _)  = 78



instance HasPath Node where
  pathNode NoNode            = NoPathD
  pathNode (RootDocNode _ pth) = PathD pth
  pathNode (HoleDocumentNode _ pth) = PathD pth
  pathNode (String_Node _ pth)  = PathD pth
  pathNode (HoleString_Node _ pth)  = PathD pth
  pathNode (Bool_Node _ pth)  = PathD pth
  pathNode (HoleBool_Node _ pth)  = PathD pth
  pathNode (Int_Node _ pth)  = PathD pth
  pathNode (HoleInt_Node _ pth)  = PathD pth
  pathNode (DummyNode _ pth)  = PathD pth
  pathNode (HoleDummyNode _ pth)  = PathD pth
  pathNode (RootEnrNode _ pth)  = PathD pth
  pathNode (HoleEnrichedDocNode _ pth)  = PathD pth
  pathNode (RootNode _ pth)  = PathD pth
  pathNode (HoleRootNode _ pth)  = PathD pth
  pathNode (RootENode _ pth)  = PathD pth
  pathNode (HoleRootENode _ pth)  = PathD pth
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
  pathNode (List_DummyNode _ pth)  = PathD pth
  pathNode (HoleList_DummyNode _ pth)  = PathD pth
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
dummyIDD (DummyNode (Dummy iDD _ _ _ _ _) _) = Just iDD
dummyIDD _                                   = Nothing

rootEnrIDD :: Node -> Maybe IDD
rootEnrIDD (RootEnrNode (RootEnr iDD _ _ _) _) = Just iDD
rootEnrIDD _                                   = Nothing

rootIDD :: Node -> Maybe IDD
rootIDD (RootNode (Root iDD _ _) _) = Just iDD
rootIDD _                                   = Nothing

rootEIDD :: Node -> Maybe IDD
rootEIDD (RootENode (RootE iDD _ _ _) _) = Just iDD
rootEIDD _                                   = Nothing

declIDD :: Node -> Maybe IDD
declIDD (DeclNode (Decl iDD _ _ _ _ _ _ _ _) _) = Just iDD
declIDD _                                   = Nothing

boardDeclIDD :: Node -> Maybe IDD
boardDeclIDD (BoardDeclNode (BoardDecl iDD _ _ _) _) = Just iDD
boardDeclIDD _                                   = Nothing

pPPresentationDeclIDD :: Node -> Maybe IDD
pPPresentationDeclIDD (PPPresentationDeclNode (PPPresentationDecl iDD _ _ _) _) = Just iDD
pPPresentationDeclIDD _                                   = Nothing

identIDD :: Node -> Maybe IDD
identIDD (IdentNode (Ident iDD _ _ _) _) = Just iDD
identIDD _                                   = Nothing

plusExpIDD :: Node -> Maybe IDD
plusExpIDD (PlusExpNode (PlusExp iDD _ _ _) _) = Just iDD
plusExpIDD _                                   = Nothing

timesExpIDD :: Node -> Maybe IDD
timesExpIDD (TimesExpNode (TimesExp iDD _ _ _) _) = Just iDD
timesExpIDD _                                   = Nothing

divExpIDD :: Node -> Maybe IDD
divExpIDD (DivExpNode (DivExp iDD _ _ _) _) = Just iDD
divExpIDD _                                   = Nothing

powerExpIDD :: Node -> Maybe IDD
powerExpIDD (PowerExpNode (PowerExp iDD _ _ _) _) = Just iDD
powerExpIDD _                                   = Nothing

boolExpIDD :: Node -> Maybe IDD
boolExpIDD (BoolExpNode (BoolExp iDD _ _) _) = Just iDD
boolExpIDD _                                   = Nothing

intExpIDD :: Node -> Maybe IDD
intExpIDD (IntExpNode (IntExp iDD _ _) _) = Just iDD
intExpIDD _                                   = Nothing

lamExpIDD :: Node -> Maybe IDD
lamExpIDD (LamExpNode (LamExp iDD _ _ _ _) _) = Just iDD
lamExpIDD _                                   = Nothing

appExpIDD :: Node -> Maybe IDD
appExpIDD (AppExpNode (AppExp iDD _ _) _) = Just iDD
appExpIDD _                                   = Nothing

caseExpIDD :: Node -> Maybe IDD
caseExpIDD (CaseExpNode (CaseExp iDD _ _ _ _) _) = Just iDD
caseExpIDD _                                   = Nothing

letExpIDD :: Node -> Maybe IDD
letExpIDD (LetExpNode (LetExp iDD _ _ _ _) _) = Just iDD
letExpIDD _                                   = Nothing

identExpIDD :: Node -> Maybe IDD
identExpIDD (IdentExpNode (IdentExp iDD _) _) = Just iDD
identExpIDD _                                   = Nothing

ifExpIDD :: Node -> Maybe IDD
ifExpIDD (IfExpNode (IfExp iDD _ _ _ _ _ _) _) = Just iDD
ifExpIDD _                                   = Nothing

parenExpIDD :: Node -> Maybe IDD
parenExpIDD (ParenExpNode (ParenExp iDD _ _ _) _) = Just iDD
parenExpIDD _                                   = Nothing

listExpIDD :: Node -> Maybe IDD
listExpIDD (ListExpNode (ListExp iDD _ _ _ _) _) = Just iDD
listExpIDD _                                   = Nothing

productExpIDD :: Node -> Maybe IDD
productExpIDD (ProductExpNode (ProductExp iDD _ _ _ _) _) = Just iDD
productExpIDD _                                   = Nothing

altIDD :: Node -> Maybe IDD
altIDD (AltNode (Alt iDD _ _ _ _) _) = Just iDD
altIDD _                                   = Nothing

boardIDD :: Node -> Maybe IDD
boardIDD (BoardNode (Board iDD _ _ _ _ _ _ _ _) _) = Just iDD
boardIDD _                                   = Nothing

boardRowIDD :: Node -> Maybe IDD
boardRowIDD (BoardRowNode (BoardRow iDD _ _ _ _ _ _ _ _) _) = Just iDD
boardRowIDD _                                   = Nothing

queenIDD :: Node -> Maybe IDD
queenIDD (QueenNode (Queen iDD _) _) = Just iDD
queenIDD _                                   = Nothing

kingIDD :: Node -> Maybe IDD
kingIDD (KingNode (King iDD _) _) = Just iDD
kingIDD _                                   = Nothing

bishopIDD :: Node -> Maybe IDD
bishopIDD (BishopNode (Bishop iDD _) _) = Just iDD
bishopIDD _                                   = Nothing

knightIDD :: Node -> Maybe IDD
knightIDD (KnightNode (Knight iDD _) _) = Just iDD
knightIDD _                                   = Nothing

rookIDD :: Node -> Maybe IDD
rookIDD (RookNode (Rook iDD _) _) = Just iDD
rookIDD _                                   = Nothing

pawnIDD :: Node -> Maybe IDD
pawnIDD (PawnNode (Pawn iDD _) _) = Just iDD
pawnIDD _                                   = Nothing

emptyIDD _                                   = Nothing

pPPresentationIDD :: Node -> Maybe IDD
pPPresentationIDD (PPPresentationNode (PPPresentation iDD _ _) _) = Just iDD
pPPresentationIDD _                                   = Nothing

slideIDD :: Node -> Maybe IDD
slideIDD (SlideNode (Slide iDD _ _) _) = Just iDD
slideIDD _                                   = Nothing

itemListIDD :: Node -> Maybe IDD
itemListIDD (ItemListNode (ItemList iDD _ _) _) = Just iDD
itemListIDD _                                   = Nothing

bulletIDD :: Node -> Maybe IDD
bulletIDD (BulletNode (Bullet iDD) _) = Just iDD
bulletIDD _                                   = Nothing

numberIDD :: Node -> Maybe IDD
numberIDD (NumberNode (Number iDD) _) = Just iDD
numberIDD _                                   = Nothing

alphaIDD :: Node -> Maybe IDD
alphaIDD (AlphaNode (Alpha iDD) _) = Just iDD
alphaIDD _                                   = Nothing

stringItemIDD :: Node -> Maybe IDD
stringItemIDD (StringItemNode (StringItem iDD _) _) = Just iDD
stringItemIDD _                                   = Nothing

heliumItemIDD :: Node -> Maybe IDD
heliumItemIDD (HeliumItemNode (HeliumItem iDD _) _) = Just iDD
heliumItemIDD _                                   = Nothing

listItemIDD :: Node -> Maybe IDD
listItemIDD (ListItemNode (ListItem iDD _) _) = Just iDD
listItemIDD _                                   = Nothing




shallowShowString_1 (String_  _ _) = "String_"
shallowShowBool_1 (Bool_  _ _) = "Bool_"
shallowShowInt_1 (Int_  _ _) = "Int_"
shallowShowDummy1 (Dummy  _ _ _ _ _ _) = "Dummy"
shallowShowEnrichedDoc1 (RootEnr  _ _ _ _) = "RootEnr"
shallowShowRoot1 (Root  _ _ _) = "Root"
shallowShowRootE1 (RootE  _ _ _ _) = "RootE"
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
shallowShowList_Dummy1 (List_Dummy  _ _) = "List_Dummy"
shallowShowConsList_Dummy1 (Cons_Dummy  _ _) = "Cons_Dummy"
shallowShowConsList_Dummy1 (Nil_Dummy ) = "Nil_Dummy"
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



toXMLString_ (String_ _ string) = Elt "String_" [] $ [toXMLString string] ++ []
toXMLString_ HoleString_ = Elt "HoleString_" [] []
toXMLString_ (ParseErrString_ _) = Elt "ParseErrString_" [] []
toXMLBool_ (Bool_ _ bool) = Elt "Bool_" [] $ [toXMLBool bool] ++ []
toXMLBool_ HoleBool_ = Elt "HoleBool_" [] []
toXMLBool_ (ParseErrBool_ _) = Elt "ParseErrBool_" [] []
toXMLInt_ (Int_ _ int) = Elt "Int_" [] $ [toXMLInt int] ++ []
toXMLInt_ HoleInt_ = Elt "HoleInt_" [] []
toXMLInt_ (ParseErrInt_ _) = Elt "ParseErrInt_" [] []
toXMLDummy (Dummy _ root dummys string_ bool_ int_) = Elt "Dummy" [] $ [toXMLRoot root] ++ toXMLList_Dummy dummys ++ [toXMLString_ string_] ++ [toXMLBool_ bool_] ++ [toXMLInt_ int_] ++ []
toXMLDummy HoleDummy = Elt "HoleDummy" [] []
toXMLDummy (ParseErrDummy _) = Elt "ParseErrDummy" [] []
toXMLEnrichedDoc (RootEnr _ root heliumTypeInfo document) = Elt "RootEnr" [] $ [toXMLRootE root] ++ [toXMLHeliumTypeInfo heliumTypeInfo] ++ [toXMLDocument document] ++ []
toXMLEnrichedDoc HoleEnrichedDoc = Elt "HoleEnrichedDoc" [] []
toXMLEnrichedDoc (ParseErrEnrichedDoc _) = Elt "ParseErrEnrichedDoc" [] []
toXMLRoot (Root _ _ decls) = Elt "Root" [] $ toXMLList_Decl decls ++ []
toXMLRoot HoleRoot = Elt "HoleRoot" [] []
toXMLRoot (ParseErrRoot _) = Elt "ParseErrRoot" [] []
toXMLRootE (RootE _ _ decls idListDecls) = Elt "RootE" [] $ toXMLList_Decl decls ++ toXMLList_Decl idListDecls ++ []
toXMLRootE HoleRootE = Elt "HoleRootE" [] []
toXMLRootE (ParseErrRootE _) = Elt "ParseErrRootE" [] []
toXMLDecl (Decl _ _ _ _ _ expanded autoLayout ident exp) = Elt "Decl" [] $ [toXMLBool_ expanded] ++ [toXMLBool_ autoLayout] ++ [toXMLIdent ident] ++ [toXMLExp exp] ++ []
toXMLDecl (BoardDecl _ _ _ board) = Elt "BoardDecl" [] $ [toXMLBoard board] ++ []
toXMLDecl (PPPresentationDecl _ _ _ pPPresentation) = Elt "PPPresentationDecl" [] $ [toXMLPPPresentation pPPresentation] ++ []
toXMLDecl HoleDecl = Elt "HoleDecl" [] []
toXMLDecl (ParseErrDecl _) = Elt "ParseErrDecl" [] []
toXMLIdent (Ident _ _ _ string_) = Elt "Ident" [] $ [toXMLString_ string_] ++ []
toXMLIdent HoleIdent = Elt "HoleIdent" [] []
toXMLIdent (ParseErrIdent _) = Elt "ParseErrIdent" [] []
toXMLExp (PlusExp _ _ exp1 exp2) = Elt "PlusExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ []
toXMLExp (TimesExp _ _ exp1 exp2) = Elt "TimesExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ []
toXMLExp (DivExp _ _ exp1 exp2) = Elt "DivExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ []
toXMLExp (PowerExp _ _ exp1 exp2) = Elt "PowerExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ []
toXMLExp (BoolExp _ _ bool_) = Elt "BoolExp" [] $ [toXMLBool_ bool_] ++ []
toXMLExp (IntExp _ _ int_) = Elt "IntExp" [] $ [toXMLInt_ int_] ++ []
toXMLExp (LamExp _ _ _ ident exp) = Elt "LamExp" [] $ [toXMLIdent ident] ++ [toXMLExp exp] ++ []
toXMLExp (AppExp _ exp1 exp2) = Elt "AppExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ []
toXMLExp (CaseExp _ _ _ exp alts) = Elt "CaseExp" [] $ [toXMLExp exp] ++ toXMLList_Alt alts ++ []
toXMLExp (LetExp _ _ _ decls exp) = Elt "LetExp" [] $ toXMLList_Decl decls ++ [toXMLExp exp] ++ []
toXMLExp (IdentExp _ ident) = Elt "IdentExp" [] $ [toXMLIdent ident] ++ []
toXMLExp (IfExp _ _ _ _ exp1 exp2 exp3) = Elt "IfExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ [toXMLExp exp3] ++ []
toXMLExp (ParenExp _ _ _ exp) = Elt "ParenExp" [] $ [toXMLExp exp] ++ []
toXMLExp (ListExp _ _ _ _ exps) = Elt "ListExp" [] $ toXMLList_Exp exps ++ []
toXMLExp (ProductExp _ _ _ _ exps) = Elt "ProductExp" [] $ toXMLList_Exp exps ++ []
toXMLExp HoleExp = Elt "HoleExp" [] []
toXMLExp (ParseErrExp _) = Elt "ParseErrExp" [] []
toXMLAlt (Alt _ _ _ ident exp) = Elt "Alt" [] $ [toXMLIdent ident] ++ [toXMLExp exp] ++ []
toXMLAlt HoleAlt = Elt "HoleAlt" [] []
toXMLAlt (ParseErrAlt _) = Elt "ParseErrAlt" [] []
toXMLBoard (Board _ r1 r2 r3 r4 r5 r6 r7 r8) = Elt "Board" [] $ [toXMLBoardRow r1] ++ [toXMLBoardRow r2] ++ [toXMLBoardRow r3] ++ [toXMLBoardRow r4] ++ [toXMLBoardRow r5] ++ [toXMLBoardRow r6] ++ [toXMLBoardRow r7] ++ [toXMLBoardRow r8] ++ []
toXMLBoard HoleBoard = Elt "HoleBoard" [] []
toXMLBoard (ParseErrBoard _) = Elt "ParseErrBoard" [] []
toXMLBoardRow (BoardRow _ ca cb cc cd ce cf cg ch) = Elt "BoardRow" [] $ [toXMLBoardSquare ca] ++ [toXMLBoardSquare cb] ++ [toXMLBoardSquare cc] ++ [toXMLBoardSquare cd] ++ [toXMLBoardSquare ce] ++ [toXMLBoardSquare cf] ++ [toXMLBoardSquare cg] ++ [toXMLBoardSquare ch] ++ []
toXMLBoardRow HoleBoardRow = Elt "HoleBoardRow" [] []
toXMLBoardRow (ParseErrBoardRow _) = Elt "ParseErrBoardRow" [] []
toXMLBoardSquare (Queen _ color) = Elt "Queen" [] $ [toXMLBool_ color] ++ []
toXMLBoardSquare (King _ color) = Elt "King" [] $ [toXMLBool_ color] ++ []
toXMLBoardSquare (Bishop _ color) = Elt "Bishop" [] $ [toXMLBool_ color] ++ []
toXMLBoardSquare (Knight _ color) = Elt "Knight" [] $ [toXMLBool_ color] ++ []
toXMLBoardSquare (Rook _ color) = Elt "Rook" [] $ [toXMLBool_ color] ++ []
toXMLBoardSquare (Pawn _ color) = Elt "Pawn" [] $ [toXMLBool_ color] ++ []
toXMLBoardSquare (Empty) = Elt "Empty" [] $ []
toXMLBoardSquare HoleBoardSquare = Elt "HoleBoardSquare" [] []
toXMLBoardSquare (ParseErrBoardSquare _) = Elt "ParseErrBoardSquare" [] []
toXMLPPPresentation (PPPresentation _ viewType slides) = Elt "PPPresentation" [] $ [toXMLBool_ viewType] ++ toXMLList_Slide slides ++ []
toXMLPPPresentation HolePPPresentation = Elt "HolePPPresentation" [] []
toXMLPPPresentation (ParseErrPPPresentation _) = Elt "ParseErrPPPresentation" [] []
toXMLSlide (Slide _ title itemList) = Elt "Slide" [] $ [toXMLString_ title] ++ [toXMLItemList itemList] ++ []
toXMLSlide HoleSlide = Elt "HoleSlide" [] []
toXMLSlide (ParseErrSlide _) = Elt "ParseErrSlide" [] []
toXMLItemList (ItemList _ listType items) = Elt "ItemList" [] $ [toXMLListType listType] ++ toXMLList_Item items ++ []
toXMLItemList HoleItemList = Elt "HoleItemList" [] []
toXMLItemList (ParseErrItemList _) = Elt "ParseErrItemList" [] []
toXMLListType (Bullet _) = Elt "Bullet" [] $ []
toXMLListType (Number _) = Elt "Number" [] $ []
toXMLListType (Alpha _) = Elt "Alpha" [] $ []
toXMLListType HoleListType = Elt "HoleListType" [] []
toXMLListType (ParseErrListType _) = Elt "ParseErrListType" [] []
toXMLItem (StringItem _ string) = Elt "StringItem" [] $ [toXMLString_ string] ++ []
toXMLItem (HeliumItem _ exp) = Elt "HeliumItem" [] $ [toXMLExp exp] ++ []
toXMLItem (ListItem _ itemList) = Elt "ListItem" [] $ [toXMLItemList itemList] ++ []
toXMLItem HoleItem = Elt "HoleItem" [] []
toXMLItem (ParseErrItem _) = Elt "ParseErrItem" [] []
toXMLList_Dummy (List_Dummy _ dummys) = toXMLConsList_Dummy dummys
toXMLList_Dummy HoleList_Dummy = []
toXMLList_Dummy (ParseErrList_Dummy _) = []
toXMLConsList_Dummy (Cons_Dummy dummy dummys) = toXMLDummy dummy : toXMLConsList_Dummy dummys
toXMLConsList_Dummy Nil_Dummy             = []
toXMLList_Decl (List_Decl _ decls) = toXMLConsList_Decl decls
toXMLList_Decl HoleList_Decl = []
toXMLList_Decl (ParseErrList_Decl _) = []
toXMLConsList_Decl (Cons_Decl decl decls) = toXMLDecl decl : toXMLConsList_Decl decls
toXMLConsList_Decl Nil_Decl             = []
toXMLList_Alt (List_Alt _ alts) = toXMLConsList_Alt alts
toXMLList_Alt HoleList_Alt = []
toXMLList_Alt (ParseErrList_Alt _) = []
toXMLConsList_Alt (Cons_Alt alt alts) = toXMLAlt alt : toXMLConsList_Alt alts
toXMLConsList_Alt Nil_Alt             = []
toXMLList_Exp (List_Exp _ exps) = toXMLConsList_Exp exps
toXMLList_Exp HoleList_Exp = []
toXMLList_Exp (ParseErrList_Exp _) = []
toXMLConsList_Exp (Cons_Exp exp exps) = toXMLExp exp : toXMLConsList_Exp exps
toXMLConsList_Exp Nil_Exp             = []
toXMLList_Slide (List_Slide _ slides) = toXMLConsList_Slide slides
toXMLList_Slide HoleList_Slide = []
toXMLList_Slide (ParseErrList_Slide _) = []
toXMLConsList_Slide (Cons_Slide slide slides) = toXMLSlide slide : toXMLConsList_Slide slides
toXMLConsList_Slide Nil_Slide             = []
toXMLList_Item (List_Item _ items) = toXMLConsList_Item items
toXMLList_Item HoleList_Item = []
toXMLList_Item (ParseErrList_Item _) = []
toXMLConsList_Item (Cons_Item item items) = toXMLItem item : toXMLConsList_Item items
toXMLConsList_Item Nil_Item             = []



parseXML_String_ = parseXMLCns_String_ <?|> parseHoleAndParseErr "String_" HoleString_
parseXMLCns_String_ = String_ NoIDD <$ startTag "String_" <*> parseXML_String <* endTag "String_"
parseXML_Bool_ = parseXMLCns_Bool_ <?|> parseHoleAndParseErr "Bool_" HoleBool_
parseXMLCns_Bool_ = Bool_ NoIDD <$ startTag "Bool_" <*> parseXML_Bool <* endTag "Bool_"
parseXML_Int_ = parseXMLCns_Int_ <?|> parseHoleAndParseErr "Int_" HoleInt_
parseXMLCns_Int_ = Int_ NoIDD <$ startTag "Int_" <*> parseXML_Int <* endTag "Int_"
parseXML_Dummy = parseXMLCns_Dummy <?|> parseHoleAndParseErr "Dummy" HoleDummy
parseXMLCns_Dummy = Dummy NoIDD <$ startTag "Dummy" <*> parseXML_Root <*> parseXML_List_Dummy <*> parseXML_String_ <*> parseXML_Bool_ <*> parseXML_Int_ <* endTag "Dummy"
parseXML_EnrichedDoc = parseXMLCns_RootEnr <?|> parseHoleAndParseErr "EnrichedDoc" HoleEnrichedDoc
parseXMLCns_RootEnr = RootEnr NoIDD <$ startTag "RootEnr" <*> parseXML_RootE <*> parseXML_HeliumTypeInfo <*> parseXML_Document <* endTag "RootEnr"
parseXML_Root = parseXMLCns_Root <?|> parseHoleAndParseErr "Root" HoleRoot
parseXMLCns_Root = Root NoIDD NoIDP <$ startTag "Root" <*> parseXML_List_Decl <* endTag "Root"
parseXML_RootE = parseXMLCns_RootE <?|> parseHoleAndParseErr "RootE" HoleRootE
parseXMLCns_RootE = RootE NoIDD NoIDP <$ startTag "RootE" <*> parseXML_List_Decl <*> parseXML_List_Decl <* endTag "RootE"
parseXML_Decl = parseXMLCns_Decl <?|> parseXMLCns_BoardDecl <?|> parseXMLCns_PPPresentationDecl <?|> parseHoleAndParseErr "Decl" HoleDecl
parseXMLCns_Decl = Decl NoIDD NoIDP NoIDP NoIDP NoIDP <$ startTag "Decl" <*> parseXML_Bool_ <*> parseXML_Bool_ <*> parseXML_Ident <*> parseXML_Exp <* endTag "Decl"
parseXMLCns_BoardDecl = BoardDecl NoIDD NoIDP NoIDP <$ startTag "BoardDecl" <*> parseXML_Board <* endTag "BoardDecl"
parseXMLCns_PPPresentationDecl = PPPresentationDecl NoIDD NoIDP NoIDP <$ startTag "PPPresentationDecl" <*> parseXML_PPPresentation <* endTag "PPPresentationDecl"
parseXML_Ident = parseXMLCns_Ident <?|> parseHoleAndParseErr "Ident" HoleIdent
parseXMLCns_Ident = Ident NoIDD NoIDP NoIDP <$ startTag "Ident" <*> parseXML_String_ <* endTag "Ident"
parseXML_Exp = parseXMLCns_PlusExp <?|> parseXMLCns_TimesExp <?|> parseXMLCns_DivExp <?|> parseXMLCns_PowerExp <?|> parseXMLCns_BoolExp <?|> parseXMLCns_IntExp <?|> parseXMLCns_LamExp <?|> parseXMLCns_AppExp <?|> parseXMLCns_CaseExp <?|> parseXMLCns_LetExp <?|> parseXMLCns_IdentExp <?|> parseXMLCns_IfExp <?|> parseXMLCns_ParenExp <?|> parseXMLCns_ListExp <?|> parseXMLCns_ProductExp <?|> parseHoleAndParseErr "Exp" HoleExp
parseXMLCns_PlusExp = PlusExp NoIDD NoIDP <$ startTag "PlusExp" <*> parseXML_Exp <*> parseXML_Exp <* endTag "PlusExp"
parseXMLCns_TimesExp = TimesExp NoIDD NoIDP <$ startTag "TimesExp" <*> parseXML_Exp <*> parseXML_Exp <* endTag "TimesExp"
parseXMLCns_DivExp = DivExp NoIDD NoIDP <$ startTag "DivExp" <*> parseXML_Exp <*> parseXML_Exp <* endTag "DivExp"
parseXMLCns_PowerExp = PowerExp NoIDD NoIDP <$ startTag "PowerExp" <*> parseXML_Exp <*> parseXML_Exp <* endTag "PowerExp"
parseXMLCns_BoolExp = BoolExp NoIDD NoIDP <$ startTag "BoolExp" <*> parseXML_Bool_ <* endTag "BoolExp"
parseXMLCns_IntExp = IntExp NoIDD NoIDP <$ startTag "IntExp" <*> parseXML_Int_ <* endTag "IntExp"
parseXMLCns_LamExp = LamExp NoIDD NoIDP NoIDP <$ startTag "LamExp" <*> parseXML_Ident <*> parseXML_Exp <* endTag "LamExp"
parseXMLCns_AppExp = AppExp NoIDD <$ startTag "AppExp" <*> parseXML_Exp <*> parseXML_Exp <* endTag "AppExp"
parseXMLCns_CaseExp = CaseExp NoIDD NoIDP NoIDP <$ startTag "CaseExp" <*> parseXML_Exp <*> parseXML_List_Alt <* endTag "CaseExp"
parseXMLCns_LetExp = LetExp NoIDD NoIDP NoIDP <$ startTag "LetExp" <*> parseXML_List_Decl <*> parseXML_Exp <* endTag "LetExp"
parseXMLCns_IdentExp = IdentExp NoIDD <$ startTag "IdentExp" <*> parseXML_Ident <* endTag "IdentExp"
parseXMLCns_IfExp = IfExp NoIDD NoIDP NoIDP NoIDP <$ startTag "IfExp" <*> parseXML_Exp <*> parseXML_Exp <*> parseXML_Exp <* endTag "IfExp"
parseXMLCns_ParenExp = ParenExp NoIDD NoIDP NoIDP <$ startTag "ParenExp" <*> parseXML_Exp <* endTag "ParenExp"
parseXMLCns_ListExp = ListExp NoIDD NoIDP NoIDP [] <$ startTag "ListExp" <*> parseXML_List_Exp <* endTag "ListExp"
parseXMLCns_ProductExp = ProductExp NoIDD NoIDP NoIDP [] <$ startTag "ProductExp" <*> parseXML_List_Exp <* endTag "ProductExp"
parseXML_Alt = parseXMLCns_Alt <?|> parseHoleAndParseErr "Alt" HoleAlt
parseXMLCns_Alt = Alt NoIDD NoIDP NoIDP <$ startTag "Alt" <*> parseXML_Ident <*> parseXML_Exp <* endTag "Alt"
parseXML_Board = parseXMLCns_Board <?|> parseHoleAndParseErr "Board" HoleBoard
parseXMLCns_Board = Board NoIDD <$ startTag "Board" <*> parseXML_BoardRow <*> parseXML_BoardRow <*> parseXML_BoardRow <*> parseXML_BoardRow <*> parseXML_BoardRow <*> parseXML_BoardRow <*> parseXML_BoardRow <*> parseXML_BoardRow <* endTag "Board"
parseXML_BoardRow = parseXMLCns_BoardRow <?|> parseHoleAndParseErr "BoardRow" HoleBoardRow
parseXMLCns_BoardRow = BoardRow NoIDD <$ startTag "BoardRow" <*> parseXML_BoardSquare <*> parseXML_BoardSquare <*> parseXML_BoardSquare <*> parseXML_BoardSquare <*> parseXML_BoardSquare <*> parseXML_BoardSquare <*> parseXML_BoardSquare <*> parseXML_BoardSquare <* endTag "BoardRow"
parseXML_BoardSquare = parseXMLCns_Queen <?|> parseXMLCns_King <?|> parseXMLCns_Bishop <?|> parseXMLCns_Knight <?|> parseXMLCns_Rook <?|> parseXMLCns_Pawn <?|> parseXMLCns_Empty <?|> parseHoleAndParseErr "BoardSquare" HoleBoardSquare
parseXMLCns_Queen = Queen NoIDD <$ startTag "Queen" <*> parseXML_Bool_ <* endTag "Queen"
parseXMLCns_King = King NoIDD <$ startTag "King" <*> parseXML_Bool_ <* endTag "King"
parseXMLCns_Bishop = Bishop NoIDD <$ startTag "Bishop" <*> parseXML_Bool_ <* endTag "Bishop"
parseXMLCns_Knight = Knight NoIDD <$ startTag "Knight" <*> parseXML_Bool_ <* endTag "Knight"
parseXMLCns_Rook = Rook NoIDD <$ startTag "Rook" <*> parseXML_Bool_ <* endTag "Rook"
parseXMLCns_Pawn = Pawn NoIDD <$ startTag "Pawn" <*> parseXML_Bool_ <* endTag "Pawn"
parseXMLCns_Empty = Empty <$ emptyTag "Empty"
parseXML_PPPresentation = parseXMLCns_PPPresentation <?|> parseHoleAndParseErr "PPPresentation" HolePPPresentation
parseXMLCns_PPPresentation = PPPresentation NoIDD <$ startTag "PPPresentation" <*> parseXML_Bool_ <*> parseXML_List_Slide <* endTag "PPPresentation"
parseXML_Slide = parseXMLCns_Slide <?|> parseHoleAndParseErr "Slide" HoleSlide
parseXMLCns_Slide = Slide NoIDD <$ startTag "Slide" <*> parseXML_String_ <*> parseXML_ItemList <* endTag "Slide"
parseXML_ItemList = parseXMLCns_ItemList <?|> parseHoleAndParseErr "ItemList" HoleItemList
parseXMLCns_ItemList = ItemList NoIDD <$ startTag "ItemList" <*> parseXML_ListType <*> parseXML_List_Item <* endTag "ItemList"
parseXML_ListType = parseXMLCns_Bullet <?|> parseXMLCns_Number <?|> parseXMLCns_Alpha <?|> parseHoleAndParseErr "ListType" HoleListType
parseXMLCns_Bullet = Bullet NoIDD <$ emptyTag "Bullet"
parseXMLCns_Number = Number NoIDD <$ emptyTag "Number"
parseXMLCns_Alpha = Alpha NoIDD <$ emptyTag "Alpha"
parseXML_Item = parseXMLCns_StringItem <?|> parseXMLCns_HeliumItem <?|> parseXMLCns_ListItem <?|> parseHoleAndParseErr "Item" HoleItem
parseXMLCns_StringItem = StringItem NoIDD <$ startTag "StringItem" <*> parseXML_String_ <* endTag "StringItem"
parseXMLCns_HeliumItem = HeliumItem NoIDD <$ startTag "HeliumItem" <*> parseXML_Exp <* endTag "HeliumItem"
parseXMLCns_ListItem = ListItem NoIDD <$ startTag "ListItem" <*> parseXML_ItemList <* endTag "ListItem"
parseXML_List_Dummy = mkList List_Dummy Cons_Dummy Nil_Dummy <$> many parseXML_Dummy
parseXML_List_Decl = mkList List_Decl Cons_Decl Nil_Decl <$> many parseXML_Decl
parseXML_List_Alt = mkList List_Alt Cons_Alt Nil_Alt <$> many parseXML_Alt
parseXML_List_Exp = mkList List_Exp Cons_Exp Nil_Exp <$> many parseXML_Exp
parseXML_List_Slide = mkList List_Slide Cons_Slide Nil_Slide <$> many parseXML_Slide
parseXML_List_Item = mkList List_Item Cons_Item Nil_Item <$> many parseXML_Item
