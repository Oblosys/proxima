module DocUtils_Generated where

import Evaluation.DocTypes
import DocTypes_Generated
import Evaluation.DocumentEdit
import DocumentEdit_Generated
import Presentation.PresTypes
import Text.ParserCombinators.Parsec
import Evaluation.DocUtils
import Common.CommonTypes
import Common.CommonUtils
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
    ; return $ RootDoc $ Root NoIDP $ ParseErrList_Decl (ColP NoIDP 0 NF . map (StringP NoIDP). lines' $ fileContents) {- [] -}
    }
    -- by putting the text in a parse error node, we don't need to specify a textual parser. Instead,
    -- the proxima parser is used when the presented document is parsed.

-- XML

-- we don't put a "RootDoc" element in the XML, because this type is not visible to the user.
toXMLDocument (RootDoc root) = toXMLRoot root
toXMLDocument _              = debug Err "DocUtils_Generated.toXMLDocument: malformed Document" $
                                 Elt "Root" [] [] -- this does not occur

parseXML_Document = RootDoc <$> parseXML_Root

toXMLHeliumTypeInfo _ = Elt "HeliumTypeInfo" [] []
parseXML_HeliumTypeInfo = ([],[],[]) <$ emptyTag "HeliumTypeInfo"



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





initBoard = demoBoard -- Board (backRow (Bool_ True)) (pawnRow (Bool_ True)) emptyRow emptyRow emptyRow emptyRow (pawnRow (Bool_ False)) (backRow (Bool_ False))

demoBoard = Board r8 r7 r6 r5 r4 r3 r2 r1
 where r1 = BoardRow e  e  bb e  e  e  br bk
       r2 = BoardRow e  e  e  e  bn bp e  bp
       r3 = BoardRow bp e  e  e  e  e  e  wp
       r4 = BoardRow e  e  br e  bq bp wq wr
       r5 = BoardRow e  bp wp e  e  e  e  e
       r6 = BoardRow e  e  e  e  wn e  e  e
       r7 = BoardRow wp wp wb e  e  wp wp e
       r8 = BoardRow e  wk e  wr e  e  e  e
       [e,wp,wr,wn,wb,wq,wk,bp,br,bn,bb,bq,bk] = Empty : pieces True ++ pieces False
       pieces c = [Pawn c, Rook c, Knight c, Bishop c, Queen c, King c]
      
  
-- Kasparov,G - Lautier,J Moscow, 1995 1.Ng4!! Qe6 [1...Rxg5 2.Nxe5 Rxh5 3.Rd8+ Ng8 4.Nxf7#] 2.Rd8 Qg6 3.Qxe7 1-0

emptyBoard = Board emptyRow emptyRow emptyRow emptyRow emptyRow emptyRow emptyRow emptyRow

emptyRow = BoardRow Empty Empty Empty Empty Empty Empty Empty Empty

pawnRow c = BoardRow (Pawn c) (Pawn c) (Pawn c) (Pawn c) 
                          (Pawn c) (Pawn c) (Pawn c) (Pawn c)
backRow c = BoardRow (Rook c) (Knight c) (Bishop c) (Queen c) 
                          (King c) (Bishop c) (Knight c) (Rook c)




initPPPresentation = 
  PPPresentation True $ List_Slide $
    mkSlides
      [ Slide "slide_1" $
          ItemList (Bullet) $ List_Item $
                         mkItems [ StringItem "item_1"
                                 , HeliumItem -- simple trick to use parser: Needs an additional parse (F1) though!
                                     --(ident "\\ x -> increaze x")
                                     (ident "\\b -> \\x -> if b then ink x else x")
                                 , StringItem "item_2"
                                 , ListItem listItem
                                 ]
       , Slide "slide_2" $
          ItemList (Alpha) $  List_Item $
                         mkItems [ StringItem "item_a"
                                 , StringItem "item_b"
                                 , StringItem "item_c"
                                 ]
      ]
 where listItem = ItemList (Number) $  List_Item $
                    mkItems [ StringItem "nested_item_1"
                            , ListItem listItem'
                            , StringItem "nested_item_2"
                            , StringItem "nested_item_3"
                            ]
       listItem' = ItemList (Bullet) $  List_Item $
                    mkItems [ StringItem "nested_nested_item"
                            , StringItem "nested_nested_item"
                            , StringItem "nested_nested_item"
                            ]
       dv e1 e2 = DivExp NoIDP e1 e2 
       lam str body = LamExp NoIDP NoIDP (Ident NoIDP NoIDP str) body
       ifxp c t e = IfExp NoIDP NoIDP NoIDP c t e 
       int i = IntExp NoIDP i
       bool b = BoolExp NoIDP b
       ident str = IdentExp (Ident NoIDP NoIDP str)
       

mkSlides []     = Nil_Slide
mkSlides (s:ss) = Cons_Slide s (mkSlides ss)

mkItems []     = Nil_Item 
mkItems (s:ss) = Cons_Item s (mkItems ss)






----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- rankNode                                                             --
--------------------------------------------------------------------------

rankNode :: Node -> Int
rankNode NoNode = 0
rankNode (RootDocNode _ _) = 1
rankNode (HoleDocumentNode _ _) = 2
rankNode (ParseErrDocumentNode _ _) = 3
rankNode (DummyNode _ _) = 4
rankNode (HoleDummyNode _ _) = 5
rankNode (ParseErrDummyNode _ _) = 6
rankNode (RootEnrNode _ _) = 7
rankNode (HoleEnrichedDocNode _ _) = 8
rankNode (ParseErrEnrichedDocNode _ _) = 9
rankNode (RootNode _ _) = 10
rankNode (HoleRootNode _ _) = 11
rankNode (ParseErrRootNode _ _) = 12
rankNode (RootENode _ _) = 13
rankNode (HoleRootENode _ _) = 14
rankNode (ParseErrRootENode _ _) = 15
rankNode (DeclNode _ _) = 16
rankNode (BoardDeclNode _ _) = 17
rankNode (PPPresentationDeclNode _ _) = 18
rankNode (HoleDeclNode _ _) = 19
rankNode (ParseErrDeclNode _ _) = 20
rankNode (IdentNode _ _) = 21
rankNode (HoleIdentNode _ _) = 22
rankNode (ParseErrIdentNode _ _) = 23
rankNode (PlusExpNode _ _) = 24
rankNode (TimesExpNode _ _) = 25
rankNode (DivExpNode _ _) = 26
rankNode (PowerExpNode _ _) = 27
rankNode (BoolExpNode _ _) = 28
rankNode (IntExpNode _ _) = 29
rankNode (LamExpNode _ _) = 30
rankNode (AppExpNode _ _) = 31
rankNode (CaseExpNode _ _) = 32
rankNode (LetExpNode _ _) = 33
rankNode (IdentExpNode _ _) = 34
rankNode (IfExpNode _ _) = 35
rankNode (ParenExpNode _ _) = 36
rankNode (ListExpNode _ _) = 37
rankNode (ProductExpNode _ _) = 38
rankNode (HoleExpNode _ _) = 39
rankNode (ParseErrExpNode _ _) = 40
rankNode (AltNode _ _) = 41
rankNode (HoleAltNode _ _) = 42
rankNode (ParseErrAltNode _ _) = 43
rankNode (BoardNode _ _) = 44
rankNode (HoleBoardNode _ _) = 45
rankNode (ParseErrBoardNode _ _) = 46
rankNode (BoardRowNode _ _) = 47
rankNode (HoleBoardRowNode _ _) = 48
rankNode (ParseErrBoardRowNode _ _) = 49
rankNode (QueenNode _ _) = 50
rankNode (KingNode _ _) = 51
rankNode (BishopNode _ _) = 52
rankNode (KnightNode _ _) = 53
rankNode (RookNode _ _) = 54
rankNode (PawnNode _ _) = 55
rankNode (EmptyNode _ _) = 56
rankNode (HoleBoardSquareNode _ _) = 57
rankNode (ParseErrBoardSquareNode _ _) = 58
rankNode (PPPresentationNode _ _) = 59
rankNode (HolePPPresentationNode _ _) = 60
rankNode (ParseErrPPPresentationNode _ _) = 61
rankNode (SlideNode _ _) = 62
rankNode (HoleSlideNode _ _) = 63
rankNode (ParseErrSlideNode _ _) = 64
rankNode (ItemListNode _ _) = 65
rankNode (HoleItemListNode _ _) = 66
rankNode (ParseErrItemListNode _ _) = 67
rankNode (BulletNode _ _) = 68
rankNode (NumberNode _ _) = 69
rankNode (AlphaNode _ _) = 70
rankNode (HoleListTypeNode _ _) = 71
rankNode (ParseErrListTypeNode _ _) = 72
rankNode (StringItemNode _ _) = 73
rankNode (HeliumItemNode _ _) = 74
rankNode (ListItemNode _ _) = 75
rankNode (HoleItemNode _ _) = 76
rankNode (ParseErrItemNode _ _) = 77
rankNode (List_DeclNode _ _) = 78
rankNode (HoleList_DeclNode _ _) = 79
rankNode (ParseErrList_DeclNode _ _) = 80
rankNode (List_AltNode _ _) = 81
rankNode (HoleList_AltNode _ _) = 82
rankNode (ParseErrList_AltNode _ _) = 83
rankNode (List_ExpNode _ _) = 84
rankNode (HoleList_ExpNode _ _) = 85
rankNode (ParseErrList_ExpNode _ _) = 86
rankNode (List_SlideNode _ _) = 87
rankNode (HoleList_SlideNode _ _) = 88
rankNode (ParseErrList_SlideNode _ _) = 89
rankNode (List_ItemNode _ _) = 90
rankNode (HoleList_ItemNode _ _) = 91
rankNode (ParseErrList_ItemNode _ _) = 92



--------------------------------------------------------------------------
-- HasPath instance for Node                                            --
--------------------------------------------------------------------------

instance HasPath Node where
  pathNode NoNode            = NoPathD
  pathNode (RootDocNode _ pth) = PathD pth
  pathNode (HoleDocumentNode _ pth) = PathD pth
  pathNode (ParseErrDocumentNode _ pth) = PathD pth
  pathNode (DummyNode _ pth) = PathD pth
  pathNode (HoleDummyNode _ pth) = PathD pth
  pathNode (ParseErrDummyNode _ pth) = PathD pth
  pathNode (RootEnrNode _ pth) = PathD pth
  pathNode (HoleEnrichedDocNode _ pth) = PathD pth
  pathNode (ParseErrEnrichedDocNode _ pth) = PathD pth
  pathNode (RootNode _ pth) = PathD pth
  pathNode (HoleRootNode _ pth) = PathD pth
  pathNode (ParseErrRootNode _ pth) = PathD pth
  pathNode (RootENode _ pth) = PathD pth
  pathNode (HoleRootENode _ pth) = PathD pth
  pathNode (ParseErrRootENode _ pth) = PathD pth
  pathNode (DeclNode _ pth) = PathD pth
  pathNode (BoardDeclNode _ pth) = PathD pth
  pathNode (PPPresentationDeclNode _ pth) = PathD pth
  pathNode (HoleDeclNode _ pth) = PathD pth
  pathNode (ParseErrDeclNode _ pth) = PathD pth
  pathNode (IdentNode _ pth) = PathD pth
  pathNode (HoleIdentNode _ pth) = PathD pth
  pathNode (ParseErrIdentNode _ pth) = PathD pth
  pathNode (PlusExpNode _ pth) = PathD pth
  pathNode (TimesExpNode _ pth) = PathD pth
  pathNode (DivExpNode _ pth) = PathD pth
  pathNode (PowerExpNode _ pth) = PathD pth
  pathNode (BoolExpNode _ pth) = PathD pth
  pathNode (IntExpNode _ pth) = PathD pth
  pathNode (LamExpNode _ pth) = PathD pth
  pathNode (AppExpNode _ pth) = PathD pth
  pathNode (CaseExpNode _ pth) = PathD pth
  pathNode (LetExpNode _ pth) = PathD pth
  pathNode (IdentExpNode _ pth) = PathD pth
  pathNode (IfExpNode _ pth) = PathD pth
  pathNode (ParenExpNode _ pth) = PathD pth
  pathNode (ListExpNode _ pth) = PathD pth
  pathNode (ProductExpNode _ pth) = PathD pth
  pathNode (HoleExpNode _ pth) = PathD pth
  pathNode (ParseErrExpNode _ pth) = PathD pth
  pathNode (AltNode _ pth) = PathD pth
  pathNode (HoleAltNode _ pth) = PathD pth
  pathNode (ParseErrAltNode _ pth) = PathD pth
  pathNode (BoardNode _ pth) = PathD pth
  pathNode (HoleBoardNode _ pth) = PathD pth
  pathNode (ParseErrBoardNode _ pth) = PathD pth
  pathNode (BoardRowNode _ pth) = PathD pth
  pathNode (HoleBoardRowNode _ pth) = PathD pth
  pathNode (ParseErrBoardRowNode _ pth) = PathD pth
  pathNode (QueenNode _ pth) = PathD pth
  pathNode (KingNode _ pth) = PathD pth
  pathNode (BishopNode _ pth) = PathD pth
  pathNode (KnightNode _ pth) = PathD pth
  pathNode (RookNode _ pth) = PathD pth
  pathNode (PawnNode _ pth) = PathD pth
  pathNode (EmptyNode _ pth) = PathD pth
  pathNode (HoleBoardSquareNode _ pth) = PathD pth
  pathNode (ParseErrBoardSquareNode _ pth) = PathD pth
  pathNode (PPPresentationNode _ pth) = PathD pth
  pathNode (HolePPPresentationNode _ pth) = PathD pth
  pathNode (ParseErrPPPresentationNode _ pth) = PathD pth
  pathNode (SlideNode _ pth) = PathD pth
  pathNode (HoleSlideNode _ pth) = PathD pth
  pathNode (ParseErrSlideNode _ pth) = PathD pth
  pathNode (ItemListNode _ pth) = PathD pth
  pathNode (HoleItemListNode _ pth) = PathD pth
  pathNode (ParseErrItemListNode _ pth) = PathD pth
  pathNode (BulletNode _ pth) = PathD pth
  pathNode (NumberNode _ pth) = PathD pth
  pathNode (AlphaNode _ pth) = PathD pth
  pathNode (HoleListTypeNode _ pth) = PathD pth
  pathNode (ParseErrListTypeNode _ pth) = PathD pth
  pathNode (StringItemNode _ pth) = PathD pth
  pathNode (HeliumItemNode _ pth) = PathD pth
  pathNode (ListItemNode _ pth) = PathD pth
  pathNode (HoleItemNode _ pth) = PathD pth
  pathNode (ParseErrItemNode _ pth) = PathD pth
  pathNode (List_DeclNode _ pth) = PathD pth
  pathNode (HoleList_DeclNode _ pth) = PathD pth
  pathNode (ParseErrList_DeclNode _ pth) = PathD pth
  pathNode (List_AltNode _ pth) = PathD pth
  pathNode (HoleList_AltNode _ pth) = PathD pth
  pathNode (ParseErrList_AltNode _ pth) = PathD pth
  pathNode (List_ExpNode _ pth) = PathD pth
  pathNode (HoleList_ExpNode _ pth) = PathD pth
  pathNode (ParseErrList_ExpNode _ pth) = PathD pth
  pathNode (List_SlideNode _ pth) = PathD pth
  pathNode (HoleList_SlideNode _ pth) = PathD pth
  pathNode (ParseErrList_SlideNode _ pth) = PathD pth
  pathNode (List_ItemNode _ pth) = PathD pth
  pathNode (HoleList_ItemNode _ pth) = PathD pth
  pathNode (ParseErrList_ItemNode _ pth) = PathD pth



--------------------------------------------------------------------------
-- toXML functions                                                      --
--------------------------------------------------------------------------

toXMLDummy (Dummy root dummy) = Elt "Dummy" [] $ [toXMLRoot root] ++ [toXMLDummy dummy]
toXMLDummy (HoleDummy) = Elt "HoleDummy" [] $ []
toXMLDummy (ParseErrDummy presentation) = Elt "ParseErrDummy" [] []
toXMLEnrichedDoc (RootEnr root heliumTypeInfo document) = Elt "RootEnr" [] $ [toXMLRootE root] ++ [toXMLHeliumTypeInfo heliumTypeInfo] ++ [toXMLDocument document]
toXMLEnrichedDoc (HoleEnrichedDoc) = Elt "HoleEnrichedDoc" [] $ []
toXMLEnrichedDoc (ParseErrEnrichedDoc presentation) = Elt "ParseErrEnrichedDoc" [] []
toXMLRoot (Root _ decls) = Elt "Root" [] $ toXMLList_Decl decls
toXMLRoot (HoleRoot) = Elt "HoleRoot" [] $ []
toXMLRoot (ParseErrRoot presentation) = Elt "ParseErrRoot" [] []
toXMLRootE (RootE _ decls idListDecls) = Elt "RootE" [] $ toXMLList_Decl decls ++ toXMLList_Decl idListDecls
toXMLRootE (HoleRootE) = Elt "HoleRootE" [] $ []
toXMLRootE (ParseErrRootE presentation) = Elt "ParseErrRootE" [] []
toXMLDecl (Decl _ _ _ _ expanded autoLayout ident exp) = Elt "Decl" [] $ [toXMLBool expanded] ++ [toXMLBool autoLayout] ++ [toXMLIdent ident] ++ [toXMLExp exp]
toXMLDecl (BoardDecl _ _ board) = Elt "BoardDecl" [] $ [toXMLBoard board]
toXMLDecl (PPPresentationDecl _ _ pPPresentation) = Elt "PPPresentationDecl" [] $ [toXMLPPPresentation pPPresentation]
toXMLDecl (HoleDecl) = Elt "HoleDecl" [] $ []
toXMLDecl (ParseErrDecl presentation) = Elt "ParseErrDecl" [] []
toXMLIdent (Ident _ _ string) = Elt "Ident" [] $ [toXMLString string]
toXMLIdent (HoleIdent) = Elt "HoleIdent" [] $ []
toXMLIdent (ParseErrIdent presentation) = Elt "ParseErrIdent" [] []
toXMLExp (PlusExp _ exp1 exp2) = Elt "PlusExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2]
toXMLExp (TimesExp _ exp1 exp2) = Elt "TimesExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2]
toXMLExp (DivExp _ exp1 exp2) = Elt "DivExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2]
toXMLExp (PowerExp _ exp1 exp2) = Elt "PowerExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2]
toXMLExp (BoolExp _ bool) = Elt "BoolExp" [] $ [toXMLBool bool]
toXMLExp (IntExp _ int) = Elt "IntExp" [] $ [toXMLInt int]
toXMLExp (LamExp _ _ ident exp) = Elt "LamExp" [] $ [toXMLIdent ident] ++ [toXMLExp exp]
toXMLExp (AppExp exp1 exp2) = Elt "AppExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2]
toXMLExp (CaseExp _ _ exp alts) = Elt "CaseExp" [] $ [toXMLExp exp] ++ toXMLList_Alt alts
toXMLExp (LetExp _ _ decls exp) = Elt "LetExp" [] $ toXMLList_Decl decls ++ [toXMLExp exp]
toXMLExp (IdentExp ident) = Elt "IdentExp" [] $ [toXMLIdent ident]
toXMLExp (IfExp _ _ _ exp1 exp2 exp3) = Elt "IfExp" [] $ [toXMLExp exp1] ++ [toXMLExp exp2] ++ [toXMLExp exp3]
toXMLExp (ParenExp _ _ exp) = Elt "ParenExp" [] $ [toXMLExp exp]
toXMLExp (ListExp _ _ _ exps) = Elt "ListExp" [] $ toXMLList_Exp exps
toXMLExp (ProductExp _ _ _ exps) = Elt "ProductExp" [] $ toXMLList_Exp exps
toXMLExp (HoleExp) = Elt "HoleExp" [] $ []
toXMLExp (ParseErrExp presentation) = Elt "ParseErrExp" [] []
toXMLAlt (Alt _ _ ident exp) = Elt "Alt" [] $ [toXMLIdent ident] ++ [toXMLExp exp]
toXMLAlt (HoleAlt) = Elt "HoleAlt" [] $ []
toXMLAlt (ParseErrAlt presentation) = Elt "ParseErrAlt" [] []
toXMLBoard (Board r1 r2 r3 r4 r5 r6 r7 r8) = Elt "Board" [] $ [toXMLBoardRow r1] ++ [toXMLBoardRow r2] ++ [toXMLBoardRow r3] ++ [toXMLBoardRow r4] ++ [toXMLBoardRow r5] ++ [toXMLBoardRow r6] ++ [toXMLBoardRow r7] ++ [toXMLBoardRow r8]
toXMLBoard (HoleBoard) = Elt "HoleBoard" [] $ []
toXMLBoard (ParseErrBoard presentation) = Elt "ParseErrBoard" [] []
toXMLBoardRow (BoardRow ca cb cc cd ce cf cg ch) = Elt "BoardRow" [] $ [toXMLBoardSquare ca] ++ [toXMLBoardSquare cb] ++ [toXMLBoardSquare cc] ++ [toXMLBoardSquare cd] ++ [toXMLBoardSquare ce] ++ [toXMLBoardSquare cf] ++ [toXMLBoardSquare cg] ++ [toXMLBoardSquare ch]
toXMLBoardRow (HoleBoardRow) = Elt "HoleBoardRow" [] $ []
toXMLBoardRow (ParseErrBoardRow presentation) = Elt "ParseErrBoardRow" [] []
toXMLBoardSquare (Queen color) = Elt "Queen" [] $ [toXMLBool color]
toXMLBoardSquare (King color) = Elt "King" [] $ [toXMLBool color]
toXMLBoardSquare (Bishop color) = Elt "Bishop" [] $ [toXMLBool color]
toXMLBoardSquare (Knight color) = Elt "Knight" [] $ [toXMLBool color]
toXMLBoardSquare (Rook color) = Elt "Rook" [] $ [toXMLBool color]
toXMLBoardSquare (Pawn color) = Elt "Pawn" [] $ [toXMLBool color]
toXMLBoardSquare (Empty) = Elt "Empty" [] $ []
toXMLBoardSquare (HoleBoardSquare) = Elt "HoleBoardSquare" [] $ []
toXMLBoardSquare (ParseErrBoardSquare presentation) = Elt "ParseErrBoardSquare" [] []
toXMLPPPresentation (PPPresentation viewType slides) = Elt "PPPresentation" [] $ [toXMLBool viewType] ++ toXMLList_Slide slides
toXMLPPPresentation (HolePPPresentation) = Elt "HolePPPresentation" [] $ []
toXMLPPPresentation (ParseErrPPPresentation presentation) = Elt "ParseErrPPPresentation" [] []
toXMLSlide (Slide title itemList) = Elt "Slide" [] $ [toXMLString title] ++ [toXMLItemList itemList]
toXMLSlide (HoleSlide) = Elt "HoleSlide" [] $ []
toXMLSlide (ParseErrSlide presentation) = Elt "ParseErrSlide" [] []
toXMLItemList (ItemList listType items) = Elt "ItemList" [] $ [toXMLListType listType] ++ toXMLList_Item items
toXMLItemList (HoleItemList) = Elt "HoleItemList" [] $ []
toXMLItemList (ParseErrItemList presentation) = Elt "ParseErrItemList" [] []
toXMLListType (Bullet) = Elt "Bullet" [] $ []
toXMLListType (Number) = Elt "Number" [] $ []
toXMLListType (Alpha) = Elt "Alpha" [] $ []
toXMLListType (HoleListType) = Elt "HoleListType" [] $ []
toXMLListType (ParseErrListType presentation) = Elt "ParseErrListType" [] []
toXMLItem (StringItem string) = Elt "StringItem" [] $ [toXMLString string]
toXMLItem (HeliumItem exp) = Elt "HeliumItem" [] $ [toXMLExp exp]
toXMLItem (ListItem itemList) = Elt "ListItem" [] $ [toXMLItemList itemList]
toXMLItem (HoleItem) = Elt "HoleItem" [] $ []
toXMLItem (ParseErrItem presentation) = Elt "ParseErrItem" [] []
toXMLList_Decl (List_Decl xs) = toXMLConsList_Decl xs
toXMLList_Decl HoleList_Decl = []
toXMLList_Decl (ParseErrList_Decl _) = []
toXMLList_Alt (List_Alt xs) = toXMLConsList_Alt xs
toXMLList_Alt HoleList_Alt = []
toXMLList_Alt (ParseErrList_Alt _) = []
toXMLList_Exp (List_Exp xs) = toXMLConsList_Exp xs
toXMLList_Exp HoleList_Exp = []
toXMLList_Exp (ParseErrList_Exp _) = []
toXMLList_Slide (List_Slide xs) = toXMLConsList_Slide xs
toXMLList_Slide HoleList_Slide = []
toXMLList_Slide (ParseErrList_Slide _) = []
toXMLList_Item (List_Item xs) = toXMLConsList_Item xs
toXMLList_Item HoleList_Item = []
toXMLList_Item (ParseErrList_Item _) = []
toXMLConsList_Decl (Cons_Decl x xs) = toXMLDecl x : toXMLConsList_Decl xs
toXMLConsList_Decl Nil_Decl             = []
toXMLConsList_Alt (Cons_Alt x xs) = toXMLAlt x : toXMLConsList_Alt xs
toXMLConsList_Alt Nil_Alt             = []
toXMLConsList_Exp (Cons_Exp x xs) = toXMLExp x : toXMLConsList_Exp xs
toXMLConsList_Exp Nil_Exp             = []
toXMLConsList_Slide (Cons_Slide x xs) = toXMLSlide x : toXMLConsList_Slide xs
toXMLConsList_Slide Nil_Slide             = []
toXMLConsList_Item (Cons_Item x xs) = toXMLItem x : toXMLConsList_Item xs
toXMLConsList_Item Nil_Item             = []



--------------------------------------------------------------------------
-- parseXML functions                                                   --
--------------------------------------------------------------------------

parseXML_Dummy = parseXMLCns_Dummy <?|> parseHoleAndParseErr "Dummy" HoleDummy
parseXMLCns_Dummy = Dummy <$ startTag "Dummy" <*> parseXML_Root <*> parseXML_Dummy<* endTag "Dummy"
parseXML_EnrichedDoc = parseXMLCns_RootEnr <?|> parseHoleAndParseErr "EnrichedDoc" HoleEnrichedDoc
parseXMLCns_RootEnr = RootEnr <$ startTag "RootEnr" <*> parseXML_RootE <*> parseXML_HeliumTypeInfo <*> parseXML_Document<* endTag "RootEnr"
parseXML_Root = parseXMLCns_Root <?|> parseHoleAndParseErr "Root" HoleRoot
parseXMLCns_Root = Root NoIDP <$ startTag "Root" <*> parseXML_List_Decl<* endTag "Root"
parseXML_RootE = parseXMLCns_RootE <?|> parseHoleAndParseErr "RootE" HoleRootE
parseXMLCns_RootE = RootE NoIDP <$ startTag "RootE" <*> parseXML_List_Decl <*> parseXML_List_Decl<* endTag "RootE"
parseXML_Decl = parseXMLCns_Decl <?|> parseXMLCns_BoardDecl <?|> parseXMLCns_PPPresentationDecl <?|> parseHoleAndParseErr "Decl" HoleDecl
parseXMLCns_Decl = Decl NoIDP NoIDP NoIDP NoIDP <$ startTag "Decl" <*> parseXML_Bool <*> parseXML_Bool <*> parseXML_Ident <*> parseXML_Exp<* endTag "Decl"
parseXMLCns_BoardDecl = BoardDecl NoIDP NoIDP <$ startTag "BoardDecl" <*> parseXML_Board<* endTag "BoardDecl"
parseXMLCns_PPPresentationDecl = PPPresentationDecl NoIDP NoIDP <$ startTag "PPPresentationDecl" <*> parseXML_PPPresentation<* endTag "PPPresentationDecl"
parseXML_Ident = parseXMLCns_Ident <?|> parseHoleAndParseErr "Ident" HoleIdent
parseXMLCns_Ident = Ident NoIDP NoIDP <$ startTag "Ident" <*> parseXML_String<* endTag "Ident"
parseXML_Exp = parseXMLCns_PlusExp <?|> parseXMLCns_TimesExp <?|> parseXMLCns_DivExp <?|> parseXMLCns_PowerExp <?|> parseXMLCns_BoolExp <?|> parseXMLCns_IntExp <?|> parseXMLCns_LamExp <?|> parseXMLCns_AppExp <?|> parseXMLCns_CaseExp <?|> parseXMLCns_LetExp <?|> parseXMLCns_IdentExp <?|> parseXMLCns_IfExp <?|> parseXMLCns_ParenExp <?|> parseXMLCns_ListExp <?|> parseXMLCns_ProductExp <?|> parseHoleAndParseErr "Exp" HoleExp
parseXMLCns_PlusExp = PlusExp NoIDP <$ startTag "PlusExp" <*> parseXML_Exp <*> parseXML_Exp<* endTag "PlusExp"
parseXMLCns_TimesExp = TimesExp NoIDP <$ startTag "TimesExp" <*> parseXML_Exp <*> parseXML_Exp<* endTag "TimesExp"
parseXMLCns_DivExp = DivExp NoIDP <$ startTag "DivExp" <*> parseXML_Exp <*> parseXML_Exp<* endTag "DivExp"
parseXMLCns_PowerExp = PowerExp NoIDP <$ startTag "PowerExp" <*> parseXML_Exp <*> parseXML_Exp<* endTag "PowerExp"
parseXMLCns_BoolExp = BoolExp NoIDP <$ startTag "BoolExp" <*> parseXML_Bool<* endTag "BoolExp"
parseXMLCns_IntExp = IntExp NoIDP <$ startTag "IntExp" <*> parseXML_Int<* endTag "IntExp"
parseXMLCns_LamExp = LamExp NoIDP NoIDP <$ startTag "LamExp" <*> parseXML_Ident <*> parseXML_Exp<* endTag "LamExp"
parseXMLCns_AppExp = AppExp <$ startTag "AppExp" <*> parseXML_Exp <*> parseXML_Exp<* endTag "AppExp"
parseXMLCns_CaseExp = CaseExp NoIDP NoIDP <$ startTag "CaseExp" <*> parseXML_Exp <*> parseXML_List_Alt<* endTag "CaseExp"
parseXMLCns_LetExp = LetExp NoIDP NoIDP <$ startTag "LetExp" <*> parseXML_List_Decl <*> parseXML_Exp<* endTag "LetExp"
parseXMLCns_IdentExp = IdentExp <$ startTag "IdentExp" <*> parseXML_Ident<* endTag "IdentExp"
parseXMLCns_IfExp = IfExp NoIDP NoIDP NoIDP <$ startTag "IfExp" <*> parseXML_Exp <*> parseXML_Exp <*> parseXML_Exp<* endTag "IfExp"
parseXMLCns_ParenExp = ParenExp NoIDP NoIDP <$ startTag "ParenExp" <*> parseXML_Exp<* endTag "ParenExp"
parseXMLCns_ListExp = ListExp NoIDP NoIDP [] <$ startTag "ListExp" <*> parseXML_List_Exp<* endTag "ListExp"
parseXMLCns_ProductExp = ProductExp NoIDP NoIDP [] <$ startTag "ProductExp" <*> parseXML_List_Exp<* endTag "ProductExp"
parseXML_Alt = parseXMLCns_Alt <?|> parseHoleAndParseErr "Alt" HoleAlt
parseXMLCns_Alt = Alt NoIDP NoIDP <$ startTag "Alt" <*> parseXML_Ident <*> parseXML_Exp<* endTag "Alt"
parseXML_Board = parseXMLCns_Board <?|> parseHoleAndParseErr "Board" HoleBoard
parseXMLCns_Board = Board <$ startTag "Board" <*> parseXML_BoardRow <*> parseXML_BoardRow <*> parseXML_BoardRow <*> parseXML_BoardRow <*> parseXML_BoardRow <*> parseXML_BoardRow <*> parseXML_BoardRow <*> parseXML_BoardRow<* endTag "Board"
parseXML_BoardRow = parseXMLCns_BoardRow <?|> parseHoleAndParseErr "BoardRow" HoleBoardRow
parseXMLCns_BoardRow = BoardRow <$ startTag "BoardRow" <*> parseXML_BoardSquare <*> parseXML_BoardSquare <*> parseXML_BoardSquare <*> parseXML_BoardSquare <*> parseXML_BoardSquare <*> parseXML_BoardSquare <*> parseXML_BoardSquare <*> parseXML_BoardSquare<* endTag "BoardRow"
parseXML_BoardSquare = parseXMLCns_Queen <?|> parseXMLCns_King <?|> parseXMLCns_Bishop <?|> parseXMLCns_Knight <?|> parseXMLCns_Rook <?|> parseXMLCns_Pawn <?|> parseXMLCns_Empty <?|> parseHoleAndParseErr "BoardSquare" HoleBoardSquare
parseXMLCns_Queen = Queen <$ startTag "Queen" <*> parseXML_Bool<* endTag "Queen"
parseXMLCns_King = King <$ startTag "King" <*> parseXML_Bool<* endTag "King"
parseXMLCns_Bishop = Bishop <$ startTag "Bishop" <*> parseXML_Bool<* endTag "Bishop"
parseXMLCns_Knight = Knight <$ startTag "Knight" <*> parseXML_Bool<* endTag "Knight"
parseXMLCns_Rook = Rook <$ startTag "Rook" <*> parseXML_Bool<* endTag "Rook"
parseXMLCns_Pawn = Pawn <$ startTag "Pawn" <*> parseXML_Bool<* endTag "Pawn"
parseXMLCns_Empty = Empty <$ emptyTag "Empty"
parseXML_PPPresentation = parseXMLCns_PPPresentation <?|> parseHoleAndParseErr "PPPresentation" HolePPPresentation
parseXMLCns_PPPresentation = PPPresentation <$ startTag "PPPresentation" <*> parseXML_Bool <*> parseXML_List_Slide<* endTag "PPPresentation"
parseXML_Slide = parseXMLCns_Slide <?|> parseHoleAndParseErr "Slide" HoleSlide
parseXMLCns_Slide = Slide <$ startTag "Slide" <*> parseXML_String <*> parseXML_ItemList<* endTag "Slide"
parseXML_ItemList = parseXMLCns_ItemList <?|> parseHoleAndParseErr "ItemList" HoleItemList
parseXMLCns_ItemList = ItemList <$ startTag "ItemList" <*> parseXML_ListType <*> parseXML_List_Item<* endTag "ItemList"
parseXML_ListType = parseXMLCns_Bullet <?|> parseXMLCns_Number <?|> parseXMLCns_Alpha <?|> parseHoleAndParseErr "ListType" HoleListType
parseXMLCns_Bullet = Bullet <$ emptyTag "Bullet"
parseXMLCns_Number = Number <$ emptyTag "Number"
parseXMLCns_Alpha = Alpha <$ emptyTag "Alpha"
parseXML_Item = parseXMLCns_StringItem <?|> parseXMLCns_HeliumItem <?|> parseXMLCns_ListItem <?|> parseHoleAndParseErr "Item" HoleItem
parseXMLCns_StringItem = StringItem <$ startTag "StringItem" <*> parseXML_String<* endTag "StringItem"
parseXMLCns_HeliumItem = HeliumItem <$ startTag "HeliumItem" <*> parseXML_Exp<* endTag "HeliumItem"
parseXMLCns_ListItem = ListItem <$ startTag "ListItem" <*> parseXML_ItemList<* endTag "ListItem"
parseXML_List_Decl = mkList List_Decl Cons_Decl Nil_Decl <$> many parseXML_Decl
parseXML_List_Alt = mkList List_Alt Cons_Alt Nil_Alt <$> many parseXML_Alt
parseXML_List_Exp = mkList List_Exp Cons_Exp Nil_Exp <$> many parseXML_Exp
parseXML_List_Slide = mkList List_Slide Cons_Slide Nil_Slide <$> many parseXML_Slide
parseXML_List_Item = mkList List_Item Cons_Item Nil_Item <$> many parseXML_Item


