module DocUtils_Generated where

import Evaluation.DocTypes
import DocTypes_Generated
import Evaluation.DocumentEdit
import Presentation.PresTypes
import Text.ParserCombinators.Parsec
import Evaluation.DocUtils
import Common.CommonTypes
import Common.CommonUtils
import System.Directory
import Presentation.XprezLib

initialDocument :: IO Document
initialDocument = 
 do { let filePath = "Heliumfile.hs"
    ; dir <- getCurrentDirectory
    ; debugLnIO Prs $ "InitDoc: opening file: "++"Proxima.hs"++ " at " ++dir  
    ; fileContents <- readFile filePath
    ; return $ RootDoc $ Root NoIDP $ ParseErrList_Decl (ColP NoIDP 0 NF . map (StringP NoIDP). lines' $ fileContents) {- [] -}
    }
    -- by putting the text in a parse error node, we don't need to specify a textual parser. Instead,
    -- the proxima parser is used when the presented document is parsed.


toXMLHeliumTypeInfo _ = Elt "HeliumTypeInfo" [] []
parseXML_HeliumTypeInfo = ([],[],[]) <$ emptyTag "HeliumTypeInfo"

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
  PPPresentation True $ toList_Slide $
      [ Slide "slide_1" $
          ItemList (Bullet) $ toList_Item $
                                [ StringItem "item_1"
                                , HeliumItem -- simple trick to use parser: Needs an additional parse (F1) though!
                                    --(ident "\\ x -> increaze x")
                                    (ident "\\b -> \\x -> if b then ink x else x")
                                , StringItem "item_2"
                                , ListItem listItem
                                ]
       , Slide "slide_2" $
          ItemList (Alpha) $ toList_Item $
                               [ StringItem "item_a"
                               , StringItem "item_b"
                               , StringItem "item_c"
                               ]
      ]
 where listItem = ItemList (Number) $ toList_Item $
                                        [ StringItem "nested_item_1"
                                        , ListItem listItem'
                                        , StringItem "nested_item_2"
                                        , StringItem "nested_item_3"
                                        ]
       listItem' = ItemList (Bullet) $ toList_Item $
                                         [ StringItem "nested_nested_item"
                                         , StringItem "nested_nested_item"
                                         , StringItem "nested_nested_item"
                                         ]
       dv e1 e2 = DivExp NoIDP e1 e2 
       lam str body = LamExp NoIDP NoIDP (Ident NoIDP NoIDP str) body
       ifxp c t e = IfExp NoIDP NoIDP NoIDP c t e 
       int i = IntExp NoIDP i
       bool b = BoolExp NoIDP b
       ident str = IdentExp (Ident NoIDP NoIDP str)
       


----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- rankNode                                                             --
--------------------------------------------------------------------------

rankNode :: Node -> Int
rankNode NoNode = 0
rankNode (Node_RootDoc _ _) = 1
rankNode (Node_HoleDocument _ _) = 2
rankNode (Node_ParseErrDocument _ _) = 3
rankNode (Node_Dummy _ _) = 4
rankNode (Node_HoleDummy _ _) = 5
rankNode (Node_ParseErrDummy _ _) = 6
rankNode (Node_RootEnr _ _) = 7
rankNode (Node_HoleEnrichedDoc _ _) = 8
rankNode (Node_ParseErrEnrichedDoc _ _) = 9
rankNode (Node_Root _ _) = 10
rankNode (Node_HoleRoot _ _) = 11
rankNode (Node_ParseErrRoot _ _) = 12
rankNode (Node_RootE _ _) = 13
rankNode (Node_HoleRootE _ _) = 14
rankNode (Node_ParseErrRootE _ _) = 15
rankNode (Node_Decl _ _) = 16
rankNode (Node_BoardDecl _ _) = 17
rankNode (Node_PPPresentationDecl _ _) = 18
rankNode (Node_HoleDecl _ _) = 19
rankNode (Node_ParseErrDecl _ _) = 20
rankNode (Node_Ident _ _) = 21
rankNode (Node_HoleIdent _ _) = 22
rankNode (Node_ParseErrIdent _ _) = 23
rankNode (Node_PlusExp _ _) = 24
rankNode (Node_TimesExp _ _) = 25
rankNode (Node_DivExp _ _) = 26
rankNode (Node_PowerExp _ _) = 27
rankNode (Node_BoolExp _ _) = 28
rankNode (Node_IntExp _ _) = 29
rankNode (Node_LamExp _ _) = 30
rankNode (Node_AppExp _ _) = 31
rankNode (Node_CaseExp _ _) = 32
rankNode (Node_LetExp _ _) = 33
rankNode (Node_IdentExp _ _) = 34
rankNode (Node_IfExp _ _) = 35
rankNode (Node_ParenExp _ _) = 36
rankNode (Node_ListExp _ _) = 37
rankNode (Node_ProductExp _ _) = 38
rankNode (Node_HoleExp _ _) = 39
rankNode (Node_ParseErrExp _ _) = 40
rankNode (Node_Alt _ _) = 41
rankNode (Node_HoleAlt _ _) = 42
rankNode (Node_ParseErrAlt _ _) = 43
rankNode (Node_Board _ _) = 44
rankNode (Node_HoleBoard _ _) = 45
rankNode (Node_ParseErrBoard _ _) = 46
rankNode (Node_BoardRow _ _) = 47
rankNode (Node_HoleBoardRow _ _) = 48
rankNode (Node_ParseErrBoardRow _ _) = 49
rankNode (Node_Queen _ _) = 50
rankNode (Node_King _ _) = 51
rankNode (Node_Bishop _ _) = 52
rankNode (Node_Knight _ _) = 53
rankNode (Node_Rook _ _) = 54
rankNode (Node_Pawn _ _) = 55
rankNode (Node_Empty _ _) = 56
rankNode (Node_HoleBoardSquare _ _) = 57
rankNode (Node_ParseErrBoardSquare _ _) = 58
rankNode (Node_PPPresentation _ _) = 59
rankNode (Node_HolePPPresentation _ _) = 60
rankNode (Node_ParseErrPPPresentation _ _) = 61
rankNode (Node_Slide _ _) = 62
rankNode (Node_HoleSlide _ _) = 63
rankNode (Node_ParseErrSlide _ _) = 64
rankNode (Node_ItemList _ _) = 65
rankNode (Node_HoleItemList _ _) = 66
rankNode (Node_ParseErrItemList _ _) = 67
rankNode (Node_Bullet _ _) = 68
rankNode (Node_Number _ _) = 69
rankNode (Node_Alpha _ _) = 70
rankNode (Node_HoleListType _ _) = 71
rankNode (Node_ParseErrListType _ _) = 72
rankNode (Node_StringItem _ _) = 73
rankNode (Node_HeliumItem _ _) = 74
rankNode (Node_ListItem _ _) = 75
rankNode (Node_HoleItem _ _) = 76
rankNode (Node_ParseErrItem _ _) = 77
rankNode (Node_List_Decl _ _) = 78
rankNode (Node_HoleList_Decl _ _) = 79
rankNode (Node_ParseErrList_Decl _ _) = 80
rankNode (Node_List_Alt _ _) = 81
rankNode (Node_HoleList_Alt _ _) = 82
rankNode (Node_ParseErrList_Alt _ _) = 83
rankNode (Node_List_Exp _ _) = 84
rankNode (Node_HoleList_Exp _ _) = 85
rankNode (Node_ParseErrList_Exp _ _) = 86
rankNode (Node_List_Slide _ _) = 87
rankNode (Node_HoleList_Slide _ _) = 88
rankNode (Node_ParseErrList_Slide _ _) = 89
rankNode (Node_List_Item _ _) = 90
rankNode (Node_HoleList_Item _ _) = 91
rankNode (Node_ParseErrList_Item _ _) = 92



--------------------------------------------------------------------------
-- HasPath instance for Node                                            --
--------------------------------------------------------------------------

instance HasPath Node where
  pathNode NoNode            = NoPathD
  pathNode (Node_RootDoc _ pth) = PathD pth
  pathNode (Node_HoleDocument _ pth) = PathD pth
  pathNode (Node_ParseErrDocument _ pth) = PathD pth
  pathNode (Node_Dummy _ pth) = PathD pth
  pathNode (Node_HoleDummy _ pth) = PathD pth
  pathNode (Node_ParseErrDummy _ pth) = PathD pth
  pathNode (Node_RootEnr _ pth) = PathD pth
  pathNode (Node_HoleEnrichedDoc _ pth) = PathD pth
  pathNode (Node_ParseErrEnrichedDoc _ pth) = PathD pth
  pathNode (Node_Root _ pth) = PathD pth
  pathNode (Node_HoleRoot _ pth) = PathD pth
  pathNode (Node_ParseErrRoot _ pth) = PathD pth
  pathNode (Node_RootE _ pth) = PathD pth
  pathNode (Node_HoleRootE _ pth) = PathD pth
  pathNode (Node_ParseErrRootE _ pth) = PathD pth
  pathNode (Node_Decl _ pth) = PathD pth
  pathNode (Node_BoardDecl _ pth) = PathD pth
  pathNode (Node_PPPresentationDecl _ pth) = PathD pth
  pathNode (Node_HoleDecl _ pth) = PathD pth
  pathNode (Node_ParseErrDecl _ pth) = PathD pth
  pathNode (Node_Ident _ pth) = PathD pth
  pathNode (Node_HoleIdent _ pth) = PathD pth
  pathNode (Node_ParseErrIdent _ pth) = PathD pth
  pathNode (Node_PlusExp _ pth) = PathD pth
  pathNode (Node_TimesExp _ pth) = PathD pth
  pathNode (Node_DivExp _ pth) = PathD pth
  pathNode (Node_PowerExp _ pth) = PathD pth
  pathNode (Node_BoolExp _ pth) = PathD pth
  pathNode (Node_IntExp _ pth) = PathD pth
  pathNode (Node_LamExp _ pth) = PathD pth
  pathNode (Node_AppExp _ pth) = PathD pth
  pathNode (Node_CaseExp _ pth) = PathD pth
  pathNode (Node_LetExp _ pth) = PathD pth
  pathNode (Node_IdentExp _ pth) = PathD pth
  pathNode (Node_IfExp _ pth) = PathD pth
  pathNode (Node_ParenExp _ pth) = PathD pth
  pathNode (Node_ListExp _ pth) = PathD pth
  pathNode (Node_ProductExp _ pth) = PathD pth
  pathNode (Node_HoleExp _ pth) = PathD pth
  pathNode (Node_ParseErrExp _ pth) = PathD pth
  pathNode (Node_Alt _ pth) = PathD pth
  pathNode (Node_HoleAlt _ pth) = PathD pth
  pathNode (Node_ParseErrAlt _ pth) = PathD pth
  pathNode (Node_Board _ pth) = PathD pth
  pathNode (Node_HoleBoard _ pth) = PathD pth
  pathNode (Node_ParseErrBoard _ pth) = PathD pth
  pathNode (Node_BoardRow _ pth) = PathD pth
  pathNode (Node_HoleBoardRow _ pth) = PathD pth
  pathNode (Node_ParseErrBoardRow _ pth) = PathD pth
  pathNode (Node_Queen _ pth) = PathD pth
  pathNode (Node_King _ pth) = PathD pth
  pathNode (Node_Bishop _ pth) = PathD pth
  pathNode (Node_Knight _ pth) = PathD pth
  pathNode (Node_Rook _ pth) = PathD pth
  pathNode (Node_Pawn _ pth) = PathD pth
  pathNode (Node_Empty _ pth) = PathD pth
  pathNode (Node_HoleBoardSquare _ pth) = PathD pth
  pathNode (Node_ParseErrBoardSquare _ pth) = PathD pth
  pathNode (Node_PPPresentation _ pth) = PathD pth
  pathNode (Node_HolePPPresentation _ pth) = PathD pth
  pathNode (Node_ParseErrPPPresentation _ pth) = PathD pth
  pathNode (Node_Slide _ pth) = PathD pth
  pathNode (Node_HoleSlide _ pth) = PathD pth
  pathNode (Node_ParseErrSlide _ pth) = PathD pth
  pathNode (Node_ItemList _ pth) = PathD pth
  pathNode (Node_HoleItemList _ pth) = PathD pth
  pathNode (Node_ParseErrItemList _ pth) = PathD pth
  pathNode (Node_Bullet _ pth) = PathD pth
  pathNode (Node_Number _ pth) = PathD pth
  pathNode (Node_Alpha _ pth) = PathD pth
  pathNode (Node_HoleListType _ pth) = PathD pth
  pathNode (Node_ParseErrListType _ pth) = PathD pth
  pathNode (Node_StringItem _ pth) = PathD pth
  pathNode (Node_HeliumItem _ pth) = PathD pth
  pathNode (Node_ListItem _ pth) = PathD pth
  pathNode (Node_HoleItem _ pth) = PathD pth
  pathNode (Node_ParseErrItem _ pth) = PathD pth
  pathNode (Node_List_Decl _ pth) = PathD pth
  pathNode (Node_HoleList_Decl _ pth) = PathD pth
  pathNode (Node_ParseErrList_Decl _ pth) = PathD pth
  pathNode (Node_List_Alt _ pth) = PathD pth
  pathNode (Node_HoleList_Alt _ pth) = PathD pth
  pathNode (Node_ParseErrList_Alt _ pth) = PathD pth
  pathNode (Node_List_Exp _ pth) = PathD pth
  pathNode (Node_HoleList_Exp _ pth) = PathD pth
  pathNode (Node_ParseErrList_Exp _ pth) = PathD pth
  pathNode (Node_List_Slide _ pth) = PathD pth
  pathNode (Node_HoleList_Slide _ pth) = PathD pth
  pathNode (Node_ParseErrList_Slide _ pth) = PathD pth
  pathNode (Node_List_Item _ pth) = PathD pth
  pathNode (Node_HoleList_Item _ pth) = PathD pth
  pathNode (Node_ParseErrList_Item _ pth) = PathD pth



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



--------------------------------------------------------------------------
-- List utility functions                                               --
--------------------------------------------------------------------------

toList_Decl vs = List_Decl (toConsList_Decl vs)

fromList_Decl (List_Decl vs) = fromConsList_Decl vs
fromList_Decl _ = []

toConsList_Decl [] = Nil_Decl
toConsList_Decl (x:xs) = Cons_Decl x (toConsList_Decl xs)

fromConsList_Decl Nil_Decl = []
fromConsList_Decl (Cons_Decl x xs) = x: fromConsList_Decl xs

replaceList_Decl _ x Nil_Decl = Nil_Decl  -- replace beyond end of list
replaceList_Decl 0 x (Cons_Decl cx cxs) = Cons_Decl x cxs
replaceList_Decl n x (Cons_Decl cx cxs) = Cons_Decl cx (replaceList_Decl (n-1) x cxs)

insertList_Decl 0 x cxs = Cons_Decl x cxs
insertList_Decl _ x Nil_Decl  = Nil_Decl  -- insert beyond end of list
insertList_Decl n x (Cons_Decl cx cxs) = Cons_Decl cx (insertList_Decl (n-1) x cxs)

removeList_Decl _ Nil_Decl  = Nil_Decl  -- remove beyond end of list
removeList_Decl 0 (Cons_Decl cx cxs) = cxs
removeList_Decl n (Cons_Decl cx cxs) = Cons_Decl cx (removeList_Decl (n-1) cxs)

toList_Alt vs = List_Alt (toConsList_Alt vs)

fromList_Alt (List_Alt vs) = fromConsList_Alt vs
fromList_Alt _ = []

toConsList_Alt [] = Nil_Alt
toConsList_Alt (x:xs) = Cons_Alt x (toConsList_Alt xs)

fromConsList_Alt Nil_Alt = []
fromConsList_Alt (Cons_Alt x xs) = x: fromConsList_Alt xs

replaceList_Alt _ x Nil_Alt = Nil_Alt  -- replace beyond end of list
replaceList_Alt 0 x (Cons_Alt cx cxs) = Cons_Alt x cxs
replaceList_Alt n x (Cons_Alt cx cxs) = Cons_Alt cx (replaceList_Alt (n-1) x cxs)

insertList_Alt 0 x cxs = Cons_Alt x cxs
insertList_Alt _ x Nil_Alt  = Nil_Alt  -- insert beyond end of list
insertList_Alt n x (Cons_Alt cx cxs) = Cons_Alt cx (insertList_Alt (n-1) x cxs)

removeList_Alt _ Nil_Alt  = Nil_Alt  -- remove beyond end of list
removeList_Alt 0 (Cons_Alt cx cxs) = cxs
removeList_Alt n (Cons_Alt cx cxs) = Cons_Alt cx (removeList_Alt (n-1) cxs)

toList_Exp vs = List_Exp (toConsList_Exp vs)

fromList_Exp (List_Exp vs) = fromConsList_Exp vs
fromList_Exp _ = []

toConsList_Exp [] = Nil_Exp
toConsList_Exp (x:xs) = Cons_Exp x (toConsList_Exp xs)

fromConsList_Exp Nil_Exp = []
fromConsList_Exp (Cons_Exp x xs) = x: fromConsList_Exp xs

replaceList_Exp _ x Nil_Exp = Nil_Exp  -- replace beyond end of list
replaceList_Exp 0 x (Cons_Exp cx cxs) = Cons_Exp x cxs
replaceList_Exp n x (Cons_Exp cx cxs) = Cons_Exp cx (replaceList_Exp (n-1) x cxs)

insertList_Exp 0 x cxs = Cons_Exp x cxs
insertList_Exp _ x Nil_Exp  = Nil_Exp  -- insert beyond end of list
insertList_Exp n x (Cons_Exp cx cxs) = Cons_Exp cx (insertList_Exp (n-1) x cxs)

removeList_Exp _ Nil_Exp  = Nil_Exp  -- remove beyond end of list
removeList_Exp 0 (Cons_Exp cx cxs) = cxs
removeList_Exp n (Cons_Exp cx cxs) = Cons_Exp cx (removeList_Exp (n-1) cxs)

toList_Slide vs = List_Slide (toConsList_Slide vs)

fromList_Slide (List_Slide vs) = fromConsList_Slide vs
fromList_Slide _ = []

toConsList_Slide [] = Nil_Slide
toConsList_Slide (x:xs) = Cons_Slide x (toConsList_Slide xs)

fromConsList_Slide Nil_Slide = []
fromConsList_Slide (Cons_Slide x xs) = x: fromConsList_Slide xs

replaceList_Slide _ x Nil_Slide = Nil_Slide  -- replace beyond end of list
replaceList_Slide 0 x (Cons_Slide cx cxs) = Cons_Slide x cxs
replaceList_Slide n x (Cons_Slide cx cxs) = Cons_Slide cx (replaceList_Slide (n-1) x cxs)

insertList_Slide 0 x cxs = Cons_Slide x cxs
insertList_Slide _ x Nil_Slide  = Nil_Slide  -- insert beyond end of list
insertList_Slide n x (Cons_Slide cx cxs) = Cons_Slide cx (insertList_Slide (n-1) x cxs)

removeList_Slide _ Nil_Slide  = Nil_Slide  -- remove beyond end of list
removeList_Slide 0 (Cons_Slide cx cxs) = cxs
removeList_Slide n (Cons_Slide cx cxs) = Cons_Slide cx (removeList_Slide (n-1) cxs)

toList_Item vs = List_Item (toConsList_Item vs)

fromList_Item (List_Item vs) = fromConsList_Item vs
fromList_Item _ = []

toConsList_Item [] = Nil_Item
toConsList_Item (x:xs) = Cons_Item x (toConsList_Item xs)

fromConsList_Item Nil_Item = []
fromConsList_Item (Cons_Item x xs) = x: fromConsList_Item xs

replaceList_Item _ x Nil_Item = Nil_Item  -- replace beyond end of list
replaceList_Item 0 x (Cons_Item cx cxs) = Cons_Item x cxs
replaceList_Item n x (Cons_Item cx cxs) = Cons_Item cx (replaceList_Item (n-1) x cxs)

insertList_Item 0 x cxs = Cons_Item x cxs
insertList_Item _ x Nil_Item  = Nil_Item  -- insert beyond end of list
insertList_Item n x (Cons_Item cx cxs) = Cons_Item cx (insertList_Item (n-1) x cxs)

removeList_Item _ Nil_Item  = Nil_Item  -- remove beyond end of list
removeList_Item 0 (Cons_Item cx cxs) = cxs
removeList_Item n (Cons_Item cx cxs) = Cons_Item cx (removeList_Item (n-1) cxs)




--------------------------------------------------------------------------
-- Miscellaneous                                                        --
--------------------------------------------------------------------------

type Presentation_Doc_Node_Clip_Token = Presentation Document Node ClipDoc UserToken

instance Doc Document where
  initialDoc = initialDocument
  toXML = toXMLDocument
  parseXML = parseXML_Document

instance DocNode Node where
  noNode = NoNode

instance Eq Node where
  nd1 == nd2 = rankNode nd1 == rankNode nd2
  
instance Ord Node where
  nd1 <= nd2 = rankNode nd1 <= rankNode nd2


-- toXML for Document and primitive types

-- we don't put a "RootDoc" element in the XML, because this type is not visible to the user.
toXMLDocument (RootDoc root) = toXMLRoot root
toXMLDocument _              = debug Err "DocUtils_Generated.toXMLDocument: malformed Document" $
                                 Elt "Root" [] [] -- this does not occur

toXMLInt i = Elt "Integer" [("val", show i)] []

toXMLInt f = Elt "Float" [("val", show f)] []

toXMLBool b = Elt "Bool" [("val", show b)] []

toXMLString str = Elt "String" [] [PCData str] 


-- parseXML for Document and primitive types

parseXML_Document = RootDoc <$> parseXML_Root

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


