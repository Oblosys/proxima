module ProxParser_Generated where

import CommonTypes
import PresLayerTypes
import PresLayerUtils

import DocumentEdit


----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

{- ------------------------------------

 generated part

-------------------------------------- -}






-- ProxParser_Generated --

-- Type specific
reuseRootEnr :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe List_Decl -> Maybe List_Decl -> Maybe HeliumTypeInfo -> Maybe Document -> EnrichedDoc
reuseRootEnr nodes  ma0 ma1 ma2 ma3 ma4 ma5
  = case extractFromNodes extractRootEnr defaultRootEnr nodes of
           (RootEnr a0 a1 a2 a3 a4 a5) -> reuse6 RootEnr a0 a1 a2 a3 a4 a5 ma0 ma1 ma2 ma3 ma4 ma5
           _ -> error "System error:<module>.reuseRootEnr"

reuseDecl :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe IDP -> Maybe IDP -> Maybe IDP -> Maybe Bool -> Maybe Bool -> Maybe Ident -> Maybe Exp -> Decl
reuseDecl nodes  ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8
  = case extractFromNodes extractDecl defaultDecl nodes of
           (Decl a0 a1 a2 a3 a4 a5 a6 a7 a8) -> reuse9 Decl a0 a1 a2 a3 a4 a5 a6 a7 a8 ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8
           _ -> error "System error:<module>.reuseDecl"

reuseBoardDecl :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe IDP -> Maybe Board -> Decl
reuseBoardDecl nodes  ma0 ma1 ma2 ma3
  = case extractFromNodes extractBoardDecl defaultBoardDecl nodes of
           (BoardDecl a0 a1 a2 a3) -> reuse4 BoardDecl a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseBoardDecl"

reusePPPresentationDecl :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe IDP -> Maybe PPPresentation -> Decl
reusePPPresentationDecl nodes  ma0 ma1 ma2 ma3
  = case extractFromNodes extractPPPresentationDecl defaultPPPresentationDecl nodes of
           (PPPresentationDecl a0 a1 a2 a3) -> reuse4 PPPresentationDecl a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reusePPPresentationDecl"

reuseIdent :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe IDP -> Maybe String -> Ident
reuseIdent nodes  ma0 ma1 ma2 ma3
  = case extractFromNodes extractIdent defaultIdent nodes of
           (Ident a0 a1 a2 a3) -> reuse4 Ident a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseIdent"

reusePlusExp :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe Exp -> Maybe Exp -> Exp
reusePlusExp nodes  ma0 ma1 ma2 ma3
  = case extractFromNodes extractPlusExp defaultPlusExp nodes of
           (PlusExp a0 a1 a2 a3) -> reuse4 PlusExp a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reusePlusExp"

reuseTimesExp :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe Exp -> Maybe Exp -> Exp
reuseTimesExp nodes  ma0 ma1 ma2 ma3
  = case extractFromNodes extractTimesExp defaultTimesExp nodes of
           (TimesExp a0 a1 a2 a3) -> reuse4 TimesExp a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseTimesExp"

reuseDivExp :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe Exp -> Maybe Exp -> Exp
reuseDivExp nodes  ma0 ma1 ma2 ma3
  = case extractFromNodes extractDivExp defaultDivExp nodes of
           (DivExp a0 a1 a2 a3) -> reuse4 DivExp a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseDivExp"

reusePowerExp :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe Exp -> Maybe Exp -> Exp
reusePowerExp nodes  ma0 ma1 ma2 ma3
  = case extractFromNodes extractPowerExp defaultPowerExp nodes of
           (PowerExp a0 a1 a2 a3) -> reuse4 PowerExp a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reusePowerExp"

reuseBoolExp :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe Bool -> Exp
reuseBoolExp nodes  ma0 ma1 ma2
  = case extractFromNodes extractBoolExp defaultBoolExp nodes of
           (BoolExp a0 a1 a2) -> reuse3 BoolExp a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseBoolExp"

reuseIntExp :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe Int -> Exp
reuseIntExp nodes  ma0 ma1 ma2
  = case extractFromNodes extractIntExp defaultIntExp nodes of
           (IntExp a0 a1 a2) -> reuse3 IntExp a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseIntExp"

reuseLamExp :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe IDP -> Maybe Ident -> Maybe Exp -> Exp
reuseLamExp nodes  ma0 ma1 ma2 ma3 ma4
  = case extractFromNodes extractLamExp defaultLamExp nodes of
           (LamExp a0 a1 a2 a3 a4) -> reuse5 LamExp a0 a1 a2 a3 a4 ma0 ma1 ma2 ma3 ma4
           _ -> error "System error:<module>.reuseLamExp"

reuseAppExp :: [Maybe Node] -> Maybe IDD -> Maybe Exp -> Maybe Exp -> Exp
reuseAppExp nodes  ma0 ma1 ma2
  = case extractFromNodes extractAppExp defaultAppExp nodes of
           (AppExp a0 a1 a2) -> reuse3 AppExp a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseAppExp"

reuseCaseExp :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe IDP -> Maybe Exp -> Maybe List_Alt -> Exp
reuseCaseExp nodes  ma0 ma1 ma2 ma3 ma4
  = case extractFromNodes extractCaseExp defaultCaseExp nodes of
           (CaseExp a0 a1 a2 a3 a4) -> reuse5 CaseExp a0 a1 a2 a3 a4 ma0 ma1 ma2 ma3 ma4
           _ -> error "System error:<module>.reuseCaseExp"

reuseLetExp :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe IDP -> Maybe List_Decl -> Maybe Exp -> Exp
reuseLetExp nodes  ma0 ma1 ma2 ma3 ma4
  = case extractFromNodes extractLetExp defaultLetExp nodes of
           (LetExp a0 a1 a2 a3 a4) -> reuse5 LetExp a0 a1 a2 a3 a4 ma0 ma1 ma2 ma3 ma4
           _ -> error "System error:<module>.reuseLetExp"

reuseIdentExp :: [Maybe Node] -> Maybe IDD -> Maybe Ident -> Exp
reuseIdentExp nodes  ma0 ma1
  = case extractFromNodes extractIdentExp defaultIdentExp nodes of
           (IdentExp a0 a1) -> reuse2 IdentExp a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseIdentExp"

reuseIfExp :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe IDP -> Maybe IDP -> Maybe Exp -> Maybe Exp -> Maybe Exp -> Exp
reuseIfExp nodes  ma0 ma1 ma2 ma3 ma4 ma5 ma6
  = case extractFromNodes extractIfExp defaultIfExp nodes of
           (IfExp a0 a1 a2 a3 a4 a5 a6) -> reuse7 IfExp a0 a1 a2 a3 a4 a5 a6 ma0 ma1 ma2 ma3 ma4 ma5 ma6
           _ -> error "System error:<module>.reuseIfExp"

reuseParenExp :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe IDP -> Maybe Exp -> Exp
reuseParenExp nodes  ma0 ma1 ma2 ma3
  = case extractFromNodes extractParenExp defaultParenExp nodes of
           (ParenExp a0 a1 a2 a3) -> reuse4 ParenExp a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseParenExp"

reuseListExp :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe IDP -> Maybe [IDP]  -> Maybe List_Exp -> Exp
reuseListExp nodes  ma0 ma1 ma2 ma3 ma4
  = case extractFromNodes extractListExp defaultListExp nodes of
           (ListExp a0 a1 a2 a3 a4) -> reuse5 ListExp a0 a1 a2 a3 a4 ma0 ma1 ma2 ma3 ma4
           _ -> error "System error:<module>.reuseListExp"

reuseProductExp :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe IDP -> Maybe [IDP]  -> Maybe List_Exp -> Exp
reuseProductExp nodes  ma0 ma1 ma2 ma3 ma4
  = case extractFromNodes extractProductExp defaultProductExp nodes of
           (ProductExp a0 a1 a2 a3 a4) -> reuse5 ProductExp a0 a1 a2 a3 a4 ma0 ma1 ma2 ma3 ma4
           _ -> error "System error:<module>.reuseProductExp"

reuseAlt :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe IDP -> Maybe Ident -> Maybe Exp -> Alt
reuseAlt nodes  ma0 ma1 ma2 ma3 ma4
  = case extractFromNodes extractAlt defaultAlt nodes of
           (Alt a0 a1 a2 a3 a4) -> reuse5 Alt a0 a1 a2 a3 a4 ma0 ma1 ma2 ma3 ma4
           _ -> error "System error:<module>.reuseAlt"

reuseBoard :: [Maybe Node] -> Maybe IDD -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Board
reuseBoard nodes  ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8
  = case extractFromNodes extractBoard defaultBoard nodes of
           (Board a0 a1 a2 a3 a4 a5 a6 a7 a8) -> reuse9 Board a0 a1 a2 a3 a4 a5 a6 a7 a8 ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8
           _ -> error "System error:<module>.reuseBoard"

reuseBoardRow :: [Maybe Node] -> Maybe IDD -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> BoardRow
reuseBoardRow nodes  ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8
  = case extractFromNodes extractBoardRow defaultBoardRow nodes of
           (BoardRow a0 a1 a2 a3 a4 a5 a6 a7 a8) -> reuse9 BoardRow a0 a1 a2 a3 a4 a5 a6 a7 a8 ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8
           _ -> error "System error:<module>.reuseBoardRow"

reuseQueen :: [Maybe Node] -> Maybe IDD -> Maybe Bool -> BoardSquare
reuseQueen nodes  ma0 ma1
  = case extractFromNodes extractQueen defaultQueen nodes of
           (Queen a0 a1) -> reuse2 Queen a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseQueen"

reuseKing :: [Maybe Node] -> Maybe IDD -> Maybe Bool -> BoardSquare
reuseKing nodes  ma0 ma1
  = case extractFromNodes extractKing defaultKing nodes of
           (King a0 a1) -> reuse2 King a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseKing"

reuseBishop :: [Maybe Node] -> Maybe IDD -> Maybe Bool -> BoardSquare
reuseBishop nodes  ma0 ma1
  = case extractFromNodes extractBishop defaultBishop nodes of
           (Bishop a0 a1) -> reuse2 Bishop a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseBishop"

reuseKnight :: [Maybe Node] -> Maybe IDD -> Maybe Bool -> BoardSquare
reuseKnight nodes  ma0 ma1
  = case extractFromNodes extractKnight defaultKnight nodes of
           (Knight a0 a1) -> reuse2 Knight a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseKnight"

reuseRook :: [Maybe Node] -> Maybe IDD -> Maybe Bool -> BoardSquare
reuseRook nodes  ma0 ma1
  = case extractFromNodes extractRook defaultRook nodes of
           (Rook a0 a1) -> reuse2 Rook a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseRook"

reusePawn :: [Maybe Node] -> Maybe IDD -> Maybe Bool -> BoardSquare
reusePawn nodes  ma0 ma1
  = case extractFromNodes extractPawn defaultPawn nodes of
           (Pawn a0 a1) -> reuse2 Pawn a0 a1 ma0 ma1
           _ -> error "System error:<module>.reusePawn"

reuseEmpty :: [Maybe Node] -> BoardSquare
reuseEmpty nodes 
  = case extractFromNodes extractEmpty defaultEmpty nodes of
           (Empty) -> reuse0 Empty
           _ -> error "System error:<module>.reuseEmpty"

reusePPPresentation :: [Maybe Node] -> Maybe IDD -> Maybe Bool -> Maybe List_Slide -> PPPresentation
reusePPPresentation nodes  ma0 ma1 ma2
  = case extractFromNodes extractPPPresentation defaultPPPresentation nodes of
           (PPPresentation a0 a1 a2) -> reuse3 PPPresentation a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reusePPPresentation"

reuseSlide :: [Maybe Node] -> Maybe IDD -> Maybe String_ -> Maybe ItemList -> Slide
reuseSlide nodes  ma0 ma1 ma2
  = case extractFromNodes extractSlide defaultSlide nodes of
           (Slide a0 a1 a2) -> reuse3 Slide a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseSlide"

reuseItemList :: [Maybe Node] -> Maybe IDD -> Maybe ListType -> Maybe List_Item -> ItemList
reuseItemList nodes  ma0 ma1 ma2
  = case extractFromNodes extractItemList defaultItemList nodes of
           (ItemList a0 a1 a2) -> reuse3 ItemList a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseItemList"

reuseBullet :: [Maybe Node] -> Maybe IDD -> ListType
reuseBullet nodes  ma0
  = case extractFromNodes extractBullet defaultBullet nodes of
           (Bullet a0) -> reuse1 Bullet a0 ma0
           _ -> error "System error:<module>.reuseBullet"

reuseNumber :: [Maybe Node] -> Maybe IDD -> ListType
reuseNumber nodes  ma0
  = case extractFromNodes extractNumber defaultNumber nodes of
           (Number a0) -> reuse1 Number a0 ma0
           _ -> error "System error:<module>.reuseNumber"

reuseAlpha :: [Maybe Node] -> Maybe IDD -> ListType
reuseAlpha nodes  ma0
  = case extractFromNodes extractAlpha defaultAlpha nodes of
           (Alpha a0) -> reuse1 Alpha a0 ma0
           _ -> error "System error:<module>.reuseAlpha"

reuseStringItem :: [Maybe Node] -> Maybe IDD -> Maybe String_ -> Item
reuseStringItem nodes  ma0 ma1
  = case extractFromNodes extractStringItem defaultStringItem nodes of
           (StringItem a0 a1) -> reuse2 StringItem a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseStringItem"

reuseHeliumItem :: [Maybe Node] -> Maybe IDD -> Maybe Exp -> Item
reuseHeliumItem nodes  ma0 ma1
  = case extractFromNodes extractHeliumItem defaultHeliumItem nodes of
           (HeliumItem a0 a1) -> reuse2 HeliumItem a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseHeliumItem"

reuseListItem :: [Maybe Node] -> Maybe IDD -> Maybe ItemList -> Item
reuseListItem nodes  ma0 ma1
  = case extractFromNodes extractListItem defaultListItem nodes of
           (ListItem a0 a1) -> reuse2 ListItem a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseListItem"

reuseString_ :: [Maybe Node] -> Maybe IDD -> Maybe String -> String_
reuseString_ nodes  ma0 ma1
  = case extractFromNodes extractString_ defaultString_ nodes of
           (String_ a0 a1) -> reuse2 String_ a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseString_"

reuseList_Decl :: [Maybe Node] -> Maybe IDD -> Maybe ConsList_Decl -> List_Decl
reuseList_Decl nodes  ma0 ma1
  = case extractFromNodes extractList_Decl defaultList_Decl nodes of
           (List_Decl a0 a1) -> reuse2 List_Decl a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Decl"

reuseList_Alt :: [Maybe Node] -> Maybe IDD -> Maybe ConsList_Alt -> List_Alt
reuseList_Alt nodes  ma0 ma1
  = case extractFromNodes extractList_Alt defaultList_Alt nodes of
           (List_Alt a0 a1) -> reuse2 List_Alt a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Alt"

reuseList_Exp :: [Maybe Node] -> Maybe IDD -> Maybe ConsList_Exp -> List_Exp
reuseList_Exp nodes  ma0 ma1
  = case extractFromNodes extractList_Exp defaultList_Exp nodes of
           (List_Exp a0 a1) -> reuse2 List_Exp a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Exp"

reuseList_Slide :: [Maybe Node] -> Maybe IDD -> Maybe ConsList_Slide -> List_Slide
reuseList_Slide nodes  ma0 ma1
  = case extractFromNodes extractList_Slide defaultList_Slide nodes of
           (List_Slide a0 a1) -> reuse2 List_Slide a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Slide"

reuseList_Item :: [Maybe Node] -> Maybe IDD -> Maybe ConsList_Item -> List_Item
reuseList_Item nodes  ma0 ma1
  = case extractFromNodes extractList_Item defaultList_Item nodes of
           (List_Item a0 a1) -> reuse2 List_Item a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseList_Item"

extractRootEnr :: Maybe Node -> Maybe EnrichedDoc
extractRootEnr (Just (EnrichedDocNode x@(RootEnr _ _ _ _ _ _) _)) = Just x
extractRootEnr _ = Nothing

extractDecl :: Maybe Node -> Maybe Decl
extractDecl (Just (DeclNode x@(Decl _ _ _ _ _ _ _ _ _) _)) = Just x
extractDecl _ = Nothing

extractBoardDecl :: Maybe Node -> Maybe Decl
extractBoardDecl (Just (DeclNode x@(BoardDecl _ _ _ _) _)) = Just x
extractBoardDecl _ = Nothing

extractPPPresentationDecl :: Maybe Node -> Maybe Decl
extractPPPresentationDecl (Just (DeclNode x@(PPPresentationDecl _ _ _ _) _)) = Just x
extractPPPresentationDecl _ = Nothing

extractIdent :: Maybe Node -> Maybe Ident
extractIdent (Just (IdentNode x@(Ident _ _ _ _) _)) = Just x
extractIdent _ = Nothing

extractPlusExp :: Maybe Node -> Maybe Exp
extractPlusExp (Just (ExpNode x@(PlusExp _ _ _ _) _)) = Just x
extractPlusExp _ = Nothing

extractTimesExp :: Maybe Node -> Maybe Exp
extractTimesExp (Just (ExpNode x@(TimesExp _ _ _ _) _)) = Just x
extractTimesExp _ = Nothing

extractDivExp :: Maybe Node -> Maybe Exp
extractDivExp (Just (ExpNode x@(DivExp _ _ _ _) _)) = Just x
extractDivExp _ = Nothing

extractPowerExp :: Maybe Node -> Maybe Exp
extractPowerExp (Just (ExpNode x@(PowerExp _ _ _ _) _)) = Just x
extractPowerExp _ = Nothing

extractBoolExp :: Maybe Node -> Maybe Exp
extractBoolExp (Just (ExpNode x@(BoolExp _ _ _) _)) = Just x
extractBoolExp _ = Nothing

extractIntExp :: Maybe Node -> Maybe Exp
extractIntExp (Just (ExpNode x@(IntExp _ _ _) _)) = Just x
extractIntExp _ = Nothing

extractLamExp :: Maybe Node -> Maybe Exp
extractLamExp (Just (ExpNode x@(LamExp _ _ _ _ _) _)) = Just x
extractLamExp _ = Nothing

extractAppExp :: Maybe Node -> Maybe Exp
extractAppExp (Just (ExpNode x@(AppExp _ _ _) _)) = Just x
extractAppExp _ = Nothing

extractCaseExp :: Maybe Node -> Maybe Exp
extractCaseExp (Just (ExpNode x@(CaseExp _ _ _ _ _) _)) = Just x
extractCaseExp _ = Nothing

extractLetExp :: Maybe Node -> Maybe Exp
extractLetExp (Just (ExpNode x@(LetExp _ _ _ _ _) _)) = Just x
extractLetExp _ = Nothing

extractIdentExp :: Maybe Node -> Maybe Exp
extractIdentExp (Just (ExpNode x@(IdentExp _ _) _)) = Just x
extractIdentExp _ = Nothing

extractIfExp :: Maybe Node -> Maybe Exp
extractIfExp (Just (ExpNode x@(IfExp _ _ _ _ _ _ _) _)) = Just x
extractIfExp _ = Nothing

extractParenExp :: Maybe Node -> Maybe Exp
extractParenExp (Just (ExpNode x@(ParenExp _ _ _ _) _)) = Just x
extractParenExp _ = Nothing

extractListExp :: Maybe Node -> Maybe Exp
extractListExp (Just (ExpNode x@(ListExp _ _ _ _ _) _)) = Just x
extractListExp _ = Nothing

extractProductExp :: Maybe Node -> Maybe Exp
extractProductExp (Just (ExpNode x@(ProductExp _ _ _ _ _) _)) = Just x
extractProductExp _ = Nothing

extractAlt :: Maybe Node -> Maybe Alt
extractAlt (Just (AltNode x@(Alt _ _ _ _ _) _)) = Just x
extractAlt _ = Nothing

extractBoard :: Maybe Node -> Maybe Board
extractBoard (Just (BoardNode x@(Board _ _ _ _ _ _ _ _ _) _)) = Just x
extractBoard _ = Nothing

extractBoardRow :: Maybe Node -> Maybe BoardRow
extractBoardRow (Just (BoardRowNode x@(BoardRow _ _ _ _ _ _ _ _ _) _)) = Just x
extractBoardRow _ = Nothing

extractQueen :: Maybe Node -> Maybe BoardSquare
extractQueen (Just (BoardSquareNode x@(Queen _ _) _)) = Just x
extractQueen _ = Nothing

extractKing :: Maybe Node -> Maybe BoardSquare
extractKing (Just (BoardSquareNode x@(King _ _) _)) = Just x
extractKing _ = Nothing

extractBishop :: Maybe Node -> Maybe BoardSquare
extractBishop (Just (BoardSquareNode x@(Bishop _ _) _)) = Just x
extractBishop _ = Nothing

extractKnight :: Maybe Node -> Maybe BoardSquare
extractKnight (Just (BoardSquareNode x@(Knight _ _) _)) = Just x
extractKnight _ = Nothing

extractRook :: Maybe Node -> Maybe BoardSquare
extractRook (Just (BoardSquareNode x@(Rook _ _) _)) = Just x
extractRook _ = Nothing

extractPawn :: Maybe Node -> Maybe BoardSquare
extractPawn (Just (BoardSquareNode x@(Pawn _ _) _)) = Just x
extractPawn _ = Nothing

extractEmpty :: Maybe Node -> Maybe BoardSquare
extractEmpty (Just (BoardSquareNode x@(Empty) _)) = Just x
extractEmpty _ = Nothing

extractPPPresentation :: Maybe Node -> Maybe PPPresentation
extractPPPresentation (Just (PPPresentationNode x@(PPPresentation _ _ _) _)) = Just x
extractPPPresentation _ = Nothing

extractSlide :: Maybe Node -> Maybe Slide
extractSlide (Just (SlideNode x@(Slide _ _ _) _)) = Just x
extractSlide _ = Nothing

extractItemList :: Maybe Node -> Maybe ItemList
extractItemList (Just (ItemListNode x@(ItemList _ _ _) _)) = Just x
extractItemList _ = Nothing

extractBullet :: Maybe Node -> Maybe ListType
extractBullet (Just (ListTypeNode x@(Bullet _) _)) = Just x
extractBullet _ = Nothing

extractNumber :: Maybe Node -> Maybe ListType
extractNumber (Just (ListTypeNode x@(Number _) _)) = Just x
extractNumber _ = Nothing

extractAlpha :: Maybe Node -> Maybe ListType
extractAlpha (Just (ListTypeNode x@(Alpha _) _)) = Just x
extractAlpha _ = Nothing

extractStringItem :: Maybe Node -> Maybe Item
extractStringItem (Just (ItemNode x@(StringItem _ _) _)) = Just x
extractStringItem _ = Nothing

extractHeliumItem :: Maybe Node -> Maybe Item
extractHeliumItem (Just (ItemNode x@(HeliumItem _ _) _)) = Just x
extractHeliumItem _ = Nothing

extractListItem :: Maybe Node -> Maybe Item
extractListItem (Just (ItemNode x@(ListItem _ _) _)) = Just x
extractListItem _ = Nothing

extractString_ :: Maybe Node -> Maybe String_
extractString_ (Just (String_Node x@(String_ _ _) _)) = Just x
extractString_ _ = Nothing

extractList_Decl :: Maybe Node -> Maybe List_Decl
extractList_Decl (Just (List_DeclNode x@(List_Decl _ _) _)) = Just x
extractList_Decl _ = Nothing

extractList_Alt :: Maybe Node -> Maybe List_Alt
extractList_Alt (Just (List_AltNode x@(List_Alt _ _) _)) = Just x
extractList_Alt _ = Nothing

extractList_Exp :: Maybe Node -> Maybe List_Exp
extractList_Exp (Just (List_ExpNode x@(List_Exp _ _) _)) = Just x
extractList_Exp _ = Nothing

extractList_Slide :: Maybe Node -> Maybe List_Slide
extractList_Slide (Just (List_SlideNode x@(List_Slide _ _) _)) = Just x
extractList_Slide _ = Nothing

extractList_Item :: Maybe Node -> Maybe List_Item
extractList_Item (Just (List_ItemNode x@(List_Item _ _) _)) = Just x
extractList_Item _ = Nothing

defaultRootEnr :: EnrichedDoc
defaultRootEnr = RootEnr NoIDD NoIDP hole hole hole hole

defaultDecl :: Decl
defaultDecl = Decl NoIDD NoIDP NoIDP NoIDP NoIDP hole hole hole hole

defaultBoardDecl :: Decl
defaultBoardDecl = BoardDecl NoIDD NoIDP NoIDP hole

defaultPPPresentationDecl :: Decl
defaultPPPresentationDecl = PPPresentationDecl NoIDD NoIDP NoIDP hole

defaultIdent :: Ident
defaultIdent = Ident NoIDD NoIDP NoIDP hole

defaultPlusExp :: Exp
defaultPlusExp = PlusExp NoIDD NoIDP hole hole

defaultTimesExp :: Exp
defaultTimesExp = TimesExp NoIDD NoIDP hole hole

defaultDivExp :: Exp
defaultDivExp = DivExp NoIDD NoIDP hole hole

defaultPowerExp :: Exp
defaultPowerExp = PowerExp NoIDD NoIDP hole hole

defaultBoolExp :: Exp
defaultBoolExp = BoolExp NoIDD NoIDP hole

defaultIntExp :: Exp
defaultIntExp = IntExp NoIDD NoIDP hole

defaultLamExp :: Exp
defaultLamExp = LamExp NoIDD NoIDP NoIDP hole hole

defaultAppExp :: Exp
defaultAppExp = AppExp NoIDD hole hole

defaultCaseExp :: Exp
defaultCaseExp = CaseExp NoIDD NoIDP NoIDP hole hole

defaultLetExp :: Exp
defaultLetExp = LetExp NoIDD NoIDP NoIDP hole hole

defaultIdentExp :: Exp
defaultIdentExp = IdentExp NoIDD hole

defaultIfExp :: Exp
defaultIfExp = IfExp NoIDD NoIDP NoIDP NoIDP hole hole hole

defaultParenExp :: Exp
defaultParenExp = ParenExp NoIDD NoIDP NoIDP hole

defaultListExp :: Exp
defaultListExp = ListExp NoIDD NoIDP NoIDP [] hole

defaultProductExp :: Exp
defaultProductExp = ProductExp NoIDD NoIDP NoIDP [] hole

defaultAlt :: Alt
defaultAlt = Alt NoIDD NoIDP NoIDP hole hole

defaultBoard :: Board
defaultBoard = Board NoIDD hole hole hole hole hole hole hole hole

defaultBoardRow :: BoardRow
defaultBoardRow = BoardRow NoIDD hole hole hole hole hole hole hole hole

defaultQueen :: BoardSquare
defaultQueen = Queen NoIDD hole

defaultKing :: BoardSquare
defaultKing = King NoIDD hole

defaultBishop :: BoardSquare
defaultBishop = Bishop NoIDD hole

defaultKnight :: BoardSquare
defaultKnight = Knight NoIDD hole

defaultRook :: BoardSquare
defaultRook = Rook NoIDD hole

defaultPawn :: BoardSquare
defaultPawn = Pawn NoIDD hole

defaultEmpty :: BoardSquare
defaultEmpty = Empty

defaultPPPresentation :: PPPresentation
defaultPPPresentation = PPPresentation NoIDD hole hole

defaultSlide :: Slide
defaultSlide = Slide NoIDD hole hole

defaultItemList :: ItemList
defaultItemList = ItemList NoIDD hole hole

defaultBullet :: ListType
defaultBullet = Bullet NoIDD

defaultNumber :: ListType
defaultNumber = Number NoIDD

defaultAlpha :: ListType
defaultAlpha = Alpha NoIDD

defaultStringItem :: Item
defaultStringItem = StringItem NoIDD hole

defaultHeliumItem :: Item
defaultHeliumItem = HeliumItem NoIDD hole

defaultListItem :: Item
defaultListItem = ListItem NoIDD hole

defaultString_ :: String_
defaultString_ = String_ NoIDD hole

defaultList_Decl :: List_Decl
defaultList_Decl = List_Decl NoIDD Nil_Decl

defaultList_Alt :: List_Alt
defaultList_Alt = List_Alt NoIDD Nil_Alt

defaultList_Exp :: List_Exp
defaultList_Exp = List_Exp NoIDD Nil_Exp

defaultList_Slide :: List_Slide
defaultList_Slide = List_Slide NoIDD Nil_Slide

defaultList_Item :: List_Item
defaultList_Item = List_Item NoIDD Nil_Item

-- General
-- return result of the first extraction application in the list that is not Nothing
--extractFromNodes ::(Node -> Maybe a) -> a -> [Node] -> a
extractFromNodes extr def []     = def
extractFromNodes extr def (n:ns) = maybe (extractFromNodes extr def ns) id (extr n)

reuse6 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> r) -> 
          a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> r
reuse6 f  a0 a1 a2 a3 a4 a5 ma0 ma1 ma2 ma3 ma4 ma5 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) (maybe a5 id ma5) 

reuse9 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> r) -> 
          a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> Maybe a6 -> Maybe a7 -> Maybe a8 -> r
reuse9 f  a0 a1 a2 a3 a4 a5 a6 a7 a8 ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) (maybe a5 id ma5) (maybe a6 id ma6) (maybe a7 id ma7) (maybe a8 id ma8) 

reuse4 :: (a0 -> a1 -> a2 -> a3 -> r) -> 
          a0 -> a1 -> a2 -> a3 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> r
reuse4 f  a0 a1 a2 a3 ma0 ma1 ma2 ma3 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) 

reuse3 :: (a0 -> a1 -> a2 -> r) -> 
          a0 -> a1 -> a2 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> r
reuse3 f  a0 a1 a2 ma0 ma1 ma2 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) 

reuse5 :: (a0 -> a1 -> a2 -> a3 -> a4 -> r) -> 
          a0 -> a1 -> a2 -> a3 -> a4 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> r
reuse5 f  a0 a1 a2 a3 a4 ma0 ma1 ma2 ma3 ma4 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) 

reuse2 :: (a0 -> a1 -> r) -> 
          a0 -> a1 -> 
          Maybe a0 -> Maybe a1 -> r
reuse2 f  a0 a1 ma0 ma1 =
  f (maybe a0 id ma0) (maybe a1 id ma1) 

reuse7 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r) -> 
          a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> Maybe a6 -> r
reuse7 f  a0 a1 a2 a3 a4 a5 a6 ma0 ma1 ma2 ma3 ma4 ma5 ma6 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) (maybe a5 id ma5) (maybe a6 id ma6) 

reuse0 :: r -> r
reuse0 f = f

reuse1 :: (a0 -> r) -> 
          a0 -> 
          Maybe a0 -> r
reuse1 f  a0 ma0 =
  f (maybe a0 id ma0) 

