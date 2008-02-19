module ProxParser_Generated where

import Common.CommonTypes
import Presentation.PresLayerTypes
import Presentation.PresLayerUtils

import Evaluation.DocumentEdit
import DocumentEdit_Generated
import Evaluation.DocTypes
import DocTypes_Generated

----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

{- ------------------------------------

 generated part

-------------------------------------- -}






-- ProxParser_Generated --

-- Type specific
reuseDummy :: [Token doc Node clip token] -> Maybe Root -> Maybe Dummy -> Dummy
reuseDummy nodes  ma0 ma1
  = case extractFromTokens extractDummy defaultDummy nodes of
           (Dummy a0 a1) -> reuse2 Dummy a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseDummy"

reuseRootEnr :: [Token doc Node clip token] -> Maybe RootE -> Maybe HeliumTypeInfo -> Maybe Document -> EnrichedDoc
reuseRootEnr nodes  ma0 ma1 ma2
  = case extractFromTokens extractRootEnr defaultRootEnr nodes of
           (RootEnr a0 a1 a2) -> reuse3 RootEnr a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseRootEnr"

reuseRoot :: [Token doc Node clip token] -> Maybe IDP -> Maybe List_Decl -> Root
reuseRoot nodes  ma0 ma1
  = case extractFromTokens extractRoot defaultRoot nodes of
           (Root a0 a1) -> reuse2 Root a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseRoot"

reuseRootE :: [Token doc Node clip token] -> Maybe IDP -> Maybe List_Decl -> Maybe List_Decl -> RootE
reuseRootE nodes  ma0 ma1 ma2
  = case extractFromTokens extractRootE defaultRootE nodes of
           (RootE a0 a1 a2) -> reuse3 RootE a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseRootE"

reuseDecl :: [Token doc Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe IDP -> Maybe IDP -> Maybe Bool -> Maybe Bool -> Maybe Ident -> Maybe Exp -> Decl
reuseDecl nodes  ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7
  = case extractFromTokens extractDecl defaultDecl nodes of
           (Decl a0 a1 a2 a3 a4 a5 a6 a7) -> reuse8 Decl a0 a1 a2 a3 a4 a5 a6 a7 ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7
           _ -> error "System error:<module>.reuseDecl"

reuseBoardDecl :: [Token doc Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe Board -> Decl
reuseBoardDecl nodes  ma0 ma1 ma2
  = case extractFromTokens extractBoardDecl defaultBoardDecl nodes of
           (BoardDecl a0 a1 a2) -> reuse3 BoardDecl a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseBoardDecl"

reusePPPresentationDecl :: [Token doc Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe PPPresentation -> Decl
reusePPPresentationDecl nodes  ma0 ma1 ma2
  = case extractFromTokens extractPPPresentationDecl defaultPPPresentationDecl nodes of
           (PPPresentationDecl a0 a1 a2) -> reuse3 PPPresentationDecl a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reusePPPresentationDecl"

reuseIdent :: [Token doc Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe String -> Ident
reuseIdent nodes  ma0 ma1 ma2
  = case extractFromTokens extractIdent defaultIdent nodes of
           (Ident a0 a1 a2) -> reuse3 Ident a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseIdent"

reusePlusExp :: [Token doc Node clip token] -> Maybe IDP -> Maybe Exp -> Maybe Exp -> Exp
reusePlusExp nodes  ma0 ma1 ma2
  = case extractFromTokens extractPlusExp defaultPlusExp nodes of
           (PlusExp a0 a1 a2) -> reuse3 PlusExp a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reusePlusExp"

reuseTimesExp :: [Token doc Node clip token] -> Maybe IDP -> Maybe Exp -> Maybe Exp -> Exp
reuseTimesExp nodes  ma0 ma1 ma2
  = case extractFromTokens extractTimesExp defaultTimesExp nodes of
           (TimesExp a0 a1 a2) -> reuse3 TimesExp a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseTimesExp"

reuseDivExp :: [Token doc Node clip token] -> Maybe IDP -> Maybe Exp -> Maybe Exp -> Exp
reuseDivExp nodes  ma0 ma1 ma2
  = case extractFromTokens extractDivExp defaultDivExp nodes of
           (DivExp a0 a1 a2) -> reuse3 DivExp a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseDivExp"

reusePowerExp :: [Token doc Node clip token] -> Maybe IDP -> Maybe Exp -> Maybe Exp -> Exp
reusePowerExp nodes  ma0 ma1 ma2
  = case extractFromTokens extractPowerExp defaultPowerExp nodes of
           (PowerExp a0 a1 a2) -> reuse3 PowerExp a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reusePowerExp"

reuseBoolExp :: [Token doc Node clip token] -> Maybe IDP -> Maybe Bool -> Exp
reuseBoolExp nodes  ma0 ma1
  = case extractFromTokens extractBoolExp defaultBoolExp nodes of
           (BoolExp a0 a1) -> reuse2 BoolExp a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseBoolExp"

reuseIntExp :: [Token doc Node clip token] -> Maybe IDP -> Maybe Int -> Exp
reuseIntExp nodes  ma0 ma1
  = case extractFromTokens extractIntExp defaultIntExp nodes of
           (IntExp a0 a1) -> reuse2 IntExp a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseIntExp"

reuseLamExp :: [Token doc Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe Ident -> Maybe Exp -> Exp
reuseLamExp nodes  ma0 ma1 ma2 ma3
  = case extractFromTokens extractLamExp defaultLamExp nodes of
           (LamExp a0 a1 a2 a3) -> reuse4 LamExp a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseLamExp"

reuseAppExp :: [Token doc Node clip token] -> Maybe Exp -> Maybe Exp -> Exp
reuseAppExp nodes  ma0 ma1
  = case extractFromTokens extractAppExp defaultAppExp nodes of
           (AppExp a0 a1) -> reuse2 AppExp a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseAppExp"

reuseCaseExp :: [Token doc Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe Exp -> Maybe List_Alt -> Exp
reuseCaseExp nodes  ma0 ma1 ma2 ma3
  = case extractFromTokens extractCaseExp defaultCaseExp nodes of
           (CaseExp a0 a1 a2 a3) -> reuse4 CaseExp a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseCaseExp"

reuseLetExp :: [Token doc Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe List_Decl -> Maybe Exp -> Exp
reuseLetExp nodes  ma0 ma1 ma2 ma3
  = case extractFromTokens extractLetExp defaultLetExp nodes of
           (LetExp a0 a1 a2 a3) -> reuse4 LetExp a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseLetExp"

reuseIdentExp :: [Token doc Node clip token] -> Maybe Ident -> Exp
reuseIdentExp nodes  ma0
  = case extractFromTokens extractIdentExp defaultIdentExp nodes of
           (IdentExp a0) -> reuse1 IdentExp a0 ma0
           _ -> error "System error:<module>.reuseIdentExp"

reuseIfExp :: [Token doc Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe IDP -> Maybe Exp -> Maybe Exp -> Maybe Exp -> Exp
reuseIfExp nodes  ma0 ma1 ma2 ma3 ma4 ma5
  = case extractFromTokens extractIfExp defaultIfExp nodes of
           (IfExp a0 a1 a2 a3 a4 a5) -> reuse6 IfExp a0 a1 a2 a3 a4 a5 ma0 ma1 ma2 ma3 ma4 ma5
           _ -> error "System error:<module>.reuseIfExp"

reuseParenExp :: [Token doc Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe Exp -> Exp
reuseParenExp nodes  ma0 ma1 ma2
  = case extractFromTokens extractParenExp defaultParenExp nodes of
           (ParenExp a0 a1 a2) -> reuse3 ParenExp a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseParenExp"

reuseListExp :: [Token doc Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe [IDP]  -> Maybe List_Exp -> Exp
reuseListExp nodes  ma0 ma1 ma2 ma3
  = case extractFromTokens extractListExp defaultListExp nodes of
           (ListExp a0 a1 a2 a3) -> reuse4 ListExp a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseListExp"

reuseProductExp :: [Token doc Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe [IDP]  -> Maybe List_Exp -> Exp
reuseProductExp nodes  ma0 ma1 ma2 ma3
  = case extractFromTokens extractProductExp defaultProductExp nodes of
           (ProductExp a0 a1 a2 a3) -> reuse4 ProductExp a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseProductExp"

reuseAlt :: [Token doc Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe Ident -> Maybe Exp -> Alt
reuseAlt nodes  ma0 ma1 ma2 ma3
  = case extractFromTokens extractAlt defaultAlt nodes of
           (Alt a0 a1 a2 a3) -> reuse4 Alt a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseAlt"

reuseBoard :: [Token doc Node clip token] -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Board
reuseBoard nodes  ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7
  = case extractFromTokens extractBoard defaultBoard nodes of
           (Board a0 a1 a2 a3 a4 a5 a6 a7) -> reuse8 Board a0 a1 a2 a3 a4 a5 a6 a7 ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7
           _ -> error "System error:<module>.reuseBoard"

reuseBoardRow :: [Token doc Node clip token] -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> BoardRow
reuseBoardRow nodes  ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7
  = case extractFromTokens extractBoardRow defaultBoardRow nodes of
           (BoardRow a0 a1 a2 a3 a4 a5 a6 a7) -> reuse8 BoardRow a0 a1 a2 a3 a4 a5 a6 a7 ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7
           _ -> error "System error:<module>.reuseBoardRow"

reuseQueen :: [Token doc Node clip token] -> Maybe Bool -> BoardSquare
reuseQueen nodes  ma0
  = case extractFromTokens extractQueen defaultQueen nodes of
           (Queen a0) -> reuse1 Queen a0 ma0
           _ -> error "System error:<module>.reuseQueen"

reuseKing :: [Token doc Node clip token] -> Maybe Bool -> BoardSquare
reuseKing nodes  ma0
  = case extractFromTokens extractKing defaultKing nodes of
           (King a0) -> reuse1 King a0 ma0
           _ -> error "System error:<module>.reuseKing"

reuseBishop :: [Token doc Node clip token] -> Maybe Bool -> BoardSquare
reuseBishop nodes  ma0
  = case extractFromTokens extractBishop defaultBishop nodes of
           (Bishop a0) -> reuse1 Bishop a0 ma0
           _ -> error "System error:<module>.reuseBishop"

reuseKnight :: [Token doc Node clip token] -> Maybe Bool -> BoardSquare
reuseKnight nodes  ma0
  = case extractFromTokens extractKnight defaultKnight nodes of
           (Knight a0) -> reuse1 Knight a0 ma0
           _ -> error "System error:<module>.reuseKnight"

reuseRook :: [Token doc Node clip token] -> Maybe Bool -> BoardSquare
reuseRook nodes  ma0
  = case extractFromTokens extractRook defaultRook nodes of
           (Rook a0) -> reuse1 Rook a0 ma0
           _ -> error "System error:<module>.reuseRook"

reusePawn :: [Token doc Node clip token] -> Maybe Bool -> BoardSquare
reusePawn nodes  ma0
  = case extractFromTokens extractPawn defaultPawn nodes of
           (Pawn a0) -> reuse1 Pawn a0 ma0
           _ -> error "System error:<module>.reusePawn"

reuseEmpty :: [Token doc Node clip token] -> BoardSquare
reuseEmpty nodes 
  = case extractFromTokens extractEmpty defaultEmpty nodes of
           (Empty) -> reuse0 Empty
           _ -> error "System error:<module>.reuseEmpty"

reusePPPresentation :: [Token doc Node clip token] -> Maybe Bool -> Maybe List_Slide -> PPPresentation
reusePPPresentation nodes  ma0 ma1
  = case extractFromTokens extractPPPresentation defaultPPPresentation nodes of
           (PPPresentation a0 a1) -> reuse2 PPPresentation a0 a1 ma0 ma1
           _ -> error "System error:<module>.reusePPPresentation"

reuseSlide :: [Token doc Node clip token] -> Maybe String -> Maybe ItemList -> Slide
reuseSlide nodes  ma0 ma1
  = case extractFromTokens extractSlide defaultSlide nodes of
           (Slide a0 a1) -> reuse2 Slide a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseSlide"

reuseItemList :: [Token doc Node clip token] -> Maybe ListType -> Maybe List_Item -> ItemList
reuseItemList nodes  ma0 ma1
  = case extractFromTokens extractItemList defaultItemList nodes of
           (ItemList a0 a1) -> reuse2 ItemList a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseItemList"

reuseBullet :: [Token doc Node clip token] -> ListType
reuseBullet nodes 
  = case extractFromTokens extractBullet defaultBullet nodes of
           (Bullet) -> reuse0 Bullet
           _ -> error "System error:<module>.reuseBullet"

reuseNumber :: [Token doc Node clip token] -> ListType
reuseNumber nodes 
  = case extractFromTokens extractNumber defaultNumber nodes of
           (Number) -> reuse0 Number
           _ -> error "System error:<module>.reuseNumber"

reuseAlpha :: [Token doc Node clip token] -> ListType
reuseAlpha nodes 
  = case extractFromTokens extractAlpha defaultAlpha nodes of
           (Alpha) -> reuse0 Alpha
           _ -> error "System error:<module>.reuseAlpha"

reuseStringItem :: [Token doc Node clip token] -> Maybe String -> Item
reuseStringItem nodes  ma0
  = case extractFromTokens extractStringItem defaultStringItem nodes of
           (StringItem a0) -> reuse1 StringItem a0 ma0
           _ -> error "System error:<module>.reuseStringItem"

reuseHeliumItem :: [Token doc Node clip token] -> Maybe Exp -> Item
reuseHeliumItem nodes  ma0
  = case extractFromTokens extractHeliumItem defaultHeliumItem nodes of
           (HeliumItem a0) -> reuse1 HeliumItem a0 ma0
           _ -> error "System error:<module>.reuseHeliumItem"

reuseListItem :: [Token doc Node clip token] -> Maybe ItemList -> Item
reuseListItem nodes  ma0
  = case extractFromTokens extractListItem defaultListItem nodes of
           (ListItem a0) -> reuse1 ListItem a0 ma0
           _ -> error "System error:<module>.reuseListItem"

reuseList_Decl :: [Token doc Node clip token] -> Maybe ConsList_Decl -> List_Decl
reuseList_Decl nodes  ma0
  = case extractFromTokens extractList_Decl defaultList_Decl nodes of
           (List_Decl a0) -> reuse1 List_Decl a0 ma0
           _ -> error "System error:<module>.reuseList_Decl"

reuseList_Alt :: [Token doc Node clip token] -> Maybe ConsList_Alt -> List_Alt
reuseList_Alt nodes  ma0
  = case extractFromTokens extractList_Alt defaultList_Alt nodes of
           (List_Alt a0) -> reuse1 List_Alt a0 ma0
           _ -> error "System error:<module>.reuseList_Alt"

reuseList_Exp :: [Token doc Node clip token] -> Maybe ConsList_Exp -> List_Exp
reuseList_Exp nodes  ma0
  = case extractFromTokens extractList_Exp defaultList_Exp nodes of
           (List_Exp a0) -> reuse1 List_Exp a0 ma0
           _ -> error "System error:<module>.reuseList_Exp"

reuseList_Slide :: [Token doc Node clip token] -> Maybe ConsList_Slide -> List_Slide
reuseList_Slide nodes  ma0
  = case extractFromTokens extractList_Slide defaultList_Slide nodes of
           (List_Slide a0) -> reuse1 List_Slide a0 ma0
           _ -> error "System error:<module>.reuseList_Slide"

reuseList_Item :: [Token doc Node clip token] -> Maybe ConsList_Item -> List_Item
reuseList_Item nodes  ma0
  = case extractFromTokens extractList_Item defaultList_Item nodes of
           (List_Item a0) -> reuse1 List_Item a0 ma0
           _ -> error "System error:<module>.reuseList_Item"

extractDummy :: Maybe Node -> Maybe Dummy
extractDummy (Just (DummyNode x@(Dummy _ _) _)) = Just x
extractDummy _ = Nothing

extractRootEnr :: Maybe Node -> Maybe EnrichedDoc
extractRootEnr (Just (RootEnrNode x@(RootEnr _ _ _) _)) = Just x
extractRootEnr _ = Nothing

extractRoot :: Maybe Node -> Maybe Root
extractRoot (Just (RootNode x@(Root _ _) _)) = Just x
extractRoot _ = Nothing

extractRootE :: Maybe Node -> Maybe RootE
extractRootE (Just (RootENode x@(RootE _ _ _) _)) = Just x
extractRootE _ = Nothing

extractDecl :: Maybe Node -> Maybe Decl
extractDecl (Just (DeclNode x@(Decl _ _ _ _ _ _ _ _) _)) = Just x
extractDecl _ = Nothing

extractBoardDecl :: Maybe Node -> Maybe Decl
extractBoardDecl (Just (BoardDeclNode x@(BoardDecl _ _ _) _)) = Just x
extractBoardDecl _ = Nothing

extractPPPresentationDecl :: Maybe Node -> Maybe Decl
extractPPPresentationDecl (Just (PPPresentationDeclNode x@(PPPresentationDecl _ _ _) _)) = Just x
extractPPPresentationDecl _ = Nothing

extractIdent :: Maybe Node -> Maybe Ident
extractIdent (Just (IdentNode x@(Ident _ _ _) _)) = Just x
extractIdent _ = Nothing

extractPlusExp :: Maybe Node -> Maybe Exp
extractPlusExp (Just (PlusExpNode x@(PlusExp _ _ _) _)) = Just x
extractPlusExp _ = Nothing

extractTimesExp :: Maybe Node -> Maybe Exp
extractTimesExp (Just (TimesExpNode x@(TimesExp _ _ _) _)) = Just x
extractTimesExp _ = Nothing

extractDivExp :: Maybe Node -> Maybe Exp
extractDivExp (Just (DivExpNode x@(DivExp _ _ _) _)) = Just x
extractDivExp _ = Nothing

extractPowerExp :: Maybe Node -> Maybe Exp
extractPowerExp (Just (PowerExpNode x@(PowerExp _ _ _) _)) = Just x
extractPowerExp _ = Nothing

extractBoolExp :: Maybe Node -> Maybe Exp
extractBoolExp (Just (BoolExpNode x@(BoolExp _ _) _)) = Just x
extractBoolExp _ = Nothing

extractIntExp :: Maybe Node -> Maybe Exp
extractIntExp (Just (IntExpNode x@(IntExp _ _) _)) = Just x
extractIntExp _ = Nothing

extractLamExp :: Maybe Node -> Maybe Exp
extractLamExp (Just (LamExpNode x@(LamExp _ _ _ _) _)) = Just x
extractLamExp _ = Nothing

extractAppExp :: Maybe Node -> Maybe Exp
extractAppExp (Just (AppExpNode x@(AppExp _ _) _)) = Just x
extractAppExp _ = Nothing

extractCaseExp :: Maybe Node -> Maybe Exp
extractCaseExp (Just (CaseExpNode x@(CaseExp _ _ _ _) _)) = Just x
extractCaseExp _ = Nothing

extractLetExp :: Maybe Node -> Maybe Exp
extractLetExp (Just (LetExpNode x@(LetExp _ _ _ _) _)) = Just x
extractLetExp _ = Nothing

extractIdentExp :: Maybe Node -> Maybe Exp
extractIdentExp (Just (IdentExpNode x@(IdentExp _) _)) = Just x
extractIdentExp _ = Nothing

extractIfExp :: Maybe Node -> Maybe Exp
extractIfExp (Just (IfExpNode x@(IfExp _ _ _ _ _ _) _)) = Just x
extractIfExp _ = Nothing

extractParenExp :: Maybe Node -> Maybe Exp
extractParenExp (Just (ParenExpNode x@(ParenExp _ _ _) _)) = Just x
extractParenExp _ = Nothing

extractListExp :: Maybe Node -> Maybe Exp
extractListExp (Just (ListExpNode x@(ListExp _ _ _ _) _)) = Just x
extractListExp _ = Nothing

extractProductExp :: Maybe Node -> Maybe Exp
extractProductExp (Just (ProductExpNode x@(ProductExp _ _ _ _) _)) = Just x
extractProductExp _ = Nothing

extractAlt :: Maybe Node -> Maybe Alt
extractAlt (Just (AltNode x@(Alt _ _ _ _) _)) = Just x
extractAlt _ = Nothing

extractBoard :: Maybe Node -> Maybe Board
extractBoard (Just (BoardNode x@(Board _ _ _ _ _ _ _ _) _)) = Just x
extractBoard _ = Nothing

extractBoardRow :: Maybe Node -> Maybe BoardRow
extractBoardRow (Just (BoardRowNode x@(BoardRow _ _ _ _ _ _ _ _) _)) = Just x
extractBoardRow _ = Nothing

extractQueen :: Maybe Node -> Maybe BoardSquare
extractQueen (Just (QueenNode x@(Queen _) _)) = Just x
extractQueen _ = Nothing

extractKing :: Maybe Node -> Maybe BoardSquare
extractKing (Just (KingNode x@(King _) _)) = Just x
extractKing _ = Nothing

extractBishop :: Maybe Node -> Maybe BoardSquare
extractBishop (Just (BishopNode x@(Bishop _) _)) = Just x
extractBishop _ = Nothing

extractKnight :: Maybe Node -> Maybe BoardSquare
extractKnight (Just (KnightNode x@(Knight _) _)) = Just x
extractKnight _ = Nothing

extractRook :: Maybe Node -> Maybe BoardSquare
extractRook (Just (RookNode x@(Rook _) _)) = Just x
extractRook _ = Nothing

extractPawn :: Maybe Node -> Maybe BoardSquare
extractPawn (Just (PawnNode x@(Pawn _) _)) = Just x
extractPawn _ = Nothing

extractEmpty :: Maybe Node -> Maybe BoardSquare
extractEmpty (Just (EmptyNode x@(Empty) _)) = Just x
extractEmpty _ = Nothing

extractPPPresentation :: Maybe Node -> Maybe PPPresentation
extractPPPresentation (Just (PPPresentationNode x@(PPPresentation _ _) _)) = Just x
extractPPPresentation _ = Nothing

extractSlide :: Maybe Node -> Maybe Slide
extractSlide (Just (SlideNode x@(Slide _ _) _)) = Just x
extractSlide _ = Nothing

extractItemList :: Maybe Node -> Maybe ItemList
extractItemList (Just (ItemListNode x@(ItemList _ _) _)) = Just x
extractItemList _ = Nothing

extractBullet :: Maybe Node -> Maybe ListType
extractBullet (Just (BulletNode x@(Bullet) _)) = Just x
extractBullet _ = Nothing

extractNumber :: Maybe Node -> Maybe ListType
extractNumber (Just (NumberNode x@(Number) _)) = Just x
extractNumber _ = Nothing

extractAlpha :: Maybe Node -> Maybe ListType
extractAlpha (Just (AlphaNode x@(Alpha) _)) = Just x
extractAlpha _ = Nothing

extractStringItem :: Maybe Node -> Maybe Item
extractStringItem (Just (StringItemNode x@(StringItem _) _)) = Just x
extractStringItem _ = Nothing

extractHeliumItem :: Maybe Node -> Maybe Item
extractHeliumItem (Just (HeliumItemNode x@(HeliumItem _) _)) = Just x
extractHeliumItem _ = Nothing

extractListItem :: Maybe Node -> Maybe Item
extractListItem (Just (ListItemNode x@(ListItem _) _)) = Just x
extractListItem _ = Nothing

extractList_Decl :: Maybe Node -> Maybe List_Decl
extractList_Decl (Just (List_DeclNode x@(List_Decl _) _)) = Just x
extractList_Decl _ = Nothing

extractList_Alt :: Maybe Node -> Maybe List_Alt
extractList_Alt (Just (List_AltNode x@(List_Alt _) _)) = Just x
extractList_Alt _ = Nothing

extractList_Exp :: Maybe Node -> Maybe List_Exp
extractList_Exp (Just (List_ExpNode x@(List_Exp _) _)) = Just x
extractList_Exp _ = Nothing

extractList_Slide :: Maybe Node -> Maybe List_Slide
extractList_Slide (Just (List_SlideNode x@(List_Slide _) _)) = Just x
extractList_Slide _ = Nothing

extractList_Item :: Maybe Node -> Maybe List_Item
extractList_Item (Just (List_ItemNode x@(List_Item _) _)) = Just x
extractList_Item _ = Nothing

defaultDummy :: Dummy
defaultDummy = Dummy hole hole

defaultRootEnr :: EnrichedDoc
defaultRootEnr = RootEnr hole hole hole

defaultRoot :: Root
defaultRoot = Root NoIDP hole

defaultRootE :: RootE
defaultRootE = RootE NoIDP hole hole

defaultDecl :: Decl
defaultDecl = Decl NoIDP NoIDP NoIDP NoIDP hole hole hole hole

defaultBoardDecl :: Decl
defaultBoardDecl = BoardDecl NoIDP NoIDP hole

defaultPPPresentationDecl :: Decl
defaultPPPresentationDecl = PPPresentationDecl NoIDP NoIDP hole

defaultIdent :: Ident
defaultIdent = Ident NoIDP NoIDP hole

defaultPlusExp :: Exp
defaultPlusExp = PlusExp NoIDP hole hole

defaultTimesExp :: Exp
defaultTimesExp = TimesExp NoIDP hole hole

defaultDivExp :: Exp
defaultDivExp = DivExp NoIDP hole hole

defaultPowerExp :: Exp
defaultPowerExp = PowerExp NoIDP hole hole

defaultBoolExp :: Exp
defaultBoolExp = BoolExp NoIDP hole

defaultIntExp :: Exp
defaultIntExp = IntExp NoIDP hole

defaultLamExp :: Exp
defaultLamExp = LamExp NoIDP NoIDP hole hole

defaultAppExp :: Exp
defaultAppExp = AppExp hole hole

defaultCaseExp :: Exp
defaultCaseExp = CaseExp NoIDP NoIDP hole hole

defaultLetExp :: Exp
defaultLetExp = LetExp NoIDP NoIDP hole hole

defaultIdentExp :: Exp
defaultIdentExp = IdentExp hole

defaultIfExp :: Exp
defaultIfExp = IfExp NoIDP NoIDP NoIDP hole hole hole

defaultParenExp :: Exp
defaultParenExp = ParenExp NoIDP NoIDP hole

defaultListExp :: Exp
defaultListExp = ListExp NoIDP NoIDP [] hole

defaultProductExp :: Exp
defaultProductExp = ProductExp NoIDP NoIDP [] hole

defaultAlt :: Alt
defaultAlt = Alt NoIDP NoIDP hole hole

defaultBoard :: Board
defaultBoard = Board hole hole hole hole hole hole hole hole

defaultBoardRow :: BoardRow
defaultBoardRow = BoardRow hole hole hole hole hole hole hole hole

defaultQueen :: BoardSquare
defaultQueen = Queen hole

defaultKing :: BoardSquare
defaultKing = King hole

defaultBishop :: BoardSquare
defaultBishop = Bishop hole

defaultKnight :: BoardSquare
defaultKnight = Knight hole

defaultRook :: BoardSquare
defaultRook = Rook hole

defaultPawn :: BoardSquare
defaultPawn = Pawn hole

defaultEmpty :: BoardSquare
defaultEmpty = Empty

defaultPPPresentation :: PPPresentation
defaultPPPresentation = PPPresentation hole hole

defaultSlide :: Slide
defaultSlide = Slide hole hole

defaultItemList :: ItemList
defaultItemList = ItemList hole hole

defaultBullet :: ListType
defaultBullet = Bullet

defaultNumber :: ListType
defaultNumber = Number

defaultAlpha :: ListType
defaultAlpha = Alpha

defaultStringItem :: Item
defaultStringItem = StringItem hole

defaultHeliumItem :: Item
defaultHeliumItem = HeliumItem hole

defaultListItem :: Item
defaultListItem = ListItem hole

defaultList_Decl :: List_Decl
defaultList_Decl = List_Decl Nil_Decl

defaultList_Alt :: List_Alt
defaultList_Alt = List_Alt Nil_Alt

defaultList_Exp :: List_Exp
defaultList_Exp = List_Exp Nil_Exp

defaultList_Slide :: List_Slide
defaultList_Slide = List_Slide Nil_Slide

defaultList_Item :: List_Item
defaultList_Item = List_Item Nil_Item

-- General
-- return result of the first extraction application in the list that is not Nothing
extractFromTokens :: (Maybe Node -> Maybe a) -> a -> [Token doc Node clip token] -> a
extractFromTokens extr def []     = def
extractFromTokens extr def (t:ts) = maybe (extractFromTokens extr def ts) id (extr (tokenNode t))

reuse2 :: (a0 -> a1 -> r) -> 
          a0 -> a1 -> 
          Maybe a0 -> Maybe a1 -> r
reuse2 f  a0 a1 ma0 ma1 =
  f (maybe a0 id ma0) (maybe a1 id ma1) 

reuse3 :: (a0 -> a1 -> a2 -> r) -> 
          a0 -> a1 -> a2 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> r
reuse3 f  a0 a1 a2 ma0 ma1 ma2 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) 

reuse8 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r) -> 
          a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> Maybe a6 -> Maybe a7 -> r
reuse8 f  a0 a1 a2 a3 a4 a5 a6 a7 ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) (maybe a5 id ma5) (maybe a6 id ma6) (maybe a7 id ma7) 

reuse4 :: (a0 -> a1 -> a2 -> a3 -> r) -> 
          a0 -> a1 -> a2 -> a3 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> r
reuse4 f  a0 a1 a2 a3 ma0 ma1 ma2 ma3 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) 

reuse1 :: (a0 -> r) -> 
          a0 -> 
          Maybe a0 -> r
reuse1 f  a0 ma0 =
  f (maybe a0 id ma0) 

reuse6 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> r) -> 
          a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> r
reuse6 f  a0 a1 a2 a3 a4 a5 ma0 ma1 ma2 ma3 ma4 ma5 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) (maybe a5 id ma5) 

reuse0 :: r -> r
reuse0 f = f

