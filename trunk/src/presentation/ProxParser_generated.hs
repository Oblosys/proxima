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

reuseDecl :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe IDP -> Maybe IDP -> Maybe IDP -> Maybe Bool_ -> Maybe Bool_ -> Maybe Ident -> Maybe Exp -> Decl
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

reuseInvDecl :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe IDP -> Maybe Inv -> Decl
reuseInvDecl nodes  ma0 ma1 ma2 ma3
  = case extractFromNodes extractInvDecl defaultInvDecl nodes of
           (InvDecl a0 a1 a2 a3) -> reuse4 InvDecl a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseInvDecl"

reuseIdent :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe IDP -> Maybe String_ -> Ident
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

reuseBoolExp :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe Bool_ -> Exp
reuseBoolExp nodes  ma0 ma1 ma2
  = case extractFromNodes extractBoolExp defaultBoolExp nodes of
           (BoolExp a0 a1 a2) -> reuse3 BoolExp a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseBoolExp"

reuseIntExp :: [Maybe Node] -> Maybe IDD -> Maybe IDP -> Maybe Int_ -> Exp
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

reuseQueen :: [Maybe Node] -> Maybe IDD -> Maybe Bool_ -> BoardSquare
reuseQueen nodes  ma0 ma1
  = case extractFromNodes extractQueen defaultQueen nodes of
           (Queen a0 a1) -> reuse2 Queen a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseQueen"

reuseKing :: [Maybe Node] -> Maybe IDD -> Maybe Bool_ -> BoardSquare
reuseKing nodes  ma0 ma1
  = case extractFromNodes extractKing defaultKing nodes of
           (King a0 a1) -> reuse2 King a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseKing"

reuseBishop :: [Maybe Node] -> Maybe IDD -> Maybe Bool_ -> BoardSquare
reuseBishop nodes  ma0 ma1
  = case extractFromNodes extractBishop defaultBishop nodes of
           (Bishop a0 a1) -> reuse2 Bishop a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseBishop"

reuseKnight :: [Maybe Node] -> Maybe IDD -> Maybe Bool_ -> BoardSquare
reuseKnight nodes  ma0 ma1
  = case extractFromNodes extractKnight defaultKnight nodes of
           (Knight a0 a1) -> reuse2 Knight a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseKnight"

reuseRook :: [Maybe Node] -> Maybe IDD -> Maybe Bool_ -> BoardSquare
reuseRook nodes  ma0 ma1
  = case extractFromNodes extractRook defaultRook nodes of
           (Rook a0 a1) -> reuse2 Rook a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseRook"

reusePawn :: [Maybe Node] -> Maybe IDD -> Maybe Bool_ -> BoardSquare
reusePawn nodes  ma0 ma1
  = case extractFromNodes extractPawn defaultPawn nodes of
           (Pawn a0 a1) -> reuse2 Pawn a0 a1 ma0 ma1
           _ -> error "System error:<module>.reusePawn"

reuseEmpty :: [Maybe Node] -> BoardSquare
reuseEmpty nodes 
  = case extractFromNodes extractEmpty defaultEmpty nodes of
           (Empty) -> reuse0 Empty
           _ -> error "System error:<module>.reuseEmpty"

reusePPPresentation :: [Maybe Node] -> Maybe IDD -> Maybe Bool_ -> Maybe List_Slide -> PPPresentation
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

reuseInv :: [Maybe Node] -> Maybe IDD -> Maybe EitherDocView -> Maybe View -> Maybe String_ -> Maybe EvalButton -> Inv
reuseInv nodes  ma0 ma1 ma2 ma3 ma4
  = case extractFromNodes extractInv defaultInv nodes of
           (Inv a0 a1 a2 a3 a4) -> reuse5 Inv a0 a1 a2 a3 a4 ma0 ma1 ma2 ma3 ma4
           _ -> error "System error:<module>.reuseInv"

reuseReEvaluate1 :: [Maybe Node] -> Maybe IDD -> EvalButton
reuseReEvaluate1 nodes  ma0
  = case extractFromNodes extractReEvaluate1 defaultReEvaluate1 nodes of
           (ReEvaluate1 a0) -> reuse1 ReEvaluate1 a0 ma0
           _ -> error "System error:<module>.reuseReEvaluate1"

reuseReEvaluate2 :: [Maybe Node] -> Maybe IDD -> EvalButton
reuseReEvaluate2 nodes  ma0
  = case extractFromNodes extractReEvaluate2 defaultReEvaluate2 nodes of
           (ReEvaluate2 a0) -> reuse1 ReEvaluate2 a0 ma0
           _ -> error "System error:<module>.reuseReEvaluate2"

reuseSkip :: [Maybe Node] -> Maybe IDD -> EvalButton
reuseSkip nodes  ma0
  = case extractFromNodes extractSkip defaultSkip nodes of
           (Skip a0) -> reuse1 Skip a0 ma0
           _ -> error "System error:<module>.reuseSkip"

reuseLeftDocView :: [Maybe Node] -> Maybe IDD -> Maybe String_ -> EitherDocView
reuseLeftDocView nodes  ma0 ma1
  = case extractFromNodes extractLeftDocView defaultLeftDocView nodes of
           (LeftDocView a0 a1) -> reuse2 LeftDocView a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseLeftDocView"

reuseRightDocView :: [Maybe Node] -> Maybe IDD -> Maybe View -> EitherDocView
reuseRightDocView nodes  ma0 ma1
  = case extractFromNodes extractRightDocView defaultRightDocView nodes of
           (RightDocView a0 a1) -> reuse2 RightDocView a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseRightDocView"

reuseANil :: [Maybe Node] -> Maybe IDD -> View
reuseANil nodes  ma0
  = case extractFromNodes extractANil defaultANil nodes of
           (ANil a0) -> reuse1 ANil a0 ma0
           _ -> error "System error:<module>.reuseANil"

reuseAN :: [Maybe Node] -> Maybe IDD -> Maybe Int_ -> View
reuseAN nodes  ma0 ma1
  = case extractFromNodes extractAN defaultAN nodes of
           (AN a0 a1) -> reuse2 AN a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseAN"

reuseAS :: [Maybe Node] -> Maybe IDD -> Maybe String_ -> View
reuseAS nodes  ma0 ma1
  = case extractFromNodes extractAS defaultAS nodes of
           (AS a0 a1) -> reuse2 AS a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseAS"

reusePr :: [Maybe Node] -> Maybe IDD -> Maybe View -> Maybe View -> View
reusePr nodes  ma0 ma1 ma2
  = case extractFromNodes extractPr defaultPr nodes of
           (Pr a0 a1 a2) -> reuse3 Pr a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reusePr"

reuseLs :: [Maybe Node] -> Maybe IDD -> Maybe View -> Maybe View -> View
reuseLs nodes  ma0 ma1 ma2
  = case extractFromNodes extractLs defaultLs nodes of
           (Ls a0 a1 a2) -> reuse3 Ls a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseLs"

reuseTr :: [Maybe Node] -> Maybe IDD -> Maybe View -> Maybe View -> View
reuseTr nodes  ma0 ma1 ma2
  = case extractFromNodes extractTr defaultTr nodes of
           (Tr a0 a1 a2) -> reuse3 Tr a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseTr"

reuseL :: [Maybe Node] -> Maybe IDD -> Maybe View -> View
reuseL nodes  ma0 ma1
  = case extractFromNodes extractL defaultL nodes of
           (L a0 a1) -> reuse2 L a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseL"

reuseR :: [Maybe Node] -> Maybe IDD -> Maybe View -> View
reuseR nodes  ma0 ma1
  = case extractFromNodes extractR defaultR nodes of
           (R a0 a1) -> reuse2 R a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseR"

reuseMark :: [Maybe Node] -> Maybe IDD -> Maybe View -> View
reuseMark nodes  ma0 ma1
  = case extractFromNodes extractMark defaultMark nodes of
           (Mark a0 a1) -> reuse2 Mark a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseMark"

reuseDelL :: [Maybe Node] -> Maybe IDD -> Maybe View -> Maybe View -> View
reuseDelL nodes  ma0 ma1 ma2
  = case extractFromNodes extractDelL defaultDelL nodes of
           (DelL a0 a1 a2) -> reuse3 DelL a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseDelL"

reuseInsL :: [Maybe Node] -> Maybe IDD -> Maybe View -> Maybe View -> View
reuseInsL nodes  ma0 ma1 ma2
  = case extractFromNodes extractInsL defaultInsL nodes of
           (InsL a0 a1 a2) -> reuse3 InsL a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseInsL"

reuseSndP :: [Maybe Node] -> Maybe IDD -> Maybe Bool_ -> Maybe View -> Maybe View -> View
reuseSndP nodes  ma0 ma1 ma2 ma3
  = case extractFromNodes extractSndP defaultSndP nodes of
           (SndP a0 a1 a2 a3) -> reuse4 SndP a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseSndP"

reuseFstP :: [Maybe Node] -> Maybe IDD -> Maybe Bool_ -> Maybe View -> Maybe View -> View
reuseFstP nodes  ma0 ma1 ma2 ma3
  = case extractFromNodes extractFstP defaultFstP nodes of
           (FstP a0 a1 a2 a3) -> reuse4 FstP a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "System error:<module>.reuseFstP"

reuseIfNil :: [Maybe Node] -> Maybe IDD -> Maybe Bool_ -> Maybe View -> View
reuseIfNil nodes  ma0 ma1 ma2
  = case extractFromNodes extractIfNil defaultIfNil nodes of
           (IfNil a0 a1 a2) -> reuse3 IfNil a0 a1 a2 ma0 ma1 ma2
           _ -> error "System error:<module>.reuseIfNil"

reuseUndef :: [Maybe Node] -> Maybe IDD -> View
reuseUndef nodes  ma0
  = case extractFromNodes extractUndef defaultUndef nodes of
           (Undef a0) -> reuse1 Undef a0 ma0
           _ -> error "System error:<module>.reuseUndef"

reuseUnit :: [Maybe Node] -> Maybe IDD -> View
reuseUnit nodes  ma0
  = case extractFromNodes extractUnit defaultUnit nodes of
           (Unit a0) -> reuse1 Unit a0 ma0
           _ -> error "System error:<module>.reuseUnit"

reuseString_ :: [Maybe Node] -> Maybe IDD -> Maybe String -> String_
reuseString_ nodes  ma0 ma1
  = case extractFromNodes extractString_ defaultString_ nodes of
           (String_ a0 a1) -> reuse2 String_ a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseString_"

reuseBool_ :: [Maybe Node] -> Maybe IDD -> Maybe Bool -> Bool_
reuseBool_ nodes  ma0 ma1
  = case extractFromNodes extractBool_ defaultBool_ nodes of
           (Bool_ a0 a1) -> reuse2 Bool_ a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseBool_"

reuseInt_ :: [Maybe Node] -> Maybe IDD -> Maybe Int -> Int_
reuseInt_ nodes  ma0 ma1
  = case extractFromNodes extractInt_ defaultInt_ nodes of
           (Int_ a0 a1) -> reuse2 Int_ a0 a1 ma0 ma1
           _ -> error "System error:<module>.reuseInt_"

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
extractRootEnr (Just (RootEnrNode x@(RootEnr _ _ _ _ _ _) _)) = Just x
extractRootEnr _ = Nothing

extractDecl :: Maybe Node -> Maybe Decl
extractDecl (Just (DeclNode x@(Decl _ _ _ _ _ _ _ _ _) _)) = Just x
extractDecl _ = Nothing

extractBoardDecl :: Maybe Node -> Maybe Decl
extractBoardDecl (Just (BoardDeclNode x@(BoardDecl _ _ _ _) _)) = Just x
extractBoardDecl _ = Nothing

extractPPPresentationDecl :: Maybe Node -> Maybe Decl
extractPPPresentationDecl (Just (PPPresentationDeclNode x@(PPPresentationDecl _ _ _ _) _)) = Just x
extractPPPresentationDecl _ = Nothing

extractInvDecl :: Maybe Node -> Maybe Decl
extractInvDecl (Just (InvDeclNode x@(InvDecl _ _ _ _) _)) = Just x
extractInvDecl _ = Nothing

extractIdent :: Maybe Node -> Maybe Ident
extractIdent (Just (IdentNode x@(Ident _ _ _ _) _)) = Just x
extractIdent _ = Nothing

extractPlusExp :: Maybe Node -> Maybe Exp
extractPlusExp (Just (PlusExpNode x@(PlusExp _ _ _ _) _)) = Just x
extractPlusExp _ = Nothing

extractTimesExp :: Maybe Node -> Maybe Exp
extractTimesExp (Just (TimesExpNode x@(TimesExp _ _ _ _) _)) = Just x
extractTimesExp _ = Nothing

extractDivExp :: Maybe Node -> Maybe Exp
extractDivExp (Just (DivExpNode x@(DivExp _ _ _ _) _)) = Just x
extractDivExp _ = Nothing

extractPowerExp :: Maybe Node -> Maybe Exp
extractPowerExp (Just (PowerExpNode x@(PowerExp _ _ _ _) _)) = Just x
extractPowerExp _ = Nothing

extractBoolExp :: Maybe Node -> Maybe Exp
extractBoolExp (Just (BoolExpNode x@(BoolExp _ _ _) _)) = Just x
extractBoolExp _ = Nothing

extractIntExp :: Maybe Node -> Maybe Exp
extractIntExp (Just (IntExpNode x@(IntExp _ _ _) _)) = Just x
extractIntExp _ = Nothing

extractLamExp :: Maybe Node -> Maybe Exp
extractLamExp (Just (LamExpNode x@(LamExp _ _ _ _ _) _)) = Just x
extractLamExp _ = Nothing

extractAppExp :: Maybe Node -> Maybe Exp
extractAppExp (Just (AppExpNode x@(AppExp _ _ _) _)) = Just x
extractAppExp _ = Nothing

extractCaseExp :: Maybe Node -> Maybe Exp
extractCaseExp (Just (CaseExpNode x@(CaseExp _ _ _ _ _) _)) = Just x
extractCaseExp _ = Nothing

extractLetExp :: Maybe Node -> Maybe Exp
extractLetExp (Just (LetExpNode x@(LetExp _ _ _ _ _) _)) = Just x
extractLetExp _ = Nothing

extractIdentExp :: Maybe Node -> Maybe Exp
extractIdentExp (Just (IdentExpNode x@(IdentExp _ _) _)) = Just x
extractIdentExp _ = Nothing

extractIfExp :: Maybe Node -> Maybe Exp
extractIfExp (Just (IfExpNode x@(IfExp _ _ _ _ _ _ _) _)) = Just x
extractIfExp _ = Nothing

extractParenExp :: Maybe Node -> Maybe Exp
extractParenExp (Just (ParenExpNode x@(ParenExp _ _ _ _) _)) = Just x
extractParenExp _ = Nothing

extractListExp :: Maybe Node -> Maybe Exp
extractListExp (Just (ListExpNode x@(ListExp _ _ _ _ _) _)) = Just x
extractListExp _ = Nothing

extractProductExp :: Maybe Node -> Maybe Exp
extractProductExp (Just (ProductExpNode x@(ProductExp _ _ _ _ _) _)) = Just x
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
extractQueen (Just (QueenNode x@(Queen _ _) _)) = Just x
extractQueen _ = Nothing

extractKing :: Maybe Node -> Maybe BoardSquare
extractKing (Just (KingNode x@(King _ _) _)) = Just x
extractKing _ = Nothing

extractBishop :: Maybe Node -> Maybe BoardSquare
extractBishop (Just (BishopNode x@(Bishop _ _) _)) = Just x
extractBishop _ = Nothing

extractKnight :: Maybe Node -> Maybe BoardSquare
extractKnight (Just (KnightNode x@(Knight _ _) _)) = Just x
extractKnight _ = Nothing

extractRook :: Maybe Node -> Maybe BoardSquare
extractRook (Just (RookNode x@(Rook _ _) _)) = Just x
extractRook _ = Nothing

extractPawn :: Maybe Node -> Maybe BoardSquare
extractPawn (Just (PawnNode x@(Pawn _ _) _)) = Just x
extractPawn _ = Nothing

extractEmpty :: Maybe Node -> Maybe BoardSquare
extractEmpty (Just (EmptyNode x@(Empty) _)) = Just x
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
extractBullet (Just (BulletNode x@(Bullet _) _)) = Just x
extractBullet _ = Nothing

extractNumber :: Maybe Node -> Maybe ListType
extractNumber (Just (NumberNode x@(Number _) _)) = Just x
extractNumber _ = Nothing

extractAlpha :: Maybe Node -> Maybe ListType
extractAlpha (Just (AlphaNode x@(Alpha _) _)) = Just x
extractAlpha _ = Nothing

extractStringItem :: Maybe Node -> Maybe Item
extractStringItem (Just (StringItemNode x@(StringItem _ _) _)) = Just x
extractStringItem _ = Nothing

extractHeliumItem :: Maybe Node -> Maybe Item
extractHeliumItem (Just (HeliumItemNode x@(HeliumItem _ _) _)) = Just x
extractHeliumItem _ = Nothing

extractListItem :: Maybe Node -> Maybe Item
extractListItem (Just (ListItemNode x@(ListItem _ _) _)) = Just x
extractListItem _ = Nothing

extractInv :: Maybe Node -> Maybe Inv
extractInv (Just (InvNode x@(Inv _ _ _ _ _) _)) = Just x
extractInv _ = Nothing

extractReEvaluate1 :: Maybe Node -> Maybe EvalButton
extractReEvaluate1 (Just (ReEvaluate1Node x@(ReEvaluate1 _) _)) = Just x
extractReEvaluate1 _ = Nothing

extractReEvaluate2 :: Maybe Node -> Maybe EvalButton
extractReEvaluate2 (Just (ReEvaluate2Node x@(ReEvaluate2 _) _)) = Just x
extractReEvaluate2 _ = Nothing

extractSkip :: Maybe Node -> Maybe EvalButton
extractSkip (Just (SkipNode x@(Skip _) _)) = Just x
extractSkip _ = Nothing

extractLeftDocView :: Maybe Node -> Maybe EitherDocView
extractLeftDocView (Just (LeftDocViewNode x@(LeftDocView _ _) _)) = Just x
extractLeftDocView _ = Nothing

extractRightDocView :: Maybe Node -> Maybe EitherDocView
extractRightDocView (Just (RightDocViewNode x@(RightDocView _ _) _)) = Just x
extractRightDocView _ = Nothing

extractANil :: Maybe Node -> Maybe View
extractANil (Just (ANilNode x@(ANil _) _)) = Just x
extractANil _ = Nothing

extractAN :: Maybe Node -> Maybe View
extractAN (Just (ANNode x@(AN _ _) _)) = Just x
extractAN _ = Nothing

extractAS :: Maybe Node -> Maybe View
extractAS (Just (ASNode x@(AS _ _) _)) = Just x
extractAS _ = Nothing

extractPr :: Maybe Node -> Maybe View
extractPr (Just (PrNode x@(Pr _ _ _) _)) = Just x
extractPr _ = Nothing

extractLs :: Maybe Node -> Maybe View
extractLs (Just (LsNode x@(Ls _ _ _) _)) = Just x
extractLs _ = Nothing

extractTr :: Maybe Node -> Maybe View
extractTr (Just (TrNode x@(Tr _ _ _) _)) = Just x
extractTr _ = Nothing

extractL :: Maybe Node -> Maybe View
extractL (Just (LNode x@(L _ _) _)) = Just x
extractL _ = Nothing

extractR :: Maybe Node -> Maybe View
extractR (Just (RNode x@(R _ _) _)) = Just x
extractR _ = Nothing

extractMark :: Maybe Node -> Maybe View
extractMark (Just (MarkNode x@(Mark _ _) _)) = Just x
extractMark _ = Nothing

extractDelL :: Maybe Node -> Maybe View
extractDelL (Just (DelLNode x@(DelL _ _ _) _)) = Just x
extractDelL _ = Nothing

extractInsL :: Maybe Node -> Maybe View
extractInsL (Just (InsLNode x@(InsL _ _ _) _)) = Just x
extractInsL _ = Nothing

extractSndP :: Maybe Node -> Maybe View
extractSndP (Just (SndPNode x@(SndP _ _ _ _) _)) = Just x
extractSndP _ = Nothing

extractFstP :: Maybe Node -> Maybe View
extractFstP (Just (FstPNode x@(FstP _ _ _ _) _)) = Just x
extractFstP _ = Nothing

extractIfNil :: Maybe Node -> Maybe View
extractIfNil (Just (IfNilNode x@(IfNil _ _ _) _)) = Just x
extractIfNil _ = Nothing

extractUndef :: Maybe Node -> Maybe View
extractUndef (Just (UndefNode x@(Undef _) _)) = Just x
extractUndef _ = Nothing

extractUnit :: Maybe Node -> Maybe View
extractUnit (Just (UnitNode x@(Unit _) _)) = Just x
extractUnit _ = Nothing

extractString_ :: Maybe Node -> Maybe String_
extractString_ (Just (String_Node x@(String_ _ _) _)) = Just x
extractString_ _ = Nothing

extractBool_ :: Maybe Node -> Maybe Bool_
extractBool_ (Just (Bool_Node x@(Bool_ _ _) _)) = Just x
extractBool_ _ = Nothing

extractInt_ :: Maybe Node -> Maybe Int_
extractInt_ (Just (Int_Node x@(Int_ _ _) _)) = Just x
extractInt_ _ = Nothing

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

defaultInvDecl :: Decl
defaultInvDecl = InvDecl NoIDD NoIDP NoIDP hole

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

defaultInv :: Inv
defaultInv = Inv NoIDD hole hole hole hole

defaultReEvaluate1 :: EvalButton
defaultReEvaluate1 = ReEvaluate1 NoIDD

defaultReEvaluate2 :: EvalButton
defaultReEvaluate2 = ReEvaluate2 NoIDD

defaultSkip :: EvalButton
defaultSkip = Skip NoIDD

defaultLeftDocView :: EitherDocView
defaultLeftDocView = LeftDocView NoIDD hole

defaultRightDocView :: EitherDocView
defaultRightDocView = RightDocView NoIDD hole

defaultANil :: View
defaultANil = ANil NoIDD

defaultAN :: View
defaultAN = AN NoIDD hole

defaultAS :: View
defaultAS = AS NoIDD hole

defaultPr :: View
defaultPr = Pr NoIDD hole hole

defaultLs :: View
defaultLs = Ls NoIDD hole hole

defaultTr :: View
defaultTr = Tr NoIDD hole hole

defaultL :: View
defaultL = L NoIDD hole

defaultR :: View
defaultR = R NoIDD hole

defaultMark :: View
defaultMark = Mark NoIDD hole

defaultDelL :: View
defaultDelL = DelL NoIDD hole hole

defaultInsL :: View
defaultInsL = InsL NoIDD hole hole

defaultSndP :: View
defaultSndP = SndP NoIDD hole hole hole

defaultFstP :: View
defaultFstP = FstP NoIDD hole hole hole

defaultIfNil :: View
defaultIfNil = IfNil NoIDD hole hole

defaultUndef :: View
defaultUndef = Undef NoIDD

defaultUnit :: View
defaultUnit = Unit NoIDD

defaultString_ :: String_
defaultString_ = String_ NoIDD hole

defaultBool_ :: Bool_
defaultBool_ = Bool_ NoIDD hole

defaultInt_ :: Int_
defaultInt_ = Int_ NoIDD hole

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

