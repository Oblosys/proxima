module ProxParser_Generated where

import Common.CommonTypes
import Presentation.PresLayerTypes
import Presentation.PresLayerUtils
import Presentation.PresentationParsing
import Evaluation.DocumentEdit
import DocumentEdit_Generated
import Evaluation.DocTypes
import DocTypes_Generated
import DocUtils_Generated

----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- ProxParser type synonym                                              --
--------------------------------------------------------------------------

type ProxParser a = ListParser Document EnrichedDoc Node ClipDoc UserToken a



--------------------------------------------------------------------------
-- Construct instance                                                   --
--------------------------------------------------------------------------

instance Construct Document EnrichedDoc Node ClipDoc UserToken where
  construct NoNode = error $ "ProxParser_Generated.construct not defined on NoNode"
  construct (Node_RootDoc _ _) = construct_RootDoc
  construct (Node_HoleDocument _ _) = construct_HoleDocument
  construct (Node_ParseErrDocument _ _) = construct_ParseErrDocument
  construct (Node_Root _ _) = construct_Root
  construct (Node_HoleRoot _ _) = construct_HoleRoot
  construct (Node_ParseErrRoot _ _) = construct_ParseErrRoot
  construct (Node_RootEnr _ _) = construct_RootEnr
  construct (Node_HoleEnrichedDoc _ _) = construct_HoleEnrichedDoc
  construct (Node_ParseErrEnrichedDoc _ _) = construct_ParseErrEnrichedDoc
  construct (Node_RootE _ _) = construct_RootE
  construct (Node_HoleRootE _ _) = construct_HoleRootE
  construct (Node_ParseErrRootE _ _) = construct_ParseErrRootE
  construct (Node_Decl _ _) = construct_Decl
  construct (Node_BoardDecl _ _) = construct_BoardDecl
  construct (Node_PPPresentationDecl _ _) = construct_PPPresentationDecl
  construct (Node_HoleDecl _ _) = construct_HoleDecl
  construct (Node_ParseErrDecl _ _) = construct_ParseErrDecl
  construct (Node_Ident _ _) = construct_Ident
  construct (Node_HoleIdent _ _) = construct_HoleIdent
  construct (Node_ParseErrIdent _ _) = construct_ParseErrIdent
  construct (Node_PlusExp _ _) = construct_PlusExp
  construct (Node_TimesExp _ _) = construct_TimesExp
  construct (Node_DivExp _ _) = construct_DivExp
  construct (Node_PowerExp _ _) = construct_PowerExp
  construct (Node_BoolExp _ _) = construct_BoolExp
  construct (Node_IntExp _ _) = construct_IntExp
  construct (Node_LamExp _ _) = construct_LamExp
  construct (Node_AppExp _ _) = construct_AppExp
  construct (Node_CaseExp _ _) = construct_CaseExp
  construct (Node_LetExp _ _) = construct_LetExp
  construct (Node_IdentExp _ _) = construct_IdentExp
  construct (Node_IfExp _ _) = construct_IfExp
  construct (Node_ParenExp _ _) = construct_ParenExp
  construct (Node_ListExp _ _) = construct_ListExp
  construct (Node_ProductExp _ _) = construct_ProductExp
  construct (Node_HoleExp _ _) = construct_HoleExp
  construct (Node_ParseErrExp _ _) = construct_ParseErrExp
  construct (Node_Alt _ _) = construct_Alt
  construct (Node_HoleAlt _ _) = construct_HoleAlt
  construct (Node_ParseErrAlt _ _) = construct_ParseErrAlt
  construct (Node_Board _ _) = construct_Board
  construct (Node_HoleBoard _ _) = construct_HoleBoard
  construct (Node_ParseErrBoard _ _) = construct_ParseErrBoard
  construct (Node_BoardRow _ _) = construct_BoardRow
  construct (Node_HoleBoardRow _ _) = construct_HoleBoardRow
  construct (Node_ParseErrBoardRow _ _) = construct_ParseErrBoardRow
  construct (Node_Queen _ _) = construct_Queen
  construct (Node_King _ _) = construct_King
  construct (Node_Bishop _ _) = construct_Bishop
  construct (Node_Knight _ _) = construct_Knight
  construct (Node_Rook _ _) = construct_Rook
  construct (Node_Pawn _ _) = construct_Pawn
  construct (Node_Empty _ _) = construct_Empty
  construct (Node_HoleBoardSquare _ _) = construct_HoleBoardSquare
  construct (Node_ParseErrBoardSquare _ _) = construct_ParseErrBoardSquare
  construct (Node_PPPresentation _ _) = construct_PPPresentation
  construct (Node_HolePPPresentation _ _) = construct_HolePPPresentation
  construct (Node_ParseErrPPPresentation _ _) = construct_ParseErrPPPresentation
  construct (Node_Slide _ _) = construct_Slide
  construct (Node_HoleSlide _ _) = construct_HoleSlide
  construct (Node_ParseErrSlide _ _) = construct_ParseErrSlide
  construct (Node_ItemList _ _) = construct_ItemList
  construct (Node_HoleItemList _ _) = construct_HoleItemList
  construct (Node_ParseErrItemList _ _) = construct_ParseErrItemList
  construct (Node_Bullet _ _) = construct_Bullet
  construct (Node_Number _ _) = construct_Number
  construct (Node_Alpha _ _) = construct_Alpha
  construct (Node_HoleListType _ _) = construct_HoleListType
  construct (Node_ParseErrListType _ _) = construct_ParseErrListType
  construct (Node_StringItem _ _) = construct_StringItem
  construct (Node_HeliumItem _ _) = construct_HeliumItem
  construct (Node_ListItem _ _) = construct_ListItem
  construct (Node_HoleItem _ _) = construct_HoleItem
  construct (Node_ParseErrItem _ _) = construct_ParseErrItem
  construct (Node_List_Decl _ _) = construct_List_Decl
  construct (Node_HoleList_Decl _ _) = construct_HoleList_Decl
  construct (Node_ParseErrList_Decl _ _) = construct_ParseErrList_Decl
  construct (Node_List_Alt _ _) = construct_List_Alt
  construct (Node_HoleList_Alt _ _) = construct_HoleList_Alt
  construct (Node_ParseErrList_Alt _ _) = construct_ParseErrList_Alt
  construct (Node_List_Exp _ _) = construct_List_Exp
  construct (Node_HoleList_Exp _ _) = construct_HoleList_Exp
  construct (Node_ParseErrList_Exp _ _) = construct_ParseErrList_Exp
  construct (Node_List_Slide _ _) = construct_List_Slide
  construct (Node_HoleList_Slide _ _) = construct_HoleList_Slide
  construct (Node_ParseErrList_Slide _ _) = construct_ParseErrList_Slide
  construct (Node_List_Item _ _) = construct_List_Item
  construct (Node_HoleList_Item _ _) = construct_HoleList_Item
  construct (Node_ParseErrList_Item _ _) = construct_ParseErrList_Item
construct_RootDoc tk ~[mClip0] = Clip_Document $ reuseRootDoc [tk]  (retrieveArg "RootDoc" "root::Root" mClip0)
construct_HoleDocument tk ~[] = Clip_Document $ hole
construct_ParseErrDocument (StructuralTk _ _ pres _ _) ~[] = Clip_Document $ parseErr (StructuralParseErr pres)
construct_Root tk ~[mClip0] = Clip_Root $ reuseRoot [tk]  Nothing (retrieveArg "Root" "decls::List_Decl" mClip0)
construct_HoleRoot tk ~[] = Clip_Root $ hole
construct_ParseErrRoot (StructuralTk _ _ pres _ _) ~[] = Clip_Root $ parseErr (StructuralParseErr pres)
construct_RootEnr tk ~[mClip0,mClip1] = Clip_EnrichedDoc $ reuseRootEnr [tk]  (retrieveArg "RootEnr" "root::RootE" mClip0) (retrieveArg "RootEnr" "heliumTypeInfo::HeliumTypeInfo" mClip1)
construct_HoleEnrichedDoc tk ~[] = Clip_EnrichedDoc $ hole
construct_ParseErrEnrichedDoc (StructuralTk _ _ pres _ _) ~[] = Clip_EnrichedDoc $ parseErr (StructuralParseErr pres)
construct_RootE tk ~[mClip0,mClip1] = Clip_RootE $ reuseRootE [tk]  Nothing (retrieveArg "RootE" "decls::List_Decl" mClip0) (retrieveArg "RootE" "idListDecls::List_Decl" mClip1)
construct_HoleRootE tk ~[] = Clip_RootE $ hole
construct_ParseErrRootE (StructuralTk _ _ pres _ _) ~[] = Clip_RootE $ parseErr (StructuralParseErr pres)
construct_Decl tk ~[mClip0,mClip1,mClip2,mClip3] = Clip_Decl $ reuseDecl [tk]  Nothing Nothing Nothing Nothing (retrieveArg "Decl" "expanded::Bool" mClip0) (retrieveArg "Decl" "autoLayout::Bool" mClip1) (retrieveArg "Decl" "ident::Ident" mClip2) (retrieveArg "Decl" "exp::Exp" mClip3)
construct_BoardDecl tk ~[mClip0] = Clip_Decl $ reuseBoardDecl [tk]  Nothing Nothing (retrieveArg "BoardDecl" "board::Board" mClip0)
construct_PPPresentationDecl tk ~[mClip0] = Clip_Decl $ reusePPPresentationDecl [tk]  Nothing Nothing (retrieveArg "PPPresentationDecl" "pPPresentation::PPPresentation" mClip0)
construct_HoleDecl tk ~[] = Clip_Decl $ hole
construct_ParseErrDecl (StructuralTk _ _ pres _ _) ~[] = Clip_Decl $ parseErr (StructuralParseErr pres)
construct_Ident tk ~[mClip0] = Clip_Ident $ reuseIdent [tk]  Nothing Nothing (retrieveArg "Ident" "string::String" mClip0)
construct_HoleIdent tk ~[] = Clip_Ident $ hole
construct_ParseErrIdent (StructuralTk _ _ pres _ _) ~[] = Clip_Ident $ parseErr (StructuralParseErr pres)
construct_PlusExp tk ~[mClip0,mClip1] = Clip_Exp $ reusePlusExp [tk]  Nothing (retrieveArg "PlusExp" "exp1::Exp" mClip0) (retrieveArg "PlusExp" "exp2::Exp" mClip1)
construct_TimesExp tk ~[mClip0,mClip1] = Clip_Exp $ reuseTimesExp [tk]  Nothing (retrieveArg "TimesExp" "exp1::Exp" mClip0) (retrieveArg "TimesExp" "exp2::Exp" mClip1)
construct_DivExp tk ~[mClip0,mClip1] = Clip_Exp $ reuseDivExp [tk]  Nothing (retrieveArg "DivExp" "exp1::Exp" mClip0) (retrieveArg "DivExp" "exp2::Exp" mClip1)
construct_PowerExp tk ~[mClip0,mClip1] = Clip_Exp $ reusePowerExp [tk]  Nothing (retrieveArg "PowerExp" "exp1::Exp" mClip0) (retrieveArg "PowerExp" "exp2::Exp" mClip1)
construct_BoolExp tk ~[mClip0] = Clip_Exp $ reuseBoolExp [tk]  Nothing (retrieveArg "BoolExp" "bool::Bool" mClip0)
construct_IntExp tk ~[mClip0] = Clip_Exp $ reuseIntExp [tk]  Nothing (retrieveArg "IntExp" "int::Int" mClip0)
construct_LamExp tk ~[mClip0,mClip1] = Clip_Exp $ reuseLamExp [tk]  Nothing Nothing (retrieveArg "LamExp" "ident::Ident" mClip0) (retrieveArg "LamExp" "exp::Exp" mClip1)
construct_AppExp tk ~[mClip0,mClip1] = Clip_Exp $ reuseAppExp [tk]  (retrieveArg "AppExp" "exp1::Exp" mClip0) (retrieveArg "AppExp" "exp2::Exp" mClip1)
construct_CaseExp tk ~[mClip0,mClip1] = Clip_Exp $ reuseCaseExp [tk]  Nothing Nothing (retrieveArg "CaseExp" "exp::Exp" mClip0) (retrieveArg "CaseExp" "alts::List_Alt" mClip1)
construct_LetExp tk ~[mClip0,mClip1] = Clip_Exp $ reuseLetExp [tk]  Nothing Nothing (retrieveArg "LetExp" "decls::List_Decl" mClip0) (retrieveArg "LetExp" "exp::Exp" mClip1)
construct_IdentExp tk ~[mClip0] = Clip_Exp $ reuseIdentExp [tk]  (retrieveArg "IdentExp" "ident::Ident" mClip0)
construct_IfExp tk ~[mClip0,mClip1,mClip2] = Clip_Exp $ reuseIfExp [tk]  Nothing Nothing Nothing (retrieveArg "IfExp" "exp1::Exp" mClip0) (retrieveArg "IfExp" "exp2::Exp" mClip1) (retrieveArg "IfExp" "exp3::Exp" mClip2)
construct_ParenExp tk ~[mClip0] = Clip_Exp $ reuseParenExp [tk]  Nothing Nothing (retrieveArg "ParenExp" "exp::Exp" mClip0)
construct_ListExp tk ~[mClip0] = Clip_Exp $ reuseListExp [tk]  Nothing Nothing Nothing (retrieveArg "ListExp" "exps::List_Exp" mClip0)
construct_ProductExp tk ~[mClip0] = Clip_Exp $ reuseProductExp [tk]  Nothing Nothing Nothing (retrieveArg "ProductExp" "exps::List_Exp" mClip0)
construct_HoleExp tk ~[] = Clip_Exp $ hole
construct_ParseErrExp (StructuralTk _ _ pres _ _) ~[] = Clip_Exp $ parseErr (StructuralParseErr pres)
construct_Alt tk ~[mClip0,mClip1] = Clip_Alt $ reuseAlt [tk]  Nothing Nothing (retrieveArg "Alt" "ident::Ident" mClip0) (retrieveArg "Alt" "exp::Exp" mClip1)
construct_HoleAlt tk ~[] = Clip_Alt $ hole
construct_ParseErrAlt (StructuralTk _ _ pres _ _) ~[] = Clip_Alt $ parseErr (StructuralParseErr pres)
construct_Board tk ~[mClip0,mClip1,mClip2,mClip3,mClip4,mClip5,mClip6,mClip7] = Clip_Board $ reuseBoard [tk]  (retrieveArg "Board" "r1::BoardRow" mClip0) (retrieveArg "Board" "r2::BoardRow" mClip1) (retrieveArg "Board" "r3::BoardRow" mClip2) (retrieveArg "Board" "r4::BoardRow" mClip3) (retrieveArg "Board" "r5::BoardRow" mClip4) (retrieveArg "Board" "r6::BoardRow" mClip5) (retrieveArg "Board" "r7::BoardRow" mClip6) (retrieveArg "Board" "r8::BoardRow" mClip7)
construct_HoleBoard tk ~[] = Clip_Board $ hole
construct_ParseErrBoard (StructuralTk _ _ pres _ _) ~[] = Clip_Board $ parseErr (StructuralParseErr pres)
construct_BoardRow tk ~[mClip0,mClip1,mClip2,mClip3,mClip4,mClip5,mClip6,mClip7] = Clip_BoardRow $ reuseBoardRow [tk]  (retrieveArg "BoardRow" "ca::BoardSquare" mClip0) (retrieveArg "BoardRow" "cb::BoardSquare" mClip1) (retrieveArg "BoardRow" "cc::BoardSquare" mClip2) (retrieveArg "BoardRow" "cd::BoardSquare" mClip3) (retrieveArg "BoardRow" "ce::BoardSquare" mClip4) (retrieveArg "BoardRow" "cf::BoardSquare" mClip5) (retrieveArg "BoardRow" "cg::BoardSquare" mClip6) (retrieveArg "BoardRow" "ch::BoardSquare" mClip7)
construct_HoleBoardRow tk ~[] = Clip_BoardRow $ hole
construct_ParseErrBoardRow (StructuralTk _ _ pres _ _) ~[] = Clip_BoardRow $ parseErr (StructuralParseErr pres)
construct_Queen tk ~[mClip0] = Clip_BoardSquare $ reuseQueen [tk]  (retrieveArg "Queen" "color::Bool" mClip0)
construct_King tk ~[mClip0] = Clip_BoardSquare $ reuseKing [tk]  (retrieveArg "King" "color::Bool" mClip0)
construct_Bishop tk ~[mClip0] = Clip_BoardSquare $ reuseBishop [tk]  (retrieveArg "Bishop" "color::Bool" mClip0)
construct_Knight tk ~[mClip0] = Clip_BoardSquare $ reuseKnight [tk]  (retrieveArg "Knight" "color::Bool" mClip0)
construct_Rook tk ~[mClip0] = Clip_BoardSquare $ reuseRook [tk]  (retrieveArg "Rook" "color::Bool" mClip0)
construct_Pawn tk ~[mClip0] = Clip_BoardSquare $ reusePawn [tk]  (retrieveArg "Pawn" "color::Bool" mClip0)
construct_Empty tk ~[] = Clip_BoardSquare $ reuseEmpty [tk] 
construct_HoleBoardSquare tk ~[] = Clip_BoardSquare $ hole
construct_ParseErrBoardSquare (StructuralTk _ _ pres _ _) ~[] = Clip_BoardSquare $ parseErr (StructuralParseErr pres)
construct_PPPresentation tk ~[mClip0,mClip1] = Clip_PPPresentation $ reusePPPresentation [tk]  (retrieveArg "PPPresentation" "viewType::Bool" mClip0) (retrieveArg "PPPresentation" "slides::List_Slide" mClip1)
construct_HolePPPresentation tk ~[] = Clip_PPPresentation $ hole
construct_ParseErrPPPresentation (StructuralTk _ _ pres _ _) ~[] = Clip_PPPresentation $ parseErr (StructuralParseErr pres)
construct_Slide tk ~[mClip0,mClip1] = Clip_Slide $ reuseSlide [tk]  (retrieveArg "Slide" "title::String" mClip0) (retrieveArg "Slide" "itemList::ItemList" mClip1)
construct_HoleSlide tk ~[] = Clip_Slide $ hole
construct_ParseErrSlide (StructuralTk _ _ pres _ _) ~[] = Clip_Slide $ parseErr (StructuralParseErr pres)
construct_ItemList tk ~[mClip0,mClip1] = Clip_ItemList $ reuseItemList [tk]  (retrieveArg "ItemList" "listType::ListType" mClip0) (retrieveArg "ItemList" "items::List_Item" mClip1)
construct_HoleItemList tk ~[] = Clip_ItemList $ hole
construct_ParseErrItemList (StructuralTk _ _ pres _ _) ~[] = Clip_ItemList $ parseErr (StructuralParseErr pres)
construct_Bullet tk ~[] = Clip_ListType $ reuseBullet [tk] 
construct_Number tk ~[] = Clip_ListType $ reuseNumber [tk] 
construct_Alpha tk ~[] = Clip_ListType $ reuseAlpha [tk] 
construct_HoleListType tk ~[] = Clip_ListType $ hole
construct_ParseErrListType (StructuralTk _ _ pres _ _) ~[] = Clip_ListType $ parseErr (StructuralParseErr pres)
construct_StringItem tk ~[mClip0] = Clip_Item $ reuseStringItem [tk]  (retrieveArg "StringItem" "string::String" mClip0)
construct_HeliumItem tk ~[mClip0] = Clip_Item $ reuseHeliumItem [tk]  (retrieveArg "HeliumItem" "exp::Exp" mClip0)
construct_ListItem tk ~[mClip0] = Clip_Item $ reuseListItem [tk]  (retrieveArg "ListItem" "itemList::ItemList" mClip0)
construct_HoleItem tk ~[] = Clip_Item $ hole
construct_ParseErrItem (StructuralTk _ _ pres _ _) ~[] = Clip_Item $ parseErr (StructuralParseErr pres)
construct_List_Decl tk mClips = genericConstruct_List "Decl" toList_Decl mClips
construct_HoleList_Decl tk ~[] = Clip_List_Decl $ hole
construct_ParseErrList_Decl (StructuralTk _ _ pres _ _) ~[] = Clip_List_Decl $ parseErr (StructuralParseErr pres)
construct_List_Alt tk mClips = genericConstruct_List "Alt" toList_Alt mClips
construct_HoleList_Alt tk ~[] = Clip_List_Alt $ hole
construct_ParseErrList_Alt (StructuralTk _ _ pres _ _) ~[] = Clip_List_Alt $ parseErr (StructuralParseErr pres)
construct_List_Exp tk mClips = genericConstruct_List "Exp" toList_Exp mClips
construct_HoleList_Exp tk ~[] = Clip_List_Exp $ hole
construct_ParseErrList_Exp (StructuralTk _ _ pres _ _) ~[] = Clip_List_Exp $ parseErr (StructuralParseErr pres)
construct_List_Slide tk mClips = genericConstruct_List "Slide" toList_Slide mClips
construct_HoleList_Slide tk ~[] = Clip_List_Slide $ hole
construct_ParseErrList_Slide (StructuralTk _ _ pres _ _) ~[] = Clip_List_Slide $ parseErr (StructuralParseErr pres)
construct_List_Item tk mClips = genericConstruct_List "Item" toList_Item mClips
construct_HoleList_Item tk ~[] = Clip_List_Item $ hole
construct_ParseErrList_Item (StructuralTk _ _ pres _ _) ~[] = Clip_List_Item $ parseErr (StructuralParseErr pres)



--------------------------------------------------------------------------
-- reuse functions                                                      --
--------------------------------------------------------------------------

reuseRootDoc :: [Token doc enr Node clip token] -> Maybe Root -> Document
reuseRootDoc nodes ma0
  = case extractFromTokens extractRootDoc defaultRootDoc nodes of
           (RootDoc a0) -> genericReuse1 RootDoc a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseRootDoc"

reuseRoot :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe List_Decl -> Root
reuseRoot nodes ma0 ma1
  = case extractFromTokens extractRoot defaultRoot nodes of
           (Root a0 a1) -> genericReuse2 Root a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseRoot"

reuseRootEnr :: [Token doc enr Node clip token] -> Maybe RootE -> Maybe HeliumTypeInfo -> EnrichedDoc
reuseRootEnr nodes ma0 ma1
  = case extractFromTokens extractRootEnr defaultRootEnr nodes of
           (RootEnr a0 a1) -> genericReuse2 RootEnr a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseRootEnr"

reuseRootE :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe List_Decl -> Maybe List_Decl -> RootE
reuseRootE nodes ma0 ma1 ma2
  = case extractFromTokens extractRootE defaultRootE nodes of
           (RootE a0 a1 a2) -> genericReuse3 RootE a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reuseRootE"

reuseDecl :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe IDP -> Maybe IDP -> Maybe Bool -> Maybe Bool -> Maybe Ident -> Maybe Exp -> Decl
reuseDecl nodes ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7
  = case extractFromTokens extractDecl defaultDecl nodes of
           (Decl a0 a1 a2 a3 a4 a5 a6 a7) -> genericReuse8 Decl a0 a1 a2 a3 a4 a5 a6 a7 ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7
           _ -> error "Internal error:ProxParser_Generated.reuseDecl"

reuseBoardDecl :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe Board -> Decl
reuseBoardDecl nodes ma0 ma1 ma2
  = case extractFromTokens extractBoardDecl defaultBoardDecl nodes of
           (BoardDecl a0 a1 a2) -> genericReuse3 BoardDecl a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reuseBoardDecl"

reusePPPresentationDecl :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe PPPresentation -> Decl
reusePPPresentationDecl nodes ma0 ma1 ma2
  = case extractFromTokens extractPPPresentationDecl defaultPPPresentationDecl nodes of
           (PPPresentationDecl a0 a1 a2) -> genericReuse3 PPPresentationDecl a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reusePPPresentationDecl"

reuseIdent :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe String -> Ident
reuseIdent nodes ma0 ma1 ma2
  = case extractFromTokens extractIdent defaultIdent nodes of
           (Ident a0 a1 a2) -> genericReuse3 Ident a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reuseIdent"

reusePlusExp :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe Exp -> Maybe Exp -> Exp
reusePlusExp nodes ma0 ma1 ma2
  = case extractFromTokens extractPlusExp defaultPlusExp nodes of
           (PlusExp a0 a1 a2) -> genericReuse3 PlusExp a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reusePlusExp"

reuseTimesExp :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe Exp -> Maybe Exp -> Exp
reuseTimesExp nodes ma0 ma1 ma2
  = case extractFromTokens extractTimesExp defaultTimesExp nodes of
           (TimesExp a0 a1 a2) -> genericReuse3 TimesExp a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reuseTimesExp"

reuseDivExp :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe Exp -> Maybe Exp -> Exp
reuseDivExp nodes ma0 ma1 ma2
  = case extractFromTokens extractDivExp defaultDivExp nodes of
           (DivExp a0 a1 a2) -> genericReuse3 DivExp a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reuseDivExp"

reusePowerExp :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe Exp -> Maybe Exp -> Exp
reusePowerExp nodes ma0 ma1 ma2
  = case extractFromTokens extractPowerExp defaultPowerExp nodes of
           (PowerExp a0 a1 a2) -> genericReuse3 PowerExp a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reusePowerExp"

reuseBoolExp :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe Bool -> Exp
reuseBoolExp nodes ma0 ma1
  = case extractFromTokens extractBoolExp defaultBoolExp nodes of
           (BoolExp a0 a1) -> genericReuse2 BoolExp a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseBoolExp"

reuseIntExp :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe Int -> Exp
reuseIntExp nodes ma0 ma1
  = case extractFromTokens extractIntExp defaultIntExp nodes of
           (IntExp a0 a1) -> genericReuse2 IntExp a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseIntExp"

reuseLamExp :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe Ident -> Maybe Exp -> Exp
reuseLamExp nodes ma0 ma1 ma2 ma3
  = case extractFromTokens extractLamExp defaultLamExp nodes of
           (LamExp a0 a1 a2 a3) -> genericReuse4 LamExp a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "Internal error:ProxParser_Generated.reuseLamExp"

reuseAppExp :: [Token doc enr Node clip token] -> Maybe Exp -> Maybe Exp -> Exp
reuseAppExp nodes ma0 ma1
  = case extractFromTokens extractAppExp defaultAppExp nodes of
           (AppExp a0 a1) -> genericReuse2 AppExp a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseAppExp"

reuseCaseExp :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe Exp -> Maybe List_Alt -> Exp
reuseCaseExp nodes ma0 ma1 ma2 ma3
  = case extractFromTokens extractCaseExp defaultCaseExp nodes of
           (CaseExp a0 a1 a2 a3) -> genericReuse4 CaseExp a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "Internal error:ProxParser_Generated.reuseCaseExp"

reuseLetExp :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe List_Decl -> Maybe Exp -> Exp
reuseLetExp nodes ma0 ma1 ma2 ma3
  = case extractFromTokens extractLetExp defaultLetExp nodes of
           (LetExp a0 a1 a2 a3) -> genericReuse4 LetExp a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "Internal error:ProxParser_Generated.reuseLetExp"

reuseIdentExp :: [Token doc enr Node clip token] -> Maybe Ident -> Exp
reuseIdentExp nodes ma0
  = case extractFromTokens extractIdentExp defaultIdentExp nodes of
           (IdentExp a0) -> genericReuse1 IdentExp a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseIdentExp"

reuseIfExp :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe IDP -> Maybe Exp -> Maybe Exp -> Maybe Exp -> Exp
reuseIfExp nodes ma0 ma1 ma2 ma3 ma4 ma5
  = case extractFromTokens extractIfExp defaultIfExp nodes of
           (IfExp a0 a1 a2 a3 a4 a5) -> genericReuse6 IfExp a0 a1 a2 a3 a4 a5 ma0 ma1 ma2 ma3 ma4 ma5
           _ -> error "Internal error:ProxParser_Generated.reuseIfExp"

reuseParenExp :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe Exp -> Exp
reuseParenExp nodes ma0 ma1 ma2
  = case extractFromTokens extractParenExp defaultParenExp nodes of
           (ParenExp a0 a1 a2) -> genericReuse3 ParenExp a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reuseParenExp"

reuseListExp :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe [IDP] -> Maybe List_Exp -> Exp
reuseListExp nodes ma0 ma1 ma2 ma3
  = case extractFromTokens extractListExp defaultListExp nodes of
           (ListExp a0 a1 a2 a3) -> genericReuse4 ListExp a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "Internal error:ProxParser_Generated.reuseListExp"

reuseProductExp :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe [IDP] -> Maybe List_Exp -> Exp
reuseProductExp nodes ma0 ma1 ma2 ma3
  = case extractFromTokens extractProductExp defaultProductExp nodes of
           (ProductExp a0 a1 a2 a3) -> genericReuse4 ProductExp a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "Internal error:ProxParser_Generated.reuseProductExp"

reuseAlt :: [Token doc enr Node clip token] -> Maybe IDP -> Maybe IDP -> Maybe Ident -> Maybe Exp -> Alt
reuseAlt nodes ma0 ma1 ma2 ma3
  = case extractFromTokens extractAlt defaultAlt nodes of
           (Alt a0 a1 a2 a3) -> genericReuse4 Alt a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "Internal error:ProxParser_Generated.reuseAlt"

reuseBoard :: [Token doc enr Node clip token] -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Maybe BoardRow -> Board
reuseBoard nodes ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7
  = case extractFromTokens extractBoard defaultBoard nodes of
           (Board a0 a1 a2 a3 a4 a5 a6 a7) -> genericReuse8 Board a0 a1 a2 a3 a4 a5 a6 a7 ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7
           _ -> error "Internal error:ProxParser_Generated.reuseBoard"

reuseBoardRow :: [Token doc enr Node clip token] -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> Maybe BoardSquare -> BoardRow
reuseBoardRow nodes ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7
  = case extractFromTokens extractBoardRow defaultBoardRow nodes of
           (BoardRow a0 a1 a2 a3 a4 a5 a6 a7) -> genericReuse8 BoardRow a0 a1 a2 a3 a4 a5 a6 a7 ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7
           _ -> error "Internal error:ProxParser_Generated.reuseBoardRow"

reuseQueen :: [Token doc enr Node clip token] -> Maybe Bool -> BoardSquare
reuseQueen nodes ma0
  = case extractFromTokens extractQueen defaultQueen nodes of
           (Queen a0) -> genericReuse1 Queen a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseQueen"

reuseKing :: [Token doc enr Node clip token] -> Maybe Bool -> BoardSquare
reuseKing nodes ma0
  = case extractFromTokens extractKing defaultKing nodes of
           (King a0) -> genericReuse1 King a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseKing"

reuseBishop :: [Token doc enr Node clip token] -> Maybe Bool -> BoardSquare
reuseBishop nodes ma0
  = case extractFromTokens extractBishop defaultBishop nodes of
           (Bishop a0) -> genericReuse1 Bishop a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseBishop"

reuseKnight :: [Token doc enr Node clip token] -> Maybe Bool -> BoardSquare
reuseKnight nodes ma0
  = case extractFromTokens extractKnight defaultKnight nodes of
           (Knight a0) -> genericReuse1 Knight a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseKnight"

reuseRook :: [Token doc enr Node clip token] -> Maybe Bool -> BoardSquare
reuseRook nodes ma0
  = case extractFromTokens extractRook defaultRook nodes of
           (Rook a0) -> genericReuse1 Rook a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseRook"

reusePawn :: [Token doc enr Node clip token] -> Maybe Bool -> BoardSquare
reusePawn nodes ma0
  = case extractFromTokens extractPawn defaultPawn nodes of
           (Pawn a0) -> genericReuse1 Pawn a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reusePawn"

reuseEmpty :: [Token doc enr Node clip token] -> BoardSquare
reuseEmpty nodes
  = case extractFromTokens extractEmpty defaultEmpty nodes of
           (Empty) -> genericReuse0 Empty
           _ -> error "Internal error:ProxParser_Generated.reuseEmpty"

reusePPPresentation :: [Token doc enr Node clip token] -> Maybe Bool -> Maybe List_Slide -> PPPresentation
reusePPPresentation nodes ma0 ma1
  = case extractFromTokens extractPPPresentation defaultPPPresentation nodes of
           (PPPresentation a0 a1) -> genericReuse2 PPPresentation a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reusePPPresentation"

reuseSlide :: [Token doc enr Node clip token] -> Maybe String -> Maybe ItemList -> Slide
reuseSlide nodes ma0 ma1
  = case extractFromTokens extractSlide defaultSlide nodes of
           (Slide a0 a1) -> genericReuse2 Slide a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseSlide"

reuseItemList :: [Token doc enr Node clip token] -> Maybe ListType -> Maybe List_Item -> ItemList
reuseItemList nodes ma0 ma1
  = case extractFromTokens extractItemList defaultItemList nodes of
           (ItemList a0 a1) -> genericReuse2 ItemList a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseItemList"

reuseBullet :: [Token doc enr Node clip token] -> ListType
reuseBullet nodes
  = case extractFromTokens extractBullet defaultBullet nodes of
           (Bullet) -> genericReuse0 Bullet
           _ -> error "Internal error:ProxParser_Generated.reuseBullet"

reuseNumber :: [Token doc enr Node clip token] -> ListType
reuseNumber nodes
  = case extractFromTokens extractNumber defaultNumber nodes of
           (Number) -> genericReuse0 Number
           _ -> error "Internal error:ProxParser_Generated.reuseNumber"

reuseAlpha :: [Token doc enr Node clip token] -> ListType
reuseAlpha nodes
  = case extractFromTokens extractAlpha defaultAlpha nodes of
           (Alpha) -> genericReuse0 Alpha
           _ -> error "Internal error:ProxParser_Generated.reuseAlpha"

reuseStringItem :: [Token doc enr Node clip token] -> Maybe String -> Item
reuseStringItem nodes ma0
  = case extractFromTokens extractStringItem defaultStringItem nodes of
           (StringItem a0) -> genericReuse1 StringItem a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseStringItem"

reuseHeliumItem :: [Token doc enr Node clip token] -> Maybe Exp -> Item
reuseHeliumItem nodes ma0
  = case extractFromTokens extractHeliumItem defaultHeliumItem nodes of
           (HeliumItem a0) -> genericReuse1 HeliumItem a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseHeliumItem"

reuseListItem :: [Token doc enr Node clip token] -> Maybe ItemList -> Item
reuseListItem nodes ma0
  = case extractFromTokens extractListItem defaultListItem nodes of
           (ListItem a0) -> genericReuse1 ListItem a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseListItem"

reuseList_Decl :: [Token doc enr Node clip token] -> Maybe ConsList_Decl -> List_Decl
reuseList_Decl nodes ma0
  = case extractFromTokens extractList_Decl defaultList_Decl nodes of
           (List_Decl a0) -> genericReuse1 List_Decl a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Decl"

reuseList_Alt :: [Token doc enr Node clip token] -> Maybe ConsList_Alt -> List_Alt
reuseList_Alt nodes ma0
  = case extractFromTokens extractList_Alt defaultList_Alt nodes of
           (List_Alt a0) -> genericReuse1 List_Alt a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Alt"

reuseList_Exp :: [Token doc enr Node clip token] -> Maybe ConsList_Exp -> List_Exp
reuseList_Exp nodes ma0
  = case extractFromTokens extractList_Exp defaultList_Exp nodes of
           (List_Exp a0) -> genericReuse1 List_Exp a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Exp"

reuseList_Slide :: [Token doc enr Node clip token] -> Maybe ConsList_Slide -> List_Slide
reuseList_Slide nodes ma0
  = case extractFromTokens extractList_Slide defaultList_Slide nodes of
           (List_Slide a0) -> genericReuse1 List_Slide a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Slide"

reuseList_Item :: [Token doc enr Node clip token] -> Maybe ConsList_Item -> List_Item
reuseList_Item nodes ma0
  = case extractFromTokens extractList_Item defaultList_Item nodes of
           (List_Item a0) -> genericReuse1 List_Item a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Item"




--------------------------------------------------------------------------
-- extract functions                                                    --
--------------------------------------------------------------------------

extractRootDoc :: Maybe Node -> Maybe Document
extractRootDoc (Just (Node_RootDoc x@(RootDoc _) _)) = Just x
extractRootDoc _ = Nothing

extractRoot :: Maybe Node -> Maybe Root
extractRoot (Just (Node_Root x@(Root _ _) _)) = Just x
extractRoot _ = Nothing

extractRootEnr :: Maybe Node -> Maybe EnrichedDoc
extractRootEnr (Just (Node_RootEnr x@(RootEnr _ _) _)) = Just x
extractRootEnr _ = Nothing

extractRootE :: Maybe Node -> Maybe RootE
extractRootE (Just (Node_RootE x@(RootE _ _ _) _)) = Just x
extractRootE _ = Nothing

extractDecl :: Maybe Node -> Maybe Decl
extractDecl (Just (Node_Decl x@(Decl _ _ _ _ _ _ _ _) _)) = Just x
extractDecl _ = Nothing

extractBoardDecl :: Maybe Node -> Maybe Decl
extractBoardDecl (Just (Node_BoardDecl x@(BoardDecl _ _ _) _)) = Just x
extractBoardDecl _ = Nothing

extractPPPresentationDecl :: Maybe Node -> Maybe Decl
extractPPPresentationDecl (Just (Node_PPPresentationDecl x@(PPPresentationDecl _ _ _) _)) = Just x
extractPPPresentationDecl _ = Nothing

extractIdent :: Maybe Node -> Maybe Ident
extractIdent (Just (Node_Ident x@(Ident _ _ _) _)) = Just x
extractIdent _ = Nothing

extractPlusExp :: Maybe Node -> Maybe Exp
extractPlusExp (Just (Node_PlusExp x@(PlusExp _ _ _) _)) = Just x
extractPlusExp _ = Nothing

extractTimesExp :: Maybe Node -> Maybe Exp
extractTimesExp (Just (Node_TimesExp x@(TimesExp _ _ _) _)) = Just x
extractTimesExp _ = Nothing

extractDivExp :: Maybe Node -> Maybe Exp
extractDivExp (Just (Node_DivExp x@(DivExp _ _ _) _)) = Just x
extractDivExp _ = Nothing

extractPowerExp :: Maybe Node -> Maybe Exp
extractPowerExp (Just (Node_PowerExp x@(PowerExp _ _ _) _)) = Just x
extractPowerExp _ = Nothing

extractBoolExp :: Maybe Node -> Maybe Exp
extractBoolExp (Just (Node_BoolExp x@(BoolExp _ _) _)) = Just x
extractBoolExp _ = Nothing

extractIntExp :: Maybe Node -> Maybe Exp
extractIntExp (Just (Node_IntExp x@(IntExp _ _) _)) = Just x
extractIntExp _ = Nothing

extractLamExp :: Maybe Node -> Maybe Exp
extractLamExp (Just (Node_LamExp x@(LamExp _ _ _ _) _)) = Just x
extractLamExp _ = Nothing

extractAppExp :: Maybe Node -> Maybe Exp
extractAppExp (Just (Node_AppExp x@(AppExp _ _) _)) = Just x
extractAppExp _ = Nothing

extractCaseExp :: Maybe Node -> Maybe Exp
extractCaseExp (Just (Node_CaseExp x@(CaseExp _ _ _ _) _)) = Just x
extractCaseExp _ = Nothing

extractLetExp :: Maybe Node -> Maybe Exp
extractLetExp (Just (Node_LetExp x@(LetExp _ _ _ _) _)) = Just x
extractLetExp _ = Nothing

extractIdentExp :: Maybe Node -> Maybe Exp
extractIdentExp (Just (Node_IdentExp x@(IdentExp _) _)) = Just x
extractIdentExp _ = Nothing

extractIfExp :: Maybe Node -> Maybe Exp
extractIfExp (Just (Node_IfExp x@(IfExp _ _ _ _ _ _) _)) = Just x
extractIfExp _ = Nothing

extractParenExp :: Maybe Node -> Maybe Exp
extractParenExp (Just (Node_ParenExp x@(ParenExp _ _ _) _)) = Just x
extractParenExp _ = Nothing

extractListExp :: Maybe Node -> Maybe Exp
extractListExp (Just (Node_ListExp x@(ListExp _ _ _ _) _)) = Just x
extractListExp _ = Nothing

extractProductExp :: Maybe Node -> Maybe Exp
extractProductExp (Just (Node_ProductExp x@(ProductExp _ _ _ _) _)) = Just x
extractProductExp _ = Nothing

extractAlt :: Maybe Node -> Maybe Alt
extractAlt (Just (Node_Alt x@(Alt _ _ _ _) _)) = Just x
extractAlt _ = Nothing

extractBoard :: Maybe Node -> Maybe Board
extractBoard (Just (Node_Board x@(Board _ _ _ _ _ _ _ _) _)) = Just x
extractBoard _ = Nothing

extractBoardRow :: Maybe Node -> Maybe BoardRow
extractBoardRow (Just (Node_BoardRow x@(BoardRow _ _ _ _ _ _ _ _) _)) = Just x
extractBoardRow _ = Nothing

extractQueen :: Maybe Node -> Maybe BoardSquare
extractQueen (Just (Node_Queen x@(Queen _) _)) = Just x
extractQueen _ = Nothing

extractKing :: Maybe Node -> Maybe BoardSquare
extractKing (Just (Node_King x@(King _) _)) = Just x
extractKing _ = Nothing

extractBishop :: Maybe Node -> Maybe BoardSquare
extractBishop (Just (Node_Bishop x@(Bishop _) _)) = Just x
extractBishop _ = Nothing

extractKnight :: Maybe Node -> Maybe BoardSquare
extractKnight (Just (Node_Knight x@(Knight _) _)) = Just x
extractKnight _ = Nothing

extractRook :: Maybe Node -> Maybe BoardSquare
extractRook (Just (Node_Rook x@(Rook _) _)) = Just x
extractRook _ = Nothing

extractPawn :: Maybe Node -> Maybe BoardSquare
extractPawn (Just (Node_Pawn x@(Pawn _) _)) = Just x
extractPawn _ = Nothing

extractEmpty :: Maybe Node -> Maybe BoardSquare
extractEmpty (Just (Node_Empty x@(Empty) _)) = Just x
extractEmpty _ = Nothing

extractPPPresentation :: Maybe Node -> Maybe PPPresentation
extractPPPresentation (Just (Node_PPPresentation x@(PPPresentation _ _) _)) = Just x
extractPPPresentation _ = Nothing

extractSlide :: Maybe Node -> Maybe Slide
extractSlide (Just (Node_Slide x@(Slide _ _) _)) = Just x
extractSlide _ = Nothing

extractItemList :: Maybe Node -> Maybe ItemList
extractItemList (Just (Node_ItemList x@(ItemList _ _) _)) = Just x
extractItemList _ = Nothing

extractBullet :: Maybe Node -> Maybe ListType
extractBullet (Just (Node_Bullet x@(Bullet) _)) = Just x
extractBullet _ = Nothing

extractNumber :: Maybe Node -> Maybe ListType
extractNumber (Just (Node_Number x@(Number) _)) = Just x
extractNumber _ = Nothing

extractAlpha :: Maybe Node -> Maybe ListType
extractAlpha (Just (Node_Alpha x@(Alpha) _)) = Just x
extractAlpha _ = Nothing

extractStringItem :: Maybe Node -> Maybe Item
extractStringItem (Just (Node_StringItem x@(StringItem _) _)) = Just x
extractStringItem _ = Nothing

extractHeliumItem :: Maybe Node -> Maybe Item
extractHeliumItem (Just (Node_HeliumItem x@(HeliumItem _) _)) = Just x
extractHeliumItem _ = Nothing

extractListItem :: Maybe Node -> Maybe Item
extractListItem (Just (Node_ListItem x@(ListItem _) _)) = Just x
extractListItem _ = Nothing

extractList_Decl :: Maybe Node -> Maybe List_Decl
extractList_Decl (Just (Node_List_Decl x@(List_Decl _) _)) = Just x
extractList_Decl _ = Nothing

extractList_Alt :: Maybe Node -> Maybe List_Alt
extractList_Alt (Just (Node_List_Alt x@(List_Alt _) _)) = Just x
extractList_Alt _ = Nothing

extractList_Exp :: Maybe Node -> Maybe List_Exp
extractList_Exp (Just (Node_List_Exp x@(List_Exp _) _)) = Just x
extractList_Exp _ = Nothing

extractList_Slide :: Maybe Node -> Maybe List_Slide
extractList_Slide (Just (Node_List_Slide x@(List_Slide _) _)) = Just x
extractList_Slide _ = Nothing

extractList_Item :: Maybe Node -> Maybe List_Item
extractList_Item (Just (Node_List_Item x@(List_Item _) _)) = Just x
extractList_Item _ = Nothing




--------------------------------------------------------------------------
-- default functions                                                    --
--------------------------------------------------------------------------

defaultRootDoc :: Document
defaultRootDoc = RootDoc hole

defaultRoot :: Root
defaultRoot = Root NoIDP hole

defaultRootEnr :: EnrichedDoc
defaultRootEnr = RootEnr hole hole

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




--------------------------------------------------------------------------
-- extractFromTokens                                                    --
--------------------------------------------------------------------------

-- return result of the first extraction application in the list that is not Nothing
extractFromTokens :: (Maybe Node -> Maybe a) -> a -> [Token doc enr Node clip token] -> a
extractFromTokens extr def []     = def
extractFromTokens extr def (t:ts) = maybe (extractFromTokens extr def ts) id (extr (tokenNode t))



--------------------------------------------------------------------------
-- genericReuse functions                                               --
--------------------------------------------------------------------------

genericReuse0 :: (r) ->
                 
                 r
genericReuse0 f =
  f

genericReuse1 :: (a0 -> r) ->
                 a0 -> 
                 Maybe a0 -> r
genericReuse1 f a0 ma0 =
  f (maybe a0 id ma0)

genericReuse2 :: (a0 -> a1 -> r) ->
                 a0 -> a1 -> 
                 Maybe a0 -> Maybe a1 -> r
genericReuse2 f a0 a1 ma0 ma1 =
  f (maybe a0 id ma0) (maybe a1 id ma1)

genericReuse3 :: (a0 -> a1 -> a2 -> r) ->
                 a0 -> a1 -> a2 -> 
                 Maybe a0 -> Maybe a1 -> Maybe a2 -> r
genericReuse3 f a0 a1 a2 ma0 ma1 ma2 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2)

genericReuse4 :: (a0 -> a1 -> a2 -> a3 -> r) ->
                 a0 -> a1 -> a2 -> a3 -> 
                 Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> r
genericReuse4 f a0 a1 a2 a3 ma0 ma1 ma2 ma3 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3)

genericReuse5 :: (a0 -> a1 -> a2 -> a3 -> a4 -> r) ->
                 a0 -> a1 -> a2 -> a3 -> a4 -> 
                 Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> r
genericReuse5 f a0 a1 a2 a3 a4 ma0 ma1 ma2 ma3 ma4 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4)

genericReuse6 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> r) ->
                 a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> 
                 Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> r
genericReuse6 f a0 a1 a2 a3 a4 a5 ma0 ma1 ma2 ma3 ma4 ma5 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) (maybe a5 id ma5)

genericReuse7 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r) ->
                 a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> 
                 Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> Maybe a6 -> r
genericReuse7 f a0 a1 a2 a3 a4 a5 a6 ma0 ma1 ma2 ma3 ma4 ma5 ma6 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) (maybe a5 id ma5) (maybe a6 id ma6)

genericReuse8 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r) ->
                 a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> 
                 Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> Maybe a6 -> Maybe a7 -> r
genericReuse8 f a0 a1 a2 a3 a4 a5 a6 a7 ma0 ma1 ma2 ma3 ma4 ma5 ma6 ma7 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3) (maybe a4 id ma4) (maybe a5 id ma5) (maybe a6 id ma6) (maybe a7 id ma7)



