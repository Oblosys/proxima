module DocTypes_Generated where

import Common.CommonTypes
import Evaluation.DocTypes

import Presentation.PresTypes
import List
import Char
import Data.Generics

data UserToken = KeyTk String  -- StrTk is for keywords, so eq takes the string value into account
               | IntTk
               | LIdentTk
               | UIdentTk
               | OpTk
               | SymTk deriving (Show, Eq, Ord, Typeable)

type HeliumTypeInfo = ([HeliumMessage],[(String,String)], [(PathDoc, String)])

instance Data HeliumMessage
instance Typeable HeliumMessage
instance Data PathDoc
instance Typeable PathDoc
               
data HeliumMessage =
         HMessage [String] 
       | HError [String] [PathDoc] [PathDoc] [PathDoc] deriving Show


----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- Proxima data type                                                    --
--------------------------------------------------------------------------

data Document = RootDoc Root
              | HoleDocument
              | ParseErrDocument (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                  deriving (Show, Data, Typeable)

data Root = Root IDP List_Decl
          | HoleRoot
          | ParseErrRoot (ParseError Document EnrichedDoc Node ClipDoc UserToken)
              deriving (Show, Data, Typeable)

data EnrichedDoc = RootEnr RootE HeliumTypeInfo
                 | HoleEnrichedDoc
                 | ParseErrEnrichedDoc (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                     deriving (Show, Data, Typeable)

data RootE = RootE IDP List_Decl List_Decl
           | HoleRootE
           | ParseErrRootE (ParseError Document EnrichedDoc Node ClipDoc UserToken)
               deriving (Show, Data, Typeable)

data Decl = Decl IDP IDP IDP IDP Bool Bool Ident Exp
          | BoardDecl IDP IDP Board
          | PPPresentationDecl IDP IDP PPPresentation
          | HoleDecl
          | ParseErrDecl (ParseError Document EnrichedDoc Node ClipDoc UserToken)
              deriving (Show, Data, Typeable)

data Ident = Ident IDP IDP String
           | HoleIdent
           | ParseErrIdent (ParseError Document EnrichedDoc Node ClipDoc UserToken)
               deriving (Show, Data, Typeable)

data Exp = PlusExp IDP Exp Exp
         | TimesExp IDP Exp Exp
         | DivExp IDP Exp Exp
         | PowerExp IDP Exp Exp
         | BoolExp IDP Bool
         | IntExp IDP Int
         | LamExp IDP IDP Ident Exp
         | AppExp Exp Exp
         | CaseExp IDP IDP Exp List_Alt
         | LetExp IDP IDP List_Decl Exp
         | IdentExp Ident
         | IfExp IDP IDP IDP Exp Exp Exp
         | ParenExp IDP IDP Exp
         | ListExp IDP IDP [IDP] List_Exp
         | ProductExp IDP IDP [IDP] List_Exp
         | HoleExp
         | ParseErrExp (ParseError Document EnrichedDoc Node ClipDoc UserToken)
             deriving (Show, Data, Typeable)

data Alt = Alt IDP IDP Ident Exp
         | HoleAlt
         | ParseErrAlt (ParseError Document EnrichedDoc Node ClipDoc UserToken)
             deriving (Show, Data, Typeable)

data Board = Board BoardRow BoardRow BoardRow BoardRow BoardRow BoardRow BoardRow BoardRow
           | HoleBoard
           | ParseErrBoard (ParseError Document EnrichedDoc Node ClipDoc UserToken)
               deriving (Show, Data, Typeable)

data BoardRow = BoardRow BoardSquare BoardSquare BoardSquare BoardSquare BoardSquare BoardSquare BoardSquare BoardSquare
              | HoleBoardRow
              | ParseErrBoardRow (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                  deriving (Show, Data, Typeable)

data BoardSquare = Queen Bool
                 | King Bool
                 | Bishop Bool
                 | Knight Bool
                 | Rook Bool
                 | Pawn Bool
                 | Empty
                 | HoleBoardSquare
                 | ParseErrBoardSquare (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                     deriving (Show, Data, Typeable)

data PPPresentation = PPPresentation Bool List_Slide
                    | HolePPPresentation
                    | ParseErrPPPresentation (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                        deriving (Show, Data, Typeable)

data Slide = Slide String ItemList
           | HoleSlide
           | ParseErrSlide (ParseError Document EnrichedDoc Node ClipDoc UserToken)
               deriving (Show, Data, Typeable)

data ItemList = ItemList ListType List_Item
              | HoleItemList
              | ParseErrItemList (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                  deriving (Show, Data, Typeable)

data ListType = Bullet
              | Number
              | Alpha
              | HoleListType
              | ParseErrListType (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                  deriving (Show, Data, Typeable)

data Item = StringItem String
          | HeliumItem Exp
          | ListItem ItemList
          | HoleItem
          | ParseErrItem (ParseError Document EnrichedDoc Node ClipDoc UserToken)
              deriving (Show, Data, Typeable)

data List_Decl = List_Decl ConsList_Decl
               | HoleList_Decl
               | ParseErrList_Decl (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                   deriving (Show, Data, Typeable)

data List_Alt = List_Alt ConsList_Alt
              | HoleList_Alt
              | ParseErrList_Alt (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                  deriving (Show, Data, Typeable)

data List_Exp = List_Exp ConsList_Exp
              | HoleList_Exp
              | ParseErrList_Exp (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                  deriving (Show, Data, Typeable)

data List_Slide = List_Slide ConsList_Slide
                | HoleList_Slide
                | ParseErrList_Slide (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                    deriving (Show, Data, Typeable)

data List_Item = List_Item ConsList_Item
               | HoleList_Item
               | ParseErrList_Item (ParseError Document EnrichedDoc Node ClipDoc UserToken)
                   deriving (Show, Data, Typeable)

data ConsList_Decl = Cons_Decl Decl ConsList_Decl
                   | Nil_Decl
                       deriving (Show, Data, Typeable)

data ConsList_Alt = Cons_Alt Alt ConsList_Alt
                  | Nil_Alt
                      deriving (Show, Data, Typeable)

data ConsList_Exp = Cons_Exp Exp ConsList_Exp
                  | Nil_Exp
                      deriving (Show, Data, Typeable)

data ConsList_Slide = Cons_Slide Slide ConsList_Slide
                    | Nil_Slide
                        deriving (Show, Data, Typeable)

data ConsList_Item = Cons_Item Item ConsList_Item
                   | Nil_Item
                       deriving (Show, Data, Typeable)




--------------------------------------------------------------------------
-- ClipDoc                                                              --
--------------------------------------------------------------------------

data ClipDoc = Clip_Document Document
             | Clip_Root Root
             | Clip_EnrichedDoc EnrichedDoc
             | Clip_RootE RootE
             | Clip_Decl Decl
             | Clip_Ident Ident
             | Clip_Exp Exp
             | Clip_Alt Alt
             | Clip_Board Board
             | Clip_BoardRow BoardRow
             | Clip_BoardSquare BoardSquare
             | Clip_PPPresentation PPPresentation
             | Clip_Slide Slide
             | Clip_ItemList ItemList
             | Clip_ListType ListType
             | Clip_Item Item
             | Clip_List_Decl List_Decl
             | Clip_List_Alt List_Alt
             | Clip_List_Exp List_Exp
             | Clip_List_Slide List_Slide
             | Clip_List_Item List_Item
             | Clip_Bool Bool
             | Clip_Int Int
             | Clip_String String
             | Clip_Float Float
             | Clip_Nothing deriving (Show, Typeable)



--------------------------------------------------------------------------
-- Node                                                                 --
--------------------------------------------------------------------------

data Node = NoNode
          | Node_RootDoc Document Path
          | Node_HoleDocument Document Path
          | Node_ParseErrDocument Document Path
          | Node_Root Root Path
          | Node_HoleRoot Root Path
          | Node_ParseErrRoot Root Path
          | Node_RootEnr EnrichedDoc Path
          | Node_HoleEnrichedDoc EnrichedDoc Path
          | Node_ParseErrEnrichedDoc EnrichedDoc Path
          | Node_RootE RootE Path
          | Node_HoleRootE RootE Path
          | Node_ParseErrRootE RootE Path
          | Node_Decl Decl Path
          | Node_BoardDecl Decl Path
          | Node_PPPresentationDecl Decl Path
          | Node_HoleDecl Decl Path
          | Node_ParseErrDecl Decl Path
          | Node_Ident Ident Path
          | Node_HoleIdent Ident Path
          | Node_ParseErrIdent Ident Path
          | Node_PlusExp Exp Path
          | Node_TimesExp Exp Path
          | Node_DivExp Exp Path
          | Node_PowerExp Exp Path
          | Node_BoolExp Exp Path
          | Node_IntExp Exp Path
          | Node_LamExp Exp Path
          | Node_AppExp Exp Path
          | Node_CaseExp Exp Path
          | Node_LetExp Exp Path
          | Node_IdentExp Exp Path
          | Node_IfExp Exp Path
          | Node_ParenExp Exp Path
          | Node_ListExp Exp Path
          | Node_ProductExp Exp Path
          | Node_HoleExp Exp Path
          | Node_ParseErrExp Exp Path
          | Node_Alt Alt Path
          | Node_HoleAlt Alt Path
          | Node_ParseErrAlt Alt Path
          | Node_Board Board Path
          | Node_HoleBoard Board Path
          | Node_ParseErrBoard Board Path
          | Node_BoardRow BoardRow Path
          | Node_HoleBoardRow BoardRow Path
          | Node_ParseErrBoardRow BoardRow Path
          | Node_Queen BoardSquare Path
          | Node_King BoardSquare Path
          | Node_Bishop BoardSquare Path
          | Node_Knight BoardSquare Path
          | Node_Rook BoardSquare Path
          | Node_Pawn BoardSquare Path
          | Node_Empty BoardSquare Path
          | Node_HoleBoardSquare BoardSquare Path
          | Node_ParseErrBoardSquare BoardSquare Path
          | Node_PPPresentation PPPresentation Path
          | Node_HolePPPresentation PPPresentation Path
          | Node_ParseErrPPPresentation PPPresentation Path
          | Node_Slide Slide Path
          | Node_HoleSlide Slide Path
          | Node_ParseErrSlide Slide Path
          | Node_ItemList ItemList Path
          | Node_HoleItemList ItemList Path
          | Node_ParseErrItemList ItemList Path
          | Node_Bullet ListType Path
          | Node_Number ListType Path
          | Node_Alpha ListType Path
          | Node_HoleListType ListType Path
          | Node_ParseErrListType ListType Path
          | Node_StringItem Item Path
          | Node_HeliumItem Item Path
          | Node_ListItem Item Path
          | Node_HoleItem Item Path
          | Node_ParseErrItem Item Path
          | Node_List_Decl List_Decl Path
          | Node_HoleList_Decl List_Decl Path
          | Node_ParseErrList_Decl List_Decl Path
          | Node_List_Alt List_Alt Path
          | Node_HoleList_Alt List_Alt Path
          | Node_ParseErrList_Alt List_Alt Path
          | Node_List_Exp List_Exp Path
          | Node_HoleList_Exp List_Exp Path
          | Node_ParseErrList_Exp List_Exp Path
          | Node_List_Slide List_Slide Path
          | Node_HoleList_Slide List_Slide Path
          | Node_ParseErrList_Slide List_Slide Path
          | Node_List_Item List_Item Path
          | Node_HoleList_Item List_Item Path
          | Node_ParseErrList_Item List_Item Path
            deriving Typeable



--------------------------------------------------------------------------
-- Show instance for Node                                               --
--------------------------------------------------------------------------

instance Show Node where
  show NoNode = "NoNode"
  show (Node_RootDoc _ _) = "Node_RootDoc" 
  show (Node_HoleDocument _ _) = "Node_HoleDocument" 
  show (Node_ParseErrDocument _ _) = "Node_ParseErrDocument" 
  show (Node_Root _ _) = "Node_Root" 
  show (Node_HoleRoot _ _) = "Node_HoleRoot" 
  show (Node_ParseErrRoot _ _) = "Node_ParseErrRoot" 
  show (Node_RootEnr _ _) = "Node_RootEnr" 
  show (Node_HoleEnrichedDoc _ _) = "Node_HoleEnrichedDoc" 
  show (Node_ParseErrEnrichedDoc _ _) = "Node_ParseErrEnrichedDoc" 
  show (Node_RootE _ _) = "Node_RootE" 
  show (Node_HoleRootE _ _) = "Node_HoleRootE" 
  show (Node_ParseErrRootE _ _) = "Node_ParseErrRootE" 
  show (Node_Decl _ _) = "Node_Decl" 
  show (Node_BoardDecl _ _) = "Node_BoardDecl" 
  show (Node_PPPresentationDecl _ _) = "Node_PPPresentationDecl" 
  show (Node_HoleDecl _ _) = "Node_HoleDecl" 
  show (Node_ParseErrDecl _ _) = "Node_ParseErrDecl" 
  show (Node_Ident _ _) = "Node_Ident" 
  show (Node_HoleIdent _ _) = "Node_HoleIdent" 
  show (Node_ParseErrIdent _ _) = "Node_ParseErrIdent" 
  show (Node_PlusExp _ _) = "Node_PlusExp" 
  show (Node_TimesExp _ _) = "Node_TimesExp" 
  show (Node_DivExp _ _) = "Node_DivExp" 
  show (Node_PowerExp _ _) = "Node_PowerExp" 
  show (Node_BoolExp _ _) = "Node_BoolExp" 
  show (Node_IntExp _ _) = "Node_IntExp" 
  show (Node_LamExp _ _) = "Node_LamExp" 
  show (Node_AppExp _ _) = "Node_AppExp" 
  show (Node_CaseExp _ _) = "Node_CaseExp" 
  show (Node_LetExp _ _) = "Node_LetExp" 
  show (Node_IdentExp _ _) = "Node_IdentExp" 
  show (Node_IfExp _ _) = "Node_IfExp" 
  show (Node_ParenExp _ _) = "Node_ParenExp" 
  show (Node_ListExp _ _) = "Node_ListExp" 
  show (Node_ProductExp _ _) = "Node_ProductExp" 
  show (Node_HoleExp _ _) = "Node_HoleExp" 
  show (Node_ParseErrExp _ _) = "Node_ParseErrExp" 
  show (Node_Alt _ _) = "Node_Alt" 
  show (Node_HoleAlt _ _) = "Node_HoleAlt" 
  show (Node_ParseErrAlt _ _) = "Node_ParseErrAlt" 
  show (Node_Board _ _) = "Node_Board" 
  show (Node_HoleBoard _ _) = "Node_HoleBoard" 
  show (Node_ParseErrBoard _ _) = "Node_ParseErrBoard" 
  show (Node_BoardRow _ _) = "Node_BoardRow" 
  show (Node_HoleBoardRow _ _) = "Node_HoleBoardRow" 
  show (Node_ParseErrBoardRow _ _) = "Node_ParseErrBoardRow" 
  show (Node_Queen _ _) = "Node_Queen" 
  show (Node_King _ _) = "Node_King" 
  show (Node_Bishop _ _) = "Node_Bishop" 
  show (Node_Knight _ _) = "Node_Knight" 
  show (Node_Rook _ _) = "Node_Rook" 
  show (Node_Pawn _ _) = "Node_Pawn" 
  show (Node_Empty _ _) = "Node_Empty" 
  show (Node_HoleBoardSquare _ _) = "Node_HoleBoardSquare" 
  show (Node_ParseErrBoardSquare _ _) = "Node_ParseErrBoardSquare" 
  show (Node_PPPresentation _ _) = "Node_PPPresentation" 
  show (Node_HolePPPresentation _ _) = "Node_HolePPPresentation" 
  show (Node_ParseErrPPPresentation _ _) = "Node_ParseErrPPPresentation" 
  show (Node_Slide _ _) = "Node_Slide" 
  show (Node_HoleSlide _ _) = "Node_HoleSlide" 
  show (Node_ParseErrSlide _ _) = "Node_ParseErrSlide" 
  show (Node_ItemList _ _) = "Node_ItemList" 
  show (Node_HoleItemList _ _) = "Node_HoleItemList" 
  show (Node_ParseErrItemList _ _) = "Node_ParseErrItemList" 
  show (Node_Bullet _ _) = "Node_Bullet" 
  show (Node_Number _ _) = "Node_Number" 
  show (Node_Alpha _ _) = "Node_Alpha" 
  show (Node_HoleListType _ _) = "Node_HoleListType" 
  show (Node_ParseErrListType _ _) = "Node_ParseErrListType" 
  show (Node_StringItem _ _) = "Node_StringItem" 
  show (Node_HeliumItem _ _) = "Node_HeliumItem" 
  show (Node_ListItem _ _) = "Node_ListItem" 
  show (Node_HoleItem _ _) = "Node_HoleItem" 
  show (Node_ParseErrItem _ _) = "Node_ParseErrItem" 
  show (Node_List_Decl _ _) = "Node_List_Decl" 
  show (Node_HoleList_Decl _ _) = "Node_HoleList_Decl" 
  show (Node_ParseErrList_Decl _ _) = "Node_ParseErrList_Decl" 
  show (Node_List_Alt _ _) = "Node_List_Alt" 
  show (Node_HoleList_Alt _ _) = "Node_HoleList_Alt" 
  show (Node_ParseErrList_Alt _ _) = "Node_ParseErrList_Alt" 
  show (Node_List_Exp _ _) = "Node_List_Exp" 
  show (Node_HoleList_Exp _ _) = "Node_HoleList_Exp" 
  show (Node_ParseErrList_Exp _ _) = "Node_ParseErrList_Exp" 
  show (Node_List_Slide _ _) = "Node_List_Slide" 
  show (Node_HoleList_Slide _ _) = "Node_HoleList_Slide" 
  show (Node_ParseErrList_Slide _ _) = "Node_ParseErrList_Slide" 
  show (Node_List_Item _ _) = "Node_List_Item" 
  show (Node_HoleList_Item _ _) = "Node_HoleList_Item" 
  show (Node_ParseErrList_Item _ _) = "Node_ParseErrList_Item" 


