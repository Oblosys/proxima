module DocTypes_Generated where

import CommonTypes
import {-# SOURCE #-} DocTypes

import PresTypes
import List
import Char

-- what about IDP fields for primitive type String_?
-- seems to be a general problem, IDP fields depend on what kind of presentations the value
-- is part of

-- Do lists need Hole's? Or can we use Nil for that?

data IDD = NoIDD | IDD Int deriving (Show, Read, Eq, Ord) -- don't want another module for this one

data PathDoc = PathD [Int]
             | NoPathD deriving (Show, Eq, Ord)

type FocusDoc = PathDoc  -- just a simple path focus for now

type PathD = [Int] -- we don't use PathDoc because NoPath makes algorithms unreadable



data HeliumMessage =
         HMessage [String] 
       | HError [String] [PathDoc] [PathDoc] [PathDoc] deriving Show



-- separate class for this show instance?
-- probably there will be a lot more document dependent utility functions.
-- maybe put all of them in DocUtils_Generated?

instance Show Node where
--  show _ = "#"
  show (NoNode)                 = "None" 
  show (EnrichedDocNode _ _)    = "EnrichedDoc"
  show (List_DeclNode _ _)      = "List_DeclNode"
  show (HeliumTypeInfoNode _ _) = "HeliumTypeInfoNode"
  show (DocumentNode _ _)       = "DocumentNode" 
  show (BoolNode _ _)           = "BoolNode"      
  show (IdentNode _ _)          = "IdentNode"     
  show (ExpNode _ _)            = "ExpNode"       
  show (BoardNode _ _)          = "BoardNode"     
  show (PPPresentationNode _ _) = "PPPresentationNode"
  show (StringNode _ _)         = "StringNode"    
  show (IntNode _ _)            = "IntNode"       
  show (List_AltNode _ _)       = "List_AltNode"  
  show (List_ExpNode _ _)       = "List_ExpNode"  
  show (BoardRowNode _ _)       = "BoardRowNode"  
  show (BoardSquareNode _ _)    = "BoardSquareNode"  
  show (List_SlideNode _ _)     = "List_SlideNode"
  show (SlideNode _ _)          = "SlideNode"
  show (String_Node  _ _)       = "String_Node "  
  show (ItemListNode _ _)       = "ItemListNode"  
  show (ListTypeNode _ _)       = "ListTypeNode"  
  show (List_ItemNode  _ _)     = "List_ItemNode"
  show (DeclNode _ _)           = "DeclNode"      
  show _                        = "{No Show}"

nodeTypeInt :: Node -> Int           -- for an easy Eq and Ord instance declaration
nodeTypeInt (NoNode)                 = 0
nodeTypeInt (EnrichedDocNode _ _)    = 1
nodeTypeInt (List_DeclNode _ _)      = 2
nodeTypeInt (HeliumTypeInfoNode _ _) = 3
nodeTypeInt (DocumentNode _ _)       = 4
nodeTypeInt (BoolNode _ _)           = 5
nodeTypeInt (IdentNode _ _)          = 6
nodeTypeInt (ExpNode _ _)            = 7
nodeTypeInt (BoardNode _ _)          = 8
nodeTypeInt (PPPresentationNode _ _) = 9
nodeTypeInt (StringNode _ _)         = 10
nodeTypeInt (IntNode _ _)            = 11
nodeTypeInt (List_AltNode _ _)       = 12
nodeTypeInt (List_ExpNode _ _)       = 13
nodeTypeInt (BoardRowNode _ _)       = 14
nodeTypeInt (BoardSquareNode _ _)    = 15
nodeTypeInt (List_SlideNode _ _)     = 16
nodeTypeInt (String_Node  _ _)       = 17
nodeTypeInt (ItemListNode _ _)       = 18
nodeTypeInt (ListTypeNode _ _)       = 19
nodeTypeInt (List_ItemNode  _ _)     = 20
nodeTypeInt (DeclNode _ _)           = 21
nodeTypeInt _                        = 9999

instance Eq Node where
  nd1 == nd2 = nodeTypeInt nd1 == nodeTypeInt nd2
  
instance Ord Node where
  nd1 <= nd2 = nodeTypeInt nd1 <= nodeTypeInt nd2

-- does not work anymore
instance Read Node where
  readsPrec _ inp = case dropWhile isSpace inp of -- requires: import Char
                      ('#':rest) -> [(NoNode, rest)]
                      _          -> []



-- clip is not a good name, since nothing is clipped, maybe node is ok after all


-- ?? what does this old comment mean:
-- data depends on IDP, not good. Now a pres change may cause a data update.
-- Do this automatically, store elsewhere?


-- this one cannot be generated, but needs to be here for now
data Document = RootDoc IDD IDP List_Decl
              | HoleDoc
              | ParseErrDoc Node Presentation deriving Show


type HeliumTypeInfo = ([HeliumMessage],[(String,String)], [(PathDoc, String)])

{-
Find out more on how to integrate non proxima types (such as HeliumTypeInfo)
-}



-- don't edit this line or below !!!

{- ------------------------------------

 generated part

-------------------------------------- -}


-- Generated Types --



data EnrichedDoc = RootEnr IDD IDP List_Decl List_Decl HeliumTypeInfo Document 
                 | HoleEnrichedDoc
                 | ParseErrEnrichedDoc Node Presentation
                    deriving Show


data Decl = Decl IDD IDP IDP IDP IDP Bool Bool Ident Exp 
          | BoardDecl IDD IDP IDP Board 
          | PPPresentationDecl IDD IDP IDP PPPresentation 
          | HoleDecl
          | ParseErrDecl Node Presentation
             deriving Show


data Ident = Ident IDD IDP IDP String 
           | HoleIdent
           | ParseErrIdent Node Presentation
              deriving Show


data Exp = PlusExp IDD IDP Exp Exp 
         | TimesExp IDD IDP Exp Exp 
         | DivExp IDD IDP Exp Exp 
         | PowerExp IDD IDP Exp Exp 
         | BoolExp IDD IDP Bool 
         | IntExp IDD IDP Int 
         | LamExp IDD IDP IDP Ident Exp 
         | AppExp IDD Exp Exp 
         | CaseExp IDD IDP IDP Exp List_Alt 
         | LetExp IDD IDP IDP List_Decl Exp 
         | IdentExp IDD Ident 
         | IfExp IDD IDP IDP IDP Exp Exp Exp 
         | ParenExp IDD IDP IDP Exp 
         | ListExp IDD IDP IDP [IDP] List_Exp 
         | ProductExp IDD IDP IDP [IDP] List_Exp 
         | HoleExp
         | ParseErrExp Node Presentation
            deriving Show


data Alt = Alt IDD IDP IDP Ident Exp 
         | HoleAlt
         | ParseErrAlt Node Presentation
            deriving Show


data Board = Board IDD BoardRow BoardRow BoardRow BoardRow BoardRow BoardRow BoardRow BoardRow 
           | HoleBoard
           | ParseErrBoard Node Presentation
              deriving Show


data BoardRow = BoardRow IDD BoardSquare BoardSquare BoardSquare BoardSquare BoardSquare BoardSquare BoardSquare BoardSquare 
              | HoleBoardRow
              | ParseErrBoardRow Node Presentation
                 deriving Show


data BoardSquare = Queen IDD Bool 
                 | King IDD Bool 
                 | Bishop IDD Bool 
                 | Knight IDD Bool 
                 | Rook IDD Bool 
                 | Pawn IDD Bool 
                 | Empty 
                 | HoleBoardSquare
                 | ParseErrBoardSquare Node Presentation
                    deriving Show


data PPPresentation = PPPresentation IDD Bool List_Slide 
                    | HolePPPresentation
                    | ParseErrPPPresentation Node Presentation
                       deriving Show


data Slide = Slide IDD String_ ItemList 
           | HoleSlide
           | ParseErrSlide Node Presentation
              deriving Show


data ItemList = ItemList IDD ListType List_Item 
              | HoleItemList
              | ParseErrItemList Node Presentation
                 deriving Show


data ListType = Bullet IDD 
              | Number IDD 
              | Alpha IDD 
              | HoleListType
              | ParseErrListType Node Presentation
                 deriving Show


data Item = StringItem IDD String_ 
          | HeliumItem IDD Exp 
          | ListItem IDD ItemList 
          | HoleItem
          | ParseErrItem Node Presentation
             deriving Show


data String_ = String_ IDD String 
             | HoleString_
             | ParseErrString_ Node Presentation
                deriving Show


data List_Decl = List_Decl IDD ConsList_Decl 
               | HoleList_Decl
               | ParseErrList_Decl Node Presentation
                  deriving Show


data ConsList_Decl = Cons_Decl Decl ConsList_Decl 
                   | Nil_Decl 
                      deriving Show


data List_Alt = List_Alt IDD ConsList_Alt 
              | HoleList_Alt
              | ParseErrList_Alt Node Presentation
                 deriving Show


data ConsList_Alt = Cons_Alt Alt ConsList_Alt 
                  | Nil_Alt 
                     deriving Show


data List_Exp = List_Exp IDD ConsList_Exp 
              | HoleList_Exp
              | ParseErrList_Exp Node Presentation
                 deriving Show


data ConsList_Exp = Cons_Exp Exp ConsList_Exp 
                  | Nil_Exp 
                     deriving Show


data List_Slide = List_Slide IDD ConsList_Slide 
                | HoleList_Slide
                | ParseErrList_Slide Node Presentation
                   deriving Show


data ConsList_Slide = Cons_Slide Slide ConsList_Slide 
                    | Nil_Slide 
                       deriving Show


data List_Item = List_Item IDD ConsList_Item 
               | HoleList_Item
               | ParseErrList_Item Node Presentation
                  deriving Show


data ConsList_Item = Cons_Item Item ConsList_Item 
                   | Nil_Item 
                      deriving Show


-- Generated Types --

data ClipDoc = Clip_List_Decl List_Decl
             | Clip_HeliumTypeInfo HeliumTypeInfo
             | Clip_Document Document
             | Clip_Bool Bool
             | Clip_Ident Ident
             | Clip_Exp Exp
             | Clip_Board Board
             | Clip_PPPresentation PPPresentation
             | Clip_String String
             | Clip_Int Int
             | Clip_List_Alt List_Alt
             | Clip_List_Exp List_Exp
             | Clip_BoardRow BoardRow
             | Clip_BoardSquare BoardSquare
             | Clip_List_Slide List_Slide
             | Clip_String_ String_
             | Clip_ItemList ItemList
             | Clip_ListType ListType
             | Clip_List_Item List_Item
             | Clip_Decl Decl
             
             | Clip_Alt Alt
             
             
             | Clip_Slide Slide
             
             | Clip_Item Item
             
             | Clip_Nothing deriving Show

data Node = NoNode 
          | EnrichedDocNode EnrichedDoc Path
          | List_DeclNode List_Decl Path 
          | HeliumTypeInfoNode HeliumTypeInfo Path 
          | DocumentNode Document Path 
          | BoolNode Bool Path 
          | IdentNode Ident Path 
          | ExpNode Exp Path 
          | BoardNode Board Path 
          | PPPresentationNode PPPresentation Path 
          | StringNode String Path 
          | IntNode Int Path 
          | List_AltNode List_Alt Path 
          | List_ExpNode List_Exp Path 
          | BoardRowNode BoardRow Path 
          | BoardSquareNode BoardSquare Path 
          | List_SlideNode List_Slide Path 
          | String_Node String_ Path 
          | ItemListNode ItemList Path 
          | ListTypeNode ListType Path 
          | List_ItemNode List_Item Path 
          | DeclNode Decl Path 
          
          | AltNode Alt Path 
          
          
          | SlideNode Slide Path 
          
          | ItemNode Item Path 
          
