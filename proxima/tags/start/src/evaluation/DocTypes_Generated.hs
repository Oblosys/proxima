module DocTypes_Generated where

import CommonTypes
import {-# SOURCE #-} DocTypes

import PresTypes
import List
import Char

-- what about IDP fields for primitive type String_?
-- seems to be a general problem, IDP fields depend on what kind of presentations the value
-- is part of

data IDD = NoIDD | IDD Int deriving (Show, Read, Eq, Ord) -- don't want another module for this one

data PathDoc = PathD [Int]
             | NoPathD deriving (Show, Eq, Ord)

type FocusDoc = PathDoc  -- just a simple path focus for now

type PathD = [Int] -- we don't use PathDoc because NoPath makes algorithms unreadable



data HeliumMessage =
         HMessage [String] 
       | HError [String] [PathDoc] [PathDoc] [PathDoc] deriving Show



--






data Node = NoNode 
          | DocNode Document Path -- Path will always be []
          | EnrNode EnrichedDoc Path -- Path will always be []
          | DeclsNode Decls Path
          | DeclNode Decl Path
          | IdentNode Ident Path
          | ExpNode Exp Path
          | ExpsNode Exps Path
          | AltsNode Alts Path
          | AltNode Alt Path 
          | BoardNode Board Path 
          | BoardRowNode BoardRow Path 
          | BoardSquareNode BoardSquare Path
               
          | PPPresentationNode PPPresentation Path
          | SlidesNode Slides Path
          | SlideNode Slide Path
          | ItemListNode ItemList Path
          | ListTypeNode ListType Path
          | ItemsNode Items Path
          | ItemNode Item Path
          | String_Node String_ Path
       
 -- deriving Show


-- separate class for this show instance?
-- probably there will be a lot more document dependent utility functions.
-- maybe put all of them in DocUtils_Generated?

instance Show Node where
  show _ = "#"
  
instance Read Node where
  readsPrec _ inp = case dropWhile isSpace inp of -- requires: import Char
                      ('#':rest) -> [(NoNode, rest)]
                      _          -> []




-- clip is not a good name, since nothing is clipped, maybe node is ok after all
data ClipDoc = Clip_Int Int
             | Clip_String String
             | Clip_Bool Bool
          
             | Clip_Decls Decls
             | Clip_Decl Decl
             | Clip_Ident Ident
             | Clip_Exp Exp
             | Clip_Exps Exps
             | Clip_Alts Alts
             | Clip_Alt Alt
          
             | Clip_Board Board
             | Clip_BoardRow BoardRow
             | Clip_BoardSquare BoardSquare

             | Clip_PPPresentation PPPresentation
             | Clip_Slides Slides
             | Clip_Slide Slide
             | Clip_ItemList ItemList
             | Clip_ListType ListType
             | Clip_Items Items
             | Clip_Item Item
             | Clip_String_ String_
                       
             | Clip_Nothing deriving Show


-- this one cannot be generated, but needs to be here for now
data EnrichedDoc = RootEnr IDD IDP Decls Decls HeliumTypeInfo Document  -- document ref is for popups only
                 | HoleEnr
                 | ParseErrEnr Node Presentation deriving Show

type HeliumTypeInfo = ([HeliumMessage],[(String,String)], [(PathDoc, String)])
-- TypeInfo should also be in EnrichedDoc definition



-- data depends on IDP, not good. Now a pres change may cause a data update.
-- Do this automatically, store elsewhere?

data Document = RootDoc IDD IDP Decls
              | HoleDoc
              | ParseErrDoc Node Presentation deriving Show


-- no pres elts of its own, so no ids for them. This also means we won't be able to recover Decls's id
data Decls = ConsDecls IDD Decl Decls
           | NilDecls IDD
           | HoleDecls
           | ParseErrDecls Node Presentation [String] deriving Show

--  4 pres elts: "=", ";", TypeDecl, and "[...]"

data Decl = Decl IDD IDP IDP IDP IDP Bool Bool Ident Exp
          | BoardDecl IDD IDP IDP Board
          | PPPresentationDecl IDD IDP IDP PPPresentation
          | HoleDecl
          | ParseErrDecl Node Presentation deriving Show

-- one pres elt for in program source, other for in list
-- however, only the one for source is used, the other has no layout
data Ident = Ident IDD IDP IDP String
           | HoleIdent
           | ParseErrIdent Node Presentation deriving Show

data Exps = ConsExps IDD Exp Exps
          | NilExps IDD
          | HoleExps
          | ParseErrExps Node Presentation deriving Show

data Alts = ConsAlts IDD Alt Alts
          | NilAlts IDD
          | HoleAlts
          | ParseErrAlts Node Presentation deriving Show


data Exp = PlusExp IDD IDP Exp Exp
         | TimesExp IDD IDP Exp Exp
         | DivExp IDD IDP Exp Exp
         | PowerExp IDD IDP Exp Exp
         | BoolExp IDD IDP Bool
         | IntExp IDD IDP Int
         | LamExp IDD IDP IDP Ident Exp
         | AppExp IDD Exp Exp
         | CaseExp IDD IDP IDP Exp Alts
         | LetExp IDD IDP IDP Decls Exp
         | IdentExp IDD Ident
         | IfExp IDD IDP IDP IDP Exp Exp Exp
         | ParenExp IDD IDP IDP Exp
         | ListExp IDD IDP IDP [IDP] Exps
         | ProductExp IDD IDP IDP [IDP] Exps
         | HoleExp
         | ParseErrExp Node Presentation deriving Show

data Alt = Alt IDD IDP IDP Ident Exp
         | HoleAlt
         | ParseErrAlt Node Presentation deriving Show




data Board       = Board    IDD BoardRow BoardRow BoardRow BoardRow BoardRow BoardRow BoardRow BoardRow deriving Show
data BoardRow    = BoardRow IDD BoardSquare BoardSquare BoardSquare BoardSquare BoardSquare BoardSquare BoardSquare BoardSquare deriving Show
data BoardSquare = Queen IDD Bool
                 | King  IDD Bool
                 | Bishop IDD Bool
                 | Knight IDD Bool
                 | Rook IDD Bool
                 | Pawn IDD Bool
                 | Empty deriving Show


data PPPresentation = PPPresentation IDD Bool Slides
                    | HolePPPresentation
                    | ParseErrPPPresentation Node Presentation deriving Show

data Slides = ConsSlides IDD Slide Slides
            | NilSlides IDD
            | HoleSlides
            | ParseErrSlides Node Presentation deriving Show

data Slide = Slide IDD String_ ItemList
           | HoleSlide
           | ParseErrSlide Node Presentation deriving Show

data ItemList = ItemList IDD ListType Items
              | HoleItemList
              | ParseErrItemList Node Presentation deriving Show

data ListType = Bullet IDD
              | Number IDD
              | Alpha IDD
              | HoleListType
              | ParseErrListType Node Presentation deriving Show

data Items = ConsItems IDD Item Items
           | NilItems IDD
           | HoleItems
           | ParseErrItems Node Presentation deriving Show

data Item = StringItem IDD String_
          | HeliumItem IDD Exp
          | ListItem IDD ItemList
          | HoleItem
          | ParseErrItem Node Presentation deriving Show

data String_ = String_ IDD String
             | HoleString_
             | ParseErrString_ Node Presentation deriving Show

-- end of generated declarations
