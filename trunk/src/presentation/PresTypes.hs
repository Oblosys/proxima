module PresTypes where

import CommonTypes
import {-# SOURCE #-} DocTypes_Generated
import {-# SOURCE #-} DocTypes
                    -- for Node

import CommonUtils

import Data.FiniteMap


data IDP = NoIDP | IDP Int deriving (Show, Read, Eq, Ord)

data PresentationLevel = PresentationLevel Presentation PresentationLS deriving Show

type PresentationLS = (LayoutMap, IDPCounter, InsertedTokenList, DeletedTokenMap)

type Layout = (Int, Int)

type LayoutMap = FiniteMap IDP Layout   -- Layout information for each element in Presentation
type IDPCounter = Int                   -- Counter for generating new unique IDPs
type InsertedTokenList = [IDP]          -- Not used now. Contains tokens that were inserted by parser
type DeletedTokenMap = FiniteMap IDP Presentation    -- Not used now. Maps deleted tokens to their successors

instance (Show a, Show b) => Show (FiniteMap a b) where
 show fm = "{FiniteMap}" -- ++show (fmToList fm)
 


data EditPresentation' =
    SetPres' PresentationLevel
  | SkipPres' Int deriving Show

data EditPresentation =
    SkipPres Int
  | SetFocusPres FocusPres
  | SetPres PresentationLevel
  | InitPres
  | ClosePres
--  | MouseDownPres PathPres Modifiers Int
  | NormalizePres
  | TestPres
  | Test2Pres
  | OpenFilePres String
  | SaveFilePres String
   
  | UpdateDocPres (DocumentLevel -> DocumentLevel) -- should encapsulate these so they automatically go to doc level
  | NavUpDocPres
  | NavDownDocPres
  | NavLeftDocPres
  | NavRightDocPres
  | CutDocPres
  | CopyDocPres
  | PasteDocPres
  | DeleteDocPres deriving Show






{- problems

node depends on document, but now all lower layers import document defs. Maybe parameterize lower layers in 
Node type?


DTD specific document stuff (the Document type etc) could be put in a different module for easy swapping
-}

-- Presentation is Xprez with ID's

data Presentation = EmptyP !IDP
           | StringP !IDP !String
           | ImageP !IDP !String
           | PolyP !IDP ![ (Float, Float) ] !Int -- pointList (0.0-1.0) lineWidth
           | RectangleP !IDP !Int !Int !Int      -- width height lineWidth
           | RowP !IDP !Int ![ Presentation ]    -- vRefNr 
           | ColP !IDP !Int ![ Presentation ]    -- hRefNr
           | OverlayP !IDP ![ Presentation ] -- 1st elt is in front of 2nd, etc.
           | WithP !AttrRule !Presentation         -- do these last two have ids?
           | StructuralP !IDP !Presentation       -- IDP?
           | ParsingP !IDP !Presentation         -- IDP?
           | LocatorP Node !Presentation -- deriving Show -- do we want a ! for location  ? 
{-         | Matrix [[ Presentation ]]       -- Stream is not a list because tree is easier in presentation.
           | Formatter [ Presentation ]
           | Alternative [ Presentation ]
-} -- are the !'s in the right place like this?
           | ArrangedP -- Presentation     -- experimental for incrementality.
                           -- arranger gets Presentation in which unchanged subtrees are replaced by
                           -- this node. For these subtrees, old arrangement is used

-- slightly less verbose show for presentation, without doc refs

instance Show Presentation where
  show (EmptyP id)           = "{"++show id++":Empty}"
  show (StringP id str)      = "{"++show id++":"++show str++"}"
  show (ImageP id str)       = "{"++show id++":Image "++str++"}"
  show (PolyP id _ _)        = "{"++show id++":Poly}"
  show (RectangleP id _ _ _) = "{"++show id++":Rectangle}"
  show (RowP id rf press)    = "RowP "++show rf++" ["++concat (intersperse ", " (map show press))++"]"
  show (ColP id rf press)    = "ColP "++show rf++" ["++concat (intersperse ", " (map show press))++"]"
  show (OverlayP  id press)  = "OverlayP ["++concat (intersperse ", " (map show press))++"]"
  show (WithP ar pres)       = "WithP <fn> "++show pres
  show (StructuralP id pres) = "StructuralP "++show id++" "++show pres
  show (ParsingP id pres)    = "ParsingP "++show id++" "++show pres
  show (LocatorP loc pres)   = "LocatorP "++ {- show loc++ -} " "++show pres
  show (ArrangedP)           = "ArrangedP" -- ++show pres
  show _                     = "<<<presentation without show>>>"


-- shallow presentation, showing only toplevel presentation node
{-
instance Show Presentation where
  show (EmptyP id)           = "{"++show id++":Empty}"
  show (StringP id str)      = "{"++show id++":StringP "++show str++"}"
  show (ImageP id str)       = "{"++show id++":ImageP "++show str++"}"
  show (PolyP id _ _)        = "{"++show id++":Poly}"
  show (RectangleP id _ _ _) = "{"++show id++":Rectangle}"
  show (RowP id rf press)    = "{"++show id++":RowP}"
  show (ColP id rf press)    = "{"++show id++":ColP}"
  show (OverlayP  id press)  = "{"++show id++":OverlayP}"
  show (WithP ar pres)       = "{WithP}"
  show (StructuralP id pres) = "{"++show id++":StructuralP}"
  show (ParsingP id pres)    = "{"++show id++":ParsingP}"
  show (LocatorP loc pres)   = "{"++show id++":LocatorP}"
  show (ArrangedP)           = "ArrangedP" -- ++show pres
  show _                     = "<<<presentation without show>>>"

-}
-- overlays: do they need a ref that says which element is the first, or in what order its elts should be parsed?
-- or is this always the order in which they appear on the screen? (== order of child list)
-- for now, the overlay can only have focus or be parsed in its head element.
{-

focus or parsing in overlay is more logical on last elt instead of first.

However, squigglies might be in front, just as boxes marking an area of the presentation. Some way of specifying
the main presentation is desirable. Pointing must also be smart enough to select the thing that was pointed at,
so in overlays with several pieces of text, both can be selected, but because a rectangle in front of a
presentation is as big as the presentation, it will always get the focus in that way.


-}





-- String has no reference, it would be rather useless, but if String is a row of characters, then it  
-- it could be logical in some cases 



-- lineWidth must be an attribute here
data Inherited = Inh { font :: Font
                     , textColor :: Color, lineColor :: Color, fillColor, backgroundColor :: Color
                     , mouseDown :: Maybe UpdateDoc
                     , popupMenuItems :: [ PopupMenuItem ]
		     , assignedWidth, assignedHeight :: Int
		     , assignedHRef, assignedVRef :: Int} deriving Show
data Synthesized = Syn { hRef, vRef, minWidth, minHeight :: Int
                       , hStretch, vStretch :: Bool
		       , finalWidth, finalHeight :: Int
		       , finalHRef, finalVRef :: Int
		       } deriving Show

type AttrRule = (Inherited, Synthesized) -> (Inherited, Synthesized)

{-
(EmptyP id)           =
(StringP id str)      =
(ImageP id str)       =
(PolyP id _ _)        =
(RectangleP id _ _ _) =
(RowP id rf press)    =
(ColP id rf press)    =
(OverlayP  id press)  =
(WithP ar pres)       =
(StructuralP id pres) =
(ParsingP id pres)    =
(LocatorP loc pres)   =
--(ArrangedP)          
-}

idP (EmptyP id)           = id
idP (StringP id _)        = id
idP (ImageP id str)       = id
idP (PolyP id _ _)        = id
idP (RectangleP id _ _ _) = id
idP (RowP id _ _)         = id
idP (ColP id _ _)         = id
idP (OverlayP  id press)  = id
idP (WithP ar pres)       = idP pres
idP (StructuralP id pres) = id
idP (ParsingP id pres)    = id
idP (LocatorP loc pres)   = idP pres
idP pres              = debug Err ("PresTypes.idP: unhandled presentation "++show pres) NoIDP


emptyAttrs = (Inh defaultFont black black black black Nothing [] 0 0 0 0, Syn 0 0 0 0 False False 0 0 0 0)


instance Show (a->b)  -- if a Show for Presentation is defined, this instance is no longer necessary
  where show f = "function"

type UpdateDoc = DocumentLevel -> DocumentLevel
type PopupMenuItem = (String, UpdateDoc)


data PathPres = PathP [Int] Int 
              | NoPathP deriving (Show, Eq, Ord)

data FocusPres = FocusP PathPres PathPres
               | NoFocusP deriving Show

fromP (FocusP from _) = from
fromP NoFocusP        = NoPathP

toP (FocusP _ to) = to
toP NoFocusP      = NoPathP

focusP from@(PathP _ _) to@(PathP _ _) = FocusP from to
focusP _ _                             = NoFocusP





-- TODO: add style to rectangles and images in presentation.
-- returns the document update function associated with the presentation at path




