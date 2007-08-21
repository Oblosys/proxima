module PresTypes where

import CommonTypes
import DocTypes (DocumentLevel)

import CommonUtils

import qualified Data.Map as Map
import Data.Map (Map)

data IDP = NoIDP | IDP Int deriving (Show, Read, Eq, Ord)

data PresentationLevel doc node clip = PresentationLevel (Presentation doc node clip) (PresentationLS doc node clip) deriving Show

type PresentationLS doc node clip = (LayoutMap, IDPCounter, InsertedTokenList, DeletedTokenMap doc node clip)

type Layout = (Int, Int)

type LayoutMap = Map IDP Layout   -- Layout information for each element in Presentation
type IDPCounter = Int                   -- Counter for generating new unique IDPs
type InsertedTokenList = [IDP]          -- Not used now. Contains tokens that were inserted by parser
type DeletedTokenMap doc node clip = Map IDP (Presentation doc node clip)    -- Not used now. Maps deleted tokens to their successors

--instance (Show a, Show b) => Show (FiniteMap a b) where
-- show fm = "{FiniteMap}" -- ++show (fmToList fm)
 

initLayout :: LayoutMap
initLayout = Map.fromList [(IDP (-1), (0,1))]

data EditPresentation' doc node clip =
    SetPres' (PresentationLevel doc node clip)
  | SkipPres' Int deriving Show

data EditPresentation documentLevel doc node clip =
    SkipPres Int
  | SetFocusPres FocusPres
  | SetPres (PresentationLevel doc node clip)
  | InitPres
  | ClosePres
--  | MouseDownPres PathPres Modifiers Int
  | NormalizePres
  | TestPres
  | Test2Pres
  | OpenFilePres String
  | SaveFilePres String
   
  | UpdateDocPres (documentLevel -> documentLevel) -- should encapsulate these so they automatically go to doc level
  | NavUpDocPres
  | NavDownDocPres
  | NavLeftDocPres
  | NavRightDocPres
  | CutDocPres
  | CopyDocPres
  | PasteDocPres
  | DeleteDocPres deriving Show





-- Presentation is Xprez with ID's

data Presentation doc node clip = EmptyP !IDP
           | StringP !IDP !String
           | ImageP !IDP !String
           | PolyP !IDP ![ (Float, Float) ] !Int -- pointList (0.0-1.0) lineWidth
           | RectangleP !IDP !Int !Int !Int      -- width height lineWidth
           | EllipseP !IDP !Int !Int !Int      -- width height lineWidth
           | RowP !IDP !Int ![Presentation doc node clip]    -- vRefNr 
           | ColP !IDP !Int ![Presentation doc node clip]    -- hRefNr
           | OverlayP !IDP ![ (Presentation doc node clip) ] -- 1st elt is in front of 2nd, etc.
           | WithP !(AttrRule doc clip) !(Presentation doc node clip)         -- do these last two have ids?
           | StructuralP !IDP !(Presentation doc node clip)       -- IDP?
           | ParsingP !IDP !(Presentation doc node clip)         -- IDP?
           | LocatorP node !(Presentation doc node clip) -- deriving Show -- do we want a ! for location  ? 
           | GraphP !IDP !Dirty !Int !Int ![(Int,Int)] ![Presentation doc node clip] -- width height edges 
           | VertexP !IDP !Int !Int !Int Outline !(Presentation doc node clip) -- vertexID x y outline       see note below
           | FormatterP !IDP ![Presentation doc node clip]

{-         | Matrix [[ (Presentation doc node clip) ]]       -- Stream is not a list because tree is easier in presentation.
           | Formatter [ (Presentation doc node clip) ]
           | Alternative [ (Presentation doc node clip) ]
-} -- are the !'s in the right place like this?
           | ArrangedP -- (Presentation doc node clip)     -- experimental for incrementality.
                           -- arranger gets Presentation in which unchanged subtrees are replaced by
                           -- this node. For these subtrees, old arrangement is used


-- Note: An alternative and safer definition for GraphP is GraphP [Vertex], but this requires all functions that
-- traverse Presentation to have a separate function for traversing the Vertex type. This is too much of a hassle.


-- slightly less verbose show for presentation, without doc refs

instance Show (Presentation doc node clip) where
  show (EmptyP id)           = "{"++show id++":Empty}"
  show (StringP id str)      = "{"++show id++":"++show str++"}"
  show (ImageP id str)       = "{"++show id++":Image "++str++"}"
  show (PolyP id _ _)        = "{"++show id++":Poly}"
  show (RectangleP id _ _ _) = "{"++show id++":Rectangle}"
  show (EllipseP id _ _ _)   = "{"++show id++":Ellipse}"
  show (RowP id rf press)    = "RowP "++show rf++" ["++concat (intersperse ", " (map show press))++"]"
  show (ColP id rf press)    = "ColP "++show rf++" ["++concat (intersperse ", " (map show press))++"]"
  show (OverlayP  id press)  = "OverlayP ["++concat (intersperse ", " (map show press))++"]"
  show (WithP ar pres)       = "WithP <fn> "++show pres
  show (StructuralP id pres) = "StructuralP "++show id++" "++show pres
  show (ParsingP id pres)    = "ParsingP "++show id++" "++show pres
  show (LocatorP loc pres)   = "LocatorP "++ {- show loc++ -} " "++show pres
  show (GraphP id _ _ _ edges press) = "GraphP "++ show edges++" ["++concat (intersperse ", " (map show press))++"]"
  show (VertexP id vid x y ol pres)  = "Vertex (#"++show vid++":"++show x++","++show y++")"++show pres
  show (FormatterP id press) = "FormatterP ["++concat (intersperse ", " (map show press))++"]"
  show (ArrangedP)           = "ArrangedP" -- ++show pres
  show _                     = "<<<presentation without show>>>"


-- shallow presentation, showing only toplevel presentation


shallowShowPres (EmptyP id)           = "{"++show id++":Empty}"
shallowShowPres (StringP id str)      = "{"++show id++":StringP "++show str++"}"
shallowShowPres (ImageP id str)       = "{"++show id++":ImageP "++show str++"}"
shallowShowPres (PolyP id _ _)        = "{"++show id++":Poly}"
shallowShowPres (RectangleP id _ _ _) = "{"++show id++":Rectangle}"
shallowShowPres (EllipseP id _ _ _)   = "{"++show id++":Ellipse}"
shallowShowPres (RowP id rf press)    = "{"++show id++":RowP, #children="++show (length press)++"}"
shallowShowPres (ColP id rf press)    = "{"++show id++":ColP, #children="++show (length press)++"}"
shallowShowPres (OverlayP  id press)  = "{"++show id++":Overlay, #children="++show (length press)++"P}"
shallowShowPres (GraphP id _ _ _ _ press)  = "{"++show id++":Graph, #children="++show (length press)++"P}"
shallowShowPres (VertexP _ _ x y _  pres)  = "{"++show id++":Vertex, x="++show x++",y="++show y++"}"
shallowShowPres (WithP ar pres)       = "{WithP}"
shallowShowPres (StructuralP id pres) = "{"++show id++":StructuralP}"
shallowShowPres (ParsingP id pres)    = "{"++show id++":ParsingP}"
shallowShowPres (LocatorP loc pres)   = "{LocatorP}"
shallowShowPres (ArrangedP)           = "ArrangedP" -- ++show pres
shallowShowPres _                     = "<<<presentation without show>>>"

getChildren (EmptyP id)           = []
getChildren (StringP id str)      = []
getChildren (ImageP id str)       = []
getChildren (PolyP id _ _)        = []
getChildren (RectangleP id _ _ _) = []
getChildren (EllipseP id _ _ _)   = []
getChildren (RowP id rf press)    = press
getChildren (ColP id rf press)    = press
getChildren (OverlayP  id press)  = press
getChildren (WithP ar pres)       = [pres]
getChildren (StructuralP id pres) = [pres]
getChildren (ParsingP id pres)    = [pres]
getChildren (LocatorP loc pres)   = [pres]
getChildren (ArrangedP)           = []
getChildren pres                  = debug Err ("PresTypes.getChildren: unhandled presentation"++shallowShowPres pres) []

setChildren [] pres@(EmptyP id)           = pres
setChildren [] pres@(StringP id str)      = pres
setChildren [] pres@(ImageP id str)       = pres
setChildren [] pres@(PolyP id _ _)        = pres
setChildren [] pres@(RectangleP id _ _ _) = pres
setChildren [] pres@(EllipseP id _ _ _)   = pres
setChildren press' (RowP id rf press)     = RowP id rf press'
setChildren press' (ColP id rf press)     = ColP id rf press'
setChildren press' (OverlayP  id press)   = OverlayP  id press'
setChildren [pres'] (WithP ar pres)       = WithP ar pres'
setChildren [pres'] (StructuralP id pres) = StructuralP id pres'
setChildren [pres'] (ParsingP id pres)    = ParsingP id pres'
setChildren [pres'] (LocatorP loc pres)   = LocatorP loc pres'
setChildren []      (ArrangedP)           = ArrangedP
setChildren press'  pres                  = debug Err ("PresTypes.getChildren: unhandled case " ++ show (length press') ++ ", " ++ shallowShowPres pres) pres


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



-- lineWidth should be an attribute here
data Inherited doc clip = Inh { font :: Font
                     , textColor :: Color, lineColor :: Color, fillColor, backgroundColor :: Color
                     , mouseDown :: Maybe (UpdateDoc doc clip)
                     , popupMenuItems :: [ PopupMenuItem doc clip ]
		     , assignedWidth, assignedHeight :: Int
		     , assignedVRef, assignedHRef :: Int} deriving Show
data Synthesized = Syn { vRef, hRef, minWidth, minHeight :: Int
                       , hStretch, vStretch :: Bool
		       , finalWidth, finalHeight :: Int
		       , finalVRef, finalHRef :: Int
		       } deriving Show

type AttrRule doc clip = (Inherited doc clip, Synthesized) -> (Inherited doc clip, Synthesized)


idP (EmptyP id)           = id
idP (StringP id _)        = id
idP (ImageP id str)       = id
idP (PolyP id _ _)        = id
idP (RectangleP id _ _ _) = id
idP (EllipseP id _ _ _)   = id
idP (RowP id _ _)         = id
idP (ColP id _ _)         = id
idP (OverlayP  id press)  = id
idP (WithP ar pres)       = idP pres
idP (StructuralP id pres) = id
idP (ParsingP id pres)    = id
idP (LocatorP loc pres)   = idP pres
idP (GraphP id _ _ _ _ _)  = id
idP (VertexP id _ _ _ _ _) = id
idP (FormatterP id _)     = id
idP pres              = debug Err ("PresTypes.idP: unhandled presentation "++show pres) NoIDP


emptyAttrs = (Inh defaultFont black black black black Nothing [] 0 0 0 0, Syn 0 0 0 0 False False 0 0 0 0)

type UpdateDoc doc clip = DocumentLevel doc clip -> DocumentLevel doc clip
type PopupMenuItem doc clip = (String, UpdateDoc doc clip)


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




