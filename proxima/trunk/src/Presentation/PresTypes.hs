module Presentation.PresTypes where

import Common.CommonTypes
import Evaluation.DocTypes (DocumentLevel)

import Common.CommonUtils

import qualified Data.Map as Map
import Data.Map (Map)

data IDP = NoIDP | IDP Int deriving (Show, Read, Eq, Ord)

data PresentationLevel doc node clip token = PresentationLevel (Presentation doc node clip token) (PresentationLS doc node clip token) deriving Show

type PresentationLS doc node clip token = (WhitespaceMap, IDPCounter)

type Whitespace = (Int, Int) -- (line breaks, spaces)

type FocusStartEnd = (Maybe Int, Maybe Int)

type WhitespaceFocus = ((Int,Int), FocusStartEnd)

data TokenLayout = TokenLayout { whitespace :: Whitespace
                               , whitespaceFocus :: FocusStartEnd -- preceding whitespace & focus
                               , tokenFocus :: FocusStartEnd        -- focus in token 
                               } deriving Show

type WhitespaceMap = Map IDP TokenLayout   -- Whitespace information for each element in Presentation

type IDPCounter = Int                   -- Counter for generating new unique IDPs

--instance (Show a, Show b) => Show (FiniteMap a b) where
-- show fm = "{FiniteMap}" -- ++show (fmToList fm)
 

initLayout :: WhitespaceMap
initLayout = Map.fromList []

data EditPresentation' doc node clip token =
    SetPres' (PresentationLevel doc node clip token)
  | SkipPres' Int deriving Show

data EditPresentation documentLevel doc node clip token =
    SkipPres Int
  | SetFocusPres FocusPres
  | SetPres (PresentationLevel doc node clip token)
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

type Position = Int

data Token doc node clip token = 
               UserTk       Position token String (Maybe node) IDP
             | StructuralTk Position (Maybe node) (Presentation doc node clip token) [Token doc node clip token] IDP
             | ParsingTk        (Presentation doc node clip token) [Token doc node clip token] IDP -- deriving (Show)
             | GraphTk          Dirty [(Int, Int)] (Maybe node) IDP
             | VertexTk         Int (Int, Int) (Maybe node) IDP
             | ErrorTk      Position String -- for storing scanner errors
-- the IDP field is used during the scanning and parsing phase

-- The Position field in UserTk StructuralTk and ErrorTk contains its position in the list of scanned tokens
-- The other tokens are not produced by the scanner, and therefore do not need this field.
-- The position is only used for children of ParsingTk. StructuralTk children of a StructuralTk all have
-- position 0
instance (Show node, Show token) => Show (Token doc node clip token) where
  show (UserTk nr u s _ id)         = "<"++show nr ++":"++"\""++show u++"\":"++show s++":"++show id++">"
  show (StructuralTk nr Nothing _ tks id) = "<"++show nr ++":"++"structural:Nothing:"++show id++">" 
  show (StructuralTk nr (Just node) _ tks id) = 
    let showNode = show node -- not the nicest way of showing the constructor. Maybe include this in the node class
        nodeStr = if "Node_" `isPrefixOf` showNode
                  then drop (length "Node_") showNode
                  else nodeStr
    in  "<"++show nr ++":"++"structural:"++nodeStr++":"++show id++">" 
  show (ParsingTk _ tks _)       = "<parsing>" 
  show (GraphTk _ edges _ _)     = "<graph:"++show edges++">"
  show (VertexTk id pos _ _)     = "<vertex: "++show id++">"
  show (ErrorTk nr str)             = "<"++show nr ++":"++"error: "++show str++">"

instance (Eq node, Eq token) => Eq (Token doc node clip token) where
  UserTk _ u1 _ _ _     == UserTk _ u2 _ _ _     = u1 == u2
  StructuralTk _ Nothing _ _ _    == StructuralTk _ _ _ _ _ = True       -- StructuralTks with no node always match
  StructuralTk _ _ _ _ _          == StructuralTk _ Nothing _ _ _ = True -- StructuralTks with no node always match
  StructuralTk _ (Just nd1) _ _ _ == StructuralTk _(Just nd2) _ _ _ = nd1 == nd2
  ParsingTk _ _ _    == ParsingTk _ _ _ = True   
  GraphTk _ _ _ _  == GraphTk _ _ _ _  = True
  VertexTk _ _ _ _ == VertexTk _ _ _ _ = True -- if we want to recognize specific vertices, maybe some
                                              -- identifier will be added, which will be involved in eq. check
  ErrorTk _ _  == ErrorTk _ _              = True
  _              == _                  = False

instance (Ord node, Ord token) => Ord (Token doc node clip token) where
  UserTk _ u1 _ _ _      <= UserTk _ u2 _ _ _    = u1 <= u2
  StructuralTk _ Nothing _ _ _    <= StructuralTk _ _ _ _ _ = True     
  StructuralTk _ _ _ _ _          <= StructuralTk _ Nothing _ _ _ = True
  StructuralTk _ (Just nd1) _ _ _ <= StructuralTk _ (Just nd2) _ _ _ = nd1 <= nd2
  StructuralTk _ _ _ _ _ <= UserTk _ _ _ _ _  = True
  ParsingTk _ _ _ <= ParsingTk _ _ _      = True
  ParsingTk _ _ _ <= StructuralTk _ _ _ _ _ = True
  ParsingTk _ _ _ <= UserTk _ _ _ _ _       = True
  GraphTk _ _ _ _ <= GraphTk _ _ _ _      = True
  GraphTk _ _ _ _ <= ParsingTk _ _ _      = True
  GraphTk _ _ _ _ <= StructuralTk _ _ _ _ _ = True
  GraphTk _ _ _ _ <= UserTk _ _ _ _ _       = True
  VertexTk _ _  _ _ <= VertexTk _ _ _ _    = True
  VertexTk _ _ _ _ <= GraphTk _ _ _ _      = True
  VertexTk _ _ _ _ <= ParsingTk _ _ _      = True
  VertexTk _ _ _ _ <= StructuralTk _ _ _ _ _ = True
  VertexTk _ _ _ _ <= UserTk _ _ _ _ _       = True 
  ErrorTk _ _        <= ErrorTk _ _            = True
  ErrorTk _ _        <= VertexTk _ _ _ _     = True
  ErrorTk _ _        <= GraphTk _ _ _ _      = True
  ErrorTk _ _        <= ParsingTk _ _ _      = True
  ErrorTk _ _        <= StructuralTk _ _ _ _ _ = True
  ErrorTk _ _        <= UserTk _ _ _ _ _       = True
  _                <= _           = False

tokenString :: Token doc node clip token -> String                  
tokenString (UserTk _ _ s n id)      = s
tokenString (StructuralTk _ n _ _ id) = "<structural token>"
tokenString (GraphTk d es n id) = "<graph token>"
tokenString (VertexTk i p n id) = "<vertex token>"
tokenString (ErrorTk _ str)       = "<error token>"
                             
tokenNode :: Token doc node clip token -> Maybe node                 
tokenNode (StructuralTk _ n _ _ id) = n
tokenNode (GraphTk d es n id) = n
tokenNode (VertexTk i p n id) = n
tokenNode (UserTk _ u s n id)   = n
tokenNode (ErrorTk _ str)       = error $ "tokenNode called on error token: " ++ str

tokenIDP :: Token doc node clip token -> IDP       
tokenIDP (UserTk _ u s n id) = id
tokenIDP (StructuralTk _ n _ _ id)  = id
tokenIDP (GraphTk d es n id) = id
tokenIDP (VertexTk i p n id) = id
tokenIDP (ErrorTk _ str)       = error $ "tokenIDP called on error token: " ++ str

deepShowTks i tok = case tok of
                      (StructuralTk _ _ _ cs _) -> indent i ++ show tok ++ "\n"
                                               ++ indent (i+1)++"[\n"
                                               ++ concatMap (deepShowTks (i+1)) cs 
                                               ++ indent (i+1)++" ]\n"
                      (ParsingTk _ cs _) -> indent i ++ show tok ++ "\n"
                                               ++ indent (i+1)++"[\n"
                                               ++ concatMap (deepShowTks (i+1)) cs 
                                               ++ indent (i+1)++" ]\n"
                      _                     -> indent i ++ show tok ++ "\n" 
 where indent i = take i (repeat ' ')




data Presentation doc node clip token = EmptyP !IDP
           | StringP !IDP !String
           | TokenP !IDP !(Token doc node clip token)
           | ImageP !IDP !String !ImgStyle
           | PolyP !IDP ![ (Float, Float) ] !Int !Style -- pointList (0.0-1.0) lineWidth
           | RectangleP !IDP !Int !Int !Int !Style      -- width height lineWidth
           | EllipseP !IDP !Int !Int !Int !Style      -- width height lineWidth
           | RowP !IDP !Int ![Presentation doc node clip token]    -- vRefNr 
           | ColP !IDP !Int !Formatted ![Presentation doc node clip token]    -- hRefNr
           | OverlayP !IDP ![ (Presentation doc node clip token) ] -- 1st elt is in front of 2nd, etc.
           | WithP !(AttrRule doc clip) !(Presentation doc node clip token)         -- do these last two have ids?
           | StructuralP !IDP !(Presentation doc node clip token)       -- IDP?
           | ParsingP !IDP !Lexer !(Presentation doc node clip token)         -- IDP?
           | LocatorP node !(Presentation doc node clip token) -- deriving Show -- do we want a ! for location  ? 
           | GraphP !IDP !Dirty !Int !Int ![(Int,Int)] ![Presentation doc node clip token] -- width height edges 
           | VertexP !IDP !Int !Int !Int Outline !(Presentation doc node clip token) -- vertexID x y outline       see note below
           | FormatterP !IDP ![Presentation doc node clip token]

{-         | Matrix [[ (Presentation doc node clip) ]]       -- Stream is not a list because tree is easier in presentation.
           | Formatter [ (Presentation doc node clip) ]
           | Alternative [ (Presentation doc node clip) ]
-} -- are the !'s in the right place like this?
           | ArrangedP -- experimental for incrementality.
                           -- arranger gets Presentation in which unchanged subtrees are replaced by
                           -- this node. For these subtrees, old arrangement is used


-- This datatype will be in the non-generic part of Proxima in the future. (when an extensible scanner is available)
-- LexInherited can be used if higher in the presentation tree the lexer is already defined.
data Lexer = LexFreeText | LexHaskell | LexInherited deriving Show
-- Lexer info is not passed on to arrangement, since scanning takes place on the presentation datatype
-- and does not involve the arrangement.

-- Note: An alternative and safer definition for GraphP is GraphP [Vertex], but this requires all functions that
-- traverse Presentation to have a separate function for traversing the Vertex type. This is too much of a hassle.


-- slightly less verbose show for presentation, without doc refs

instance (Show node, Show token) => Show (Presentation doc node clip token) where
  show (EmptyP id)           = "{"++show id++":Empty}"
  show (StringP id str)      = "{"++show id++":"++show str++"}"
  show (TokenP id t)         = "{"++show id++":"++show t++"}"
  show (ImageP id str _)     = "{"++show id++":Image "++str++"}"
  show (PolyP id _ _ _)        = "{"++show id++":Poly}"
  show (RectangleP id _ _ _ _) = "{"++show id++":Rectangle}"
  show (EllipseP id _ _ _ _)   = "{"++show id++":Ellipse}"
  show (RowP id rf press)    = "RowP "++show rf++" ["++concat (intersperse ", " (map show press))++"]"
  show (ColP id rf f press)    = "ColP "++show rf++" "++show f++" ["++concat (intersperse ", " (map show press))++"]"
  show (OverlayP  id press)  = "OverlayP ["++concat (intersperse ", " (map show press))++"]"
  show (WithP ar pres)       = "WithP <fn> "++show pres
  show (StructuralP id pres) = "StructuralP "++show id++" "++show pres
  show (ParsingP id l pres)    = "ParsingP "++show l++" "++show pres
  show (LocatorP loc pres)   = "LocatorP "++ {- show loc++ -} " "++show pres
  show (GraphP id _ _ _ edges press) = "GraphP "++ show edges++" ["++concat (intersperse ", " (map show press))++"]"
  show (VertexP id vid x y ol pres)  = "Vertex (#"++show vid++":"++show x++","++show y++")"++show pres
  show (FormatterP id press) = "FormatterP ["++concat (intersperse ", " (map show press))++"]"
  show (ArrangedP)           = "ArrangedP" -- ++show pres
  show _                     = "<<<presentation without show>>>"


-- shallow presentation, showing only toplevel presentation


shallowShowPres (EmptyP id)           = "{"++show id++":Empty}"
shallowShowPres (StringP id str)      = "{"++show id++":StringP "++show str++"}"
shallowShowPres (TokenP id t)         = "{"++show id++":TokenP "++show t++"}"
shallowShowPres (ImageP id str _)       = "{"++show id++":ImageP "++show str++"}"
shallowShowPres (PolyP id _ _ _)        = "{"++show id++":Poly}"
shallowShowPres (RectangleP id _ _ _ _) = "{"++show id++":Rectangle}"
shallowShowPres (EllipseP id _ _ _ _)   = "{"++show id++":Ellipse}"
shallowShowPres (RowP id rf press)    = "{"++show id++":RowP, #children="++show (length press)++"}"
shallowShowPres (ColP id rf f press)    = "{"++show id++":ColP, f= "++show f++", #children="++show (length press)++"}"
shallowShowPres (OverlayP  id press)  = "{"++show id++":Overlay, #children="++show (length press)++"}"
shallowShowPres (FormatterP  id press)  = "{"++show id++":Formatter, #children="++show (length press)++"}"
shallowShowPres (GraphP id _ _ _ _ press)  = "{"++show id++":Graph, #children="++show (length press)++"}"
shallowShowPres (VertexP _ _ x y _  pres)  = "{"++show id++":Vertex, x="++show x++",y="++show y++"}"
shallowShowPres (WithP ar pres)       = "{WithP}"
shallowShowPres (StructuralP id pres) = "{"++show id++":StructuralP}"
shallowShowPres (ParsingP id l pres)    = "{"++show id++":ParsingP}"
shallowShowPres (LocatorP loc pres)   = "{LocatorP}"
shallowShowPres (ArrangedP)           = "ArrangedP" -- ++show pres
shallowShowPres _                     = "<<<presentation without show>>>"

getChildrenP (EmptyP id)           = []
getChildrenP (StringP id str)      = []
getChildrenP (TokenP id str)      = []
getChildrenP (ImageP id str _)       = []
getChildrenP (PolyP id _ _ _)        = []
getChildrenP (RectangleP id _ _ _ _) = []
getChildrenP (EllipseP id _ _ _ _)   = []
getChildrenP (RowP id rf press)    = press
getChildrenP (ColP id rf _ press)    = press
getChildrenP (OverlayP  id press)  = press
getChildrenP (FormatterP  id press) = press
getChildrenP (GraphP id _ _ _ _ press) = press
getChildrenP (VertexP _ _ x y _  pres) = [pres]
getChildrenP (WithP ar pres)       = [pres]
getChildrenP (StructuralP id pres) = [pres]
getChildrenP (ParsingP id l pres)    = [pres]
getChildrenP (LocatorP loc pres)   = [pres]
getChildrenP (ArrangedP)           = []
getChildrenP pres                  = debug Err ("PresTypes.getChildren: unhandled presentation: "++shallowShowPres pres) []

getChildP pres = case getChildrenP pres of
                  [child] -> child
                  _       -> debug Err ("PresTypes.getChild: not a single-child presentation: "++shallowShowPres pres) $ EmptyP NoIDP

setChildrenP [] pres@(EmptyP id)           = pres
setChildrenP [] pres@(StringP id str)      = pres
setChildrenP [] pres@(TokenP id str)      = pres
setChildrenP [] pres@(ImageP id str _)     = pres
setChildrenP [] pres@(PolyP id _ _ _)      = pres
setChildrenP [] pres@(RectangleP id _ _ _ _) = pres
setChildrenP [] pres@(EllipseP id _ _ _ _)   = pres
setChildrenP press' (RowP id rf _)     = RowP id rf press'
setChildrenP press' (ColP id rf f _)     = ColP id rf f press'
setChildrenP press' (OverlayP  id _)   = OverlayP id press'
setChildrenP press' (FormatterP  id _) = FormatterP id press'
setChildrenP press' (GraphP id d w h es _) = GraphP id d w h es press'
setChildrenP [pres'] (VertexP id vid x y ol _) = VertexP id vid x y ol pres'
setChildrenP [pres'] (WithP ar _)       = WithP ar pres'
setChildrenP [pres'] (StructuralP id _) = StructuralP id pres'
setChildrenP [pres'] (ParsingP id l _)    = ParsingP id l pres'
setChildrenP [pres'] (LocatorP loc _)   = LocatorP loc pres'
setChildrenP []      (ArrangedP)        = ArrangedP
setChildrenP press'  pres                  = debug Err ("PresTypes.setChildrenP: unhandled case " ++ show (length press') ++ ", " ++ shallowShowPres pres) pres


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
idP (TokenP id _)        = id
idP (ImageP id str _)       = id
idP (PolyP id _ _ _)        = id
idP (RectangleP id _ _ _ _) = id
idP (EllipseP id _ _ _ _)   = id
idP (RowP id _ _)         = id
idP (ColP id _ _ _)       = id
idP (OverlayP  id press)  = id
idP (WithP ar pres)       = idP pres
idP (StructuralP id pres) = id
idP (ParsingP id l pres)    = id
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



-- because StructuralTk and PresentationTk contain the Layout in case of parse errors, we need
-- to declare the Layout type here in PresTypes.hs instead of LayTypes.hs.

data Layout_   -- type without constructor to use as token parameter, so values of Layout
               -- are guaranteed not to have a TokenP case.
instance Show Layout_

instance Eq Layout_

type Layout doc node clip = Presentation doc node clip Layout_
