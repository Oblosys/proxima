module Arrangement.ArrTypes where

import Common.CommonTypes
import Evaluation.DocTypes (DocumentLevel) -- for Locations

import Layout.LayTypes
                  
data IDA = NoIDA | IDA Int deriving (Show, Read, Eq, Ord)
               
                                                              -- ugly hack for popups, need pres to get items
data ArrangementLevel doc node clip token = ArrangementLevel (Arrangement node) FocusArr (Layout doc node clip token) deriving Show

data EditArrangement' doc node clip token =
    SetArr' (ArrangementLevel doc node clip token)
  | SkipArr' Int deriving Show

data EditArrangement documentLevel =
    SkipArr Int
  | SetFocusArr FocusArr
  | InitArr
  | CloseArr
  | CutArr
  | CopyArr
  | PasteArr
  | DeleteArr -- probably don't need delete because right delete can take its function
  | SplitArr
  | LeftDeleteArr
  | RightDeleteArr
  | LeftArr
  | RightArr
  | EnlargeLeftArr
  | EnlargeRightArr
  | NormalizeArr
  | ParseArr
  | Test2Arr
  | KeyCharArr Char
  | KeySpecialArr SpecialKey Modifiers
  | MouseDownArr Int Int Modifiers Int
  | MouseDragArr Int Int Modifiers 
  | MouseUpArr Int Int Modifiers
  | OpenFileArr String
  | SaveFileArr String
  | UndoDocArr
  | RedoDocArr
  | UpdateDocArr (documentLevel -> documentLevel) -- should encapsulate these so they automatically go to doc level
  | NavUpDocArr
  | NavDownDocArr
  | NavLeftDocArr
  | NavRightDocArr
  | CutDocArr
  | CopyDocArr
  | PasteDocArr
  | DeleteDocArr                                  --
  | MouseDownDocArr PathArr Modifiers Int -- bit hacky, will disappear
  | DocumentLoadedArr String deriving Show


-- node is parameter for Node type
data Arrangement node =
    EmptyA      !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !Color
  | StringA     !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !String !FGColor !BGColor !Font [Int]
  | ImageA      !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !String !ImgStyle !FGColor !BGColor
  | PolyA       !IDA  !XCoord !YCoord !Width !Height !HRef !VRef ![(XCoord, YCoord)] !Int !Style !LineColor !FillColor !BGColor
  | RectangleA  !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !Int !Style !LineColor !FillColor !BGColor
  | EllipseA    !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !Int !Style !LineColor !FillColor !BGColor
  | RowA        !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !BGColor ![Arrangement node]
  | ColA        !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !BGColor !Formatted ![Arrangement node]
  | OverlayA    !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !BGColor !Direction ![Arrangement node]
  | StructuralA !IDA  !(Arrangement node)
  | ParsingA    !IDA  !(Arrangement node)
  | GraphA      !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !BGColor !NrOfVertices ![Arrangement node]
  | VertexA     !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !BGColor !Outline !(Arrangement node)
  | EdgeA       !IDA  !XCoord !YCoord !XCoord !YCoord !HRef !VRef !Int !LineColor
  | LocatorA    node !(Arrangement node) deriving (Show) -- do we want a ! for location  ?  

unarrangedA x y w h hrf vrf =
  PolyA (IDA (-10)) x y w h hrf vrf [(0,0),(w',0),(w',h'),(0,h'),(0,0),(w',h'),(w',0),(0,h')] 1 Transparent black white transparent
 where (h',w') = ((h-1) `max` 0, (w-1) `max` 0)


-- empty will have size when it is stretched. But maybe empty should be ignored in the arrangement,
-- although it might have a background color. Check out what desired focus behaviour is when navigating
-- over empty's

-- Strings have no background color! this is illogical
-- need a transparent color

-- *** having a WithA makes the arrangement a lot smaller and rendering faster. Are there objections?

-- *** a more logical data structure might be one in which rows and columns contain the positions 
-- of the children, and elements have no position of their own.
-- How does that affect dirty bits? And locators/with nodes?


-- Lines are different from all other arrangement elements with their corner coordinates. 
-- Note:  Line from x to x+w includes both x and x+w whereas box line from x to x+w does not include points on x+w

-- should Empty have x y w and h
-- Can empties have nonzero size? Background color? Focus? If they can they're not really neutral
-- elements anymore. This is also a presentation question.
-- maybe Empties should not even be part of the arrangement. Still, in the presentation, we want to have something
-- like stretching glue. using a filled rectangle is not what we want.
-- locators need coordinates! so we can show them in debug arrangements nicely


-- overlay problem needs to be solved. Sometimes front object is not the parsed one
-- Workaround hack: if last elt is empty, the overlay is presented in reverse order, but parsed normally
-- so overlay [Exp, squiggly, empty] is presented with squiggly in front


-- last argument of StringA is a list of offsets of the characters in the string. 
-- TODO: don't want cumulative character widths           (? why not?)
-- TODO: background for StringA, RectangleA, and EllipseA




-- do we really need that index?
data PathArr = PathA [Int] Int 
              | NoPathA deriving (Show, Eq, Ord)


-- focus? (from, to)? yes, seems ok, now pres/formatting difference is not very big. even formatter follows normal
-- directions, so from to in arr is from to in pres. Maybe for more complicated presentations we can keep this property


data FocusArr = FocusA PathArr PathArr
              | NoFocusA deriving Show 

-- At this time, Focus path is always in a leaf string

-- focus is not perfect yet. Selection halfway a letter should include the letter (or element). This makes
-- enlargement dependent on drag origin. Furthermore, pointing in empty space should lead to some path. 
-- E.g. first thing on the left in a column and first thing above in a column.

-- pointing in stretched rows might lead to a focus which is not a leaf path, a next version of the
-- rendering level must handle this correctly. For now, non stretching rows are assumed

-- smart constructor

focusA from@(PathA _ _) to@(PathA _ _) = FocusA from to
focusA _ _                             = NoFocusA

-- selectors

focusAL (ArrangementLevel _ f _) = f


fromA (FocusA from _) = from
fromA NoFocusA        = NoPathA

toA (FocusA _ to) = to
toA NoFocusA        = NoPathA

shallowShowArr (EmptyA _ x y w h _ _ _)           = "{EmptyA: x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (StringA _ x y w h _ _ str _ _ _ _)= "{StringA \""++str++"\": x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (ImageA _ x y w h _ _ src _ _ _)   = "{ImageA: \""++src++"\": x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (PolyA _ x y w h _ _ _ _ _ _ _ _)  = "{PolyA: x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (RectangleA _ x y w h _ _ _ _ _ _ _) = "{RectangleA: x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (EllipseA _ x y w h _ _ _ _ _ _ _)   = "{EllipseA: x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (RowA _ x y w h _ _ _ arrs)        = "{RowA: x="++show x++", y="++show y++", w="++show w++", h="++show h++", #children"++show (length arrs)++"}"
shallowShowArr (ColA _ x y w h _ _ _ f arrs)        = "{ColA: x="++show x++", y="++show y++", w="++show w++", h="++show h++", f="++show f++", children"++show (length arrs)++"}"
shallowShowArr (OverlayA _ x y w h _ _ _ _ arrs)    = "{OverlayA: x="++show x++", y="++show y++", w="++show w++", h="++show h++", #children"++show (length arrs)++"}"
shallowShowArr (GraphA _ x y w h _ _ _ _ arrs)    = "{GraphA: x="++show x++", y="++show y++", w="++show w++", h="++show h++", #children"++show (length arrs)++"}"
shallowShowArr (VertexA _ x y w h _ _ _ _ _)      = "{VertexA: x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (EdgeA _ x y x' y' _ _ _ _)        = "{EdgeA: x="++show x++", y="++show y++", x'="++show x'++", y'="++show y'++"}"
shallowShowArr (StructuralA _ child)              = "{StructuralA}"
shallowShowArr (ParsingA _ child)                 = "{ParsingA}"
shallowShowArr (LocatorA location child)          = "{LocatorA}"
shallowShowArr arr                                = "{Arrangement not handled by shallowShowArr: "++show arr++"}"


xA (EmptyA _ x y w h _ _ _)           = x
xA (StringA _ x y w h _ _ _ _ _ _ _)  = x
xA (ImageA _ x y w h _ _ _ _ _ _)     = x
xA (PolyA _ x y w h _ _ _ _ _ _ _ _)  = x
xA (RectangleA _ x y w h _ _ _ _ _ _ _) = x
xA (EllipseA _ x y w h _ _ _ _ _ _ _)   = x
xA (RowA _ x y w h _ _ _ _)           = x
xA (ColA _ x y w h _ _ _ _ _)         = x
xA (OverlayA _ x y w h _ _ _ _ _)       = x
xA (GraphA _ x y w h _ _ _ _ _)         = x
xA (VertexA _ x y w h _ _ _ _ _)        = x
xA (EdgeA _ x y x' y' _ _ _ _)        = min x x'
xA (StructuralA _ child)          = xA child
xA (ParsingA _ child)             = xA child
xA (LocatorA location child)      = xA child
xA arr                            = debug Err ("ArrTypes.xA: unhandled arrangement "++show arr) 0

yA (EmptyA _ x y w h _ _ _)           = y
yA (StringA _ x y w h _ _ _ _ _ _ _)  = y
yA (ImageA _ x y w h _ _ _ _ _ _)     = y
yA (PolyA _ x y w h _ _ _ _ _ _ _ _)  = y
yA (RectangleA _ x y w h _ _ _ _ _ _ _) = y
yA (EllipseA _ x y w h _ _ _ _ _ _ _)   = y
yA (RowA _ x y w h _ _ _ _)           = y
yA (ColA _ x y w h _ _ _ _ _)         = y
yA (OverlayA _ x y w h _ _ _ _ _)       = y
yA (GraphA _ x y w h _ _ _ _ _)         = y
yA (VertexA _ x y w h _ _ _ _ _)        = y
yA (EdgeA _ x y x' y' _ _ _ _)        = min y y'
yA (StructuralA _ child)          = yA child
yA (ParsingA _ child)             = yA child
yA (LocatorA location child)      = yA child
yA arr                            = debug Err ("ArrTypes.yA: unhandled arrangement "++show arr) 0

widthA (EmptyA _ x y w h _ _ _)           = w
widthA (StringA _ x y w h _ _ _ _ _ _ _)  = w
widthA (ImageA _ x y w h _ _ _ _ _ _)     = w
widthA (PolyA _ x y w h _ _ _ _ _ _ _ _)  = w
widthA (RectangleA _ x y w h _ _ _ _ _ _ _) = w
widthA (EllipseA _ x y w h _ _ _ _ _ _ _)   = w
widthA (RowA _ x y w h _ _ _ _)           = w
widthA (ColA _ x y w h _ _ _ _ _)         = w
widthA (OverlayA _ x y w h _ _ _ _ _)       = w
widthA (GraphA _ x y w h _ _ _ _ _)         = w
widthA (VertexA _ x y w h _ _ _ _ _)        = w
widthA (EdgeA _ x y x' y' _ _ _ _)        = max x x' - min x x'
widthA (StructuralA _ child)          = widthA child
widthA (ParsingA _ child)             = widthA child
widthA (LocatorA location child)      = widthA child
widthA arr                            = debug Err ("ArrTypes.widthA: unhandled arrangement "++show arr) 0

heightA (EmptyA _ x y w h _ _ _)           = h
heightA (StringA _ x y w h _ _ _ _ _ _ _)  = h
heightA (ImageA _ x y w h _ _ _ _ _ _)     = h
heightA (PolyA _ x y w h _ _ _ _ _ _ _ _)  = h
heightA (RectangleA _ x y w h _ _ _ _ _ _ _) = h
heightA (EllipseA _ x y w h _ _ _ _ _ _ _)   = h
heightA (RowA _ x y w h _ _ _ _)           = h
heightA (ColA _ x y w h _ _ _ _ _)         = h
heightA (OverlayA _ x y w h _ _ _ _ _)       = h
heightA (GraphA _ x y w h _ _ _ _ _)         = h
heightA (VertexA _ x y w h _ _ _ _ _)        = h
heightA (EdgeA _ x y x' y' _ _ _ _)        = max y y' - min y y'
heightA (StructuralA _ child)          = heightA child
heightA (ParsingA _ child)             = heightA child
heightA (LocatorA location child)      = heightA child
heightA arr                            = debug Err ("ArrTypes.heightA: unhandled arrangement "++show arr) 0
 
hRefA (EmptyA _ x y w h hr vr _)           = hr
hRefA (StringA _ x y w h hr vr _ _ _ _ _)  = hr
hRefA (ImageA _ x y w h hr vr _ _ _ _)     = hr
hRefA (PolyA _ x y w h hr vr _ _ _ _ _ _)  = hr
hRefA (RectangleA _ x y w h hr vr _ _ _ _ _) = hr
hRefA (EllipseA _ x y w h hr vr _ _ _ _ _)   = hr
hRefA (RowA _ x y w h hr vr _ _)           = hr
hRefA (ColA _ x y w h hr vr _ _ _)         = hr
hRefA (OverlayA _ x y w h hr vr _ _ _)       = hr
hRefA (GraphA _ x y w h hr vr _ _ _)       = hr
hRefA (VertexA _ x y w h hr vr _ _ _)      = hr
hRefA (EdgeA _ x y x' y' hr vr _ _)        = hr
hRefA (StructuralA _ child)                = hRefA child
hRefA (ParsingA _ child)                   = hRefA child
hRefA (LocatorA location child)            = hRefA child
hRefA arr                                  = debug Err ("ArrTypes.hRefA: unhandled arrangement "++show arr) 0

vRefA (EmptyA _ x y w h hr vr _)           = vr
vRefA (StringA _ x y w h hr vr _ _ _ _ _)  = vr
vRefA (ImageA _ x y w h hr vr _ _ _ _)     = vr
vRefA (PolyA _ x y w h hr vr _ _ _ _ _ _)  = vr
vRefA (RectangleA _ x y w h hr vr _ _ _ _ _) = vr
vRefA (EllipseA _ x y w h hr vr _ _ _ _ _)   = vr
vRefA (RowA _ x y w h hr vr _ _)           = vr
vRefA (ColA _ x y w h hr vr _ _ _)         = vr
vRefA (OverlayA _ x y w h hr vr _ _ _)       = vr
vRefA (GraphA _ x y w h hr vr _ _ _)       = vr
vRefA (VertexA _ x y w h hr vr _ _ _)      = vr
vRefA (EdgeA _ x y x' y' hr vr _ _)        = vr
vRefA (StructuralA _ child)                = vRefA child
vRefA (ParsingA _ child)                   = vRefA child
vRefA (LocatorA location child)            = vRefA child
vRefA arr                                  = debug Err ("ArrTypes.vRefA: unhandled arrangement "++show arr) 0

-- use named fields?



idA (EmptyA id x y w h _ _ _)           = id
idA (StringA id x y w h _ _ _ _ _ _ _)  = id
idA (ImageA id x y w h _ _ _ _ _ _)     = id
idA (PolyA id x y w h _ _ _ _ _ _ _ _)  = id
idA (RectangleA id x y w h _ _ _ _ _ _ _) = id
idA (EllipseA id x y w h _ _ _ _ _ _ _)   = id
idA (RowA id x y w h _ _ _ _)           = id
idA (ColA id x y w h _ _ _ _ _)         = id
idA (OverlayA id x y w h _ _ _ _ _)       = id
idA (GraphA id x y w h hr vr _ _ _)       = id
idA (VertexA id x y w h hr vr _ _ _)      = id
idA (EdgeA id x y x' y' _ _ _ _)        = id
idA (StructuralA _ child)          = idA child
idA (ParsingA _ child)             = idA child
idA (LocatorA location child)      = idA child
idA arr                            = debug Err ("ArrTypes.idA: unhandled arrangement "++show arr) NoIDA

getChildrenA (RowA     _ _ _ _ _ _ _ _ arrs)   = arrs
getChildrenA (ColA     _ _ _ _ _ _ _ _ _ arrs) = arrs 
getChildrenA (OverlayA _ _ _ _ _ _ _ _ _ arrs)   = arrs
getChildrenA (GraphA   _ _ _ _ _ _ _ _ _ arrs) = arrs
getChildrenA (VertexA  _ _ _ _ _ _ _ _ _ arr)  = [arr]
getChildrenA (StructuralA _ arr)               = [arr]
getChildrenA (ParsingA _ arr)                  = [arr]
getChildrenA (LocatorA _ arr)                  = [arr]
getChildrenA _                                 = []

getChildA :: Show node => String -> Arrangement node -> Arrangement node
getChildA caller arr = case getChildrenA arr of
  [child] -> child
  []      -> debug Err ("ArrUtils.getChildA: not a single-child arrangement " ++show arr++ caller) arr

getAreaA :: Show node => Arrangement node -> Rectangle
getAreaA a = ((xA a, yA a), (widthA a, heightA a))


setXYWHA x y w h (EmptyA id _ _ _ _ hr vr bc)                  = EmptyA id x y w h  hr vr bc         
setXYWHA x y w h (StringA id _ _ _ _ hr vr str c bc f cxs)     = StringA id x y w h hr vr str c bc f cxs
setXYWHA x y w h (ImageA id _ _ _ _ hr vr src style lc bc)     = ImageA id x y w h hr vr src style lc bc        
setXYWHA x y w h (PolyA id _ _ _ _ hr vr  pts lw style lc fc bc)        = PolyA id x y w h hr vr pts lw style lc fc bc            
setXYWHA x y w h (RectangleA id _ _ _ _ hr vr  lw style lc fc bc) = RectangleA id x y w h hr vr lw style lc fc bc    
setXYWHA x y w h (EllipseA id _ _ _ _ hr vr  lw style lc fc bc)   = EllipseA id x y w h hr vr lw style lc fc bc
setXYWHA x y w h (RowA id _ _ _ _ hr vr  c arrs)               = RowA id x y w h hr vr c arrs                   
setXYWHA x y w h (ColA id _ _ _ _ hr vr  c f arrs)               = ColA id x y w h hr vr c f arrs                   
setXYWHA x y w h (OverlayA id _ _ _ _ hr vr  c d arrs)           = OverlayA id x y w h hr vr c d arrs               
setXYWHA x y w h (GraphA id  _ _ _ _ hr vr c nvs arrs)            = GraphA id x y w h hr vr  c nvs arrs
setXYWHA x y w h (VertexA id _ _ _ _ hr vr c ol arr)              = VertexA id x y w h hr vr  c ol arr
setXYWHA x y w h (LocatorA location arr)                = LocatorA location $ setXYWHA x y w h arr
setXYWHA x y w h (StructuralA id arr)                   = StructuralA id $ setXYWHA x y w h arr
setXYWHA x y w h (ParsingA id arr)                      = ParsingA id    $ setXYWHA x y w h arr                         
setXYWHA _ _ _ _ arr                                    = debug Err ("ArrTypes.setXYWHA: unimplemented arrangement: "++show arr) arr

setBGColor bc (EmptyA id x y w h hr vr _)                  = EmptyA id x y w h  hr vr bc         
setBGColor bc (StringA id x y w h hr vr str c _ f cxs)     = StringA id x y w h hr vr str c bc f cxs
setBGColor bc (ImageA id x y w h hr vr src style lc _)     = ImageA id x y w h hr vr src style lc bc        
setBGColor bc (PolyA id x y w h hr vr  pts lw style lc fc _)        = PolyA id x y w h hr vr pts lw style lc fc bc            
setBGColor bc (RectangleA id x y w h hr vr  lw style lc fc _) = RectangleA id x y w h hr vr lw style lc fc bc    
setBGColor bc (EllipseA id x y w h hr vr  lw style lc fc _)   = EllipseA id x y w h hr vr lw style lc fc bc
setBGColor bc (RowA id x y w h hr vr _ arrs)               = RowA id x y w h hr vr bc arrs                   
setBGColor bc (ColA id x y w h hr vr _ f arrs)               = ColA id x y w h hr vr bc f arrs                   
setBGColor bc (OverlayA id x y w h hr vr _ d arrs)           = OverlayA id x y w h hr vr bc d arrs               
setBGColor bc (GraphA id  x y w h hr vr _ nvs arrs)            = GraphA id x y w h hr vr  bc nvs arrs
setBGColor bc (VertexA id x y w h hr vr _ ol arr)              = VertexA id x y w h hr vr  bc ol arr
setBGColor bc (LocatorA location arr)                = LocatorA location $ setBGColor bc arr
setBGColor bc (StructuralA id arr)                   = StructuralA id $ setBGColor bc arr
setBGColor bc (ParsingA id arr)                      = ParsingA id    $ setBGColor bc arr                         
setBGColor _ arr                                    = debug Err ("ArrTypes.setBGColor: unimplemented arrangement: "++show arr) arr
