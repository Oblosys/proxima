module Arrangement.ArrUtils where

import Common.CommonTypes
import Arrangement.ArrTypes
import Common.CommonUtils
import Proxima.Wrap

import Control.Exception
import Data.Maybe 
import Data.IORef

-- utils

ifFocusA NoFocusA           _   = NoFocusA
ifFocusA (FocusA NoPathA _) _   = NoFocusA
ifFocusA (FocusA _ NoPathA) _   = NoFocusA
ifFocusA (FocusA _ _)       exp = exp

ifPathA NoPathA _       = NoPathA
ifPathA (PathA _ _) exp = exp

focusStartPathA (FocusA (PathA pth _) _) = pth
focusStartPathA _                        = []


orderFocusA foc@(FocusA from to) = ifFocusA foc $ FocusA (min from to) (max from to)

showPathNodesA :: Show node => Path -> Arrangement node -> String
showPathNodesA []      arr = shallowShowArr arr
showPathNodesA (p:pth) arr = shallowShowArr arr ++ "\n" ++
                               let children = getChildrenA arr
                               in  if p < length children
                                   then showPathNodesA pth (index "ArrUtils.showPathNodesA" children p)
                                   else "ArrUtils.showPathNodesA: index out of bounds"

sizeA :: Show node => Path -> Arrangement node -> (Int, Int, Int, Int)
sizeA path arr = sizeA' 0 0 path arr
sizeA' x' y' []       arr                               = (x' + xA arr, y' + yA arr, widthA arr, heightA arr)
sizeA' x' y' [p]      (StringA _ x y w h _ _ _ _ _ _ cxs)   = (x' + x + (index "ArrUtils.sizeA'" cxs p), y'+y, (index "ArrUtils.sizeA'" cxs p+1)-(index "ArrUtils.sizeA'" cxs p+1), h)
sizeA' x' y' (p:path) (RowA _ x y w h _ _ _ arrs)           = sizeA' (x'+x) (y'+y) path (index "ArrUtils.sizeA'" arrs p)
sizeA' x' y' (p:path) (ColA _ x y w h _ _ _ _ arrs)         = sizeA' (x'+x) (y'+y) path (index "ArrUtils.sizeA'" arrs p)
sizeA' x' y' (0:path) (OverlayA _ x y w h _ _ _ _ (arr:arrs)) = sizeA' (x'+x) (y'+y) path arr
sizeA' x' y' (p:path) (GraphA _ x y w h _ _ _ _ (arr:arrs)) = sizeA' (x'+x) (y'+y) path (index "ArrUtils.sizeA'" arrs p)
sizeA' x' y' (0:path) (VertexA _ x y w h _ _ _ _ arr)       = sizeA' (x'+x) (y'+y) path arr
sizeA' x' y' (0:path) (StructuralA _ arr)               = sizeA' x' y' path arr
sizeA' x' y' (0:path) (ParsingA _ arr)                  = sizeA' x' y' path arr
sizeA' x' y' (0:path) (LocatorA location arr)           = sizeA' x' y' path arr
sizeA' x' y' (0:path) (TagA _ arr)           = sizeA' x' y' path arr
sizeA' _  _  pth      arr                               = debug Err ("ArrUtils.sizeA': "++show pth++" "++show arr) (0,0,0,0)
-- what about focus paths in other places than leaf strings?



-- is this necessary? The data structure is strict already.
walk (EmptyA _ x y w h hr vr c)             = x+y+w+h+hr+vr + walkC c
walk (StringA _ x y w h hr vr str c bc f _) = x+y+w+h+hr+vr+length str+ walkC c+ walkC bc+ fSize f
walk (ImageA _ x y w h hr vr _ _ c1 c2)     = x+y+w+h+hr+vr + walkC c1 + walkC c2
walk (PolyA _ x y w h hr vr _ _ _ c1 c2 c3) = x+y+w+h+hr+vr + walkC c1 + walkC c2 + walkC c3
walk (RectangleA _ x y w h hr vr _ _ c1 c2 c3) = x+y+w+h+hr+vr + walkC c1 + walkC c2 + walkC c3
walk (EllipseA _ x y w h hr vr _ _ c1 c2 c3)   = x+y+w+h+hr+vr + walkC c1 + walkC c2 + walkC c3
walk (RowA _ x y w h hr vr c1 arrs)         = x+y+w+h+hr+vr + walkC c1 +  walkList arrs
walk (ColA _ x y w h hr vr c1 f arrs)         = x+y+w+h+hr+vr + walkC c1 + walkList arrs
walk (OverlayA _ x y w h hr vr c1 _ arrs)     = x+y+w+h+hr+vr + walkC c1 + walkList arrs
walk (GraphA _ x y w h hr vr c1 nvs arrs)   = x+y+w+h+hr+vr + walkC c1 + walkList arrs
walk (VertexA _ x y w h hr vr c1 ol arr)    = x+y+w+h+hr+vr + walkC c1 + walk arr
walk (EdgeA _ x y x' y' hr vr _ c1)         = x+y+x'+y'+hr+vr + walkC c1
walk (StructuralA _ arr)              = walk arr
walk (ParsingA _ arr)                 = walk arr
walk (LocatorA _ arr)                 = walk arr
walk (TagA _ arr)                 = walk arr
walk _                                = 0

walkList [] = 0
walkList (arr:arrs) = walk arr + walkList arrs

walkC (r,g,b) = r+g+b

{-
data Arrangement =
    EmptyA      !IDA !XCoord !YCoord !Width !Height 
  | StringA     !IDA !XCoord !YCoord !Width !Height !String !RColor !Font [Int]
  | ImageA      !IDA !XCoord !YCoord !Width !Height !String !ImgStyle !RColor !RColor
  | PolyA       !IDA !XCoord !YCoord !Width !Height ![(XCoord, YCoord)] !Int !RColor !RColor
  | RectangleA  !IDA !XCoord !YCoord !Width !Height !Int !Style !RColor !RColor
  | EllipseA    !IDA !XCoord !YCoord !Width !Height !Int !Style !RColor !RColor
  | RowA        !IDA !XCoord !YCoord !Width !Height !RColor ![Arrangement]
  | ColA        !IDA !XCoord !YCoord !Width !Height !RColor ![Arrangement]
  | OverlayA    !IDA !XCoord !YCoord !Width !Height !RColor ![Arrangement]
  | GraphA      !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !Color !NrOfVertices ![Arrangement node]
  | VertexA     !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !Color !Outline !(Arrangement node)
  | EdgeA       !IDA !XCoord !YCoord !XCoord !YCoord !Int !RColor
  -- | matrix is different from col of rows, even in arrangement (e.g. selection)

-}

-- for creating edge arrangements during graph arranging
-- PRECONDITION: if edges is non-empty, then vertices is non-empty as well.
mkEdges :: Show node => [(Int,Int)] -> [(Int,Int,Int, Outline)] -> Color -> [Arrangement node]
mkEdges edges vertices lineColor = concatMap mkEdge edges 
 where mkEdge (fromV, toV) = 
        case (lookupVertex fromV vertices, lookupVertex toV vertices) of
            (Just (fromVx,fromVy,fromVol), Just (toVx,toVy,toVol)) ->
              let (offsetFromx, offsetFromy) = fromVol (computeAngle fromVx fromVy toVx toVy)
                  (offsetTox, offsetToy)     = toVol   (computeAngle toVx toVy fromVx fromVy)
              in [EdgeA NoIDA  (fromVx+offsetFromx) (fromVy+offsetFromy)
                            (toVx+offsetTox)     (toVy+offsetToy)     0 0 1 lineColor]
            _ -> []
       lookupVertex vid [] = debug Err ("ArrUtils.mkEdges: edge refers to non-existing vertex: " ++ show vid) Nothing
       lookupVertex vid ((i,x,y,ol):vs) | vid == i = Just (x,y,ol)
                                        | otherwise = lookupVertex vid vs




-- add correct case for graph
-- what about diff for overlays? Should diff always return False False when a child is different?
-- or should the render have special behavior? (this is is tricky since focus is added with an
-- overlay)


-- diffArr skips Structural, Parsing and Locator arrangements, since these do not
-- influence the rendering. For the same reason, Formatter parameters to rows and
-- h/vRefs are ignored
-- we do mark them as self dirty if their child (or descendent in case of a chain) is dirty.
-- this is necessary to be able to see at parent level that a direct child may have changed size
-- (used for example in updatedRectArr)

-- Does not take into account direction of Overlay

--       new arrangement     old arrangement
diffArr (StructuralA _ arr) arr'                   = let childDT = diffArr arr arr'
                                                      in  DiffNodeArr (isCleanDTArr childDT) (isSelfCleanDTArr childDT) (getMove childDT) (getInsertDelete childDT) [childDT]
diffArr arr                  (StructuralA _ arr')  = diffArr arr arr'
diffArr (ParsingA _ arr)    arr'                   = let childDT = diffArr arr arr'
                                                      in  DiffNodeArr (isCleanDTArr childDT) (isSelfCleanDTArr childDT) (getMove childDT) (getInsertDelete childDT) [childDT]
diffArr arr                  (ParsingA _ arr')     = diffArr arr arr'
diffArr (LocatorA l arr)     arr'                   = let childDT = diffArr arr arr'
                                                      in  DiffNodeArr (isCleanDTArr childDT) (isSelfCleanDTArr childDT) (getMove childDT) (getInsertDelete childDT) [childDT]
diffArr arr                  (LocatorA l arr')      = diffArr arr arr'
diffArr (TagA t arr)     arr'                   = let childDT = diffArr arr arr'
                                                      in  DiffNodeArr (isCleanDTArr childDT) (isSelfCleanDTArr childDT) (getMove childDT) (getInsertDelete childDT) [childDT]
diffArr arr                  (TagA t arr')      = diffArr arr arr'
diffArr arr1 arr2 = let dt = diffArr' arr1 arr2
                    in  case dt of
                          -- only when child is clean we will compute a move
                          -- todo replace by selfClean
                          DiffLeafArr True _ -> DiffLeafArr True $ computeMove arr1 arr2
                          DiffNodeArr descendentsClean True _ insdel dts -> 
                                   DiffNodeArr descendentsClean True (computeMove arr1 arr2) insdel dts
                          dn -> dn 
-- move is now copied along a chain of structurals/locators/etc. just like clean attrs.
-- does this make sense?
-- maybe don't put moves everywhere, and also don't put selfclean everywhere, just copy descendentclean

-- why are edit ops duplicated on mouse up?

-- we can alleviate clean restrictions on graphs because browser paints everything
-- what about graph and edge?

computeMove newArr oldArr =
  let a1@((x1,y1),(w1,h1)) = getAreaA newArr
      a2@((x2,y2),(w2,h2)) = getAreaA oldArr
  in  if a1 == a2 then Nothing else Just a1 


-- Poly, Rectangle, Ellipse and Edge cannot be moved because width and height is encoded in the svg code
-- (actually, they could be moved, but not resized). Anyhow, because these are just leafs, a move is not much more efficient
-- than a redraw. Hence, we let them return False on a position or dimension change by including the x1 == x2 ... in the DiffLeafArr
diffArr' (EmptyA _ x y w h hr vr bc)     (EmptyA _  x' y' w' h' hr' vr' bc') = DiffLeafArr (bc == bc') Nothing
diffArr' (EmptyA _ x y w h hr vr bc)     _                       = DiffLeafArr False Nothing
diffArr' (StringA _ x y w h hr vr str lc bc f _) (StringA _ x' y' w' h' hr' vr' str' lc' bc' f' _) = 
  DiffLeafArr (str==str' && lc==lc' && bc==bc' && f==f') Nothing
diffArr' (StringA _ x y w h hr vr str lc bc f _)  _                                    = DiffLeafArr False Nothing
diffArr' (ImageA _ x y w h hr vr src style fc bc) (ImageA _ x' y' w' h' hr' vr' src' style' fc' bc') =
  DiffLeafArr (src == src' && style == style' && fc == fc' && bc == bc') Nothing
diffArr' (ImageA _  _ _ _ _ _ _ _ _ _ _)      _                 = DiffLeafArr False Nothing
diffArr' (PolyA _ x y w h hr vr pts lw style lc fc bc) (PolyA _ x' y' w' h' hr' vr' pts' lw' style' lc' fc' bc') =
  DiffLeafArr (x==x' && y==y' && w==w' && h==h' && pts == pts' && lw == lw' && style == style' && lc == lc' && fc == fc' && bc == bc') Nothing
diffArr' (PolyA _  _ _ _ _ _ _ _ _ _ _ _ _)      _                 = DiffLeafArr False Nothing
diffArr' (RectangleA _ x y w h hr vr lw style lc fc bc) (RectangleA _ x' y' w' h' hr' vr' lw' style' lc' fc' bc') =
  DiffLeafArr (x==x' && y==y' && w==w' && h==h' && lw == lw' && style == style' && lc == lc' && fc == fc' && bc == bc') Nothing
diffArr' (RectangleA _  _ _ _ _ _ _ _ _ _ _ _)      _                 = DiffLeafArr False Nothing
diffArr' (EllipseA _ x y w h hr vr lw style lc fc bc) (EllipseA _ x' y' w' h' hr' vr' lw' style' lc' fc' bc') =
  DiffLeafArr (x==x' && y==y' && w==w' && h==h' && lw == lw'  && style == style' && lc == lc' && fc == fc' && bc == bc') Nothing
diffArr' (EllipseA _  _ _ _ _ _ _ _ _ _ _ _)      _                 = DiffLeafArr False Nothing
diffArr' (EdgeA _ x1 y1 x2 y2 hr vr lw lc) (EdgeA _ x1' y1' x2' y2' hr' vr' lw' lc') =
  DiffLeafArr (x1==x1' && y1==y1' && x2==x2' && y2==y2' && lw == lw'  && lc == lc') Nothing
diffArr' (EdgeA _  _ _ _ _ _ _ _ _)      _                 = DiffLeafArr False Nothing

diffArr' (RowA _ x y w h hr vr bc arrs) (RowA _ x' y' w' h' hr' vr' bc' arrs') =  
  diffArrs x y w h bc arrs x' y' w' h' bc' arrs'
diffArr' (ColA _ x y w h hr vr bc _ arrs) (ColA _ x' y' w' h' hr' vr' bc' _ arrs') = 
  diffArrs x y w h bc arrs x' y' w' h' bc' arrs'
diffArr' (OverlayA _ x y w h hr vr bc d arrs) (OverlayA _ x' y' w' h' hr' vr' bc' d' arrs') =
  diffArrs x y w h bc arrs x' y' w' h' bc' arrs'
diffArr' (GraphA _ x y w h hr vr bc nvs arrs) (GraphA _ x' y' w' h' hr' vr' bc' nvs' arrs') =
  case diffArrs x y w h bc arrs x' y' w' h' bc' arrs' of
    DiffNodeArr childrenClean selfClean _ _ _ -> DiffLeafArr (selfClean && childrenClean) Nothing
    _ -> debug Err ("ArrUtils.diffArr: problem in difArrs") $ DiffLeafArr False  Nothing -- TODO what about this?
    -- a graph is only clean when all children and the graph itself are clean
diffArr' arr@(RowA _ x y w h hr vr bc arrs) _                            = DiffLeafArr False Nothing
diffArr' arr@(ColA _ x y w h hr vr bc _ arrs) _                          = DiffLeafArr False  Nothing
diffArr' arr@(OverlayA _ x y w h hr vr bc _ arrs) _                        = DiffLeafArr False Nothing
diffArr' arr@(GraphA _ x y w h hr vr bc nvs arrs) _                      = DiffLeafArr False Nothing
diffArr' (VertexA _ x y w h hr vr bc ol arr) (VertexA _ x' y' w' h' hr' vr' bc' ol' arr') =
 let childDT = diffArr arr arr'
 in  DiffNodeArr (isCleanDTArr childDT) (bc==bc') Nothing Nothing [childDT] -- TODO and what about this?
diffArr' (VertexA _ _ _ _ _ _ _ _ _ _)      _                 = DiffLeafArr False Nothing
diffArr' arr                           _                                = debug Err ("ArrUtils.diffArr: can't handle "++ show arr) $ DiffLeafArr False Nothing
-- At the moment, we ignore outline and nrOfVertices

-- pres is different when either self has changed or children

-- first list (new) determines size of diffTree
diffArrs x y w h bc newArrs x' y' w' h' bc' oldArrs =  
  let newNrOfArrs    = length newArrs
      oldNrOfArrs   = length oldArrs
      childDiffs  = zipWith diffArr newArrs oldArrs
      reverseChildDiffs = zipWith diffArr (reverse newArrs) (reverse oldArrs)
      firstSelfDirtyChildIx = length $ takeWhile isSelfCleanDTArr childDiffs
      leftChildDiffs = take firstSelfDirtyChildIx childDiffs
      rightChildDiffs = reverse $ take (newNrOfArrs - firstSelfDirtyChildIx) reverseChildDiffs
      childDiffs' = leftChildDiffs ++ rightChildDiffs
      selfClean   = bc==bc' 
      insertDelete = if newNrOfArrs < oldNrOfArrs then Just 
                       $ DeleteChildrenRen firstSelfDirtyChildIx 
                                           (oldNrOfArrs - newNrOfArrs) 
                     else Nothing
  in  if length childDiffs' /= newNrOfArrs then error "problem!!!!!!!!!!!!" else
      debug Arr ("diffArrs:"++show(x,x',y,y',w,w',h,h',bc,bc',newNrOfArrs,oldNrOfArrs)) $
      DiffNodeArr ( selfClean && all isCleanDTArr childDiffs') selfClean Nothing insertDelete
               (if not selfClean
                then replicate (length newArrs) (DiffLeafArr False Nothing)  -- is self is dirty, all below need to be rerendered
                else childDiffs')

-- | Returns a list of all areas that are dirty according to the diffTree
-- not used in Proxima 2.0, browser takes care of this
updatedRectArr :: Show node => DiffTreeArr -> Arrangement node -> [((Int, Int), (Int, Int))]
updatedRectArr dt arr = updatedRectArr' 0 0 dt arr 

-- check this for new difftree with moves and insert/delete
updatedRectArr' :: Show node => Int -> Int -> DiffTreeArr -> Arrangement node -> [((Int, Int), (Int, Int))]
updatedRectArr' x' y' dt arr = 
  case dt of
    DiffLeafArr True _        -> []                --
    DiffNodeArr True  _ _  _  _   -> []  -- moves and insdel are currently ignored
    DiffLeafArr False _         -> let x = x' + xA arr
                                       y = y' + yA arr
                                   in [((x, y), (widthA arr, heightA arr))]
    DiffNodeArr False False _ _ dts -> let x = x' + xA arr -- moves and insdel are currently ignored
                                           y = y' + yA arr
                                     in  [((x, y), (widthA arr, heightA arr))]
    DiffNodeArr False True _ _ dts    -> -- moves and insdel are currently ignored
                                       -- self is clean, so take union of rectangles of children
      case arr of                      -- NOTE for overlay and graph, this should not occur
      (StructuralA _ arr)           -> if not (null dts) then updatedRectArr' x' y' (head' "ArrUtils.updatedRectArr'" dts) arr else problem
      (ParsingA _ arr)              -> if not (null dts) then updatedRectArr' x' y' (head' "ArrUtils.updatedRectArr'" dts) arr else problem
      (LocatorA _ arr)              -> if not (null dts) then updatedRectArr' x' y' (head' "ArrUtils.updatedRectArr'" dts) arr else problem
      (TagA _ arr)              -> if not (null dts) then updatedRectArr' x' y' (head' "ArrUtils.updatedRectArr'" dts) arr else problem
      (RowA id x y w h hr vr bc arrs)     -> updatedRectRow (x'+x) (y'+y) h dts arrs
      (ColA id x y w h hr vr bc f arrs)   -> updatedRectCol (x'+x) (y'+y) w dts arrs 
      (OverlayA id x y w h hr vr bc _ arrs) -> updatedRectArrs (x'+x) (y'+y) dts arrs
      (GraphA id x y w h hr vr bc nvs arrs) -> updatedRectArrs (x'+x) (y'+y) dts arrs
      (VertexA id x y w h hr vr bc ol arr) -> if not (null dts) then updatedRectArr' (x'+x) (y'+y) (head' "ArrUtils.updatedRectArr'" dts) arr else problem
      _                                    -> problem
 where updatedRectArrs x' y' dts arrs = concat $ zipWith (updatedRectArr' x' y') dts arrs
       problem = debug Err ("ArrUtils.updatedRectArr: problem with "++ shallowShowArr arr) []

-- Special handling of rows and columns:
--   for children that are self clean, just continue the recursion
--   for self-dirty children, return the entire segment of the col/row where the child
--   is positioned, in order to redraw the background if the child shrunk

-- check this for new difftree with moves and insert/delete
updatedRectRow x' y' h dts arrs = 
  concat [ if isSelfCleanDTArr dt 
           then updatedRectArr' x' y' dt arr 
           else [((x'+xA arr, y'),(widthA arr, h))] 
         | (dt, arr) <- zip dts arrs ]

updatedRectCol x' y' w dts arrs = 
  concat [ if isSelfCleanDTArr dt 
           then updatedRectArr' x' y' dt arr 
           else [((x', y'+yA arr),(w, heightA arr))] 
         | (dt, arr) <- zip dts arrs ]
                


edgeClickDistance = 4.0

-- | point returns a path to the element at (x,y) in the arrangement
point :: Show node => Int -> Int -> Arrangement node -> Maybe [Int]
point x' y' arr = fmap fst $ point' (clip 0 (widthA arr-1) x') (clip 0 (heightA arr-1) y') [] (error "point: no root locator") arr

-- Temporary point function for hacked popups
pointDoc :: Show node => Int -> Int -> Arrangement node -> Maybe node
pointDoc x' y' arr = fmap snd $ point' (clip 0 (widthA arr-1) x') (clip 0 (heightA arr-1) y') [] (error "point: no root locator") arr

-- point only recurses in children that may have the focus
-- Stretching rows lead to incorrect pointing.

-- precondition: x' y' falls inside the arrangement. (Except for GraphA and EdgeA)
point' :: Show node => Int -> Int -> [Int] -> node -> Arrangement node -> Maybe ([Int], node)
point' x' y' pth loc p@(EmptyA _ x y w h _ _ _)           = Just (pth, loc)
point' x' y' pth loc p@(StringA _ x y w h _ _ _ _ _ _ _)  = Just (pth, loc)
point' x' y' pth loc p@(ImageA _ x y w h _ _ _ _ _ _)     = Just (pth, loc)
point' x' y' pth loc p@(RectangleA _ x y w h _ _ _ __ _ _ _) = Just (pth, loc)
point' x' y' pth loc p@(EllipseA _ x y w h _ _ _ _ _ _ _)   = Just (pth, loc)
point' x' y' pth loc p@(RowA _ x y w h _ _ _ arrs)        = pointRowList 0 (x'-x) (y'-y) pth loc arrs
point' x' y' pth loc p@(ColA _ x y w h _ _ _ _ arrs)      = pointColList 0 (x'-x) (y'-y) pth loc arrs
point' x' y' pth loc p@(OverlayA _ x y w h _ _ _ _ arrs@(arr:_)) = point' (clip 0 (widthA arr-1) (x'-x)) -- TODO: why always take the first one?
                                                            (clip 0 (heightA arr-1) (y'-y)) (pth++[0]) loc arr
point' x' y' pth loc p@(StructuralA _ child)              = point' x' y' (pth++[0]) loc child
point' x' y' pth loc p@(ParsingA _ child)                 = point' x' y' (pth++[0]) loc child
point' x' y' pth _   p@(LocatorA location child)          = point' x' y' (pth++[0]) location child
point' x' y' pth loc p@(TagA _ child)                 = point' x' y' (pth++[0]) loc child
point' x' y' pth loc p@(GraphA _ x y w h _ _ _ _ arrs) =
  pointGraphList (x'-x) (y'-y) pth loc arrs
point' x' y' pth loc p@(VertexA _ x y w h _ _ _ outline arr) =
  let dragAreaPoly = map outline [0, pi/10 ..2*pi]
  in if dragAreaPoly `polyContains` (x'-x, y'-y) -- inside the outline means point is on Vertex
     then Just (pth, loc)
     else
       if (x' > x) && (x' < x+w) &&       -- otherwise, if it is inside the Vertex, we continue on its children
             (y' >= y) && (y' < y+h) 
          then point' (x'-x) (y'-y) (pth++[0]) loc arr 
          else Nothing
point' x' y' pth loc p@(EdgeA _ x1 y1 x2 y2 _ _ _ _) =
  if distanceSegmentPoint (x1,y1) (x2,y2) (x',y') < edgeClickDistance
  then Just (pth, loc)
  else Nothing
point' x' y' _ _   arr                                 = debug Err ("ArrTypes.point': unhandled arrangement: "++show x'++show y'++show arr) Nothing

-- precondition: x' y' falls inside the arrangement width
pointRowList :: Show node => Int -> Int -> Int -> [Int] -> node -> [Arrangement node] -> Maybe ([Int], node)
pointRowList i x' y' pth loc []         = debug Err "ArrTypes.pointRowList: empty Row list" $ Nothing
pointRowList i x' y' pth loc (arr:arrs) = 
  --debug Arr ("pointRowList on "++show i++":"++shallowShowArr arr) $
  if x' >= xA arr + widthA arr 
  then pointRowList (i+1) x' y' pth loc arrs
  else point' x' (clip 0 (heightA arr-1) y') 
                 (pth++[i]) loc arr

pointColList :: Show node => Int -> Int -> Int -> [Int] -> node -> [Arrangement node] -> Maybe ([Int], node)
pointColList i x' y' pth loc [] = debug Err "ArrTypes.pointRowList: empty Row list" $ Nothing
pointColList i x' y' pth loc (arr:arrs) =
  --debug Arr ("pointColList on "++show i++":"++shallowShowArr arr) $
  if y' >= yA arr + heightA arr 
  then pointColList (i+1) x' y' pth loc arrs
  else point' (clip 0 (widthA arr-1) x') y' 
              (pth++[i]) loc arr

-- Graphs let the pointing be handled by child arrangements. This is safe, because they must be
-- VertexA's or EdgeA's
pointGraphList :: Show node => Int -> Int -> [Int] -> node -> [Arrangement node] -> Maybe ([Int], node)
pointGraphList x' y' pth loc arrs =
  case catMaybes [ point' x' y' (pth++[i]) loc arr | (i,arr) <- zip [0..] arrs ] of
    []      -> Just (pth, loc) -- not focused on a child, so the focus is on the graph itself
    ((pth', loc'):_) -> Just (pth', loc')
                                          

-- returns a list of all nodes on the path together with their path, starting with the root
getPathNodesPathsA = getPathNodesPathsA' []    

getPathNodesPathsA' :: Show node => Path -> Path -> Arrangement node -> [(Arrangement node, Path)]
getPathNodesPathsA' rootPath []       arr                                = [(arr,rootPath)]
getPathNodesPathsA' rootPath (p:path) arr@(RowA _ _ _ _ _ _ _ _ arrs)           = (arr,rootPath):getPathNodesPathsA' (rootPath++[p]) path (index "ArrUtils.getPathNodesPathsA'.1" arrs p)
getPathNodesPathsA' rootPath (p:path) arr@(ColA _ _ _ _ _ _ _ _ _ arrs)         = (arr,rootPath):getPathNodesPathsA' (rootPath++[p]) path (index "ArrUtils.getPathNodesPathsA'.2" arrs p)
getPathNodesPathsA' rootPath (p:path) arr@(OverlayA _ _ _ _ _ _ _ _ _ arrs)       = (arr,rootPath):getPathNodesPathsA' (rootPath++[p]) path (index "ArrUtils.getPathNodesPathsA'.3" arrs p)
getPathNodesPathsA' rootPath (p:path) arr@(GraphA _ _ _ _ _ _ _ _ _ arrs)           = (arr,rootPath):getPathNodesPathsA' (rootPath++[p]) path (index "ArrUtils.getPathNodesPathsA'.4" arrs p)
getPathNodesPathsA' rootPath (0:path) arr@(VertexA _ _ _ _ _ _ _ _ _ child)     = (arr,rootPath):getPathNodesPathsA' (rootPath++[0]) path child
getPathNodesPathsA' rootPath (p:path) arr@(StructuralA _ child)             = (arr,rootPath):getPathNodesPathsA' (rootPath++[p]) path child
getPathNodesPathsA' rootPath (p:path) arr@(ParsingA _ child)                = (arr,rootPath):getPathNodesPathsA' (rootPath++[p]) path child
getPathNodesPathsA' rootPath (p:path) arr@(LocatorA _ child)                = (arr,rootPath):getPathNodesPathsA' (rootPath++[p]) path child
getPathNodesPathsA' rootPath (p:path) arr@(TagA _ child)                = (arr,rootPath):getPathNodesPathsA' (rootPath++[p]) path child
getPathNodesPathsA' rootPath (p:path) arr                               = debug Err ("ArrTypes.getPathNodesPathsA': unhandled non-empty path: "++show (p:path)++show arr) []

getPathNodesCoordsPathsA = getPathNodesCoordsPathsA' 0 0 []
getPathNodesCoordsPathsA' :: Show node => Int -> Int -> Path -> Path -> Arrangement node -> [(Arrangement node, Int, Int, Path)]
getPathNodesCoordsPathsA' x' y' rootPath []       arr                                = [(arr,x',y',rootPath)]
getPathNodesCoordsPathsA' x' y' rootPath (p:path) arr@(RowA _ x y _ _ _ _ _ arrs)           = (arr,x',y',rootPath):getPathNodesCoordsPathsA' (x'+x) (y'+y) (rootPath++[p]) path (index "ArrUtils.getPathNodesCoordsPathsA'.1" arrs p)
getPathNodesCoordsPathsA' x' y' rootPath (p:path) arr@(ColA _ x y _ _ _ _ _ _ arrs)         = (arr,x',y',rootPath):getPathNodesCoordsPathsA' (x'+x) (y'+y) (rootPath++[p]) path (index "ArrUtils.getPathNodesCoordsPathsA'.2" arrs p)
getPathNodesCoordsPathsA' x' y' rootPath (p:path) arr@(OverlayA _ x y _ _ _ _ _ _ arrs)       = (arr,x',y',rootPath):getPathNodesCoordsPathsA' (x'+x) (y'+y) (rootPath++[p]) path (index "ArrUtils.getPathNodesCoordsPathsA'.3" arrs p)
getPathNodesCoordsPathsA' x' y' rootPath (p:path) arr@(GraphA _ x y _ _ _ _ _ _ arrs)           = (arr,x',y',rootPath):getPathNodesCoordsPathsA' (x'+x) (y'+y) (rootPath++[p]) path (index "ArrUtils.getPathNodesCoordsPathsA'.4" arrs p)
getPathNodesCoordsPathsA' x' y' rootPath (0:path) arr@(VertexA _ x y _ _ _ _ _ _ child)     = (arr,x',y',rootPath):getPathNodesCoordsPathsA' (x'+x) (y'+y) (rootPath++[0]) path child
getPathNodesCoordsPathsA' x' y' rootPath (p:path) arr@(StructuralA _ child)             = (arr,x',y',rootPath):getPathNodesCoordsPathsA' x' y' (rootPath++[p]) path child
getPathNodesCoordsPathsA' x' y' rootPath (p:path) arr@(ParsingA _ child)                = (arr,x',y',rootPath):getPathNodesCoordsPathsA' x' y' (rootPath++[p]) path child
getPathNodesCoordsPathsA' x' y' rootPath (p:path) arr@(LocatorA _ child)                = (arr,x',y',rootPath):getPathNodesCoordsPathsA' x' y' (rootPath++[p]) path child
getPathNodesCoordsPathsA' x' y' rootPath (p:path) arr@(TagA _ child)                = (arr,x',y',rootPath):getPathNodesCoordsPathsA' x' y' (rootPath++[p]) path child
getPathNodesCoordsPathsA' x' y' rootPath (p:path) arr                               = debug Err ("ArrTypes.getPathNodesCoordsPathsA': unhandled non-empty path: "++show (p:path)++show arr) []


 
-- Returns the subtree rooted at path, together with the coordinates of its upper left corner                                                        
selectTreeA :: Show node => [Int] -> Arrangement node -> (Int, Int, Arrangement node)
selectTreeA path arr = selectTreeA' 0 0 path arr

selectTreeA' x' y' []       arr                                = (x', y', arr)
selectTreeA' x' y' (p:path) (RowA _ x y _ _ _ _ _ arrs)           = selectTreeA' (x'+x) (y'+y) path (index "ArrUtils.selectTreeA'.1" arrs p)
selectTreeA' x' y' (p:path) (ColA _ x y _ _ _ _ _ _ arrs)         = selectTreeA' (x'+x) (y'+y) path (index "ArrUtils.selectTreeA'.2" arrs p)
selectTreeA' x' y' (p:path) (OverlayA _ x y _ _ _ _ _ _ arrs)     = selectTreeA' (x'+x) (y'+y) path (index "ArrUtils.selectTreeA'.3" arrs p)
selectTreeA' x' y' (p:path) (GraphA _ x y _ _ _ _ _ _ arrs)           = selectTreeA' (x'+x) (y'+y) path (index "ArrUtils.selectTreeA'.4" arrs p)
selectTreeA' x' y' (0:path) (VertexA _ x y _ _ _ _ _ _ arr)     = selectTreeA' (x'+x) (y'+y) path arr
selectTreeA' x' y' (p:path) (StructuralA _ child)             = selectTreeA' x' y' path child
selectTreeA' x' y' (p:path) (ParsingA _ child)                = selectTreeA' x' y' path child
selectTreeA' x' y' (p:path) (LocatorA _ child)                = selectTreeA' x' y' path child
selectTreeA' x' y' (p:path) (TagA _ child)                = selectTreeA' x' y' path child
selectTreeA' x' y' (p:path) arr                               = debug Err ("ArrTypes.selectTreeA: unhandled non-empty path: "++show (p:path)++show arr) (x', y', arr)


{-
point can give a negative focus. happens when clicking in non string location in debugged arrangement 
-}

-- Focus stuff

focusAFromXY x y arr  = let path = navigateFocus x y arr
                        in  FocusA path path

navigateFocus x y arr = case point x y arr of
                                       Nothing -> (PathA [] 0) 
                                       Just p  -> (PathA p (getOffset p x arr))
 where getOffset ps mx a =
         case selectTreeA ps a of 
           (x',y', StringA _ x y w h _ _ _ _ _ _ cxs) ->
             let pos = mx - (clip 0 x x) - x'        -- TODO: can x even be negative here? old commment was "in case x is negative (point' takes care of clipping itself)"
             in  (length (takeWhile (<=pos) (centerXCoords cxs)))-1
           _                                                            -> 0

-- for pointing after a character when to the right of its center
centerXCoords []      = [] -- this never occurs
centerXCoords xcoords = let widths = zipWith (-) (tail xcoords) xcoords
                            halfwidths = map (`div` 2) widths
                        in  head' "ArrUtils.centerXCoords" xcoords : zipWith (+) xcoords halfwidths
-- don't want cumulative character widths


tryFocus :: Show node => (FocusArr -> Arrangement node -> Maybe FocusArr) -> Direction -> IORef Rectangle -> FocusArr -> Arrangement node -> 
            EditArrangement doc enr node clip token -> IO [EditArrangement doc enr node clip token]
tryFocus computeFocus dir viewedAreaRef focus arr editop = 
  do { let mFocus = computeFocus focus arr
     ; seq mFocus $ return ()
     ; return $ case mFocus of
         Just f  -> [ SetFocusArr f ] 
         Nothing -> [ SkipArr 0 ]
     } `Control.Exception.catch` \UnarrangedException -> 
         do { ((x,y),(w,h)) <- readIORef viewedAreaRef
            ; return $ if (dir == Up && y > 0) ||
                          (dir == Down && y+h < heightA arr) then 
                         [ castArr $ ScrollViewedAreaArr dir, editop ]
                       else [ SkipArr 0 ]
            }


setFocus x y arr     = showDebug' GI "focus set to " $
                       let f = navigateFocus x y arr in {- showDebug Arr -} (FocusA f f)

enlargeFocusXY focus x y arr = enlargeFocus focus (navigateFocus x y arr)

enlargeFocusUp focus arr = case upPath (toA focus) arr of
                             Nothing  -> Nothing
                             Just pth -> Just $ enlargeFocus focus pth 

enlargeFocusDown focus arr = case downPath (toA focus) arr of
                               Nothing  -> Nothing
                               Just pth -> Just $ enlargeFocus focus pth 

enlargeFocus (FocusA f@(PathA _ _) t) pth = {- showDebug Arr $ -} (FocusA f pth)
enlargeFocus f                        pth = debug Err "ArrUtils.enlargeFocus: selection without focus set" $ (FocusA pth pth)



upFocus (FocusA f  t) arr = case upPath f arr of
                              Just pth' -> Just $ FocusA pth' pth'
                              Nothing   -> Nothing
upFocus _             _   = Nothing

upPath (PathA pth i) arr = let (x,y,w,h) = {- showDebug Arr $ -} sizeA (pth++[i]) arr
                           in  navigateFocusUp (x+w `div` 2) pth arr -- use the horizontal center of currently focused item
upPath _             _   = Nothing


downFocus (FocusA f  t) arr = case downPath f arr of
                                Just pth' -> Just $ FocusA pth' pth'
                                Nothing   -> Nothing
downFocus _             _   = Nothing

downPath (PathA pth i) arr = let (x,y,w,h) = {- showDebug Ren $ -} sizeA (pth++[i]) arr
                             in  navigateFocusDown (x+w `div` 2) pth arr -- use the horizontal center of currently focused item
downPath _             _   = Nothing


navigateFocusUp x pth arr = 
  let arrsAndPaths = getArrsAndPathsUp pth arr 
  in  -- debug Arr ("ArrsandPaths "++concat [ shallowShowArr a ++ show (x,y) ++ show p ++"\n" |(p,x,y,a) <- arrsAndPaths]) $
      firstJust $ map (navigateFocusXFromBottom x) arrsAndPaths
      
      
navigateFocusDown x pth arr = 
  let arrsAndPaths = getArrsAndPathsDown pth arr 
  in  -- debug Arr ("ArrsandPaths "++concat [ shallowShowArr a ++ show (x,y) ++ show p ++"\n" |(p,x,y,a) <- arrsAndPaths]) $
      firstJust $ map (navigateFocusXFromTop x) arrsAndPaths

{-
TODO explain algorithm.
TODO overlay focus

arrsAndPaths = [row,  col,  elt]

path = [2,3]

col [ bla
    , row [ bla, col [ bla
                       bla


-}
getArrsAndPathsUp pth arr =  
  let pathNodesCoordsAndPaths = getPathNodesCoordsPathsA pth arr
      pathNodesCoordsPathsAndChildIndex = zip (init pathNodesCoordsAndPaths) pth -- this gives us the index in each arrangement on the path
  in  -- debug Arr ("PathNodesCoordsAndPathsIndex "++concat [ shallowShowArr a ++ show (x,y) ++ show p ++ " "++show i++"\n" |((a,x,y,p),i) <- pathNodesCoordsPathsAndChildIndex]) $
      reverse $ concatMap getUpperColumnChildren pathNodesCoordsPathsAndChildIndex
 where getUpperColumnChildren ((ColA _ x y w h hr vr c1 f arrs,x',y',pth),i) = take i 
                                                                             [ (pth++[j],x'+x,y'+y, arr) | (arr,j) <- zip arrs [0..] ]
       getUpperColumnChildren _ = []

getArrsAndPathsDown pth arr =  
  let pathNodesCoordsAndPaths = getPathNodesCoordsPathsA pth arr
      pathNodesCoordsPathsAndChildIndex = zip (init pathNodesCoordsAndPaths) pth -- this gives us the index in each arrangement on the path
  in  -- debug Arr ("PathNodesCoordsAndPathsIndex "++concat [ shallowShowArr a ++ show (x,y) ++ show p ++ " "++show i++"\n" |((a,x,y,p),i) <- pathNodesCoordsPathsAndChildIndex]) $
      concat $ reverse $ map getLowerColumnChildren pathNodesCoordsPathsAndChildIndex
 where getLowerColumnChildren ((ColA _ x y w h hr vr c1 f arrs,x',y',pth),i) = drop (i+1) 
                                                                             [ (pth++[j],x'+x,y'+y, arr) | (arr,j) <- zip arrs [0..] ]
       getLowerColumnChildren _ = []

navigateFocusXFromBottom :: Show node => Int -> (Path, Int, Int, Arrangement node) -> Maybe PathArr
navigateFocusXFromBottom fx (rootPath,x',y', EmptyA _ x y w h hr vr c) = Nothing
navigateFocusXFromBottom fx (rootPath,x',y', StringA _ x y w h hr vr str c bc f cxs) =
  let pos = fx - x - x'         
  in  Just $ PathA rootPath $ (length (takeWhile (<=pos) (centerXCoords cxs)))-1
navigateFocusXFromBottom fx (rootPath,x',y', ImageA _ x y w h hr vr _ _ c1 c2) = Nothing
navigateFocusXFromBottom fx (rootPath,x',y', PolyA (IDA (-10)) x y w h hr vr _ _ _ c1 c2 c3) = throw UnarrangedException   
navigateFocusXFromBottom fx (rootPath,x',y', PolyA _ x y w h hr vr _ _ _ c1 c2 c3) = Nothing   
navigateFocusXFromBottom fx (rootPath,x',y', RectangleA _ x y w h hr vr _ _ c1 c2 c3) = Nothing
navigateFocusXFromBottom fx (rootPath,x',y', EllipseA _ x y w h hr vr _ _ c1 c2 c3) = Nothing
navigateFocusXFromBottom fx (rootPath,x',y', OverlayA _ x y w h hr vr c1 _ (arr:_)) = navigateFocusXFromBottom fx (rootPath++[0],x'+x,y'+y, arr)
navigateFocusXFromBottom fx (rootPath,x',y', GraphA _ x y w h hr vr c1 nvs arrs) = Nothing  
navigateFocusXFromBottom fx (rootPath,x',y', VertexA _ x y w h hr vr c1 ol arr)  = Nothing
navigateFocusXFromBottom fx (rootPath,x',y', EdgeA _ x y _ _ hr vr _ c1)       = Nothing     
navigateFocusXFromBottom fx (rootPath,x',y', StructuralA _ arr) = navigateFocusXFromBottom fx (rootPath++[0],x',y', arr)
navigateFocusXFromBottom fx (rootPath,x',y', ParsingA _ arr)    = navigateFocusXFromBottom fx (rootPath++[0],x',y', arr)
navigateFocusXFromBottom fx (rootPath,x',y', LocatorA _ arr)    = navigateFocusXFromBottom fx (rootPath++[0],x',y', arr)
navigateFocusXFromBottom fx (rootPath,x',y', TagA _ arr)        = navigateFocusXFromBottom fx (rootPath++[0],x',y', arr)
navigateFocusXFromBottom fx (rootPath,x',y', ColA _ x y w h hr vr c1 f arrs) = 
  firstJust [ navigateFocusXFromBottom fx (rootPath ++ [i], x'+x, y'+y, arr)
            | (i,arr) <- reverse $ zip [0..] arrs
            ]
navigateFocusXFromBottom fx (rootPath,x',y', RowA _ x y w h hr vr c1 arrs) = 
  let arrsWithPaths = [ (rootPath ++ [i], x'+x, y'+y, arr) | (i,arr) <- zip [0..] arrs ]
      (left,right) = break (\(_,_,_,arr) -> x' + x + xA arr > fx ) arrsWithPaths -- if fx lies inside a child, this child will be included in left
  in  firstJust $ map (navigateFocusXFromBottom fx) $ reverse left ++ right
-- first we try to find the focus to the left, if not there we go right. An improvement would be to try both and take the closest. (



navigateFocusXFromTop :: Show node => Int -> (Path, Int, Int, Arrangement node) -> Maybe PathArr
navigateFocusXFromTop fx (rootPath,x',y', EmptyA _ x y w h hr vr c) = Nothing
navigateFocusXFromTop fx (rootPath,x',y', StringA _ x y w h hr vr str c bc f cxs) =
  let pos = fx - x - x'         
  in  Just $ PathA rootPath $ (length (takeWhile (<=pos) (centerXCoords cxs)))-1
navigateFocusXFromTop fx (rootPath,x',y', ImageA _ x y w h hr vr _ _ c1 c2) = Nothing
navigateFocusXFromTop fx (rootPath,x',y', PolyA (IDA (-10)) x y w h hr vr _ _ _ c1 c2 c3) = throw UnarrangedException   
navigateFocusXFromTop fx (rootPath,x',y', PolyA _ x y w h hr vr _ _ _ c1 c2 c3) = Nothing   
navigateFocusXFromTop fx (rootPath,x',y', RectangleA _ x y w h hr vr _ _ c1 c2 c3) = Nothing
navigateFocusXFromTop fx (rootPath,x',y', EllipseA _ x y w h hr vr _ _ c1 c2 c3) = Nothing
navigateFocusXFromTop fx (rootPath,x',y', OverlayA _ x y w h hr vr c1 _ (arr:_)) = navigateFocusXFromBottom fx (rootPath++[0],x'+x,y'+y, arr)
navigateFocusXFromTop fx (rootPath,x',y', GraphA _ x y w h hr vr c1 nvs arrs) = Nothing  
navigateFocusXFromTop fx (rootPath,x',y', VertexA _ x y w h hr vr c1 ol arr)  = Nothing
navigateFocusXFromTop fx (rootPath,x',y', EdgeA _ x y _ _ hr vr _ c1)       = Nothing     
navigateFocusXFromTop fx (rootPath,x',y', StructuralA _ arr) = navigateFocusXFromTop fx (rootPath++[0],x',y', arr)
navigateFocusXFromTop fx (rootPath,x',y', ParsingA _ arr)    = navigateFocusXFromTop fx (rootPath++[0],x',y', arr)
navigateFocusXFromTop fx (rootPath,x',y', LocatorA _ arr)    = navigateFocusXFromTop fx (rootPath++[0],x',y', arr)
navigateFocusXFromTop fx (rootPath,x',y', TagA _ arr)        = navigateFocusXFromTop fx (rootPath++[0],x',y', arr)
navigateFocusXFromTop fx (rootPath,x',y', ColA _ x y w h hr vr c1 f arrs) = 
  firstJust [ navigateFocusXFromTop fx (rootPath ++ [i], x'+x, y'+y, arr)
            | (i,arr) <- zip [0..] arrs
            ]
navigateFocusXFromTop fx (rootPath,x',y', RowA _ x y w h hr vr c1 arrs) = 
  let arrsWithPaths = [ (rootPath ++ [i], x'+x, y'+y, arr) | (i,arr) <- zip [0..] arrs ]
      (left,right) = break (\(_,_,_,arr) -> x' + x + xA arr > fx ) arrsWithPaths -- if fx lies inside a child, this child will be included in left
  in  firstJust $ map (navigateFocusXFromTop fx) $ reverse left ++ right


{-
EmptyA _ x y w h hr vr c)               
StringA _ x y w h hr vr str c bc f _)   
ImageA _ x y w h hr vr _ _ c1 c2)       
PolyA _ x y w h hr vr _ _ _ c1 c2 c3)   
RectangleA _ x y w h hr vr _ _ c1 c2 c3)
EllipseA _ x y w h hr vr _ _ c1 c2 c3)  
RowA _ x y w h hr vr c1 arrs)           
ColA _ x y w h hr vr c1 f arrs)         
OverlayA _ x y w h hr vr c1 _ arrs)     
GraphA _ x y w h hr vr c1 nvs arrs)     
VertexA _ x y w h hr vr c1 ol arr)      
EdgeA _ x y x' y' hr vr _ c1)           
StructuralA _ arr)                      
ParsingA _ arr)                         
LocatorA _ arr)                         
TagA _ arr)                             

-}




-- Debugging

pd = 16 -- padding for debug boxes
hpd = pd `div` 2

-- for debug, we do pass through all children of the overlay, otherwise they have the wrong coordinates


-- strings are 1 wider than image. see StringA case

debugArrangement arr = let (arr', wOffset, hOffset) = debugArrangement' 0 0 arr in arr'
-- tricky algorithm to resize the arrangement, so boxes & lines can be drawn in the rendering
-- unfortunately, ref objects in rows and columns are not taken into account because they are not present in the rendering

-- keeping child position in row and col makes the computation a lot easier
-- ideally, use overlay and add the boxes in this function as well, then everything can be done in a separate layer
-- now we also share pd and hpd with the rendering

-- what to do with ref lines?? probably ignore
debugArrangement' xOffset yOffset (EmptyA id x y w h hr vr bc) = 
  ( EmptyA id (x+xOffset) (y+yOffset) w h hr vr bc, 0,0)
debugArrangement' xOffset yOffset (StringA id x y w h hr vr str c bc f cxs) = 
  ( StringA id (x+xOffset) (y+yOffset) (w+1) h hr vr str c bc f cxs, 1, 0) -- widen with 1, so focus is inside or on box
debugArrangement' xOffset yOffset (ImageA id x y w h hr vr src style lc bc) = 
  ( ImageA id (x+xOffset) (y+yOffset) (w+pd) (h+pd) hr vr src style lc bc, pd, pd)
debugArrangement' xOffset yOffset (PolyA id x y w h hr vr pts lw style lc fc bc) = 
  ( PolyA id (x+xOffset) (y+yOffset) (w+pd) (h+pd) hr vr pts lw style lc bc fc, pd, pd)
debugArrangement' xOffset yOffset (RectangleA id x y w h hr vr lw style lc fc bc) = 
  ( RectangleA id (x+xOffset) (y+yOffset) (w+pd) (h+pd) hr vr lw style lc fc bc, pd, pd)
debugArrangement' xOffset yOffset (EllipseA id x y w h hr vr lw style lc fc bc) = 
  ( EllipseA id (x+xOffset) (y+yOffset) (w+pd) (h+pd) hr vr lw style lc fc bc, pd, pd)
debugArrangement' xOffset yOffset (RowA id x y w h hr vr c arrs)             = 
--  let (arrs', wOffsets, hOffsets) = unzip3 [ debugArrangement' (hpd+wo) hpd arr |  -- pd-1 and not 2pd-1 because child does its own left padding
--                                             (arr,wo) <- zip arrs (scanl (\n1 n2 -> n1 +(pd-1)+n2 ) 0 wOffsets) ]
  let (arrs', wOffsets, hOffsets) = unzip3 [ debugArrangement' (hpd+wo) (hpd {-+ho-}) arr |  -- pd-1 and not 2pd-1 because child does its own left padding
                                             (arr,wo,ho) <- zip3' arrs 
                                                                  (scanl (\n1 n2 -> n1 +(pd-1)+n2 ) 0 wOffsets)
                                                                  hOffsets--(map (\o -> (maximum hOffsets-o) `div`2) hOffsets)
                                           ]
      wOffset = sum wOffsets + pd + (length wOffsets - 1) * (pd -1)
      hOffset = maximum (0:hOffsets) + pd 
  in  ( RowA id (x+xOffset) (y+yOffset) (w+wOffset) (h+hOffset) hr vr c arrs'
      , wOffset ,hOffset )
debugArrangement' xOffset yOffset (ColA id x y w h hr vr c f arrs)             =
--  let (arrs', wOffsets, hOffsets) = unzip3 [ debugArrangement' hpd (hpd+ho) arr | 
--                                             (arr,ho) <- zip arrs (scanl (\n1 n2 -> n1+(2*pd-1)+n2 ) 0 hOffsets) ]
  let (arrs', wOffsets, hOffsets) = unzip3 [ debugArrangement' (hpd {-+wo-}) (hpd+ho) arr |  -- pd-1 and not 2pd-1 because child does its own left padding
                                             (arr,wo,ho) <- zip3' arrs 
                                                                  wOffsets --(map (\o -> (maximum wOffsets-o) `div`2) wOffsets)
                                                                  (scanl (\n1 n2 -> n1 +(pd-1)+n2 ) 0 hOffsets)
                                           ]
      wOffset = maximum (0:wOffsets) + pd 
      hOffset = sum hOffsets + pd + (length hOffsets - 1) * (pd -1)
  in  ( ColA id (x+xOffset) (y+yOffset) (w+wOffset) (h+hOffset) hr vr c f arrs'
      , wOffset ,hOffset )
debugArrangement' xOffset yOffset (OverlayA id x y w h hr vr c d arrs)              =
  let (arrs', wOffsets, hOffsets) = unzip3 [ debugArrangement' (hpd {-+wo-}) (hpd {-+ho-}) arr | 
                                             (arr,wo,ho) <- zip3' arrs 
                                                                  wOffsets --(map (\o -> (maximum wOffsets-o) `div`2) wOffsets)
                                                                  hOffsets -- (map (\o -> (maximum hOffsets-o) `div`2) hOffsets)
                                           ]
      wOffset = maximum (0:wOffsets) + pd 
      hOffset = maximum (0:hOffsets) + pd 
  in  ( OverlayA id (x+xOffset) (y+yOffset) (w+wOffset) (h+hOffset) hr vr c d arrs'
      , wOffset ,hOffset )
debugArrangement' xOffset yOffset (GraphA id x y w h hr vr c nvs arrs)              =
  let (arrs', wOffsets, hOffsets) = unzip3 [ debugArrangement' (hpd {-+wo-}) (hpd {-+ho-}) arr | 
                                             (arr,wo,ho) <- zip3' arrs 
                                                                  wOffsets --(map (\o -> (maximum wOffsets-o) `div`2) wOffsets)
                                                                  hOffsets -- (map (\o -> (maximum hOffsets-o) `div`2) hOffsets)
                                           ]
      wOffset = maximum (0:wOffsets) + pd 
      hOffset = maximum (0:hOffsets) + pd 
  in  ( GraphA id (x+xOffset) (y+yOffset) (w+wOffset) (h+hOffset) hr vr c nvs arrs'
      , wOffset ,hOffset )
debugArrangement' xOffset yOffset (VertexA id x y w h hr vr c ol arr)     =
  let (arr', wOffset, hOffset) = debugArrangement' (xOffset) (yOffset) arr
  in  (VertexA id (x+xOffset) (y+yOffset) (w+wOffset+pd) (h+hOffset+pd) hr vr c ol arr', wOffset+pd, hOffset+pd)
debugArrangement' xOffset yOffset (EdgeA id x y x' y' hr vr lw lc) = 
  ( EdgeA id (x+xOffset) (y+yOffset) (x'+xOffset) (y'+yOffset) hr vr lw lc, pd, pd)
debugArrangement' xOffset yOffset (LocatorA location arr)              =
  let (arr', wOffset, hOffset) = debugArrangement' (xOffset) (yOffset) arr
  in  (LocatorA location arr', wOffset, hOffset)
debugArrangement' xOffset yOffset (TagA t arr)              =
  let (arr', wOffset, hOffset) = debugArrangement' (xOffset) (yOffset) arr
  in  (TagA t arr', wOffset, hOffset)
debugArrangement' xOffset yOffset (StructuralA id arr)              =
  let (arr', wOffset, hOffset) = debugArrangement' (xOffset) (yOffset) arr
  in  (StructuralA id arr', wOffset, hOffset)
debugArrangement' xOffset yOffset (ParsingA id arr)              =
  let (arr', wOffset, hOffset) = debugArrangement' (xOffset) (yOffset) arr
  in  (ParsingA id arr', wOffset, hOffset)
debugArrangement' _ _ arr = debug Err ("Renderer.debugArrangement': unimplemented arrangement: "++show arr) (EmptyA NoIDA 0 0 0 0 0 0 transparent,0,0)

-- we need to use a more lazy zip3, which only works when bs and cs are longer than as
-- positions in row&col solve this. Maybe this is not necessary anymore without aligning
zip3'             :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3'              = zipWith3' (\a b c -> (a,b,c))

zipWith3'                 :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3' z (a:as) ~(b:bs) ~(c:cs)
	  		   = z a b c : zipWith3' z as bs cs
zipWith3' _ _ _ _          = []



-- to get this view completely right, ref objects have to be taken into account.
-- anyhow, stretching objects will not adapt their size. So a more advanced debug view would be in the presentation

-- aligning the elts of a row so the left sides of the children match will require returning a direct wOffset apart
-- from the sum of total wOffset, which is needed for resizing the row box. This is not worth the trouble.
-- as a result, squigglies under strings are rendered to the right because an image has a padded box and a string hasn't




-- leftDocPathsA returns a list of document paths whose locator can be reached by goining to the left while
-- only encountering whitespace     <PlusExp>  <IntExp>  | 12</IntExp> <Exp> </PlusExp>
-- for |, the paths would include [IntExp, PlusExp] ++ ...
{-
docFocusArr :: FocusArr -> Arrangement node -> FocusDoc
docFocusArr NoFocusA = NoFocusD
docFocusArr 

leftDocPathsA :: PathArr -> Arrangement node -> [PathDoc]
leftDocPathsA 
-}

           
-- some code from Dazzle's Math.hs
data DoublePoint = DoublePoint
    { doublePointX :: !Double
    , doublePointY :: !Double
    }
    deriving (Show, Eq, Read)

data Vector = Vector !Double !Double

square :: Double -> Double
square d = d*d

-- | Compute distance from a segment (as opposed to a line) to a point
--   Formulas taken from
--   <http://geometryalgorithms.com/Archive/algorithm_0102/algorithm_0102.htm>
distanceSegmentPoint :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Double
distanceSegmentPoint (x0,y0) (x1,y1) (x,y) =
    let p0 = DoublePoint (fromIntegral x0) (fromIntegral y0)
        p1 = DoublePoint (fromIntegral x1) (fromIntegral y1)
        p  = DoublePoint (fromIntegral x)  (fromIntegral y)
        v  = p1 `subtractDoublePointVector` p0
        w  = p  `subtractDoublePointVector` p0
        c1 = dotProduct w v
        c2 = dotProduct v v
    in if c1 <= 0 then distancePointPoint p p0
       else if c2 <= c1 then distancePointPoint p p1
       else distanceLinePoint p0 p1 p

-- | Compute distance between two points
distancePointPoint :: DoublePoint -> DoublePoint -> Double
distancePointPoint (DoublePoint x0 y0) (DoublePoint x1 y1) =
    sqrt (square (x0 - x1)  + square (y0 - y1))

-- | Compute distance from a line to a point
distanceLinePoint :: DoublePoint -> DoublePoint -> DoublePoint -> Double
distanceLinePoint (DoublePoint x0 y0) (DoublePoint x1 y1) (DoublePoint x y) =
    abs ( ( (y0 - y1) * x + (x1 - x0) * y + (x0 * y1 - x1 * y0) ) /
          sqrt (square (x1 - x0) + square (y1 - y0))
        )

subtractDoublePointVector :: DoublePoint -> DoublePoint -> Vector
subtractDoublePointVector (DoublePoint x0 y0) (DoublePoint x1 y1) =
    Vector (x0 - x1) (y0 - y1)

dotProduct :: Vector -> Vector -> Double
dotProduct (Vector v1 v2) (Vector w1 w2) = v1 * w1 + v2 * w2

edges vs = (head' "ArrUtils.edges" vs, last vs) : edges' vs
 where edges' (e1:rest@(e2:_)) = (e1,e2) : edges' rest
       edges' _                = []
       
polyContains :: (Num a, Ord a) => [(a,a)] -> (a, a) -> Bool
polyContains poly p           = eq>0 || odd pos || odd neg
    where (pos, eq, neg)      = countCrossings (edges qs) 0 0 0 
--          qs                  = map ((-)p) (last ps : init ps)
          qs                  = map (\(x,y) -> (x-fst p, y - snd p)) (last ps : init ps)
          ps		          = poly

countCrossings :: (Ord a, Num a) => [((a,a),(a, a))] -> Int -> Int -> Int -> (Int, Int, Int)  
countCrossings [] cp ce cn    = (cp, ce, cn)
countCrossings ((p@(x',y'), q@(x,y)):ps) cp ce cn
  | straddlesXaxis            = incr
  | straddlesFromBelow || p==(0,0) || q==(0,0)
                              = countCrossings ps cp (ce+1) cn
  | otherwise                 = countCrossings ps cp ce cn
  where straddlesXaxis        = (y>0 && y'<=0) || (y'>0 && y<=0) 
        straddlesFromBelow    = (y<=0 && y'==0) && (y==0 && y'<=0) 
                                && ((x<=0 && x'>=0) || (x>=0 && x'<=0))
        incr | sgn>0          = countCrossings ps (cp+1) ce cn 
             | sgn==0         = countCrossings ps cp (ce+1) cn
             | sgn<0          = countCrossings ps cp ce (cn+1)
             where sgn        = signum (x*y' - x'*y) * signum (y' - y)
