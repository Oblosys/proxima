module ArrUtils where

import CommonTypes
import ArrTypes
import CommonUtils

import Maybe 

-- utils

ifFocusA NoFocusA           _   = NoFocusA
ifFocusA (FocusA NoPathA _) _   = NoFocusA
ifFocusA (FocusA _ NoPathA) _   = NoFocusA
ifFocusA (FocusA _ _)       exp = exp

ifPathA NoPathA _       = NoPathA
ifPathA (PathA _ _) exp = exp


orderFocusA foc@(FocusA from to) = ifFocusA foc $ FocusA (min from to) (max from to)

sizeA path arr = sizeA' 0 0 path arr
sizeA' x' y' []       arr                               = (x' + xA arr, y' + yA arr, widthA arr, heightA arr)
sizeA' x' y' [p]      (StringA _ x y w h _ _ _ _ _ _ cxs)   = (x' + x + (index "ArrUtils.sizeA'" cxs p), y'+y, (index "ArrUtils.sizeA'" cxs p+1)-(index "ArrUtils.sizeA'" cxs p+1), h)
sizeA' x' y' (p:path) (RowA _ x y w h _ _ _ arrs)           = sizeA' (x'+x) (y'+y) path (index "ArrUtils.sizeA'" arrs p)
sizeA' x' y' (p:path) (ColA _ x y w h _ _ _ _ arrs)         = sizeA' (x'+x) (y'+y) path (index "ArrUtils.sizeA'" arrs p)
sizeA' x' y' (0:path) (OverlayA _ x y w h _ _ _ (arr:arrs)) = sizeA' (x'+x) (y'+y) path arr
sizeA' x' y' (p:path) (GraphA _ x y w h _ _ _ _ (arr:arrs)) = sizeA' (x'+x) (y'+y) path (index "ArrUtils.sizeA'" arrs p)
sizeA' x' y' (0:path) (VertexA _ x y w h _ _ _ _ arr)       = sizeA' (x'+x) (y'+y) path arr
sizeA' x' y' (0:path) (StructuralA _ arr)               = sizeA' x' y' path arr
sizeA' x' y' (0:path) (ParsingA _ arr)                  = sizeA' x' y' path arr
sizeA' x' y' (0:path) (LocatorA location arr)           = sizeA' x' y' path arr
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
walk (OverlayA _ x y w h hr vr c1 arrs)     = x+y+w+h+hr+vr + walkC c1 + walkList arrs
walk (GraphA _ x y w h hr vr c1 nvs arrs)   = x+y+w+h+hr+vr + walkC c1 + walkList arrs
walk (VertexA _ x y w h hr vr c1 ol arr)    = x+y+w+h+hr+vr + walkC c1 + walk arr
walk (EdgeA _ x y x' y' hr vr _ c1)         = x+y+x'+y'+hr+vr + walkC c1
walk (StructuralA _ arr)              = walk arr
walk (ParsingA _ arr)                 = walk arr
walk (LocatorA _ arr)                 = walk arr
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

--       new arrangement     old arrangement
diffArr (StructuralA id arr) arr'                   = let childDT = diffArr arr arr'
                                                      in  DiffNode (isCleanDT childDT) True [childDT]
diffArr arr                  (StructuralA id arr')  = diffArr arr arr'
diffArr (ParsingA id arr)    arr'                   = let childDT = diffArr arr arr'
                                                      in  DiffNode (isCleanDT childDT) True [childDT]
diffArr arr                  (ParsingA id arr')     = diffArr arr arr'
diffArr (LocatorA l arr)     arr'                   = let childDT = diffArr arr arr'
                                                      in  DiffNode (isCleanDT childDT) True [childDT]
diffArr arr                  (LocatorA l arr')      = diffArr arr arr'
-- StructuralA ParsingA and LocatorA elts

diffArr (EmptyA id x y w h hr vr bc)     (EmptyA _  x' y' w' h' hr' vr' bc') = DiffLeaf $ x==x' && y==y' && w==w' && h==h' && bc == bc'                                                         
diffArr (EmptyA id x y w h hr vr bc)     _                       = DiffLeaf False
diffArr (StringA id x y w h hr vr str lc bc f _) (StringA _ x' y' w' h' hr' vr' str' lc' bc' f' _) = 
  DiffLeaf $ x==x' && y==y' && w==w' && h==h' && str==str' && lc==lc' && bc==bc' && f==f'
diffArr (StringA id x y w h hr vr str lc bc f _)  _                                    = DiffLeaf False
diffArr (ImageA id x y w h hr vr src style fc bc) (ImageA id' x' y' w' h' hr' vr' src' style' fc' bc') =
  DiffLeaf $ x==x' && y==y' && w==w' && h==h' && src == src' && style == style' && fc == fc' && bc == bc'
diffArr (ImageA id  _ _ _ _ _ _ _ _ _ _)      _                 = DiffLeaf False
diffArr (PolyA id x y w h hr vr pts lw style lc fc bc) (PolyA id' x' y' w' h' hr' vr' pts' lw' style' lc' fc' bc') =
  DiffLeaf $ x==x' && y==y' && w==w' && h==h' && pts == pts' && lw == lw' && style == style' && lc == lc' && fc == fc' && bc == bc'
diffArr (PolyA id  _ _ _ _ _ _ _ _ _ _ _ _)      _                 = DiffLeaf False
diffArr (RectangleA id x y w h hr vr lw style lc fc bc) (RectangleA id' x' y' w' h' hr' vr' lw' style' lc' fc' bc') =
  DiffLeaf $ x==x' && y==y' && w==w' && h==h' && lw == lw' && style == style' && lc == lc' && fc == fc' && bc == bc'
diffArr (RectangleA id  _ _ _ _ _ _ _ _ _ _ _)      _                 = DiffLeaf False
diffArr (EllipseA id x y w h hr vr lw style lc fc bc) (EllipseA id' x' y' w' h' hr' vr' lw' style' lc' fc' bc') =
  DiffLeaf $ x==x' && y==y' && w==w' && h==h' && lw == lw'  && style == style' && lc == lc' && fc == fc' && bc == bc'
diffArr (EllipseA id  _ _ _ _ _ _ _ _ _ _ _)      _                 = DiffLeaf False
diffArr (EdgeA id x1 y1 x2 y2 hr vr lw lc) (EdgeA id' x1' y1' x2' y2' hr' vr' lw' lc') =
  DiffLeaf $ x1==x1' && y1==y1' && x2==x2' && y2==y2' && lw == lw'  && lc == lc'
diffArr (EdgeA id  _ _ _ _ _ _ _ _)      _                 = DiffLeaf False

diffArr (RowA id x y w h hr vr bc arrs) (RowA id' x' y' w' h' hr' vr' bc' arrs') =  
  diffArrs x y w h bc arrs x' y' w' h' bc' arrs'
diffArr (ColA id x y w h hr vr bc _ arrs) (ColA id' x' y' w' h' hr' vr' bc' _ arrs') = 
  diffArrs x y w h bc arrs x' y' w' h' bc' arrs'
diffArr (OverlayA id x y w h hr vr bc arrs) (OverlayA id' x' y' w' h' hr' vr' bc' arrs') =
  diffArrs x y w h bc arrs x' y' w' h' bc' arrs'
diffArr (GraphA id x y w h hr vr bc nvs arrs) (GraphA id' x' y' w' h' hr' vr' bc' nvs' arrs') =
  case diffArrs x y w h bc arrs x' y' w' h' bc' arrs' of
    DiffNode childrenClean selfClean _ -> DiffLeaf (selfClean && childrenClean)
    _ -> debug Err ("ArrUtils.diffArr: problem in difArrs") $ DiffLeaf False  
    -- a graph is only clean when all children and the graph itself are clean
diffArr arr@(RowA id x y w h hr vr bc arrs) _                            = DiffLeaf False 
diffArr arr@(ColA id x y w h hr vr bc _ arrs) _                          = DiffLeaf False 
diffArr arr@(OverlayA id x y w h hr vr bc arrs) _                        = DiffLeaf False 
diffArr arr@(GraphA id x y w h hr vr bc nvs arrs) _                      = DiffLeaf False 
diffArr (VertexA id x y w h hr vr bc ol arr) (VertexA id' x' y' w' h' hr' vr' bc' ol' arr') =
 let childDT = diffArr arr arr'
 in  DiffNode (isCleanDT childDT) (x==x' && y==y' && w==w' && h==h' && bc==bc') [childDT]
diffArr (VertexA id _ _ _ _ _ _ _ _ _)      _                 = DiffLeaf False
diffArr arr                           _                                = debug Err ("ArrUtils.diffArr: can't handle "++ show arr) $ DiffLeaf False
-- At the moment, we ignore outline and nrOfVertices

-- pres is different when either self has changed or children

-- first list (new) determines size of diffTree
diffArrs x y w h bc arrs x' y' w' h' bc' arrs' =  
  let nrOfArrs    = length arrs
      nrOfArrs'   = length arrs'
      childDiffs  = zipWith diffArr arrs arrs'
      childDiffs' = take nrOfArrs $ childDiffs ++ repeat (DiffLeaf False)
      selfClean   =    x==x' && y==y' && w==w' && h==h' && bc==bc' 
                    && nrOfArrs == nrOfArrs'
  in  DiffNode ( selfClean && all isCleanDT childDiffs') selfClean
               (if not selfClean
                then replicate (length arrs) (DiffLeaf False)  -- is self is dirty, all below need to be rerendered
                else childDiffs')

-- | Returns a list of all areas that are dirty according to the diffTree
updatedRectArr :: Show node => DiffTree -> Arrangement node -> [((Int, Int), (Int, Int))]
updatedRectArr dt arr = updatedRectArr' 0 0 dt arr 

updatedRectArr' :: Show node => Int -> Int -> DiffTree -> Arrangement node -> [((Int, Int), (Int, Int))]
updatedRectArr' x' y' dt arr = 
  case dt of
    DiffLeaf True            -> []                --
    DiffNode True  _     _   -> []
    DiffLeaf False           -> let x = x' + xA arr
                                    y = y' + yA arr
                                in  [((x, y), (widthA arr, heightA arr))]
    DiffNode False False dts -> let x = x' + xA arr
                                    y = y' + yA arr
                                in  [((x, y), (widthA arr, heightA arr))]
    DiffNode False True  dts    ->     -- self is clean, so take union of rectangles of children
      case arr of                      -- NOTE for overlay and graph, this should not occur
      (StructuralA _ arr)           -> if not (null dts) then updatedRectArr' x' y' (head dts) arr else problem
      (ParsingA _ arr)              -> if not (null dts) then updatedRectArr' x' y' (head dts) arr else problem
      (LocatorA _ arr)              -> if not (null dts) then updatedRectArr' x' y' (head dts) arr else problem
      (RowA id x y w h hr vr bc arrs)     -> updatedRectRow (x'+x) (y'+y) h dts arrs
      (ColA id x y w h hr vr bc f arrs)   -> updatedRectCol (x'+x) (y'+y) w dts arrs 
      (OverlayA id x y w h hr vr bc arrs) -> updatedRectArrs (x'+x) (y'+y) dts arrs
      (GraphA id x y w h hr vr bc nvs arrs) -> updatedRectArrs (x'+x) (y'+y) dts arrs
      (VertexA id x y w h hr vr bc ol arr) -> if not (null dts) then updatedRectArr' (x'+x) (y'+y) (head dts) arr else problem
      (OverlayA id x y w h hr vr bc arrs)  -> updatedRectArrs (x'+x) (y'+y) dts arrs
      _                                    -> problem
 where updatedRectArrs x' y' dts arrs = concat $ zipWith (updatedRectArr' x' y') dts arrs
       problem = debug Err ("ArrUtils.updatedRectArr: problem with "++ shallowShowArr arr) []

-- Special handling of rows and columns:
--   for children that are self clean, just continue the recursion
--   for self-dirty children, return the entire segment of the col/row where the child
--   is positioned, in order to redraw the background if the child shrunk

updatedRectRow x' y' h dts arrs = 
  concat [ if isSelfCleanDT dt 
           then updatedRectArr' x' y' dt arr 
           else [((x'+xA arr, y'),(widthA arr, h))] 
         | (dt, arr) <- zip dts arrs ]

updatedRectCol x' y' w dts arrs = 
  concat [ if isSelfCleanDT dt 
           then updatedRectArr' x' y' dt arr 
           else [((x', y'+yA arr),(w, heightA arr))] 
         | (dt, arr) <- zip dts arrs ]
         
       

-- mark the focus path as changed in the DiffTree
-- should be done for old as well as new focus (in case of navigate without update)
markFocusDirtyArr :: Arrangement node -> FocusArr -> DiffTree -> DiffTree
markFocusDirtyArr arr (FocusA (PathA fromPth _) (PathA toPth _)) dt = markDirty arr (commonPrefix fromPth toPth) dt
markFocusDirtyArr _ arr dt = dt


-- mark all nodes on path as children dirty, when descending beyond clean leaf, add new clean nodes around dirty path
markDirty :: Arrangement node -> [Int] -> DiffTree -> DiffTree
markDirty _ [] _ = DiffLeaf False               -- entire subtree is marked dirty
markDirty _ _ (DiffLeaf False) = DiffLeaf False -- subtree already dirty
-- When focus is in a graph, the entire graph is marked dirty.
markDirty (GraphA _ _ _ _ _ _ _ _ _ _) _ dt = DiffLeaf False
markDirty arr (p:pth) (DiffLeaf True) = DiffNode False True $  -- make self clean node with one dirty child on the path
                                         replicate (p) (DiffLeaf True)
                                      ++ [markDirty (index "ArrUtils.markDirty" (getChildrenA arr) p) pth (DiffLeaf True)] -- do the same thing for the rest of the path
                                      ++ replicate (length (getChildrenA arr) - p - 1) (DiffLeaf True)
markDirty arr (p:pth) (DiffNode _ self dts) = DiffNode False self $ -- leaf self 
                                               take p dts
                                            ++ [markDirty (index "ArrUtils.markDirty" (getChildrenA arr) p) pth (dts !! p)]
                                            ++ drop (p+1) dts


edgeClickDistance = 4.0

-- | point returns a path to the element at (x,y) in the arrangement
point :: Show node => Int -> Int -> Arrangement node -> Maybe [Int]
point x' y' arr = fmap fst $ point' (clip 0 (widthA arr-1) x') (clip 0 (heightA arr-1) y') [] (error "point: no root locator") arr

-- Temporary point function for hacked popups
pointDoc :: Show node => Int -> Int -> Arrangement node -> Maybe node
pointDoc x' y' arr = fmap snd $ point' (clip 0 (widthA arr-1) x') (clip 0 (heightA arr-1) y') [] (error "point: no root locator") arr

-- point only recurses in children that may have the focus
-- Stretching rows do not lead to correct pointing.

-- precondition: x' y' falls inside the arrangement. (Except for GraphA and EdgeA)
point' :: Show node => Int -> Int -> [Int] -> node -> Arrangement node -> Maybe ([Int], node)
--point' x' y' pth loc p@(EmptyA _)                     = Nothing -- does not occur at the moment
point' x' y' pth loc p@(StringA _ x y w h _ _ _ _ _ _ _)  = Just (pth, loc)
point' x' y' pth loc p@(RectangleA _ x y w h _ _ _ __ _ _ _) = Just (pth, loc)
point' x' y' pth loc p@(EllipseA _ x y w h _ _ _ _ _ _ _)   = Just (pth, loc)
point' x' y' pth loc p@(RowA _ x y w h _ _ _ arrs)        = pointRowList 0 (x'-x) (y'-y) pth loc arrs
point' x' y' pth loc p@(ColA _ x y w h _ _ _ _ arrs)      = pointColList 0 (x'-x) (y'-y) pth loc arrs
point' x' y' pth loc p@(OverlayA _ x y w h _ _ _ arrs@(arr:_)) = point' (clip 0 (widthA arr-1) (x'-x)) -- TODO: why always take the first one?
                                                            (clip 0 (heightA arr-1) (y'-y)) (pth++[0]) loc arr
point' x' y' pth loc p@(StructuralA _ child)              = point' x' y' (pth++[0]) loc child
point' x' y' pth loc p@(ParsingA _ child)                 = point' x' y' (pth++[0]) loc child
point' x' y' pth _   p@(LocatorA location child)          = point' x' y' (pth++[0]) location child
point' x' y' pth loc p@(GraphA _ x y w h _ _ _ _ arrs) =
  pointGraphList (x'-x) (y'-y) pth loc arrs
point' x' y' pth loc p@(VertexA _ x y w h _ _ _ outline arr) =
  let dragAreaPoly = map outline [0, pi/10 ..2*pi]
  in if dragAreaPoly `polyContains` (x'-x, y'-y) -- inside the outline means point is on Vertex
     then Just (pth, loc)
     else
       if (x' > x) && (x' < x+w) &&       -- otherwise, if it is inside the Vertex, we continue on its children
             (y' >= y) && (y' < y+h) 
          then debug Arr "inside Vertex pres" $ point' (x'-x) (y'-y) (pth++[0]) loc arr 
          else Nothing
point' x' y' pth loc p@(EdgeA _ x1 y1 x2 y2 _ _ _ _) =
  if distanceSegmentPoint (x1,y1) (x2,y2) (x',y') < edgeClickDistance
  then Just (pth, loc)
  else Nothing
point' x' y' _ _   arr                                 = debug Err ("ArrTypes.point': unhandled arrangement: "++show x'++show y'++show arr) Nothing

-- precondition: x' y' falls inside the arrangement width
pointRowList :: Show node => Int -> Int -> Int -> [Int] -> node -> [Arrangement node] -> Maybe ([Int], node)
pointRowList i x' y' pth loc []         = debug Err "ArrTypes.pointRowList: empty Row list" $ Nothing
pointRowList i x' y' pth loc (arr:arrs) = if x' >= xA arr + widthA arr 
                                      then pointRowList (i+1) x' y' pth loc arrs
                                      else point' x' (clip 0 (heightA arr-1) y') 
                                                  (pth++[i]) loc arr

pointColList :: Show node => Int -> Int -> Int -> [Int] -> node -> [Arrangement node] -> Maybe ([Int], node)
pointColList i x' y' pth loc [] = debug Err "ArrTypes.pointRowList: empty Row list" $ Nothing
pointColList i x' y' pth loc (arr:arrs) = if y' >= yA arr + heightA arr 
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
                                          




 
-- Returns the subtree rooted at path, together with the coordinates of its upper left corner                                                        
selectTreeA :: Show node => [Int] -> Arrangement node -> (Int, Int, Arrangement node)
selectTreeA path arr = selectTreeA' 0 0 path arr

selectTreeA' x' y' []       tr                                = (x', y', tr)
selectTreeA' x' y' (p:path) (RowA _ x y _ _ _ _ _ arrs)           = selectTreeA' (x'+x) (y'+y) path (arrs!!p)
selectTreeA' x' y' (p:path) (ColA _ x y _ _ _ _ _ _ arrs)         = selectTreeA' (x'+x) (y'+y) path (arrs!!p)
--selectTreeA' x' y' (0:path) (OverlayA _ x y _ _ _ _ _ arrs@(arr:_)) = selectTreeA' (x'+x) (y'+y) path (last arrs)
selectTreeA' x' y' (0:path) (OverlayA _ x y _ _ _ _ _ arrs@(arr:_)) = selectTreeA' (x'+x) (y'+y) path arr
selectTreeA' x' y' (p:path) (GraphA _ x y _ _ _ _ _ _ arrs)           = selectTreeA' (x'+x) (y'+y) path (arrs!!p)
selectTreeA' x' y' (0:path) (VertexA _ x y _ _ _ _ _ _ arr)     = selectTreeA' (x'+x) (y'+y) path arr
selectTreeA' x' y' (p:path) (StructuralA _ child)             = selectTreeA' x' y' path child
selectTreeA' x' y' (p:path) (ParsingA _ child)                = selectTreeA' x' y' path child
selectTreeA' x' y' (p:path) (LocatorA _ child)                = selectTreeA' x' y' path child
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
             let pos = mx - (clip 0 x x) - x'        -- in case x is negative (point' takes care of clipping itself)
             in  (length (takeWhile (<=pos) (centerXCoords cxs)))-1
           _                                                            -> 0

-- for pointing after a character when to the right of its center
centerXCoords []      = [] -- this never occurs
centerXCoords xcoords = let widths = zipWith (-) (tail xcoords) xcoords
                            halfwidths = map (`div` 2) widths
                        in  head xcoords : zipWith (+) xcoords halfwidths
-- don't want cumulative character widths

setFocus x y arr     = showDebug' GI "focus set to " $
                       let f = navigateFocus x y arr in showDebug GI (FocusA f f)

enlargeFocusXY focus x y arr = enlargeFocus focus (navigateFocus x y arr)

enlargeFocus (FocusA f@(PathA _ _) t) pth = showDebug Ren $ (FocusA f pth)
enlargeFocus f                        pth = debug Err "GestureInterpreter.enlargeFocus: selection without focus set" $ (FocusA pth pth)

-- this works but is it a general solution? No, can't go up from:                 col
--                                                                        bla|bla col
-- probably we should get out of enclosing rows and into preceding/following elt of column

upFocus (FocusA f  t) arr = let pth' = upPath f arr in FocusA pth' pth'
upFocus _             _   = NoFocusA

upPath (PathA pth i) arr = let (x,y,w,h) = showDebug Ren $ sizeA (pth++[i]) arr
                               focused   = selectTreeA pth arr
                           in  navigateFocus x (y-2) arr
upPath _             _   = NoPathA


downFocus (FocusA f t) arr = let pth' = downPath f arr in FocusA pth' pth'
downFocus _            _   = NoFocusA

downPath (PathA pth i) arr = let (x,y,w,h) = showDebug Ren $ sizeA (pth++[i]) arr
                                 focused   = selectTreeA pth arr
                             in  navigateFocus x (y+h) arr
downPath _             _   = NoPathA






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
debugArrangement' xOffset yOffset (OverlayA id x y w h hr vr c arrs)              =
  let (arrs', wOffsets, hOffsets) = unzip3 [ debugArrangement' (hpd {-+wo-}) (hpd {-+ho-}) arr | 
                                             (arr,wo,ho) <- zip3' arrs 
                                                                  wOffsets --(map (\o -> (maximum wOffsets-o) `div`2) wOffsets)
                                                                  hOffsets -- (map (\o -> (maximum hOffsets-o) `div`2) hOffsets)
                                           ]
      wOffset = maximum (0:wOffsets) + pd 
      hOffset = maximum (0:hOffsets) + pd 
  in  ( OverlayA id (x+xOffset) (y+yOffset) (w+wOffset) (h+hOffset) hr vr c arrs'
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

-- refs are not passed, since they are not used in rendering.
-- What happens if an empty is used as oldArrangement? Can this happen? And do we need the refs then?
arrangeWhenViewed absx absy x y w h hrf vrf viewedArea idA arrangement =
  if overlap ((absx,absy),(w,h)) viewedArea then arrangement else -- EmptyA idA x y w h 0 0
    --arrangement 
    PolyA idA x y w h hrf vrf [(0,0),(w-1,0),(w-1,h-1),(0,h-1),(0,0),(w-1,h-1),(w-1,0),(0,h-1)] 1 Transparent black white transparent
           
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

edges vs = (head vs, last vs) : edges' vs
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
