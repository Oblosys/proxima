module ArrUtils where

import CommonTypes
import ArrTypes

import CommonUtils

-- utils

ifFocusA NoFocusA           _   = NoFocusA
ifFocusA (FocusA NoPathA _) _   = NoFocusA
ifFocusA (FocusA _ NoPathA) _   = NoFocusA
ifFocusA (FocusA _ _)       exp = exp

ifPathA NoPathA _       = NoPathA
ifPathA (PathA _ _) exp = exp


orderFocusA foc@(FocusA from to) = ifFocusA foc $ FocusA (min from to) (max from to)

shallowShowArr (EmptyA _ x y w h _ _)             = "{EmptyA: x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (StringA _ x y w h _ _ str _ _ _)  = "{StringA \""++str++"\": x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (ImageA _ x y w h _ _ src _ _ _)   = "{ImageA: \""++src++"\": x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (PolyA _ x y w h _ _ _ _ _ _)      = "{PolyA: x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (RectangleA _ x y w h _ _ _ _ _ _) = "{RectangleA: x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (EllipseA _ x y w h _ _ _ _ _ _)   = "{EllipseA: x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (LineA _ x y x' y' _ _ _ _)        = "{LineA: x="++show x++", y="++show y++", x'="++show x'++", y'="++show y'++"}"
shallowShowArr (RowA _ x y w h _ _ _ _)           = "{RowA: x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (ColA _ x y w h _ _ _ _)           = "{ColA: x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (OverlayA _ x y w h _ _ _ _)       = "{OverlayA: x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (GraphA _ x y w h _ _ _ _)         = "{GraphA: x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (VertexA _ x y w h _ _ _ _)        = "{VertexA: x="++show x++", y="++show y++", w="++show w++", h="++show h++"}"
shallowShowArr (StructuralA _ child)          = "{StructuralA}"
shallowShowArr (ParsingA _ child)             = "{ParsingA}"
shallowShowArr (LocatorA location child)      = "{LocatorA}"
shallowShowArr arr                            = "{Arrangement not handled by shallowShowArr: "++show arr++"}"

-- Bug? why does: shallowShowArr (StringA NoIDA 1 2 3 4 "f" undefined undefined undefined)
-- return undefined?

sizeA path arr = sizeA' 0 0 path arr
sizeA' x' y' []       arr                               = (x' + xA arr, y' + yA arr, widthA arr, heightA arr)
sizeA' x' y' [p]      (StringA _ x y w h _ _ _ _ _ cxs)     = (x' + x + (cxs!!!p), y'+y, (cxs!!!p+1)-(cxs!!!p+1), h)
sizeA' x' y' (p:path) (RowA _ x y w h _ _ _ arrs)           = sizeA' (x'+x) (y'+y) path (arrs!!!p)
sizeA' x' y' (p:path) (ColA _ x y w h _ _ _ arrs)           = sizeA' (x'+x) (y'+y) path (arrs!!!p)
sizeA' x' y' (0:path) (OverlayA _ x y w h _ _ _ (arr:arrs)) = sizeA' (x'+x) (y'+y) path arr
sizeA' x' y' (p:path) (GraphA _ x y w h _ _ _ (arr:arrs))   = sizeA' (x'+x) (y'+y) path (arrs!!!p)
sizeA' x' y' (0:path) (VertexA _ x y w h _ _ _ arr)         = sizeA' (x'+x) (y'+y) path arr
sizeA' x' y' (0:path) (StructuralA _ arr)               = sizeA' x' y' path arr
sizeA' x' y' (0:path) (ParsingA _ arr)                  = sizeA' x' y' path arr
sizeA' x' y' (0:path) (LocatorA location arr)           = sizeA' x' y' path arr
sizeA' _  _  pth      arr                               = debug Err ("ArrUtils.sizeA': "++show pth++" "++show arr) (0,0,0,0)
-- what about focus paths in other places than leaf strings?



-- is this necessary? The data structure is strict already.
walk (EmptyA _ x y w h hr vr)               = x+y+w+h+hr+vr
walk (StringA _ x y w h hr vr str c f _)    = x+y+w+h+hr+vr+length str+ walkC c+ fSize f
walk (ImageA _ x y w h hr vr _ _ c1 c2)     = x+y+w+h+hr+vr + walkC c1 + walkC c2
walk (PolyA _ x y w h hr vr _ _ c1 c2)      = x+y+w+h+hr+vr + walkC c1 + walkC c2
walk (RectangleA _ x y w h hr vr _ _ c1 c2) = x+y+w+h+hr+vr + walkC c1 + walkC c2
walk (EllipseA _ x y w h hr vr _ _ c1 c2)   = x+y+w+h+hr+vr + walkC c1 + walkC c2
walk (LineA _ x y x' y' hr vr _ c1)         = x+y+x'+y'+hr+vr + walkC c1
walk (RowA _ x y w h hr vr c1 arrs)         = x+y+w+h+hr+vr + walkC c1 +  walkList arrs
walk (ColA _ x y w h hr vr c1 arrs)         = x+y+w+h+hr+vr + walkC c1 + walkList arrs
walk (OverlayA _ x y w h hr vr c1 arrs)     = x+y+w+h+hr+vr + walkC c1 + walkList arrs
walk (GraphA _ x y w h hr vr c1 arrs)       = x+y+w+h+hr+vr + walkC c1 + walkList arrs
walk (VertexA _ x y w h hr vr c1 arr)       = x+y+w+h+hr+vr + walkC c1 + walk arr
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
  | LineA       !IDA !XCoord !YCoord !XCoord !YCoord !Int !RColor
  | RowA        !IDA !XCoord !YCoord !Width !Height !RColor ![Arrangement]
  | ColA        !IDA !XCoord !YCoord !Width !Height !RColor ![Arrangement]
  | OverlayA    !IDA !XCoord !YCoord !Width !Height !RColor ![Arrangement]
  | GraphA      !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !Color ![Arrangement node]
  | VertexA     !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !Color !(Arrangement node)
  -- | matrix is different from col of rows, even in arrangement (e.g. selection)

-}

-- for creating edge arrangements during graph arranging
mkEdges :: Show node => [(Int,Int)] -> [(Int,Int, Outline)] -> Color -> [Arrangement node]
mkEdges edges vertices lineColor = showDebug' Err ("mkEdges"++show edges ++ show vertices) $ map mkEdge edges 
 where mkEdge (fromV, toV) = let (fromVx,fromVy,fromVol) = vertices !! fromV
                                 (toVx,toVy,toVol) = vertices !! toV
                                 (offsetTox, offsetToy) = toVol (computeAngle fromVx fromVy toVx toVy)
                             in  LineA NoIDA  fromVx fromVy (toVx+offsetTox) (toVy+offsetToy) 0 0 1 lineColor 


-- for now, ignore ref's in diff. Even if ref changes but nothing else, no need to redraw.

-- experimental diff, only strings are checked.
-- skip StructuralA ParsingA and LocatorA elts
diffArr (StructuralA id arr) arr'                   = let childDT = diffArr arr arr'
                                                      in  DiffNode (isClean childDT) True [childDT]
diffArr arr                  (StructuralA id arr')  = diffArr arr arr'
diffArr (ParsingA id arr)    arr'                   = let childDT = diffArr arr arr'
                                                      in  DiffNode (isClean childDT) True [childDT]
diffArr arr                  (ParsingA id arr')     = diffArr arr arr'
diffArr (LocatorA l arr)     arr'                   = let childDT = diffArr arr arr'
                                                      in  DiffNode (isClean childDT) True [childDT]
diffArr arr                  (LocatorA l arr')      = diffArr arr arr'

diffArr (EmptyA id x y w h hr vr)     (EmptyA _  x' y' w' h' hr' vr') = DiffLeaf $ x==x' && y==y' && w==w' && h==h'                                                          
diffArr (EmptyA id x y w h hr vr)     _                       = DiffLeaf False
diffArr (StringA id x y w h hr vr str lc f _) (StringA _ x' y' w' h' hr' vr' str' lc' f' _) = 
  DiffLeaf $ x==x' && y==y' && w==w' && h==h' && str==str' && lc==lc' && f==f'
diffArr (StringA id x y w h hr vr str lc f _)  _                                    = DiffLeaf False
{-
diffArr (ImageA  id src)       (ImageA  _ src') = Clean 
diffArr (ImageA  id src)       _                = Dirty
diffArr (PolyA   id  _ _)      (PolyA   _  _ _) = Clean 
diffArr (PolyA   id _ _)       _                = Dirty
diffArr (RectangleA id  _ _ _) (RectangleA _  _ _ _)  = Clean 
diffArr (RectangleA id  _ _ _) _                 = Dirty 
diffArr (EllipseA id  _ _ _) (EllipseA _  _ _ _)  = Clean 
diffArr (EllipseA id  _ _ _) _                 = Dirty 
-}
diffArr (RowA id x y w h hr vr bc arrs) (RowA id' x' y' w' h' hr' vr' bc' arrs') =  
  diffArrs x y w h bc arrs x' y' w' h' bc' arrs'
diffArr (ColA id x y w h hr vr bc arrs) (ColA id' x' y' w' h' hr' vr' bc' arrs') = 
  diffArrs x y w h bc arrs x' y' w' h' bc' arrs'
diffArr (OverlayA id x y w h hr vr bc arrs) (OverlayA id' x' y' w' h' hr' vr' bc' arrs') =
  diffArrs x y w h bc arrs x' y' w' h' bc' arrs'
diffArr (GraphA id x y w h hr vr bc arrs) (GraphA id' x' y' w' h' hr' vr' bc' arrs') =
  diffArrs x y w h bc arrs x' y' w' h' bc' arrs'
diffArr arr@(RowA id x y w h hr vr bc arrs) _                            = DiffLeaf False 
diffArr arr@(ColA id x y w h hr vr bc arrs) _                            = DiffLeaf False 
diffArr arr@(OverlayA id x y w h hr vr bc arrs) _                        = DiffLeaf False 
diffArr arr@(GraphA id x y w h hr vr bc arrs) _                          = DiffLeaf False 
diffArr (VertexA id x y w h hr vr bc arr) (VertexA id' x' y' w' h' hr' vr' bc' arr') =
 let childDT = diffArr arr arr'
 in  DiffNode (isClean childDT) (x==x' && y==y' && w==w' && h==h' && bc==bc') [childDT]
diffArr _                             _                                = DiffLeaf True -- all others are unchanged
diffArr arr                           _                                = debug Err ("ArrUtils.diffArr: can't handle "++ show arr) $ DiffLeaf False

-- pres is different when either self has changed or children

diffArrs x y w h bc arrs x' y' w' h' bc' arrs' =  
  let nrOfArrs    = length arrs
      nrOfArrs'   = length arrs'
      childDiffs  = zipWith diffArr arrs arrs'
      childDiffs' = take nrOfArrs $ childDiffs ++ repeat (DiffLeaf False)
      selfClean   =    x==x' && y==y' && w==w' && h==h' && bc==bc' 
                    && nrOfArrs == nrOfArrs'
  in  DiffNode ( selfClean && all isClean childDiffs) selfClean
               (if not selfClean
                then repeat (DiffLeaf False)  -- is self is dirty, all below need to be rerendered
                else childDiffs)

updatedRectArr :: Show node => DiffTree -> Arrangement node -> Maybe (Int, Int, Int, Int)
updatedRectArr dt arr = updatedRectArr' 0 0 dt arr 

updatedRectArr' :: Show node => Int -> Int -> DiffTree -> Arrangement node -> Maybe (Int, Int, Int, Int)
updatedRectArr' x' y' dt arr = 
  case dt of
    DiffLeaf True            -> Nothing                --
    DiffNode True  _     _   -> Nothing                -- if selfAndChildren clean then no rect.
    DiffLeaf False           -> let x = x' + xA arr
                                    y = y' + yA arr
                                in  Just (x, y, x+widthA arr, y+heightA arr)
    DiffNode False False dts -> let x = x' + xA arr
                                    y = y' + yA arr
                                in  Just (x, y, x+widthA arr, y+heightA arr)
    DiffNode False True  dts    ->     -- self is clean, so take union of rectangles of children
      case arr of
      (StructuralA _ arr)           -> if not (null dts) then updatedRectArr' x' y' (head dts) arr else problem
      (ParsingA _ arr)              -> if not (null dts) then updatedRectArr' x' y' (head dts) arr else problem
      (LocatorA _ arr)              -> if not (null dts) then updatedRectArr' x' y' (head dts) arr else problem
      (RowA id x y w h hr vr bc arrs)     -> updatedRectArrs (x'+x) (y'+y) dts arrs
      (ColA id x y w h hr vr bc arrs)     -> updatedRectArrs (x'+x) (y'+y) dts arrs 
      (OverlayA id x y w h hr vr bc arrs) -> updatedRectArrs (x'+x) (y'+y) dts arrs
      (GraphA id x y w h hr vr bc arrs)   -> updatedRectArrs (x'+x) (y'+y) dts arrs
      (VertexA id x y w h hr vr bc arr)   -> if not (null dts) then updatedRectArr' (x'+x) (y'+y) (head dts) arr else problem
      (OverlayA id x y w h hr vr bc arrs) -> updatedRectArrs (x'+x) (y'+y) dts arrs
      _                             -> problem
 where updatedRectArrs x' y' dts arrs = let (luxs,luys,rlxs,rlys) = unzip4 . concat .  map (maybe [] (:[])) $ zipWith (updatedRectArr' x' y') dts arrs
                                        in  if null luxs
                                            then problem
                                            else Just (minimum luxs, minimum luys, maximum rlxs, maximum rlys)
       problem = debug Err ("ArrUtils.updatedRectArr: problem with "++ shallowShowArr arr) $ Nothing -- show dt when we want to debug this


-- mark the focus path as changed in the DiffTree
-- should be done for old as well as new focus (in case of navigate without update)
markFocusArr :: FocusArr -> DiffTree -> DiffTree
markFocusArr (FocusA (PathA fromPth _) (PathA toPth _)) dt = markDirty (commonPrefix fromPth toPth) dt
markFocusArr _ dt = dt


-- mark all nodes on path as children dirty, when descending beyond clean leaf, add new clean nodes around dirty path
markDirty :: [Int] -> DiffTree -> DiffTree
markDirty [] _ = DiffLeaf False               -- entire subtree is marked dirty
markDirty _ (DiffLeaf False) = DiffLeaf False -- subtree already dirty
markDirty (p:pth) (DiffLeaf True) = DiffNode False True $  -- make self clean node with one dirty child on the path
                                         replicate (p) (DiffLeaf True)
                                      ++ [markDirty pth (DiffLeaf True)] -- do the same thing for the rest of the path
                                      ++ replicate 100 (DiffLeaf True) -- aargh! This standalone diff tree is not a good solution
                                                                                     -- can't repeat here, since renderer sometimes does a reverse
-- need to pass the arrangement along (which is a nasty pattern match), just for that case.
-- Otherwise, fix renderer hack with overlay order (so no reverse is needed)
-- or fix dirty bit in actual arrangement nodes.
markDirty (p:pth) (DiffNode _ self dts) = DiffNode False self $ -- leaf self 
                                               take p dts
                                            ++ [markDirty pth (dts !! p)]
                                            ++ drop (p+1) dts



{-
--quite inefficient at the moment
-- function is a bit weird anyway, accumulating parameter is list of paths

-- result is list of paths because pointing can be ambiguous (overlays)
point' :: Int -> Int -> [[Int]] -> Arrangement node -> [[Int]]
point' x' y' loc p@(EmptyA _)                     = [] -- ?? strange case, does empty have size?
point' x' y' loc p@(StringA _ x y w h _ _ _ _)    = if inside x' y' x y w h then loc else [] 
point' x' y' loc p@(RectangleA _ x y w h _ _ _ _) = if inside x' y' x y w h then loc else [] 
point' x' y' loc p@(EllipseA _ x y w h _ _ _ _)   = if inside x' y' x y w h then loc else [] 
point' x' y' loc p@(LineA _ x y w h _ _)          = if inside x' y' x y w h then loc else [] 
point' x' y' loc p@(RowA _ x y w h _ arrs)        = if inside x' y' x y w h 
                                                   then let locs = concat [point' (x'-x) (y'-y) (map (++[i]) loc) p | (i,p) <- zip [0..] arrs] 
                                                        in  if null locs then loc else locs
                                                   else [] 
point' x' y' loc p@(ColA _ x y w h _ arrs)        = if inside x' y' x y w h 
                                                   then let locs = concat [point' (x'-x) (y'-y) (map (++[i]) loc) p | (i,p) <- zip [0..] arrs] 
                                                        in  if null locs then loc else locs
                                                   else [] 
point' x' y' loc p@(LocatorA location child)      = point' x' y' (map (++[0]) loc) child

inside x' y' x y w h = x' >= x && x' <= x+w && y' >= y && y'<= y+h
-}
-- Stretching rows do not lead to correct pointing.
point' :: Show node => Int -> Int -> [[Int]] -> Arrangement node -> [[Int]]
point' x' y' _ arr = showDebug Ren $ point (clip 0 (widthA arr-1) (x'-xA arr)) 
                                       (clip 0 (heightA arr-1) (y'-yA arr)) [] arr

-- precondition: x' y' falls inside the arrangement
point :: Show node => Int -> Int -> [Int] -> Arrangement node -> [[Int]]
--point x' y' loc p@(EmptyA _)                     = [] -- does not occur at the moment
point x' y' loc p@(StringA _ x y w h _ _ _ _ _ _)       = [loc]
point x' y' loc p@(RectangleA _ x y w h _ _ _ _ _ _) = [loc]
point x' y' loc p@(EllipseA _ x y w h _ _ _ _ _ _)   = [loc]
--point x' y' loc p@(LineA _ x y w h _ _ _ _)          = [loc]
point x' y' loc p@(RowA _ x y w h _ _ _ arrs)           = pointRowList 0 (x') (y') loc arrs
point x' y' loc p@(ColA _ x y w h _ _ _ arrs)           = pointColList 0 (x') (y') loc arrs
point x' y' loc p@(OverlayA _ x y w h _ _ _ arrs@(arr:_)) = point (clip 0 (widthA arr-1) x')  
                                                            (clip 0 (heightA arr-1) y') (loc++[0]) arr
point x' y' loc p@(StructuralA _ child)             = point x' y' (loc++[0]) child
point x' y' loc p@(ParsingA _ child)                = point x' y' (loc++[0]) child
point x' y' loc p@(LocatorA location child)         = point x' y' (loc++[0]) child
point x' y' _   arr                                 = debug Err ("ArrTypes.point': unhandled arrangement: "++show x'++show y'++show arr) [[]]

-- precondition: x' y' falls inside the arrangement width
pointRowList :: Show node => Int -> Int -> Int -> [Int] -> [Arrangement node] -> [[Int]]
pointRowList i x' y' loc []         = debug Err "ArrTypes.pointRowList: empty Row list" $ []
pointRowList i x' y' loc (arr:arrs) = if x' >= xA arr + widthA arr 
                                      then pointRowList (i+1) x' y' loc arrs
                                      else point (x'-xA arr) (clip 0 (heightA arr-1) (y'- yA arr)) 
                                                 (loc++[i]) arr

pointColList i x' y' loc [] = debug Err "ArrTypes.pointRowList: empty Row list" $ []
pointColList i x' y' loc (arr:arrs) = if y' >= yA arr + heightA arr 
                                      then pointColList (i+1) x' y' loc arrs
                                      else point (clip 0 (widthA arr-1) (x'-xA arr)) (y'-yA arr) 
                                                 (loc++[i]) arr


---pointOvlRef is just for now, until overlays are adjusted to have last arr in front
-- this point is called from popupMenu handler 

pointOvlRev' :: Show node => Int -> Int -> [[Int]] -> Arrangement node -> [[Int]]
pointOvlRev' x' y' _ arr = showDebug Ren $ pointOvlRev (clip 0 (widthA arr-1) (x'-xA arr)) 
                                       (clip 0 (heightA arr-1) (y'-yA arr)) [] arr

-- precondition: x' y' falls inside the arrangement
pointOvlRev :: Show node => Int -> Int -> [Int] -> Arrangement node -> [[Int]]
--pointOvlRev x' y' loc p@(EmptyA _)                     = [] -- does not occur at the moment
pointOvlRev x' y' loc p@(StringA _ x y w h _ _ _ _ _ _)       = [loc]
--pointOvlRev x' y' loc p@(RectangleA _ x y w h _ _ _ _ _ _) = [loc]
--pointOvlRev x' y' loc p@(EllipseA _ x y w h _ _ _ _ _ _)   = [loc]
--pointOvlRev x' y' loc p@(LineA _ x y w h _ _ _ _)          = [loc]
pointOvlRev x' y' loc p@(RowA _ x y w h _ _ _ arrs)           = pointOvlRevRowList 0 (x') (y') loc arrs
pointOvlRev x' y' loc p@(ColA _ x y w h _ _ _ arrs)           = pointOvlRevColList 0 (x') (y') loc arrs
--pointOvlRev x' y' loc p@(OverlayA _ x y w h _ _ _ arrs@(arr:_)) = pointOvlRev (clip 0 (widthA arr-1) x') -- last arr is pointOvlReved one
--                                                            (clip 0 (heightA arr-1) y') (loc++[0]) (last arrs)
pointOvlRev x' y' loc p@(OverlayA _ x y w h _ _ _ arrs@(arr:_)) = pointOvlRev (clip 0 (widthA arr-1) x')  
                                                            (clip 0 (heightA arr-1) y') (loc++[0]) arr
pointOvlRev x' y' loc p@(StructuralA _ child)             = pointOvlRev x' y' (loc++[0]) child
pointOvlRev x' y' loc p@(ParsingA _ child)                = pointOvlRev x' y' (loc++[0]) child
pointOvlRev x' y' loc p@(LocatorA location child)         = pointOvlRev x' y' (loc++[0]) child
pointOvlRev x' y' _   arr                                 = debug Err ("ArrTypes.pointOvlRev': unhandled arrangement: "++show x'++show y'++show arr) [[]]

-- precondition: x' y' falls inside the arrangement width
pointOvlRevRowList :: Show node => Int -> Int -> Int -> [Int] -> [Arrangement node] -> [[Int]]
pointOvlRevRowList i x' y' loc []         = debug Err "ArrTypes.pointOvlRevRowList: empty Row list" $ []
pointOvlRevRowList i x' y' loc (arr:arrs) = if x' >= xA arr + widthA arr 
                                      then pointOvlRevRowList (i+1) x' y' loc arrs
                                      else pointOvlRev (x'-xA arr) (clip 0 (heightA arr-1) (y'- yA arr)) 
                                                 (loc++[i]) arr

pointOvlRevColList i x' y' loc [] = debug Err "ArrTypes.pointOvlRevRowList: empty Row list" $ []
pointOvlRevColList i x' y' loc (arr:arrs) = if y' >= yA arr + heightA arr 
                                      then pointOvlRevColList (i+1) x' y' loc arrs
                                      else pointOvlRev (clip 0 (widthA arr-1) (x'-xA arr)) (y'-yA arr) 
                                                 (loc++[i]) arr




-- Temporary pointing stuff for hacked popups

--getDoc :: Arrangement node -> () -- Document
--getDoc arr = debug err (show rr) $ 

pointDoc' :: Show node =>  Int -> Int -> Arrangement node -> [node]
pointDoc' x' y' arr = showDebug Ren $ pointDoc (clip 0 (widthA arr-1) (x'-xA arr)) 
                                       (clip 0 (heightA arr-1) (y'-yA arr)) [] arr

-- loc is now list of nodes, but should be a (Maybe node).

-- precondition: x' y' falls inside the arrangement
pointDoc :: Show node => Int -> Int -> [node] -> Arrangement node -> [node]
--pointDoc x' y' loc p@(EmptyA _ _ _ _ _ _)                     = [] -- does not occur at the moment
pointDoc x' y' loc p@(StringA _ x y w h _ _ _ _ _ _)       = loc
--pointDoc x' y' loc p@(RectangleA _ x y w h _ _ _ _ _ _) = [loc]
--pointDoc x' y' loc p@(EllipseA _ x y w h _ _ _ _ _ _)   = [loc]
--pointDoc x' y' loc p@(LineA _ x y w h _ _ _ _)          = [loc]
pointDoc x' y' loc p@(RowA _ x y w h _ _ _ arrs)           = pointDocRowList 0 (x') (y') loc arrs
pointDoc x' y' loc p@(ColA _ x y w h _ _ _ arrs)           = pointDocColList 0 (x') (y') loc arrs
pointDoc x' y' loc p@(OverlayA _ x y w h _ _ _ arrs@(arr:_)) = pointDoc (clip 0 (widthA arr-1) x') 
                                                            (clip 0 (heightA arr-1) y') loc arr -- (last arrs)
pointDoc x' y' loc p@(StructuralA _ child)             = pointDoc x' y' loc child
pointDoc x' y' loc p@(ParsingA _ child)                = pointDoc x' y' loc child
pointDoc x' y' loc p@(LocatorA location child)         = pointDoc x' y' [location] child
pointDoc x' y' loc arr                                 = debug Err ("ArrTypes.pointDoc': unhandled arrangement: "++show x'++show y'++show arr) loc

-- precondition: x' y' falls inside the arrangement width
pointDocRowList :: Show node => Int -> Int -> Int -> [node] -> [Arrangement node] -> [node]
pointDocRowList i x' y' loc []         = debug Err "ArrTypes.pointDocRowList: empty Row list" $ loc
pointDocRowList i x' y' loc (arr:arrs) = if x' >= xA arr + widthA arr 
                                      then pointDocRowList (i+1) x' y' loc arrs
                                      else pointDoc (x'-xA arr) (clip 0 (heightA arr-1) (y'- yA arr)) 
                                                 loc arr
                                                 
pointDocColList :: Show node => Int -> Int -> Int -> [node] -> [Arrangement node] -> [node]
pointDocColList i x' y' loc [] = debug Err "ArrTypes.pointDocRowList: empty Row list" $ loc
pointDocColList i x' y' loc (arr:arrs) = if y' >= yA arr + heightA arr 
                                      then pointDocColList (i+1) x' y' loc arrs
                                      else pointDoc (clip 0 (widthA arr-1) (x'-xA arr)) (y'-yA arr) 
                                                 loc arr
                                                                   





---
 
                                                                   
selectTreeA :: Show node => [Int] -> Arrangement node -> (Int, Int, Arrangement node)
selectTreeA = selectTreeA' 0 0

selectTreeA' x' y' []       tr                                = (x', y', tr)
selectTreeA' x' y' (p:path) (RowA _ x y _ _ _ _ _ arrs)           = selectTreeA' (x'+x) (y'+y) path (arrs!!p)
selectTreeA' x' y' (p:path) (ColA _ x y _ _ _ _ _ arrs)           = selectTreeA' (x'+x) (y'+y) path (arrs!!p)
--selectTreeA' x' y' (0:path) (OverlayA _ x y _ _ _ _ _ arrs@(arr:_)) = selectTreeA' (x'+x) (y'+y) path (last arrs)
selectTreeA' x' y' (0:path) (OverlayA _ x y _ _ _ _ _ arrs@(arr:_)) = selectTreeA' (x'+x) (y'+y) path arr
selectTreeA' x' y' (p:path) (GraphA _ x y _ _ _ _ _ arrs)           = selectTreeA' (x'+x) (y'+y) path (arrs!!p)
selectTreeA' x' y' (0:path) (VertexA _ x y _ _ _ _ _ arr)     = selectTreeA' (x'+x) (y'+y) path arr
selectTreeA' x' y' (p:path) (StructuralA _ child)             = selectTreeA' x' y' path child
selectTreeA' x' y' (p:path) (ParsingA _ child)                = selectTreeA' x' y' path child
selectTreeA' x' y' (p:path) (LocatorA _ child)                = selectTreeA' x' y' path child
selectTreeA' x' y' (p:path) arr                               = debug Err ("ArrTypes.selectTreeA: unhandled non-empty path: "++show (p:path)++show arr) (x', y', arr)




{-
point can give a negative focus. happens when clicking in non string location in debugged arrangement 
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
debugArrangement' xOffset yOffset (EmptyA id x y w h hr vr) = 
  ( EmptyA id (x+xOffset) (y+yOffset) w h hr vr, 0,0)
debugArrangement' xOffset yOffset (StringA id x y w h hr vr str c f cxs) = 
  ( StringA id (x+xOffset) (y+yOffset) (w+1) h hr vr str c f cxs, 1, 0) -- widen with 1, so focus is inside or on box
debugArrangement' xOffset yOffset (ImageA id x y w h hr vr src style lc bc) = 
  ( ImageA id (x+xOffset) (y+yOffset) (w+pd) (h+pd) hr vr src style lc bc, pd, pd)
debugArrangement' xOffset yOffset (PolyA id x y w h hr vr pts lw lc bc) = 
  ( PolyA id (x+xOffset) (y+yOffset) (w+pd) (h+pd) hr vr pts lw lc bc, pd, pd)
debugArrangement' xOffset yOffset (RectangleA id x y w h hr vr lw style lc fc) = 
  ( RectangleA id (x+xOffset) (y+yOffset) (w+pd) (h+pd) hr vr lw style lc fc, pd, pd)
debugArrangement' xOffset yOffset (EllipseA id x y w h hr vr lw style lc fc) = 
  ( EllipseA id (x+xOffset) (y+yOffset) (w+pd) (h+pd) hr vr lw style lc fc, pd, pd)
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
debugArrangement' xOffset yOffset (ColA id x y w h hr vr c arrs)             =
--  let (arrs', wOffsets, hOffsets) = unzip3 [ debugArrangement' hpd (hpd+ho) arr | 
--                                             (arr,ho) <- zip arrs (scanl (\n1 n2 -> n1+(2*pd-1)+n2 ) 0 hOffsets) ]
  let (arrs', wOffsets, hOffsets) = unzip3 [ debugArrangement' (hpd {-+wo-}) (hpd+ho) arr |  -- pd-1 and not 2pd-1 because child does its own left padding
                                             (arr,wo,ho) <- zip3' arrs 
                                                                  wOffsets --(map (\o -> (maximum wOffsets-o) `div`2) wOffsets)
                                                                  (scanl (\n1 n2 -> n1 +(pd-1)+n2 ) 0 hOffsets)
                                           ]
      wOffset = maximum (0:wOffsets) + pd 
      hOffset = sum hOffsets + pd + (length hOffsets - 1) * (pd -1)
  in  ( ColA id (x+xOffset) (y+yOffset) (w+wOffset) (h+hOffset) hr vr c arrs'
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
debugArrangement' xOffset yOffset (GraphA id x y w h hr vr c arrs)              =
  let (arrs', wOffsets, hOffsets) = unzip3 [ debugArrangement' (hpd {-+wo-}) (hpd {-+ho-}) arr | 
                                             (arr,wo,ho) <- zip3' arrs 
                                                                  wOffsets --(map (\o -> (maximum wOffsets-o) `div`2) wOffsets)
                                                                  hOffsets -- (map (\o -> (maximum hOffsets-o) `div`2) hOffsets)
                                           ]
      wOffset = maximum (0:wOffsets) + pd 
      hOffset = maximum (0:hOffsets) + pd 
  in  ( GraphA id (x+xOffset) (y+yOffset) (w+wOffset) (h+hOffset) hr vr c arrs'
      , wOffset ,hOffset )

--debugArrangement' xOffset yOffset (OverlayA id x y w h c (arr:arrs))              =
--  let (arr', wOffset, hOffset) = debugArrangement' (xOffset+pd) (yOffset+pd) arr
--  in  (OverlayA id (x+xOffset) (y+yOffset) (w+wOffset+pd) (h+hOffset+pd) c (arr':arrs), wOffset+pd, hOffset+pd)
debugArrangement' xOffset yOffset (LocatorA location arr)              =
  let (arr', wOffset, hOffset) = debugArrangement' (xOffset) (yOffset) arr
  in  (LocatorA location arr', wOffset, hOffset)
debugArrangement' xOffset yOffset (StructuralA id arr)              =
  let (arr', wOffset, hOffset) = debugArrangement' (xOffset) (yOffset) arr
  in  (StructuralA id arr', wOffset, hOffset)
debugArrangement' xOffset yOffset (ParsingA id arr)              =
  let (arr', wOffset, hOffset) = debugArrangement' (xOffset) (yOffset) arr
  in  (ParsingA id arr', wOffset, hOffset)
debugArrangement' _ _ arr = debug Err ("Renderer.debugArrangement': unimplemented arrangement: "++show arr) (EmptyA NoIDA 0 0 0 0 0 0,0,0)

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