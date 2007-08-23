module PresUtils (module XprezLib, module PresUtils) where

import CommonTypes
import PresTypes
import CommonUtils
 
import XprezLib
import Maybe

import ArrTypes

squiggly :: Color -> Xprez doc node clip -> Xprez doc node clip
squiggly c xp = overlay [xp, img "img/squiggly.png" `withHeight` 3 `withColor` c, empty]
-- png is the red one, only temporary

ifFocusP NoFocusP           _   = NoFocusP
ifFocusP (FocusP NoPathP _) _   = NoFocusP
ifFocusP (FocusP _ NoPathP) _   = NoFocusP
ifFocusP (FocusP _ _)       exp = exp

ifPathP NoPathP _       = NoPathP
ifPathP (PathP _ _) exp = exp

consFocusP s foc@(FocusP from to) = FocusP (consPathP s from) (consPathP s to) 
consFocusP s _                    = NoFocusP

consPathP s p@(PathP pth ix) = ifPathP p $ PathP (s:pth) ix
consPathP s _                = NoPathP

orderFocusP (FocusP from to) = FocusP (min from to) (max from to)
orderFocusP NoFocusP         = NoFocusP


-- set background so user knows doc is unparsed
-- if top node is With, don't do anything, so we won't pile up background with nodes.
-- this is a hack, and means that the presentation must not have a with node at the top
-- need some local state for this, and some way of putting the information in the presentation

--markUnparsed p@(WithP ar _) = let c = backgroundColor.fst $ (ar emptyAttrs)
--                              in  if c /= whiteSmoke
--                              then setUpd AllUpdated $  debug Prs "no With" (p`withbgColor` whiteSmoke)
--                              else debug Prs "With" p
--markUnparsed p           =  debug Prs "no With2" p `withbgColor` whiteSmoke
--markUnparsedF (WithP ar p) f = let c = backgroundColor.fst $ (ar emptyAttrs)
--                              in  if c /= whiteSmoke then debug Prs "no With" consFocusP 0 f else debug Prs "With" f
--markUnparsedF _           f = consFocusP 0 f
markUnparsed p           =  p
markUnparsedF _           f = f

xyFromPath :: PathPres -> Presentation doc node clip -> (Int,Int, Bool)
xyFromPath path pres = xyFromPathPres 0 0 path pres

pathFromXY :: (Int,Int,Bool) -> Presentation doc node clip -> PathPres
pathFromXY xy pres = pathFromXYPres xy pres


-- experimental diff
-- attributes are still ignored.
-- skip WithP StructuralP ParsingP and LocatorP elts
diffPres :: Presentation doc node clip -> Presentation doc node clip -> DiffTree

-- WithP is not handled yet.
-- StructuralP ParsingP and LocatorP are ignored since they don't affect rendering
diffPres (WithP ar pres)       pres'                   = diffPres pres pres'
diffPres pres                  (WithP ar' pres')       = diffPres pres pres'
diffPres (StructuralP id pres) pres'                   = diffPres pres pres'
diffPres pres                  (StructuralP id pres')  = diffPres pres pres'
diffPres (ParsingP id pres)    pres'               = diffPres pres pres'
diffPres pres                  (ParsingP id pres') = diffPres pres pres'
diffPres (LocatorP l pres)     pres'               = diffPres pres pres'
diffPres pres                  (LocatorP l pres')  = diffPres pres pres'

diffPres (EmptyP id)            (EmptyP _)       = DiffLeaf True
diffPres (EmptyP id)             _               = DiffLeaf False
diffPres (StringP id str)       (StringP _ str') = DiffLeaf $ str == str'
diffPres (StringP id str)       _                = DiffLeaf False
diffPres (ImageP  id src style)       (ImageP  _ src' style') = DiffLeaf $ src == src' && style == style'
diffPres (ImageP  id src _)       _                = DiffLeaf False
diffPres (PolyP   id pts lw style)      (PolyP   _ pts' lw' style') = DiffLeaf $ lw==lw' && pts==pts' && style == style'
diffPres (PolyP   id _ _ _)       _                       = DiffLeaf False
diffPres (RectangleP id  w h lw style) (RectangleP _ w' h' lw' style')  = DiffLeaf $ w==w' && h==h' && lw==lw' && style == style'
diffPres (RectangleP id  _ _ _ _) _                          = DiffLeaf False
diffPres (EllipseP id  w h lw style) (EllipseP _ w' h' lw' style')  = DiffLeaf $ w==w' && h==h' && lw==lw' && style == style'
diffPres (EllipseP id _ _ _ _) _                        = DiffLeaf False
diffPres (RowP id rf press) (RowP id' rf' press')  = diffPress rf press rf' press'
diffPres (ColP id rf press) (ColP id' rf' press')  = diffPress rf press rf' press'
diffPres (OverlayP id press) (OverlayP id' press') = diffPress 0  press 0   press'
diffPres (FormatterP id press) (FormatterP id' press')  = diffPress 0 press 0 press'
diffPres (GraphP id _ _ _ _ press) (GraphP id' _ _ _ _ press') = diffPress 0  press 0   press'
diffPres (VertexP id _ _ _ _ pres) (VertexP id' _ _ _ _ pres') = diffPres pres pres'
diffPres (RowP id rf press) _                      = DiffLeaf False
diffPres (ColP id rf press) _                      = DiffLeaf False
diffPres (OverlayP id press) _                     = DiffLeaf False 
diffPres (GraphP id _ _ _ _ press) _               = DiffLeaf False
diffPres (VertexP id _ _ _ _ pres) _                 = DiffLeaf False
diffPres pr                  _                     = debug Err ("PresUtils.diffPres: can't handle "++ show pr) DiffLeaf False


diffPress rf press rf' press' =
  let nrOfPress   = length press
      nrOfPress'  = length press'
      childDiffs  = zipWith diffPres press press'
      childDiffs' = take nrOfPress $ childDiffs ++ repeat (DiffLeaf False)
      selfClean   = rf==rf' && nrOfPress==nrOfPress'
  in  DiffNode (selfClean && all isCleanDT childDiffs') 
               selfClean
               childDiffs'


-- we can prune safely whenever the arrangement is not in the newly uncovered area
-- (x',y') is the absolute offset of the arrangement
uncovered :: Show node => (Int,Int) -> Rectangle -> Rectangle -> Arrangement node -> Bool
uncovered (x',y') va ova arr =
  let ((x,y),(w,h)) = getAreaA arr
      arrAreaAbs = ((x'+x,y'+y),(w,h))
  in  overlap arrAreaAbs va

-- getChildA :: Show node => Arrangement node -> Arrangement node
getChildA caller arr = case getChildrenA arr of
                  child : _ -> child
                  []        -> debug Err ("ArrUtils.getChildA: no child " ++show arr++ caller)arr

getAreaA :: Show node => Arrangement node -> Rectangle
getAreaA a = ((xA a, yA a), (widthA a, heightA a))

addOffsetA :: Show node => (Int,Int) -> Arrangement node -> (Int,Int)
addOffsetA (x,y) a = (x+xA a, y+yA a)
addOffsetA (x,y) (ParsingA _ _) = debug Err "ArrUtils.addOffsetA called on ParsingA" (x,y)
addOffsetA (x,y) (StructuralA _ _) = debug Err "ArrUtils.addOffsetA called on StructuralA" (x,y)
addOffsetA (x,y) (LocatorA _ _) = debug Err "ArrUtils.addOffsetA called on LocatorA" (x,y)

-- belong in ArrLayerUtils, also remove import ArrTypes
prunePres  va ova (x,y) arr dt pres@(FormatterP _ _) = 
  debug Err (shallowShowArr arr ++ "\n\n\n\n\n\n\n\n\n\n!!!!!!!!!          "++shallowShowPres pres) $
  prunePres' va ova (x,y) arr dt pres
prunePres  va ova (x,y) arr dt pres = 
  debug Err (shallowShowArr arr ++ "          "++shallowShowPres pres) $
  prunePres' va ova (x,y) arr dt pres

prunePres' :: Show node => Rectangle -> Rectangle -> (Int,Int) -> Arrangement node -> DiffTree ->
              Presentation doc node clip -> Presentation doc node clip
prunePres' va ova (x,y) arr (DiffLeaf False) p = p
prunePres' va ova (x,y) arr dt (WithP wr pres)       = WithP wr       $ prunePres va ova (x,y) arr dt pres
prunePres' va ova (x,y) arr dt (StructuralP id pres) = StructuralP id $ prunePres va ova (x,y)  (getChildA "str" arr) dt pres
prunePres' va ova (x,y) arr dt (ParsingP id pres)    = ParsingP id    $ prunePres va ova (x,y)  (getChildA "pars" arr) dt pres
prunePres' va ova (x,y) arr dt (LocatorP l pres)     = LocatorP l     $ prunePres va ova (x,y)  (getChildA "loc" arr) dt pres
prunePres' va ova (x,y) arr dt (VertexP id v x' y' ol pres) = VertexP id v x' y' ol $ prunePres va ova (addOffsetA (x,y) arr) (getChildA "vrt" arr) dt pres

-- maybe not useful for leafs.
--prunePres' va ova arr (DiffLeaf c) p@(FormatterP id _)      = p
prunePres' va ova (x,y) arr (DiffLeaf c) p@(EmptyP id)            = if c && not (uncovered (x,y) va ova arr) then ArrangedP else p
prunePres' va ova (x,y) arr (DiffLeaf c) p@(StringP id str)       = if c && not (uncovered (x,y) va ova arr) then ArrangedP else p
prunePres' va ova (x,y) arr (DiffLeaf c) p@(ImageP  id src _)     = if c && not (uncovered (x,y) va ova arr) then ArrangedP else p
prunePres' va ova (x,y) arr (DiffLeaf c) p@(PolyP   id _ _ _)       = if c && not (uncovered (x,y) va ova arr) then ArrangedP else p
prunePres' va ova (x,y) arr (DiffLeaf c) p@(RectangleP id  _ _ _ _) = if c && not (uncovered (x,y) va ova arr) then ArrangedP else p
prunePres' va ova (x,y) arr (DiffLeaf c) p@(EllipseP id  _ _ _ _)   = if c && not (uncovered (x,y) va ova arr) then ArrangedP else p
prunePres' va ova (x,y) (PolyA _ _ _ _ _ _ _ _ _ _ _ _ _) _ p = p
prunePres' va ova (x,y) arr (DiffNode c _ dts) p =
  let pruned = zipWith3 (prunePres va ova (addOffsetA (x,y) arr)) (getChildrenA arr++repeat (EmptyA NoIDA 0 0 0 0 0 0 transparent)) dts (getChildren p)
  -- if arr has fewer kids than p then dts must end with (DiffLeaf False)'s, so it does not matter
  -- what we extend arr with (pres is used anyway in the recursion)
  in if c then if showDebug' Err ("unc1"++show (getAreaA arr)++show va) $ uncovered (x,y) va ova arr
                then setChildren pruned p
                else debug Err ("#############Arranged "++ show (getAreaA arr)) ArrangedP 
      else setChildren pruned p
prunePres' va ova (x,y) arr (DiffLeaf c) p       = 
  let pruned = zipWith3 (prunePres va ova (addOffsetA (x,y) arr)) (getChildrenA arr) (repeat $ DiffLeaf True) (getChildren p)
  in  if c
      then if showDebug' Err ("unc"++show (getAreaA arr)++show va) $ uncovered (x,y) va ova arr
           then setChildren pruned p
           else debug Err ("#############Arranged "++ show (getAreaA arr)) ArrangedP
      else p
prunePres' va ova (x,y) arr dt                 pr                  = debug Err ("PresUtils.prunePres: can't handle "++ show pr++" with "++show dt) $ pr


                   

-- probably goes wrong when inserted is not direct child of row, col, or overlay
-- anyway, the token tree will soon be replaced by a list, making this function easy
deleteInsertedTokens :: InsertedTokenList -> Presentation doc node clip -> Presentation doc node clip
deleteInsertedTokens inss (RowP i r press)     = let press' = map (deleteInsertedTokens inss) press
                                                     press'' = filter ({-not.(`elem` inss)-} (/=IDP (-1)) .idP) press'
                                                 in  RowP i r press''
deleteInsertedTokens inss (ColP i r press)     = let press' = map (deleteInsertedTokens inss) press
                                                     press'' = filter ({-not.(`elem` inss)-} (/=IDP (-1)).idP) press'
                                                 in  ColP i r press''
deleteInsertedTokens inss (OverlayP i press)     = let press' = map (deleteInsertedTokens inss) press
                                                       press'' = filter ({-not.(`elem` inss)-} (/=IDP (-1)).idP) press'
                                                   in  OverlayP i press''
deleteInsertedTokens inss (GraphP i d w h es press) = let press' = map (deleteInsertedTokens inss) press
                                                          press'' = filter ({-not.(`elem` inss)-} (/=IDP (-1)).idP) press'
                                                      in  GraphP i d w h es press''
deleteInsertedTokens inss (VertexP i v x y pl pres) = VertexP i v x y pl $ deleteInsertedTokens inss pres
deleteInsertedTokens inss (WithP ar pres)       = WithP ar $ deleteInsertedTokens inss pres
deleteInsertedTokens inss (StructuralP id pres) = StructuralP id $ deleteInsertedTokens inss pres
deleteInsertedTokens inss (ParsingP id pres)    = ParsingP id $ deleteInsertedTokens inss pres
deleteInsertedTokens inss (LocatorP l pres)     = LocatorP l $ deleteInsertedTokens inss pres
deleteInsertedTokens inss pres = pres


-- Normalization

-- is it ok if we normalize away all ids? Or should we only remove the NoIDP's?
-- what about rows with refs?  look at strip
-- maybe bit harder for normalizeRow

-- in overlay, only head is normalized

normalizePres pres@(EmptyP               _)            = pres
normalizePres pres@(StringP              _  str)       = pres
normalizePres pres@(ImageP               _ _  _)       = pres
normalizePres pres@(PolyP                _  _  _  _)    = pres
normalizePres pres@(RectangleP           _  _  _  _  _) = pres
normalizePres (RowP id rf press)                       = RowP id rf $ normalizeRow press
normalizePres (ColP id rf press)                       = normalizeCol id 0 rf [] press
normalizePres (OverlayP id (pres:press))               = OverlayP id (normalizePres pres : press)
normalizePres (WithP ar pres)                          = WithP ar $ normalizePres pres
normalizePres (StructuralP id pres)                    = StructuralP id $ normalizePres pres
normalizePres (ParsingP id pres)                       = ParsingP id $ normalizePres pres
normalizePres (LocatorP l pres)                        = LocatorP l $ normalizePres pres
normalizePres (GraphP id d w h es press)               = GraphP id d w h es $ map normalizePres press
normalizePres (VertexP id v x y ol pres)               = VertexP id v x y ol $ normalizePres pres
normalizePres pr                                       = debug Err ("PresUtils.normalizePres: can't handle "++ show pr) pr

normalizeRow :: [Presentation doc node clip] -> [Presentation doc node clip] -- not fixed for refs
normalizeRow []                      = []
normalizeRow (RowP id rf press: row) = normalizeRow (press ++ row)
-- normalizeRow (StringP id [] : row) = normalizeRow row        -- don't remove empty strings, because row[row []] presents wrongly 
normalizeRow (StringP id txt : row)  = case normalizeRow row of
                                        (StringP _ txt':row') -> StringP id (txt++txt') : row'
                                        row'                  -> StringP id txt : row'
normalizeRow (pres            : row) = normalizePres pres : normalizeRow row

normalizeCol :: IDP -> Int -> Int -> [Presentation doc node clip] -> [Presentation doc node clip] -> Presentation doc node clip
normalizeCol id p rf prs [] = ColP id rf (reverse prs) 
normalizeCol id p rf prs (ColP _ rf' press: col) = 
  let rf'' = if p < rf then rf + length press - 1 -- -1 because the col (1) is replaced by length press children 
                     else if p == rf then rf+rf'
                     else rf
  in  normalizeCol id p rf''  prs (press ++ col)
normalizeCol id p rf prs (pres            : col) = normalizeCol id (p+1) rf (normalizePres pres:prs) col

-- | Return innermost enclosing locator for path in pres
locateTreePres :: PathPres -> Presentation doc node clip -> Maybe node
locateTreePres NoPathP        pres = Nothing
locateTreePres (PathP path _) pres = locateTreePres' Nothing path pres

locateTreePres' location _        (StringP id str)           = location
locateTreePres' location _        (ImageP _ _ _)             = location
locateTreePres' location _        (PolyP _ _ _ _)            = location
locateTreePres' location []       (VertexP id _ _ _ _ pres)  = location
locateTreePres' location (p:path) (RowP id rf press)         = locateTreePres' location path (index "PresUtils.locateTreePres'" press p)
locateTreePres' location (p:path) (ColP id rf press)         = locateTreePres' location path (index "PresUtils.locateTreePres'" press p)                            
locateTreePres' location (0:path) (OverlayP id press@(pres:_)) = locateTreePres' location path (index "PresUtils.locateTreePres'" press 0)              
locateTreePres' location []       (GraphP id _ _ _ _ press)  = location
locateTreePres' location (p:path) (GraphP id _ _ _ _ press)  = locateTreePres' location path (index "PresUtils.locateTreePres'" press p)
locateTreePres' location (0:path) (VertexP id _ _ _ _ pres)  = locateTreePres' location path pres
locateTreePres' location (0:path) (WithP ar pres)            = locateTreePres' location path pres
locateTreePres' location (0:path) (StructuralP id pres)      = locateTreePres' location path pres
locateTreePres' location (0:path) (ParsingP id pres)         = locateTreePres' location path pres
locateTreePres' location (0:path) (LocatorP l pres)          = locateTreePres' (Just l) path pres
locateTreePres' location (p:path) (FormatterP id press)      = locateTreePres' location path (index "PresUtils.locateTreePres'" press p)
locateTreePres' location pth      pr                         = debug Err ("*** PresUtils.locateTreePres: can't handle "++show pth++" "++ show pr++"***") Nothing

isEditableTreePres path pres = isEditableTreePres' True path pres

isEditableTreePres' editable []       _                          = editable
isEditableTreePres' editable (p:path) (RowP id rf press)         = isEditableTreePres' editable path (index "PresUtils.isEditableTreePres'" press p)
isEditableTreePres' editable (p:path) (ColP id rf press)         = isEditableTreePres' editable path (index "PresUtils.isEditableTreeTreePres'" press p)            
isEditableTreePres' editable (0:path) (OverlayP id press@(pres:_)) = isEditableTreePres' editable path (index "PresUtils.isEditableTreeTreePres'" press 0)              
isEditableTreePres' editable (p:path) (GraphP id _ _ _ _ press) = isEditableTreePres' editable path (index "PresUtils.isEditableTreeTreePres'" press p)
isEditableTreePres' editable (0:path) (VertexP id _ _ _ _ pres)  = isEditableTreePres' editable path pres
isEditableTreePres' editable (0:path) (WithP ar pres)            = isEditableTreePres' editable path pres
isEditableTreePres' editable (0:path) (StructuralP id pres)      = isEditableTreePres' False path pres
isEditableTreePres' editable (0:path) (ParsingP id pres)         = isEditableTreePres' True path pres
isEditableTreePres' editable (0:path) (LocatorP l pres)          = isEditableTreePres' editable path pres
isEditableTreePres' editable (p:path) (FormatterP id press)      = isEditableTreePres' editable path (index "PresUtils.isEditableTreePres'" press p)
isEditableTreePres' editable pth      pr                         = debug Err ("*** PresUtils.isEditableTreePres': can't handle "++show pth++" "++ show pr++"***") False


-- hack for repositioning focus after document present or normalize
-- goes wrong if focus is in empty string on left side of column

-- Bool is for disambiguating end of one string and start of the next. True means at start of string
xyFromPathPres x y (PathP p i)     (EmptyP _)                = (x, y, i==0)  -- should not occur
xyFromPathPres x y (PathP p i)     (StringP _ str)           = (x+i, y, i==0 && length str /= 0 )
xyFromPathPres x y (PathP p i)     (ImageP _ _ _)            = (x, y, i==0)
xyFromPathPres x y (PathP p i)     (PolyP _ _ _ _)           = (x, y, i==0)
xyFromPathPres x y path           pr@(RowP id rf press)     = xyFromPathRow (x) (y+topHeightPres pr) path press
xyFromPathPres x y path           pr@(ColP id rf press)     = xyFromPathCol (x+leftWidthPres pr) (y) path press
xyFromPathPres x y (PathP (0:p) i) (OverlayP _ (pres:press)) = xyFromPathPres x y (PathP p i) pres
xyFromPathPres x y (PathP (_:p) i) (WithP ar pres)           = xyFromPathPres x y (PathP p i) pres
xyFromPathPres x y (PathP (_:p) i) (StructuralP id pres)     = xyFromPathPres x y (PathP p i) pres
xyFromPathPres x y (PathP (_:p) i) (ParsingP id pres)        = xyFromPathPres x y (PathP p i) pres
xyFromPathPres x y (PathP (_:p) i) (LocatorP l pres)         = xyFromPathPres x y (PathP p i) pres
xyFromPathPres x y pth             pr                        = debug Err ("PresUtils.xyFromPathPres: can't handle "++show pth {-++" "++ show pr-}) (0,0, True)

xyFromPathRow x y path@(PathP (s:p) i) press = xyFromPathPres (sum (map widthPres (take s press)) + x) 
                                                              (y-topHeightPres (index "PresUtils.xyFromPathRow" press s))
                                                              (PathP p i) (index "PresUtils.xyFromPathRow" press s)  
xyFromPathRow x y pth  pr = debug Err ("PresUtils.xyFromPathRow: incorrect path"++show pth) (0,0,True)

xyFromPathCol x y path@(PathP (s:p) i) press = xyFromPathPres (x-leftWidthPres (index "PresUtils.xyFromPathCol" press s))
                                                              (sum (map heightPres (take s press)) + y) (PathP p i) (index "PresUtils.xyFromPathCol" press s)  
xyFromPathCol x y pth  pr = debug Err  ("PresUtils.xyFromPathCol: incorrect path"++show pth) (0,0,True)

leftWidthRow rf press = sum (map widthPres (take rf press)) + leftWidthPres (index "PresUtils.leftWidthRow" press rf)
topHeightRow rf press = sum (map heightPres (take rf press)) + topHeightPres (index "PresUtils.topHeightRow" press rf)

s = StringP NoIDP 
r = RowP NoIDP
c = ColP NoIDP

-- does not take ref nrs into account
-- images are hacky
widthPres pres = leftWidthPres pres + rightWidthPres pres
heightPres pres = topHeightPres pres + bottomHeightPres pres

leftWidthPres (EmptyP _)                = 0
leftWidthPres (StringP _ str)           = 0
leftWidthPres (ImageP _ _ _)            = 0
leftWidthPres (PolyP _ _ _ _)           = 0
leftWidthPres (RowP _ rf [])            = 0
leftWidthPres (RowP _ rf press)         = sum (map widthPres (take rf press)) + leftWidthPres (index "PresUtils.leftWidthPres" press rf)
leftWidthPres (ColP _ _ press)          = maximum (0:(map leftWidthPres press))
leftWidthPres (OverlayP _ (pres:press)) = leftWidthPres pres
leftWidthPres (WithP _ pres)            = leftWidthPres pres
leftWidthPres (StructuralP _ pres)      = leftWidthPres pres
leftWidthPres (ParsingP _ pres)         = leftWidthPres pres
leftWidthPres (LocatorP _ pres)         = leftWidthPres pres
leftWidthPres pr                        = debug Err ("PresUtils.leftWidthPres: can't handle "++ show pr) 0

rightWidthPres (EmptyP _)                = 0
rightWidthPres (StringP _ str)           = length str
rightWidthPres (ImageP _ _ _)            = 1
rightWidthPres (PolyP _ _ _ _)           = 1
rightWidthPres (RowP _ rf [])            = 0
rightWidthPres (RowP _ rf press)         = rightWidthPres (index "PresUtils.leftWidthPres" press rf) + sum (map widthPres (drop (rf+1) press))
rightWidthPres (ColP _ _ press)          = maximum (0:(map rightWidthPres press))
rightWidthPres (OverlayP _ (pres:press)) = rightWidthPres pres
rightWidthPres (WithP _ pres)            = rightWidthPres pres
rightWidthPres (StructuralP _ pres)      = rightWidthPres pres
rightWidthPres (ParsingP _ pres)         = rightWidthPres pres
rightWidthPres (LocatorP _ pres)         = rightWidthPres pres
rightWidthPres pr                        = debug Err ("PresUtils.rightWidthPres: can't handle "++ show pr) 0


topHeightPres (EmptyP _)                = 0
topHeightPres (StringP _ str)           = 0
topHeightPres (ImageP _ _ _)            = 0
topHeightPres (PolyP _ _ _ _)           = 0
topHeightPres (RowP _ rf press)         = maximum (0:(map topHeightPres press))
topHeightPres (ColP _ _ [])             = 0
topHeightPres (ColP _ rf press)         = sum (map heightPres (take rf press)) + topHeightPres (index "PresUtils.topHeightPres" press rf)
topHeightPres (OverlayP _ (pres:press)) = topHeightPres pres
topHeightPres (WithP _ pres)            = topHeightPres pres
topHeightPres (StructuralP _ pres)      = topHeightPres pres
topHeightPres (ParsingP _ pres)         = topHeightPres pres
topHeightPres (LocatorP _ pres)         = topHeightPres pres
topHeightPres pr                        = debug Err ("PresUtils.topHeightPres: can't handle "++ show pr) 0


-- call bottomHeight depth? but then topHeight will be height and we need totalHeight for height
bottomHeightPres (EmptyP _)                = 0
bottomHeightPres (StringP _ str)           = 1
bottomHeightPres (ImageP _ _ _)            = 1
bottomHeightPres (PolyP _ _ _ _)           = 1
bottomHeightPres (RowP _ rf press)         = maximum (0:(map bottomHeightPres press))
bottomHeightPres (ColP _ rf [])            = 0
bottomHeightPres (ColP _ rf press)         = bottomHeightPres (index "PresUtils.bottomHeightPres" press rf) + sum (map heightPres (drop (rf+1) press))
bottomHeightPres (OverlayP _ (pres:press)) = bottomHeightPres pres
bottomHeightPres (WithP _ pres)            = bottomHeightPres pres
bottomHeightPres (StructuralP _ pres)      = bottomHeightPres pres
bottomHeightPres (ParsingP _ pres)         = bottomHeightPres pres
bottomHeightPres (LocatorP _ pres)         = bottomHeightPres pres
bottomHeightPres pr                        = debug Err ("PresUtils.bottomHeightPres: can't handle "++ show pr) 0

-- Bool is for disambiguating end of one string and start of the next. True means at start of string
pathFromXYPres (x,0,b) (EmptyP _)    = PathP [] x 
pathFromXYPres (x,0,b) (StringP _ txt)   = if x > length txt 
                                         then debug Err "PresUtils.pathFromXYPres: x>length" $ PathP [] (length txt) 
                                         else PathP [] x
pathFromXYPres (x,0,b) (ImageP _ _ _)  = PathP [] x 
pathFromXYPres (x,0,b) (PolyP _ _ _ _)   = PathP [] x 
pathFromXYPres (x,y,b) pr@(RowP id rf press) = pathFromXYRow 0 (x,y-topHeightPres pr,b) press
pathFromXYPres (x,y,b) pr@(ColP id rf press) = pathFromXYCol 0 (x-leftWidthPres pr,y,b) press
pathFromXYPres (x,y,b) (OverlayP id (pres:press))  = 0 `consPathP` pathFromXYPres (x,y,b) pres
pathFromXYPres (x,y,b) (WithP ar pres)    = 0 `consPathP` pathFromXYPres (x,y,b) pres
pathFromXYPres (x,y,b) (StructuralP id pres)  = 0 `consPathP` pathFromXYPres (x,y,b) pres
pathFromXYPres (x,y,b) (ParsingP id pres)  = 0 `consPathP` pathFromXYPres (x,y,b) pres
pathFromXYPres (x,y,b) (LocatorP l pres)  = 0 `consPathP` pathFromXYPres (x,y,b) pres
pathFromXYPres (x,y,b) pres = debug Err  ("PresUtils.pathFromXYPres: can't handle "++show (x,y)++" "++show pres) NoPathP

pathFromXYRow i (x,y,b) [] = debug Err "PresUtils.pathFromXYPres: empty row list" $ NoPathP
pathFromXYRow i (x,y,b) (pres:press) = let w = widthPres pres
                                     in if x<w then (i `consPathP` pathFromXYPres (x,y+topHeightPres pres,b) pres)
                                        else if x == w then 
                                             if b then pathFromXYRow (i+1) (x-w, y,b) press
                                                  else i `consPathP` pathFromXYPres (x,y+topHeightPres pres,b) pres
                                        else  pathFromXYRow (i+1) (x-w, y,b) press
                                      
-- FromXYCol 0 (1,1) [text "sdf", text "bloe"]
pathFromXYCol i (x,y,b) [] = debug Prs "PresUtils.pathFromXYPres: empty column list" $ NoPathP
pathFromXYCol i (x,y,b) (pres:press) = let h = heightPres pres
                                     in   if y < h  then i `consPathP` pathFromXYPres (x+leftWidthPres pres,y,b) pres
                                     else if y == h then pathFromXYCol (i+1) (x, y-h,b) press
                                                    else pathFromXYCol (i+1) (x, y-h,b) press
                                                

{-
-- get rid of everything but alternating rows and columns with correct refs
stripPres :: Presentation doc node clip -> Presentation doc node clip
stripPres pres@(StringP _ str) = pres
stripPres pres@(ImageP _ _) = pres
stripPres pres@(PolyP _ _ _) = pres
stripPres (RowP id rf press) = stripRow 0 rf [] press
stripPres (ColP id rf press) = undefined -- stripCol press
stripPres (OverlayP id (pres:press)) = stripPres pres
stripPres (WithP ar pres)    = stripPres pres
stripPres (StructuralP id pres) = stripPres pres
stripPres (ParsingP id pres)    = stripPres pres
stripPres (LocatorP l pres)     = stripPres pres
stripPres pr = debug Err ("PresUtils.stripPres: can't handle "++ show pr) pr

--stripRow :: [Presentation doc node clip] -> [Presentation doc node clip]
stripRow p rf prs []                       = RowP NoIDP rf (reverse prs) 
stripRow p rf prs (RowP id rf' press: row) = stripRow p (if p < rf then rf + length press -1 -- -1 because the row (1) is replaced by length press children 
                                                         else if p == rf then rf+rf'
                                                         else rf) prs (press ++ row)
stripRow p rf prs (pres : press )          = stripRow (p+1) rf (pres:prs) press


stripCol p rf prs []                       = ColP NoIDP rf (reverse prs) 
stripCol p rf prs (ColP id rf' press: col) = stripCol p (if p < rf then rf + length press -1 -- -1 because the col (1) is replaced by length press children 
                                                         else if p == rf then rf+rf'
                                                         else rf) prs (press ++ col)
stripCol p rf prs (pres : press )          = stripCol (p+1) rf (pres:prs) press


-}


-- this only works for simple column of rows with strings
-- tricky bit: String as result is no good, because then for exampe both (with (col []) and (with (text "")) 
-- give result "", but in a col, the first one should not produce a newline and the second one should
stringFromPres pres = concatMap (++"\n") (stringFromPres' pres)

stringFromPres' (StringP _ str)           = [str]
stringFromPres' (ImageP _ _ _)            = ["#"]
stringFromPres' (PolyP _ _ _ _)             = ["@"]
stringFromPres' (RowP _ rf press)         = [concat $ concatMap stringFromPres' press] -- will go wrong if row has cols higher than 2
stringFromPres' (ColP _ rf press)         = concatMap stringFromPres' press
stringFromPres' (OverlayP _ (pres:press)) = stringFromPres' pres
stringFromPres' (WithP _ pres)            = stringFromPres' pres
stringFromPres' (StructuralP _ pres)      = stringFromPres' pres
stringFromPres' (ParsingP _ pres)         = stringFromPres' pres
stringFromPres' (LocatorP _ pres)         = stringFromPres' pres
stringFromPres' pr                        = debug Err ("PresUtils.stringFromPres': can't handle "++ show pr) []

-- this only works for simple column of rows with strings
presFromString :: String -> Presentation doc node clip
presFromString str = ColP NoIDP 0 . map (StringP NoIDP) $ lines str



-- for navigation, overlays are characterized by their head element

-- images are not handled yet
-- these functions are horrible, can't they be a bit simpler?

pathToLeftmostLeaf (StringP _ _)        = []
pathToLeftmostLeaf (ImageP _ _ _)       = []
pathToLeftmostLeaf (PolyP _ _ _ _)      = []
pathToLeftmostLeaf (RowP _ _ press)     = 0 : pathToLeftmostLeaf (head press)
pathToLeftmostLeaf (ColP _ _ press)     = 0 : pathToLeftmostLeaf (head press)
pathToLeftmostLeaf (OverlayP _ press)   = 0 : pathToLeftmostLeaf (head press)  -- only navigate in head of overlay
pathToLeftmostLeaf (WithP _ pres)       = 0 : pathToLeftmostLeaf pres
pathToLeftmostLeaf (StructuralP _ pres) = 0 : pathToLeftmostLeaf pres
pathToLeftmostLeaf (ParsingP _ pres)    = 0 : pathToLeftmostLeaf pres
pathToLeftmostLeaf (LocatorP _ pres)    = 0 : pathToLeftmostLeaf pres
pathToLeftmostLeaf (FormatterP _ press) = 0 : pathToLeftmostLeaf (head press)
pathToLeftmostLeaf pres                 = debug Err ("PresUtils.pathToLeftmostLeaf: can't handle "++show pres) []

pathToRightmostLeaf (StringP _ _)        = []
pathToRightmostLeaf (ImageP _ _ _)       = []
pathToRightmostLeaf (PolyP _ _ _ _)        = []
pathToRightmostLeaf (RowP _ _ press)     = length press - 1 : pathToRightmostLeaf (last press)
pathToRightmostLeaf (ColP _ _ press)     = length press - 1 : pathToRightmostLeaf (last press)
pathToRightmostLeaf (OverlayP _ press)   = 0 : pathToRightmostLeaf (head press)  -- only navigate in head of overlay
pathToRightmostLeaf (WithP _ pres)       = 0 : pathToRightmostLeaf pres
pathToRightmostLeaf (StructuralP _ pres) = 0 : pathToRightmostLeaf pres
pathToRightmostLeaf (ParsingP _ pres)    = 0 : pathToRightmostLeaf pres
pathToRightmostLeaf (LocatorP _ pres)    = 0 : pathToRightmostLeaf pres
pathToRightmostLeaf (FormatterP _ press) = length press - 1  : pathToLeftmostLeaf (last press)
pathToRightmostLeaf pres                 = debug Err ("PresUtils.pathToRightmostLeaf: can't handle "++show pres) []

selectTree []       tr                        = tr
selectTree (p:path) (RowP _ _ press)          = selectTree path (index "PresUtils.selectTree" press p)
selectTree (p:path) (ColP _ _ press)          = selectTree path (index "PresUtils.selectTree" press p)
selectTree (p:path) (OverlayP _ press)        = selectTree path (index "PresUtils.selectTree" press p)
selectTree (p:path) (GraphP _ _ _ _ _ press)  = selectTree path (index "PresUtils.selectTree" press p)
selectTree (0:path) (VertexP _ _ _ _ _ pres)  = selectTree path pres
selectTree (0:path) (WithP _ pres)            = selectTree path pres
selectTree (0:path) (StructuralP _ pres)      = selectTree path pres
selectTree (0:path) (ParsingP _ pres)         = selectTree path pres
selectTree (0:path) (LocatorP _ pres)         = selectTree path pres
selectTree (p:path) (FormatterP _ press)      = selectTree path (index "PresUtils.selectTree" press p)
selectTree pth      pres                      = debug Err ("PresUtils.selectTree: can't handle "++show pth++" "++show pres) (StringP NoIDP "unselectable")


pathsToAncestorRightSiblings _    []       _                     = []
pathsToAncestorRightSiblings _    (p:path) (StringP _ _)         = [] 
pathsToAncestorRightSiblings root (p:path) (RowP _ _ press)      = pathsToAncestorRightSiblings (root++[p]) path (index "PresUtils.pathsToAncestorRightSiblings" press p)
                                                              ++ (if p < length press - 1 then [root++[p+1]] else [])
pathsToAncestorRightSiblings root (p:path) (ColP _ _ press)      = pathsToAncestorRightSiblings (root++[p]) path (index "PresUtils.pathsToAncestorRightSiblings" press p)
                                                              ++ (if p < length press - 1 then [root++[p+1]] else [])
pathsToAncestorRightSiblings root (0:path) (OverlayP _ (pres:_)) = pathsToAncestorRightSiblings (root++[0]) path pres
pathsToAncestorRightSiblings root (p:path) (StructuralP _ pres)  = pathsToAncestorRightSiblings (root++[p]) path pres
pathsToAncestorRightSiblings root (p:path) (ParsingP _ pres)     = pathsToAncestorRightSiblings (root++[p]) path pres
pathsToAncestorRightSiblings root (p:path) (LocatorP _ pres)     = pathsToAncestorRightSiblings (root++[p]) path pres
pathsToAncestorRightSiblings root (p:path) (WithP _ pres)        = pathsToAncestorRightSiblings (root++[p]) path pres
pathsToAncestorRightSiblings root (p:path) (FormatterP _ press)  = pathsToAncestorRightSiblings (root++[p]) path (index "PresUtils.pathsToAncestorRightSiblings" press p)
                                                              ++ (if p < length press - 1 then [root++[p+1]] else [])
pathsToAncestorRightSiblings root pth      pres                  = debug Err ("PresUtils.pathsToAncestorRightSiblings: can't handle "++show root++" "++show pth++" "++show pres) []


pathsToAncestorLeftSiblings _    []       _                     = []
pathsToAncestorLeftSiblings _    (p:path) (StringP _ _)         = [] 
pathsToAncestorLeftSiblings root (p:path) (RowP _ _ press)      = pathsToAncestorLeftSiblings (root++[p]) path (index "PresUtils.pathsToAncestorLeftSiblings" press p)
                                                             ++ (if p > 0 then [root++[p-1]] else [])
pathsToAncestorLeftSiblings root (p:path) (ColP _ _ press)      = pathsToAncestorLeftSiblings (root++[p]) path (index "PresUtils.pathsToAncestorLeftSiblings" press p)
                                                             ++ (if p > 0 then [root++[p-1]] else [])
pathsToAncestorLeftSiblings root (0:path) (OverlayP _ (pres:_)) = pathsToAncestorLeftSiblings (root++[0]) path pres
pathsToAncestorLeftSiblings root (p:path) (StructuralP _ pres)  = pathsToAncestorLeftSiblings (root++[p]) path pres 
pathsToAncestorLeftSiblings root (p:path) (ParsingP _ pres)     = pathsToAncestorLeftSiblings (root++[p]) path pres 
pathsToAncestorLeftSiblings root (p:path) (LocatorP _ pres)     = pathsToAncestorLeftSiblings (root++[p]) path pres 
pathsToAncestorLeftSiblings root (p:path) (WithP _ pres)        = pathsToAncestorLeftSiblings (root++[p]) path pres 
pathsToAncestorLeftSiblings root (p:path) (FormatterP _ press)  = pathsToAncestorLeftSiblings (root++[p]) path (index "PresUtils.pathsToAncestorLeftSiblings" press p)
                                                             ++ (if p > 0 then [root++[p-1]] else [])
pathsToAncestorLeftSiblings root pth      pres                  = debug Err ("PresUtils.pathsToAncestorLeftSiblings: can't handle "++show root++" "++show pth++" "++show pres) []



pathToNearestAncestorRightSibling path pres = let rss = (pathsToAncestorRightSiblings [] path pres)
                                              in  if null rss then Nothing else Just $ head rss

pathToNearestAncestorLeftSibling path pres = let lss = (pathsToAncestorLeftSiblings [] path pres)
                                             in  if null lss then Nothing else Just $ head lss

leafLength (StringP _ str) = length str
leafLength pres = debug Err ("PresUtils.leafLength: non string leaf"++show pres) 0

-- tricky, in one row [text "12|", text "34"] right navigation must be row [text "12", text "3|4"]
--         but if there is a column, we want col [text "12", text "|34"]
rightNavigatePath (PathP path offset) pres = 
  if offset < leafLength (selectTree path pres) 
  then PathP path (offset+1)
  else case showDebug Prs $ pathToNearestAncestorRightSibling path pres of
         Nothing              -> PathP path offset
         Just siblingTreePath -> let leafPath = siblingTreePath ++ pathToLeftmostLeaf (selectTree siblingTreePath pres)
                                     pathP = PathP leafPath 0
                                 in  -- pathP
{- If we pass through a column, we do want to navigate to
   the empty, otherwise we skip empty lines. Also apart from skipping empties, in a string we should go to 1 instead o 0
   Also, if these things are implemented, we still need a way to do the fine grained navigation
   Same issues apply to leftNavigatePath
   
   eg row [ col [ ...,"..|"], col ["..",..]] gives [ col [ ...,".."], col ["|..",..]]
   for rows with singleton colums this may give an unexpected result, but in order to change
   that a more sophisticated navigation is required.
-}
                                     if passedColumn path leafPath pres
                                     then pathP
                                     else -- no column encountered, so if leaf is empty go to next one
                                          if leafLength (selectTree leafPath pres) == 0 
                                          then rightNavigatePath pathP pres
                                          else PathP leafPath 1 -- and skip first pos

leftNavigatePath  (PathP path offset) pres = 
  if offset > 0 
  then PathP path (offset-1)
  else case pathToNearestAncestorLeftSibling path pres of
         Nothing              -> PathP path offset
         Just siblingTreePath -> let leafPath = siblingTreePath ++ pathToRightmostLeaf (selectTree siblingTreePath pres)
                                     leafLn = (leafLength (selectTree leafPath pres)) 
                                     pathP = PathP leafPath leafLn
                                 in  -- pathP
                                     if passedColumn path leafPath pres
                                     then pathP
                                     else -- no column encountered, so if leaf is empty go to previous one
                                     if leafLn == 0 
                                     then leftNavigatePath pathP pres
                                     else PathP leafPath (leafLn -1 ) -- and skip last pos

-- passedColumn returns true if a column is present from the common prefix of fromPath and toPath to
-- fromPath and toPath. Meaning that if a column is encountered if we move from fromPath to 
-- toPath in the shortest way then True is returned.
passedColumn :: [Int] -> [Int] -> Presentation doc node clip -> Bool
passedColumn fromPath toPath pres =
  let commonPath = commonPrefix fromPath toPath
      commonTree = selectTree commonPath pres
      commonToFrom = drop (length commonPath) fromPath
      commonToTo   = drop (length commonPath) toPath
  in -- debug Err (   show commonTree ++"\n"
     --            ++ show (selectTree fromPath pres) ++"\n"
     --            ++ show (selectTree toPath pres) ++"\n"
     --            ++ show commonToFrom ++"\n"++ show commonToTo) $
      containsColPres commonToFrom commonTree || containsColPres commonToTo commonTree
  
-- return True if any node on the path, including the root, is a column
containsColPres _        (StringP id str)           = False
containsColPres _        (ImageP _ _ _)             = False
containsColPres _        (PolyP _ _ _ _)            = False
containsColPres (p:path) (RowP id rf press)         = containsColPres path (index "PresUtils.containsColPres" press p)
containsColPres (p:path) (ColP id rf press)         = True
containsColPres (0:path) (OverlayP id press@(pres:_)) = containsColPres path (index "PresUtils.containsColPres" press 0)     
containsColPres (p:path) (WithP ar pres)            = containsColPres path pres
containsColPres (p:path) (StructuralP id pres)      = containsColPres path pres
containsColPres (p:path) (ParsingP id pres)         = containsColPres path pres
containsColPres (p:path) (LocatorP _ pres)          = containsColPres path pres
containsColPres (p:path) (FormatterP id press)      = containsColPres path (index "PresUtils.containsColPres" press p)
containsColPres pth      pr                         = debug Err ("*** PresUtils.containsColPres: can't handle "++show pth++" "++ show pr++"***") False

-- | Return True if the focus is on either a vertex, or an edge (focus on graph, index larger than nr of vertices)
focusIsOnGraph :: FocusPres -> Presentation doc node clip -> Bool
focusIsOnGraph (FocusP (PathP path _) _) pres = focusIsOnGraphPres path pres
focusIsOnGraph _ _         = False

focusIsOnGraphPres :: [Int] -> Presentation doc node clip -> Bool
focusIsOnGraphPres []       (VertexP _ _ _ _ _ _)     = True 
focusIsOnGraphPres [p]      (GraphP _ _ _ _ _ press)  = if p >= length press then True else False
focusIsOnGraphPres []        tr                       = False
focusIsOnGraphPres (p:path) (RowP _ _ press)          = focusIsOnGraphPres path (index "PresUtils.focusIsOnGraphPres" press p)
focusIsOnGraphPres (p:path) (ColP _ _ press)          = focusIsOnGraphPres path (index "PresUtils.focusIsOnGraphPres" press p)
focusIsOnGraphPres (0:path) (OverlayP _ (pres:press)) = focusIsOnGraphPres path pres
focusIsOnGraphPres (p:path) (GraphP _ _ _ _ _ press)  = focusIsOnGraphPres path (index "PresUtils.focusIsOnGraphPres" press p)
focusIsOnGraphPres (0:path) (VertexP _ _ _ _ _ pres)  = focusIsOnGraphPres path pres
focusIsOnGraphPres (0:path) (WithP _ pres)            = focusIsOnGraphPres path pres
focusIsOnGraphPres (0:path) (StructuralP _ pres)      = focusIsOnGraphPres path pres
focusIsOnGraphPres (0:path) (ParsingP _ pres)         = focusIsOnGraphPres path pres
focusIsOnGraphPres (0:path) (LocatorP _ pres)         = focusIsOnGraphPres path pres
focusIsOnGraphPres (p:path) (FormatterP _ press)      = focusIsOnGraphPres path (index "PresUtils.focusIsOnGraphPres" press p)
focusIsOnGraphPres pth      pres                      = debug Err ("PresUtils.focusIsOnGraph: can't handle "++show pth++" "++show pres) False



-- VVV HACK VVV              will be handled more generally in the future
-- | Collect the bottom-most mouseDown update function that is added by WithP nodes on path in pres 
mouseDownDocPres :: [Int] -> Presentation doc node clip -> Maybe (UpdateDoc doc clip)
mouseDownDocPres = mouseDownDocPres' Nothing

mouseDownDocPres' :: Maybe (UpdateDoc doc clip) -> [Int] -> Presentation doc node clip -> Maybe (UpdateDoc doc clip)
mouseDownDocPres' upd []       tr                        = upd
mouseDownDocPres' upd (p:path) (RowP _ _ press)          = mouseDownDocPres' upd path (index "PresUtils.mouseDownDocPres'" press p)
mouseDownDocPres' upd (p:path) (ColP _ _ press)          = mouseDownDocPres' upd path (index "PresUtils.mouseDownDocPres'" press p)
mouseDownDocPres' upd (0:path) (OverlayP _ press@(pres:_)) = mouseDownDocPres' upd path pres --(last press)
mouseDownDocPres' upd (p:path) (GraphP _ _ _ _ _ press)  = mouseDownDocPres' upd path (index "PresUtils.mouseDownDocPres'" press p)
mouseDownDocPres' upd (p:path) (VertexP _ _ _ _ _ pres)  = mouseDownDocPres' upd path pres
mouseDownDocPres' upd (p:path) (WithP w pres)            = mouseDownDocPres' (let (inh,syn)   = ((fst emptyAttrs) {mouseDown = upd}, snd emptyAttrs)
                                                                                  (inh',syn') = w (inh,syn)
                                                                              in mouseDown inh') path pres
mouseDownDocPres' upd (p:path) (StructuralP _ pres)      = mouseDownDocPres' upd path pres
mouseDownDocPres' upd (p:path) (ParsingP _ pres)         = mouseDownDocPres' upd path pres
mouseDownDocPres' upd (p:path) (LocatorP _ pres)         = mouseDownDocPres' upd path pres
mouseDownDocPres' upd (p:path) (FormatterP _ press)      = mouseDownDocPres' upd path (index "PresUtils.mouseDownDocPres'" press p)
mouseDownDocPres' upd pth      pres                      = debug Err ("PresTypes.mouseDownDocPres: can't handle "++show pth++" "++show pres) Nothing


-- | Collect all popupMenuItems that are added by WithP nodes on path in pres 
popupMenuItemsPres :: [Int] -> Presentation doc node clip -> [PopupMenuItem doc clip]
popupMenuItemsPres path pres = popupMenuItemsPres' [] path pres

popupMenuItemsPres' :: [PopupMenuItem doc clip] -> [Int] -> Presentation doc node clip -> [PopupMenuItem doc clip]
popupMenuItemsPres' its []       tr                        = its
popupMenuItemsPres' its (p:path) (RowP _ _ press)          = popupMenuItemsPres' its path (index "PresUtils.popupMenuItemsPres'" press p)
popupMenuItemsPres' its (p:path) (ColP _ _ press)          = popupMenuItemsPres' its path (index "PresUtils.popupMenuItemsPres'" press p)
popupMenuItemsPres' its (0:path) (OverlayP _ press@(pres:_)) = popupMenuItemsPres' its path pres --(last press)
popupMenuItemsPres' its (p:path) (GraphP _ _ _ _ _ press)  = popupMenuItemsPres' its path (index "PresUtils.popupMenuItemsPres'" press p)
popupMenuItemsPres' its (p:path) (VertexP _ _ _ _ _ pres)  = popupMenuItemsPres' its path pres
popupMenuItemsPres' its (p:path) (WithP w pres)            = popupMenuItemsPres' (let (inh,syn)   = ((fst emptyAttrs) {popupMenuItems = its}, snd emptyAttrs)
                                                                                      (inh',syn') = w (inh,syn)
                                                                                  in popupMenuItems inh') path pres
popupMenuItemsPres' its (p:path) (StructuralP _ pres)      = popupMenuItemsPres' its path pres
popupMenuItemsPres' its (p:path) (ParsingP _ pres)         = popupMenuItemsPres' its path pres
popupMenuItemsPres' its (p:path) (LocatorP _ pres)         = popupMenuItemsPres' its path pres
popupMenuItemsPres' its (p:path) (FormatterP _ press)      = popupMenuItemsPres' its path (index "PresUtils.popupMenuItemsPres'" press p)
popupMenuItemsPres' its pth      pres                      = debug Err ("PresTypes.popupMenuItemsPres: can't handle "++show pth++" "++show pres) []



-- ^^^ HACK ^^^







-- test xprez values


prez = prez2' --fontChars
prez1 = row [text "bla", img "img/squiggly.bmp" `withSize` (20,3),img "img/squiggly.bmp" `withSize` (20,3) ,text "Xxx",overlay [text "vout", img "img/squiggly.bmp" `withHeight` 3 `withColor` red], text "X"] `withHStretch` False
prez2 =                             col [text "12345"
    ,  structural $ bold $ rowR 1 [text "abcd",  rowR 1 [ text "ref", vLineW 2,text "ref", strikeOut $ text "ref"], text "noppes", text "bla"],
                rowR 2 [text "ab",   text "cd", underline $ text "ref", text "noppes", text "bla"]
                                            , text "678Y9"]
prez2' = structural $                             col [text "12345"
    , parsing $ rowR 1 [text "abcd", structural $ rowR 1 [italic $ text "ref", vLineW 2,text "ref", strikeOut $ text "ref"], text "noppes", text "bla"],
                rowR 2 [text "ab", parsing $ bold $ text "cd", underline $ text "ref", text "noppes", text "bla"]
                                            , text "678Y9"]

-- why the big speed difference between prez2 and prez2'?

-- BUG sometimes index > length while cutting in prez2

prez5 = row [col [text "dasdasf", text "er"], col [text "dasdasf", text "er", text "fsfd"], col [text "gdsfgsdgf"]]

prez3 = (row [ StringP (IDP 1) "123Y45", {-loc [1] $ -} col[StringP (IDP 3) "blaaa", row [text "x",col [text "11", text "22"] `withbgColor` grey ,text "y"]] `withbgColor` yellow,text "blabla"
           , colR 1 [text "een",{- loc [2] $-} text"bloe", text "brr"]
          
           ,row [text "12345" `withFontSize` 10,text "x" `withFontSize` 3, text "xbloe"]
           ] `withbgColor` (240,240,240) `withFont'` ("courier",12))

prez4 = (col $ map text prez4Strs) `withFont'` ("courier new",8)
prez6 = col [text "a", row [] `withVStretch` False , text "v"]
fontChars = (col $ [ row $ text (show i++": ")  `withFontFam` "Arial": concat [ [text [chr j], text "," `withFontFam` "Arial"]  |j <- [i..i+15]] |  i <- [0,16..240]])
            `withFont'` ("arial", 20)
prez4Strs =
 [ "main =                                                               -- initial local state should not be here"
 , " do { let TransStep translate =           lift (wrap presentationLayer) (EmptyP NoIDP, error \"Empty document\")"
 , "                                `combine` lift (wrap arrangementLayer) ((),EmptyP NoIDP)"
 , "                                `combine` lift (wrap renderingLayer) (NoFocusA, EmptyA NoIDP) -- initLocalRendererState"
 , "    ; let doc = RootD NoID"
 , ""
 , "    ; stepRf <- newIORef (translate, error \"empty rendering\")"
 , "    "
 , "    ; let handler event =  -- tricky bit, assigment of rendering to local state must be same as in layers"
 , "           do { liftIO  (putStrLn $ show event)                           -- seems ok now"
 , ""
 , "              ; (translate, rendering) <- liftIO $ readIORef stepRf"
 , "              ; ((doc, docEdit), PresStep present) <- translate (rendering, event)"
 , "              "
 , "              ; liftIO  (putStrLn $ show docEdit)"
 , "              ; (rendering', translate') <-"
 , "                  do { ((rendering', renderingEdit') , TransStep translate') <- present (doc, (redirect docEdit))"
 , "                     ; case renderingEdit' of "
 , "                              SetRen' rendering' -> return (rendering', translate')"
 , "                              SkipRen' _         -> return (rendering, translate')"
 , "                     }"
 , "              ; liftIO $ writeIORef stepRf (translate', rendering')"
 , "              ; return rendering'"
 , "              }"
 , ""
 , "    ; startGUI handler"
 , "    }"
 ]


{-TODO:

pathFromXYPres



up and down goes wrong will go wrong in more complex press than col [row, row, ...]
maybe find outermost enclosing row, and take its x value - 1 
     COL
sjkdlCOLjdf
     COL

seems to go right 



delete is crappy + focus is sometimes incorrect
and focus moves get stuck between rows

-}


