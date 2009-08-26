module Layout.LayUtils (module Presentation.PresUtils, module Layout.LayUtils) where

import Presentation.PresUtils
import Layout.LayLayerTypes

import Common.CommonTypes
import Common.CommonUtils

{-

go from Layout with two paths to Maybe Document path

first take common prefix of paths and move upward to nearest surrounding parsing column of rows 
select column of rows

Issues:

Does not work for structural presentations yet. (this includes the numerator/denominator of a fraction)


Problem: 
When a NavPathDocPres is returned, the layout (with the extended focus) is not scanned.
Hence, the whitespace map does not contain the right focus.
-}

pathDocFromFocusPres (FocusP pth1@(PathP _ _) pth2@(PathP _ _)) layout = pathDocFromPathPres pth1 pth2 layout
pathDocFromFocusPres _                                      layout = NoPathD
-- only a two path pres focus can give rise to a document focus

                   

pathDocFromPathPres pth1 pth2 layout =
  case split pth1 pth2 layout of
    Nothing -> NoPathD
    Just (prefix, selection, suffix) -> 
      let selectionWithoutLeadingWS = dropLeadingWhitespace selection
          selectionWithoutSurroundingWS = reverse $ dropLeadingWhitespace $ reverse selectionWithoutLeadingWS
          selectionPaths = map gatherDocPaths selectionWithoutSurroundingWS
          sharedPaths = getSharedPaths selectionPaths
          leftBoundPaths = case dropLeadingWhitespace (reverse prefix) of
                              x:_ -> gatherDocPaths x
                              _   -> []
          sharedPaths'  = sharedPaths \\ leftBoundPaths -- remove paths appearing before
          rightBoundPaths = case dropLeadingWhitespace suffix of
                              x:_ -> gatherDocPaths x
                              _   -> []
          sharedPaths'' =  sharedPaths' \\ rightBoundPaths -- remove paths appearing after
      in  --debug Lay ("SelectionPathss:\n"++showPathss selectionPaths) $
          --debug Lay ("SharedPaths:"++show sharedPaths) $
          --debug Lay ("LeftBoundPaths:"++show leftBoundPaths) $
          --debug Lay ("RightBoundPaths:"++show rightBoundPaths) $
          case sort $ zip [0..] sharedPaths'' of  -- return the shortest, if any
          (_,path):_ -> debug Lay ("Shortest:"++show path) $
                          path
          []         -> NoPathD
 where showPathss pathss = unlines $ map show pathss

getSharedPaths :: [[PathDoc]] -> [PathDoc]
getSharedPaths [] = []
getSharedPaths [paths] = paths
getSharedPaths (paths:pathss) = paths `intersect` getSharedPaths pathss

split (PathP pth1 i1) (PathP pth2 i2) layout =
 do { let commonPrefix = map fst $ takeWhile (uncurry (==)) $ zip pth1 pth2
    ; (column,columnPath) <- nearestParsingColumn commonPrefix layout
    ; let rows = getColumnRows column
    ; (pos1,offsetPath1) <- getColPosAndOffsetPath columnPath pth1
    ; (pos2,offsetPath2) <- getColPosAndOffsetPath columnPath pth2
        
    ; let ((start,startOffset),(end,endOffset)) = 
            if pos1 <= pos2 then ((pos1,offsetPath1++[i1]),(pos2,offsetPath2++[i2])) 
                            else ((pos2,offsetPath2++[i2]),(pos1,offsetPath1++[i1]))
          

    ; (prefix, selection, suffix) <- splitNaive start end rows
    ; (prefix',selection') <- fixLeft startOffset prefix selection
    ; (selection'', suffix') <- fixRight endOffset selection' suffix
    ; return  (prefix', selection'', suffix')
    }

getColPosAndOffsetPath columnPath path = 
  case drop (length columnPath) path of
    (r:c:offset) -> Just ((r,c), offset)
    _            -> Nothing
    
fixLeft path prefix (left:selection) =
  fixWS path prefix left selection
fixLeft path prefix _ = Nothing
  
fixRight path selection suffix =
  if null selection 
  then Nothing
  else fixWS path (init selection) (last selection) suffix
  
fixWS path left elt right =
  --debug Lay ("Twiddling "++show path ++ ":\n"++show elt) $
  case isWhitespaceLR path elt of
    (False,False) -> Nothing
    (False,True ) -> Just (left++[elt],right)
    (True, False) -> Just (left,elt:right)
    (True, True ) ->   Just (left,right)
     

-- the middle part includes the layouts that contained the focus
splitNaive start end rows =
  if checkBounds start
  then debug Err "LayUtils.splitNaive: Start position out of bounds" Nothing
  else if checkBounds end
  then debug Err "LayUtils.splitNaive: End position out of bounds" Nothing
  else
    let (prefixAndMiddle, suffix) = splitAfter end rows
        (prefix, middle)          = splitBefore start prefixAndMiddle
    in  Just (concat prefix, concat middle, concat suffix)
    where checkBounds (r,c) = r >= length rows || c >= length (rows!!r)
          splitAfter  (r,c) rs = let (leadingRows,(row:followingRows)) = splitAt r rs
                                     (left,x:right) = splitAt c row
                                 in  (leadingRows++[left++[x]],right:followingRows)
          splitBefore (r,c) rs = let (leadingRows,(row:followingRows)) = splitAt r rs
                                     (left,right) = splitAt c row
                                 in  (leadingRows++[left],right:followingRows)

-- PRECONDITION: nested ParsingP may not be ParsingP (ColP ..)
nearestParsingColumn :: (Show node, Show token) => Path -> Layout doc enr node clip token ->
                        Maybe (Layout doc enr node clip token, Path)
nearestParsingColumn path layout = nearestParsingColumn' [] Nothing path layout

nearestParsingColumn' colPth nearest [] _ = nearest
nearestParsingColumn' colPth nearest (0:pth) (ParsingP _ _ _ column@(ColP _ _ _ _))  = 
  nearestParsingColumn' (colPth++[0]) (Just (column, colPth++[0])) pth column
nearestParsingColumn' colPth nearest (p:pth) (RowP _ _ lays)        = nearestParsingColumn' (colPth++[p]) nearest pth (index "LayUtils.nearestParsingColumn'" lays p)
nearestParsingColumn' colPth nearest (p:pth) (ColP _ _ _ lays)      = nearestParsingColumn' (colPth++[p]) nearest pth (index "LayUtils.nearestParsingColumn'" lays p)
nearestParsingColumn' colPth nearest (p:pth) (FormatterP _ lays)    = nearestParsingColumn' (colPth++[p]) nearest pth (index "LayUtils.nearestParsingColumn'" lays p)
nearestParsingColumn' colPth nearest (0:pth) (OverlayP _ _ (lay:_)) = nearestParsingColumn' (colPth++[0]) nearest pth lay
nearestParsingColumn' colPth nearest (0:pth) (WithP _ lay)          = nearestParsingColumn' (colPth++[0]) nearest pth lay
nearestParsingColumn' colPth nearest (0:pth) (StructuralP _ lay)    = nearestParsingColumn' (colPth++[0]) nearest pth lay
nearestParsingColumn' colPth nearest (0:pth) (ParsingP _ _ _ lay)   = nearestParsingColumn' (colPth++[0]) nearest pth lay
nearestParsingColumn' colPth nearest (0:pth) (LocatorP _ lay)       = nearestParsingColumn' (colPth++[0]) nearest pth lay
nearestParsingColumn' colPth nearest (0:pth) (TagP _ lay)       = nearestParsingColumn' (colPth++[0]) nearest pth lay
nearestParsingColumn' colPth nearest pth lay                        = debug Err ("LayUtils.nearestParsingColumn': can't handle "++show pth++" "++ show lay) $ Nothing
                                                  


getColumnRows :: (Show node, Show token) => 
                 Layout doc enr node clip token -> [[Layout doc enr node clip token]]
getColumnRows (ColP _ _ _ lays) = map getRowElts lays
 where getRowElts (RowP _ _ lays) = lays
       getRowElts lay = debug Err ("LayUtils.getColumnRows: not a row: "++ show lay) []
getColumnRows lay = debug Err ("LayUtils.getColumnRows: not a column: "++ show lay) []

gatherDocPaths :: (DocNode node, Show token) => Layout doc enr node clip token -> [PathDoc]
gatherDocPaths lay = gatherDocPaths' [] lay

gatherDocPaths' paths (EmptyP _)                = paths
gatherDocPaths' paths (StringP _ str)           = paths
gatherDocPaths' paths (ImageP _ _ _)            = paths
gatherDocPaths' paths (PolyP _ _ _ _)           = paths
gatherDocPaths' paths (OverlayP _ _ (lay:_))    = gatherDocPaths' paths lay
gatherDocPaths' paths (WithP _ lay)             = gatherDocPaths' paths lay
gatherDocPaths' paths (StructuralP _ lay)       = paths
gatherDocPaths' paths (ParsingP _ _ _ lay)      = gatherDocPaths' paths lay
gatherDocPaths' paths (LocatorP node lay)       = gatherDocPaths' (pathNode node:paths) lay
gatherDocPaths' paths (TagP _ lay)              = gatherDocPaths' paths lay
gatherDocPaths' paths lay                       = debug Err ("LayUtils.gatherDocPaths: can't handle "++ show lay) paths

dropLeadingWhitespace lays = dropWhile isWhitespace lays

isWhitespace :: (Show node, Show token) => Layout doc enr node clip token -> Bool
isWhitespace (EmptyP _)                = True
isWhitespace (EmptyP _)                = True
isWhitespace (StringP _ str)           = all isSpace str
isWhitespace (RowP _ _ lays)           = all isWhitespace lays
isWhitespace (ColP _ _ _ lays)         = all isWhitespace lays
isWhitespace (FormatterP _ lays)         = all isWhitespace lays
isWhitespace (OverlayP _ _ (lay:_))    = isWhitespace lay
isWhitespace (WithP _ lay)             = isWhitespace lay
isWhitespace (StructuralP _ lay)       = isWhitespace lay
isWhitespace (ParsingP _ _ _ lay)      = isWhitespace lay
isWhitespace (LocatorP _ lay)          = isWhitespace lay
isWhitespace (TagP _ lay)          = isWhitespace lay
isWhitespace lay                        = debug Err ("LayUtils.isWhitespace: can't handle "++ show lay) False

isWhitespaceLR :: (Show node, Show token) => Path -> Layout doc enr node clip token -> (Bool,Bool)
isWhitespaceLR [] (EmptyP _)             = (True,True)
isWhitespaceLR [p] (StringP _ str)       = let (left,right) = splitAt p str
                                           in  (all isSpace left, all isSpace right)
isWhitespaceLR [] (ImageP _ _ _)         = (False,False)
isWhitespaceLR [] (PolyP _ _ _ _)        = (False,False)
isWhitespaceLR (p:pth) (RowP _ _ lays)   = let (l,r) = isWhitespaceLR pth (index "isWhitespaceLR" lays p)
                                               (ls,rs) = splitAt p lays  
                                           in  ( all isWhitespace ls && l
                                               , all isWhitespace rs && r )
isWhitespaceLR (p:pth) (ColP _ _ _ lays) = let (l,r) = isWhitespaceLR pth (index "isWhitespaceLR" lays p)
                                               (ls,rs) = splitAt p lays  
                                           in  ( all isWhitespace ls && l
                                               , all isWhitespace rs && r )
isWhitespaceLR (0:pth) (OverlayP _ _ (lay:_)) = isWhitespaceLR pth lay
isWhitespaceLR (0:pth) (WithP _ lay)          = isWhitespaceLR pth lay
isWhitespaceLR (0:pth) (StructuralP _ lay)    = isWhitespaceLR pth lay
isWhitespaceLR (0:pth) (ParsingP _ _ _ lay)   = isWhitespaceLR pth lay
isWhitespaceLR (0:pth) (LocatorP _ lay)       = isWhitespaceLR pth lay
isWhitespaceLR (0:pth) (TagP _ lay)       = isWhitespaceLR pth lay
isWhitespaceLR pth     lay                    = debug Err ("LayUtils.isWhitespaceLR: can't handle "++ show pth++" " ++show lay) (False,False)

-- TODO can be improved by stopping when begin point is reached
--      and by alert when not found
-- Bug: guaranteefocusinview fails if focus is in unarranged
-- Bug: control gets stuck after find
findLay (FocusP from@(PathP _ _) to@(PathP _ _)) str lay = let PathP pth i = from `min` to
                                                           in  firstJust [ findLay' str pth i [maxBound] 0 [] lay, findLay' str [] 0 pth i [] lay ]
findLay (FocusP from@(PathP pth i) _)            str lay = firstJust [ findLay' str pth i [maxBound] 0 [] lay, findLay' str [] 0 pth i [] lay ]
findLay _ str lay = findLay' str [] 0 [maxBound] 0 [] lay
-- [maxBound] 0 is greater than any valid path, so we search until the end of the presentation


findLay' :: (Show node, Show token) => String -> Path -> Int -> Path -> Int -> Path -> Layout doc enr node clip token -> Maybe FocusPres
findLay' str fromPath fromIndex toPath toIndex rootPath lay =
  --debug Prs ("paths "++show fromPath++show toPath) $
  if rootPath < take (length rootPath) fromPath || rootPath > toPath
  then Nothing
  else -- this does not take into account focus selections of subtrees (but these aren't valid anyway) 
   case lay of
    (StringP id str')         -> let searchStr = if rootPath == toPath then take toIndex str' else str'
                                     searchStart = if rootPath == fromPath then fromIndex+1 else 0
                                 in  case (drop searchStart searchStr) `containsStr` str of 
                                       Just pos -> Just $ FocusP (PathP rootPath $ searchStart + pos) 
                                                                 (PathP rootPath $ searchStart + pos + length str)
                                       Nothing -> Nothing
    (RowP id rf press)        -> firstJust $ [ findLay' str fromPath fromIndex toPath toIndex (rootPath ++ [p]) pres | (p, pres) <- zip [0..] press]
    (ColP id rf f press)      -> firstJust $ [ findLay' str fromPath fromIndex toPath toIndex (rootPath ++ [p]) pres | (p, pres) <- zip [0..] press]
    (OverlayP d id press)     -> firstJust $ [ findLay' str fromPath fromIndex toPath toIndex (rootPath ++ [p]) pres | (p, pres) <- zip [0..] press]
    (FormatterP  id press)    -> firstJust $ [ findLay' str fromPath fromIndex toPath toIndex (rootPath ++ [p]) pres | (p, pres) <- zip [0..] press]
    (GraphP id _ _ _ _ press) -> firstJust $ [ findLay' str fromPath fromIndex toPath toIndex (rootPath ++ [p]) pres | (p, pres) <- zip [0..] press]
    (VertexP _ _ x y _  pres) -> findLay' str fromPath fromIndex toPath toIndex (rootPath ++ [0]) pres
    (WithP ar pres)           -> findLay' str fromPath fromIndex toPath toIndex (rootPath ++ [0]) pres
    (StructuralP id pres)     -> findLay' str fromPath fromIndex toPath toIndex (rootPath ++ [0]) pres
    (ParsingP id p l pres)    -> findLay' str fromPath fromIndex toPath toIndex (rootPath ++ [0]) pres
    (LocatorP loc pres)       -> findLay' str fromPath fromIndex toPath toIndex (rootPath ++ [0]) pres
    (TagP loc pres)           -> findLay' str fromPath fromIndex toPath toIndex (rootPath ++ [0]) pres
    _                         -> Nothing -- all atomic presentations other than string

containsStr str substr = contains' 0 str substr
 where contains' pos []           _      = Nothing
       contains' pos str@(_:rest) substr = if substr `isPrefixOf` str 
                                           then Just pos 
                                           else contains' (pos+1) rest substr 

{-
findLay :: (Show node, Show token) => PresentationBase doc enr node clip token level -> FocusPres
findLay (EmptyP id)               = 
findLay (StringP id str)          = 
findLay (TokenP id t)             = 
findLay (ImageP id str _)         = 
findLay (PolyP id _ _ _)          = 
findLay (RectangleP id _ _ _ _)   = 
findLay (EllipseP id _ _ _ _)     = 
findLay (RowP id rf press)        = 
findLay (ColP id rf f press)      = 
findLay (OverlayP d id press)     = 
findLay (FormatterP  id press)    = 
findLay (GraphP id _ _ _ _ press) =
findLay (VertexP _ _ x y _  pres) =
findLay (WithP ar pres)           =
findLay (StructuralP id pres)     =
findLay (ParsingP id p l pres)    =
findLay (LocatorP loc pres)       =
findLay (TagP loc pres)           =
-}