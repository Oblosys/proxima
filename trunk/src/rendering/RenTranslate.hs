module RenTranslate where

import CommonTypes
import RenLayerTypes
import RenLayerUtils

import Char
import IOExts

translateIO state low high editLow = return $ interpret state low high editLow

translate state low high editLow = 
  let (editHigh, state', low') = interpret state low high editLow
  in (editHigh, state', low')



-- descaling seems a bit messy, maybe we should do it before processing the edit command

-- updating rendering does not seem to make sense
-- apply key remapper first

-- edit ops are recognized here because they need the focus. When focus is handled differently, the
-- edit ops might be recognized elsewhere in the future
-- focus is passed all the time, which is a hint that the current focus model is no good
-- also focus behaviour (e.g. what to do when navigating with a nonempty focus) is split over layers




interpret :: Show node => LocalStateRen -> RenderingLevel documentLevel -> ArrangementLevel node -> EditRendering documentLevel -> (EditArrangement documentLevel, LocalStateRen, RenderingLevel documentLevel)
interpret state renLvl@(RenderingLevel scale c r sz debugging ur)
                arrLvl@(ArrangementLevel arr focus _) editRen =
  case editRen of
    InitRen             -> (InitArr,       state, renLvl) 
    CloseRen            -> (CloseArr,      state, renLvl)
    SkipRen i           -> (SkipArr (i+1), state, renLvl)
    KeyCharRen '\ETX'   -> (CopyArr,       state, renLvl) -- Ctrl-c
    KeyCharRen '\SYN'   -> (PasteArr,      state, renLvl) -- Ctrl-v
    KeyCharRen '\CAN'   -> (CutArr,        state, renLvl) -- Ctrl-x
    KeyCharRen '\ACK'   -> (CopyDocArr,    state, renLvl) -- Ctrl-f
    KeyCharRen '\a'     -> (PasteDocArr,   state, renLvl) -- Ctrl-g
    KeyCharRen '\EOT'   -> (CutDocArr,     state, renLvl) -- Ctrl-d
    KeyCharRen '\DC4'   -> (TestArr,       state, renLvl) -- Ctrl-t
-- TODO: make selectors scaleR and debuggingR for RenderingLevel
    KeySpecialRen UpKey   (Modifiers False False True) -> setUpd AllUpdated $ (SkipArr 0, state, RenderingLevel (scale*2) c r sz debugging ur)
    KeySpecialRen DownKey (Modifiers False False True) -> setUpd AllUpdated $ (SkipArr 0, state, RenderingLevel (scale/2) c r sz debugging ur)
    KeySpecialRen F9Key ms                             -> setUpd AllUpdated $ (SkipArr 0, state, RenderingLevel scale c r sz (not debugging) ur)

    KeySpecialRen UpKey (Modifiers False True False)    -> (NavUpDocArr, state, renLvl) -- Ctrl
    KeySpecialRen DownKey (Modifiers False True False)  -> (NavDownDocArr, state, renLvl) -- Ctrl
    KeySpecialRen LeftKey (Modifiers False True False)  -> (NavLeftDocArr, state, renLvl) -- Ctrl
    KeySpecialRen RightKey (Modifiers False True False) -> (NavRightDocArr, state, renLvl) -- Ctrl
    KeySpecialRen LeftKey (Modifiers True False False)  -> (EnlargeLeftArr, state, renLvl) -- Shift
    KeySpecialRen RightKey (Modifiers True False False) -> (EnlargeRightArr, state, renLvl) -- Shift
    
    KeySpecialRen EnterKey ms     -> (SplitArr, state, renLvl)
    KeySpecialRen BackspaceKey ms -> (LeftDeleteArr, state, renLvl)
    KeySpecialRen DeleteKey ms    -> (RightDeleteArr, state, renLvl)
    KeySpecialRen LeftKey ms      -> (LeftArr, state, renLvl)
    KeySpecialRen RightKey ms     -> (RightArr, state, renLvl)
    KeySpecialRen F1Key ms        -> (TestArr, state, renLvl)
    KeySpecialRen F2Key ms        -> (Test2Arr, state, renLvl)
    KeySpecialRen F5Key ms        -> (NormalizeArr, state, renLvl)


-- partial redraw hack
    KeySpecialRen UpKey (Modifiers True False False)   -> setUpd NothingUpdated $ -- shift down
      ( SetFocusArr (enlargeFocus focus (upPath (toA focus) (if debugging then debugArrangement arr else arr)))
      , state, renLvl )
    KeySpecialRen DownKey (Modifiers True False False) -> setUpd NothingUpdated $ -- shift down
      ( SetFocusArr  (enlargeFocus focus (downPath (toA focus) (if debugging then debugArrangement arr else arr)))
      , state, renLvl )
    KeySpecialRen UpKey ms        -> setUpd NothingUpdated $ 
      ( SetFocusArr (upFocus focus (if debugging then debugArrangement arr else arr))
      , state, renLvl )
    KeySpecialRen DownKey ms      -> setUpd NothingUpdated $ 
      ( SetFocusArr (downFocus focus (if debugging then debugArrangement arr else arr))
      , state, renLvl )

    KeyCharRen c        -> (keyRemapChar c, state, renLvl)
    KeySpecialRen c ms  -> (KeySpecialArr c ms, state, renLvl)

    MouseDownRen x' y' (Modifiers True False False) i ->  -- shift down
      let (x,y) = (descaleInt scale x',descaleInt scale y')
      in  ( SetFocusArr (enlargeFocusXY focus x y (if debugging then debugArrangement arr else arr))
          , state, renLvl)


    MouseDownRen x' y' ms@(Modifiers False False True) i ->  -- alt down
      let (x,y) = (descaleInt scale x',descaleInt scale y') -- hack, alt mouse is mouseclick for doc
      in  ( MouseDownDocArr (navigateFocus x y arr) ms i
          , state, renLvl)
    MouseDownRen x' y' ms i -> 
      let (x,y) = (descaleInt scale x',descaleInt scale y')
--    in  (MouseDownArr (navigateFocus x y arrLvl) ms i, setFocus x y arrLvl, renLvl)  
      in  ( SetFocusArr (setFocus x y (if debugging then debugArrangement arr else arr))
          , state, renLvl )
-- non shift MouseDown either here or passed on. Doing both is not possible. If we pass it on, the higher layers are 
-- responsible for the focus update
    MouseDragRen x' y' ms@(Modifiers False False False)  ->
      let (x,y) = (descaleInt scale x',descaleInt scale y')
      in  ( SetFocusArr (enlargeFocusXY focus x y (if debugging then debugArrangement arr else arr))
          , state, renLvl )
    MouseUpRen x' y' ms    -> 
      let (x,y) = (descaleInt scale x',descaleInt scale y')
      in  ( SkipArr 0, state, renLvl ) 
    
    
    UpdateDocRen upd      -> (UpdateDocArr upd,      state, renLvl) 
    DocumentLoadedRen str -> (DocumentLoadedArr str, state, renLvl) 
    OpenFileRen filePath  -> (OpenFileArr filePath,  state, renLvl) 
    SaveFileRen filePath  -> (SaveFileArr filePath,  state, renLvl) 
    _                     -> (SkipArr 0,             state, renLvl)
{-
    OpenFileRen filePath)   =  unsafePerformIO $ --(OpenFileArr filePath, state, renLvl) 
  do { debugLnIO Prs "Opening file"
    ; str <- readFile filePath
    ; let arr':: Arrangement = read str
    ; return (SkipArr 0 , state, renLvl) -- there is no SetArr! 
    }
interpret state ren arrLvl@(ArrangementLevel arr _ _) (SaveFileRen filePath)        = unsafePerformIO $ --(SaveFileArr filePath, state, renLvl) 
  do { debugLnIO Ren "Saving Arrangement"
     ; writeFile filePath $ show arr
     ; return (SkipArr 0, state, renLvl)
     }
-}



-- arrangement is widened in case of debug, but not for skip


{- 
ObjectIO does not map Ctrl key combinations very well

Some Ctrl+char key combinations are mapped on special keys and therefore indistinguishable from the special key + ctrl
Furthermore, menus with hot keys bind ctrl+char keys as well.
The other ctrl+char keys are mapped on char keys ('b'->'\2'),('c'->'\3'), ..

keys that cannot be caught:
CTRL +
a = BeginKey (=home)
d = EndKey 
e = HelpKey
h = BackSpaceKey
k = PgUpKey
l = PgDownKey
m = EnterKey

tab is a special case, as it is a char key under 32 but not a ctrl+char key
-}
ctrlM :: Modifiers
ctrlM = Modifiers False True False

keyRemapChar '\t' = KeyCharArr '\t'
keyRemapChar c    = if ord c <= 26 then KeySpecialArr (CharKey (chr (ord c+96))) ctrlM
                                   else KeyCharArr c

  

-- At this time, Focus path is always in a leaf string

-- focus is not perfect yet. Selection halfway a letter should include the letter (or element). This makes
-- enlargement dependent on drag origin. Furthermore, pointing in empty space should lead to some path. 
-- E.g. first thing on the left in a column and first thing above in a column.

-- pointing in stretched rows might lead to a focus which is not a leaf path, a next version of the
-- rendering level must handle this correctly. For now, non stretching rows are assumed

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



-- focus path stuff is reused and combined with index.

navigateFocus x y constrainedPicob = let ps = point' x y [[]] constrainedPicob
                                     in  if null ps then (PathA [] 0) else (PathA (head ps) (getOffset (head ps) x constrainedPicob))
 where getOffset ps mx constrainedPicob =
         case selectTreeA ps constrainedPicob of 
           (x',y', StringA _ x y w h _ _ _ _ _ cxs) ->
             let pos = mx - (clip 0 x x) - x'        -- in case x is negative (point' takes care of clipping itself)
             in  (length (takeWhile (<=pos) (centerXCoords cxs)))-1
           _                                                            -> 0

-- for pointing after a character when to the right of its center
centerXCoords []      = [] -- this never occurs
centerXCoords xcoords = let widths = zipWith (-) (tail xcoords) xcoords
                            halfwidths = map (`div` 2) widths
                        in  head xcoords : zipWith (+) xcoords halfwidths
-- don't want cumulative character widths

