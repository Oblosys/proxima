module RenTranslate where

import CommonTypes
import RenLayerTypes
import RenLayerUtils

import Char
-- import IOExts

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




interpret :: Show node => LocalStateRen -> RenderingLevel documentLevel ->
             ArrangementLevel doc node clip -> EditRendering documentLevel ->
             (EditArrangement documentLevel, LocalStateRen, RenderingLevel documentLevel)
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
    KeySpecialRen UpKey   (Modifiers False False True) -> (SkipArr 0, state, RenderingLevel (scale*2) c r sz debugging ur)
    KeySpecialRen DownKey (Modifiers False False True) -> (SkipArr 0, state, RenderingLevel (scale/2) c r sz debugging ur)
    KeySpecialRen F9Key ms                             -> (SkipArr 0, state, RenderingLevel scale c r sz (not debugging) ur)

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
    KeySpecialRen UpKey (Modifiers True False False)   -> -- shift down
      ( SetFocusArr (enlargeFocus focus (upPath (toA focus) (if debugging then debugArrangement arr else arr)))
      , state, renLvl )
    KeySpecialRen DownKey (Modifiers True False False) -> -- shift down
      ( SetFocusArr  (enlargeFocus focus (downPath (toA focus) (if debugging then debugArrangement arr else arr)))
      , state, renLvl )
    KeySpecialRen UpKey ms        ->
      ( SetFocusArr (upFocus focus (if debugging then debugArrangement arr else arr))
      , state, renLvl )
    KeySpecialRen DownKey ms      ->
      ( SetFocusArr (downFocus focus (if debugging then debugArrangement arr else arr))
      , state, renLvl )

    KeyCharRen c          -> (keyRemapChar c, state, renLvl)
    KeySpecialRen c ms    -> (KeySpecialArr c ms, state, renLvl)
    MouseDownRen x y ms i -> debug Err "Mouze down" $ (MouseDownArr (descaleInt scale x) (descaleInt scale y) ms i, state, renLvl)
    MouseDragRen x y ms   -> (MouseDragArr (descaleInt scale x) (descaleInt scale y) ms, state, renLvl)
    MouseUpRen x y ms     -> (MouseUpArr (descaleInt scale x) (descaleInt scale y) ms, state, renLvl)
    
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

  


