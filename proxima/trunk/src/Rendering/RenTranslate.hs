module Rendering.RenTranslate where

import Common.CommonTypes
import Rendering.RenLayerTypes
import Rendering.RenLayerUtils
import Proxima.Wrap

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




interpret :: Show node => LocalStateRen -> RenderingLevel docLevel doc enr node clip token ->
             ArrangementLevel doc node clip token -> EditRendering docLevel doc enr node clip token ->
             (EditArrangement docLevel doc enr node clip token, LocalStateRen, RenderingLevel docLevel doc enr node clip token)
interpret state renLvl@(RenderingLevel scale c r fr sz debugging ur lmd)
                arrLvl@(ArrangementLevel arr focus _) editRen = debug Ren ("Rendering edit:"++show editRen) $
  case editRen of
    InitRen             -> (InitArr,       state, renLvl) 
    CloseRen            -> (CloseArr,      state, renLvl)
    SkipRen i           -> (SkipArr (i+1), state, renLvl)
-- TODO: make selectors scaleR and debuggingR for RenderingLevel
    KeySpecialRen (CharKey 'c') (Modifiers False True False) -> (CopyArr,       state, renLvl) -- Ctrl-c
    KeySpecialRen (CharKey 'v') (Modifiers False True False) -> (PasteArr,      state, renLvl) -- Ctrl-v
    KeySpecialRen (CharKey 'x') (Modifiers False True False) -> (CutArr,        state, renLvl) -- Ctrl-x
    KeySpecialRen (CharKey 'f') (Modifiers False True False) -> (CopyDocArr,    state, renLvl) -- Ctrl-f
    KeySpecialRen (CharKey 'g') (Modifiers False True False) -> (PasteDocArr,   state, renLvl) -- Ctrl-g
    KeySpecialRen (CharKey 's') (Modifiers False True False) -> (CutDocArr,     state, renLvl) -- Ctrl-d
    KeySpecialRen (CharKey 'z') (Modifiers False True False) -> (UndoDocArr,     state, renLvl) -- Ctrl-d
    KeySpecialRen (CharKey 'y') (Modifiers False True False) -> (RedoDocArr,     state, renLvl) -- Ctrl-d
    KeySpecialRen UpKey   (Modifiers False False True) -> (SkipArr 0, state, RenderingLevel (scale*2) c r fr sz debugging ur lmd)
    KeySpecialRen DownKey (Modifiers False False True) -> (SkipArr 0, state, RenderingLevel (scale/2) c r fr sz debugging ur lmd)
    KeySpecialRen F9Key ms                             -> (SkipArr 0, state, RenderingLevel scale c r fr sz (not debugging) ur lmd)

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
    KeySpecialRen F1Key ms        -> (ParseArr, state, renLvl)
    KeySpecialRen F2Key ms        -> (Test2Arr, state, renLvl)
    KeySpecialRen F5Key ms        -> (NormalizeArr, state, renLvl)


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

    KeyCharRen c          -> (KeyCharArr c, state, renLvl)
    KeySpecialRen c ms    -> (KeySpecialArr c ms, state, renLvl)
    MouseDownRen x y ms i -> (MouseDownArr (descaleInt scale x) (descaleInt scale y) ms i, state, RenderingLevel scale c r fr sz debugging ur True)
    MouseDragRen x y ms   -> (MouseDragArr (descaleInt scale x) (descaleInt scale y) ms, state, renLvl)
    MouseUpRen x y ms     -> (MouseUpArr (descaleInt scale x) (descaleInt scale y) ms, state, RenderingLevel scale c r fr sz debugging ur False)
    
    UpdateDocRen upd      -> (UpdateDocArr upd,      state, renLvl) 
    DocumentLoadedRen str -> (DocumentLoadedArr str, state, renLvl) 
    OpenFileRen filePath  -> (OpenFileArr filePath,  state, renLvl) 
    SaveFileRen filePath  -> (SaveFileArr filePath,  state, renLvl) 
    _                     -> (SkipArr 0,             state, renLvl)
