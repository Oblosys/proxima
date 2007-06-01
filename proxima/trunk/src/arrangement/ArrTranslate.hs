module ArrTranslate where

import CommonTypes
import ArrLayerTypes
import ArrLayerUtils

import DocTypes
import DocUtils
import qualified TreeEditPres -- for mouse handling stuff


translateIO state low high editLow = return $ unArrange state low high editLow

translate state low high editLow = 
  let (editHigh, state', low') = unArrange state low high editLow
  in (editHigh, state', low')


unArrange :: (HasPath node, Show node) => LocalStateArr -> ArrangementLevel doc node clip -> LayoutLevel doc node clip ->
             EditArrangement (DocumentLevel doc clip) ->
             (EditLayout (DocumentLevel doc clip) doc node clip, LocalStateArr, ArrangementLevel doc node clip)
unArrange state arrLvl@(ArrangementLevel arr _ p) laylvl@(LayoutLevel pres _ _) editArr = 
  case editArr of
    SkipArr i             -> (SkipLay (i+1),         state, arrLvl) 
    SetFocusArr focus     -> ( SetFocusLay (focusPFromFocusA focus pres)
                             , state, ArrangementLevel arr focus p)
    InitArr               -> (InitLay,               state, arrLvl) 
    CloseArr              -> (CloseLay,              state, arrLvl) 
    CutArr                -> (CutLay,                state, arrLvl)
    CopyArr               -> (CopyLay,               state, arrLvl)
    PasteArr              -> (PasteLay,              state, arrLvl)
    DeleteArr             -> (DeleteLay,             state, arrLvl)
    SplitArr              -> (SplitLay,              state, arrLvl)
    LeftDeleteArr         -> (LeftDeleteLay,         state, arrLvl)
    RightDeleteArr        -> (RightDeleteLay,        state, arrLvl)
    LeftArr               -> (LeftLay,               state, arrLvl)
    RightArr              -> (RightLay,              state, arrLvl)
    EnlargeLeftArr        -> (EnlargeLeftLay,        state, arrLvl)
    EnlargeRightArr       -> (EnlargeRightLay,       state, arrLvl)
    NormalizeArr          -> (NormalizeLay,          state, arrLvl)
    TestArr               -> (TestLay,               state, arrLvl)
    Test2Arr              -> (Test2Lay,              state, arrLvl)
    KeyCharArr c          -> (InsertLay c,           state, arrLvl)--debug UnA (show$KeyCharArr c) (let (a,b) = editArr c state in (SkipLay 0, a,b) )
    KeySpecialArr c ms    -> (SkipLay 0,             state, arrLvl) 
-- shift mouseDown is handled here
    MouseDownDocArr fA (Modifiers False False True) i -> mouseDownDoc state arrLvl pres fA i
    MouseDownArr fA ms i  -> ( MouseDownLay (pathPFromPathA fA pres) ms i
                             , state, arrLvl)--setFocus arrLvl state x y, arrLvl)
    MouseDragArr fA ms    -> (SkipLay 0,             state, arrLvl)--enlargeFocus arrLvl state x y, arrLvl)
    MouseUpArr fA ms      -> (SkipLay 0,             state, arrLvl) 
    OpenFileArr str       -> (OpenFileLay str,       state, arrLvl) 
    SaveFileArr str       -> (SaveFileLay str,       state, arrLvl) 
    UpdateDocArr upd      -> (UpdateDocLay upd,      state, arrLvl) 
    NavUpDocArr           -> (NavUpDocLay,           state, arrLvl) 
    NavDownDocArr         -> (NavDownDocLay,         state, arrLvl) 
    NavLeftDocArr         -> (NavLeftDocLay,         state, arrLvl) 
    NavRightDocArr        -> (NavRightDocLay,        state, arrLvl) 
    CopyDocArr            -> (CopyDocLay,            state, arrLvl) 
    CutDocArr             -> (CutDocLay,             state, arrLvl) 
    PasteDocArr           -> (PasteDocLay,           state, arrLvl) 
    DeleteDocArr          -> (DeleteDocLay,          state, arrLvl) 
    DocumentLoadedArr str -> (DocumentLoadedLay str, state, arrLvl) 
    _                     -> (SkipLay 0,             state, arrLvl) 


  
  
-- mouseDownDocPres and \DocumentLevel cause dependency on type DocumentLevel
mouseDownDoc :: HasPath node  => state -> ArrangementLevel doc node clip ->
                Presentation doc node clip -> PathArr -> Int ->
                (EditLayout (DocumentLevel doc clip) doc node clip, state, ArrangementLevel doc node clip)  
mouseDownDoc state arrLvl layout (PathA pthA _) i = -- only look at start of focus. focus will be empty
  let pthP = pathPFromPathA' pthA layout
  in  case mouseDownDocPres pthP layout of
        Just upd -> debug UnA ("mouseDownDoc EVENT: Something") (UpdateDocLay upd, state, arrLvl)
        Nothing  -> debug UnA ("mouseDownDoc EVENT: Nothing:"++show pthP)   
                    $ case locateTreePres (PathP pthP 0) layout of
                        Just node -> case pathNode node of
                                       (PathD pth) -> ( UpdateDocLay (\(DocumentLevel d _ cl) -> DocumentLevel d (PathD pth) cl)
                                                      , state, arrLvl)
                                       _                -> (SkipLay 0, state, arrLvl) -- node has no path
                        _         -> (SkipLay 0, state, arrLvl) -- no locator
mouseDownDoc state arrLvl layout pathA i =
  debug Err ("UnArranger.mouseDownDoc: empty path ") (SkipLay 0, state, arrLvl)                                                 
