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
unArrange state arrLvl@(ArrangementLevel arr focus p) laylvl@(LayoutLevel pres _ _) editArr = 
  debug Arr ("Edit arr is "++show editArr) $
  case editArr of
    SkipArr i             -> (SkipLay (i+1),         state, arrLvl) 
    SetFocusArr focus'     -> ( SetFocusLay (focusPFromFocusA focus' arr pres)
                             , state, ArrangementLevel arr focus p) -- new focus is not set on arr level
                                                                    -- this is done on presentation, so we
                                                                    -- still have the old focus for incrementality
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
    ParseArr              -> (ParseLay,              state, arrLvl)
    Test2Arr              -> (Test2Lay,              state, arrLvl)
    KeyCharArr c          -> (InsertLay c,           state, arrLvl)--debug UnA (show$KeyCharArr c) (let (a,b) = editArr c state in (SkipLay 0, a,b) )
    KeySpecialArr c ms    -> (SkipLay 0,             state, arrLvl) 
    MouseDownArr x y (Modifiers False False False) i ->
          ( SetFocusLay (focusPFromFocusA (focusAFromXY x y arr) arr pres)
          , state { getLastMousePress = Just (x,y)}, arrLvl)
-- shift mouseDown is handled here
    MouseDownArr x y (Modifiers True False False) i ->  -- shift down 
      case isGraphEdit x y arr pres of
        Just addVertex    -> ( addVertex, state, arrLvl )
        Nothing           -> ( SetFocusLay (focusPFromFocusA (enlargeFocusXY focus x y arr) arr pres)
                       , state, arrLvl )
    MouseDownArr x y ms@(Modifiers False False True) i -> -- alt down 
          mouseDownDoc state arrLvl pres (navigateFocus x y arr) i
    MouseDragArr x y ms@(Modifiers False False False)  ->
      case getLastMousePress state of -- should not be Nothing, since a mouseDrag is preceded by a mouseDown
        Just (x',y') -> 
          case navigateFocus x' y' arr of
            PathA pth _ ->
              case selectTreeA pth arr of -- for Vertex, we drag, for graph and edge, drag is ignored
                (_,_,VertexA _ _ _ _ _ _ _ _ _ _) -> (MoveVertexLay (pathPFromPathA' arr pres pth ) (x-x',y-y')
                                                     , state { getLastMousePress = Just (x, y)}, arrLvl) 
                (_,_,GraphA _ _ _ _ _ _ _ _ _ _) -> (SkipLay 0
                                                     , state { getLastMousePress = Just (x, y)}, arrLvl) 
                (_,_,EdgeA _ _ _ _ _ _ _ _ _) -> (SkipLay 0
                                                     , state { getLastMousePress = Just (x, y)}, arrLvl) 
                _ ->               ( SetFocusLay (focusPFromFocusA (enlargeFocusXY focus x y arr) arr pres)
                                    , state, arrLvl )
            _ ->               ( SetFocusLay (focusPFromFocusA (enlargeFocusXY focus x y arr) arr pres)
                               , state, arrLvl )
        Nothing -> ( SetFocusLay (focusPFromFocusA (enlargeFocusXY focus x y arr) arr pres)
                   , state, arrLvl ) -- does not occur
                            
    MouseUpArr x y ms     -> (ParseLay,            state { getLastMousePress = Nothing }, arrLvl) 
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
  
  
-- mouseDownDocPres and DocumentLevel cause dependency on type DocumentLevel
mouseDownDoc :: (Show node, HasPath node)  => state -> ArrangementLevel doc node clip ->
                Presentation doc node clip -> PathArr -> Int ->
                (EditLayout (DocumentLevel doc clip) doc node clip, state, ArrangementLevel doc node clip)  
mouseDownDoc state arrLvl@(ArrangementLevel arr _ _) layout (PathA pthA _) i = -- only look at start of focus. focus will be empty
  let pthP = pathPFromPathA' arr layout pthA
  in  case mouseDownDocPres pthP layout of
        Just upd -> debug UnA ("mouseDownDoc EVENT: Something") (UpdateDocLay upd, state, arrLvl)
        Nothing  -> debug UnA ("mouseDownDoc EVENT: Nothing:"++show pthP)
                    $ case locateTreePres (PathP pthP 0) layout of -- set the document focus
                        Just node -> case pathNode node of
                                       (PathD pth) -> ( UpdateDocLay (\(DocumentLevel d _ cl) -> DocumentLevel d (PathD pth) cl)
                                                      , state, arrLvl)
                                       _                -> (SkipLay 0, state, arrLvl) -- node has no path
                        _         -> (SkipLay 0, state, arrLvl) -- no locator
mouseDownDoc state arrLvl layout pathA i =
  debug Err ("UnArranger.mouseDownDoc: empty path ") (SkipLay 0, state, arrLvl)                                                 

isGraphEdit :: Show node => Int -> Int -> Arrangement node -> Presentation doc node clip ->
               Maybe (EditLayout docLvl doc node clip)
isGraphEdit x y arr pres =
      case navigateFocus x y arr of 
        PathA pth _ -> case selectTreeA pth arr of
                         (xGraph,yGraph, GraphA _ _ _ _ _ _ _ _ _ _) ->
                           Just $ AddVertexLay (pathPFromPathA' arr pres pth) (x-xGraph, y-yGraph)
                         (_,_, VertexA _ _ _ _ _ _ _ _ _ _) ->
                           Just $ AddEdgeLay (pathPFromPathA' arr pres pth)
                         _                                             -> Nothing
        _ -> Nothing