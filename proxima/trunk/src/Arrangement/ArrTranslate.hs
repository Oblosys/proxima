module Arrangement.ArrTranslate where

import Common.CommonTypes
import Common.CommonUtils
import Arrangement.ArrLayerTypes
import Arrangement.ArrLayerUtils

import Proxima.Wrap

import Evaluation.DocTypes
import Evaluation.DocUtils
import qualified Layout.TreeEditPres as TreeEditPres -- for mouse handling stuff

import Data.IORef
import qualified Data.Map as Map
import Data.Map (Map)

import Evaluation.DocumentEdit

translateIO state low high =  castRemainingEditOps $ \editLow ->
 do { (editHigh, state', low') <- unArrangeIO state low high editLow
    ; return ([editHigh], state', low')
    }


    
unArrangeIO  state arrLvl@(ArrangementLevel arr focus p) layLvl@(LayoutLevel pres _ _) ClearMetricsArr = 
 do { writeIORef (getFontMetricsRef state) Map.empty -- TODO: put Map.empty as a function in FontLib
    ; return (SkipLay 0,             state, (ArrangementLevel emptyA focus p))
    }
unArrangeIO  state arrLvl@(ArrangementLevel arr focus p) layLvl@(LayoutLevel pres _ _) editArr = 
 do { let (editHigh, state', low') = unArrange state arrLvl layLvl editArr
    ; return (editHigh, state', low')
    }
    
unArrange :: forall doc enr node clip token state .
             (Show doc, Show enr, Show token, Show node, DocNode node, Clip clip
             ,Editable doc doc node clip token) => LocalStateArr -> ArrangementLevel doc node clip token -> LayoutLevel doc node clip token ->
             EditArrangement doc enr node clip token ->
             (EditLayout doc enr node clip token, LocalStateArr, ArrangementLevel doc node clip token)
unArrange state arrLvl@(ArrangementLevel arr focus p) layLvl@(LayoutLevel pres _ _) editArr = 
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
    RedrawArr             -> (SkipLay 0,             state, (ArrangementLevel emptyA focus p))
    Test2Arr              -> (Test2Lay,              state, arrLvl)
    KeyCharArr c          -> (InsertLay c,           state, arrLvl)--debug UnA (show$KeyCharArr c) (let (a,b) = editArr c state in (SkipLay 0, a,b) )
    KeySpecialArr c ms    -> (SkipLay 0,             state, arrLvl) 
    MouseDownArr x y (Modifiers False False False) i ->
      (case navigateFocus x y arr of
        PathA pthA _ ->
          case mouseDownDocPres (pathPFromPathA' arr pres pthA) pres of
              Just upd -> debug UnA ("mouseDownDoc EVENT: Something") 
                            cast (UpdateDoc' upd :: EditDocument' doc enr node clip token)
                            
              Nothing  -> SetFocusLay (computeFocus arr pres x y)  
        _ ->  debug Err ("UnArranger.mouseDownDoc: empty path ") $ SkipLay 0   
      , state , arrLvl)
-- shift mouseDown is handled here
    MouseDownArr x y (Modifiers True False False) i ->  -- shift down 
      case isGraphEdit x y arr pres of
        Just addVertex    -> ( addVertex, state, arrLvl )
        Nothing           -> ( SetFocusLay (focusPFromFocusA (enlargeFocusXY focus x y arr) arr pres)
                       , state, arrLvl )
--    MouseDownArr x y ms@(Modifiers False False True) i -> -- alt down 
    MouseDownArr x y ms@(Modifiers False True False) i -> -- ctrl down 
          mouseDownDoc state arrLvl pres (navigateFocus x y arr) i
    MouseDragArr x y ms@(Modifiers False False False)  ->
      ( SetFocusLay (focusPFromFocusA (enlargeFocusXY focus x y arr) arr pres)
      , state, arrLvl ) -- does not occur
                            
    MouseUpArr x y ms     -> (ParseLay, state, arrLvl) 
    
    DragStartArr x y -> debug Arr "Drag started" $
      (SkipLay 0, state { getLastMousePress = Just (x,y)}, arrLvl)

    DropArr x y -> 
      (case getLastMousePress state of
        Just (x',y') -> 
         case point x' y' arr of
          Nothing -> debug Err ("No path for drag source at "++show (x',y')) $ SkipLay 0 
          Just completeDragPath ->
           let pathToDraggable = getPathToDraggable completeDragPath arr
           in  debug Arr ("Drag ended " ++ show (selectTreeA pathToDraggable arr)) $
             case (\(_,_,x)->x) $ selectTreeA pathToDraggable arr of 
                   VertexA _ _ _ _ _ _ _ _ _ _ -> MoveVertexLay (pathPFromPathA' arr pres pathToDraggable ) (x-x',y-y')
                   -- for vertex we don't want the tag included, for others maybe we do
                   -- figure out how to do this
                   (LocatorA n a) -> debug Arr ("dragging"++shallowShowArr a) $ 
                     case pathNode n of
                       PathD srcPath ->
                         cast ( UpdateDoc' (\(DocumentLevel d p cl) -> 
                                         DocumentLevel (fst $ deleteD srcPath d) p cl)
                          :: EditDocument' doc enr node clip token)        
                       NoPathD -> SkipLay 0
                   _ -> SkipLay 0
        _ -> SkipLay 0 -- no last mouse press, does not occur
      , state { getLastMousePress = Nothing }, arrLvl) 
      
    OpenFileArr str       -> (OpenFileLay str,       state, arrLvl) 
    SaveFileArr str       -> (SaveFileLay str,       state, arrLvl) 
    WrapArr wrapped       -> (unwrap wrapped,        state, arrLvl)
    _                     -> (SkipLay 0,             state, arrLvl) 
  
{- old drop for vertices
           case getLastMousePress state of -- should not be Nothing
            Just (x',y') -> 
             case navigateFocus x' y' arr of
               PathA pth _ ->
                 case selectTreeA pth arr of -- for Vertex, we drag, for graph and edge, drag is ignored
                   (_,_,VertexA _ _ _ _ _ _ _ _ _ _) -> MoveVertexLay (pathPFromPathA' arr pres pth ) (x-x',y-y')
                   _ -> SkipLay 0
               _ -> SkipLay 0

-}

indexOfDragSourceTag i [] = Nothing
indexOfDragSourceTag i (TagA DragSourceTag _:arrs) = Just i
indexOfDragSourceTag i (_:arrs) = indexOfDragSourceTag (i+1) arrs

getPathToDraggable completeDragPath arr = 
  case indexOfDragSourceTag 0 $ reverse (getPathNodesA completeDragPath arr) of
    Nothing -> error "no drag source on path"
    Just reverseIndexDeepestDragSourceTag ->
      let indexDeepestDragSourceTag = length completeDragPath - reverseIndexDeepestDragSourceTag
      in  take (indexDeepestDragSourceTag+1) completeDragPath            
          -- +1: return path to actual drag source, not to tag
              
-- mouseDownDocPres and DocumentLevel cause dependency on type DocumentLevel
mouseDownDoc :: forall doc enr node clip token state .
                (DocNode node, Show token)  => state -> ArrangementLevel doc node clip token ->
                Layout doc node clip token -> PathArr -> Int ->
                (EditLayout doc enr node clip token, state, ArrangementLevel doc node clip token)  
mouseDownDoc state arrLvl@(ArrangementLevel arr _ _) layout (PathA pthA _) i = -- only look at start of focus. focus will be empty
  let pthP = pathPFromPathA' arr layout pthA
  in  case locateTreePres (PathP pthP 0) layout of -- set the document focus
        Just node -> case pathNode node of
                       (PathD pth) -> ( cast ( UpdateDoc' (\(DocumentLevel d _ cl) -> DocumentLevel d (PathD pth) cl)
                                                :: EditDocument' doc enr node clip token)
                                      , state, arrLvl)
                       _                -> (SkipLay 0, state, arrLvl) -- node has no path
        _         -> (SkipLay 0, state, arrLvl) -- no locator
mouseDownDoc state arrLvl layout pathA i =
  debug Err ("UnArranger.mouseDownDoc: empty path ") (SkipLay 0, state, arrLvl)                                                 

isGraphEdit :: (Show node, Show token) => Int -> Int -> Arrangement node -> Layout doc node clip token ->
               Maybe (EditLayout doc enr node clip token)
isGraphEdit x y arr pres =
      case navigateFocus x y arr of 
        PathA pth _ -> case selectTreeA pth arr of
                         (xGraph,yGraph, GraphA _ _ _ _ _ _ _ _ _ _) ->
                           Just $ AddVertexLay (pathPFromPathA' arr pres pth) (x-xGraph, y-yGraph)
                         (_,_, VertexA _ _ _ _ _ _ _ _ _ _) ->
                           Just $ AddEdgeLay (pathPFromPathA' arr pres pth)
                         _                                             -> Nothing
        _ -> Nothing





computeFocus arr pres x y = -- if we focus on empty, add a navigate left/right, based
                           -- on whether x is left or right of the middle
                           -- not yet ideal, we want to have left/right depend on whether
                           -- there is something focusable on the same level. 
                           -- click at . in (.   1    /     2   )  should go right to |1
          let focusA = focusAFromXY x y arr 
              focusP = focusPFromFocusA focusA arr pres
          in  case (focusA,focusP) of
                (FocusA (PathA pth i) _,FocusP pathPres _) ->
                  case selectTreeA pth arr of
                    (x',_,EmptyA _ x'' _ width _ _ _ _) -> 
                      if x-(x'+x'') < width `div` 2 
                      then TreeEditPres.navigateLeftTreePres pathPres pres
                      else TreeEditPres.navigateRightTreePres pathPres pres
                    _ -> focusP
            
                _ -> focusP

{-
isEditablePres ~> isParsingPres

if isParsing ..
else 
  get nearestParentStructuralInParsing
  


in structural

select while keep track of p/s
-}
