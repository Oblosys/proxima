module LayPresent where

import CommonTypes
import LayLayerTypes
import LayLayerUtils

import Layout
import TreeEditPres

import qualified Data.Map as Map
import Data.Map (Map)

presentIO :: (Show token, Eq node) => LayerStateLay doc node clip -> PresentationLevel doc node clip token -> LayoutLevel doc node clip ->
             EditPresentation' doc node clip token ->
             IO (EditLayout' doc node clip, LayerStateLay doc node clip, PresentationLevel doc node clip token)
presentIO  state high low@(LayoutLevel pres focus _) editHigh = 
  let (editLow, state', high') = present state high low editHigh
  in do { -- debugLnIO Prs ("editLay':"++show editLow);
    --      case editLow of
    --        SetLay' (LayoutLevel pres' _) -> debugLnIO Prs ("diffs: "++show (diffPres pres' pres))  
    --        _                                    -> return ()

          return $ (editLow, state', high')
        }

-- layout is old layout level, but on skip, the old one is the edited one that should be arranged again. The diff
-- is therefore done in LayTranslate after the layout is edited.

-- for a set operation, the new layout from presentation can be diffed with the old layout


-- doc in pattern should be pres
present :: (Show token, Eq node) => LayerStateLay doc node clip -> PresentationLevel doc node clip token -> LayoutLevel doc node clip ->
           EditPresentation' doc node clip token ->
           (EditLayout' doc node clip, LayerStateLay doc node clip, PresentationLevel doc node clip token)
present state pres (LayoutLevel lay focus dt) (SkipPres' 0) =
  let (lay', focus') = (,) {-normalizePresentation -} lay focus -- Normalize does not work in chess board, find out why
      diffTree = dt -- diffTree was created by translate
  in  (SetLay' (LayoutLevel lay' focus' diffTree), state, pres)  -- we should re present here because of local state
present state pres lay (SkipPres' i) = (SkipLay' (i-1), state, pres)
present state _ (LayoutLevel lay focus dt) (SetPres' hp@(PresentationLevel pres (layout,idCounter,inserted, deleted)))  = 
  let -- focusXY = saveFocus focus lay
      lay'  = {- normalizeTreePres $ -} detokenize (Map.insert (IDP (-1)) (0,1) layout, inserted, deleted) pres
      focus' = focus  -- restoreFocus focusXY presL'              -- focus hack. should be combined with higher level focus
      diffTree = diffPres lay' lay
  in  (SetLay' (LayoutLevel lay' focus' diffTree), state, hp) 




 

-- goes wrong if focus is in empty string on left side of column
saveFocus :: Show token => FocusPres -> Presentation doc node clip token -> ((Int, Int, Bool), (Int, Int, Bool))
saveFocus NoFocusP _  = ((1,0,True),(1,0,True))
saveFocus focus pres = debug Err "AFOCUS" (xyFromPath (fromP focus) pres, xyFromPath (toP focus) pres)

restoreFocus :: Show token => ((Int, Int, Bool), (Int, Int, Bool)) -> Presentation doc node clip token -> FocusPres
restoreFocus (fxy,txy) pres = FocusP (pathFromXY fxy pres) (pathFromXY fxy pres) 
