module LayPresent where

import CommonTypes
import LayLayerTypes
import LayLayerUtils

import Layout

import TreeEditPres

presentIO :: LayerStateLay -> PresentationLevel -> LayoutLevel -> EditPresentation' 
          -> IO (EditLayout', LayerStateLay, PresentationLevel)
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
present :: LayerStateLay -> PresentationLevel -> LayoutLevel -> EditPresentation' -> (EditLayout', LayerStateLay, PresentationLevel)
present state doc (LayoutLevel pres focus dt) (SkipPres' 0) = {-debug Prs ("Present:"++show pres++"\n focus "++show focus)-} 
  let (pres', focus') = (,) {-normalizePresentation -} pres focus -- Normalize does not work in chess board, find out why
--      diffTree = DiffLeaf False
      diffTree = dt
  in  (SetLay' (LayoutLevel pres' focus' diffTree), state, doc)  -- we should re present here because of local state
present state doc pres (SkipPres' i) = (SkipLay' (i-1), state, doc)
present state doc (LayoutLevel presL focus dt) (SetPres' hp@(PresentationLevel presH (layout,idCounter,inserted, deleted)))  = 
  let -- focusXY = saveFocus focus presL
      presL'  = {- normalizeTreePres $ -} detokenize (addToFM layout (IDP (-1)) (0,1), inserted, deleted) presH
      focus' = focus  -- restoreFocus focusXY presL'              -- focus hack. should be combined with higher level focus
      diffTree = DiffLeaf False
--      diffTree = diffPres presL' presL
 --     (pres'', focus'') = (pres',focus')--normalizePresentation pres focus
  in  (SetLay' (LayoutLevel presL' focus' diffTree), state, hp) 




 

-- goes wrong if focus is in empty string on left side of column
saveFocus :: FocusPres -> Presentation -> ((Int, Int, Bool), (Int, Int, Bool))
saveFocus NoFocusP _  = ((1,0,True),(1,0,True))
saveFocus focus pres = debug Err "AFOCUS" (xyFromPath (fromP focus) pres, xyFromPath (toP focus) pres)

restoreFocus :: ((Int, Int, Bool), (Int, Int, Bool)) -> Presentation -> FocusPres
restoreFocus (fxy,txy) pres = FocusP (pathFromXY fxy pres) (pathFromXY fxy pres) 
