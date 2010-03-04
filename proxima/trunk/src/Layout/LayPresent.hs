module Layout.LayPresent where

import Common.CommonTypes
import Common.CommonUtils
import Layout.LayLayerTypes
import Layout.LayLayerUtils

import Layout.Layout
import Layout.TreeEditPres

import qualified Data.Map as Map
import Data.Map (Map)

import Proxima.Wrap

presentIO :: (DocNode node, Show token, Eq token) => LayerStateLay doc enr node clip token -> PresentationLevel doc enr node clip token -> LayoutLevel doc enr node clip token ->
             [EditPresentation' doc enr node clip token] ->
             IO ([EditLayout' doc enr node clip token], LayerStateLay doc enr node clip token, PresentationLevel doc enr node clip token)
presentIO  state high low@(LayoutLevel pres focus _) = castRemainingEditOps $ \editHigh -> 
  let (editLow, state', high') = present state high low editHigh
  in do { -- debugLnIO Prs ("editLay':"++show editLow);
    --      case editLow of
    --        SetLay' (LayoutLevel pres' _) -> debugLnIO Prs ("diffs: "++show (diffPres pres' pres))  
    --        _                                    -> return ()

          return $ ([editLow], state', high')
        }

-- layout is old layout level, but on skip, the old one is the edited one that should be arranged again. The diff
-- is therefore done in LayInterpret after the layout is edited.

-- for a set operation, the new layout from presentation can be diffed with the old layout

-- doc in pattern should be pres
present :: (DocNode node, Show token, Eq token) => LayerStateLay doc enr node clip token -> PresentationLevel doc enr node clip token -> LayoutLevel doc enr node clip token ->
           EditPresentation' doc enr node clip token ->
           (EditLayout' doc enr node clip token, LayerStateLay doc enr node clip token, PresentationLevel doc enr node clip token)
present state pres (LayoutLevel lay focus dt) (SkipPres' 0) =
  let (lay', focus') = (,) {-normalizePresentation -} lay focus -- Normalize does not work in chess board, find out why
      diffTree = dt -- diffTree was created by interpret
  in  (SetLay' (LayoutLevel lay' focus' diffTree), state, pres)  -- we should re present here because of local state
present state pres lay (SkipPres' i) = (SkipLay' (i-1), state, pres)
present state _ (LayoutLevel lay focus dt) (SetPres' hp@(PresentationLevel pres (layout,idCounter)))  = 
  let (lay', scannedFocus) = {- normalizeTreePres $ -} detokenizer layout pres
      focus' = 
                 --debug Lay ("Layout map is "++show layout) $
                 --debug Lay ("Scanned focus is "++show scannedFocus) $
               case (scannedFocus, focus) of
                 (NoFocusP,               focus@(FocusP _ _)) -> debug Err "\n\nError: Focus was not restored\n\n"$ focus
                 (FocusP NoPathP NoPathP, FocusP fp tp) -> FocusP fp      tp
                 (FocusP NoPathP tp,      FocusP fp _ ) -> FocusP fp      tp
                 (FocusP fp      NoPathP, FocusP _  tp) -> FocusP fp      tp
                 (FocusP fp      tp,      _           ) -> FocusP fp      tp
                 
      diffTree = diffPres lay' lay
  in  --debug Lay ("old focus:    " ++ show focus ++ "\nScannedFocus: "++show scannedFocus 
                -- ++ case focus' of FocusP (PathP p _) _ -> "\nNodes on focus path:\n"++showPathNodes p lay
                --                _ -> ""
      --          )
      (SetLay' (LayoutLevel lay' focus' diffTree), state, hp) 
present state pres lay (WrapPres' wrapped) = (unwrap wrapped, state, pres)




