module Arrangement.ArrPresent where

import Common.CommonTypes
import Arrangement.ArrLayerTypes
import Arrangement.ArrLayerUtils

import Arrangement.Arranger

--present ::  state -> high -> low -> editHigh' -> (editLow', state, high)
presentIO state high low editHigh =
  do { (editLow, state', high') <- arrange state high low editHigh
     --; debugLnIO Arr $ "EditLayout':"++show editHigh
     --; debugLnIO Arr $ "EditArrangement':"++show editLow
     ; return (editLow, state', high')
     }

-- arrangement and focus are tricky. focus selection and focus arranging both need an arrangement without the focus

-- SkipLay' 0: layout has been edited dt contains the correct diffs

-- on a skipLay, the local arr state may have changed, so rearrange
arrange :: (Show node, Show token) => LocalStateArr -> LayoutLevel doc node clip token -> ArrangementLevel doc node clip token ->
           EditLayout' doc node clip token ->
           IO (EditArrangement' doc node clip token, LocalStateArr, LayoutLevel doc node clip token)
arrange state layLvl@(LayoutLevel pres focus dt) arrLvl@(ArrangementLevel oldArrangement _ _) (SkipLay' 0) =
 do { (arr', state') <- arrangePresentation state (getFontMetricsRef state) focus oldArrangement dt pres -- DiffLeaf True? or can arr have changed
    ; return (SetArr' (ArrangementLevel arr' (focusAFromFocusP focus arr' pres) pres), state', layLvl)
    }
arrange state layLvl arrLvl (SkipLay' i) = return (SkipArr' (i-1), state, layLvl)
arrange state layLvl arrLvl@(ArrangementLevel oldArrangement _ _) (SetLay' (LayoutLevel pres' focus' dt)) = 
 do { (arrangement',state') <- arrangePresentation state (getFontMetricsRef state) focus' oldArrangement dt pres'
    ; return (SetArr' (ArrangementLevel arrangement' (focusAFromFocusP focus' arrangement' pres') pres'), state', LayoutLevel pres' focus' dt)
    }
