module RenPresent where

import CommonTypes
import RenLayerTypes
import RenLayerUtils -- for context menu hack

import Renderer

import DocTypes (DocumentLevel, HasPath)

presentIO state high low editHigh = return $ present state high low editHigh

--present ::  state -> high -> low -> editHigh' -> (editLow', state, high)
present state high low editHigh =
  let (editLow, state', high') = render state high low editHigh
  in  (editLow, state', high')



-- background rendering must be fixed. The boxes must be rendered behind the actual rendering elements,
-- and ideally, only when the background changes, the box is drawn.




-- debug & scaling is now done directly. This should be done with a setRendering
{-
render :: (HasPath node, Show node) =>
          LocalStateRen -> ArrangementLevel doc node clip -> RenderingLevel (DocumentLevel doc clip) ->
          EditArrangement' doc node clip ->
          (EditRendering' (DocumentLevel doc clip), LocalStateRen, ArrangementLevel doc node clip)
-}
render state (ArrangementLevel arr focus prs) ren@(RenderingLevel scale _ _ _ debugging updRegions) (SkipArr' 0) = 
   let arr'        = if debugging then debugArrangement arr else arr
       diffTree    = DiffLeaf False
       rendering   = render' scale debugging focus diffTree arr' 
       updRegions' = computeUpdatedRegions updRegions scale focus diffTree arr arr'
       size        = (widthA arr', heightA arr')
   in  ( SetRen' (RenderingLevel scale (mkPopupMenuXY prs scale arr') rendering size debugging updRegions')
       , state, ArrangementLevel arr focus prs)
render state arrLvl ren (SkipArr' i) = (SkipRen' (i-1), state, arrLvl)
render state (ArrangementLevel arrOld focusOld _) ren@(RenderingLevel scale _ _ _ debugging updRegions) (SetArr' (ArrangementLevel arr focus prs)) =  -- arr is recomputed, so no debug
   let arr'        = if debugging then debugArrangement arr else arr
       diffTree    = DiffLeaf False -- nonincremental
--       diffTree    = markFocusArr focus (markFocusArr focusOld (diffArr arr' arrOld)) -- incremental
       rendering   = render' scale debugging focus diffTree arr'
       updRegions' = computeUpdatedRegions updRegions scale focus diffTree arrOld arr'
       size        = (widthA arr', heightA arr')
   in  -- debug Arr ("Arr Diffs: "++show (diffArr arr' arrOld)) $
       ( SetRen' (RenderingLevel scale (mkPopupMenuXY prs scale arr') rendering size debugging updRegions')
       , state, ArrangementLevel arr focus prs)

