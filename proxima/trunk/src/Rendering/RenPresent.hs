module Rendering.RenPresent where

import Common.CommonTypes
import Rendering.RenLayerTypes
import Rendering.RenLayerUtils -- for context menu hack
import Proxima.Wrap

--import RendererGTK
import Rendering.Renderer

import Evaluation.DocTypes (DocumentLevel)

presentIO state high low editsHigh =  castRemainingEditOps editsHigh $ \editHigh ->
  return $ present state high low editHigh

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
render state (ArrangementLevel arr focus prs) ren@(RenderingLevel scale _ _ _ _ debugging updRegions lmd) (SkipArr' 0) = 
   let arr'        = if debugging then debugArrangement arr else arr
       diffTree    = DiffLeaf False
       rendering   = render' scale debugging diffTree arr' 
       focusRendering = renderFocus scale debugging focus arr'
       updRegions' = computeUpdatedRegions updRegions scale focus diffTree arr arr'
       size        = (widthA arr', heightA arr')
   in  ( SetRen' (RenderingLevel scale (mkPopupMenuXY prs scale arr') rendering focusRendering size debugging updRegions' lmd)
       , state, ArrangementLevel arr focus prs)
render state arrLvl ren (SkipArr' i) = (SkipRen' (i-1), state, arrLvl)
render state (ArrangementLevel arrOld focusOld _) ren@(RenderingLevel scale _ _ _ _ debugging updRegions lmd) (SetArr' (ArrangementLevel arr focus prs)) =  -- arr is recomputed, so no debug
   let arr'        = if debugging then debugArrangement arr else arr
       diffTree    = diffArr arr' arrOld
       updRegions' = computeUpdatedRegions updRegions scale focus diffTree arrOld arr'
       rendering   = if rendererIncrementality 
                     then render' scale debugging diffTree arr'
                     else render' scale debugging (DiffLeaf False) arr'
       focusRendering = renderFocus scale debugging focus arr'
       size        = (widthA arr', heightA arr')
   in  {-debug Arr ("\n\n\nRender: old/new size "++ show (widthA arrOld, heightA arrOld)++ show (widthA arr', heightA arr')
                  ++ "\nDiffTree: "++ show diffTree
                  ++"Updated Regions" ++ show updRegions'
                  ) 
       
       $ -}
       ( SetRen' (RenderingLevel scale (mkPopupMenuXY prs scale arr') rendering focusRendering size debugging updRegions' lmd)
       , state, ArrangementLevel arr focus prs)
render state arrLvl ren (WrapArr' wrapped) = (unwrap wrapped, state, arrLvl)
