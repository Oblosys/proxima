module Rendering.RenPresent where

import Common.CommonTypes
import Rendering.RenLayerTypes
import Rendering.RenLayerUtils -- for context menu hack
import Proxima.Wrap

--import RendererGTK
import Rendering.Renderer

import Evaluation.DocTypes (DocumentLevel)

import Data.Time.Clock

presentIO settings state high low =  castRemainingEditOps $ \editHigh ->
 do { (editLow, state', high') <- Rendering.RenPresent.render settings state high low editHigh
    ; return ([editLow], state', high')
    }
    
-- debug & scaling is now done directly. This should be done with a setRendering
{-
render :: (HasPath node, Show node) => Settings ->
          LocalStateRen -> ArrangementLevel doc node clip -> RenderingLevel (DocumentLevel doc clip) ->
          EditArrangement' doc node clip ->
          (EditRendering' (DocumentLevel doc clip), LocalStateRen, ArrangementLevel doc node clip)
-}
render settings state (ArrangementLevel arr focus prs) ren@(RenderingLevel scale _ _ _ _ debugging updRegions lmd) (SkipArr' 0) = 
   let arr'        = if debugging then debugArrangement arr else arr
       diffTree    = DiffLeafArr False Nothing
       rendering   = Rendering.Renderer.render scale debugging diffTree arr' 
       focusRendering = renderFocus scale debugging focus arr'
       updRegions' = computeUpdatedRegions updRegions scale focus diffTree arr arr'
       size        = (widthA arr', heightA arr')
   in  return ( SetRen' (RenderingLevel scale (mkPopupMenuXY settings prs scale arr') rendering focusRendering size debugging updRegions' lmd)
       , state, ArrangementLevel arr focus prs)
render settings state arrLvl ren (SkipArr' i) = return (SkipRen' (i-1), state, arrLvl)
render settings state (ArrangementLevel arrOld focusOld _) ren@(RenderingLevel scale _ _ _ _ debugging updRegions lmd) (SetArr' (ArrangementLevel arr focus prs)) =  -- arr is recomputed, so no debug
   let arr'        = if debugging then debugArrangement arr else arr
       diffTree    = diffArr arr' arrOld
       updRegions' = computeUpdatedRegions updRegions scale focus diffTree arrOld arr'
       rendering   = if rendererIncrementality settings 
                     then Rendering.Renderer.render scale debugging diffTree arr'
                     else Rendering.Renderer.render scale debugging (DiffLeafArr False Nothing) arr'
       focusRendering = renderFocus scale debugging focus arr'
       size        = (widthA arr', heightA arr')
   in  {-debug Arr ("\n\n\nRender: old/new size "++ show (widthA arrOld, heightA arrOld)++ show (widthA arr', heightA arr')
                  ++ "\nDiffTree: "++ show diffTree
                  ++"Updated Regions" ++ show updRegions'
                  ) 
       
       $ -}
   do { t <- getCurrentTime
      --; putStrLn "\n\n\nBefore diff"
      ; seq (length $ show diffTree) $ return ()
     -- ; diff <- fmap (flip diffUTCTime $ t) getCurrentTime
     -- ; putStrLn $ "After diff: " ++ show diff
      ; return
       ( SetRen' (RenderingLevel scale (mkPopupMenuXY settings prs scale arr') rendering focusRendering size debugging updRegions' lmd)
       , state, ArrangementLevel arr focus prs)
      }
render settings state arrLvl ren (WrapArr' wrapped) = return (unwrap wrapped, state, arrLvl)
