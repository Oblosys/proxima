module Arrangement.Arranger where

import Common.CommonTypes
import Common.CommonUtils
import qualified Common.CommonTypes as CommonTypes
import Arrangement.ArrLayerTypes
import Arrangement.ArrLayerUtils

import Arrangement.ArrangerAG
import Arrangement.FontLib
{-import Arrangement.ArrLayerTypes
import Arrangement.ArrLayerUtils

import Arrangement.ArrangerAG
import Arrangement.FontLib-}
import Data.IORef
import System.IO

import qualified Data.Map as Map
import Data.Map (Map)

arrangePresentation :: (Show node, Show token) => Settings ->
                       LocalStateArr -> FontMetricsRef -> FocusPres -> Arrangement node ->
                       DiffTree -> Layout doc enr node clip token -> IO (Arrangement node, LocalStateArr)
arrangePresentation settings state fontMetricsRef focus oldArrangement dt pres =

 do { viewedArea <- readIORef $ getViewedAreaRef state
    ; let oldViewedArea = getLastViewedArea state
          state' = state { getLastViewedArea = viewedArea }

          prunedPres = if arrangerIncrementality settings 
                       then prunePresentation viewedArea oldViewedArea dt pres
                       else prunePresentation viewedArea oldViewedArea (DiffLeaf False) pres
                       -- viewedArea is only passed to check if it changed, it is not
                       -- actually used in the pruning algorithm (since the presentation has no 
                       -- position & size information)
                            
    ; let ((x,y),(w,h)) = viewedArea
          extendedViewedArea = ( (clip 0 10000 (x- w `div` 4), clip 0 10000 (y- h `div` 4))
                               , (w+ w `div` 2, h + h `div` 2)
                               )           

    ; let ((x,y),(w,h)) = oldViewedArea
          extendedOldViewedArea = ( (clip 0 10000 (x- w `div` 4), clip 0 10000 (y- h `div` 4))
                               , (w+ w `div` 2, h + h `div` 2)
                               )          
    -- extension is to arrange a bit more for scrolling
    -- NOTE: in arrangerAG, the old width and height are computed, so changes here also require updating screenWidth/Height in arrangerAG
 
    ; debugLnIO Arr $ "Viewed area: "++show viewedArea ++ " last viewed area: "++show oldViewedArea
    ; debugLnIO Arr $ "Extended viewed area: "++show extendedViewedArea
--    ; debugLnIO Arr ("Diff tree"++show dt)
--    ; debugLnIO Arr ("Presentation"++show pres)
--    ; debugLnIO Arr ("Pruned Presentation"++show prunedPres)
--    ; debugLnIO Arr ("Old arrangement "++ show oldArrangement)

    ; (attrTree, idCounter', maxFDepth) <- fixed settings fontMetricsRef (getIDACounter state') focus prunedPres pres extendedViewedArea extendedOldViewedArea oldArrangement

    ; let state'' = state' { getIDACounter = idCounter' }
    ; when (maxFDepth > 1) $
        debugLnIO Err "Nested formatters may be arranged incorrectly"
    ; return (attrTree, state'')
    }


fixed :: (Show node, Show token) => Settings -> FontMetricsRef -> Int -> FocusPres -> Layout doc enr node clip token -> Layout doc enr node clip token -> Rectangle -> Rectangle -> 
         Arrangement node -> IO (Arrangement node, Int, Int)
fixed settings fontMetricsRef idACounter focus (pres :: Layout doc enr node clip token) (unprunedPres :: Layout doc enr node clip token) viewedArea oldViewedArea oldArrangement = 
 mdo { (fontMetrics,arrangement, idACounter', maxFDepth) <- f (fontMetrics,arrangement, idACounter, maxFDepth)
     ; return (arrangement, idACounter', maxFDepth)
     }
 
 where (focusMinPath,focusMaxPath) = 
         case focus of
                FocusP (PathP f _) (PathP t _) -> if f <= t then (f,t) else (t,f)
                _                              -> ([-1],[-1])
  
       f :: (FontMetrics, Arrangement node, Int, Int) ->
            IO (FontMetrics, Arrangement node, Int, Int) -- doc and node are scoped type variables
       f (fontMetrics,_, _, _) = 
         do { let (allFonts, arrangement, idACounter', maxFDepth,_) = -- _ is the self attribute
                    sem_Root (Root pres) [defaultFont]
                                               defaultBackColor defaultFillColor
                                               focusMinPath
                                               focusMaxPath
                                               defaultFont 
                                               fontMetrics
                                               idACounter
                                               defaultLineColor
                                               Nothing  -- mouseDown : Maybe (UpdateDoc doc clip)
                                               (Just oldArrangement)
                                               oldViewedArea
                                               defaultTextColor
                                               unprunedPres
                                               viewedArea
               
            ; let usedFonts = nub allFonts
-- When implementing cleanup of unused fonts (might not be necessary), note that pruned presentations
-- may yield incomplete list of fonts

--            ; debugLnIO Arr ("The fonts are:"++show usedFonts)
            ; queriedMetrics <- readIORef fontMetricsRef
            
            ; let queriedFonts = Map.keys queriedMetrics
            ; let newFonts =  deleteFirstsBy (==) usedFonts queriedFonts -- usedFonts was nubbed
{-
            ; debugLnIO Arr $ "used: "           ++ show usedFonts
            ; debugLnIO Arr $ "already queried: "++ show queriedFonts
            ; debugLnIO Arr $ "new:             "++ show newFonts
-}
            ; newMetrics <- mkFontMetrics settings newFonts
            ; let updatedMetrics = newMetrics `Map.union` queriedMetrics
            ; writeIORef fontMetricsRef updatedMetrics            
            ; return (updatedMetrics, arrangement, idACounter', maxFDepth)
            }
            
