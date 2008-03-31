module Arrangement.Arranger where

import Common.CommonTypes hiding (defaultFont)
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

defaultBackColor = transparent
defaultFillColor = white
defaultLineColor = black
defaultTextColor = black
defaultFont = CommonTypes.defaultFont

arrangePresentation :: Show node => LocalStateArr -> FontMetricsRef -> FocusPres -> Arrangement node ->
                       DiffTree -> Layout doc node clip token -> IO (Arrangement node, LocalStateArr)
arrangePresentation state fontMetricsRef focus oldArrangement dt pres =

 do { viewedArea <- readIORef $ getViewedAreaRef state
    ; let oldViewedArea = getLastViewedArea state
          state' = state { getLastViewedArea = viewedArea }
--          prunedPres = prunePres viewedArea lastViewedArea (0,0) dt oldArrangement pres -- old prune

          prunedPres = if arrangerIncrementality 
                       then prunePresentation viewedArea oldViewedArea dt pres  -- uncomment this line for incremental arrangement
                       else prunePresentation viewedArea oldViewedArea (DiffLeaf False) pres -- and this one for non-incremental
          
    ; debugLnIO Err $ "Viewed area: "++show viewedArea ++ " last viewed area: "++show oldViewedArea
--    ; debugLnIO Err ("Diff tree"++show dt)
--    ; debugLnIO Err ("Presentation"++show pres)
--    ; debugLnIO Err ("Pruned Presentation"++show prunedPres)
--    ; debugLnIO Arr ("Old arrangement "++ show oldArrangement)
    
    ; (attrTree, maxFDepth) <- fixed fontMetricsRef focus prunedPres pres viewedArea oldViewedArea oldArrangement
    ; when (maxFDepth > 1) $
        debugLnIO Err "Nested formatters may be arranged incorrectly"
    ; return (attrTree, state')
    }

fixed :: Show node => FontMetricsRef -> FocusPres -> Layout doc node clip token -> Layout doc node clip token -> Rectangle -> Rectangle -> 
         Arrangement node -> IO (Arrangement node, Int)
fixed fontMetricsRef focus (pres :: Layout doc node clip token) (unprunedPres :: Layout doc node clip token) viewedArea oldViewedArea oldArrangement = 
 mdo { (fontMetrics,arrangement, maxFDepth) <- f (fontMetrics,arrangement, maxFDepth)
    ; return (arrangement, maxFDepth)
    }
 where f :: (FontMetrics, Arrangement node, Int) ->
            IO (FontMetrics, Arrangement node, Int) -- doc and node are scoped type variables
       f (fontMetrics,_, _) = 
         do { let (allFonts, arrangement, maxFDepth,_) = -- _ is the self attribute
                    sem_Root (Root pres) [defaultFont]
                                               defaultBackColor defaultFillColor
                                               focus
                                               defaultFont 
                                               fontMetrics
                                               defaultLineColor
                                               Nothing  -- mouseDown : Maybe (UpdateDoc doc clip)
                                               (Just oldArrangement)
                                               oldViewedArea
                                               []       -- popupMenu : [String, (UpdateDoc doc clip)] 
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
            ; newMetrics <- mkFontMetrics newFonts
            ; let updatedMetrics = newMetrics `Map.union` queriedMetrics
            ; writeIORef fontMetricsRef updatedMetrics            
            ; return (updatedMetrics, arrangement, maxFDepth)
            }
            
