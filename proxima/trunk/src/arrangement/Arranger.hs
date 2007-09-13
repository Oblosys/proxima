module Arranger where

import CommonTypes hiding (defaultFont)
import qualified CommonTypes
import ArrLayerTypes
import ArrLayerUtils

import ArrangerAG
--import FontLibGTK
import FontLib
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
                       DiffTree -> Presentation doc node clip -> IO (Arrangement node, LocalStateArr)
arrangePresentation state fontMetricsRef focus oldArrangement dt pres =

 do { viewedArea <- readIORef $ getViewedAreaRef state
    ; let lastViewedArea = getLastViewedArea state
          state' = state { getLastViewedArea = viewedArea }
--          prunedPres = prunePres viewedArea lastViewedArea (0,0) dt oldArrangement pres
          prunedPres = prunePres dt pres
    --; putStrLn $ "Viewed area: "++show viewedArea ++ " last viewed area: "++show lastViewedArea
    --; debugLnIO Err ("Diff tree"++show dt)
    --; debugLnIO Err ("Presentation"++show pres)
    --; debugLnIO Err ("Pruned Presentation"++show prunedPres)
    ; (attrTree, maxFDepth) <- fixed fontMetricsRef focus prunedPres pres viewedArea lastViewedArea oldArrangement
    ; if maxFDepth > 1 
      then debugLnIO Err "Nested formatters may be arranged incorrectly"
      else return ()
    ; return (attrTree, state')
    }

fixed :: Show node => FontMetricsRef -> FocusPres -> Presentation doc node clip -> Presentation doc node clip -> Rectangle -> Rectangle -> 
         Arrangement node -> IO (Arrangement node, Integer)
fixed fontMetricsRef focus (pres :: Presentation doc node clip) (unprunedPres :: Presentation doc node clip) viewedArea oldViewedArea oldArrangement = 
 mdo { (fontMetrics,arrangement, maxFDepth) <- f (fontMetrics,arrangement, maxFDepth)
    ; return (arrangement, maxFDepth)
    }
 where f :: (FontMetrics, Arrangement node, Integer) ->
            IO (FontMetrics, Arrangement node, Integer) -- doc and node are scoped type variables
       f (fontMetrics,_, _) = 
         do { let (allFonts, arrangement, maxFDepth,_) = -- _ is the self attribute
                    sem_Root (Root pres) [defaultFont]
                                               defaultBackColor defaultFillColor
                                               focus
                                               defaultFont 
                                               fontMetrics
                                               defaultLineColor
                                               Nothing  -- mouseDown : Maybe (UpdateDoc doc clip)
                                               oldArrangement
                                               oldViewedArea
                                               []       -- popupMenu : [String, (UpdateDoc doc clip)] 
                                               defaultTextColor
                                               unprunedPres
                                               viewedArea
               
            ; let usedFonts = nub allFonts
             
           -- ; debugLnIO Arr ("The fonts are:"++show usedFonts)
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
            
