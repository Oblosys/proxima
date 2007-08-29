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
          pres' = prunePres viewedArea lastViewedArea (0,0) dt oldArrangement pres
    --; putStrLn $ "Viewed area: "++show viewedArea ++ " last viewed area: "++show lastViewedArea
    --; debugLnIO Err ("Diff tree"++show dt)
    --; debugLnIO Err ("Presentation"++show pres)
    --; debugLnIO Err ("Pruned Presentation"++show pres')
    ; (attrTree, maxFDepth, unfoldedTree) <- fixed fontMetricsRef focus pres' viewedArea oldArrangement
    ; debugLnIO Arr ("  maxFormatterDepth = "++ show maxFDepth)   
    ; debugLnIO Err ("Unfolded presentation"++show unfoldedTree)
    ; print attrTree      
    ; if maxFDepth == 0 then
        return (attrTree, state')
      else if maxFDepth == 1 
      then 
       do { (arrangement, maxFDepth, unfoldedTree) <- fixed fontMetricsRef focus unfoldedTree viewedArea oldArrangement
          ; return (arrangement, state')
          }
      else 
        debug Err "no nested formatters allowed yet" (return (attrTree, state'))
 
   
    }

fixed :: Show node => FontMetricsRef -> FocusPres -> Presentation doc node clip -> Rectangle -> 
         Arrangement node -> IO (Arrangement node, Integer, Presentation doc node clip)
fixed fontMetricsRef focus (pres :: Presentation doc node clip) viewedArea oldArrangement = f
 where --f :: [Font] -> IO ([Font], Arrangement node, Integer, Presentation doc node clip) -- doc and node are scoped type variables
       f = 
         do { debugLnIO Arr ("Start collecting fonts")
            ; let (allFonts, arrangement, maxFDepth, unfoldedTree) =
                    sem_Root (Root pres) [defaultFont]
                                               defaultBackColor defaultFillColor
                                               focus
                                               defaultFont 
                                               Map.empty
                                               defaultLineColor
                                               Nothing  -- mouseDown : Maybe (UpdateDoc doc clip)
                                               oldArrangement
                                               []       -- popupMenu : [String, (UpdateDoc doc clip)] 
                                               defaultTextColor
                                               viewedArea
               
            ; let usedFonts = nub allFonts
            ; seq (length allFonts) $ return ()
            ; debugLnIO Arr ("Done collecting fonts")
             
           -- ; debugLnIO Arr ("The fonts are:"++show usedFonts)
            ; queriedMetrics <- readIORef fontMetricsRef
            
            ; let queriedFonts = Map.keys queriedMetrics
            ; let newFonts =  deleteFirstsBy (==) usedFonts queriedFonts -- usedFonts was nubbed
{-
            ; debugLnIO Arr $ "used: "           ++ show usedFonts
            ; debugLnIO Arr $ "already queried: "++ show queriedFonts
            ; debugLnIO Arr $ "new:             "++ show newFonts
-}
            -- filter the ones that are already present

            ; newMetrics <- mkFontMetrics newFonts
            ; let updatedMetrics = newMetrics `Map.union` queriedMetrics
            ; writeIORef fontMetricsRef updatedMetrics
            ; putStrLn "Done querying"
            
            ; let (allFonts, arrangement,  maxFDepth, unfoldedTree) =
                    sem_Root (Root pres) [defaultFont]
                                          defaultBackColor defaultFillColor
                                          focus
                                          defaultFont
                                          updatedMetrics
                                          defaultLineColor
                                          Nothing
                                          oldArrangement
                                          []
                                          defaultTextColor
                                          viewedArea 
                                          
            ; return (arrangement, maxFDepth, unfoldedTree)
            }
            
{-
fixed :: Show node => FontMetricsRef -> FocusPres -> Presentation doc node clip -> Rectangle -> 
         Arrangement node -> IO (Arrangement node, Integer, Presentation doc node clip)
fixed fontMetricsRef focus (pres :: Presentation doc node clip) viewedArea oldArrangement = 
 mdo { (fontMetrics,arrangement, maxFDepth, unfoldedTree) <- f (fontMetrics,arrangement, maxFDepth, unfoldedTree)
    ; return (arrangement, maxFDepth, unfoldedTree)
    }
 where --f :: [Font] -> IO ([Font], Arrangement node, Integer, Presentation doc node clip) -- doc and node are scoped type variables
       f (fontMetrics,_, _, _) = 
         do { debugLnIO Arr ("Start collecting fonts")
            ; let (allFonts, arrangement, maxFDepth, unfoldedTree) =
                    sem_Root (Root pres) [defaultFont]
                                               defaultBackColor defaultFillColor
                                               focus
                                               defaultFont 
                                               fontMetrics
                                               defaultLineColor
                                               Nothing  -- mouseDown : Maybe (UpdateDoc doc clip)
                                               oldArrangement
                                               []       -- popupMenu : [String, (UpdateDoc doc clip)] 
                                               defaultTextColor
                                               viewedArea
               
            ; let usedFonts = nub allFonts
            ; seq (length allFonts) $ return ()
            ; debugLnIO Arr ("Done collecting fonts")
             
           -- ; debugLnIO Arr ("The fonts are:"++show usedFonts)
            ; queriedMetrics <- readIORef fontMetricsRef
            
            ; let queriedFonts = Map.keys queriedMetrics
            ; let newFonts =  deleteFirstsBy (==) usedFonts queriedFonts -- usedFonts was nubbed
{-
            ; debugLnIO Arr $ "used: "           ++ show usedFonts
            ; debugLnIO Arr $ "already queried: "++ show queriedFonts
            ; debugLnIO Arr $ "new:             "++ show newFonts
-}
            -- filter the ones that are already present

            ; newMetrics <- mkFontMetrics newFonts
            ; let updatedMetrics = newMetrics `Map.union` queriedMetrics
            ; writeIORef fontMetricsRef updatedMetrics
            ; putStrLn "Done querying"
            
            ; let (allFonts, arrangement,  maxFDepth, unfoldedTree) =
                    sem_Root (Root pres) [defaultFont]
                                          defaultBackColor defaultFillColor
                                          focus
                                          defaultFont
                                          updatedMetrics
                                          defaultLineColor
                                          Nothing
                                          oldArrangement
                                          []
                                          defaultTextColor
                                          viewedArea 
                                          
            ; return (fontMetrics, arrangement, maxFDepth, unfoldedTree)
            }
            

-}