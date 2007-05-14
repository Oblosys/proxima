module Arranger where

import CommonTypes
import ArrLayerTypes
import ArrLayerUtils

import ArrangerAG
import FontLib
import Data.IORef

import qualified Data.Map as Map
import Data.Map (Map)

arrangePresentation :: Show node => FontMetricsRef -> FocusPres -> Arrangement node ->
                       DiffTree -> Presentation doc node clip -> IO (Arrangement node)
arrangePresentation fontMetricsRef focus oldArrangement dt pres = -- return $ sel $ dummyArr  undefined undefined undefined undefined undefined undefined undefined undefined pres

 do { let screenSize = 1000      
    ; let pres' = prunePres dt pres
  --  ; debugLnIO Err ("Diff tree"++show dt)
  --  ; debugLnIO Err ("pruned presentation"++show pres')
    ; (attrTree, maxFDepth, unfoldedTree) <- fixed fontMetricsRef focus pres' screenSize oldArrangement
 -- ; debugLnIO Arr ("  maxFormatterDepth = "++ show maxFDepth)   
    ; if maxFDepth == 0 then
        return attrTree
      else if maxFDepth == 1 
      then 
       do { --debugLnIO Arr "Unfolding formatters"
           (arrangement, maxFDepth, unfoldedTree) <- fixed fontMetricsRef focus unfoldedTree screenSize oldArrangement
          ; return arrangement
          }
      else 
        debug Err "no nested formatters allowed yet" (return attrTree)
 
   
    }

-- non-pure font queries mess up this computation. Using a fixIO does not work because we are in the IO monad, and
-- unsafePerformDraw is not available     -- obsolete comment
-- Monad is IO again so fixIO can be used
fixed :: Show node => FontMetricsRef -> FocusPres -> Presentation doc node clip -> Int ->
         Arrangement node -> IO (Arrangement node, Integer, Presentation doc node clip)
fixed fontMetricsRef focus (pres :: Presentation doc node clip) screenSize oldArrangement = f --fixit
 where f :: IO (Arrangement node, Integer, Presentation doc node clip) -- doc and node are scoped type variables
       f = 
         do { let (defBackColor, defFillColor, defLineColor, defTextColor) = (transparent, white, black, black)
            ; let defFont = defaultFont 
         
            ; -- debugLnIO Arr ("Start collecting fonts")
            ; let (allFonts, _, _, _) =
                    sem_Root (Root pres) [defFont]
                                               defBackColor defFillColor
                                               focus
                                               defFont 
                                               (error "font computation depends on font metrics")
                                               defLineColor
                                               Nothing  -- mouseDown : Maybe (UpdateDoc doc clip)
                                               oldArrangement
                                               []       -- popupMenu : [String, (UpdateDoc doc clip)] 
                                               screenSize 
                                               defTextColor
                                               
            ; let usedFonts = nub allFonts
            ; seq (length allFonts) $ return ()
           -- ; debugLnIO Arr ("Done collecting fonts")
             
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
            ; metrics <- mapM queryFont newFonts

            ; let updatedMetrics = mkFontMetrics metrics `Map.union` queriedMetrics
            ; writeIORef fontMetricsRef updatedMetrics
            
            ; let (_, arrangement,  maxFDepth, unfoldedTree) =
                    sem_Root (Root pres) [defFont]
                                          defBackColor defFillColor
                                          focus
                                          defFont
                                          updatedMetrics
                                          defLineColor
                                          Nothing
                                          oldArrangement
                                          []
                                          screenSize 
                                          defTextColor
                                              
            ; return (arrangement, maxFDepth, unfoldedTree)
            }
            
            
-- just generating an arrangement from the presentation takes almost no time, so the expenses
-- in the arrangementAG must come from somewhere else
dummyArr _ _ _ _ _ _ _ _ (EmptyP id)            = tup $ EmptyA (idAFromP id) 0 0 0 0 0 0
dummyArr _ _ _ _ _ _ _ _ (StringP id str)       = tup $ StringA (idAFromP id) 0 0 20 20 0 0 str black defaultFont []
dummyArr _ _ _ _ _ _ _ _ (RectangleP id _ _ lw) = tup $ RectangleA (idAFromP id) 0 0 20 20 0 0 lw Solid black white
dummyArr _ _ _ _ _ _ _ _ (ImageP id src)        = tup $ ImageA (idAFromP id) 0 0 20 20 0 0 src Tile black white
dummyArr _ _ _ _ _ _ _ _ (PolyP id pl lw)       = let mkPoint (rx, ry) = ( round (rx * fromIntegral 10)
			                                   , round (ry * fromIntegral 10) )
		                          in tup $   PolyA (idAFromP id) 0 0 20 20 0 0 (map mkPoint pl) lw black white
		      
dummyArr _ _ _ _ _ _ _ _ (RowP id rf prs)       = tup $ RowA (idAFromP id) 0 0 20 20 0 0 white (sel $ dummyArrs undefined undefined undefined undefined undefined undefined undefined undefined prs)
dummyArr _ _ _ _ _ _ _ _ (ColP id rf prs)       = tup $ ColA (idAFromP id) 0 0 20 20 0 0 white (sel $ dummyArrs undefined undefined undefined undefined undefined undefined undefined undefined prs)
dummyArr _ _ _ _ _ _ _ _ (OverlayP id prs)      = tup $ OverlayA (idAFromP id) 0 0 20 20 0 0 white (sel $ dummyArrs undefined undefined undefined undefined undefined undefined undefined undefined prs)
dummyArr _ _ _ _ _ _ _ _ (StructuralP id child) = tup $ StructuralA (idAFromP id) (sel $ dummyArr undefined undefined undefined undefined undefined undefined undefined undefined child)
dummyArr _ _ _ _ _ _ _ _ (ParsingP id child)    = tup $ ParsingA (idAFromP id) (sel $ dummyArr undefined undefined undefined undefined undefined undefined undefined undefined child)
dummyArr _ _ _ _ _ _ _ _ (LocatorP loc child)   = tup $ LocatorA loc (sel $ dummyArr undefined undefined undefined undefined undefined undefined undefined undefined child)
dummyArr _ _ _ _ _ _ _ _ (WithP _ child)        = tup $ sel $ dummyArr undefined undefined undefined undefined undefined undefined undefined undefined child


dummyArrs _ _ _ _ _ _ _ _ [] = tup []
dummyArrs _ _ _ _ _ _ _ _ (prs:prss) = let arr  = sel $ dummyArr undefined undefined undefined undefined undefined undefined undefined undefined prs
                                           arrs = sel $ dummyArrs undefined undefined undefined undefined undefined undefined undefined undefined prss
                                       in tup $ arr:arrs 
                       
tup x = (undefined,undefined,undefined,undefined,undefined,undefined,undefined,undefined,x )
sel (_,_,_,_,_,_,_,_,x) = x 