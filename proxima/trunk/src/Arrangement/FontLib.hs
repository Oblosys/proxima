module Arrangement.FontLib where

import Common.CommonTypes
import Common.CommonUtils
import Graphics.UI.Gtk hiding (FontMetrics)

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Array

import Char
import Data.IORef
import System.IO
import Data.Maybe

--import Graphics.UI.WX
--import Graphics.UI.WXCore hiding (Font)

-- use different structure to make lookup more efficient? Or is this a waste of time
type FontMetrics = Map Font (Int, Int, Array Int Int)

type FontMetricsRef = IORef FontMetrics

newFontMetricsRef :: IO FontMetricsRef
newFontMetricsRef = newIORef Map.empty 

initFontMetrics :: IO FontMetricsRef
initFontMetrics = newFontMetricsRef
   
-- Because Underline and strikeOut have no influence on the metrics, all
-- fonts are stored in the Map with these attributes set to False.
mkFontMetrics :: [Font] -> IO FontMetrics
mkFontMetrics fonts = mkFontMetricsHTML fonts {-
  fmap Map.fromList $ mapM mkFontMetric fonts
 where mkFontMetric font = 
        do { (f,(h, b, ws)) <- queryFont font
           ; return $ (f {fUnderline = False, fStrikeOut = False}, (h, b, listArray (0,223) ws)) 
           }
           -}
-- | Lookup the metrics for font. Because Underline and strikeOut have no influence on the metrics, all 
-- fonts are stored in the Map with these attributes set to False.
metricsLookup :: Font -> FontMetrics -> (Int, Int, Array Int Int)
metricsLookup font fontMetrics = 
  -- debug Err ("looking up: " ++ show (fSize font) ++ " " ++ (fFamily font)) $
  case Map.lookup (font {fUnderline = False, fStrikeOut = False}) fontMetrics  of
            Just metrics -> metrics
            Nothing      -> {- debug Err "metrics for font not queried" $ -} (20,15, listArray (0,223) (repeat 10))

--- query the metrics for font. 
queryFont :: Font -> IO (Font,(Int, Int, [Int]))
queryFont font =
 do { --debugLnIO Arr $ "Querying: " ++ show (fSize font) ++ " " ++ (fFamily font)
    ; context <- cairoCreateContext Nothing
    ; language <- contextGetLanguage context
    ; fontDescription <- fontDescriptionFromProximaFont font
    
    ; let allChars = map chr [32..255]
    ; widths <- mapM (\c -> do { pangoItems <- pangoItemize context [c] [ AttrFontDescription 0 255 fontDescription]
                               ; glyphItem <- pangoShape (head' "Fontlib.queryFont" pangoItems)
                               ; widths <- glyphItemGetLogicalWidths glyphItem (Just False)
                               ; return (round $ head' "Fontlib.queryFont" widths)
                               })
                     allChars
    
    ; metrics <- contextGetMetrics context fontDescription language
  
    
    ; let ascnt = round $ ascent metrics    
          dscnt = round $ descent metrics
          hght = ascnt + dscnt
    

    ; debugLnIO Arr $      "Metrics for: "++show font 
                        ++ "\nascent:   " ++ show ascnt
                        ++ "\ndescent:  " ++ show dscnt
                        ++ "\nheight: " ++ show hght
    ; debugLnIO Arr $    "\nwidths:   " ++ show widths  
   

    ; return (font, (hght,ascnt,widths))
    }
    
fontDescriptionFromProximaFont :: Font -> IO FontDescription
fontDescriptionFromProximaFont (Font fFam fSiz fBld fUnderln fItlc fStrkt) =
 do { fontDescription <- fontDescriptionNew    
    ; fontDescriptionSetFamily fontDescription fFam
    ; fontDescriptionSetStyle fontDescription (if fItlc then StyleItalic else StyleNormal) -- check if the font has italic or oblique?
    ; fontDescriptionSetVariant fontDescription VariantNormal
    ; fontDescriptionSetWeight fontDescription (if fBld then WeightBold else WeightNormal)
    ; fontDescriptionSetStretch fontDescription StretchNormal
    ; fontDescriptionSetSize fontDescription (fromIntegral fSiz)
    ; return fontDescription
    }

forceEval :: Show a => a -> IO ()
forceEval a = seq (last (show a)) (return ())


textWidth :: FontMetrics -> Font -> String -> Int
textWidth fms f str = let (h,b,ws) = metricsLookup f fms
                          toWidth c = let i = ord c 
                                      in  if i < 32 then 0 else ws ! (ord c - 32)
                      in sum (map toWidth str)
-- round (fromInt (length str) * charWidth fs)

-- Is it accurate enough to add the widhts of the characters? The width of the string might
-- be different due to rounding errors. We could use a stringwidth function, but this results 
-- in more communication with the renderer.
cumulativeCharWidths :: FontMetrics -> Font -> String -> [Int]
cumulativeCharWidths fms f str = let (h,b,ws) = metricsLookup f fms
                                     toWidth c = let i = ord c 
                                                 in  if i < 32 then 0 else ws ! (ord c - 32)
                                 in  scanl (+) 0 (map toWidth str)

charHeight :: FontMetrics -> Font -> Int
charHeight fms f  = let (h,b,ws) = metricsLookup f fms
                    in  (h)

baseLine :: FontMetrics -> Font -> Int
baseLine fms f = let (h,b,ws) = metricsLookup f fms
                 in  (b)




-- first time, fonts are not present and are put in request.txt
-- second time, fonts will be absent again, and
mkFontMetricsHTML :: [Font] -> IO FontMetrics
mkFontMetricsHTML fonts =
 do { putStrLn "Before reading queriedMetrics.txt"
    ; fh <- openFile "queriedMetrics.txt" ReadMode -- readFile and seq gives problems when clearing it in GUI.hs
    ; queriedFontsTxt <- hGetContents fh 
    ; seq (length queriedFontsTxt) $ return ()
    ; hClose fh
    ; let queriedFonts :: [((String, Int),(Int,Int,[Int]))]
            = map read $ lines queriedFontsTxt
    ; let alreadyQueried = catMaybes $ map (lookupFont queriedFonts) fonts
    
    ; pendingQueriesTxt <-  readFile "metricsQueries.txt"
    ; seq (length pendingQueriesTxt) $ return ()
    ; let pendingQueries = map read $ lines pendingQueriesTxt 
          newQueries  = fonts \\ (map fst alreadyQueried)
          queryTuples = [ (fFamily font, fSize font) | font <- newQueries ]
          newQueryTuples = queryTuples \\ pendingQueries
          
    ; fh' <- openFile "metricsQueries.txt" AppendMode
    ; hPutStr fh' $ unlines (map show newQueryTuples)
    ; hClose fh'

    ; return $ Map.fromList $ map mkFontMetric alreadyQueried
    }
 where mkFontMetric (f,(h,b,ws)) = 
         (f {fUnderline = False, fStrikeOut = False}, (h, b, listArray (0,223) [ w `div` 10 | w <- ws])) 
       lookupFont queries font = case lookup (fFamily font, fSize font)  queries of
                                   Nothing -> Nothing
                                   Just metrics -> Just (font, metrics)
       