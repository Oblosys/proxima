module Main where

import IOExts
import System
import Graphics.UI.ObjectIO

import Architecture

import GUI

import CommonTypes
import DocUtils -- for redirect

import PresPresent
import DocTypes
import PresTypes -- temporarily
import PresUtils -- temporarily
import LayTypes
import ArrTypes -- temporarily
import RenTypes -- temporarily
--import HeliumPlugin -- for debugging on command line
import FontLib  -- for initial FontMetrics, the Init should take care of this.
import ProxParser -- for debugging on command line and for initMap
import PresentationParsing hiding (walk) -- for debugging on command line and for initMap
import Layout hiding (combine)
import Scanner
import ArrUtils

tok str = let (toks,layoutmap,counter) = tokenize 0 Nothing . ParsingP NoIDP . StringP NoIDP $ str
          in  ParsePres toks

-- for profiling: use main' instead of main, and use alternative 'queryFont' in FontLib

gain = main -- when typing during compilation GHCI replaces the first command line char by 'g'


-- inital system local state in Main is not nice
main =                         -- system (layer)local state,  initial higher level value
 do { fontMetricsRef <- initFontMetrics
    ; let layers = 
            proximaLayers (([],[],[]),     DocumentLevel (error "document not initialized") NoPathD Clip_Nothing)   
                          (EmptyP NoIDP,   PresentationLevel (EmptyP NoIDP) (initLayout,0, [IDP 1, IDP 2], emptyFM))   
                          (fontMetricsRef, LayoutLevel (EmptyP NoIDP) NoFocusP (DiffLeaf False))
                          ((),             ArrangementLevel (EmptyA NoIDA 0 0 0 0) NoFocusA (EmptyP NoIDP)) 
                               -- initial Rendering is given to startGUI.
    ; let TransStep translate = layers
                               
                                    
    ; args <- getArgs
    ; let initEvent = case args of 
                        []      -> InitRen
                        [fName] -> OpenFileRen fName
                        _       -> InitRen -- putStrLn $ "Usage: Proxima [filename]"
     
    
    ; stepRf <- newIORef translate
    
    ; let handler :: ((RenderingLevel, EditRendering) -> IO (RenderingLevel, EditRendering'))
          handler (renderingLvl, SkipRen 0) = return $ (renderingLvl, SkipRen' 0) -- just so unimportant events don't flood debugging traces
          handler (renderingLvl, event) =
           do {
--                debugLnIO Main $ "Rendering edit is "++show event

              ; translate <- readIORef stepRf
              
              ; ((doc, docEdit), PresStep present) <- translate (renderingLvl, event)
              
              ; debugLnIO Main $ "Doc edit is "++show docEdit
              ; ((renderingLvl', renderingEdit'), TransStep translate') <- present (doc, (redirect docEdit))
--              ; debugLnIO Main $ "RenderingLevel is "++show renderingLvl'
--              ; debugLnIO Main $ "RenderingEdit' is "++show renderingEdit'
              
              ; writeIORef stepRf translate'
              ; return $ (renderingLvl', renderingEdit')
              }
                      -- initial RenderingLevel 
    ; startGUI handler ( RenderingLevel 1.0 (\_ _ x y -> return ()) (return ()) (0,0) False  
                                            ( Rectangle (Point2 0 0) (Point2 0 0)
                                            , Rectangle (Point2 0 0) (Point2 0 0)
                                            , Rectangle (Point2 0 0) (Point2 0 0) )
                       , initEvent)


    }


{-

proximaLayers' presentationLS layoutLS arrangementLS =
            lift (wrap presentationLayer) presentationLS
  `combine` lift (wrap layoutLayer)       layoutLS
  `combine` lift (wrap arrangementLayer)  arrangementLS

-- system without renderer for profiling

main' =                         -- system local state,  initial value
 do { fontMetricsRef <- initFontMetrics
    ; let layers = proximaLayers' (([],[],[]),         DocumentLevel undefined NoPathD Clip_Nothing)
                                 (EmptyP NoIDP,        PresentationLevel (EmptyP NoIDP) (initLayout,0))   
                                 (fontMetricsRef,      LayoutLevel (EmptyP NoIDP) NoFocusP (DiffLeaf False))
                               -- initial ArrangementLevel is given to startGUI.
    ; let TransStep translate = layers
                               
                                    
    ; let initEvent = InitArr
    
    ; stepRf <- newIORef translate
    
    ; let handler :: ((ArrangementLevel, EditArrangement) -> IO (ArrangementLevel, EditArrangement'))
          handler (renderingLvl, SkipArr 0) = return $ (renderingLvl, SkipArr' 0) -- just so unimportant events don't flood debugging traces
          handler (renderingLvl, event) =
           do {
--                debugLnIO Main $ "Rendering edit is "++show event

              ; translate <- readIORef stepRf          -- initial RenderingLevel 

              ; ((doc, docEdit), PresStep present) <- translate (renderingLvl, event)
              
              ; debugLnIO Main $ "Doc edit is "++show docEdit
              ; ((renderingLvl', renderingEdit'), TransStep translate') <- present (doc, (redirect docEdit))
--              ; debugLnIO Main $ "RenderingLevel is "++show renderingLvl'
--              ; debugLnIO Main $ "RenderingEdit' is "++show renderingEdit'
              
              ; writeIORef stepRf translate'
              ; return $ (renderingLvl', renderingEdit')
              }
  ; return ()
  ; startGUI'' handler (  ArrangementLevel (EmptyA NoIDA 0 0 0 0) NoFocusA (EmptyP NoIDP)
                       , initEvent)


    }





startGUI'' :: ((ArrangementLevel, EditArrangement) -> IO (ArrangementLevel, EditArrangement')) -> (ArrangementLevel, EditArrangement) -> IO ()
startGUI'' handler (arrangementLvl, editArrangement) =
 do { (arrangementLvl', SetArr' ren') <- handler (arrangementLvl, editArrangement)
    ; (_, SetArr' arrLvl@(ArrangementLevel arr focus prs)) <- handler (ren', SetFocusArr (FocusA (PathA [0,0,0,0,0,0,0,0,0,0,0] 9) (PathA [0,0,0,0,0,0,0,0,0,0,0] 9)))
    ; putStrLn $ show (walk arr)
    ; (_, SetArr' arrLvl@(ArrangementLevel arr focus prs)) <- handler (arrLvl, KeyCharArr 'a')
    ; putStrLn $ show (length (show arr)) 

    ; arrLvl <- doo 0 arrLvl $ (\arrLvl ->
                     do { (_, SetArr' arrLvl@(ArrangementLevel arr focus prs)) <- handler (arrLvl, RightArr)
--                        ; deepSeqM arr
                        ; putStrLn $ show focus
                        ; return arrLvl
                        })
    ; arrLvl <- doo 30 arrLvl $ (\arrLvl ->
                     do { (_, SetArr' arrLvl@(ArrangementLevel arr focus prs)) <- handler (arrLvl, KeyCharArr 'a')
                        --; deepSeqM arr
                        ; putStrLn $ show (walk arr)
                        ; putStrLn $ show focus
                        ; (_, SetArr' arrLvl@(ArrangementLevel arr focus prs)) <- handler (arrLvl, NormalizeArr)
                        --; deepSeqM arr
                        ; putStrLn $ show (walk arr)
                        ; putStrLn $ show focus
                        ; return arrLvl
                        })
    ; putStrLn $ show (length (show arrLvl))

    ; return ()
    }
doo n x act = doo' n x 
 where doo' 0 x = return x
       doo' n x = do { x' <- act x
                     ; doo' (n-1) x'
                     }

-}

{- on all layers:
group edit commands that have similar behaviour, the data structure will be neater and common behaviour
can more easily be factorized.

where do we translate? descaling and translating to arr path is both in renderer, but this is probably right

what about rejection of edit ops at higher levels leading to edit ops on lower levels, (structure vs pres edit)
or even on both? (click changes focus and leads to presentation change, eg. click/move into squigglied word presents
its error message somewhere)

cycles and shortcuts: parse only if space or navigation is performed? Then navigation is slow, so only parsed if 
something has changed and navigation is performed. Does this fit?

-- current arch does not have threaded horiz. args for incremental data stuff
rework architecture. Thread doc and doclocalstate

document to pres focus and back?


BUG:  row [row[]] does not present right


BUG: row [] does crashes on navigation. wortkaround: add empty, row [empty] is ok.


-}
