module Proxima where
 
import Data.IORef
import System

import Architecture

import GUIGTK

import CommonTypes
import DocTypes
import DocUtils -- for redirect

import PresPresent
import EnrTypes
import Graphics.UI.Gtk
--import Graphics.UI.WX.Types hiding (Size)
import PresTypes -- temporarily
import PresUtils -- temporarily
import LayTypes
import ArrTypes -- temporarily
--import RenTypesGTK -- temporarily
import RenTypes -- temporarily
--import HeliumPlugin -- for debugging on command line
--import FontLibGTK  -- for initial FontMetrics, the Init should take care of this.
import FontLib  -- for initial FontMetrics, the Init should take care of this.

import EvalLayerTypes (EvaluationSheet, ReductionSheet)
import PresLayerTypes (PresentationSheet, ParseSheet)
import LayLayerTypes (ScannerSheet)


import PresentationParsing
import Layout hiding (combine)
import ArrUtils
import EvalLayerTypes (LayerStateEval (..))

import qualified Data.Map as Map
import Data.Map (Map)


--tok str = let (toks,layoutmap,counter) = tokenize 0 Nothing . ParsingP NoIDP . StringP NoIDP $ str
--          in  ParsePres toks




-- incrementality:
-- renPresent   render (SetArr ..) ..  diffTree    = DiffLeaf False   
--                                             ->  diffTree    = diffArr arr' arrOld
       
-- layPresent: present (SkipPres ..) .. diffTree = DiffLeaf False
--                                                -> diffTree = dt

-- for profiling: use main' instead of main, and use alternative 'queryFont' in FontLib



-- initial system local state in Main is not nice

{-
proxima :: PresentationSheet doc enr node -> ParseSheet doc enr node ->
           ScannerSheet doc node ->
           DocumentLevel doc clip -> EnrichedDocLevel enr ->
           IO ()
-}
proxima evaluationSheet reductionSheet presentationSheet parseSheet scannerSheet
        initDoc initEnr =
 do { fontMetricsRef <- initFontMetrics
    ; let layers = 
            proximaLayers evaluationSheet reductionSheet presentationSheet parseSheet scannerSheet
                          (LayerStateEval, initDoc)   
                          ((),     initEnr)
                          (EmptyP NoIDP,   PresentationLevel (EmptyP NoIDP) (initLayout,0, [IDP 1, IDP 2], Map.empty))   
                          (fontMetricsRef, LayoutLevel (EmptyP NoIDP) NoFocusP (DiffLeaf False))
                          ((),             ArrangementLevel (EmptyA NoIDA 0 0 0 0 0 0) NoFocusA (EmptyP NoIDP)) 
                          -- system (layer)local state,  initial higher level value
                        
                          -- initial Rendering is given to startGUI.
                               
                               -- maybe better to do init stuff at handling of Init.. edit command in translate modules
    ; let TransStep translate = layers
    ; let initEvent = InitRen
{-                                    
    ; args <- getArgs
    ; let initEvent = case args of 
                        []      -> InitRen
                        [fName] -> OpenFileRen fName
                        _       -> InitRen -- putStrLn $ "Usage: Proxima [filename]"
-}     
    
    ; stepRf <- newIORef translate
    
    ; let --handler :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel))
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
    ; startGUI handler ( RenderingLevel 1.0 (\_ _ _ x y -> return ()) (\_ -> return ()) (0,0) False  
                                            ( Rectangle 0 0 0 0
                                            , Rectangle 0 0 0 0
                                            , Rectangle 0 0 0 0)
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


Parse always before doing higher level op!! After pres edit, locators can be completely wrong.

what about rejection of edit ops at higher levels leading to edit ops on lower levels, (structure vs pres edit)
or even on both? (click changes focus and leads to presentation change, eg. click/move into squigglied word presents
its error message somewhere)

cycles and shortcuts: parse only if space or navigation is performed? Then navigation is slow, so only parsed if 
something has changed and navigation is performed. Does this fit?


idPs in doc: how to store?? Now datatype changes when pres is modified, which is not good

Popups in doc level? or in enriched doc level? It should be possible to specify doc updates, without having 
to specify them as enriched doc updates and providing a translation from enriched to doc


Selections/focus.  What about left clicking on objects without presentation (eg AppExp), place left click as high as possible in the
tree, while disregarding whitespace?


-- current arch does not have threaded horiz. args for incremental data stuff
rework architecture. Thread doc and doclocalstate

document to pres focus and back?

BUG: navigating from the right into row' [ text @string, empty ]   crashes with Prelude.last empty list
BUG:  row [row[]] does not present right


BUG: row [] does crashes on navigation. wortkaround: add empty, row [empty] is ok.



Layout of type sigs and value comments is not right. Maybe some chaining rule failed
after adding enriched document stuff?


parse after pres edit of ident list destroys the layout seems logical, as tokens are
no longer there to be reused. what to do about it?


-}
