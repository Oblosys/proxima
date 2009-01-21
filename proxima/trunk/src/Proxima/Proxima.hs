module Proxima.Proxima where
 
import Data.IORef
import System
import Control.Exception

import Proxima.Architecture

--import GUIGTK
import Proxima.GUI

import Proxima.Wrap

import Common.CommonTypes
import Evaluation.DocTypes
import Evaluation.DocUtils -- for redirect

import Presentation.PresPresent
import Evaluation.EnrTypes
--import Graphics.UI.WX.Types hiding (Size)

import Presentation.PresTypes -- temporarily
import Presentation.PresUtils -- temporarily
import Layout.LayTypes
import Arrangement.ArrTypes -- temporarily
import Arrangement.ArrLayerTypes -- for initial extra state
--import RenTypesGTK -- temporarily
import Rendering.RenTypes -- temporarily
--import HeliumPlugin -- for debugging on command line
--import FontLibGTK  -- for initial FontMetrics, the Init should take care of this.
import Arrangement.FontLib  -- for initial FontMetrics, the Init should take care of this.

import Evaluation.EvalLayerTypes (EvaluationSheet, ReductionSheet (..))
import Evaluation.DocumentEdit
import Presentation.PresLayerTypes (PresentationSheet, ParseSheet)
import Layout.LayLayerTypes (ScannerSheet)


import Presentation.PresentationParsing
import Layout.Layout hiding (combine)
import Arrangement.ArrUtils
import Evaluation.EvalLayerTypes (LayerStateEval (..))

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


proxima :: ( DocNode node, Show token, Ord token, Show enr, Doc doc, Clip clip
           , EvaluationSheet doc enr clip, ReductionSheet doc enr clip
           , Editable doc doc node clip token) =>
           Settings ->
           PresentationSheet doc enr node clip token -> ParseSheet doc enr node clip token ->
           ScannerSheet doc node clip token ->
           DocumentLevel doc clip -> EnrichedDocLevel enr doc ->
           IO ()
proxima settings presentationSheet parseSheet scannerSheet
        initDoc initEnr =
 do { fontMetricsRef <- initFontMetrics
    ; viewedAreaRef <- newIORef ((0,0),(0,0)) -- shared by GUI and extra state on Arrangement layer
    ; let layers = 
            proximaLayers settings
                          presentationSheet parseSheet scannerSheet
                          (LayerStateEval [] [], initDoc)   
                          ((),     initEnr)
                          (EmptyP NoIDP,   PresentationLevel (EmptyP NoIDP) (initLayout,0))   
                          (LocalStateArr fontMetricsRef Nothing viewedAreaRef ((0,0),(0,0)) 0, LayoutLevel (EmptyP NoIDP) NoFocusP (DiffLeaf False))
                          ((), ArrangementLevel emptyA NoFocusA (EmptyP NoIDP)) 
                          -- (system (layer)local state,  initial higher level value)
                        
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
          handler (renderingLvl, SkipRen 0) = return $ (renderingLvl, [SkipRen' 0]) -- just so unimportant events don't flood debugging traces
          handler (renderingLvl, event) =
           do {
--                debugLnIO Main $ "Rendering edit is "++show event

              ; translate <- readIORef stepRf
              
              ; (renderingLvl', renderingEdits, translate') <- 
                   performEditCycles translate renderingLvl [event]             
              
              ; writeIORef stepRf translate'
              ; return $ (renderingLvl', renderingEdits)
              }
           where performEditCycles translate renderingLvl [] = return (renderingLvl, [], translate)
                 performEditCycles translate renderingLvl (event:events) =
                  do {  ((doc, docEdits), PresStep present) <- translate (renderingLvl, [event])             
                     ; ((renderingLvl', renderingEdit':newEvents), TransStep translate') <- present (doc, (redirect docEdits))
                     ; (renderingLvl'', renderingEdits, translate'') <-
                         performEditCycles translate' renderingLvl' (map cast newEvents ++ events)
                     ; return (renderingLvl'', renderingEdit':renderingEdits, translate'')
                     }
                      -- initial RenderingLevel 
    ; startGUI settings handler viewedAreaRef
                       ( RenderingLevel 1.0 (\_ _ _ _ _ _ _ x y -> return Nothing) (\_ _ -> []) (\_ _ -> return ()) (\_ _ -> return ()) (\_ -> return ()) (\_ -> return ()) (0,0) False  
                                            []
                                            False
                       , initEvent)


    }
 `Control.Exception.catch`
   \err -> 
    do { putStrLn "\n\n\nProxima terminated abnormally:\n" 
       ; print err
       ; putStrLn "\n<Press return to exit>"
       ; getLine
       ; return ()
       } -- This way, the dos window on Windows does not exit until the user can see the error.
       

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
