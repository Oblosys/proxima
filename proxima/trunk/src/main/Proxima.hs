module Proxima where
 
import Data.IORef
import System
import Control.Exception

import Architecture

--import GUIGTK
import GUI

import CommonTypes
import DocTypes
import DocUtils -- for redirect

import PresPresent
import EnrTypes
--import Graphics.UI.WX.Types hiding (Size)
import Graphics.UI.Gtk

import PresTypes -- temporarily
import PresUtils -- temporarily
import LayTypes
import ArrTypes -- temporarily
import ArrLayerTypes -- for initial extra state
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
    ; viewedAreaRef <- newIORef ((0,0),(0,0)) -- shared by GUI and extra state on Arrangement layer
    ; let layers = 
            proximaLayers evaluationSheet reductionSheet presentationSheet parseSheet scannerSheet
                          (LayerStateEval, initDoc)   
                          ((),     initEnr)
                          (EmptyP NoIDP,   PresentationLevel (EmptyP NoIDP) (initLayout,0))   
                          (LocalStateArr fontMetricsRef Nothing viewedAreaRef ((0,0),(0,0)), LayoutLevel (EmptyP NoIDP) NoFocusP (DiffLeaf False))
                          ((), ArrangementLevel (EmptyA NoIDA 0 0 0 0 0 0 transparent) NoFocusA (EmptyP NoIDP)) 
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
    ; startGUI handler viewedAreaRef
                       ( RenderingLevel 1.0 (\_ _ _ _ _ _ _ x y -> return Nothing) (\_ _ -> return ()) (0,0) False  
                                            []
                                            False
                       , initEvent)


    }
 `Control.Exception.catch`
   \err -> 
    do { putStrLn "\n\n\nProxima terminated abnormally:\n" 
       ; print err
       ; putStrLn "\n<Press a key to exit>"
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
