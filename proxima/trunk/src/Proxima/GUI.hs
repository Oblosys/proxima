module Proxima.GUI where

{-
Initialization of the document is done with timer handler, because it may show a dialog (about the
backup document), which is not possible before the GUI event loop is started. The initialization
handler starts the backup handler, so this won't be called before initialization.


Unclear: if edit is skip, update renderingLevel? Maybe it was changed by renderer? (popup etc.)

TODO: fix wrong hRef (hSpaces are the problem)

-}
import Data.IORef

import Common.CommonTypes ( DebugLevel (..), debug, showDebug, showDebug', debugIO, debugLnIO
                          , Settings (..) )
import qualified Common.CommonTypes as CommonTypes
import Rendering.RenTypes
import Rendering.RenUtils
import Common.CommonUtils
import Proxima.Wrap
import Evaluation.DocTypes (DocumentLevel, EditDocument'_ (..))
import Char
import Maybe
import System.IO
import Directory
import Data.Time.Clock
import Control.Exception

import qualified Proxima.GUIServer as Server
import qualified Proxima.GUIGtk as Gtk





initialWindowSize :: (Int, Int)
initialWindowSize = (900, 760)

documentFilename = "Document.xml"

startGUI :: (Show doc, Show enr, Show node, Show token) =>
            Settings ->
            ((RenderingLevel doc enr node clip token, EditRendering doc enr node clip token) -> IO (RenderingLevel doc enr node clip token, [EditRendering' doc enr node clip token])) ->
            IORef CommonTypes.Rectangle ->
            (RenderingLevel doc enr node clip token, EditRendering doc enr node clip token) -> IO ()
startGUI settings handler viewedAreaRef (initRenderingLvl, initEvent) = 
 do { renderingLvlVar <- newIORef initRenderingLvl


{-
    --    ; timeoutAdd (withCatch $ backupDocumentHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas) 30000 
    -- about every half minute, save a backup of the document

--    ; timeoutAddFull (withCatch $ performEditSequence handler renderingLvlVar buffer viewedAreaRef window vp canvas) priorityHighIdle 0

-}
-- TODO: remove server mode thing from GUIGtk generic handler

    ; if serverMode settings then
       do {
    ; () <- Server.initialize (settings,handler,renderingLvlVar,viewedAreaRef,initialWindowSize)
    ; Server.genericHandler' settings handler renderingLvlVar viewedAreaRef () initEvent
      
    ; Server.genericHandler' settings handler renderingLvlVar viewedAreaRef () (OpenFileRen "Document.xml")
      
    ; Server.genericHandler' settings handler renderingLvlVar viewedAreaRef () (KeySpecialRen CommonTypes.F1Key (CommonTypes.Modifiers False False False))
     
    ; Server.startEventLoop (settings,handler,renderingLvlVar,viewedAreaRef)
    } else
       do {
    ; (buffer, window, vp, canvas) <- Gtk.initialize (settings,handler,renderingLvlVar,viewedAreaRef,initialWindowSize)
    ; Gtk.genericHandler' settings handler renderingLvlVar viewedAreaRef (buffer, window, vp, canvas) initEvent
      
    ; Gtk.genericHandler' settings handler renderingLvlVar viewedAreaRef (buffer, window, vp, canvas) (OpenFileRen "Document.xml")
      
    ; Gtk.genericHandler' settings handler renderingLvlVar viewedAreaRef (buffer, window, vp, canvas) (KeySpecialRen CommonTypes.F1Key (CommonTypes.Modifiers False False False))
     
    ; Gtk.startEventLoop (settings,handler,renderingLvlVar,viewedAreaRef)
    }  
  


{-
            ; withCatch $ genericHandler settings handler renderingLvlVar buffer viewedAreaRef window vp canvas initEvent
              
              ; initDocFilename <-
                  withCatch $ initialDocumentName
              ; genericHandler settings handler renderingLvlVar buffer viewedAreaRef window vp canvas (OpenFileRen initDocFilename)
         
              ; withCatch $ genericHandler settings handler renderingLvlVar buffer viewedAreaRef window vp canvas (KeySpecialRen CommonTypes.F1Key (CommonTypes.Modifiers False False False))

-}
    }    

