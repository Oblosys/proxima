{-# LANGUAGE CPP #-} 
module Proxima.GUI where

{-
Initialization of the document is done with timer handler, because it may show a dialog (about the
backup document), which is not possible before the GUI event loop is started. The initialization
handler starts the backup handler, so this won't be called before initialization.


Unclear: if edit is skip, update renderingLevel? Maybe it was changed by renderer? (popup etc.)

TODO: fix wrong hRef (hSpaces are the problem)

-}
import Data.IORef

import Common.CommonTypes
import Common.CommonUtils
import Rendering.RenTypes
import Proxima.Wrap

#ifdef SERVER
import Proxima.GUIServer
#else
import Proxima.GUIGtk
#endif




initialWindowSize :: (Int, Int)
initialWindowSize = (900, 760)

documentFilename = "Document.xml"

startGUI :: (Show doc, Show enr, Show node, Show token) =>
            Settings ->
            ((RenderingLevel doc enr node clip token, EditRendering doc enr node clip token) -> IO (RenderingLevel doc enr node clip token, [EditRendering' doc enr node clip token])) ->
            IORef Rectangle ->
            (RenderingLevel doc enr node clip token, EditRendering doc enr node clip token) -> IO ()
startGUI settings handler viewedAreaRef (initRenderingLvl, initEvent) = 
 do { renderingLvlVar <- newIORef initRenderingLvl

{-
    --    ; timeoutAdd (withCatch $ backupDocumentHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas) 30000 
    -- about every half minute, save a backup of the document

--    ; timeoutAddFull (withCatch $ performEditSequence handler renderingLvlVar buffer viewedAreaRef window vp canvas) priorityHighIdle 0

-}

    ; params <- withCatch $ initialize (settings,handler,renderingLvlVar,viewedAreaRef,initialWindowSize)
    ; withCatch $ genericHandler settings handler renderingLvlVar viewedAreaRef params initEvent
      
    ; withCatch $ genericHandler settings handler renderingLvlVar viewedAreaRef params (OpenFileRen "Document.xml")
      
    ; withCatch $ genericHandler settings handler renderingLvlVar viewedAreaRef params (KeySpecialRen F1Key (Modifiers False False False))
     
    ; startEventLoop (settings,handler,renderingLvlVar,viewedAreaRef)
  


{-
            ; withCatch $ genericHandler settings handler renderingLvlVar buffer viewedAreaRef window vp canvas initEvent
              
              ; initDocFilename <-
                  withCatch $ initialDocumentName
              ; genericHandler settings handler renderingLvlVar buffer viewedAreaRef window vp canvas (OpenFileRen initDocFilename)
         
              ; withCatch $ genericHandler settings handler renderingLvlVar buffer viewedAreaRef window vp canvas (KeySpecialRen CommonTypes.F1Key (CommonTypes.Modifiers False False False))

-}
    }    

