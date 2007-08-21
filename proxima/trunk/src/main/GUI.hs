module GUI where

{-
Initialization of the document is done with timer handler, because it may show a dialog (about the
backup document), which is not possible before the GUI event loop is started. The initialization
handler starts the backup handler, so this won't be called before initialization.


Unclear: if edit is skip, update renderingLevel? Maybe it was changed by renderer? (popup etc.)

TODO: fix wrong hRef (hSpaces are the problem)

-}
import Graphics.UI.Gtk hiding (Size)
import Data.IORef

import CommonTypes (DebugLevel (..), debug, showDebug, showDebug', debugIO, debugLnIO)
import qualified CommonTypes
import RenTypes
import RenUtils
import CommonUtils

import Char
import Maybe
import IO
import Directory

initialWindowSize :: (Int, Int)
initialWindowSize = (1000, 900)

documentFilename = "Document.xml"
backupFilename = "BackupDocument.xml"

startGUI :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) ->
            IORef CommonTypes.Rectangle ->
            (RenderingLevel documentLevel, EditRendering documentLevel) -> IO ()
startGUI handler viewedAreaRef (initRenderingLvl, initEvent) = 
 do { initGUI
    ; window <- windowNew
    ; onDestroy window mainQuit
    ; set window [ windowTitle := "Proxima v0.3" ]
    ; windowSetDefaultSize window (fst initialWindowSize) (snd initialWindowSize)

    ; canvas <- drawingAreaNew 
    ; widgetSetCanFocus canvas True
  
    ; sw <- scrolledWindowNew Nothing Nothing
    ; hAdj <- scrolledWindowGetHAdjustment sw
    ; vAdj <- scrolledWindowGetVAdjustment sw
    ; vp <- viewportNew hAdj vAdj
    ; containerAdd vp canvas
    ; containerAdd sw vp
    --; sw `scrolledWindowAddWithViewport` canvas
    ; scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
    ; scrolledWindowSetShadowType sw ShadowNone

    ; buffer <- newIORef Nothing
    ; renderingLvlVar <- newIORef initRenderingLvl

    ; onExpose canvas $ onPaint handler renderingLvlVar buffer viewedAreaRef window vp canvas
    ; onKeyPress canvas $ onKeyboard handler renderingLvlVar buffer viewedAreaRef window vp canvas
    ; onMotionNotify canvas False $ onMouse handler renderingLvlVar buffer viewedAreaRef window vp canvas
    ; onButtonPress canvas $ onMouse handler renderingLvlVar buffer viewedAreaRef window vp canvas
    ; onButtonRelease canvas $ onMouse handler renderingLvlVar buffer viewedAreaRef window vp canvas
    ; onDelete window $ closeHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas
    ; fileMenu <- mkMenu
        [ ("_Open", fileMenuHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas "open")
        , ("_Save", fileMenuHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas "save")
        , ("_Quit", mainQuit)
        ]
    
    ; fileItem <- menuItemNewWithMnemonic "_File"
    ; menuItemSetSubmenu fileItem fileMenu 
   
    ; menuBar <- menuBarNew
    ; menuShellAppend menuBar fileItem
  
    ; vBox <- vBoxNew False 0
    ; set vBox [boxHomogeneous := False]
    
    ; boxPackStart vBox menuBar PackNatural 0
    ; boxPackStart vBox sw PackGrow 0

    ; containerAdd window vBox
    
    ; widgetShowAll window
      
    ; genericHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas initEvent
    
    --; sequence_ $ replicate 100 $  -- for profiling
    --    genericHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas ((KeySpecialRen CommonTypes.F1Key (CommonTypes.Modifiers False False False)))
    
    --; genericHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas ((KeySpecialRen CommonTypes.F1Key (CommonTypes.Modifiers False False False)))
    --; genericHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas ((KeySpecialRen CommonTypes.F1Key (CommonTypes.Modifiers False False False)))
    -- interpret twice, so helium code in strings is also parsed

    ; initializeDocument handler renderingLvlVar buffer viewedAreaRef window vp canvas
    ; mainGUI
    }    

onMouse :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) ->
              IORef (RenderingLevel documentLevel) -> IORef (Maybe Pixmap) -> IORef CommonTypes.Rectangle -> Window -> Viewport -> DrawingArea ->
              Event -> IO Bool
onMouse handler renderingLvlVar buffer viewedAreaRef window vp canvas evt@(Button _ ReleaseClick tm x y _ RightButton _ _) =
 do { (RenderingLevel _ makePopupMenu _ _ _ _ _)  <- readIORef renderingLvlVar
    ; mContextMenu <- makePopupMenu handler renderingLvlVar buffer viewedAreaRef window vp canvas (round x) (round y)
    ; case mContextMenu of
       Just contextMenu ->
        do { widgetShowAll contextMenu
           ; menuPopup contextMenu (Just (RightButton,tm))
           ; return True
           }
       Nothing -> return False
    }
onMouse handler renderingLvlVar buffer viewedAreaRef window vp canvas mouseEvt =
 do { (RenderingLevel _ _ _ _ _ _ leftMouseDown) <- readIORef renderingLvlVar
    ; let editRendering = 
            case mouseEvt of 
              Button _ SingleClick _ x y mods LeftButton _ _   -> MouseDownRen (round x) (round y) (translateModifiers mods) 1
              Button _ ReleaseClick _ x y mods LeftButton _ _  -> MouseUpRen (round x) (round y) (translateModifiers mods) 
              Motion _ _ x y mods _ _ _   -> if leftMouseDown 
                                             then MouseDragRen (round x) (round y) (translateModifiers mods)
                                             else SkipRen 0
              _                                                -> SkipRen 0
    
    ; case editRendering of 
        SkipRen _ -> return False
        _         ->
          do { genericHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas editRendering
             ; return True
             }
    }
  
onKeyboard :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) ->
              IORef (RenderingLevel documentLevel) -> IORef (Maybe Pixmap) -> IORef CommonTypes.Rectangle -> Window -> Viewport -> DrawingArea ->
              Event -> IO Bool
onKeyboard handler renderingLvlVar buffer viewedAreaRef window vp canvas (Key _ _ _ mods _ _ _  _ keyName mKeyChar) = 
 do { let editRendering = translateKey keyName mKeyChar (translateModifiers mods)

    ; case editRendering of -- TODO: put this in genericHandler?
        SkipRen _ -> return False
        _         ->
          do { genericHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas editRendering
             ; return True
             }
    }

popupMenuHandler :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) ->
                    IORef (RenderingLevel documentLevel) -> IORef (Maybe Pixmap) -> IORef CommonTypes.Rectangle -> Window -> Viewport -> DrawingArea ->
                    (documentLevel -> documentLevel) -> IO ()
popupMenuHandler handler renderingLvlVar buffer viewedArea window vp canvas editDoc =
 do { let editRendering = (UpdateDocRen editDoc)
                                
    ; genericHandler handler renderingLvlVar buffer viewedArea window vp canvas editRendering
    }

fileMenuHandler :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) ->
                       IORef (RenderingLevel documentLevel) -> IORef (Maybe Pixmap) -> IORef CommonTypes.Rectangle -> Window -> Viewport -> DrawingArea ->
                       String -> IO ()
fileMenuHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas menuItem =
 do { editRendering <- 
        case menuItem of
          "open" ->
           do { debugLnIO Err "Open"
              ; dialog <- fileChooserDialogNew
                            (Just "Open")      -- dialog title
                            (Just window)      -- the parent window
	                        FileChooserActionOpen  -- the kind of dialog we want
	                        [ ("Open", ResponseAccept)
	                        , ("Cancel", ResponseCancel) ]-- The buttons to display
              
              ; widgetShow dialog
              ; response <- dialogRun dialog
              ; filePathM <- case response of 
                               ResponseAccept -> fileChooserGetFilename dialog
                               ResponseCancel      -> return Nothing 
                               ResponseDeleteEvent -> return Nothing
              ; widgetHide dialog

              
              ; debugLnIO Err $ show filePathM
              ; let editRendering = maybe (SkipRen 0) OpenFileRen filePathM
              ; return editRendering
              }
          "save" -> 
           do { debugLnIO Err "Save"
              ;   dialog <- fileChooserDialogNew
                             (Just "Save")     -- dialog title
                             (Just window)     --the parent window
	                         FileChooserActionSave --the kind of dialog we want
	                         [ ("Save", ResponseAccept) 
	                         , ("Cancel", ResponseCancel) ] --The buttons to display
              ;  widgetShow dialog
              ;  response <- dialogRun dialog
              ;  filePathM <- case response of 
                                ResponseAccept -> fileChooserGetFilename dialog
                                ResponseCancel -> return Nothing
                                ResponseDeleteEvent -> return Nothing
              ; widgetHide dialog
 
              
              ; debugLnIO Err $ show filePathM
              ; let editRendering = maybe (SkipRen 0) SaveFileRen filePathM
              ; return editRendering
              }
          _      -> return $ SkipRen 0
    ; genericHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas editRendering
    }



-- genericHandler does the things all handlers need to do.(invoking Proxima layers, updating window, etc.) 
-- A specific handler maps its event (mouse/keyboard/etc.) onto an EditRendering event and passes this with the
-- current EditLevel to genericHandler.
-- If successful, the updated RenderingLevel is returned.

genericHandler :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) ->
               IORef (RenderingLevel documentLevel) -> IORef (Maybe Pixmap) -> IORef CommonTypes.Rectangle -> 
               Window -> Viewport -> DrawingArea -> EditRendering documentLevel -> IO ()
genericHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas evt =   
 do { renderingLvl@(RenderingLevel _ _ _ (w,h) _ _ _) <- readIORef renderingLvlVar
    ; viewedArea <- getViewedArea vp
    
    ; putStrLn $ "Writing viewed area"++show viewedArea
    ; writeIORef viewedAreaRef viewedArea
   
    ; (renderingLvl', editRendering) <- handler (renderingLvl,evt)
    ; case editRendering of
        SkipRen' _ -> return () -- set the renderingLvlVar ??
         
        SetRen' renderingLvl''@(RenderingLevel scale _ rendering' (w',h') _ updRegions _) -> 
         do { writeIORef renderingLvlVar renderingLvl''
            ; widgetSetSizeRequest canvas w' h'
            
            ; dw <- drawingAreaGetDrawWindow canvas
            
            ; maybePm <- readIORef buffer
            ; if (isNothing maybePm || (w,h) /= (w',h')) -- if there was no pixmap, or if the size changed
                 && (w /= 0 && h /= 0) -- gtk crashes if we create an empty pixmap
              then do { pm <- pixmapNew (Just dw) w' h' Nothing -- we create a new one
                      ; writeIORef buffer (Just pm)
                      }
              else return ()                           
            
            ; maybePm <- readIORef buffer
            ; case maybePm of
                Just pm -> drawRendering renderingLvlVar window vp pm
                Nothing -> return () -- will not occur
            
            
            ; updatedRegion <- regionNew
            
            ; mapM_ (\((x,y),(w,h))-> regionUnionWithRect updatedRegion (Rectangle x y w h)) updRegions
            ; drawWindowInvalidateRegion dw updatedRegion False -- False: don't invalidate children
            ; drawWindowInvalidateRect dw (Rectangle 0 0 2000 2000)  False
            }
    }

drawRendering :: DrawableClass d => 
                 IORef (RenderingLevel documementLevel) -> Window -> Viewport -> d -> IO ()
drawRendering renderingLvlVar wi vp pm = 
 do { RenderingLevel scale mkPopupMenu rendering (w,h) debug updRegions _ <- readIORef renderingLvlVar
    
    ; let drawFilledRectangle drw grc ((x,y),(w,h)) = drawRectangle drw grc True x y w h
    ; gc <- gcNew pm
        
    ; gcSetValues gc $ newGCValues { foreground = gtkColor CommonTypes.white }
    ; mapM_ print updRegions
      -- clear background for updated regions
    ; mapM_ (\((x,y),(w,h)) -> drawRectangle pm gc True x y w h) updRegions

    ; viewedArea <- getViewedArea vp 
    ; putStrLn $ "The viewed area is" ++ show viewedArea
    ; rendering (wi, pm, gc) viewedArea -- rendering only viewedArea is not extremely useful,
                                        -- since arranger already only arranges elements in view
                                        -- currently, it only prevents rendering edges out of view
    }


onPaint :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) -> 
           IORef (RenderingLevel documentLevel) -> IORef (Maybe Pixmap) -> IORef CommonTypes.Rectangle ->
           Window -> Viewport -> DrawingArea -> Event -> IO Bool
onPaint handler renderingLvlVar buffer viewedAreaRef wi vp canvas (Expose { eventArea=rect }) =
 do { maybePm <- readIORef buffer
    ; case maybePm of 
        Nothing -> return True -- buffer has not been initialized yet, so don't paint.
        Just pm ->
         do { renderedViewedArea <- readIORef viewedAreaRef
            ; viewedArea <- getViewedArea vp 
            ; dw <- drawingAreaGetDrawWindow canvas
    
              -- if renderedViewedArea is different from viewedArea, we need to re-arrange
              -- SkipRen (-2) starts presenting at the arrangement layer
            ; when (renderedViewedArea /= viewedArea) $ 
               genericHandler handler renderingLvlVar buffer viewedAreaRef wi vp canvas (SkipRen (-2))
                 
            
            ; gc <- gcNew dw
            ; drawDrawable dw gc pm 0 0 0 0 (-1) (-1) -- draw the Pixmap on the canvas

            
              -- Mark the updated rectangles by surrounding them with red rectangles
              -- If several edit events have taken place without paint events, only the last is shown
            ; gcSetValues gc $ newGCValues { foreground = gtkColor CommonTypes.red }
            ; RenderingLevel scale mkPopupMenu rendering (w,h) debug updRegions _ <- readIORef renderingLvlVar
            ; mapM_ (\((x,y),(w,h)) -> drawRectangle dw gc False (x-1) (y-1) (w+1) (h+1)) updRegions
                                       -- outline rectangles are 1 px too large
              -- when using this marking, invalidate whole screen instead of updatedRegions (in genericHandler) 
            ; let ((x,y),(w,h)) = viewedArea
            ; drawRectangle dw gc False x y w h 
            ; return True
            }
    }

getViewedArea :: Viewport -> IO (Point,Size)
getViewedArea vp =
 do { vA <- viewportGetVAdjustment vp
    ; y <- adjustmentGetValue vA
    ; hA <- viewportGetHAdjustment vp
    ; x <- adjustmentGetValue hA
    ; (w,h) <- widgetGetSize vp
--    ; return ((round x,round y),(w-5,h-5))  -- Unclear why this -5 is necessary. Maybe for relief?
    ; return ((round x+ (w `div` 4),round y + (h `div` 4)),((w `div` 2) -5,(h `div` 2) -5))  -- Unclear why this -5 is necessary. Maybe for relief?
    }         
    

translateKey :: String -> Maybe Char -> CommonTypes.Modifiers -> EditRendering documentLevel
translateKey _ (Just ch) (CommonTypes.Modifiers False False False)  = KeyCharRen ch
translateKey _ (Just ch) (CommonTypes.Modifiers True False False)  = KeyCharRen ch
translateKey _ (Just ch) m  = KeySpecialRen (CommonTypes.CharKey ch) m
translateKey "Return" _ m = KeySpecialRen CommonTypes.EnterKey m
translateKey "BackSpace" _ m = KeySpecialRen CommonTypes.BackspaceKey m
translateKey "Delete" _ m = KeySpecialRen CommonTypes.DeleteKey m
translateKey "Left" _ m = KeySpecialRen CommonTypes.LeftKey m
translateKey "Right" _ m = KeySpecialRen CommonTypes.RightKey m
translateKey "Up" _ m = KeySpecialRen CommonTypes.UpKey m
translateKey "Down" _ m = KeySpecialRen CommonTypes.DownKey m
translateKey "F1" _ m = KeySpecialRen CommonTypes.F1Key m
translateKey "F2" _ m = KeySpecialRen CommonTypes.F2Key m
translateKey "F3" _ m = KeySpecialRen CommonTypes.F3Key m
translateKey "F4" _ m = KeySpecialRen CommonTypes.F4Key m
translateKey "F5" _ m = KeySpecialRen CommonTypes.F5Key m
translateKey "F6" _ m = KeySpecialRen CommonTypes.F6Key m
translateKey "F7" _ m = KeySpecialRen CommonTypes.F7Key m
translateKey "F8" _ m = KeySpecialRen CommonTypes.F8Key m
translateKey "F9" _ m = KeySpecialRen CommonTypes.F9Key m
translateKey "F10" _ m = KeySpecialRen CommonTypes.F10Key m
translateKey "F11" _ m = KeySpecialRen CommonTypes.F11Key m
translateKey "F12" _ m = KeySpecialRen CommonTypes.F12Key m
translateKey _    _ _ = SkipRen 0


translateModifiers :: [Modifier] -> CommonTypes.Modifiers
translateModifiers ms = CommonTypes.Modifiers (Shift `elem` ms) (Control `elem` ms) (Alt `elem` ms)



initializeDocument handler renderingLvlVar buffer viewedAreaRef window vp canvas =
 do { backupExists <- doesFileExist backupFilename
    ; filename <-
        if False -- backupExists 
        then 
         do { response <- okDialog "During the previous session, Proxima terminated unexpectedly. Do you wish to continue editing the document from that session?"
            ; if response == ResponseOk
              then return backupFilename -- we do not delete the backup, in case Proxima crashes again.
              else do { removeFile backupFilename 
                      ; return documentFilename
                      }
            }
        else return documentFilename
    ; genericHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas (OpenFileRen filename)
    
--    ; timeoutAdd (backupDocumentHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas )
--                 30000 -- about every half minute, save a backup of the document
    }

backupDocumentHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas =
 do { let editRendering = SaveFileRen backupFilename 
 
      -- Parse and save
    ; genericHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas ((KeySpecialRen CommonTypes.F1Key (CommonTypes.Modifiers False False False)))
    ; genericHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas editRendering
    ; return True
    }
         
closeHandler handler renderingLvlVar buffer viewedAreaRef window vp canvas _ =
 do { removeFile backupFilename 
    ; return False -- False means exit
    }
    
okDialog txt =
 do { dia <- dialogNew
    ; dialogAddButton dia stockOk ResponseOk
    ; dialogAddButton dia stockCancel ResponseCancel
    ; contain <- dialogGetUpper dia
    ; msg <- labelNew (Just txt)
    ; contain `boxPackStartDefaults` msg
    ; widgetShow msg
    ; response <- dialogRun dia
    ; widgetDestroy dia
    ; return response
    }