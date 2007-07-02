module GUI where




{-

fix item lists
fix rectangle
fix wrong hRef (hSpaces are the problem)

-}



{-
title set?
optimization?

Unclear: if edit is skip, update renderingLevel? Maybe it was changed by renderer? (popup etc.)
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

startGUI :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) -> (RenderingLevel documentLevel, EditRendering documentLevel) -> IO ()
startGUI handler (initRenderingLvl, initEvent) = 
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

    ; onExpose canvas $ onPaint buffer window vp renderingLvlVar canvas
    ; onKeyPress canvas $ onKeyboard handler renderingLvlVar buffer window canvas
    ; onMotionNotify canvas False $ onMouse handler renderingLvlVar buffer window canvas
    ; onButtonPress canvas $ onMouse handler renderingLvlVar buffer window canvas
    ; onButtonRelease canvas $ onMouse handler renderingLvlVar buffer window canvas
  

    ; fileMenu <- mkMenu
        [ ("_Open", fileMenuHandler handler renderingLvlVar buffer window canvas "open")
        , ("_Save", fileMenuHandler handler renderingLvlVar buffer window canvas "save")
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
      
    ; genericHandler handler renderingLvlVar buffer window canvas initEvent
    --; genericHandler handler renderingLvlVar window ((KeySpecialRen CommonTypes.F1Key (CommonTypes.Modifiers False False False)))
    --; genericHandler handler renderingLvlVar window ((KeySpecialRen CommonTypes.F1Key (CommonTypes.Modifiers False False False)))
    -- interpret twice, so helium code in strings is also parsed

       
    ; mainGUI
    }    

onPaint :: IORef (Maybe Pixmap) -> Window -> Viewport -> IORef (RenderingLevel documentLevel) -> DrawingArea -> Event -> IO Bool
onPaint buffer wi vp renderingLvlVar canvas (Expose { eventArea=rect }) =
 do { maybePm <- readIORef buffer
    ; case maybePm of 
        Nothing -> return True
        Just pm ->
         do { RenderingLevel scale mkPopupMenu rendering (w,h) debug updRegions _ <- readIORef renderingLvlVar
    
            ; viewedArea <- getViewedArea vp
            
            ; dw <- drawingAreaGetDrawWindow canvas
            ; gc <- gcNew pm
            ; rendering (wi, pm, gc) viewedArea
            -- paint events are less frequent than generic handler events, so we render here
            -- a possible optimization is to only render on the pixmap once. 
            
            ; drawDrawable dw gc pm 0 0 0 0 (-1) (-1) 
            ; drawRectangle dw gc False 100 100 100 100
            
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
    ; return ((round x,round y),(w-5,h-5))  -- Unclear why this -5 is necessary. Maybe for relief?
    }         
    
onMouse :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) ->
              IORef (RenderingLevel documentLevel) -> IORef (Maybe Pixmap) -> Window -> DrawingArea ->
              Event -> IO Bool
onMouse handler renderingLvlVar buffer window canvas evt@(Button _ ReleaseClick tm x y _ RightButton _ _) =
 do { (RenderingLevel _ makePopupMenu _ _ _ _ _)  <- readIORef renderingLvlVar
    ; mContextMenu <- makePopupMenu handler renderingLvlVar buffer window canvas (round x) (round y)
    ; case mContextMenu of
       Just contextMenu ->
        do { widgetShowAll contextMenu
           ; menuPopup contextMenu (Just (RightButton,tm))
           ; return True
           }
       Nothing -> return False
    }
onMouse handler renderingLvlVar buffer window canvas mouseEvt =
 do { (RenderingLevel _ _ _ _ _ _ leftMouseDown) <- readIORef renderingLvlVar
    ; let editRendering = 
            case mouseEvt of 
              Button _ SingleClick _ x y mods LeftButton _ _   -> MouseDownRen (round x) (round y) (translateModifiers mods) 1
              Button _ ReleaseClick _ x y mods LeftButton _ _  -> MouseUpRen (round x) (round y) (translateModifiers mods) 
              Motion _ _ x y mods _ _ _   -> if leftMouseDown 
                                             then MouseDragRen (round x) (round y) (translateModifiers mods)
                                             else SkipRen 0
              _                                             -> SkipRen 0
    
    ; case editRendering of 
        SkipRen _ -> return False
        _         ->
          do { genericHandler handler renderingLvlVar buffer window canvas editRendering
             ; return True
             }
    }
  
onKeyboard :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) ->
              IORef (RenderingLevel documentLevel) -> IORef (Maybe Pixmap) -> Window -> DrawingArea ->
              Event -> IO Bool
onKeyboard handler renderingLvlVar buffer window canvas (Key _ _ _ mods _ _ _  _ keyName mKeyChar) = 
 do { let editRendering = translateKey keyName mKeyChar (translateModifiers mods)

    ; case editRendering of -- TODO: put this in genericHandler?
        SkipRen _ -> return False
        _         ->
          do { genericHandler handler renderingLvlVar buffer window canvas editRendering
             ; return True
             }
    }

popupMenuHandler :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) ->
                    IORef (RenderingLevel documentLevel) -> IORef (Maybe Pixmap) -> Window -> DrawingArea ->
                    (documentLevel -> documentLevel) -> IO ()
popupMenuHandler handler renderingLvlVar buffer window canvas editDoc =
 do { let editRendering = (UpdateDocRen editDoc)
                                
    ; genericHandler handler renderingLvlVar buffer window canvas editRendering
    }

fileMenuHandler :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) ->
                       IORef (RenderingLevel documentLevel) -> IORef (Maybe Pixmap) -> Window -> DrawingArea ->
                       String -> IO ()
fileMenuHandler handler renderingLvlVar buffer window canvas menuItem =
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
    ; genericHandler handler renderingLvlVar buffer window canvas editRendering
    }



-- genericHandler does the things all handlers need to do.(invoking Proxima layers, updating window, etc.) 
-- A specific handler maps its event (mouse/keyboard/etc.) onto an EditRendering event and passes this with the
-- current EditLevel to genericHandler.
-- If successful, the updated RenderingLevel is returned.

genericHandler :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) 
               -> IORef (RenderingLevel documentLevel) -> IORef (Maybe Pixmap) -> Window -> DrawingArea -> EditRendering documentLevel -> IO ()
genericHandler handler renderingLvlVar buffer window canvas evt =   
 do { renderingLvl@(RenderingLevel _ _ _ (w,h) _ _ _) <- readIORef renderingLvlVar
    ; (renderingLvl', editRendering) <- handler (renderingLvl,evt)
    ; case editRendering of
        SkipRen' _ -> return () -- set the renderingLvlVar ??
         
        SetRen' renderingLvl''@(RenderingLevel scale _ rendering' (w',h') _ updRegions _) -> 
         do { writeIORef renderingLvlVar renderingLvl''
            ; widgetSetSizeRequest canvas w' h'
            
            ; dw <- drawingAreaGetDrawWindow canvas
            
            ; maybePm <- readIORef buffer
            ; if isNothing maybePm || (w,h) /= (w',h') -- if there was no pixmap, or if the size changed
              then do { pm <- pixmapNew (Just dw) w' h' Nothing -- we create a new one
                      ; writeIORef buffer (Just pm)
                      }
              else return ()                           
            -- The rendering itself is done on paint events
            
            ; let (fCur, fOld, editedRegion) = updRegions
            ; mapM_ (\region-> drawWindowInvalidateRect dw region False) [fCur, fOld, editedRegion]
            }
    }

translateKey :: String -> Maybe Char -> CommonTypes.Modifiers -> EditRendering documentLevel
translateKey _ (Just ch) m  = KeyCharRen (if CommonTypes.shift m then ch else toLower ch)
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