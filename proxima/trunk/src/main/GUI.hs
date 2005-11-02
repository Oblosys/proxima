module GUI where




{-

fix item lists
fix rectangle
fix wrong hRef (hSpaces are the problem)

-}



{-
type stuff: need to downcast ScrolledWindow in a few places
title set?
optimization?

Unclear: if edit is skip, update renderingLevel? Maybe it was changed by renderer? (popup etc.)
-}
--import Graphics.UI.ObjectIO
import Graphics.UI.WX
import Graphics.UI.WXCore

import CommonTypes (DebugLevel (..), debug, showDebug, showDebug', debugIO, debugLnIO)
import qualified CommonTypes
import RenTypes hiding (Size)
import CommonUtils


import Char
import IO
import Directory

initialWindowSize :: Size
initialWindowSize = sz 1260 900

startGUI :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) -> (RenderingLevel documentLevel, EditRendering documentLevel) -> IO ()
startGUI handler (initRenderingLvl, initEvent) = run $
 do { frame <- frameCreateTopFrame "Proxima v0.2"

       -- for good measure: put a scrolled window inside the frame
       -- note that 'wxNO_FULL_REPAINT_ON_RESIZE'  is needed to prevent flicker on resize.
    ; window <- scrolledWindowCreate frame idAny rectNull (wxHSCROLL + wxVSCROLL + wxNO_FULL_REPAINT_ON_RESIZE + wxCLIP_CHILDREN)
       -- virtual size is 20*40 = 800 pixels
    
--    ; scrolledWindowSetScrollbars window 20 20 80 80 0 0 False

    ; windowSetClientSize frame initialWindowSize

    ; renderingLvlVar <- varCreate initRenderingLvl

    ; windowOnMouse window True {- get motion events -} (onMouse handler renderingLvlVar window)
    ; windowOnContextMenu window                        (onContextMenu handler renderingLvlVar window)
    ; windowOnKeyChar window                            (onKeyboard handler renderingLvlVar window)
    ; windowOnPaint window {- double buffer? -}         (onPaint renderingLvlVar)
       
    ; file <- menuPane [ text := "&File"]
    ; open   <- menuItem file [ text := "&Open...\tCtrl+O", help := "Open an existing document" ]
    ; saveAs <- menuItem file [ text := "&Save As...\tCtrl+S", help := "Save the document in XML format" ]
    ; quit   <- menuItem file [ text := "&Quit\tCtrl+Q", help := "Quit the application"  ]
          
    ; set frame [menubar := [file]]
    ; set frame [on (menu open)   := fileMenuHandler handler renderingLvlVar window "open" ] 
    ; set frame [on (menu saveAs) := fileMenuHandler handler renderingLvlVar window "save" ] 
    ; set frame [on (menu quit) := close frame] 

     -- show the frame
    ; windowShow frame
    ; windowRaise frame

    ; genericHandler handler renderingLvlVar window initEvent
    ; genericHandler handler renderingLvlVar window ((KeySpecialRen CommonTypes.F1Key (CommonTypes.Modifiers False False False)))
    ; genericHandler handler renderingLvlVar window ((KeySpecialRen CommonTypes.F1Key (CommonTypes.Modifiers False False False)))
    -- why parse twice?

    --; windowRefresh window False {- erase background -}  
    -- somehow initial repaint goes wrong. A mouse click does what we want for now.

    ; return ()
    }    

onPaint :: Var (RenderingLevel documentLevel) -> DC () -> Rect -> IO ()          
onPaint renderingLvlVar dc viewRect =
 do { dcClear dc
    ; RenderingLevel scale mkPopupMenu rendering (w,h) debug updRegions <- varGet renderingLvlVar
    --; dcSetUserScale dc 2.0 2.0
    
    ; rendering dc
    
 {-
 when switching from POINTS to TEXT, all fonts switch as well, so this can't be 
 used to determine pixel sizes for fonts


Even when mapmode is points, an  x point rectangle is smaller than an x point letter
furthermore, 10pt has height 20m and 100pt has height 181


 MAC: dcSetMapMode has no effect
      somehow fonts are smaller than on windows

from http://www.wxwindows.org/manuals/2.4.2/wx482.htm#wxfontoverview :
> Note: There is currently a difference between the appearance of fonts on the two platforms, if the mapping
> mode is anything other than wxMM_TEXT. Under X, font size is always specified in points. Under MS Windows,
> the unit for text is points but the text is scaled according to the current mapping mode. However, user scaling
> on a device context will also scale fonts under both environments.
-}
{-
    ; dcSetMapMode dc wxMM_TEXT
    ; w <- dcLogicalToDeviceXRel dc 10
    ; dcSetFontStyle dc $ fontDefault { _fontFace = "Courier New", _fontSize = 10 }
    ; size <- dcGetCharHeight dc
    ; debugLnIO Arr $ "1!!!!!!!!!!!!!!!!!!!!!!size of 100 is"++show size
    ; dcSetFontStyle dc $ fontDefault { _fontFace = "Courier New", _fontSize = 10 }
    ; size <- getTextExtent dc "x"
    ; debugLnIO Arr $ "1!!!!!!!!!!!!!!!!!!!!!!size of 10 is"++show size
    ; drawText dc "x" (pt 0 0) []
    ; drawRect dc (rect (pt 0 0) (sz 10 10)) []
    
    ; dcSetMapMode dc wxMM_TEXT
    
    ; size <- getTextExtent dc "x"
    ; debugLnIO Arr $ "2!!!!!!!!!!!!!!!!!!!!!!size of 100 is"++show size
    ; dcSetFontStyle dc $ fontDefault { _fontFace = "Courier New", _fontSize = 100 }
    
    ; size <- getTextExtent dc "x"
    ; debugLnIO Arr $ "3!!!!!!!!!!!!!!!!!!!!!!size of 100 is"++show size
    
    
    ; drawRect dc (rect (pt 200 0) (sz 100 100)) []
    ; drawText dc "x" (pt 200 0) []

    ; dcSetMapMode dc wxMM_POINTS
    ; w <- dcLogicalToDeviceXRel dc 100
    ; debugLnIO Arr $ "logical 100 is "++show w
    ; dcSetMapMode dc wxMM_TEXT
    ; w <- dcLogicalToDeviceXRel dc 100
    ; debugLnIO Arr $ "logical 100 is "++show w
-}    
    }

onMouse :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) ->
              Var (RenderingLevel documentLevel) -> ScrolledWindow () ->
              EventMouse -> IO ()
onMouse handler renderingLvlVar window mouseEvt =
 do { editRendering <-
        case mouseEvt of 
          MouseLeftDown (Point x y) mods -> return $ MouseDownRen x y (translateModifiers mods) 1
          MouseLeftDrag (Point x y) mods -> return $ MouseDragRen x y (translateModifiers mods)
          MouseLeftUp   (Point x y) mods -> return $ MouseUpRen x y   (translateModifiers mods)
          MouseRightUp  (Point x y) mods -> do { (RenderingLevel _ makePopupMenu _ _ _ _)  <- varGet renderingLvlVar
                                               -- x and y are on the underlying window, not on the scrolled window
                                               -- ; (Point x' y') <- scrolledWindowCalcScrolledPosition window (pt x y)

                                               ; makePopupMenu handler renderingLvlVar window x y
                                               ; return $ SkipRen 0
                                               }
                                   -- do { propagateEvent; return $ SkipRen 0 } 
          _                              -> do { propagateEvent; return $ SkipRen 0 }
          
    ; genericHandler handler renderingLvlVar window editRendering
    }
  
-- not used because position is not available here, when using this, be sure to propagateEvent in mouse handler
-- (and also in keyboard handler for f10)
onContextMenu :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) ->
                 Var (RenderingLevel documentLevel) -> ScrolledWindow () ->
                 IO ()
onContextMenu handler renderingLvlVar window =
 do { (RenderingLevel _ makePopupMenu _ _ _ _)  <- varGet renderingLvlVar
    
    ; makePopupMenu handler renderingLvlVar window 130 120
    }

onKeyboard :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) ->
              Var (RenderingLevel documentLevel) -> ScrolledWindow a ->
              EventKey -> IO ()
onKeyboard handler renderingLvlVar window (EventKey key mods fp) = 
  do { putStrLn $ "key:" ++ show key ++ ", "++ show mods ++ ", "++ show fp ++ show (keyKey (EventKey key mods fp))
     ; let editRendering = translateKey key (translateModifiers mods)
     ; genericHandler handler renderingLvlVar window editRendering
    -- add skip event?
     }

popupMenuHandler :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) ->
                    Var (RenderingLevel documentLevel) -> ScrolledWindow a ->
                    (documentLevel -> documentLevel) -> IO ()
popupMenuHandler handler renderingLvlVar window editDoc =
 do { let editRendering = (UpdateDocRen editDoc)
                                
    ; genericHandler handler renderingLvlVar window editRendering
    }

fileMenuHandler :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) ->
                       Var (RenderingLevel documentLevel) -> ScrolledWindow a ->
                       String -> IO ()
fileMenuHandler handler renderingLvlVar window menuItem =
 do { editRendering <- 
        case menuItem of
          "open" ->
           do { debugLnIO Err "Open"

              ; filePathM <- fileOpenDialog window False True "Open" [("Helium Files",["*.hs"]),("Any Files",["*.*"])] "" ""
              ; debugLnIO Err $ show filePathM
              ; let editRendering = maybe (SkipRen 0) OpenFileRen filePathM
              ; return editRendering
              }
          "save" -> 
           do { debugLnIO Err "Save"
              ; filePathM <- fileSaveDialog window False True "Save" [] "" "doc.xml"
 
              
              --; filePathM <- fileSaveDialog window False True "Select" "." "doc.xml"
              ; debugLnIO Err $ show filePathM
              ; let editRendering = maybe (SkipRen 0) SaveFileRen filePathM
              ; return editRendering
              }
          _      -> return $ SkipRen 0
    ; genericHandler handler renderingLvlVar window editRendering
    }


-- WX: instead of ObjectIO's process state, we use a variable to store the rendering level
--     this variable is also used by paint

-- genericHandler does the things all handlers need to do.(invoking Proxima layers, updating window, etc.) 
-- A specific handler maps its event (mouse/keyboard/etc.) onto an EditRendering event and passes this with the
-- current EditLevel to genericHandler.
-- If successful, the updated RenderingLevel is returned.

genericHandler :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) 
               -> Var (RenderingLevel documentLevel) -> ScrolledWindow a -> EditRendering documentLevel -> IO ()
genericHandler handler renderingLvlVar window evt =   
 do { renderingLvl <- varGet renderingLvlVar
    ; (renderingLvl', editRendering) <- handler (renderingLvl,evt)
    ; case editRendering of
        SkipRen' _ -> return () -- set the renderingLvlVar ??
         
        SetRen' renderingLvl''@(RenderingLevel scale _ rendering' (w',h') _ updRegions) -> 
         do { varSet renderingLvlVar renderingLvl''
            
----              setWindowLook windowID False (True, look rendering')
--            ; invalidateWindow windowID
-----            ; setWindowViewDomain windowID (rectBetween (Point 0 0) (Point w' h'))  
-----            ; setWindowTitle windowID ("Proxima v1.0   ("++show (scaleInt scale 100)++"%)")
           ; viewStart <- scrolledWindowGetViewStart window
--           ; putStrLn $ "viewStart :"++show point

           -- ; windowSetClientSize window (sz w' h')
           ; scrolledWindowSetScrollbars window 1 1 w' h' (pointX viewStart) (pointY viewStart) False
                    
--           ; putStrLn $ "virtSize :"++show w' ++","++ show h'

            -- hack: only partial window updates for flickering reduce, see RenTypes.hs      
            ; let (fCur, fOld, editedRegion) = updRegions
            -- 3 updates leads to three re-renderings and we can't give update a list :-(
            -- so we compute the enclosing rectangle. (if editedRegion.x1 == -1, ignore it)
                               
            ; let rects = [fCur, fOld] ++ if (pointX.rectTopLeft) editedRegion /= -1 then [editedRegion] else []
            
            ; let updR = editedRegion
           {-
            ; let updR = 
                    rectBetween ( Point (minimum.map (pointX.rectTopLeft) $ rects)
                                        (minimum.map (pointY.rectTopLeft) $ rects) )
                                ( Point (maximum.map (pointX.rectBottomRight) $ rects)
                                        (maximum.map (pointY.rectBottomRight) $ rects) )
            -}
            ; scrolledPos <- scrolledWindowCalcScrolledPosition window (pt (rectLeft updR) (rectTop updR))
            ; let scrolledUpdR = rect scrolledPos (sz (rectWidth updR) (rectHeight updR))
            --; let updR =  -- with ghc -o2, partial update goes wrong. We need a new renderer!
            --        rectBetween ( Point 0 0 ) (Point 1200 700)
              ; debugIO GUI $ "rectangle = "++show updR

            --; windowRefresh window False {- erase background -}
            
            ; windowRefreshRect window False scrolledUpdR
            }
    }

translateKey :: Key -> CommonTypes.Modifiers -> EditRendering documentLevel
translateKey (KeyChar ch) m  = KeyCharRen (if CommonTypes.shift m then ch else toLower ch)
translateKey KeySpace m  = KeyCharRen ' '
translateKey KeyReturn m = KeySpecialRen CommonTypes.EnterKey m
translateKey KeyBack m = KeySpecialRen CommonTypes.BackspaceKey m
translateKey KeyDelete m = KeySpecialRen CommonTypes.DeleteKey m
translateKey KeyLeft m = KeySpecialRen CommonTypes.LeftKey m
translateKey KeyRight m = KeySpecialRen CommonTypes.RightKey m
translateKey KeyUp  m = KeySpecialRen CommonTypes.UpKey m
translateKey KeyDown m = KeySpecialRen CommonTypes.DownKey m
translateKey KeyF1 m = KeySpecialRen CommonTypes.F1Key m
translateKey KeyF2 m = KeySpecialRen CommonTypes.F2Key m
translateKey KeyF3 m = KeySpecialRen CommonTypes.F3Key m
translateKey KeyF4 m = KeySpecialRen CommonTypes.F4Key m
translateKey KeyF5 m = KeySpecialRen CommonTypes.F5Key m
translateKey KeyF6 m = KeySpecialRen CommonTypes.F6Key m
translateKey KeyF7 m = KeySpecialRen CommonTypes.F7Key m
translateKey KeyF8 m = KeySpecialRen CommonTypes.F8Key m
translateKey KeyF9 m = KeySpecialRen CommonTypes.F9Key m
translateKey KeyF10 m = KeySpecialRen CommonTypes.F10Key m
translateKey KeyF11 m = KeySpecialRen CommonTypes.F11Key m
translateKey KeyF12 m = KeySpecialRen CommonTypes.F12Key m
translateKey _      _ = SkipRen 0

translateModifiers :: Modifiers -> CommonTypes.Modifiers
translateModifiers m = CommonTypes.Modifiers (shiftDown m) (controlDown m) (altDown m)


downCast :: ScrolledWindow a -> Window () 
downCast a = objectCast a



-------------------- OLD OBJECTIO RENDERER

{-
 do { ids <- openIds 4
    ; startGUI' handler (initRenderingLvl, initEvent) ids -- trick to get the id's in local definition scope
    ; return ()
    }
    
-}

{-
startGUI' :: ((RenderingLevel documentLevel, EditRendering documentLevel) -> IO (RenderingLevel documentLevel, EditRendering' documentLevel)) -> 
             (RenderingLevel documentLevel, EditRendering documentLevel) -> [Id] -> IO () 
startGUI' handler (initRendering, initEvent) [windowID, editorControlID, id0, id1] =
 do {
    ; startIO SDI initRendering initialize [ProcessClose closeProcess]
    }
 where                           -- do all of these need to be local?
   initialize renderingLvl =
    do { ps <- openMenu  () fileMenu renderingLvl
       --; size <- getProcessWindowSize
       
       ; ps      <- openWindow () (window (Size 800 700){-size-}) renderingLvl
       
       -- initialize all layers by giving an initEvent to the handler

       ; renderingLvlM' <- genericHandler windowID handler renderingLvl initEvent
       ; renderingLvl' <- case renderingLvlM' of
                            Nothing            -> debug Err "Gui.startGUI: initEvent failed" $ return renderingLvl
                            Just renderingLvl' -> return renderingLvl'
                            
                            
                            -- just for now, an F1 event to parse the initial presentation
       ; renderingLvlM'' <- genericHandler windowID handler renderingLvl' ((KeySpecialRen CommonTypes.F1Key (CommonTypes.Modifiers False False False)))
       ; renderingLvl''  <- case renderingLvlM'' of
                              Nothing             -> debug Err "Gui.startGUI: initEvent failed" $ return renderingLvl'
                              Just renderingLvl'' -> return renderingLvl''
  

                            -- and one more, to get whitespace right
       ; renderingLvlM''' <- genericHandler windowID handler renderingLvl'' ((KeySpecialRen CommonTypes.F1Key (CommonTypes.Modifiers False False False)))
       ; renderingLvl'''  <- case renderingLvlM''' of
                              Nothing             -> debug Err "Gui.startGUI: initEvent failed" $ return renderingLvl''
                              Just renderingLvl''' -> return renderingLvl'''
       
 --      ; setActiveWindow windowID -- does not work in GHCI, window does not go to front
       ; return renderingLvl'''
       }
    
   window size =                
     Window ("Proxima v1.0") NilLS
       [ WindowId      windowID
       , WindowClose       (noLS closeProcess)
            
       , WindowViewSize    size
       , WindowOrigin      zero
       , WindowPen     [PenBack white]
       , WindowVScroll     vscroll
       , WindowHScroll     vscroll
       , WindowViewDomain (Rectangle (Point2 0 0) (Point2 2000 4000))  -- this should be the arrangement size
       
       , WindowMouse    (const True) Able (noLS1 (mouseHandler id0 windowID handler))
       , WindowKeyboard (const True) Able (noLS1 (keyboardHandler windowID handler))
     --   , WindowDoubleBuffered
 
       ]



-}

-- menuHandler, buttonHandler, ...




-------------- Experiments with dynamic tooltip support:
{-

tooltipLook str _ _ = 
 do { setPenColour (RGB 255 255 0)
    ; fillAt (Point2 0 0) (Box 100 100)
    ; setPenColour (RGB 0 0 0)
    ; drawAt (Point2 0 15) str
    }
-}

{-in mouse handler:

     ; case mstate of
        MouseDown (Point2 x y) ms _ -> 
          do { hideControl cID
             }
        _                           -> 
               return ()
   
    
    -- handle event


    ; case mstate of
        MouseDown (Point2 x y) ms _ -> 
          do { debugLnGUI GUI $ "moving control"++show ms
             ; p <- setControlPos windowID [(cID, (Fix, Vector2 x y))]
             ; setControlLook cID True (True, tooltipLook (show x))
             ; showControl cID
             
             ; debugLnGUI GUI $ ""
             }
        _ ->
               return ()
    


-- local startGUI decls
-- experiments with moving and transparent tooltip controls. Failed, because controls can't be transparent and moving causes
-- massive repainting of underlying control. Also can't manage to let mouse events be handled by an underlying control
 
  in window def:
   ... Window ("Proxima v1.0") () tooltipControl:+: tooltipControl2)
   ...                         -- need to give both, otherwise tooltipControl does not work!?!? Controls are buggy in this version

   tooltipControl =
     CustomControl (Size 50 20) (tooltipLook "hallo!")
       [ ControlId  id0
       , ControlPos (Fix, (Vector2 1000 1000))
     --  , ControlMouse  (const True) Able (noLS1 (mouseH)) -- Mouse handler for these controls crashes at first mouse over
       ]

   tooltipControl2 = 
     CustomControl (Size 0 0) (\_ _ -> drawAt (Point2 10 10) "hahkjhkjfhkjhllo")
       [ ControlId  id1
       , ControlPos (Fix, (Vector2 1000 1000))
       ]


   mouseH :: MouseState -> state -> GUI state state
   mouseH mevent state = 
    do { debugLnGUI GUI "yes"
       ; return state
       }


   editorControl =
     CustomControl (Size 5000 1500) (\_ _ -> return ())
       [ ControlId editorControlID
       , ControlPos (Fix, zero)
       , ControlTip ""
       ] 

   controlTester state =
     do { -- disableControl id0 
          
        ; return state
        }



-}






-- If mouse is on window outside control, drawInWindow returns Nothing

-- Aargh. lot of work to use a custom control instead of the window, so a tooltip can be shown. However, the 
-- tooltip text can't be changed... Using separate controls for all tooltips will be far too heavy weight, and
-- also the background should be transparent then. Probably better to let Proxima create the tooltip. 
-- Proxima tooltips may only show inside the window, but they can use full Xprez instead of only text
-- anyhow, the control refresh seems to flicker even more than the window refresh.



-- file dialogues change current directory and do not start in the current directory
-- no mouse exit events


-- something wrong with keyboard handler: enter, backspace and delete always trigger two key events,
-- one key down and one keydown repeat. for now, repeat is disabled for these keys

