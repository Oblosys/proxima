module GUI where

{-
-}
import Graphics.UI.ObjectIO

import CommonTypes (DebugLevel (..), debug, showDebug, showDebug', debugIO, debugLnIO, debugDraw, debugGUI, debugLnDraw, debugLnGUI)
import qualified CommonTypes
import RenTypes
import CommonUtils

import DocTypes

import IO
import IOExts
import Directory


startGUI :: ((RenderingLevel, EditRendering) -> IO (RenderingLevel, EditRendering')) -> (RenderingLevel, EditRendering) -> IO ()
startGUI handler (initRenderingLvl, initEvent) = 
 do { ids <- openIds 4
    ; startGUI' handler (initRenderingLvl, initEvent) ids -- trick to get the id's in local definition scope
    ; return ()
    }

startGUI' :: ((RenderingLevel, EditRendering) -> IO (RenderingLevel, EditRendering')) -> 
             (RenderingLevel, EditRendering) -> [Id] -> IO () 
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

   fileMenu = 
     Menu "&File" (   MenuItem "&Open"    [MenuShortKey 'o', MenuFunction (noLS (openFileMenuHandler "open" windowID handler))]
                  :+: MenuItem "Save &As"    [MenuShortKey 'a', MenuFunction (noLS (openFileMenuHandler "save" windowID handler))]
                  :+: MenuItem "&Quit"    [MenuShortKey 'q', MenuFunction (noLS closeProcess)]
         --         :+: MenuItem "&Test"    [MenuShortKey 't', MenuFunction (noLS controlTester)]
                  )   []

   vscroll 
        viewFrame 
        sliderState@(SliderState { sliderMin=sliderMin, sliderMax=sliderMax, sliderThumb=sliderThumb }) 
        sliderMove = 
        case sliderMove of 
	        SliderIncSmall -> sliderThumb + 20
	        SliderDecSmall -> sliderThumb - 20
	        SliderIncLarge -> sliderThumb + 100
	        SliderDecLarge -> sliderThumb - 100
	        SliderThumb i -> i

look :: Draw () -> SelectState -> UpdateState -> Draw ()
look rendering _ (UpdateState {updArea=updArea}) =
 do { -- sequence_ $ map unfill updArea
      rendering
    }

-- genericHandler does the things all handlers need to do.(invoking Proxima layers, updating window, etc.) 
-- A specific handler maps its event (mouse/keyboard/etc.) onto an EditRendering event and passes this with the
-- current EditLevel to genericHandler.
-- If successful, the updated RenderingLevel is returned.
genericHandler :: Id -> ((RenderingLevel, EditRendering) -> IO (RenderingLevel, EditRendering')) 
               -> RenderingLevel -> EditRendering -> GUI ps (Maybe RenderingLevel)
genericHandler windowID handler renderingLvl evt =   
 do { (renderingLvl', editRendering) <- liftIO $ handler (renderingLvl,evt)
    ; case editRendering of
        SkipRen' _ -> return $ Just renderingLvl'
         
        SetRen' renderingLvl''@(RenderingLevel scale _ rendering' (w',h') _ updRegions) -> 
         do { setWindowLook windowID False (True, look rendering')
--              setWindowLook windowID False (True, look rendering')
--            ; invalidateWindow windowID
            ; setWindowViewDomain windowID (Rectangle (Point2 0 0) (Point2 w' h'))  
            ; setWindowTitle windowID ("Proxima v1.0   ("++show (scaleInt scale 100)++"%)")

            -- hack: only partial window updates for flickering reduce, see RenTypes.hs      
            ; let (fCur, fOld, editedRegion) = updRegions
            -- 3 updates leads to three re-renderings and we can't give update a list :-(
            -- so we compute the enclosing rectangle. (if editedRegion.x1 == -1, ignore it)
                               
            ; let rects = [fCur, fOld] ++ if (x.corner1) editedRegion /= -1 then [editedRegion] else []
            
            ; let updR = 
                    Rectangle ( Point2 (minimum.map (x.corner1) $ rects)
                                       (minimum.map (y.corner1) $ rects) )
                              ( Point2 (maximum.map (x.corner2) $ rects)
                                       (maximum.map (y.corner2) $ rects) )
            
            --; let updR =  -- with ghc -o2, partial update goes wrong. We need a new renderer!
            --        Rectangle ( Point2 0 0 ) (Point2 1200 700)
              ; debugGUI GUI $ "rectangle = "++show updR

--            ; invalidateWindowRect windowID (updR)
              ; updateWindow windowID (Just updR)
             
              ; return $ Just renderingLvl''
            }
    }




openFileMenuHandler :: String -> Id -> ((RenderingLevel, EditRendering) -> IO (RenderingLevel, EditRendering')) 
                    -> RenderingLevel -> GUI RenderingLevel RenderingLevel
openFileMenuHandler menuItem windowID handler renderingLvl =
 do { editEvent <- 
        case menuItem of
          "open" ->
           do { dirPath <- liftIO $ getCurrentDirectory -- select changes current directory!
              ; (filePathM,_) <- selectInputFile renderingLvl
              ; liftIO $ setCurrentDirectory dirPath    -- so change it back
              ; debugLnGUI GUI $ show filePathM
              ; let editEvent = maybe (SkipRen 0) OpenFileRen filePathM
              ; return editEvent
              }
          "save" -> 
           do { dirPath <- liftIO $ getCurrentDirectory -- select changes current directory!
              ; (filePathM,_) <- selectOutputFile "" "" renderingLvl
              ; liftIO $ setCurrentDirectory dirPath    -- so change it back
              ; debugLnGUI GUI $ show filePathM
              ; let editEvent = maybe (SkipRen 0) SaveFileRen filePathM
              ; return editEvent
              }
          _      -> return $ SkipRen 0

                
    ; renderingLvlM' <- genericHandler windowID handler renderingLvl editEvent
    ;  case renderingLvlM' of
         Nothing            -> debug Err ("Gui.openFileMenuHandler: "++menuItem++" event failed") $ return renderingLvl
         Just renderingLvl' -> return renderingLvl'
    }


popupMenuHandler :: (DocumentLevel->DocumentLevel) ->
                    Id -> ((RenderingLevel, EditRendering) -> IO (RenderingLevel, EditRendering')) 
                    -> RenderingLevel -> GUI RenderingLevel RenderingLevel
popupMenuHandler editDoc windowID handler renderingLvl =
 do { let editEvent = (UpdateDocRen editDoc)
                                
    ; renderingLvlM' <- genericHandler windowID handler renderingLvl editEvent
    ;  case renderingLvlM' of
         Nothing            -> debug Err ("Gui.popupMenuHandler: Event failed") $ return renderingLvl
         Just renderingLvl' -> return renderingLvl'
    }


keyboardHandler :: Id -> ((RenderingLevel, EditRendering) -> IO (RenderingLevel, EditRendering')) 
                -> KeyboardState -> RenderingLevel -> GUI RenderingLevel RenderingLevel
keyboardHandler windowID handler kstate renderingLvl =
 do { let editEvent = case kstate of
                        CharKey key (KeyDown _)         -> KeyCharRen key
                        SpecialKey spkey (KeyDown False) ms -> 
                          case translateSpecialKey spkey of
                            Just spKeyA -> KeySpecialRen spKeyA (translateModifiers ms)
                            Nothing    -> SkipRen 0
                        SpecialKey spkey (KeyDown True) ms ->   -- special case for repeat, due to ObjectIO bug 
                          case translateSpecialKey spkey of     -- see end of file
                            Just CommonTypes.EnterKey -> SkipRen 0                         
                            Just CommonTypes.DeleteKey -> SkipRen 0                         
                            Just CommonTypes.BackspaceKey -> SkipRen 0                         
                            Just spKeyA -> KeySpecialRen spKeyA (translateModifiers ms)
                            Nothing    -> SkipRen 0                        
                        _                               -> SkipRen 0  
    ; debugLnGUI GUI $ "keyboard event: " ++ show kstate
  
    ; renderingLvlM' <- genericHandler windowID handler renderingLvl editEvent
    ;  case renderingLvlM' of
         Nothing            -> debug Err "Gui.keyBoardHandler: keyboardEvent failed" $ return renderingLvl
         Just renderingLvl' -> return renderingLvl'
    }

--data RenderingLevel = RenderingLevel Scale GUICommand Rendering Debugging UpdatedRegions
mouseHandler :: Id -> Id -> ((RenderingLevel, EditRendering) -> IO (RenderingLevel, EditRendering')) 
             -> MouseState -> RenderingLevel -> GUI RenderingLevel RenderingLevel
mouseHandler cID windowID handler mstate renderingLvl@(RenderingLevel _ makePopupMenu _ _ _ _)  =
 do { editEvent <- case mstate of
                     (MouseUp   (Point2 x y) (Modifiers _ _ _ True _))   -> do { makePopupMenu windowID handler x y; return $ SkipRen 0 }
                     (MouseDown (Point2 x y) (Modifiers _ _ _ True _) _) -> return $ SkipRen 0
                     (MouseDrag (Point2 x y) (Modifiers _ _ _ True _))   -> return $ SkipRen 0
                     (MouseDown (Point2 x y) modifiers i) -> return $ MouseDownRen x y (translateModifiers modifiers) i
                     (MouseDrag (Point2 x y) modifiers)   -> return $ MouseDragRen x y (translateModifiers modifiers)

--                     (MouseUp   (Point2 x y) modifiers)   -> return $ MouseUpRen x y (translateModifiers modifiers)
                     _                                    -> return $ SkipRen 0
-- up is disabled for now, because it is not used and causes a lot of redrawing    
--    ; debugLnGUI GUI $ "mouse event: " ++ show mstate
    ; case editEvent of 
        MouseDragRen _ _ (CommonTypes.Modifiers False False True) -> setWindowCursor windowID FatCrossCursor
        _                                                        -> setWindowCursor windowID StandardCursor
 -- it's better to keep track of dragging status and only update the cursor on change, but cursor changes
 -- are probably not expensive
 -- Unfortunately, ObjectIO has no drag cursor
 
    ; renderingLvlM' <- genericHandler windowID handler renderingLvl editEvent
    ;  case renderingLvlM' of
         Nothing            -> debug Err "Gui.mouseHandler: mouseEvent failed" $ return renderingLvl
         Just renderingLvl' -> return renderingLvl'
    }


                
                


-- we'd rather pattern match here, but ObjectIO doesn't export the constructors
translateSpecialKey :: SpecialKey -> Maybe CommonTypes.SpecialKey
translateSpecialKey spKey = lookup spKey keyMap
keyMap = [ (enterKey, CommonTypes.EnterKey) 
     , (backSpaceKey, CommonTypes.BackspaceKey)
     , (deleteKey   , CommonTypes.DeleteKey)   
     , (leftKey     , CommonTypes.LeftKey)    
     , (rightKey    , CommonTypes.RightKey)    
     , (upKey       , CommonTypes.UpKey)    
     , (downKey     , CommonTypes.DownKey)
     , (f1Key       , CommonTypes.F1Key)
     , (f2Key       , CommonTypes.F2Key)
     , (f3Key       , CommonTypes.F3Key)
     , (f4Key       , CommonTypes.F4Key)
     , (f5Key       , CommonTypes.F5Key)
     , (f6Key       , CommonTypes.F6Key)
     , (f7Key       , CommonTypes.F7Key)
     , (f8Key       , CommonTypes.F8Key)
     , (f9Key       , CommonTypes.F9Key)
     , (f10Key      , CommonTypes.F10Key)
     , (f11Key      , CommonTypes.F11Key)
     , (f12Key      , CommonTypes.F12Key)
     ]
     
translateModifiers :: Modifiers -> CommonTypes.Modifiers
translateModifiers (Modifiers s _ _ c a) = CommonTypes.Modifiers s c a

-- menuHandler, buttonHandler, ...




-------------- Experiments with dynamic tooltip support:


tooltipLook str _ _ = 
 do { setPenColour (RGB 255 255 0)
    ; fillAt (Point2 0 0) (Box 100 100)
    ; setPenColour (RGB 0 0 0)
    ; drawAt (Point2 0 15) str
    }


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

