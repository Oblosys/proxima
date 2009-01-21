{-# LANGUAGE CPP #-} 
-- CPP is enabled only for this module, since it slows the build process down quite a bit
module Rendering.RendererGtk where

import Common.CommonTypes hiding (Rectangle)
import qualified Common.CommonTypes as CommonTypes
import Common.CommonUtils

import Rendering.RenLayerTypes
import Rendering.RenLayerUtils
import Proxima.Wrap

import Arrangement.ArrLayerUtils (point, popupMenuItemsPres, pathPFromPathA')  -- for context menu hack
--import Presentation.PresTypes hiding (font) -- For Locations
import Layout.LayTypes hiding (Point)

import Evaluation.DocTypes (DocumentLevel)
import Arrangement.FontLib

import System.IO.Unsafe
import Data.IORef
import System.IO

import Graphics.UI.Gtk hiding (Scale, Solid, Size, Layout, fill, setSourceColor)
import Graphics.Rendering.Cairo hiding (Path)
import Proxima.GUIGtk


{-
TODO:
- underline and strikeout don't work.
- images: fix load exception and garbage collect. + use clipping 
- line sizes
- scaling
- set view size and title
- putting a tree beneath the graph leads to drawing problems.

-- why does mouse click lead to entire redraw??


-- caret at end rendered 1 pixel to the left because of poor man's incrementality algorithm



GTK sizes:
 filled rectangles and outlined circles have correct widht and height.
 outlined rectangles are 1 pixel too large
 filled circles are 1 pixel too small, and circles of size <= 2 are not drawn at all
-}
{-

Issues:

-F5 gives error.
-multiple spaces are problem
-will image requests by the browser interfere with command queuing?

- popups require imports of PresTypes in Renderer and GUI, maybe restructure this?

- popups cannot handle page scrolling 
  (and probably also not scrolling in elements between root and "proxima", but maybe we don't want to allow proxima inside other elements)

- style and background & fill colors should be implemented a bit more accurately
- handle scaling?
- handle clipping (maybe already done by div elts)
- clean up renderHTML and produce String instead of using files

Strange: after installing catch in handler loop, there were no more commitandrelease errors..

1 inch = 96 px  96 = dpi 
1 pt = 1/72 inch

so 1 pt = dpi/72 px


-}





mkPopupMenuXY :: (DocNode node, Show token) => Settings ->
                 Layout doc node clip token -> Scale -> Arrangement node ->
                 ((RenderingLevel doc enr node clip token, EditRendering doc enr node clip token) ->
                 IO (RenderingLevel doc enr node clip token, [EditRendering' doc enr node clip token])) ->
                 IORef (RenderingLevel doc enr node clip token) ->
                 IORef (Maybe Pixmap) -> IORef CommonTypes.Rectangle -> Window -> Viewport -> DrawingArea -> Int -> Int -> IO (Maybe Menu)
mkPopupMenuXY settings prs scale arr handler renderingLvlVar buffer viewedAreaRef window vp canvas x' y'  =
 do { let (x,y) = (descaleInt scale x',descaleInt scale y')
    ; let ctxtItems = case point x y arr of
                        Nothing -> []
                        Just pthA -> popupMenuItemsPres (pathPFromPathA' arr prs pthA) prs
    ; let menuItems = [ (str, popupMenuHandler settings handler renderingLvlVar buffer viewedAreaRef window vp canvas upd)
                      | (str, upd) <- ctxtItems]
    ; print (map fst menuItems)
    
    ; contextMenu <- mkMenu menuItems
    ; return $ Just contextMenu                                          
    }

escape [] = []
escape ('_':cs) = escape cs
escape ('{':cs) = escape cs
escape ('}':cs) = escape cs
escape (c:cs)   = c : escape cs
 
render' scale arrDb diffTree arrangement (wi,dw,gc) viewedArea =
 do { setLineCap LineCapRound
    ; setLineJoin LineJoinRound
--    ; seq (length (show arrangement)) $ return ()
    ; renderArr undefined (wi,dw,gc) arrDb scale origin viewedArea diffTree arrangement
    }
    
renderFocus scale arrDb focus arrangement (wi, dw, gc) viewedArea =
  let focusArrList = arrangeFocus focus arrangement
  in  do { setLineCap LineCapRound
         ; setLineJoin LineJoinRound
         ; renderArr undefined
                (wi,dw,gc) arrDb scale origin viewedArea
                (DiffLeaf False)
                (OverlayA NoIDA (xA arrangement) (yA arrangement)  
                                (widthA arrangement) (heightA arrangement) 
                                0 0 transparent
                          HeadInFront
                          focusArrList) 
         }

{- make renderArrangement that does background setting (later do this only when needed)
-}
renderArr :: (DocNode node, DrawableClass drawWindow) => Region -> (Window, drawWindow, GC) -> Bool -> Scale -> (Int,Int) ->
                                         (Point, Size) -> DiffTree -> Arrangement node -> Render ()    
renderArr oldClipRegion (wi,dw,gc) arrDb scale (lux, luy) viewedArea diffTree arrangement =
 do { -- debugLnIO Err (shallowShowArr arrangement ++":"++ show (isCleanDT diffTree));
     --if True then return () else    -- uncomment this line to skip rendering


     if (isSelfCleanDT diffTree)  -- if self is clean, only render its children (if present)
     then if (isCleanDT diffTree)
          then return ()
          else let renderChildren x' y' arrs =
                    do { let (x,y)=(lux+scaleInt scale x', luy+scaleInt scale y')
                       ; let childDiffTrees = case diffTree of
                                                DiffLeaf c     -> repeat $ DiffLeaf c
                                                DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
                       ; sequence_ $ zipWith (renderArr oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs 
                       }
               in case arrangement of
                    RowA     _ x' y' _ _ _ _ _ arrs -> renderChildren x' y' arrs
                    ColA     _ x' y' _ _ _ _ _ _ arrs -> renderChildren x' y' arrs
                    OverlayA _ x' y' _ _ _ _ _ _ arrs -> renderChildren x' y' arrs
                    GraphA   _ x' y' _ _ _ _ _ _ arrs -> renderChildren x' y' arrs
                    VertexA  _ x' y' _ _ _ _ _ _ arr  -> renderChildren x' y' [arr]
                    StructuralA _ arr           -> renderChildren 0 0 [arr]
                    ParsingA _ arr              -> renderChildren 0 0 [arr]
                    LocatorA _ arr              -> renderChildren 0 0 [arr]
                    _ -> return ()
     else -- in this case, all children are also dirty (as enforced by ArrUtils.diffArr)
          --when (overlap ((lux+xA arrangement, luy+yA arrangement),
          --               (widthA arrangement, heightA arrangement)) viewedArea) $
          -- only render when the arrangement is in the viewed area   
  case arrangement of 

    (EmptyA  id x' y' w' h' _ _ bColor) ->
      return () {-  do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
           
           ; when (not (isTransparent bColor)) $
               drawFilledRectangle (Rectangle x y w h) bColor bColor                              
           } -}
     {- do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; when (not (isTransparent bColor)) $
           do { let bgColor = gtkColor bColor -- if isCleanDT diffTree then gtkColor bColor else red
              ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
              }
        } -}
        
      
    (StringA id x' y' w' h' _ vRef' str fColor bColor fnt _) ->
      if str == "" then return () else       
        do { let (x,y,w,h, vRef)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h', scaleInt scale vRef')
           
           ; when (not (isTransparent bColor)) $
               drawFilledRectangle (Rectangle x y w h) bColor bColor                              
           -- TODO background (factorize this out)
           
           ; selectFontFace (fFamily fnt) 
                            (if fItalic fnt then FontSlantItalic else FontSlantNormal) 
                            (if fBold fnt   then FontWeightBold  else FontWeightNormal)
           ; setFontSize ((fromIntegral $ fSize fnt)*1.25)
           -- TODO fontsize seems to be in pixels
           
           ; setSourceColor fColor
           
           ; fExts <- fontExtents     
           ; moveTo (fromIntegral x) (fromIntegral y + fontExtentsAscent fExts)
           
           ; showText str
           }
  {-      ; when (str /= "") $ 
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = gtkColor bColor
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }
           
              ; context <- widgetCreatePangoContext wi
              ; fontDescription <- fontDescriptionFromProximaFont fnt
              
              ; language <- contextGetLanguage context
              ; metrics <- contextGetMetrics context fontDescription language
              ; let ascnt = round $ ascent metrics    
           
              ; gcSetValues gc $ newGCValues { foreground = gtkColor fColor }
                        
              ; pangoItems <- pangoItemize context str 
                                [ AttrFontDescription 0 (length str) fontDescription
                                , AttrUnderline 0 (length str) (if fUnderline fnt then UnderlineSingle else UnderlineNone)
                                , AttrStrikethrough 0 (length str) (fStrikeOut fnt)
                                ] -- underline and strikeout don't seem to work
              ; case pangoItems of 
                  [pangoItem] -> do { glyphItem <- pangoShape (head' "Renderer.renderArr" pangoItems)
                                    ; drawGlyphs dw gc x (y+ascnt) glyphItem
                                    }
                  pangoItem:_ -> do { glyphItem <- pangoShape (head' "Renderer.renderArr" pangoItems)
                                    ; drawGlyphs dw gc x y glyphItem
                                    ; debug Err ("Renderer.renderArr: too many pango items for string \""++str++"\"") $ return ()
                                    }
                  []          -> debug Err ("Renderer.renderArr: no pango items for string \""++str++"\"") $ return ()


           ; putStrLn $ "Ascent:" ++ show ascnt ++ show str 
 
            ; context <- widgetCreatePangoContext wi
            ; fontDescription <- fontDescriptionNew
            ; fontDescriptionSetFamily fontDescription "Courier New"
            ; fontDescriptionSetSize fontDescription 26
    
            ; gcSetValues gc $ newGCValues { foreground = gtkColor (0,0,0) }
            ; pangoItems <- pangoItemize context str [ AttrFontDescription 0 (length str) fontDescription ] 
            ; case pangoItems of 
                [pangoItem] ->
                       do { glyphItem <- pangoShape (head' "GUI.onPaint" pangoItems)
                          ; drawGlyphs dw gc x y glyphItem
                          }


              }
-}
{-

              ; context <- widgetCreatePangoContext wi
              ; fontDescription <-  fontDescriptionFromProximaFont fnt

              ; language <- contextGetLanguage context
              ; metrics <- contextGetMetrics context fontDescription language
              ; let ascnt = round $ ascent metrics    
              -- ; putStrLn $ "Ascent:" ++ show ascnt 
              ; gcSetValues gc $ newGCValues { foreground = gtkColor (0,0,0) }
              ; pangoItems <- pangoItemize context str
                                [ AttrFontDescription 0 (length str) fontDescription
                                , AttrUnderline 0 (length str) (if fUnderline fnt then UnderlineSingle else UnderlineNone)
                                , AttrStrikethrough 0 (length str) (fStrikeOut fnt)
                                ] -- underline and strikeout don't seem to work

            ; case pangoItems of 
                [pangoItem] ->
                       do { glyphItem <- pangoShape (head' "GUI.onPaint" pangoItems)
                          ; drawGlyphs dw gc x y glyphItem
                          }
                pangoItem:_ -> do { glyphItem <- pangoShape (head' "Renderer.renderArr" pangoItems)
                                    ; drawGlyphs dw gc x y glyphItem
                                    ; debug Err ("Renderer.renderArr: too many pango items for string \""++str++"\"") $ return ()
                                    }
                []          -> debug Err ("Renderer.renderArr: no pango items for string \""++str++"\""++show (length str)) $ return ()


 
        ; when arrDb $
           do { gcSetValues gc $ newGCValues { foreground = stringColor }
              ; when (w>0 && h>0) $ drawRectangle dw gc False x y (w-1) (h-1)
              -- outlined gtk rectangles are 1 pixel larger than filled ones
              }
        }
-}
{-          

    (ImageA id x' y' w' h' _ _ src style lColor bColor) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; when (not arrDb) $
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = gtkColor bColor
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }
              } 

        ; when arrDb $
            drawFilledRectangle dw gc (Rectangle x y w h) imageColor imageColor
 
-- Unfortunately, this check cannot distinguish between 6.6 and 6.6.1 (both are 606)
-- Therefore, when using GHC 6.6.1, the #else part must be enabled manually
#if __GLASGOW_HASKELL__ <= 606
        ; ePB <- pixbufNewFromFile src 
        ; case ePB of
            Left (_,errStr) -> debugLnIO Err $ "Renderer.renderArr: could not open bitmap "++show src ++
                                               "\n" ++ errStr
            Right pb ->
             do { 
#else
        ; pb <- pixbufNewFromFile src
        ;    do {
#endif 
-- TODO After removing these cpp constructs, also remove option LANGUAGE CPP at top of this file
                -- TODO: make this the Tile case and make a Resize case, and add clipping
                -- and store pixbufs in rendering state, to avoid reloading.
                
                -- ; pb <- pixbufScaleSimple pb w h InterpBilinear       -- scale the pixmap
                
                ; iw <- pixbufGetWidth pb
                ; ih <- pixbufGetHeight pb

                ; when (iw /= 0 && ih /= 0) $
             do { let (hrepeats, hrest) = w `divMod` iw 
                ; let (vrepeats, vrest) = h `divMod` ih 
                ; gcSetValues gc $ newGCValues { foreground = gtkColor lColor }
              
                ; sequence_ [ drawPixbuf dw gc pb 0 0 (x+i*iw) (y+j*ih) iw ih RgbDitherNormal 0 0 
                            | i<-[0..hrepeats-1], j<-[0..vrepeats-1] ]
                ; sequence_ [ drawPixbuf dw gc pb 0 0 (x+i*iw) (y+vrepeats*ih) iw vrest RgbDitherNormal 0 0 
                            | i<-[0..hrepeats-1] ] -- clipped images at the bottom
                ; sequence_ [ drawPixbuf dw gc pb 0 0 (x+hrepeats*iw) (y+j*ih) hrest ih RgbDitherNormal 0 0 
                            | j<-[0..vrepeats-1] ] -- clipped images on the right

                ; drawPixbuf dw gc pb 0 0 (x+hrepeats*iw) (y+vrepeats*ih) hrest vrest RgbDitherNormal 0 0 
                  -- clipped image at bottom right
                }}               
        }

    (RectangleA id x' y' w' h' _ _ lw' style lColor fColor bColor) ->
        ; setLineWidth (scale * fromIntegral lw')
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; when (not arrDb) $
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = gtkColor bColor
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }
              } 
        ; when (style == Solid) $
            do { gcSetValues gc $ newGCValues { foreground = gtkColor fColor }
               ; drawRectangle dw gc True x y w h
               }
        ; gcSetValues gc $ newGCValues { foreground = gtkColor lColor, lineWidth = scaleInt scale lw' `max` 1 }
        
        ; when (w>0 && h>0) $ drawRectangle dw gc False x y (w-1) (h-1)
        -- outlined gtk rectangles are 1 pixel larger than filled ones
        }
-}
    (EllipseA id x' y' w' h' _ _ lw' style lColor fColor bColor) ->
     do { let (x,y,w,h)= (lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')       
        ; when (not (isTransparent bColor)) $
            drawFilledRectangle (Rectangle x y w h) bColor bColor

        ; when (style == Solid) $
            do { setSourceColor fColor
               ; save
               ; translate (fromIntegral x + fromIntegral w / 2) (fromIntegral y + fromIntegral h / 2)
               ; Graphics.Rendering.Cairo.scale (fromIntegral h / 2) (fromIntegral w / 2)
               ; arc 0 0 1 0 (2 * pi)
               ; restore
               ; fill
               }
        ; setLineWidth (scale * fromIntegral lw')
        
        ; setSourceColor lColor
        ; save
        ; translate (fromIntegral x + fromIntegral w / 2) (fromIntegral y + fromIntegral h / 2)
        ; Graphics.Rendering.Cairo.scale (fromIntegral h / 2) (fromIntegral w / 2)
        ; arc 0 0 1 0 (2 * pi)
        ; restore
        ; stroke
        }

{-
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')       
        ; when (not arrDb) $
               drawFilledRectangle (Rectangle x y w h) bColor bColor
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = gtkColor bColor
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }
              } 
        ; when (style == Solid) $
            do { gcSetValues gc $ newGCValues { foreground = gtkColor fColor }
               ; drawArc dw gc True x y (w+1) (h+1) (0*64) (360*64)
               -- filled gtk arcs are 1 pixel smaller than outlined ones
               }
        ; gcSetValues gc $ newGCValues { foreground = gtkColor lColor, lineWidth = scaleInt scale lw' `max` 1 }
        ; drawArc dw gc False x y w h (0*64) (360*64)
        }
-}

    (PolyA id x' y' w' h' _ _ pts' lw' style lColor fColor bColor) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let pts = map (\(x',y') -> (fromIntegral $ x+scaleInt scale x', fromIntegral $ y+scaleInt scale y')) pts'
        ; setAntialias AntialiasNone

        ; setLineWidth (scale * fromIntegral lw')
        ; setSourceColor lColor
        
        ; case pts of
           []             -> return ()
           ((startX,startY):points) -> do { moveTo startX startY
                                          ; mapM_ (uncurry lineTo) points
                                          }
           
        ; stroke
        {-
        
       
        ; when (style == Solid) $
            do { gcSetValues gc $ newGCValues { foreground = gtkColor fColor }
               ; drawPolygon dw gc True pts
               }
        ; gcSetValues gc $ newGCValues { foreground = gtkColor lColor, lineWidth = scaleInt scale lw' `max` 1 
                                       , joinStyle = JoinRound }
        --; drawPolygon dw gc False pts
        -}
        
        ; setAntialias AntialiasDefault

        ; return ()
        }
{-
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let pts = map (\(x',y') -> (x+scaleInt scale x', y+scaleInt scale y')) pts'
  

        ; when (not arrDb) $
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = gtkColor bColor
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }
              } 
        
        
        -- the old clip region needs to be passed around, since gtk does not offer a getClip..
        ; newClipRegion <- regionRectangle $ Rectangle x y w h
        ; regionIntersect newClipRegion oldClipRegion
        -- intersect, so we don't draw outside the old clip region
       
        ; gcSetClipRegion gc newClipRegion
        ; when (style == Solid) $
            do { gcSetValues gc $ newGCValues { foreground = gtkColor fColor }
               ; drawPolygon dw gc True pts
               }
        ; gcSetValues gc $ newGCValues { foreground = gtkColor lColor, lineWidth = scaleInt scale lw' `max` 1 
                                       , joinStyle = JoinRound }
        ; drawPolygon dw gc False pts
        ; gcSetClipRegion gc oldClipRegion
        
        }
-}

    (RowA id x' y' w' h' _ _ bColor arrs) ->
      do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
         ; when (not (isTransparent bColor)) $
            drawFilledRectangle (Rectangle x y w h) bColor bColor

         
         ; let childDiffTrees = case diffTree of
                                  DiffLeaf c     -> repeat $ DiffLeaf c
                                  DiffNode c c' dts -> dts ++ repeat (DiffLeaf False) -- in case there are too few dts
         ; sequence_ $ zipWith (renderArr oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs
         }      
{-     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False) -- in case there are too few dts

        ; if arrDb
          then
           do { let childCnt = length arrs 

              ; gcSetValues gc $ newGCValues { foreground = rowColor }
              ; when (w>0 && h>0) $ drawRectangle dw gc False x y (w-1) (h-1)
              -- outlined gtk rectangles are 1 pixel larger than filled ones
              
               ; sequence_ $ [ drawLine dw gc (x+i,y) (x+i,y+h-1)
                             | i <- if null arrs then []
                                                 else tail.init $
                                                      scanl (\n1 n2 -> n1 + (scaleInt scale (pd-1))+n2 ) 0 
                                                            (map (scaleInt scale . widthA) arrs)]
              ; sequence_ $ zipWith (renderArr oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs
             }
          else 
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = gtkColor bColor -- if isCleanDT diffTree then gtkColor bColor else red
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }
              ; sequence_ $ zipWith (renderArr oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs
              }
        }
-}
    (ColA id x' y' w' h' _ _ bColor _ arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
         ; when (not (isTransparent bColor)) $
            drawFilledRectangle (Rectangle x y w h) bColor bColor

        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; sequence_ $ zipWith (renderArr oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs
        }
{-
        ; if arrDb
          then
           do { let childCnt = length arrs 

              ; gcSetValues gc $ newGCValues { foreground = colColor }
              ; when (w>0 && h>0) $ drawRectangle dw gc False x y (w-1) (h-1)
              -- outlined gtk rectangles are 1 pixel larger than filled ones
                            
              ; sequence_ $ [ drawLine dw gc (x,y+i) (x+w-1,y+i)
                            | i <- if null arrs then []
                                                else tail.init $
                                                     scanl (\n1 n2 -> n1+(scaleInt scale (pd-1))+n2 ) 0 
                                                           (map (scaleInt scale . heightA) arrs)]
        
              ; sequence_ $ zipWith (renderArr oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs
              }
          else 
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = gtkColor bColor --  if isCleanDT diffTree then gtkColor bColor else red
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }
              ; sequence_ $ zipWith (renderArr oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs
              }
        }
-}
    (OverlayA id x' y' w' h' _ _ bColor direction arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)

        ; let order = case direction of
                        HeadInFront -> reverse
                        HeadAtBack  -> Prelude.id
                              
        ; sequence_ $ order $
            zipWith (renderArr oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs
        }


{-
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)

        ; if arrDb
          then
           do { let childCnt = length arrs 

              ; gcSetValues gc $ newGCValues { foreground = overlayColor }
              ; when (w>0 && h>0) $ drawRectangle dw gc False x y (w-1) (h-1)
              -- outlined gtk rectangles are 1 pixel larger than filled ones
              }
          else 
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = gtkColor bColor -- if isCleanDT diffTree then gtkColor bColor else red
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }
                    
              }
        ; let order = case direction of
                        HeadInFront -> reverse
                        HeadAtBack  -> Prelude.id
                              
        ; sequence_ $ order $
            zipWith (renderArr oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs
        }
-}
    (GraphA id x' y' w' h' _ _ bColor _ arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)

        {- 
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = gtkColor bColor -- if isCleanDT diffTree then gtkColor bColor else red
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }        
              } -}
        ; sequence_ $ reverse $ zipWith (renderArr oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs -- reverse so first is drawn in front
        }
 {-   do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)

        ; newClipRegion <- regionRectangle $ Rectangle x y w h
        ; regionIntersect newClipRegion oldClipRegion
        -- intersect, so we don't draw outside the old clip region
        ; gcSetClipRegion gc newClipRegion
         
        ; if arrDb then
            drawFilledRectangle dw gc (Rectangle x y w h) graphColor graphColor
          else 
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = gtkColor bColor -- if isCleanDT diffTree then gtkColor bColor else red
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }        
              }
        ; sequence_ $ reverse $ zipWith (renderArr newClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs -- reverse so first is drawn in front
        ; gcSetClipRegion gc oldClipRegion
        }
-}
    (VertexA id x' y' w' h' _ _ bColor _ arr) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        
        
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
--        ; when arrDb $
--            drawFilledRectangle dw gc (Rectangle x y w h) vertexColor vertexColor

        ; renderArr oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea (head' "Renderer.renderArr" childDiffTrees) arr
        }
{-     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        
        ; when (not (isTransparent bColor)) $
           do { let bgColor = gtkColor bColor
              ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
              }
        
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; when arrDb $
            drawFilledRectangle dw gc (Rectangle x y w h) vertexColor vertexColor

        ; renderArr oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea (head' "Renderer.renderArr" childDiffTrees) arr
        }
-}
    (EdgeA id lux' luy' rlx' rly' _ _ lw' lColor) ->
     do { let (fromx, fromy, tox, toy)=(fromIntegral $ lux+scaleInt scale lux', fromIntegral $ luy+scaleInt scale luy', fromIntegral $ lux+scaleInt scale rlx', fromIntegral $ luy+scaleInt scale rly')
              angleFromEnd = atan ((tox-fromx) / (toy-fromy)) -- atan works okay for pos and neg infinity
                             + if fromy > toy then pi  else 0
              
              (head1x,head1y) = (tox - (arrowHeadSize * sin (angleFromEnd + arrowHeadHalfAngle)), toy - (arrowHeadSize * cos (angleFromEnd + arrowHeadHalfAngle))) 
              (head2x,head2y) = (tox - (arrowHeadSize * sin (angleFromEnd - arrowHeadHalfAngle)), toy - (arrowHeadSize * cos (angleFromEnd - arrowHeadHalfAngle))) 

        ; setLineWidth (scale * fromIntegral lw')

        ; setSourceColor lColor
        
        ; moveTo fromx fromy
        ; lineTo tox toy
        ; stroke
        ; moveTo tox toy -- this one is necessary
        ; lineTo head1x head1y
        ; lineTo head2x head2y
        ; fill
{-
          let (fromx, fromy, tox, toy)=(lux+scaleInt scale lux', luy+scaleInt scale luy', lux+scaleInt scale rlx', luy+scaleInt scale rly')
        ; let angleFromEnd = atan (fromIntegral (tox-fromx) / fromIntegral (toy-fromy)) -- atan works okay for pos and neg infinity
                             + if fromy > toy then pi  else 0
              
              pt1 = (tox - round (arrowHeadSize * sin (angleFromEnd + arrowHeadHalfAngle)), toy - round (arrowHeadSize * cos (angleFromEnd + arrowHeadHalfAngle))) 
              pt2 = (tox - round (arrowHeadSize * sin (angleFromEnd - arrowHeadHalfAngle)), toy - round (arrowHeadSize * cos (angleFromEnd - arrowHeadHalfAngle))) 
        
        
        ; moveTo (fromIntegral fromx) (fromIntegral fromy)
        ; lineTo (fromIntegral tox)   (fromIntegral toy)
        -- draw arrow head
        ; lineTo (fromIntegral $ fst pt1) (fromIntegral $ snd pt1)
        ; lineTo (fromIntegral $ fst pt2) (fromIntegral $ snd pt2)
        
--        ; drawPolygon dw gc True [pt1, pt2, (tox, toy)] 
        ; stroke
--        ; fill
-}
        {-
        
       
        ; when (style == Solid) $
            do { gcSetValues gc $ newGCValues { foreground = gtkColor fColor }
               ; drawPolygon dw gc True pts
               }
        ; gcSetValues gc $ newGCValues { foreground = gtkColor lColor, lineWidth = scaleInt scale lw' `max` 1 
                                       , joinStyle = JoinRound }
        --; drawPolygon dw gc False pts
        -}
        
        ; setAntialias AntialiasDefault

        ; return ()
        }


{-     do { let (fromx, fromy, tox, toy)=(lux+scaleInt scale lux', luy+scaleInt scale luy', lux+scaleInt scale rlx', luy+scaleInt scale rly')
        ; let angleFromEnd = atan (fromIntegral (tox-fromx) / fromIntegral (toy-fromy)) -- atan works okay for pos and neg infinity
                             + if fromy > toy then pi  else 0
              
              pt1 = (tox - round (arrowHeadSize * sin (angleFromEnd + arrowHeadHalfAngle)), toy - round (arrowHeadSize * cos (angleFromEnd + arrowHeadHalfAngle))) 
              pt2 = (tox - round (arrowHeadSize * sin (angleFromEnd - arrowHeadHalfAngle)), toy - round (arrowHeadSize * cos (angleFromEnd - arrowHeadHalfAngle))) 
        
        ; gcSetValues gc $ newGCValues { foreground = gtkColor lColor, lineWidth = scaleInt scale lw' `max` 1 }
        ; drawLine dw gc (fromx,fromy) (tox,toy) 
        -- draw arrow head
        ; drawPolygon dw gc True [pt1, pt2, (tox, toy)] 
        }
-}
    (StructuralA id arr) -> 
     do { let (x,y,w,h)=( lux+scaleInt scale (xA arr), luy+scaleInt scale (yA arr) 
                        , scaleInt scale (widthA arr), scaleInt scale (heightA arr) )
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
--        ; when arrDb $
--            drawFilledRectangle dw gc (Rectangle x y w h) structuralBGColor structuralBGColor
       
        ; renderArr oldClipRegion (wi,dw,gc) arrDb scale (lux, luy) viewedArea (head' "Renderer.renderArr" childDiffTrees) arr
        }
    
    (ParsingA id arr) ->
     do { let (x,y,w,h)=( lux+scaleInt scale (xA arr), luy+scaleInt scale (yA arr) 
                        , scaleInt scale (widthA arr), scaleInt scale (heightA arr) )
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
--        ; when arrDb $
--            drawFilledRectangle dw gc (Rectangle x y w h) parsingBGColor parsingBGColor
       
        ; renderArr oldClipRegion (wi,dw,gc) arrDb scale (lux, luy) viewedArea (head' "Renderer.renderArr" childDiffTrees) arr
        }

    (LocatorA _ arr) ->
     do {
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; renderArr oldClipRegion (wi,dw,gc) arrDb scale (lux, luy) viewedArea (head' "Renderer.renderArr" childDiffTrees) arr
        }

    _ ->  return () --dcDrawText dc ("unimplemented arrangement: "++shallowShowArr arrangement) (pt lux luy)
        

--  ; when arrDb $
--      renderID (wi,dw,gc) scale (lux+xA arrangement) (luy+yA arrangement) (idA arrangement)      
  }

cairoR (r,g,b) = fromIntegral r / 255
cairoG (r,g,b) = fromIntegral g / 255
cairoB (r,g,b) = fromIntegral b / 255

setSourceColor (r,g,b) = setSourceRGB (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255)

drawFilledRectangle :: Rectangle -> CommonTypes.Color -> CommonTypes.Color -> Render ()
drawFilledRectangle (Rectangle x y w h) lineColor fillColor =
 do setSourceColor fillColor
    rectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
    fill

{-
 do { gcSetValues gc $ newGCValues { foreground = fillColor }
    ; drawRectangle dw gc True x y w h 
    ; gcSetValues gc $ newGCValues { foreground = lineColor }
    ; when (w>0 && h>0) $ drawRectangle dw gc False x y (w-1) (h-1)
    -- outlined gtk rectangles are 1 pixel larger than filled ones
    } 
-}

-- usage: for basic Arrangements, render after image so id is visible
--        for composites,         render before, so stacking order of arrangement is represented correctly

renderID (wi,dw,gc) scale x y id  = 
 do { let idStr = case id of NoIDA -> ""
                             IDA i -> show i

    ; context <- widgetCreatePangoContext wi
    ; fontDescription <- fontDescriptionNew
    ; fontDescriptionSetFamily fontDescription "Courier New"
    ; fontDescriptionSetSize fontDescription 6
    
    ; gcSetValues gc $ newGCValues { foreground = gtkColor black }
    ; pangoItems <- pangoItemize context idStr [ AttrFontDescription 0 (length idStr) fontDescription ] 
    ; case pangoItems of 
        [pangoItem] -> do { glyphItem <- pangoShape (head' "Renderer.renderID" pangoItems)
                          ; (_, PangoRectangle x' y' w' h') <-glyphItemExtents glyphItem
                          --; drawFilledRectangle dw gc (Rectangle (x+round x') y (round w'+2) (round h'+1)) (gtkColor black) (gtkColor yellow)
                          ; drawGlyphs dw gc (x+1) (y+1 - round y') glyphItem
                          }
        _ -> return ()
    }

-- debug colors, only supported for Gtk rendering

stringColor       = gtkColor (0, 255, 255)
imageColor        = gtkColor (92, 64, 0)
rowColor          = gtkColor (255, 0, 0)
colColor          = gtkColor (0, 0, 200) 
overlayColor      = gtkColor (0, 0, 200) 
graphColor        = gtkColor (255, 255, 0)
vertexColor       = gtkColor (255, 0, 255)
structuralBGColor = gtkColor (230, 230, 255)
parsingBGColor    = gtkColor (255, 230, 230)
