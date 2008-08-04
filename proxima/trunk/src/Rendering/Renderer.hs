{-# LANGUAGE CPP #-} 
-- CPP is enabled only for this module, since it slows the build process down quite a bit
module Rendering.Renderer where

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
import Proxima.GUI
import Arrangement.FontLib

import Graphics.UI.Gtk hiding (Scale, Solid, Size, Layout)
import System.IO.Unsafe
import Data.IORef
import System.IO
-----

arrowHeadSize :: Double
arrowHeadSize = 10

arrowHeadHalfAngle :: Double
arrowHeadHalfAngle = pi /6

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


computeUpdatedRegions oldUpdRegions scale focus diffTree oldArrangement arrangement =
  -- showDebug' Err ("updated regions for\n" ++ show oldArrangement ++"\n\n\n" ++ show arrangement ++ "\n\n\n\n" ) $
  let (oldW,oldH) = (widthA oldArrangement, heightA oldArrangement)
      (newW,newH) = (widthA arrangement, heightA arrangement)
  in if oldW>newW || oldH > newH     -- if arr got smaller, repaint whole thing for now
     then [((0, 0),(max oldW newW, max oldH newH))]
     else updatedRectArr diffTree arrangement  

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
              
    ; contextMenu <- mkMenu [ (str, popupMenuHandler settings handler renderingLvlVar buffer viewedAreaRef window vp canvas upd)
                            | (str, upd) <- ctxtItems]
    ; return $ Just contextMenu                                          
    }

--render' :: (LocalStateRen, MappingRen) -> Arrangement -> (Rendering, (LocalStateRen, MappingRen))
render' scale arrDb diffTree arrangement (wi,dw,gc) viewedArea =
 do { -- seq (walk arrangement) $ return ()        -- maybe this is not necessary anymore, now the datastructure is strict
    --; putStrLn $ "Rendering on viewedArea " ++ show viewedArea
    --; putStrLn $ "DiffTree is " ++ show diffTree
    --; debugLnIO Ren ("Arrangement is "++show arrangement)
    --; debugLnIO Err ("The updated rectangle is: "++show (updatedRectArr diffTree arrangement))
    ; clipRegion <- regionRectangle $ Rectangle (xA arrangement) (yA arrangement) (widthA arrangement) (heightA arrangement)
    ; fh <- openFile "rendering.html" WriteMode
    ; renderArr fh clipRegion
                (wi,dw,gc) arrDb scale origin viewedArea diffTree arrangement
    ; hClose fh
    }


-- old comment: debugged rendering also displays overlay for focus adding, but this has not been processed by debugArrangement
-- this makes it tricky to move the debuggedArrangement, since the Gest.Int. will not know about it
-- however, we don't want to debug the focus
    
renderFocus scale arrDb focus arrangement (wi, dw, gc) viewedArea =
 do { clipRegion <- regionRectangle $ Rectangle (xA arrangement) (yA arrangement) (widthA arrangement) (heightA arrangement)

    ; fh <- openFile "focusRendering.html" WriteMode
    ; let focusArrList = arrangeFocus focus arrangement
--    ; debugLnIO Ren ("Focus: "++show focus ++ "\nFocus arrangement:\n"++show focusArrList)
    ; renderArr fh clipRegion
                (wi,dw,gc) arrDb scale origin viewedArea
                (DiffLeaf False)
                (OverlayA NoIDA (xA arrangement) (yA arrangement)  
                                (widthA arrangement) (heightA arrangement) 
                                0 0 transparent
                          HeadInFront
                          focusArrList) 
   ; hClose fh
   }

divOpen fh x y w h (r,g,b) = hPutStr fh $ 
  "<div style='position: absolute; left:"++show x++"; top:"++show y++";"++
                "width:"++show w++"height:"++show h++";"++
                (if r /= -1 then "background-color:rgb("++show (r::Int)++","++show (g::Int)++","++show (b::Int)++");"
                           else "")++
                "'>"
divClose fh = hPutStr fh "</div>"

polyHTML fh x y w h pts (r,g,b) = hPutStr fh $
  "<div style='position: absolute; left:"++show x++"; top:"++show y++";"++
                "width:"++show w++"; height:"++show h++";"++
                "background-color:rgb("++show (r::Int)++","++show (g::Int)++","++show (b::Int)++");"++
                "'></div>"
 
stringHTML fh str x y w h (Font fFam fSiz fBld fUnderln fItlc fStrkt) (r,g,b) = hPutStr fh $ 
  "<div style='position:absolute;left:"++show x++";top:"++show y++";"++
                "width:"++show w++"; height:"++show h++";"++
                "font-family:"++show fFam++";"++
                "font-size:"++show fSiz++"pt;"++
                (if fBld then "font-weight: bold;" else "")++
                (if fItlc then "font-style: italic;" else "")++
                "color:rgb("++show (r::Int)++","++show (g::Int)++","++show (b::Int)++");"++
                "'>"++
                concatMap htmlChar str ++ "</div>"
 where htmlChar '\n' = "<br/>"
       --htmlChar ' '  = "&#8194;"
       htmlChar ' '  = "&nbsp;"
       htmlChar '<'  = "&lt;"
       htmlChar '>'  = "&gt;"
       htmlChar c    = [c]


renderArr :: (DocNode node, DrawableClass drawWindow) => Handle -> Region -> (Window, drawWindow, GC) -> Bool -> Scale -> (Int,Int) ->
                                         (Point, Size) -> DiffTree -> Arrangement node -> IO ()    
renderArr fh oldClipRegion (wi,dw,gc) arrDb scale (lux, luy) viewedArea diffTree arrangement =
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
                       ; sequence_ $ zipWith (renderArr fh oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs 
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
     else --when (overlap ((lux+xA arrangement, luy+yA arrangement),
          --               (widthA arrangement, heightA arrangement)) viewedArea) $
          -- only render when the arrangement is in the viewed area   
  case arrangement of 

    (EmptyA  id x' y' w' h' _ _ bColor) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; when (not (isTransparent bColor)) $
           do { let bgColor = gtkColor bColor -- if isCleanDT diffTree then gtkColor bColor else red
              ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
              }
        }
      
    (StringA id x' y' w' h' _ vRef' str fColor bColor fnt _) ->
     do { let (x,y,w,h, vRef)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h', scaleInt scale vRef')
        ; when (str /= "") $ 
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
                                    ; stringHTML fh str x' y' w' h' fnt fColor
                                    }
                  pangoItem:_ -> do { glyphItem <- pangoShape (head' "Renderer.renderArr" pangoItems)
                                    ; drawGlyphs dw gc x y glyphItem
                                    ; debug Err ("Renderer.renderArr: too many pango items for string \""++str++"\"") $ return ()
                                    }
                  []          -> debug Err ("Renderer.renderArr: no pango items for string \""++str++"\"") $ return ()
              }
 
          
        ; when arrDb $
           do { gcSetValues gc $ newGCValues { foreground = stringColor }
              ; when (w>0 && h>0) $ drawRectangle dw gc False x y (w-1) (h-1)
              -- outlined gtk rectangles are 1 pixel larger than filled ones
              }
        }

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

    (EllipseA id x' y' w' h' _ _ lw' style lColor fColor bColor) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')       
        ; when (not arrDb) $
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

    (PolyA id x' y' w' h' _ _ pts' lw' style lColor fColor bColor) ->
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
        ; polyHTML fh x' y' w' h' pts lColor
        ; gcSetClipRegion gc oldClipRegion
        
        }


    (RowA id x' y' w' h' _ _ bColor arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
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
              ; sequence_ $ zipWith (renderArr fh oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs
             }
          else 
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = gtkColor bColor -- if isCleanDT diffTree then gtkColor bColor else red
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }
              ; divOpen fh x' y' w' h' bColor
              ; sequence_ $ zipWith (renderArr fh oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs
              ; divClose fh
              }
        }

    (ColA id x' y' w' h' _ _ bColor _ arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)

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
        
              ; sequence_ $ zipWith (renderArr fh oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs
              }
          else 
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = gtkColor bColor --  if isCleanDT diffTree then gtkColor bColor else red
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }
              ; divOpen fh x' y' w' h' bColor
              ; sequence_ $ zipWith (renderArr fh oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs
              ; divClose fh
              }
        }

    (OverlayA id x' y' w' h' _ _ bColor direction arrs) ->
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
            zipWith (renderArr fh oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs
        }

    (GraphA id x' y' w' h' _ _ bColor _ arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
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
        ; sequence_ $ reverse $ zipWith (renderArr fh newClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea) childDiffTrees arrs -- reverse so first is drawn in front
        ; gcSetClipRegion gc oldClipRegion
        }

    (VertexA id x' y' w' h' _ _ bColor _ arr) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        
        ; when (not (isTransparent bColor)) $
           do { let bgColor = gtkColor bColor
              ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
              }
        
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; when arrDb $
            drawFilledRectangle dw gc (Rectangle x y w h) vertexColor vertexColor

        ; renderArr fh oldClipRegion (wi,dw,gc) arrDb scale (x, y) viewedArea (head' "Renderer.renderArr" childDiffTrees) arr
        }

    (EdgeA id lux' luy' rlx' rly' _ _ lw' lColor) ->
     do { let (fromx, fromy, tox, toy)=(lux+scaleInt scale lux', luy+scaleInt scale luy', lux+scaleInt scale rlx', luy+scaleInt scale rly')
        ; let angleFromEnd = atan (fromIntegral (tox-fromx) / fromIntegral (toy-fromy)) -- atan works okay for pos and neg infinity
                             + if fromy > toy then pi  else 0
              
              pt1 = (tox - round (arrowHeadSize * sin (angleFromEnd + arrowHeadHalfAngle)), toy - round (arrowHeadSize * cos (angleFromEnd + arrowHeadHalfAngle))) 
              pt2 = (tox - round (arrowHeadSize * sin (angleFromEnd - arrowHeadHalfAngle)), toy - round (arrowHeadSize * cos (angleFromEnd - arrowHeadHalfAngle))) 
        
        ; gcSetValues gc $ newGCValues { foreground = gtkColor lColor, lineWidth = scaleInt scale lw' `max` 1 }
        ; drawLine dw gc (fromx,fromy) (tox,toy) 
        -- draw arrow head
        ; drawPolygon dw gc True [pt1, pt2, (tox, toy)] 
        }

    (StructuralA id arr) -> 
     do { let (x,y,w,h)=( lux+scaleInt scale (xA arr), luy+scaleInt scale (yA arr) 
                        , scaleInt scale (widthA arr), scaleInt scale (heightA arr) )
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; when arrDb $
            drawFilledRectangle dw gc (Rectangle x y w h) structuralBGColor structuralBGColor
       
        ; renderArr fh oldClipRegion (wi,dw,gc) arrDb scale (lux, luy) viewedArea (head' "Renderer.renderArr" childDiffTrees) arr
        }
    
    (ParsingA id arr) ->
     do { let (x,y,w,h)=( lux+scaleInt scale (xA arr), luy+scaleInt scale (yA arr) 
                        , scaleInt scale (widthA arr), scaleInt scale (heightA arr) )
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; when arrDb $
            drawFilledRectangle dw gc (Rectangle x y w h) parsingBGColor parsingBGColor
       
        ; renderArr fh oldClipRegion (wi,dw,gc) arrDb scale (lux, luy) viewedArea (head' "Renderer.renderArr" childDiffTrees) arr
        }

    (LocatorA _ arr) ->
     do {
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; renderArr fh oldClipRegion (wi,dw,gc) arrDb scale (lux, luy) viewedArea (head' "Renderer.renderArr" childDiffTrees) arr
        }

    _ ->  return () --dcDrawText dc ("unimplemented arrangement: "++shallowShowArr arrangement) (pt lux luy)
        

  ; when arrDb $
      renderID (wi,dw,gc) scale (lux+xA arrangement) (luy+yA arrangement) (idA arrangement)      
  }

drawFilledRectangle :: DrawableClass drawWindow => drawWindow -> GC -> Rectangle -> Graphics.UI.Gtk.Color -> Graphics.UI.Gtk.Color -> IO ()
drawFilledRectangle dw gc (Rectangle x y w h) lineColor fillColor =
 do { gcSetValues gc $ newGCValues { foreground = fillColor }
    ; drawRectangle dw gc True x y w h 
    ; gcSetValues gc $ newGCValues { foreground = lineColor }
    ; when (w>0 && h>0) $ drawRectangle dw gc False x y (w-1) (h-1)
    -- outlined gtk rectangles are 1 pixel larger than filled ones
    } 
                          

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
                          ; drawFilledRectangle dw gc (Rectangle (x+round x') y (round w'+2) (round h'+1)) (gtkColor black) (gtkColor yellow)
                          ; drawGlyphs dw gc (x+1) (y+1 - round y') glyphItem
                          }
        _ -> return ()
    }


-- debug colors
stringColor       = gtkColor (0, 255, 255)
imageColor        = gtkColor (92, 64, 0)
rowColor          = gtkColor (255, 0, 0)
colColor          = gtkColor (0, 0, 200) 
overlayColor      = gtkColor (0, 0, 200) 
graphColor        = gtkColor (255, 255, 0)
vertexColor       = gtkColor (255, 0, 255)
structuralBGColor = gtkColor (230, 230, 255)
parsingBGColor    = gtkColor (255, 230, 230)


-- focus is not perfect, but this has to do with the selection algorithm as well.
-- for now, it is a working solution

arrangeFocus :: Show node => FocusArr -> Arrangement node -> [Arrangement node]
arrangeFocus (FocusA from to) arrangement = {- scaleFocusArrList scale $-} {-debug Ren ("focus:" ++show (from,to)) $-} mkFocus (FocusA from to) arrangement
                                      --[ picobToCaretC from arrangement green
                                      --, picobToCaretC to arrangement blue 
                                      -- ]
arrangeFocus (NoFocusA) _ = []


-- caret at end rendered 1 pixel to the left because of incrementality algorithm

-- ref lines not used, because caret is not incrementally rendered
mkFocus :: Show node => FocusArr -> Arrangement node -> [Arrangement node]
mkFocus focus arr = mkFocus' [] 0 0 (orderFocusA focus) arr



-- precondition, node is only visited if it part of it is focused
mkFocus' p x' y' focus          (EmptyA _ _ _ _ _ _ _ _) = []
mkFocus' p x' y' (FocusA (PathA stp sti) (PathA enp eni)) (StringA _  x y w h _ _ _ _ _ _ cxs) = 
  let st = if length stp < length p|| stp < p then 0 else index "Renderer.mkFocus'" cxs sti
      en = if length enp < length p || enp > p then last cxs else index "Renderer.mkFocus'" cxs eni                        
  in  if length stp > length p && p `isPrefixOf` stp ||
         length enp > length p && p `isPrefixOf` enp
      then debug Err ("Renderer.mkFocus': focus path too long:") []
      else mkBoxCaret (x'+x+st) (y'+y) (en-st + 1) h
mkFocus' p x' y' focus          (ImageA _ x y w h _ _ _ _ _ _)       = mkBoxCaret (x'+x) (y'+y) w h
mkFocus' p x' y' focus          (PolyA _ x y w h _ _ _ _ _ _ _ _)    = mkBoxCaret (x'+x) (y'+y) w h
mkFocus' p x' y' focus          (RectangleA _ x y w h _ _ _ _ _ _ _) = mkBoxCaret (x'+x) (y'+y) w h
mkFocus' p x' y' focus          (EllipseA _ x y w h _ _ _ _ _ _ _)   = mkBoxCaret (x'+x) (y'+y) w h
mkFocus' p x' y' (FocusA st en) (RowA _ x y w h _ _ _ arrs) = mkFocusList' p 0 (x'+x) (y'+y) (FocusA st en) arrs
mkFocus' p x' y' (FocusA st en) (ColA _ x y w h _ _ _ _ arrs) = mkFocusList' p 0 (x'+x) (y'+y) (FocusA st en) arrs
mkFocus' p x' y' (FocusA st en) (OverlayA _ x y w h _ _ _ _ (arr:arrs)) = mkFocus' (p++[0]) (x'+x) (y'+y) (FocusA st en) arr
mkFocus' p x' y' (FocusA st en) (GraphA _ x y w h _ _ _ _ arrs)     =  mkFocusList' p 0 (x'+x) (y'+y) (FocusA st en) arrs
mkFocus' p x' y' focus@(FocusA (PathA pth _) en) (VertexA _ x y w h _ _ _ outline arr) =
  if p == pth then mkOutlineCaret (x'+x) (y'+y) w h outline
  else mkFocus' (p++[0]) (x'+x) (y'+y) focus arr -- we assume that focus on vertex is always with startPath
mkFocus' p x' y' focus          (EdgeA _ x1 y1 x2 y2 _ _ _ _)        = mkEdgeCaret (x'+x1) (y'+y1) (x'+x2) (y'+y2)
mkFocus' p x' y' focus          (StructuralA l arr)      = mkFocus' (p++[0]) x' y' focus arr
mkFocus' p x' y' focus          (ParsingA l arr)         = mkFocus' (p++[0]) x' y' focus arr
mkFocus' p x' y' focus          (LocatorA l arr)         = mkFocus' (p++[0]) x' y' focus arr
mkFocus' p x' y' focus arr = debug Err ("Renderer.mkFocus': unimplemented arrangement: "++show focus++" "++show arr) []

mkFocusList' p i x' y' _ [] = []
mkFocusList' p i x' y' focus@(FocusA (PathA stp sti) (PathA enp eni))(a:as) = 
                           if take (length p+1) enp < p++[i]then []
                           else if length stp < length p+1 || take (length p+1) stp <= p++[i]  -- if start falls in or before a 
                           then mkFocus' (p++[i]) x' y' focus a ++ mkFocusList' p (i+1) x' y' focus as
                           else mkFocusList' p (i+1) x' y' focus as
mkFocusList' p i x' y' focus arr = debug Err ("Renderer.mkFocusList': unimplemented arrangement: "++show focus++" "++show arr) []


layoutFocusColor = CommonTypes.blue

-- ref lines not used, because caret is not incrementally rendered
-- because of line/box difference (line x y (x+w) y) is wider than (box x y w h) all to points are decreased
-- just decreasing w and h does not work
mkBoxCaret x y w h = 
  [ PolyA NoIDA x y w h 0 0 [(0,0),(0, h-1), (w-1, h-1),(w-1, 0), (0, 0)] 1 Transparent layoutFocusColor white transparent ]
mkEdgeCaret x1 y1 x2 y2 =
  [ EdgeA NoIDA x1 y1 x2 y2 0 0 2 layoutFocusColor ]
mkOutlineCaret x y w h outline = 
  [ PolyA NoIDA x y w h 0 0 (map outline [0, pi/10 ..2*pi]) 2 Transparent layoutFocusColor white transparent ]


arrangedFocusArea :: Show node => [Arrangement node] -> (Int,Int,Int,Int)
arrangedFocusArea fArrList = -- compute the region that is covered by the focus
  let (xs, ys, xs', ys') = unzip4 [(xA fLine, yA fLine, xA fLine + widthA fLine, yA fLine + heightA fLine) | fLine <- fArrList ]
  in  (if null xs then 0 else minimum xs, if null ys then 0 else minimum ys, maximum (0:xs'), maximum (0:ys'))

