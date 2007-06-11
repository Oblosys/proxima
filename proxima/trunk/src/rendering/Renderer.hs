module Renderer where

import CommonTypes
import qualified CommonTypes
import CommonUtils

import RenLayerTypes
import RenLayerUtils

import ArrLayerUtils (point, popupMenuItemsPres, pathAFromPathP')  -- for context menu hack
import PresTypes hiding (font) -- For Locations

import DocTypes -- For Locations
import DocUtils
import DocumentEdit -- Just for now
import GUI
import FontLib

import Graphics.UI.Gtk hiding (Scale, Solid)
import System.IO.Unsafe
import Data.IORef

-- for automatic popup menus, allow these imports
import DocTypes_Generated (Document, ClipDoc, Node (..))
import DocUtils_Generated ()  -- instance HasPath Node
import DocumentEdit (menuD)
import DocumentEdit_Generated -- instance Editable Document Document Node ClipDoc
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
  let (oldW,oldH) = (widthA oldArrangement, heightA oldArrangement)
      (newW,newH) = (widthA arrangement, heightA arrangement)
  in if oldW>newW || oldH > newH     -- if arr got smaller, repaint whole thing for now
     then (Rectangle 0 0 0 0, Rectangle 0 0 0 0, Rectangle 0 0 oldW oldH)
     else let (regx, regy, regx', regy') = maybe (-1,0,0,0) id $ updatedRectArr diffTree arrangement
          in  (Rectangle 0 0 0 0, Rectangle 0 0 0 0, rectBetween (regx,regy) (regx',regy') )
        
rectBetween (x,y) (x',y') = Rectangle (x `min` x') (y `min` y') (abs $ x-x') (abs $ y-y')
  
-- Automatic popups turned on: enable imports from editor

mkPopupMenuXY :: Presentation Document Node ClipDoc -> Scale -> Arrangement Node ->
                 ((RenderingLevel (DocumentLevel Document ClipDoc), EditRendering (DocumentLevel Document ClipDoc)) ->
                 IO (RenderingLevel (DocumentLevel Document ClipDoc), EditRendering' (DocumentLevel Document ClipDoc))) ->
                 IORef (RenderingLevel (DocumentLevel Document ClipDoc)) ->
                 Window -> DrawingArea -> Int -> Int -> IO (Maybe Menu)
mkPopupMenuXY prs scale arr@(LocatorA (RootDocNode doc _) _) handler renderingLvlVar window canvas x' y'  =
 do { let (x,y) = (descaleInt scale x',descaleInt scale y')
    ; let ctxtItems = case ArrLayerUtils.point x y arr of
                        Nothing -> []
                        Just pthA -> popupMenuItemsPres (pathAFromPathP' pthA prs) prs
              
   ; case pointDoc x y arr of
        Just node ->
         do { let pathDoc = pathNode node
                  alts = DocumentEdit.menuD pathDoc doc
                  items = ctxtItems ++ alts
            ; contextMenu <- mkMenu [ (str, popupMenuHandler handler renderingLvlVar window canvas upd)
                                    | (str, upd) <- items]
            ; return $ Just contextMenu                                          
            }            
        Nothing -> return Nothing  
    }
mkPopupMenuXY _ _ _ _ _ _ _ x y = 
 do { debugLnIO Err "Arrangement root is no document locator" 
    ; return Nothing
    }

{-
-- no automatic popups, document specific part separated from generic Proxima
mkPopupMenuXY :: Presentation Document Node ClipDoc -> Scale -> Arrangement Node ->
                 ((RenderingLevel (DocumentLevel Document ClipDoc), EditRendering (DocumentLevel Document ClipDoc)) ->
                 IO (RenderingLevel (DocumentLevel Document ClipDoc), EditRendering' (DocumentLevel Document ClipDoc))) ->
                 IORef (RenderingLevel (DocumentLevel Document ClipDoc)) ->
                 Window -> DrawingArea -> Int -> Int -> IO (Maybe Menu)
mkPopupMenuXY prs scale arr handler renderingLvlVar window canvas x' y'  =
 do { let (x,y) = (descaleInt scale x',descaleInt scale y')
    ; let ctxtItems = case ArrLayerUtils.point x y arr of
                        Nothing -> []
                        Just pthA -> popupMenuItemsPres (pathAFromPathP' pthA prs) prs
              
   ; case pointDoc x y arr of
        Just node ->
         do { let pathDoc = pathNode node
                  items = ctxtItems
            ; contextMenu <- mkMenu [ (str, popupMenuHandler handler renderingLvlVar window canvas upd)
                                    | (str, upd) <- items]
            ; return $ Just contextMenu                                          
            }            
        Nothing -> return Nothing  
    }
-}



-- debugged rendering also displays overlay for focus adding, but this has not been processed by debugArrangement
-- this makes it tricky to move the debuggedArrangement, since the Gest.Int. will not know about it
-- however, we don't want to debug the focus
--render' :: (LocalStateRen, MappingRen) -> Arrangement -> (Rendering, (LocalStateRen, MappingRen))
render' scale arrDb focus diffTree arrangement (wi,dw,gc) =
 do { -- seq (walk arrangement) $ return ()        -- maybe this is not necessary anymore, now the datastructure is strict
   -- ; debugLnIO Ren ("Arrangement is "++show arrangement)
    ; let focusArrList = arrangeFocus focus arrangement
    --; debugLnIO Err ("The updated rectangle is: "++show (updatedRectArr diffTree arrangement))
    
    ; gcSetValues gc $ newGCValues { foreground = Color 65535 65535 65535 }
    ; drawRectangle dw gc True 0 0 (2000-1) (1000-1)

    ; renderArr (wi,dw,gc) arrDb scale origin 
                         (DiffNode False False $ replicate (length focusArrList) (DiffLeaf False)++[diffTree])
                         
                         (OverlayA NoIDA 0 0 2000 1000 0 0 (255,255,255)
                              --(xA arrangement) (yA arrangement)  (widthA arrangement) (heightA arrangement)
-- using arrangement size goes wrong when arrangement shrinks
-- TODO fix viewsize/windowsize/arrangementSize, etc.
--                         ( arrangement : focusArrList)) -- ++ [arrangement]))
                              (focusArrList ++ [arrangement])) 
  
    }

computeRenderedArea (lux, luy) diffTree arr =
      if (isSelfClean diffTree)  -- if self is clean, only render its children (if present)
      then if (isClean diffTree)
           then []
           else let computeChildrenRenderedArea x' y' []   = [] 
                    computeChildrenRenderedArea x' y' arrs =
                      let (x,y)=(lux+x', luy+y')
                          childDiffTrees = case diffTree of
                                                DiffLeaf c     -> repeat $ DiffLeaf c
                                                DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
                          areas = zipWith (computeRenderedArea (x, y)) childDiffTrees arrs 
                          (luxs, luys, rlxs, rlys) = unzip4 . concat $ areas
                      in [(minimum luxs, minimum luys, maximum rlxs, maximum rlys)]
               in case arr of
                    RowA     _ x' y' _ _ _ _ _ arrs -> computeChildrenRenderedArea x' y' arrs
                    ColA     _ x' y' _ _ _ _ _ arrs -> computeChildrenRenderedArea x' y' arrs
                    OverlayA _ x' y' _ _ _ _ _ arrs -> computeChildrenRenderedArea x' y' arrs
                    GraphA   _ x' y' _ _ _ _ _ _ arrs -> computeChildrenRenderedArea x' y' arrs
                    VertexA  _ x' y' _ _ _ _ _ _ arr  -> computeChildrenRenderedArea x' y' [arr]
                    StructuralA _ arr               -> computeChildrenRenderedArea 0 0 [arr]
                    ParsingA _ arr                  -> computeChildrenRenderedArea 0 0 [arr]
                    LocatorA _ arr                  -> computeChildrenRenderedArea 0 0 [arr]
                    _ -> debug Err "Renderer.computeRenderedArea: dirty children for node without children" []
     else [(xA arr,yA arr, xA arr + widthA arr,  yA arr + widthA arr)]
    
renderArr :: (Window, DrawWindow, GC) -> Bool -> Scale -> (Int,Int) -> DiffTree -> Arrangement Node -> IO ()    
renderArr (wi,dw,gc) arrDb scale (lux, luy) diffTree arrangement =
 do { -- debugLnIO Err (shallowShowArr arrangement ++":"++ show (isClean diffTree));
     --if True then return () else    -- uncomment this line to skip rendering


     if (isSelfClean diffTree)  -- if self is clean, only render its children (if present)
     then if (isClean diffTree)
          then return ()
          else let renderChildren x' y' arrs =
                    do { let (x,y)=(lux+scaleInt scale x', luy+scaleInt scale y')
                       ; let childDiffTrees = case diffTree of
                                                DiffLeaf c     -> repeat $ DiffLeaf c
                                                DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
                       ; sequence_ $ zipWith (renderArr (wi,dw,gc) arrDb scale (x, y)) childDiffTrees arrs 
                       }
               in case arrangement of
                    RowA     _ x' y' _ _ _ _ _ arrs -> renderChildren x' y' arrs
                    ColA     _ x' y' _ _ _ _ _ arrs -> renderChildren x' y' arrs
                    OverlayA _ x' y' _ _ _ _ _ arrs -> renderChildren x' y' arrs
                    GraphA   _ x' y' _ _ _ _ _ _ arrs -> renderChildren x' y' arrs
                    VertexA  _ x' y' _ _ _ _ _ _ arr  -> renderChildren x' y' [arr]
                    StructuralA _ arr           -> renderChildren 0 0 [arr]
                    ParsingA _ arr              -> renderChildren 0 0 [arr]
                    LocatorA _ arr              -> renderChildren 0 0 [arr]
                    _ -> return ()
     else
  case arrangement of 

    (EmptyA _ _ _ _ _ _ _) ->
      return ()
      
    (StringA id x' y' w' h' hRef' _ str fColor fnt _) ->
     do { let (x,y,w,h, hRef)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h', scaleInt scale hRef')
        ; when (str /= "") $ 
           do { context <- widgetCreatePangoContext wi
              ; fontDescription <- fontDescriptionFromProximaFont fnt
           
              ; gcSetValues gc $ newGCValues { foreground = colorRGB fColor }
                        
                        
              ; pangoItems <- pangoItemize context str 
                                [ AttrFontDescription 0 (length str) fontDescription
                                , AttrUnderline 0 (length str) (if fUnderline fnt then UnderlineSingle else UnderlineNone)
                                , AttrStrikethrough 0 (length str) (fStrikeOut fnt)
                                ] -- underline and strikeout don't seem to work
              ; case pangoItems of 
                  [pangoItem] -> do { glyphItem <- pangoShape (head pangoItems)
                                    ; drawGlyphs dw gc x (y+hRef) glyphItem
                                    }
                  pangoItem:_ -> do { glyphItem <- pangoShape (head pangoItems)
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

        ; when arrDb $
            drawFilledRectangle dw gc (Rectangle x y w h) imageColor imageColor
 
        ; ePB <- pixbufNewFromFile src 
        ; case ePB of
            Left (_,errStr) -> debugLnIO Err $ "Renderer.renderArr: could not open bitmap "++show src ++
                                               "\n" ++ errStr
            Right pb ->
             do {        
                -- TODO: make this the Tile case and make a Resize case, and add clipping
                -- and store pixbufs in rendering state, to avoid reloading.
                
                -- ; pb <- pixbufScaleSimple pb w h InterpBilinear       -- scale the pixmap
                
                ; iw <- pixbufGetWidth pb
                ; ih <- pixbufGetHeight pb

                ; when (iw /= 0 && ih /= 0) $
             do { let (hrepeats, hrest) = w `divMod` iw 
                ; let (vrepeats, vrest) = h `divMod` ih 
                ; gcSetValues gc $ newGCValues { foreground = colorRGB lColor }
              
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

    (RectangleA id x' y' w' h' _ _ lw' style lColor fColor) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; when (style == Solid) $
            do { gcSetValues gc $ newGCValues { foreground = colorRGB fColor }
               ; drawRectangle dw gc True x y w h
               }
        ; gcSetValues gc $ newGCValues { foreground = colorRGB lColor, lineWidth = scaleInt scale lw' `max` 1 }
        
        ; when (w>0 && h>0) $ drawRectangle dw gc False x y (w-1) (h-1)
        -- outlined gtk rectangles are 1 pixel larger than filled ones
        }

    (EllipseA id x' y' w' h' _ _ lw' style lColor fColor) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')       
        ; when (style == Solid) $
            do { gcSetValues gc $ newGCValues { foreground = colorRGB fColor }
               ; drawArc dw gc True x y (w+1) (h+1) (0*64) (360*64)
               -- filled gtk arcs are 1 pixel smaller than outlined ones
               }
        ; gcSetValues gc $ newGCValues { foreground = colorRGB lColor, lineWidth = scaleInt scale lw' `max` 1 }
        ; drawArc dw gc False x y w h (0*64) (360*64)
        }


    (LineA id lux' luy' rlx' rly' _ _ lw' lColor) ->
     do { let (fromx, fromy, tox, toy)=(lux+scaleInt scale lux', luy+scaleInt scale luy', lux+scaleInt scale rlx', luy+scaleInt scale rly')
        ; let angleFromEnd = atan (fromIntegral (tox-fromx) / fromIntegral (toy-fromy)) -- atan works okay for pos and neg infinity
                             + if fromy > toy then pi  else 0
              
              pt1 = (tox - round (arrowHeadSize * sin (angleFromEnd + arrowHeadHalfAngle)), toy - round (arrowHeadSize * cos (angleFromEnd + arrowHeadHalfAngle))) 
              pt2 = (tox - round (arrowHeadSize * sin (angleFromEnd - arrowHeadHalfAngle)), toy - round (arrowHeadSize * cos (angleFromEnd - arrowHeadHalfAngle))) 
        
        ; gcSetValues gc $ newGCValues { foreground = colorRGB lColor, lineWidth = scaleInt scale lw' `max` 1 }
        ; drawLine dw gc (fromx,fromy) (tox,toy) 
        -- draw arrow head
        ; drawPolygon dw gc True [pt1, pt2, (tox, toy)] 
        }

-- Poly seems to be rendered incorrectly (bottom/right line is blank)
    (PolyA id x' y' w' h' _ _ pts' lw' lColor bColor) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let pts = map (\(x',y') -> (x+scaleInt scale x', y+scaleInt scale y')) pts'
        ; gcSetValues gc $ newGCValues { foreground = colorRGB lColor, lineWidth = scaleInt scale lw' `max` 1 }
        ; drawLines dw gc pts
  

        ; when (not arrDb) $
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = colorRGB bColor
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }
              } 
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
                                                 else showDebug Ren $ tail.init $
                                                      scanl (\n1 n2 -> n1 + (scaleInt scale (pd-1))+n2 ) 0 
                                                            (map (scaleInt scale . widthA) arrs)]
              ; sequence_ $ zipWith (renderArr (wi,dw,gc) arrDb scale (x, y)) childDiffTrees arrs
             }
          else 
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = colorRGB bColor -- if isClean diffTree then colorRGB bColor else red
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }
              ; sequence_ $ zipWith (renderArr (wi,dw,gc) arrDb scale (x, y)) childDiffTrees arrs
              }
        }

    (ColA id x' y' w' h' _ _ bColor arrs) ->
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
                                                else showDebug Ren $ tail.init $
                                                     scanl (\n1 n2 -> n1+(scaleInt scale (pd-1))+n2 ) 0 
                                                           (map (scaleInt scale . heightA) arrs)]
        
              ; sequence_ $ zipWith (renderArr (wi,dw,gc) arrDb scale (x, y)) childDiffTrees arrs
              }
          else 
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = colorRGB bColor --  if isClean diffTree then colorRGB bColor else red
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }
              ; sequence_ $ zipWith (renderArr (wi,dw,gc) arrDb scale (x, y)) childDiffTrees arrs
              }
        }

    (OverlayA id x' y' w' h' _ _ bColor arrs) ->
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
              ; let (arrs',cdts') = if null arrs then (arrs,childDiffTrees)
                                                 else case (last arrs) of EmptyA _ _ _ _ _ _ _ -> (arrs, childDiffTrees)
                                                                          _                -> unzip . reverse $ zip arrs childDiffTrees
              
              ; sequence_ $ zipWith (renderArr (wi,dw,gc) arrDb scale (x, y))  cdts' arrs'
              }
          else 
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = colorRGB bColor -- if isClean diffTree then colorRGB bColor else red
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }
               -- nasty workaround hack for overlay problem: if last elt of overlay is EmptyA,
               -- the children are reversed. Squigglies are the only presentations for now that
               -- need to be put in front of the overlay, but that should not get parsed.
              ; let (arrs',cdts') = if null arrs then (arrs,childDiffTrees)
                                                 else case (last arrs) of EmptyA _ _ _ _ _ _ _ -> (arrs, childDiffTrees)
                                                                          _                -> unzip . reverse $ zip arrs childDiffTrees
              -- until ovl is properly reversed
              ; sequence_ $ zipWith (renderArr (wi,dw,gc) arrDb scale (x, y)) cdts' arrs'
              }
        }

    (GraphA id x' y' w' h' _ _ bColor _ arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; gcSetClipRectangle gc (Rectangle x y w h)
        ; if arrDb then
            drawFilledRectangle dw gc (Rectangle x y w h) graphColor graphColor
          else 
           do { when (not (isTransparent bColor)) $
                 do { let bgColor = colorRGB bColor -- if isClean diffTree then colorRGB bColor else red
                    ; drawFilledRectangle dw gc (Rectangle x y w h) bgColor bgColor
                    }        
              }
        ; sequence_ $ reverse $ zipWith (renderArr (wi,dw,gc) arrDb scale (x, y)) childDiffTrees arrs -- reverse so first is drawn in front
        }

    (VertexA id x' y' w' h' _ _ bColor _ arr) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; when arrDb $
            drawFilledRectangle dw gc (Rectangle x y w h) vertexColor vertexColor

        ; renderArr (wi,dw,gc) arrDb scale (x, y) (head childDiffTrees) arr
        }

    (StructuralA id arr) -> 
     do { let (x,y,w,h)=( lux+scaleInt scale (xA arr), luy+scaleInt scale (yA arr) 
                        , scaleInt scale (widthA arr), scaleInt scale (heightA arr) )
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; when arrDb $
            drawFilledRectangle dw gc (Rectangle x y w h) structuralBGColor structuralBGColor
       
        ; renderArr (wi,dw,gc) arrDb scale (lux, luy) (head childDiffTrees) arr
        }
    
    (ParsingA id arr) ->
     do { let (x,y,w,h)=( lux+scaleInt scale (xA arr), luy+scaleInt scale (yA arr) 
                        , scaleInt scale (widthA arr), scaleInt scale (heightA arr) )
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; when arrDb $
            drawFilledRectangle dw gc (Rectangle x y w h) parsingBGColor parsingBGColor
       
        ; renderArr (wi,dw,gc) arrDb scale (lux, luy) (head childDiffTrees) arr
        }

    (LocatorA _ arr) ->
     do {
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; renderArr (wi,dw,gc) arrDb scale (lux, luy) (head childDiffTrees) arr
        }

    _ ->  return () --dcDrawText dc ("unimplemented arrangement: "++shallowShowArr arrangement) (pt lux luy)
        

  ; when arrDb $
       do { renderID (wi,dw,gc) scale (lux+xA arrangement) (luy+yA arrangement) (idA arrangement)
          }

  }

drawFilledRectangle :: DrawWindow -> GC -> Rectangle -> Graphics.UI.Gtk.Color -> Graphics.UI.Gtk.Color -> IO ()
drawFilledRectangle dw gc (Rectangle x y w h) lineColor fillColor =
 do { gcSetValues gc $ newGCValues { foreground = fillColor }
    ; drawRectangle dw gc True x y w h 
    ; gcSetValues gc $ newGCValues { foreground = lineColor }
    ; when (w>0 && h>0) $ drawRectangle dw gc False x y (w-1) (h-1)
    -- outlined gtk rectangles are 1 pixel larger than filled ones
    } 
                          

-- usage: for basic Arrangements, render after image so id is visible
--        for composites,         render before, so stacking order of arrangement is represented correctly

--renderID _          scale x y NoIDA = return ()   -- use this case to disable NoIDA renderings
renderID (wi,dw,gc) scale x y id  = 
 do { let idStr = case id of NoIDA -> " "
                             IDA i -> show i

    ; context <- widgetCreatePangoContext wi
    ; fontDescription <- fontDescriptionNew
    ; fontDescriptionSetFamily fontDescription "Courier New"
    ; fontDescriptionSetSize fontDescription 6
    
    ; gcSetValues gc $ newGCValues { foreground = colorRGB black }
    ; pangoItems <- pangoItemize context idStr [ AttrFontDescription 0 (length idStr) fontDescription ] 
    ; case pangoItems of 
        [pangoItem] -> do { glyphItem <- pangoShape (head pangoItems)
                          ; (_, PangoRectangle x' y' w' h') <-glyphItemExtents glyphItem
                          ; drawFilledRectangle dw gc (Rectangle (x+round x') y (round w'+2) (round h'+1)) (colorRGB black) (colorRGB yellow)
                          ; drawGlyphs dw gc (x+1) (y+1 - round y') glyphItem
                          }
        pangoItem:_ -> debug Err ("Renderer.renderID: incorrect nr of pango items") $ return ()
    }


-- debug colors
stringColor       = colorRGB (0, 255, 255)
imageColor        = colorRGB (92, 64, 0)
rowColor          = colorRGB (255, 0, 0)
colColor          = colorRGB (0, 0, 200) 
overlayColor      = colorRGB (0, 0, 200) 
graphColor        = colorRGB (255, 255, 0)
vertexColor       = colorRGB (255, 0, 255)
structuralBGColor = colorRGB (230, 230, 255)
parsingBGColor    = colorRGB (255, 230, 230)

colorRGB (r, g, b) = Color (256*fromIntegral r) (256*fromIntegral g) (256*fromIntegral b)

-- focus is not perfect, but this has to do with the selection algorithm as well.
-- for now, it is a working solution

arrangeFocus :: Show node => FocusArr -> Arrangement node -> [Arrangement node]
arrangeFocus (FocusA from to) arrangement = {- scaleFocusArrList scale $-} {-debug Ren ("focus:" ++show (from,to)) $-} mkFocus (FocusA from to) arrangement
                                      --[ picobToCaretC from arrangement green
                                      --, picobToCaretC to arrangement blue 
                                      -- ]
arrangeFocus (NoFocusA) _ = []


-- caret at end rendered 1 pixel to the left because of poor man's incrementality algorithm

-- ref lines not used, because caret is not incrementally rendered
mkFocus :: Show node => FocusArr -> Arrangement node -> [Arrangement node]
mkFocus focus arr = mkFocus' [] 0 0 (orderFocusA focus) arr



-- precondition, node is only visited if it part of it is focused
mkFocus' p x' y' focus          (EmptyA _ _ _ _ _ _ _) = []
mkFocus' p x' y' (FocusA (PathA stp sti) (PathA enp eni)) (StringA _  x y w h _ _ _ _ _ cxs') = 
  let cxs = init cxs' ++ [last cxs'-1]
      st = if length stp < length p || stp < p then 0 else cxs!!!sti
      en = if  enp > p then last cxs else cxs!!!eni                        
  in  mkBoxCaret (x'+x+st) (y'+y) (en-st + 1) h
mkFocus' p x' y' focus          (ImageA _ x y w h _ _ _ _ _ _)       = mkBoxCaret (x'+x) (y'+y) w h
mkFocus' p x' y' focus          (PolyA _ x y w h _ _ _ _ _ _)        = mkBoxCaret (x'+x) (y'+y) w h
mkFocus' p x' y' focus          (RectangleA _ x y w h _ _ _ _ _ _)   = mkBoxCaret (x'+x) (y'+y) w h
mkFocus' p x' y' focus          (EllipseA _ x y w h _ _ _ _ _ _)     = mkBoxCaret (x'+x) (y'+y) w h
mkFocus' p x' y' focus          (LineA _ x1 y1 x2 y2 _ _ _ _)        = mkLineCaret (x'+x1) (y'+y1) (x'+x2) (y'+y2)
mkFocus' p x' y' (FocusA st en) (RowA _ x y w h _ _ _ arrs) = mkFocusList' p 0 (x'+x) (y'+y) (FocusA st en) arrs
mkFocus' p x' y' (FocusA st en) (ColA _ x y w h _ _ _ arrs) = mkFocusList' p 0 (x'+x) (y'+y) (FocusA st en) arrs
mkFocus' p x' y' (FocusA st en) (OverlayA _ x y w h _ _ _ (arr:arrs)) = mkFocus' (p++[0]) (x'+x) (y'+y) (FocusA st en) arr
mkFocus' p x' y' (FocusA st en) (GraphA _ x y w h _ _ _ _ arrs)     =  mkFocusList' p 0 (x'+x) (y'+y) (FocusA st en) arrs
mkFocus' p x' y' (FocusA st en) (VertexA _ x y w h _ _ _ outline _) = mkOutlineCaret (x'+x) (y'+y) w h outline
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
  [ PolyA NoIDA x y w h 0 0 [(0,0),(0, h-1), (w-1, h-1),(w-1, 0), (0, 0)] 1 layoutFocusColor transparent ]
mkLineCaret x1 y1 x2 y2 =
  [ LineA NoIDA x1 y1 x2 y2 0 0 2 layoutFocusColor ]
mkOutlineCaret x y w h outline = 
  [ PolyA NoIDA x y w h 0 0 (map outline [0, pi/10 ..2*pi]) 2 layoutFocusColor transparent ]


arrangedFocusArea :: Show node => [Arrangement node] -> (Int,Int,Int,Int)
arrangedFocusArea fArrList = -- compute the region that is covered by the focus
  let (xs, ys, xs', ys') = unzip4 [(xA fLine, yA fLine, xA fLine + widthA fLine, yA fLine + heightA fLine) | fLine <- fArrList ]
  in  (if null xs then 0 else minimum xs, if null ys then 0 else minimum ys, maximum (0:xs'), maximum (0:ys'))

