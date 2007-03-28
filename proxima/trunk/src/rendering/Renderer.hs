module Renderer where

import CommonTypes hiding (black, blue, green, cyan, red, magenta, yellow, white, grey) 
import qualified CommonTypes
import RenLayerTypes
import RenLayerUtils

import ArrLayerUtils hiding (rect,text) -- for context menu hack
import PresTypes hiding (font) -- For Locations

import DocTypes -- For Locations
import DocUtils
import DocumentEdit -- Just for now
import GUI

import Graphics.UI.WX hiding (Color) 
import Graphics.UI.WXCore hiding (Color, Document) 
import System.IO.Unsafe


-- for automatic popup menus, allow these imports
-- comment out the _Generated imports when doing "make depend" (very nasty)
import DocTypes_Generated (Document, ClipDoc, Node (..))
import DocUtils_Generated ()  -- instance HasPath Node
import DocumentEdit (menuD)
import DocumentEdit_Generated -- instance Editable Document Document Node ClipDoc
-----

{-
TODO:

- images: fix load exception and garbage collect. + use clipping 
- line sizes
- scaling
- set view size and title

-- why does mouse click lead to entire redraw??


-- caret at end rendered 1 pixel to the left because of poor man's incrementality algorithm

-}
computeUpdatedRegions oldUpdRegions scale focus diffTree oldArrangement arrangement =
  let (oldW,oldH) = (widthA oldArrangement, heightA oldArrangement)
      (newW,newH) = (widthA arrangement, heightA arrangement)
  in if oldW>newW || oldH > newH     -- if arr got smaller, repaint whole thing for now
     then (rect (pt 0 0) (sz 0 0), rect (pt 0 0) (sz 0 0), rect (pt 0 0) (sz oldW oldH))
     else let (regx, regy, regx', regy') = maybe (-1,0,0,0) id $ updatedRectArr diffTree arrangement
          in  (rect (pt 0 0) (sz 0 0), rect (pt 0 0) (sz 0 0), rectBetween (Point regx regy) (Point (regx') (regy')) )
        

  
  
  



myArr' = arr00
myArr = ColA NoIDA 0 0 0 0 0 0 CommonTypes.blue
         [ RectangleA NoIDA 0 0 10 10 0 0 1 Solid (0,0,0) (255,0,0)
         ,strr 40 80 "bla"
        
         ]
strr x y str = StringA NoIDA x y 0 0 0 0 str (0,0,0) defaultFont [0,28,40,68,96,124]--,

arr00 = ColA NoIDA 10 10 0 0 0 0 CommonTypes.blue -- offsets are not correct here
        [ RectangleA NoIDA 0 0 80 50 0 0 1 Solid (0,0,0) (255,0,0)
        , StringA NoIDA 80 0 0 0 0 0 "Hello World!" (0,0,0) defaultFont []
        , StringA NoIDA 0 40 0 0  0 0 "Bla" (0,0,0) defaultFont []
        , StringA NoIDA 80 0 0 0 0 0 "B" (0,0,0) defaultFont []
        ]

{-
arr1 =  StringA NoIDA 0 0 124 50 "blaaa" (0,0,0) defaultFont [0,28,40,68,96,124]--,
                                    --       StringA NoIDA 0 50 24 50 "x" (0,0,0) "Arial" 30 [0,24

-}

-- end of hack.

-- Automatic popups turned on: enable imports from editor

mkPopupMenuXY :: Presentation Document Node ClipDoc -> Scale -> Arrangement Node ->
                 ((RenderingLevel (DocumentLevel Document ClipDoc), EditRendering (DocumentLevel Document ClipDoc)) ->
                 IO (RenderingLevel (DocumentLevel Document ClipDoc), EditRendering' (DocumentLevel Document ClipDoc))) ->
                 Var (RenderingLevel (DocumentLevel Document ClipDoc)) ->
                 ScrolledWindow a -> Int -> Int -> IO ()
mkPopupMenuXY prs scale arr@(LocatorA (RootDocNode doc _) _) handler renderingLvlVar window x' y'  =
 do { let (x,y) = (descaleInt scale x',descaleInt scale y')
    ; let ctxtItems = case pointOvlRev' x y [[]] arr of
                        (pthA:_) -> popupMenuItemsPres (addWithSteps pthA prs) prs
                        []       -> []
              
   ; case map pathNode (pointDoc' x y arr) of
        (pth:_) ->
         do { let alts = DocumentEdit.menuD pth doc
            ; let items = ctxtItems ++ alts
            --; putStrLn $ "popup"++show (map fst items)
            ; popupMenu <- menuPane []
            ; sequence_ (map (mkMenuItem popupMenu) items)
            
            ; (Point scrX scrY) <- scrolledWindowCalcScrolledPosition window (pt x y)

            ; menuPopup popupMenu (pt scrX scrY) window
            }
        _ -> return ()  
    }                         -- This stuff belongs in GUI, renderer should only export names + edit ops
  where mkMenuItem popupMenu (str, upd) =
         do { item <- menuItem popupMenu [ text := str ]
            ; set window [on (menu item) := popupMenuHandler handler renderingLvlVar window upd ]
            ; return item
            }

{-
-- no automatic popups, document specific part separated from generic Proxima
mkPopupMenuXY :: (HasPath node, Show node) => Presentation doc node clip -> Scale -> Arrangement node ->
                 ((RenderingLevel (DocumentLevel doc clip), EditRendering (DocumentLevel doc clip)) ->
                 IO (RenderingLevel (DocumentLevel doc clip), EditRendering' (DocumentLevel doc clip))) ->
                 Var (RenderingLevel (DocumentLevel doc clip)) ->
                 ScrolledWindow a -> Int -> Int -> IO ()
mkPopupMenuXY prs scale arr handler renderingLvlVar window x' y'  =
 do { let (x,y) = (descaleInt scale x',descaleInt scale y')
    ; let ctxtItems = case pointOvlRev' x y [[]] arr of
                        (pthA:_) -> popupMenuItemsPres (addWithSteps pthA prs) prs
                        []       -> []
              
   ; case map pathNode (pointDoc' x y arr) of
        (pth:_) ->
         do { let items = ctxtItems
            --; putStrLn $ "popup"++show (map fst items)
            ; popupMenu <- menuPane []
            ; sequence_ (map (mkMenuItem popupMenu) items)
            
            ; (Point scrX scrY) <- scrolledWindowCalcScrolledPosition window (pt x y)

            ; menuPopup popupMenu (pt scrX scrY) window
            }
        _ -> return ()  
    }                         -- This stuff belongs in GUI, renderer should only export names + edit ops
  where mkMenuItem popupMenu (str, upd) =
         do { item <- menuItem popupMenu [ text := str ]
            ; set window [on (menu item) := popupMenuHandler handler renderingLvlVar window upd ]
            ; return item
            }

mkPopupMenuXY _ _ _ _ _ _ x y = 
 do { debugLnIO Err "Arrangement root is no document locator"
    }
-}

{-

mouseDownDoc state arrLvl layout (PathA pthA _) i = -- only look at start of focus. focus will be empty


-}

                      

-- debugged rendering also displays overlay for focus adding, but this has not been processed by debugArrangement
-- this makes it tricky to move the debuggedArrangement, since the Gest.Int. will not know about it
-- however, we don't want to debug the focus
--render' :: (LocalStateRen, MappingRen) -> Arrangement -> (Rendering, (LocalStateRen, MappingRen))
render' scale arrDb focus diffTree arrangement dc =
 do {  -- seq (walk arrangement) $ return ()        -- maybe this is not necessary anymore, now the datastructure is strict
--    ; debugLnIO Ren ("Arrangement is "++show arrangement)
    ; let focusArrList = arrangeFocus focus arrangement
    --; debugLnIO Err ("The updated rectangle is: "++show (
    ; debugLnIO Err ("The updated rectangle is: "++show (updatedRectArr diffTree arrangement))
    
    ; drawRect dc (Rect 0 0 2000 1000) [color := white, brush := BrushStyle BrushSolid white]


    ; renderArr dc arrDb scale origin 
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
                    StructuralA _ arr               -> computeChildrenRenderedArea 0 0 [arr]
                    ParsingA _ arr                  -> computeChildrenRenderedArea 0 0 [arr]
                    LocatorA _ arr                  -> computeChildrenRenderedArea 0 0 [arr]
                    _ -> debug Err "Renderer.computeRenderedArea: dirty children for node without children" []
     else [(xA arr,yA arr, xA arr + widthA arr,  yA arr + widthA arr)]
        
{-renderArr dc _ _ _ _ _ = 
  drawText dc "Rendering display is turned off" (pt 20 20) [ color := red ]
-}

renderArr dc arrDb scale (lux, luy) diffTree arr =
 do { -- debugLnIO Err (shallowShowArr arr ++":"++ show (isClean diffTree));
     --if True then return () else    -- uncomment this line to skip rendering

     if (isSelfClean diffTree)  -- if self is clean, only render its children (if present)
     then if (isClean diffTree)
          then return ()
          else let renderChildren x' y' arrs =
                    do { let (x,y)=(lux+scaleInt scale x', luy+scaleInt scale y')
                       ; let childDiffTrees = case diffTree of
                                                DiffLeaf c     -> repeat $ DiffLeaf c
                                                DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
                       ; sequence_ $ zipWith (renderArr dc arrDb scale (x, y)) childDiffTrees arrs 
                       }
               in case arr of
                    RowA     _ x' y' _ _ _ _ _ arrs -> renderChildren x' y' arrs
                    ColA     _ x' y' _ _ _ _ _ arrs -> renderChildren x' y' arrs
                    OverlayA _ x' y' _ _ _ _ _ arrs -> renderChildren x' y' arrs
                    StructuralA _ arr           -> renderChildren 0 0 [arr]
                    ParsingA _ arr              -> renderChildren 0 0 [arr]
                    LocatorA _ arr              -> renderChildren 0 0 [arr]
                    _ -> return ()
     else
  case arr of 

    (EmptyA _ _ _ _ _ _ _) ->
      return ()
      
    (StringA id x' y' w' h' _ _ str (fr, fg, fb) fnt _) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; drawText dc (str) (pt x y) [ font := fontDefault { _fontFace = fFamily fnt
                                                           , _fontSize = fSize fnt
                                                           , _fontWeight = if fBold fnt then WeightBold else WeightNormal
                                                           , _fontShape  = if fItalic fnt then ShapeItalic else ShapeNormal
                                                           , _fontUnderline = fUnderline fnt }
                                     , color := colorRGB fr fg fb ]
        

          
        ; when arrDb $
           do { drawRect dc (Rect x y w h) [ color := stringColor, brush:= BrushStyle BrushTransparent black ]
              }
        }

    (ImageA id x' y' w' h' _ _ src style (lr,lg,lb) (br,bg,bb)) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')

--        ; setPenSize 1
        
        ; (x,y,w,h) <- if arrDb
                       then
                        do { ----setPenColour imageColor
                         ----  ; drawAt (Point2 x y) (Box w h)
           do { drawRect dc (Rect x y w h) [ color := imageColor, brush:= BrushStyle BrushTransparent black ]
              ; renderID dc scale x y id
              }
                           ; return (x+(scaleInt scale hpd), y+(scaleInt scale hpd), w-(scaleInt scale pd), h-(scaleInt scale pd))
                           }
                       else 
                             return (x,y,w,h)
        
        ; bm <- bitmapCreateFromFile src  -- can fail with exception
                     
        
        
        -- This is from ObjectIO, WX can clip, but we don't do it yet:
        -- can't clip! For squiggly rendering, we center and tile, if image is too large it is not shown
        -- TODO: make this the Tile case and make a Resize case 
       
        --  debugLnIO Err $ "Renderer.renderArr: could not open bitmap "++show src
        
        
        ; iw <- bitmapGetWidth bm
        ; ih <- bitmapGetHeight bm
        ; id' <- bitmapGetDepth bm
        
       -- ; let (iw, ih) = (scaleInt scale iw', scaleInt scale (ih'+id'))
       -- ; let bitmap = resizeBitmap (Size iw ih) bitmap' 
        ; let (hrepeats, hrest) = w `divMod` iw 
        ; let (vrepeats, vrest) = h `divMod` ih 
        ; let cx = hrest `div` 2
        ; let cy = vrest `div` 2
        ; sequence_ [ drawBitmap dc bm (pt (x+i*iw+cx) (y+j*ih+cy)) True [brush := BrushStyle BrushSolid (colorRGB lr lg lb)] 
                    | i<-[0..hrepeats-1], j<-[0..vrepeats-1] ]
    
        
       -- use this one to do tiling and clipping automatically. move 1 left and 1 up and use transparent pen
       -- ; drawRect dc (Rect 10 10 50 50) [ color := green
       --                                  , brush := BrushStyle (BrushStipple bm) red]
       --                                                        color doesn't seem to work
        
       
        }

    (RectangleA id x' y' w' h' _ _ lw' style (lr,lg,lb) (fr,fg,fb)) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
       
----        ; setPenSize $ let ls = scaleInt scale lw' in if ls < 1 then 1 else ls
-- also do pensize for other line drawings
        ; drawRect dc (Rect x y w h) [ color := (colorRGB lr lg lb)
                                     , brush := BrushStyle (if style == Solid then BrushSolid else BrushTransparent)
                                                                (colorRGB fr fg fb)]                     

        }


    (LineA id lux' luy' rlx' rly' _ _ lw' (lr,lg,lb)) ->
     do { let (lnlux,lnluy, lnrlx, lnrly)=(lux+scaleInt scale lux', luy+scaleInt scale luy', scaleInt scale rlx', scaleInt scale rly')
        ; line dc (pt lnlux lnluy) (pt lnrlx lnrly) [color := colorRGB lr lg lb]
        }
-- Poly seems to be rendered incorrectly (bottom/right line is blank)
    (PolyA id x' y' w' h' _ _ pts' lw' (lr,lg,lb) (br,bg,bb)) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let pts = map (\(x',y') -> pt (x+scaleInt scale x') (y+scaleInt scale y')) pts'
        ; polyline dc pts [ color := colorRGB lr lg lb ]
    

     
--        ; when (not arrDb) $
--            drawRect dc (Rect x y w h) [ color := (colorRGB br bg bb)
--                                       , brush:= BrushStyle BrushSolid (colorRGB br bg bb) ]     
-- for now, no background fill, because poly in overlay will cover
-- first child. We want a Transparent color for this
 
        }


    (RowA id x' y' w' h' _ _ (br,bg,bb) arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False) -- in case there are too few dts

        ; if arrDb
          then
           do { let childCnt = length arrs 
    --          ; drawFilledRect dc (Rect x y w h) (colorRGB br bg bb)  
              ; drawRect dc (Rect x y w h) [ color := rowColor, brush:= BrushStyle BrushTransparent black ]
         
              ; sequence_ $ [ line dc (pt (x+i) y) (pt (x+i) (y+h)) [ color := rowColor ]
                            | i <- if null arrs then []
                                                else showDebug Ren $ tail.init $
                                                     scanl (\n1 n2 -> n1 + (scaleInt scale (pd-1))+n2 ) 0 
                                                           (map (scaleInt scale . widthA) arrs)]
              ; sequence_ $ zipWith (renderArr dc arrDb scale (x, y)) childDiffTrees arrs
              }
          else 
           do { let bgColor = colorRGB br bg bb -- if isClean diffTree then colorRGB br bg bb else red
              ; drawFilledRect dc (Rect x y w h) bgColor
              ; sequence_ $ zipWith (renderArr dc arrDb scale (x, y)) childDiffTrees arrs
              }
        }

    (ColA id x' y' w' h' _ _ (br,bg,bb) arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)

        ; if arrDb
          then
           do { let childCnt = length arrs 
    --          ; drawFilledRect dc (Rect x y w h) (colorRGB br bg bb)  
              ; drawRect dc (Rect x y w h) [ color := colColor, brush:= BrushStyle BrushTransparent black ]
         
              ; sequence_ $ [ line dc (pt x (y+i)) (pt (x+w) (y+i)) [ color := colColor ]
                           | i <- if null arrs then []
                                               else showDebug Ren $ tail.init $
                                                    scanl (\n1 n2 -> n1+(scaleInt scale (pd-1))+n2 ) 0 
                                                          (map (scaleInt scale . heightA) arrs)]
              
 


              ; sequence_ $ zipWith (renderArr dc arrDb scale (x, y)) childDiffTrees arrs
              }
          else 
           do { let bgColor = colorRGB br bg bb --  if isClean diffTree then colorRGB br bg bb else red
              ; drawFilledRect dc (Rect x y w h) bgColor
              ; sequence_ $ zipWith (renderArr dc arrDb scale (x, y)) childDiffTrees arrs
              }
        }

    (OverlayA id x' y' w' h' _ _ (br,bg,bb) arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)

        ; if arrDb
          then
           do { let childCnt = length arrs 
    --          ; drawFilledRect dc (Rect x y w h) (colorRGB br bg bb)  
              ; drawRect dc (Rect x y w h) [ color := overlayColor, brush:= BrushStyle BrushTransparent black ]
         
              
              ; let (arrs',cdts') = if null arrs then (arrs,childDiffTrees)
                                                 else case (last arrs) of EmptyA _ _ _ _ _ _ _ -> (arrs, childDiffTrees)
                                                                          _                -> unzip . reverse $ zip arrs childDiffTrees
              
              ; sequence_ $ zipWith (renderArr dc arrDb scale (x, y))  cdts' arrs'
              }
          else 
           do { -- drawFilledRect dc (Rect x y w h) (colorRGB br bg bb)  
               -- nasty workaround hack for overlay problem: if last elt of overlay is EmptyA,
               -- the children are reversed. Squigglies are the only presentations for now that
               -- need to be put in front of the overlay, but that should not get parsed.
              ; let (arrs',cdts') = if null arrs then (arrs,childDiffTrees)
                                                 else case (last arrs) of EmptyA _ _ _ _ _ _ _ -> (arrs, childDiffTrees)
                                                                          _                -> unzip . reverse $ zip arrs childDiffTrees
              -- until ovl is properly reversed
              ; sequence_ $ zipWith (renderArr dc arrDb scale (x, y)) cdts' arrs'
              ; return ()
              }
        }

--- weird stuff, 255 255 255 is transparent? + line colors of images are no right. is this a bug in ObjectIO?

    (StructuralA id arr) -> 
     do { let (x,y,w,h)=( lux+scaleInt scale (xA arr), luy+scaleInt scale (yA arr) 
                        , scaleInt scale (widthA arr), scaleInt scale (heightA arr) )
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; when arrDb $
            drawFilledRect dc (Rect x y w h) structuralBGColor
       
        ; renderArr dc arrDb scale (lux, luy) (head childDiffTrees) arr
        }
    
    (ParsingA id arr) ->
     do { let (x,y,w,h)=( lux+scaleInt scale (xA arr), luy+scaleInt scale (yA arr) 
                        , scaleInt scale (widthA arr), scaleInt scale (heightA arr) )
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; when arrDb $
            drawFilledRect dc (Rect x y w h) parsingBGColor
       
        ; renderArr dc arrDb scale (lux, luy) (head childDiffTrees) arr
        }

    (LocatorA _ arr) ->
     do {
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; renderArr dc arrDb scale (lux, luy) (head childDiffTrees) arr
        }

    _ ->  dcDrawText dc ("unimplemented arrangement: "++shallowShowArr arr) (pt lux luy)
        

  ; when arrDb $
       do { -- drawRect dc (Rect (lux+ y w h) [ color := stringColor, brush:= BrushStyle BrushTransparent black ]
           renderID dc scale (lux+xA arr) (luy+yA arr) (idA arr)
          }

    }


drawFilledRect dc rect c =
  drawRect dc rect [ color := c, brush:= BrushStyle BrushSolid c ]     
                          


-- usage: for basic Arrangements, render after image so id is visible
--        for composites,         render before, so stacking order of arrangement is represented correctly
renderID dc scale x y NoIDA = return ()   -- use this case to disable NoIDA renderings
renderID dc scale x y id = 
 do { let idStr = case id of NoIDA -> " "
                             IDA i -> show i
    ; (Size w h) <- getTextExtent dc idStr
    
    ; drawRect dc (Rect (x+2) (y-5) (w+3) 10)
                  [ color := black, brush := BrushStyle BrushSolid yellow ]
    ; drawText dc idStr (pt (x+4) (y-6)) 
                  [ font := fontDefault { _fontFace = "Courier New", _fontSize = 6 }
                  , color := black ]
    }

debugColor (EmptyA id x y w h _ _)             = red
debugColor (StringA id x y w h _ _ _ _ _ _)    = colorRGB 0 255 255
debugColor (ImageA id x y w h _ _ _ _ _ _)     = colorRGB 92 64 0
debugColor (PolyA id x y w h _ _ _ _ _ _)      = red
debugColor (RectangleA id x y w h _ _ _ _ _ _) = red
debugColor (LineA id x y x' y' _ _ _ _)        = red
debugColor (RowA id x y w h _ _ _ _)           = colorRGB 255 0 0
debugColor (ColA id x y w h _ _ _ _)           = colorRGB 0 0 200  
debugColor (OverlayA id x y w h _ _ _ _)       = colorRGB 0 0 200  
debugColor (StructuralA _ child)           = colorRGB 230 230 255
debugColor (ParsingA _ child)              = colorRGB 255 230 230
--debugColor (LocatorA location child)     = debugColor child
debugColor arr                            = debug Err ("Renderer.debugColor: unhandled arrangement "++show arr) red



-- debug colors
stringColor       = colorRGB 0 255 255
imageColor        = colorRGB 92 64 0
rowColor          = colorRGB 255 0 0
colColor          = colorRGB 0 0 200  
overlayColor      = colorRGB 0 0 200  
structuralBGColor = colorRGB 230 230 255
parsingBGColor    = colorRGB 255 230 230

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
picobToCaretC :: Show node => ([Int], Int) -> Arrangement node -> Color -> Arrangement node
picobToCaretC path p color = picobToCaret 0 0 path p
 where picobToCaret x' y' _            (EmptyA _ _ _ _ _ _ _)               = debug Err "picobToCaret: empty Arrangement" (EmptyA NoIDA 0 0 0 0 0 0)
       picobToCaret x' y'  (_,o)       (StringA _  x y w h _ _ _ _ _ cxs')  = let cxs = init cxs' ++ [last cxs'-1]
                                                                              in  mkCaret (x'+x) (y'+y) h (cxs!!o)
       picobToCaret x' y'  _           (RectangleA _ x y w h _ _ _ _ _ _)   = mkBoxCaret (x'+x) (y'+y) w h
       picobToCaret x' y'  _           (LineA _ x y w h _ _ _ _)            = mkBoxCaret (x'+x) (y'+y) w h 
       picobToCaret x' y'  (p:path, o) (RowA _ x y w h _ _ _ arrs)           = picobToCaret (x'+x) (y'+y) (path,o) (arrs!!p)
       picobToCaret x' y'  ([], o)     (RowA _ x y w h _ _ _ arrs)           = mkBoxCaret (x'+x) (y'+y) w h 
       picobToCaret x' y'  (p:path, o) (ColA _ x y w h _ _ _ arrs)           = picobToCaret (x'+x) (y'+y) (path,o) (arrs!!p)
       picobToCaret x' y'  ([], o)     (ColA _ x y w h _ _ _ arrs)           = mkBoxCaret (x'+x) (y'+y) w h 
       picobToCaret x' y'  (p:path, o) (OverlayA _ x y w h _ _ _ arrs)       = picobToCaret (x'+x) (y'+y) (path,o) (arrs!!p)
       picobToCaret x' y'  ([], o)     (OverlayA _ x y w h _ _ _ arrs)       = mkBoxCaret (x'+x) (y'+y) w h 
       picobToCaret _ _ _           po                               = debug Err ("picobToCaret: unhandled case:"++show po) (EmptyA NoIDA 0 0 0 0 0 0)

       mkBoxCaret x y w h = --RectangleA 0 x y w h 0 Solid color color
                              RowA NoIDA 0 0 0 0 0 0 CommonTypes.white [ LineA NoIDA x y x (y+h) 0 0 2 color, LineA NoIDA x (y+h) (x+w) (y+h) 0 0 2 color
                                                      , LineA NoIDA (x+w) (y+h) (x+w) y 0 0 2 color, LineA NoIDA (x+w) y x y 0 0 2 color ]
       mkCaret x y h offset = RowA NoIDA 0 0 0 0 0 0 CommonTypes.white [LineA NoIDA (x+offset) y (x+offset) (y+h) 0 0 2 color]



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
mkFocus' p x' y' focus          (RectangleA _ x y w h _ _ _ _ _ _) = mkBoxCaret (x'+x) (y'+y) w h
mkFocus' p x' y' focus          (LineA _ x y x'' y'' _ _ _ _)          = mkBoxCaret (x'+x) (y'+y) (x''-x) (y''-y)
mkFocus' p x' y' (FocusA st en) (RowA _ x y w h _ _ _ arrs) = mkFocusList' p 0 (x'+x) (y'+y) (FocusA st en) arrs
mkFocus' p x' y' (FocusA st en) (ColA _ x y w h _ _ _ arrs) = mkFocusList' p 0 (x'+x) (y'+y) (FocusA st en) arrs
mkFocus' p x' y' (FocusA st en) (OverlayA _ x y w h _ _ _ (arr:arrs)) = mkFocus' (p++[0]) (x'+x) (y'+y) (FocusA st en) arr
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


-- ref lines not used, because caret is not incrementally rendered
-- because of line/box difference (line x y (x+w) y) is wider than (box x y w h) all to points are decreased
-- just decreasing w and h does not work
mkBoxCaret x y w h = 
  let color = CommonTypes.blue in
    [ LineA NoIDA x y x (y+h-1) 0 0 1 color, LineA NoIDA x (y+h-1) (x+w-1) (y+h-1) 0 0 1 color
    , LineA NoIDA (x+w-1) (y+h-1) (x+w-1) y 0 0 1 color, LineA NoIDA (x+w-1) y x y 0 0 1 color ]

arrangedFocusArea :: Show node => [Arrangement node] -> (Int,Int,Int,Int)
arrangedFocusArea fArrList = -- compute the region that is covered by the focus
  let (xs, ys, xs', ys') = unzip4 [(xA fLine, yA fLine, xA fLine + widthA fLine, yA fLine + heightA fLine) | fLine <- fArrList ]
  in  (if null xs then 0 else minimum xs, if null ys then 0 else minimum ys, maximum (0:xs'), maximum (0:ys'))


-- do we need draw the focus with appWindowPicture or accWindowPicture?

testArr :: Read node => String -> Arrangement node
testArr filePath = unsafePerformIO $
 do { putStrLn $ "Opening Arrangement file " ++ filePath
    ; str <- readFile filePath
    ; return $ read str
    }      


-- test stuff

-- ref lines are only used in incremental arranging, so can be 0 here
{-
arr0 = ColA NoIDA 100 100 0 0 0 0  CommonTypes.blue -- offsets are not correct here
        [ StringA NoIDA 0 0 0 0 0 0 "Hello World!" (0,0,0) defaultFont []
        , StringA NoIDA 0 40 0 0 0 0  "Bla" (0,0,0) defaultFont []
        , LocatorA NoNode
         $ RowA NoIDA 0 80 100 100 0 0 CommonTypes.red
                       [ RectangleA NoIDA 0 0 80 50 0 0 1 Solid (0,0,0) (255,0,0)
                       , StringA NoIDA 80 0 0 0 0 0 "B" (0,0,0) defaultFont []
                       ]
        ]
-}

arr1 =  StringA NoIDA 0 0 124 50 0 0 "blaaa" (0,0,0) defaultFont [0,28,40,68,96,124]--,
                                    --       StringA NoIDA 0 50 24 50 0 0 "x" (0,0,0) "Arial" 30 [0,24
  
  
  
  {-
  
  
  -- have to take into account the increased linesize when scaling
      -- actually, half the lineSize would be enough
--      lineSize                = let ls = scaleInt scale 1 in if ls < 1 then 1 else ls
--      focusArrList            = arrangeFocus focus arrangement
--      addLineSize (x,y,x',y') = (x-lineSize,y-lineSize,x'+lineSize,y'+lineSize)
--      (x,y,x',y')             = addLineSize $ scale4Tuple scale $ arrangedFocusArea focusArrList
    --; debugLnDraw Ren $ "focus area is :"++show (x,y,x',y')
--    ; debugLnDraw Ren $ "FocusA is :"++show focus
--      (oldFocus@(Rect ox oy ox' oy'),_,_) = oldUpdRegions
--      updatedRegionPres                                            = readUpdRegionPres ox

{- using incrementality hack -}
      (regx, regy, regx', regy') = maybe (-1,0,0,0) id $ updatedRectArr diffTree arrangement
       
{- using MVar hack:
      (regx, regy, regx', regy')                                   =
        case updatedRegionPres of
          AllUpdated  -> (0,0, 10000, 10000)
          BlockUpdated -> (0, min y oy, 10000, 10000)
          LineUpdated -> (min x ox, min y oy, 10000, max y' oy')
          NothingUpdated -> (-1,0,0,0)
-}


  in --{- showDebug Ren' "upd regions:" -}( rectBetween (Point x y) (Point (x') (y'))   -- current focus
       --        , oldFocus
       --        , rectBetween (Point regx regy) (Point (regx') (regy')) )
       (rect (pt 0 0) (sz 0 0), rect (pt 0 0) (sz 0 0), rectBetween (Point regx regy) (Point (regx') (regy')) )
       -- only the third one is used in GUI
--  where scale4Tuple scale (x,y,x',y') = (scaleInt scale x, scaleInt scale y, scaleInt scale x', scaleInt scale y')
    -}    

