module Renderer where

import CommonTypes hiding (black, blue, green, cyan, red, magenta, yellow, white, grey) 
import qualified CommonTypes
import RenLayerTypes
import RenLayerUtils

import ArrLayerUtils -- for context menu hack
import PresTypes -- For Locations
import DocTypes -- For Locations
import DocUtils

import DocumentEdit -- Just for now
import GUI

import IOExts

import Graphics.UI.ObjectIO hiding (Font, fSize, defaultFont)
import qualified Graphics.UI.ObjectIO



computeUpdatedRegions oldUpdRegions scale focus diffTree arrangement =
  let -- have to take into account the increased linesize when scaling
      -- actually, half the lineSize would be enough
      lineSize                = let ls = scaleInt scale 1 in if ls < 1 then 1 else ls
      focusArrList            = arrangeFocus focus arrangement
      addLineSize (x,y,x',y') = (x-lineSize,y-lineSize,x'+lineSize,y'+lineSize)
      (x,y,x',y')             = addLineSize $ scale4Tuple scale $ arrangedFocusArea focusArrList
    --; debugLnDraw Ren $ "focus area is :"++show (x,y,x',y')
--    ; debugLnDraw Ren $ "FocusA is :"++show focus
      (oldFocus@( Rectangle (Point2 ox oy) (Point2 ox' oy')) ,_,_) = oldUpdRegions
      updatedRegionPres                                            = readUpdRegionPres ox

{- using incrementality hack -}
      (regx, regy, regx', regy')                                   = maybe (-1,0,0,0) id $ updatedRectArr diffTree arrangement
       
{- using MVar hack:
      (regx, regy, regx', regy')                                   =
        case updatedRegionPres of
          AllUpdated  -> (0,0, 10000, 10000)
          BlockUpdated -> (0, min y oy, 10000, 10000)
          LineUpdated -> (min x ox, min y oy, 10000, max y' oy')
          NothingUpdated -> (-1,0,0,0)
-}


  in {- showDebug Ren' "upd regions:" -}( Rectangle (Point2 x y) (Point2 (x'+1) (y'+1))   -- current focus
               , oldFocus
               , Rectangle (Point2 regx regy) (Point2 (regx'+1) (regy'+1)) )
  where scale4Tuple scale (x,y,x',y') = (scaleInt scale x, scaleInt scale y, scaleInt scale x', scaleInt scale y')
        




myArr' = arr00
myArr = ColA NoIDA 0 0 0 0 CommonTypes.blue
         [ RectangleA NoIDA 0 0 10 10 1 Solid (0,0,0) (255,0,0)
         ,strr 40 80 "bla"
        
         ]
strr x y str = StringA NoIDA x y 0 0 str (0,0,0) defaultFont [0,28,40,68,96,124]--,

arr00 = ColA NoIDA 10 10 0 0 CommonTypes.blue -- offsets are not correct here
        [ RectangleA NoIDA 0 0 80 50 1 Solid (0,0,0) (255,0,0)
        , StringA NoIDA 80 0 0 0 "Hello World!" (0,0,0) defaultFont []
        , StringA NoIDA 0 40 0 0  "Bla" (0,0,0) defaultFont []
        , StringA NoIDA 80 0 0 0 "B" (0,0,0) defaultFont []
        ]

{-
arr1 =  StringA NoIDA 0 0 124 50 "blaaa" (0,0,0) defaultFont [0,28,40,68,96,124]--,
                                    --       StringA NoIDA 0 50 24 50 "x" (0,0,0) "Arial" 30 [0,24

-}

-- end of hack.
      
mkPopupMenuXY :: Presentation -> Scale -> Arrangement -> Id -> ((RenderingLevel, EditRendering) -> IO (RenderingLevel, EditRendering')) 
              -> Int -> Int -> GUI RenderingLevel ()
mkPopupMenuXY prs scale arr@(LocatorA (DocNode doc _) _) windowId handler x' y' = 
 do { let (x,y) = (descaleInt scale x',descaleInt scale y')
    ; let ctxtItems = case pointOvlRev' x y [[]] arr of
                        (pthA:_) -> popupMenuItemsPres (addWithSteps pthA prs) prs
                        []       -> []
              
    ; case map pathNode (pointDoc' x y arr) of
        (pth:_) ->
         do { let alts = menuD pth doc
            ; let items = ctxtItems++alts
            ; liftIO $ putStrLn $ show (map fst items)
            
            ; openMenu () (PopUpMenu (ListLS (map mkMenuItem items))) undefined
            ; return ()
            }
        _ -> return ()  
    }                         -- This stuff belongs in GUI, renderer should only export names + edit ops
 where mkMenuItem (str, upd) = MenuItem str [ MenuFunction (noLS (popupMenuHandler upd windowId handler))]
mkPopupMenuXY _ _ _ _ _ x y = 
 do { debugLnGUI Err "Arrangement root is no document locator"
    ; return ()
    }

{-

mouseDownDoc state arrLvl layout (PathA pthA _) i = -- only look at start of focus. focus will be empty


-}

                      

-- debugged rendering also displays overlay for focus adding, but this has not been processed by debugArrangement
-- this makes it tricky to move the debuggedArrangement, since the Gest.Int. will not know about it
-- however, we don't want to debug the focus
--render' :: (LocalStateRen, MappingRen) -> Arrangement -> (Rendering, (LocalStateRen, MappingRen))
render' scale arrDb focus diffTree arrangement = 
 do {  -- seq (walk arrangement) $ return ()        -- maybe this is not necessary anymore, now the datastructure is strict
    --; debugLnDraw Ren (show arrangement)
    ; let focusArrList = arrangeFocus focus arrangement
    ; debugLnDraw Err ("The updated rectangle is: "++show (updatedRectArr diffTree arrangement))
    

    ; setPenColour (RGB 255 255 255)  
    ; fillAt (Point2 0 0) (Box 2000 1000)                                     -- extra node for Overlay + dirties for focus
    ; renderArr arrDb scale origin 
                         (DiffNode False False $ replicate (length focusArrList) (DiffLeaf False)++[diffTree])
                         
                         (OverlayA NoIDA 0 0 2000 1000 (255,255,255)
                              --(xA arrangement) (yA arrangement)  (widthA arrangement) (heightA arrangement)
-- using arrangement size goes wrong when arrangement shrinks
-- TODO fix viewsize/windowsize/arrangementSize, etc.
--                         ( arrangement : focusArrList)) -- ++ [arrangement]))
                              (focusArrList ++ [arrangement])) 
  
    }

{- renderArr _ _ _ _ = 
 do { setPenColour red
    ; drawAt (Point2 20 20) "Rendering display is turned off"
    } -}

renderArr arrDb scale (lux, luy) diffTree arr =
 do { -- debugLnDraw Err (shallowShowArr arr ++":"++ show (isClean diffTree));
     --if True then return () else    -- uncomment this line to skip rendering

     if (isSelfClean diffTree)  -- if self is clean, only render its children (if present)
     then if (isClean diffTree)
          then return ()
          else let renderChildren x' y' arrs =
                    do { let (x,y)=(lux+scaleInt scale x', luy+scaleInt scale y')
                       ; let childDiffTrees = case diffTree of
                                                DiffLeaf c     -> repeat $ DiffLeaf c
                                                DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
                       ; sequence_ $ zipWith (renderArr arrDb scale (x, y)) childDiffTrees arrs 
                       }
               in case arr of
                    RowA     _ x' y' _ _ _ arrs -> renderChildren x' y' arrs
                    ColA     _ x' y' _ _ _ arrs -> renderChildren x' y' arrs
                    OverlayA _ x' y' _ _ _ arrs -> renderChildren x' y' arrs
                    StructuralA _ arr           -> renderChildren 0 0 [arr]
                    ParsingA _ arr              -> renderChildren 0 0 [arr]
                    LocatorA _ arr              -> renderChildren 0 0 [arr]
                    _ -> return ()
     else
  case arr of 

    (EmptyA _ _ _ _ _) ->
      return ()
      
    (StringA id x' y' w' h' str (fr, fg, fb) font _) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; setPenSize 1
        
        ; if arrDb
          then
           do { setPenColour stringColor
              ; drawAt (Point2 x y) (Box w h)
              ; renderID scale x y id
              }
          else return ()
    
        ; setPenColour (RGB fr fg fb) 
             
        ; let fontOIO = Graphics.UI.ObjectIO.Font (fFamily font) (scaleInt scale (fSize font)) 
                                                  (fBold font) (fUnderline font) (fItalic font) (fStrikeOut font)
    
        ; setPenFont fontOIO
        ; fontMetrics <- getPenFontMetrics
        ; drawAt (Point2 x (y+fAscent fontMetrics)) str
    
        ; if arrDb then renderID scale x y id else return ()
        }

    (ImageA id x' y' w' h' src style (lr,lg,lb) (br,bg,bb)) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; setPenSize 1
        
        ; (x,y,w,h) <- if arrDb
                       then
                        do { setPenColour imageColor
                           ; drawAt (Point2 x y) (Box w h)
                           ; return (x+(scaleInt scale hpd), y+(scaleInt scale hpd), w-(scaleInt scale pd), h-(scaleInt scale pd))
                           }
                       else 
                             return (x,y,w,h)
    
        ; mbitmap <- liftIO $ openBitmap src
        
        -- can't clip! For squiggly rendering, we center and tile, if image is too large it is not shown
        -- TODO: make this the Tile case and make a Resize case 
        ; setPenColour (RGB lr lg lb)  
        ; setPenBack (RGB br bg bb)  
        ; case mbitmap of
            Nothing -> debugLnDraw Err $ "Renderer.renderArr: could not open bitmap "++show src
            Just bitmap' ->
              do { let Size iw' ih' = getBitmapSize bitmap'
                 ; let (iw, ih) = (scaleInt scale iw', scaleInt scale ih')
                 ; let bitmap = resizeBitmap (Size iw ih) bitmap' 
                 ; let (hrepeats, hrest) = w `divMod` iw 
                 ; let (vrepeats, vrest) = h `divMod` ih 
                 ; let cx = hrest `div` 2
                 ; let cy = vrest `div` 2
                 ; sequence_ [ drawAt (Point2 (x+i*iw+cx) (y+j*ih+cy)) bitmap | i<-[0..hrepeats-1], j<-[0..vrepeats-1] ]
                 }
        ; if arrDb then renderID scale x y id else return ()    
        }

    (RectangleA id x' y' w' h' lw' style (lr,lg,lb) (fr,fg,fb)) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        
        ; setPenSize $ let ls = scaleInt scale lw' in if ls < 1 then 1 else ls
    
        ; case style of 
            Solid -> do { setPenColour (RGB fr fg fb)  
                        ; fillAt (Point2 x y) (Box w h)
                        }
            _     -> return ()
        ; setPenColour (RGB lr lg lb)  
        ; drawAt (Point2 x y) (Box w h)
        }


    (LineA id lux' luy' rlx' rly' lw' (lr,lg,lb)) ->
     do { let (lnlux,lnluy, lnrlx, lnrly)=(lux+scaleInt scale lux', luy+scaleInt scale luy', scaleInt scale rlx', scaleInt scale rly')
    
        ; setPenSize $ let ls = scaleInt scale lw' in if ls < 1 then 1 else ls
        ; setPenColour (RGB lr lg lb)  
        ; draw (Line2 (Point2 lnlux lnluy) (Point2 lnrlx lnrly))
        }


    (PolyA id x' y' w' h' pts' lw' (lr,lg,lb) (br,bg,bb)) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let pts = map (\(x',y') -> (x+scaleInt scale x', y+scaleInt scale y')) pts'
        ; setPenSize 1
    
        ; if not arrDb then
           do { setPenColour (RGB br bg bb)      
            --  ; fillAt (Point2 x y) (Box w h) -- for now, no background fill, because poly in overlay will cover
              }                                 -- first child. We want a Transparent color for this
          else return ()
    
        ; setPenSize $ let ls = scaleInt scale lw' in if ls < 1 then 1 else ls
        ; setPenColour (RGB lr lg lb)  
        
        ; if null pts      -- objectIO poly's are always closed, so we draw them ourselves
          then return ()   -- with line widths and round corners etc., this would lead to incorrect renderings
          else sequence_ $ [ draw (Line2 (Point2 lux luy) (Point2 rlx rly))
                           | ((lux,luy),(rlx,rly)) <- zip (init pts) (tail pts) ]
        --; draw (Line2 (Point2 lux luy) (Point2 rlx rly))
        }


    (RowA id x' y' w' h' (br,bg,bb) arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False) -- in case there are too few dts
        ; setPenSize 1
        ; if arrDb
          then
           do { let childCnt = length arrs 
    --          ; setPenColour (RGB br bg bb)  
    --          ; fillAt (Point2 x y) (Box w h)
              ; setPenColour rowColor
              ; drawAt (Point2 x y) (Box w h)
              ; sequence_ $ [draw (Line2 (Point2 (x+i) y) (Point2 (x+i) (y+h))) 
                            | i <- if null arrs then []
                                                else showDebug Ren $ tail.init $
                                                     scanl (\n1 n2 -> n1 + (scaleInt scale (pd-1))+n2 ) 0 
                                                           (map (scaleInt scale . widthA) arrs)]
              ; renderID scale x y id
              ; sequence_ $ zipWith (renderArr arrDb scale (x, y)) childDiffTrees arrs
              }
          else 
           do { --setPenColour (if isClean diffTree then (RGB br bg bb) else red)
                setPenColour (RGB br bg bb)
              ; fillAt (Point2 x y) (Box w h)
              ; setPenColour (RGB 0 0 0)
              ; setPenBack (RGB 0 0 0)
              ; sequence_ $ zipWith (renderArr arrDb scale (x, y)) childDiffTrees arrs
              }
        }

    (ColA id x' y' w' h' (br,bg,bb) arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; setPenSize 1
        ; if arrDb
          then
           do { let childCnt = length arrs 
              ; setPenColour (RGB br bg bb)  
              ; fillAt (Point2 x y) (Box w h)
              ; setPenColour colColor
              ; drawAt (Point2 x y) (Box w h)
              ; sequence_ $ [draw (Line2 (Point2 x (y+i)) (Point2 (x+w) (y+i))) 
                            | i <- if null arrs then []
                                                else showDebug Ren $ tail.init $
                                                     scanl (\n1 n2 -> n1+(scaleInt scale (pd-1))+n2 ) 0 
                                                           (map (scaleInt scale . heightA) arrs)]
              
              ; renderID scale x y id
              ; sequence_ $ zipWith (renderArr arrDb scale (x, y)) childDiffTrees arrs
              }
          else 
           do { setPenColour (RGB br bg bb)  
              ; fillAt (Point2 x y) (Box w h)
              ; sequence_ $ zipWith (renderArr arrDb scale (x, y)) childDiffTrees arrs
              }
        }

    (OverlayA id x' y' w' h' (br,bg,bb) arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; setPenSize 1
        ; if arrDb
          then
           do { let childCnt = length arrs 
    --          ; setPenColour (RGB br bg bb)  
    --          ; fillAt (Point2 x y) (Box w h)
              ; setPenColour overlayColor
              ; drawAt (Point2 x y) (Box w h)
              ; renderID scale x y id
              
              ; let (arrs',cdts') = if null arrs then (arrs,childDiffTrees)
                                                 else case (last arrs) of EmptyA _ _ _ _ _ -> (arrs, childDiffTrees)
                                                                          _                -> unzip . reverse $ zip arrs childDiffTrees
              
              ; sequence_ $ zipWith (renderArr arrDb scale (x, y))  cdts' arrs'
              }
          else 
           do {  setPenColour (RGB br bg bb)  
              ; fillAt (Point2 x y) (Box w h)
               -- nasty workaround hack for overlay problem: if last elt of overlay is EmptyA,
               -- the children are reversed. Squigglies are the only presentations for now that
               -- need to be put in front of the overlay, but that should not get parsed.
              ; let (arrs',cdts') = if null arrs then (arrs,childDiffTrees)
                                                 else case (last arrs) of EmptyA _ _ _ _ _ -> (arrs, childDiffTrees)
                                                                          _                -> unzip . reverse $ zip arrs childDiffTrees
              -- until ovl is properly reversed
              ; sequence_ $ zipWith (renderArr arrDb scale (x, y)) cdts' arrs'
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
        ; if arrDb 
          then
           do { renderID scale x y id
              ; setPenColour structuralBGColor
              ; setPenSize 1
              ; fillAt (Point2 x y) (Box w h)
              }
          else return ()
        ; renderArr arrDb scale (lux, luy) (head childDiffTrees) arr
        }
    
    (ParsingA id arr) ->
     do { let (x,y,w,h)=( lux+scaleInt scale (xA arr), luy+scaleInt scale (yA arr) 
                        , scaleInt scale (widthA arr), scaleInt scale (heightA arr) )
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; if arrDb 
          then
           do { renderID scale x y id
              ; setPenColour parsingBGColor 
              ; setPenSize 1
              ; fillAt (Point2 x y) (Box w h)
              }
          else return ()
        ; renderArr arrDb scale (lux, luy) (head childDiffTrees) arr
        }

    (LocatorA _ arr) ->
     do {
        ; let childDiffTrees = case diffTree of
                                 DiffLeaf c     -> repeat $ DiffLeaf c
                                 DiffNode c c' dts -> dts ++ repeat (DiffLeaf False)
        ; renderArr arrDb scale (lux, luy) (head childDiffTrees) arr
        }
        
    _ -> 
     do { setPenColour (RGB 255 0 0) 
        ; drawAt (Point2 10 100) ("Renderer: unimplemented arrangement "++show arr) 
        }

    }



-- usage: for basic Arrangements, render after image so id is visible
--        for composites,         render before, so stacking order of arrangement is represented correctly
renderID scale x y NoIDA = return ()   -- use this case to disable NoIDA renderings
renderID scale x y id = 
 do { let font = Graphics.UI.ObjectIO.Font "Courier new" 6 False False False False
    ; setPenFont font
    ; setPenSize 1
    ; fontMetrics <- getPenFontMetrics
    ; setPenColour (RGB 255 255 0)
    ; let idStr = case id of NoIDA -> " "
                             IDA i -> show i
    ; strW <- getPenFontStringWidth idStr
    ; let box = Box (strW+3) 10
    ; fillAt (Point2 (x+2) (y-5)) box     
    ; setPenColour (RGB 0 0 0)  
    ; drawAt (Point2 (x+2) (y-5)) box     
    ; drawAt (Point2 (x+4) (y+3)) idStr 
    }


-- debug colors
stringColor       = RGB 0 255 255
imageColor        = RGB 92 64 0    
rowColor          = RGB 255 0 0
colColor          = RGB 0 0 200  
overlayColor      = RGB 128 0 128
structuralBGColor = RGB 230 230 255
parsingBGColor    = RGB 255 230 230

-- focus is not perfect, but this has to do with the selection algorithm as well.
-- for now, it is a working solution

arrangeFocus :: FocusArr -> Arrangement -> [Arrangement]
arrangeFocus (FocusA from to) arrangement = {- scaleFocusArrList scale $-} {-debug Ren ("focus:" ++show (from,to)) $-} mkFocus (FocusA from to) arrangement
                                      --[ picobToCaretC from arrangement green
                                      --, picobToCaretC to arrangement blue 
                                      -- ]
arrangeFocus (NoFocusA) _ = []


picobToCaretC :: ([Int], Int) -> Arrangement -> Color -> Arrangement
picobToCaretC path p color = picobToCaret 0 0 path p
 where picobToCaret x' y' _            (EmptyA _ _ _ _ _)               = debug Err "picobToCaret: empty Arrangement" (EmptyA NoIDA 0 0 0 0)
       picobToCaret x' y'  (_,o)       (StringA _  x y w h _ _ _ cxs)   = mkCaret (x'+x) (y'+y) h (cxs!!o)
       picobToCaret x' y'  _           (RectangleA _ x y w h _ _ _ _)   = mkBoxCaret (x'+x) (y'+y) w h
       picobToCaret x' y'  _           (LineA _ x y w h _ _)            = mkBoxCaret (x'+x) (y'+y) w h 
       picobToCaret x' y'  (p:path, o) (RowA _ x y w h _ arrs)           = picobToCaret (x'+x) (y'+y) (path,o) (arrs!!p)
       picobToCaret x' y'  ([], o)     (RowA _ x y w h _ arrs)           = mkBoxCaret (x'+x) (y'+y) w h 
       picobToCaret x' y'  (p:path, o) (ColA _ x y w h _ arrs)           = picobToCaret (x'+x) (y'+y) (path,o) (arrs!!p)
       picobToCaret x' y'  ([], o)     (ColA _ x y w h _ arrs)           = mkBoxCaret (x'+x) (y'+y) w h 
       picobToCaret x' y'  (p:path, o) (OverlayA _ x y w h _ arrs)       = picobToCaret (x'+x) (y'+y) (path,o) (arrs!!p)
       picobToCaret x' y'  ([], o)     (OverlayA _ x y w h _ arrs)       = mkBoxCaret (x'+x) (y'+y) w h 
       picobToCaret _ _ _           po                               = debug Err ("picobToCaret: unhandled case:"++show po) (EmptyA NoIDA 0 0 0 0)

       mkBoxCaret x y w h = --RectangleA 0 x y w h 0 Solid color color
                              RowA NoIDA 0 0 0 0 CommonTypes.white [ LineA NoIDA x y x (y+h) 2 color, LineA NoIDA x (y+h) (x+w) (y+h) 2 color
                                                      , LineA NoIDA (x+w) (y+h) (x+w) y 2 color, LineA NoIDA (x+w) y x y 2 color ]
       mkCaret x y h offset = RowA NoIDA 0 0 0 0 CommonTypes.white [LineA NoIDA (x+offset) y (x+offset) (y+h) 2 color]



mkFocus :: FocusArr -> Arrangement -> [Arrangement]
mkFocus focus arr = mkFocus' [] 0 0 (orderFocusA focus) arr



-- precondition, node is only visited if it part of it is focused
mkFocus' p x' y' focus          (EmptyA _ _ _ _ _) = []
mkFocus' p x' y' (FocusA (PathA stp sti) (PathA enp eni)) (StringA _  x y w h _ _ _ cxs) = 
  let st = if length stp < length p || stp < p then 0 else cxs!!!sti
      en = if  enp > p then last cxs else cxs!!!eni                        
  in  mkBoxCaret (x'+x+st) (y'+y) (en-st + 1) h
mkFocus' p x' y' focus          (ImageA _ x y w h _ _ _ _)       = mkBoxCaret (x'+x) (y'+y) w h
mkFocus' p x' y' focus          (PolyA _ x y w h _ _ _ _)        = mkBoxCaret (x'+x) (y'+y) w h
mkFocus' p x' y' focus          (RectangleA _ x y w h _ _ _ _) = mkBoxCaret (x'+x) (y'+y) w h
mkFocus' p x' y' focus          (LineA _ x y x'' y'' _ _)          = mkBoxCaret (x'+x) (y'+y) (x''-x) (y''-y)
mkFocus' p x' y' (FocusA st en) (RowA _ x y w h _ arrs) = mkFocusList' p 0 (x'+x) (y'+y) (FocusA st en) arrs
mkFocus' p x' y' (FocusA st en) (ColA _ x y w h _ arrs) = mkFocusList' p 0 (x'+x) (y'+y) (FocusA st en) arrs
mkFocus' p x' y' (FocusA st en) (OverlayA _ x y w h _ (arr:arrs)) = mkFocus' (p++[0]) (x'+x) (y'+y) (FocusA st en) arr
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


-- because of line/box difference (line x y (x+w) y) is wider than (box x y w h) all to points are decreased
-- just decreasing w and h does not work
mkBoxCaret x y w h = 
  let color = CommonTypes.blue in
    [ LineA NoIDA x y x (y+h-1) 1 color, LineA NoIDA x (y+h-1) (x+w-1) (y+h-1) 1 color
    , LineA NoIDA (x+w-1) (y+h-1) (x+w-1) y 1 color, LineA NoIDA (x+w-1) y x y 1 color ]

arrangedFocusArea :: [Arrangement] -> (Int,Int,Int,Int)
arrangedFocusArea fArrList = -- compute the region that is covered by the focus
  let (xs, ys, xs', ys') = unzip4 [(xA fLine, yA fLine, xA fLine + widthA fLine, yA fLine + heightA fLine) | fLine <- fArrList ]
  in  (if null xs then 0 else minimum xs, if null ys then 0 else minimum ys, maximum (0:xs'), maximum (0:ys'))


-- do we need draw the focus with appWindowPicture or accWindowPicture?

testArr :: String -> Arrangement
testArr filePath = unsafePerformIO $
 do { putStrLn $ "Opening Arrangement file " ++ filePath
    ; str <- readFile filePath
    ; return $ read str
    }      


-- test stuff


arr0 = ColA NoIDA 100 100 0 0 CommonTypes.blue -- offsets are not correct here
        [ StringA NoIDA 0 0 0 0 "Hello World!" (0,0,0) defaultFont []
        , StringA NoIDA 0 40 0 0  "Bla" (0,0,0) defaultFont []
        , LocatorA NoNode
         $ RowA NoIDA 0 80 100 100 CommonTypes.red
                       [ RectangleA NoIDA 0 0 80 50 1 Solid (0,0,0) (255,0,0)
                       , StringA NoIDA 80 0 0 0 "B" (0,0,0) defaultFont []
                       ]
        ]


arr1 =  StringA NoIDA 0 0 124 50 "blaaa" (0,0,0) defaultFont [0,28,40,68,96,124]--,
                                    --       StringA NoIDA 0 50 24 50 "x" (0,0,0) "Arial" 30 [0,24