module Rendering.RenLayerUtils ( module Common.CommonUtils
                     , module Arrangement.ArrUtils
                     , module Rendering.RenUtils
                     , module Rendering.RenLayerUtils    ) where


import Common.CommonTypes
import Rendering.RenLayerTypes

import Common.CommonUtils
import Arrangement.ArrUtils
import Rendering.RenUtils

arrowHeadSize :: Double
arrowHeadSize = 10

arrowHeadHalfAngle :: Double
arrowHeadHalfAngle = pi /6

layoutFocusColor = Common.CommonTypes.blue

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



-- ref lines not used, because caret is not incrementally rendered
-- because of line/box difference (line x y (x+w) y) is wider than (box x y w h) all to points are decreased
-- just decreasing w and h does not work
mkBoxCaret x y w h = 
  [ PolyA NoIDA x y w h 0 0 [(0,0),(0, h-1), (w-1, h-1),(w-1, 0), (0, 0)] 1 Transparent layoutFocusColor transparent transparent ]
mkEdgeCaret x1 y1 x2 y2 =
  [ GraphA NoIDA 0 0 (x1 `max` x2) (y1 `max` y2) 0 0 transparent 0 
      [ EdgeA NoIDA x1 y1 x2 y2 0 0 2 layoutFocusColor ] 
  ] -- dummy graph, since renderer expects edge to be inside graph (only for HTML rendering)
mkOutlineCaret x y w h outline = 
  [ PolyA NoIDA x y w h 0 0 (map outline [0, pi/10 ..2*pi]) 2 Transparent layoutFocusColor transparent transparent ]

