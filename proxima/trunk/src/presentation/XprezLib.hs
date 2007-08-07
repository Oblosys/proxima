module XprezLib where

import CommonTypes
import DocTypes
import PresTypes

import Maybe

-- switch href and vref href is y and vref is x, so (vref, href) is more logical

type Xprez doc node clip = Presentation doc node clip
{-
data Xprez = Empty
           | Text String
           | Row Int [ Xprez ] 
           | Column Int [ Xprez ] 
           | Overlay [ Xprez ]
           | Matrix [[ Xprez ]]
           | Rectangle Int Int Int
           | Ellipse Int Int Int
           | Image String
           | Poly [ (Float, Float) ] Int
           | Formatter [ Xprez ]
           | Alternative [ Xprez ]
           | With Xprez AttrRule
           | WithC Xprez ConstrRule 
           | Locator Xprez node deriving Show
-}

-- the with parameters are ordered for infix use:  xp `with` ...
empty = EmptyP NoIDP
text = StringP NoIDP
row = RowP NoIDP 0
rowR r = RowP NoIDP r
col = ColP NoIDP 0
colR r = ColP NoIDP r
overlay = OverlayP NoIDP 
--matrix = Matrix 
rect w h = RectangleP NoIDP w h 1
ellipse w h = EllipseP NoIDP w h 1
img src = ImageP NoIDP src
poly pts = PolyP NoIDP pts 1
polyW w pts = PolyP NoIDP pts w
formatter xps = FormatterP NoIDP xps
--alt xps = Alternative xps
with_ xp f = WithP f xp
structural xp = StructuralP NoIDP xp
parsing xp = ParsingP NoIDP xp
loc l xp  = LocatorP l xp

graph :: Int -> Int -> [(Int,Int)] -> [Xprez doc node clip] -> Xprez doc node clip
graph width height edges vertexPress = 
  GraphP NoIDP Clean width height edges vertexPress
 
vertex :: Int -> Int -> Int -> Outline -> Xprez doc node clip -> Xprez doc node clip
vertex id x y outline pres = VertexP NoIDP id x y outline pres

leftXp `beside` rightXp = row [leftXp, rightXp]

topXp `above` bottomXp = col [topXp, bottomXp]


withInh xp inf =  with_ xp (\(i,s) -> (inf i, s))
withSyn xp snf =  with_ xp (\(i,s) -> (i, snf s))

withColor :: Xprez doc node clip -> Color -> Xprez doc node clip
withColor xp c = withInh xp (\i -> i { textColor = c, lineColor = c})

withtxtColor :: Xprez doc node clip -> Color -> Xprez doc node clip
withtxtColor xp c = withInh xp (\i -> i { textColor = c})

withtxtColor_ :: Xprez doc node clip -> (Color -> Color) -> Xprez doc node clip
withtxtColor_ xp f = withInh xp (\i -> i { textColor = f (textColor i)})

withlnColor :: Xprez doc node clip -> Color -> Xprez doc node clip
withlnColor xp c = withInh xp (\i -> i { lineColor = c})

withfColor :: Xprez doc node clip -> Color -> Xprez doc node clip
withfColor xp c = withInh xp (\i -> i { fillColor = c})

withbgColor :: Xprez doc node clip -> Color -> Xprez doc node clip
withbgColor xp c = withInh xp (\i -> i { backgroundColor = c})

withbgColor_ :: Xprez doc node clip -> (Color -> Color) -> Xprez doc node clip
withbgColor_ xp f = withInh xp (\i -> i { backgroundColor = f (backgroundColor i)})

withFontSize :: Xprez doc node clip -> Int -> Xprez doc node clip
withFontSize xp fs = withInh xp (\i -> i { font = (font i) { fSize = fs }})

withFontFam :: Xprez doc node clip -> String -> Xprez doc node clip
withFontFam xp ff = withInh xp (\i -> i { font = (font i) { fFamily = ff }})

withFontSize_ :: Xprez doc node clip -> (Int -> Int) -> Xprez doc node clip
withFontSize_ xp ffs = withInh xp (\i -> i { font = (font i) { fSize = ffs (fSize (font i)) }})

withFont :: Xprez doc node clip -> Font -> Xprez doc node clip
withFont xp f = withInh xp (\i -> i { font = f })

withFont_ :: Xprez doc node clip -> (Font -> Font) -> Xprez doc node clip
withFont_ xp ff = withInh xp (\i -> i { font = ff (font i) })

-- just set Family and Size
withFont' :: Xprez doc node clip -> (String, Int) -> Xprez doc node clip
withFont' xp (ff,fs) = withFont_ xp (\f -> f { fFamily = ff, fSize = fs })

bold :: Xprez doc node clip -> Xprez doc node clip
bold xp = withFont_ xp (\f -> f { fBold = True })

italic :: Xprez doc node clip -> Xprez doc node clip
italic xp = withFont_ xp (\f -> f { fItalic = True })

underline :: Xprez doc node clip -> Xprez doc node clip
underline xp = withFont_ xp (\f -> f { fUnderline = True })

strikeOut :: Xprez doc node clip -> Xprez doc node clip
strikeOut xp = withFont_ xp (\f -> f { fStrikeOut = True })


withMouseDown :: Xprez doc node clip -> UpdateDoc doc clip -> Xprez doc node clip
withMouseDown xp upd = withInh xp (\i -> i { mouseDown = Just upd })

withPopupMenuItems :: Xprez doc node clip -> [PopupMenuItem doc clip] -> Xprez doc node clip
withPopupMenuItems xp mis = withInh xp (\i -> i { popupMenuItems = mis })

withPopupMenuItems_ :: Xprez doc node clip -> ([PopupMenuItem doc clip] -> [PopupMenuItem doc clip]) -> Xprez doc node clip
withPopupMenuItems_ xp fmis = withInh xp (\i -> i { popupMenuItems = fmis (popupMenuItems i) })

addPopupItems :: Xprez doc node clip -> [PopupMenuItem doc clip] -> Xprez doc node clip
addPopupItems xp mis = withPopupMenuItems_ xp (\pmis -> mis++pmis) 

withXRef :: Xprez doc node clip -> Int -> Xprez doc node clip
withXRef xp v = withSyn xp (\s -> s { xRef = v })

withXRef_ :: Xprez doc node clip -> (Int -> Int) -> Xprez doc node clip
withXRef_ xp fx = withSyn xp (\s -> s { xRef = fx (xRef s) })

withHRef :: Xprez doc node clip -> Int -> Xprez doc node clip
withHRef xp h = withSyn xp (\s -> s { hRef = h })

withHRef_ :: Xprez doc node clip -> (Int -> Int) -> Xprez doc node clip
withHRef_ xp fh = withSyn xp (\s -> s { hRef = fh (hRef s) })

withRef :: Xprez doc node clip -> (Int, Int) -> Xprez doc node clip
withRef xp (x,h) = withSyn xp (\s -> s { xRef = x, hRef = h })

withRef_ :: Xprez doc node clip -> ((Int, Int) -> (Int, Int)) -> Xprez doc node clip
withRef_ xp fhw = withSyn xp (\s -> let (v,h) = fhw (xRef s, hRef s)
                                     in  s { xRef = v, hRef = h })

withHStretch :: Xprez doc node clip -> Bool -> Xprez doc node clip
withHStretch xp hs = withSyn xp (\s -> s { hStretch = hs })

withVStretch :: Xprez doc node clip -> Bool -> Xprez doc node clip
withVStretch xp vs = withSyn xp (\s -> s { vStretch = vs })

withStretch :: Xprez doc node clip -> Bool -> Xprez doc node clip
withStretch xp str = withSyn xp (\s -> s { hStretch = str, vStretch = str })

withWidth :: Xprez doc node clip -> Int -> Xprez doc node clip
withWidth xp w = withSyn xp (\s -> s { minWidth = w, hStretch = False })

withHeight :: Xprez doc node clip -> Int -> Xprez doc node clip
withHeight xp h = withSyn xp (\s -> s { minHeight = h, vStretch = False })

withSize :: Xprez doc node clip -> (Int, Int) -> Xprez doc node clip
withSize xp (w,h) = withSyn xp (\s -> s { minWidth = w, hStretch = False 
                                        , minHeight = h, vStretch = False  })


move x y xp = xp `withRef_` (\(h,v)-> (h-y, v-x))
                 `with_` (\(i,s) -> let i' = i { assignedHRef = assignedHRef i + x
                                               , assignedXRef = assignedXRef i + y
                                               } 
                                        s' = s { finalHRef = finalHRef s -x
                                               , finalXRef = finalXRef s-y
                                               }
                                    in (i',s'))



hLine :: Xprez doc node clip
--hLine = poly [(0,0),(1.0,0)] `withHeight` 1 
hLine = row [empty `withHStretch` True `withHeight` 1] `withInh` (\i -> i {backgroundColor = lineColor i})
-- workaround for incorrect poly rendering.

vLine :: Xprez doc node clip
--vLine = poly [(0,0),(0,1.0)] `withWidth` 1
vLine = row [empty `withVStretch` True `withWidth` 1] `withInh` (\i -> i {backgroundColor = lineColor i})
-- workaround for incorrect poly rendering.

-- lineWidth should be an attribute, so we can use a with here
hLineW :: Int -> Xprez doc node clip
hLineW lw = polyW lw [(0,0),(1.0,0)] `withHeight` 1

vLineW :: Int -> Xprez doc node clip
vLineW lw = polyW lw [(0,0),(0,1.0)] `withWidth` 1

hvStretch = empty `withHStretch` True `withVStretch` True


hSpace w = col [ empty ] `withWidth` w `withVStretch` True

vSpace h = row [ empty ] `withHeight` h `withHStretch` True

hAlignCenter xp = row [ hvStretch, xp, hvStretch ] 
hAlignLeft xp = rowR  1 [xp,  hvStretch ]
hAlignRight xp = row [ hvStretch, xp]

vAlignCenter xp = col [ hvStretch, xp, hvStretch]
vAlignTop xp = colR  1 [ xp,  hvStretch]
vAlignBottom xp = col [ hvStretch, xp]


hRefHalf xp = xp `with_` (\(i,s) -> let refdif = hRef s - assignedWidth i `div` 2
                                        newI = i { assignedHRef = assignedHRef i + refdif
                                                 }
                                        newS = s { hRef = hRef s - refdif -- assignedHeight i `div` 2
                                                 , finalHRef = finalHRef s - refdif
                                                 }
                                                  
                                     in (newI,newS))

xRefHalf xp = xp `with_` (\(i,s) -> let refdif = xRef s - assignedHeight i `div` 2
                                        newI = i { assignedXRef = assignedXRef i + refdif
                                                 }
                                        newS = s { xRef = xRef s - refdif -- assignedHeight i `div` 2
                                                 , finalXRef = finalXRef s - refdif
                                                 }
                                                  
                                     in (newI,newS))

glue :: Xprez doc node clip
glue = empty `withHStretch` True `withVStretch` True

boxed :: Xprez doc node clip -> Xprez doc node clip
boxed p = colR 1 [ hLine, rowR 1 [ vLine, p, vLine ], hLine ]

-- multiply x with a percentage
percent :: Int -> Int -> Int
percent a x = a * x `div` 100

