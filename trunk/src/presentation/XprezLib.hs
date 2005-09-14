module XprezLib where

import CommonTypes


import DocTypes
import PresTypes

-- switch href and vref href is y and vref is x, so (vref, href) is more logical

type Xprez node = Presentation node
{-
data Xprez = Empty
           | Text String
           | Row Int [ Xprez ] 
           | Column Int [ Xprez ] 
           | Overlay [ Xprez ]
           | Matrix [[ Xprez ]]
           | Rectangle Int Int Int
           | Image String
           | Poly [ (Float, Float) ] Int
           | Formatter [ Xprez ]
           | Alternative [ Xprez ]
           | With Xprez AttrRule
           | WithC Xprez ConstrRule 
           | Locator Xprez Node deriving Show
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
img src = ImageP NoIDP src
poly pts = PolyP NoIDP pts 1
polyW w pts = PolyP NoIDP pts w
--formatter xps = Formatter xps
--alt xps = Alternative xps
with_ xp f = WithP f xp
structural xp = StructuralP NoIDP xp
parsing xp = ParsingP NoIDP xp
loc l xp  = LocatorP l xp


leftXp `beside` rightXp = row [leftXp, rightXp]

topXp `above` bottomXp = col [topXp, bottomXp]


withInh xp inf =  with_ xp (\(i,s) -> (inf i, s))
withSyn xp snf =  with_ xp (\(i,s) -> (i, snf s))

withColor :: Xprez node -> Color -> Xprez node
withColor xp c = withInh xp (\i -> i { textColor = c, lineColor = c})

withtxtColor :: Xprez node -> Color -> Xprez node
withtxtColor xp c = withInh xp (\i -> i { textColor = c})

withtxtColor_ :: Xprez node -> (Color -> Color) -> Xprez node
withtxtColor_ xp f = withInh xp (\i -> i { textColor = f (textColor i)})

withlnColor :: Xprez node -> Color -> Xprez node
withlnColor xp c = withInh xp (\i -> i { lineColor = c})

withfColor :: Xprez node -> Color -> Xprez node
withfColor xp c = withInh xp (\i -> i { fillColor = c})

withbgColor :: Xprez node -> Color -> Xprez node
withbgColor xp c = withInh xp (\i -> i { backgroundColor = c})

withbgColor_ :: Xprez node -> (Color -> Color) -> Xprez node
withbgColor_ xp f = withInh xp (\i -> i { backgroundColor = f (backgroundColor i)})

withFontSize :: Xprez node -> Int -> Xprez node
withFontSize xp fs = withInh xp (\i -> i { font = (font i) { fSize = fs }})

withFontFam :: Xprez node -> String -> Xprez node
withFontFam xp ff = withInh xp (\i -> i { font = (font i) { fFamily = ff }})

withFontSize_ :: Xprez node -> (Int -> Int) -> Xprez node
withFontSize_ xp ffs = withInh xp (\i -> i { font = (font i) { fSize = ffs (fSize (font i)) }})

withFont :: Xprez node -> Font -> Xprez node
withFont xp f = withInh xp (\i -> i { font = f })

withFont_ :: Xprez node -> (Font -> Font) -> Xprez node
withFont_ xp ff = withInh xp (\i -> i { font = ff (font i) })

-- just set Family and Size
withFont' :: Xprez node -> (String, Int) -> Xprez node
withFont' xp (ff,fs) = withFont_ xp (\f -> f { fFamily = ff, fSize = fs })

bold :: Xprez node -> Xprez node
bold xp = withFont_ xp (\f -> f { fBold = True })

italic :: Xprez node -> Xprez node
italic xp = withFont_ xp (\f -> f { fItalic = True })

underline :: Xprez node -> Xprez node
underline xp = withFont_ xp (\f -> f { fUnderline = True })

strikeOut :: Xprez node -> Xprez node
strikeOut xp = withFont_ xp (\f -> f { fStrikeOut = True })


withMouseDown :: Xprez node -> UpdateDoc -> Xprez node
withMouseDown xp upd = withInh xp (\i -> i { mouseDown = Just upd })

withPopupMenuItems :: Xprez node -> [PopupMenuItem] -> Xprez node
withPopupMenuItems xp mis = withInh xp (\i -> i { popupMenuItems = mis })

withPopupMenuItems_ :: Xprez node -> ([PopupMenuItem] -> [PopupMenuItem]) -> Xprez node
withPopupMenuItems_ xp fmis = withInh xp (\i -> i { popupMenuItems = fmis (popupMenuItems i) })

addPopupItems :: Xprez node -> [PopupMenuItem] -> Xprez node
addPopupItems xp mis = withPopupMenuItems_ xp (\pmis -> mis++pmis) 

withHRef :: Xprez node -> Int -> Xprez node
withHRef xp h = withSyn xp (\s -> s { hRef = h })

withHRef_ :: Xprez node -> (Int -> Int) -> Xprez node
withHRef_ xp fh = withSyn xp (\s -> s { hRef = fh (hRef s) })

withVRef :: Xprez node -> Int -> Xprez node
withVRef xp v = withSyn xp (\s -> s { vRef = v })

withVRef_ :: Xprez node -> (Int -> Int) -> Xprez node
withVRef_ xp fw = withSyn xp (\s -> s { vRef = fw (vRef s) })

withRef :: Xprez node -> (Int, Int) -> Xprez node
withRef xp (h,v) = withSyn xp (\s -> s { hRef = h, vRef = v })

withRef_ :: Xprez node -> ((Int, Int) -> (Int, Int)) -> Xprez node
withRef_ xp fhw = withSyn xp (\s -> let (h,v) = fhw (hRef s, vRef s)
                                     in  s { hRef = h, vRef = v })

withHStretch :: Xprez node -> Bool -> Xprez node
withHStretch xp hs = withSyn xp (\s -> s { hStretch = hs })

withVStretch :: Xprez node -> Bool -> Xprez node
withVStretch xp vs = withSyn xp (\s -> s { vStretch = vs })

withWidth :: Xprez node -> Int -> Xprez node
withWidth xp w = withSyn xp (\s -> s { minWidth = w, hStretch = False })

withHeight :: Xprez node -> Int -> Xprez node
withHeight xp h = withSyn xp (\s -> s { minHeight = h, vStretch = False })

withSize :: Xprez node -> (Int, Int) -> Xprez node
withSize xp (w,h) = withSyn xp (\s -> s { minWidth = w, hStretch = False 
                                        , minHeight = h, vStretch = False  })


move x y xp = xp `withRef_` (\(h,v)-> (h-y, v-x))
                 `with_` (\(i,s) -> let i' = i { assignedHRef = assignedHRef i + y
                                               , assignedVRef = assignedVRef i + x
                                               }
                                        s' = s { finalHRef = finalHRef s-y
                                               , finalVRef = finalVRef s -x
                                               }
                                    in (i',s'))



hLine :: Xprez node
hLine = poly [(0,0),(1.0,0)] `withHeight` 1 

vLine :: Xprez node
vLine = poly [(0,0),(0,1.0)] `withWidth` 1

-- lineWidth should be an attribute, so we can use a with here
hLineW :: Int -> Xprez node
hLineW lw = polyW lw [(0,0),(1.0,0)] `withHeight` 1

vLineW :: Int -> Xprez node
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


refHalf xp = xp `with_` (\(i,s) -> let refdif = hRef s - assignedHeight i `div` 2
                                       newI = i { assignedHRef = assignedHRef i + refdif
                                                }
                                       newS = s {hRef = hRef s - refdif -- assignedHeight i `div` 2
                                                , finalHRef = finalHRef s - refdif
                                                }
                                                 
                                    in (newI,newS))


-- multiply x with a percentage
percent :: Int -> Int -> Int
percent a x = a * x `div` 100

