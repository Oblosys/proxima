module XprezLib where

import CommonTypes


import DocTypes
import PresTypes

-- switch href and vref href is y and vref is x, so (vref, href) is more logical

type Xprez = Presentation
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

withColor :: Xprez -> Color -> Xprez
withColor xp c = withInh xp (\i -> i { textColor = c, lineColor = c})

withtxtColor :: Xprez -> Color -> Xprez
withtxtColor xp c = withInh xp (\i -> i { textColor = c})

withtxtColor_ :: Xprez -> (Color -> Color) -> Xprez
withtxtColor_ xp f = withInh xp (\i -> i { textColor = f (textColor i)})

withlnColor :: Xprez -> Color -> Xprez
withlnColor xp c = withInh xp (\i -> i { lineColor = c})

withfColor :: Xprez -> Color -> Xprez
withfColor xp c = withInh xp (\i -> i { fillColor = c})

withbgColor :: Xprez -> Color -> Xprez
withbgColor xp c = withInh xp (\i -> i { backgroundColor = c})

withbgColor_ :: Xprez -> (Color -> Color) -> Xprez
withbgColor_ xp f = withInh xp (\i -> i { backgroundColor = f (backgroundColor i)})

withFontSize :: Xprez -> Int -> Xprez
withFontSize xp fs = withInh xp (\i -> i { font = (font i) { fSize = fs }})

withFontFam :: Xprez -> String -> Xprez
withFontFam xp ff = withInh xp (\i -> i { font = (font i) { fFamily = ff }})

withFontSize_ :: Xprez -> (Int -> Int) -> Xprez
withFontSize_ xp ffs = withInh xp (\i -> i { font = (font i) { fSize = ffs (fSize (font i)) }})

withFont :: Xprez -> Font -> Xprez
withFont xp f = withInh xp (\i -> i { font = f })

withFont_ :: Xprez -> (Font -> Font) -> Xprez
withFont_ xp ff = withInh xp (\i -> i { font = ff (font i) })

-- just set Family and Size
withFont' :: Xprez -> (String, Int) -> Xprez
withFont' xp (ff,fs) = withFont_ xp (\f -> f { fFamily = ff, fSize = fs })

bold :: Xprez -> Xprez
bold xp = withFont_ xp (\f -> f { fBold = True })

italic :: Xprez -> Xprez
italic xp = withFont_ xp (\f -> f { fItalic = True })

underline :: Xprez -> Xprez
underline xp = withFont_ xp (\f -> f { fUnderline = True })

strikeOut :: Xprez -> Xprez
strikeOut xp = withFont_ xp (\f -> f { fStrikeOut = True })


withMouseDown :: Xprez -> UpdateDoc -> Xprez
withMouseDown xp upd = withInh xp (\i -> i { mouseDown = Just upd })

withPopupMenuItems :: Xprez -> [PopupMenuItem] -> Xprez
withPopupMenuItems xp mis = withInh xp (\i -> i { popupMenuItems = mis })

withPopupMenuItems_ :: Xprez -> ([PopupMenuItem] -> [PopupMenuItem]) -> Xprez
withPopupMenuItems_ xp fmis = withInh xp (\i -> i { popupMenuItems = fmis (popupMenuItems i) })

addPopupItems :: Xprez -> [PopupMenuItem] -> Xprez
addPopupItems xp mis = withPopupMenuItems_ xp (\pmis -> mis++pmis) 

withHRef :: Xprez -> Int -> Xprez
withHRef xp h = withSyn xp (\s -> s { hRef = h })

withHRef_ :: Xprez -> (Int -> Int) -> Xprez
withHRef_ xp fh = withSyn xp (\s -> s { hRef = fh (hRef s) })

withVRef :: Xprez -> Int -> Xprez
withVRef xp v = withSyn xp (\s -> s { vRef = v })

withVRef_ :: Xprez -> (Int -> Int) -> Xprez
withVRef_ xp fw = withSyn xp (\s -> s { vRef = fw (vRef s) })

withRef :: Xprez -> (Int, Int) -> Xprez
withRef xp (h,v) = withSyn xp (\s -> s { hRef = h, vRef = v })

withRef_ :: Xprez -> ((Int, Int) -> (Int, Int)) -> Xprez
withRef_ xp fhw = withSyn xp (\s -> let (h,v) = fhw (hRef s, vRef s)
                                     in  s { hRef = h, vRef = v })

withHStretch :: Xprez -> Bool -> Xprez
withHStretch xp hs = withSyn xp (\s -> s { hStretch = hs })

withVStretch :: Xprez -> Bool -> Xprez
withVStretch xp vs = withSyn xp (\s -> s { vStretch = vs })

withWidth :: Xprez -> Int -> Xprez
withWidth xp w = withSyn xp (\s -> s { minWidth = w, hStretch = False })

withHeight :: Xprez -> Int -> Xprez
withHeight xp h = withSyn xp (\s -> s { minHeight = h, vStretch = False })

withSize :: Xprez -> (Int, Int) -> Xprez
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



hLine :: Xprez
hLine = poly [(0,0),(1.0,0)] `withHeight` 1 

vLine :: Xprez
vLine = poly [(0,0),(0,1.0)] `withWidth` 1

-- lineWidth should be an attribute, so we can use a with here
hLineW :: Int -> Xprez
hLineW lw = polyW lw [(0,0),(1.0,0)] `withHeight` 1

vLineW :: Int -> Xprez
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

