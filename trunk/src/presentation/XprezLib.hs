module XprezLib where

import CommonTypes


import DocTypes
import PresTypes

-- switch href and vref href is y and vref is x, so (vref, href) is more logical

type Xprez doc node = Presentation doc node
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

withColor :: Xprez doc node -> Color -> Xprez doc node
withColor xp c = withInh xp (\i -> i { textColor = c, lineColor = c})

withtxtColor :: Xprez doc node -> Color -> Xprez doc node
withtxtColor xp c = withInh xp (\i -> i { textColor = c})

withtxtColor_ :: Xprez doc node -> (Color -> Color) -> Xprez doc node
withtxtColor_ xp f = withInh xp (\i -> i { textColor = f (textColor i)})

withlnColor :: Xprez doc node -> Color -> Xprez doc node
withlnColor xp c = withInh xp (\i -> i { lineColor = c})

withfColor :: Xprez doc node -> Color -> Xprez doc node
withfColor xp c = withInh xp (\i -> i { fillColor = c})

withbgColor :: Xprez doc node -> Color -> Xprez doc node
withbgColor xp c = withInh xp (\i -> i { backgroundColor = c})

withbgColor_ :: Xprez doc node -> (Color -> Color) -> Xprez doc node
withbgColor_ xp f = withInh xp (\i -> i { backgroundColor = f (backgroundColor i)})

withFontSize :: Xprez doc node -> Int -> Xprez doc node
withFontSize xp fs = withInh xp (\i -> i { font = (font i) { fSize = fs }})

withFontFam :: Xprez doc node -> String -> Xprez doc node
withFontFam xp ff = withInh xp (\i -> i { font = (font i) { fFamily = ff }})

withFontSize_ :: Xprez doc node -> (Int -> Int) -> Xprez doc node
withFontSize_ xp ffs = withInh xp (\i -> i { font = (font i) { fSize = ffs (fSize (font i)) }})

withFont :: Xprez doc node -> Font -> Xprez doc node
withFont xp f = withInh xp (\i -> i { font = f })

withFont_ :: Xprez doc node -> (Font -> Font) -> Xprez doc node
withFont_ xp ff = withInh xp (\i -> i { font = ff (font i) })

-- just set Family and Size
withFont' :: Xprez doc node -> (String, Int) -> Xprez doc node
withFont' xp (ff,fs) = withFont_ xp (\f -> f { fFamily = ff, fSize = fs })

bold :: Xprez doc node -> Xprez doc node
bold xp = withFont_ xp (\f -> f { fBold = True })

italic :: Xprez doc node -> Xprez doc node
italic xp = withFont_ xp (\f -> f { fItalic = True })

underline :: Xprez doc node -> Xprez doc node
underline xp = withFont_ xp (\f -> f { fUnderline = True })

strikeOut :: Xprez doc node -> Xprez doc node
strikeOut xp = withFont_ xp (\f -> f { fStrikeOut = True })


withMouseDown :: Xprez doc node -> UpdateDoc doc -> Xprez doc node
withMouseDown xp upd = withInh xp (\i -> i { mouseDown = Just upd })

withPopupMenuItems :: Xprez doc node -> [PopupMenuItem doc] -> Xprez doc node
withPopupMenuItems xp mis = withInh xp (\i -> i { popupMenuItems = mis })

withPopupMenuItems_ :: Xprez doc node -> ([PopupMenuItem doc] -> [PopupMenuItem doc]) -> Xprez doc node
withPopupMenuItems_ xp fmis = withInh xp (\i -> i { popupMenuItems = fmis (popupMenuItems i) })

addPopupItems :: Xprez doc node -> [PopupMenuItem doc] -> Xprez doc node
addPopupItems xp mis = withPopupMenuItems_ xp (\pmis -> mis++pmis) 

withHRef :: Xprez doc node -> Int -> Xprez doc node
withHRef xp h = withSyn xp (\s -> s { hRef = h })

withHRef_ :: Xprez doc node -> (Int -> Int) -> Xprez doc node
withHRef_ xp fh = withSyn xp (\s -> s { hRef = fh (hRef s) })

withVRef :: Xprez doc node -> Int -> Xprez doc node
withVRef xp v = withSyn xp (\s -> s { vRef = v })

withVRef_ :: Xprez doc node -> (Int -> Int) -> Xprez doc node
withVRef_ xp fw = withSyn xp (\s -> s { vRef = fw (vRef s) })

withRef :: Xprez doc node -> (Int, Int) -> Xprez doc node
withRef xp (h,v) = withSyn xp (\s -> s { hRef = h, vRef = v })

withRef_ :: Xprez doc node -> ((Int, Int) -> (Int, Int)) -> Xprez doc node
withRef_ xp fhw = withSyn xp (\s -> let (h,v) = fhw (hRef s, vRef s)
                                     in  s { hRef = h, vRef = v })

withHStretch :: Xprez doc node -> Bool -> Xprez doc node
withHStretch xp hs = withSyn xp (\s -> s { hStretch = hs })

withVStretch :: Xprez doc node -> Bool -> Xprez doc node
withVStretch xp vs = withSyn xp (\s -> s { vStretch = vs })

withWidth :: Xprez doc node -> Int -> Xprez doc node
withWidth xp w = withSyn xp (\s -> s { minWidth = w, hStretch = False })

withHeight :: Xprez doc node -> Int -> Xprez doc node
withHeight xp h = withSyn xp (\s -> s { minHeight = h, vStretch = False })

withSize :: Xprez doc node -> (Int, Int) -> Xprez doc node
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



hLine :: Xprez doc node
hLine = poly [(0,0),(1.0,0)] `withHeight` 1 

vLine :: Xprez doc node
vLine = poly [(0,0),(0,1.0)] `withWidth` 1

-- lineWidth should be an attribute, so we can use a with here
hLineW :: Int -> Xprez doc node
hLineW lw = polyW lw [(0,0),(1.0,0)] `withHeight` 1

vLineW :: Int -> Xprez doc node
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

