module Presentation.XLatex where

import Presentation.XprezLib
import Presentation.PresTypes
import Common.CommonTypes

import Char

fontsize = 20 :: Int

tex xp = xp `withFont'` ("CMR12", fontsize)

latex =
  overlay [ text"L" `withFontSize` 24
          , move 7 (-5) $ text "A" `withFontSize` 17
          , move 17 0 $ text"T" `withFontSize` 24
          , move 29 5 $ text"E" `withFontSize` 24
          , move 43 0 $ text"X" `withFontSize` 24
          ] `withFontFam` "CMR10"

variable str = ttext str `withFontFam` "CMMI12"

bold xp = xp `withFontFam` "CMBX12"

math xp = centerline xp

plusSym = text "+"
minusSym = text "\161" `withFontFam` "CMSY10" `withHeight` fontsize
multiplySym = text "\162" `withFontFam` "CMSY10" `withHeight` fontsize
timesSym = text "\163" `withFontFam` "CMSY10" `withHeight` fontsize
eqSym = text "="
ltSym = text "<" `withFontFam` "CMCSC10" `withHeight` fontsize
gtSym = text ">" `withFontFam` "CMCSC10" `withHeight` fontsize
leSym = text "\183" `withFontFam` "CMSY10" `withHeight` fontsize
geSym = text "\184" `withFontFam` "CMSY10" `withHeight` fontsize
rightArrowSym = text "\33" `withFontFam` "CMSY10" `withHeight` fontsize
infinitySym = text "\49" `withFontFam` "CMSY10" `withHeight` fontsize
deltaSym = text "\162" `withFontFam` "CMR12" `withHeight` fontsize
gammaSym = text "\176"  `withFontFam` "CMMI6" `withHeight` fontsize
sigmaSym = move 0 7 $ text "\167"  `withFont'` ("CMR12", 120 `percent` fontsize) `withHeight` fontsize
rSym = text "R" `withFontFam` "CMBX6" `withHeight` fontsize

sigma =
  polyW 2 [(0.9,0.1),(0.9,0),(0,0),(0.5,0.5),(0,1),(0.9,1),(0.9,0.9)] Transparent
  `with_` (\(i,s) -> let i' = i
                         s' = s { minWidth = (assignedHeight i*7`div`10), hStretch = True }
                     in  (i',s'))


rootSym =
  polyW 2 [(0.0,0.6),(0.1,0.6),(0.4,1.0),(1.0,0.0)] Transparent
  `with_` (\(i,s) -> let i' = i
                         s' = s { minWidth = (assignedHeight i*5`div`10), hStretch = False }
                     in  (i',s'))


allchars = col [line i (i+31) | i <- [32,64..224]]
 where line b e = rowR 1 [text (show b++":") `withFontFam` "courier new", text [chr b..chr e] ]


frac e1 e2 = let numerator   = hAlignCenter (shrink e1)
                 bar         = hLine
                 denominator = hAlignCenter (shrink e2)
             in centerline $ row [ hSpace 2, colR 1 [ numerator, bar, denominator] `withHStretch` False
                                 , hSpace 2 ]

equals e1 e2 = row [ e1, hSpace 4, eqSym, hSpace 4, e2]

plus e1 e2 = row [ e1, hSpace 4, plusSym, hSpace 4, e2]

dif e1 e2 = row [ e1, hSpace 4, minusSym, hSpace 4, e2]

prod e1 e2 = row [ e1, hSpace 4, multiplySym, hSpace 4, e2]

power xp exp = row [ xp, move 0 (-12) $ shrink exp
                               ]
subscript exp sup = row [ exp, hSpace 1, move 0 5 $ shrink $ sup ]
--  where subHRef i = - (fontSize i * 1 `div` 6)
--        ex i = fontSize i `div` 3

superscript exp sup = row [ exp, hSpace 1, move 0 (-10) $ shrink $ sup ]
--  where superHRef i = fontSize i * 1 `div` 3
--        ex i = fontSize i `div` 3

subsuperscript exp sub sup = row [ exp, hSpace 1, overlay [ move 0 (-10) $ shrink $ sup
                                                  , move 0 (7) $ shrink $ sub
                                                  ]
                                   ]
--  where superHRef i = fontSize i * 1 `div` 3
--        ex i = fontSize i `div` 3

centerline xp = xp `with_` (\(i,s) -> let i' = i { assignedVRef = assignedVRef i - cmttex i
                                                 }
                                          s' = s { vRef = vRef s + cmttex i
                                                 , finalVRef = finalVRef s + cmttex i
                                                 }

                                                   in (i',s'))

cmttex i = fSize (font i) * 185 `div` 600

summation from to exp = row [ subsuperscript sigmaSym from to, hSpace 4, exp]

sqRoot xp = rootSym `beside` colR 2 [hLine, empty `withHeight` 2, empty `withWidth` 2 `beside` xp]

shrink e = e `withFontSize_` (\fs -> round(fromIntegral fs*0.7) `max` 5)


scaleFont xp percentage = xp `withFontSize_` (\fs -> fs * percentage `div` 100)

ttext str = colR 1 [ vSpace 2
                   , text . mapLigatures . map (\c -> if c == ' ' then chr 160 else c) $ str
                   , vSpace 1
                   ]

mapLigatures []  = []
mapLigatures [c] = [c]
mapLigatures ('f':'f':cs) = ffChar : mapLigatures cs
mapLigatures ('f':'i':cs) = fiChar : mapLigatures cs
mapLigatures ('f':'l':cs) = flChar : mapLigatures cs
mapLigatures (c:cs)       = c : mapLigatures cs

ffChar = chr 174
fiChar = chr 175
flChar = chr 176



presType :: String -> Xprez doc node clip token
presType tpStr = row $ intersperse rightArrow (map text (splitAtArrows "" tpStr))

rightArrow :: Xprez doc node clip token
rightArrow = text  "\174" `withFontFam` "symbol"


splitAtArrows :: String -> String -> [String]
splitAtArrows seg []           = [seg]
splitAtArrows seg [c]          = [seg++[c]]
splitAtArrows seg ('-':'>':cs) = seg : splitAtArrows [] cs
splitAtArrows seg (c:cs)       = splitAtArrows (seg++[c]) cs

presMsg :: String -> Xprez doc node clip token
presMsg tpStr = row $ intersperse wok (map text (splitAtWoks "" tpStr))

wok :: Xprez doc node clip token
wok = move 0 (-6) $ (shrink . shrink) (text  "\200" `withFontFam` "mt symbol")


splitAtWoks :: String -> String -> [String]
splitAtWoks seg []           = [seg]
splitAtWoks seg [c]          = [seg++[c]]
splitAtWoks seg ('^':'o':cs) = seg : splitAtWoks [] cs
splitAtWoks seg (c:cs)       = splitAtWoks (seg++[c]) cs

slide title body = overlay [
                 move 0 30 $
                  col [ hAlignCenter $ title `withColor` white
                                                    `withFont'` ("cmr10", 20)
                      , row [ hSpace 20, body `withbgColor` myBlue ]
                      ] `withHStretch` True

                , rect 500 300 Solid `withfColor` myBlue `withColor` myBlue
                ] `withColor` yellow `withbgColor` myBlue `withFont'` ("Arial", 15)
  where myBlue = (0,0,192)



trroot  = mkTreeNode True  True  (text "food") [trnode1, trnode2, trleaf6]
trnode1 = mkTreeNode False True  (text "fruit") [trleaf1]
trleaf1 = mkTreeLeaf True        (text "peach")
trnode2 = mkTreeNode False True (text "vegetables") [trnode3,trleaf2,trnode4]
trnode3 = mkTreeNode False False (text "cabbage") []
trleaf2 = mkTreeLeaf False (text "spinach")
trnode4 = mkTreeNode True  True  (row[text "citrus"] `withbgColor` lightGrey) [trleaf3,trleaf4]
trleaf3 = mkTreeLeaf False (text "orange")
trleaf4 = mkTreeLeaf True  (text "lemon")
trleaf6 = mkTreeLeaf True  (text "snack")

trrroot  = mkTreeNode True  True  (text "food") [trrnode1, trrnode2, trrleaf6]
trrnode1 = mkTreeNode False True  (text "fruit") [trrnode4, trrleaf1]
trrleaf1 = mkTreeLeaf True        (text "peach")
trrnode2 = mkTreeNode False True (text "vegetables") [trrnode3,trrleaf2]
trrnode3 = mkTreeNode False False (text "cabbage") []
trrleaf2 = mkTreeLeaf True (text "spinach")
trrnode4 = mkTreeNode False  True  (row[text "citrus"] `withbgColor` lightGrey) [trrleaf3,trrleaf4]
trrleaf3 = mkTreeLeaf False (text "orange")
trrleaf4 = mkTreeLeaf True  (text "lemon")
trrleaf6 = mkTreeLeaf True  (text "snack")

modroot  = mkTreeNode True  True  (text "Proxima") [commonNd,evalNd,presentNd,layoutNd,modNd1,modNd2,mainNd]
commonNd = mkTreeNode False False  (text "Common") []
evalNd = mkTreeNode False False  (text "Evaluation") []
modLf2   = mkTreeLeaf True        (text "EvaluateTypes")
presentNd = mkTreeNode False True  (text "Presentation") [typesLf, utilsLf, presentLf, interpretLf, modLf1,modLf3]
typesLf = mkTreeLeaf False        (text "Types")
utilsLf = mkTreeLeaf False        (text "Utils")
interpretLf = mkTreeLeaf False        (text "Interpret")
presentLf   = mkTreeLeaf False        (text "Present")
modLf1   = mkTreeLeaf False        (row[text "XprezLib"] `withbgColor` lightGrey)
modLf3   = mkTreeLeaf True        (text "Parser")
layoutNd = mkTreeNode False False  (text "Layout") [presentLf, interpretLf]
modNd1 = mkTreeNode False False  (text "Arrangement") []
modNd2 = mkTreeNode False False  (text "Rendering") []
mainNd =  mkTreeNode True True  (text "Main") [modLf4,modLf5,modLf6]
modLf4   = mkTreeLeaf False        (text "Architecture")
modLf5   = mkTreeLeaf False        (text "Gui")
modLf6   = mkTreeLeaf True        (text "Main")
-- tree:
--               isLast isExp
l1  = mkTreeLeaf False       (text "leaf 1")
n2  = mkTreeNode True  False (text "leafnode 2") [] --  `withFontSize` 50) []
n3  = mkTreeNode False False (text "node 3") []
l3  = mkTreeLeaf True        (text "leaf 3")
tr1 = mkTreeNode False True  (img "img/yahoo.bmp" `withSize` (134,38)) [l1, n3, l3]
l4  = mkTreeLeaf False       (text "leaf 4")
l5  = mkTreeLeaf False       (text "leaf 5")
l6  = mkTreeLeaf False       (text "leaf 6")
                          -- (boxed testcase `withFontSize_` (`div` 2))
myTree = row [ trroot, hSpace 10, trrroot, hSpace 10, modroot ] -- mkTreeNode True True (text "root") [l4, tr1, l6, n2]

