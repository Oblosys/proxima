module XLatex where

import XprezLib
import PresTypes
import CommonTypes

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
  polyW 2 [(0.9,0.1),(0.9,0),(0,0),(0.5,0.5),(0,1),(0.9,1),(0.9,0.9)]
  `with_` (\(i,s) -> let i' = i 
                         s' = s { minWidth = (assignedHeight i*7`div`10), hStretch = True }
                     in  (i',s'))


rootSym = 
  polyW 2 [(0.0,0.6),(0.1,0.6),(0.4,1.0),(1.0,0.0)] 
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

centerline xp = xp `with_` (\(i,s) -> let i' = i { assignedHRef = assignedHRef i - cmttex i
                                                 }
                                          s' = s { hRef = hRef s + cmttex i 
                                                 , finalHRef = finalHRef s + cmttex i
                                                   -- , fontSize = fontSize s + 2
                                                 } -- great error when i field update is added, eg fontSize
                                    
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





slide title body = overlay [
                 move 0 30 $
                  col [ hAlignCenter $ title `withColor` white
                                                    `withFont'` ("cmr10", 20) 
                      , row [ hSpace 20, body `withbgColor` blue ]
                      ] `withHStretch` True
                      
                , rect 500 300 `withfColor` blue `withColor` blue
                ] `withColor` yellow `withbgColor` blue `withFont'` ("Arial", 15)




{-
-- tree:
--               isExp isLast
l1 = mkTreeLeaf        False (text "leaf 1")
n2 = mkTreeNode False False (text "node 2" `withFontSize` 50) []
l3 = mkTreeLeaf        True  (text "leaf 3")
tr1 = mkTreeNode True False (img "img/yahoo.bmp" `withSize` (134,38)) [l1, n2, l3]
l4 = mkTreeLeaf        False (text "leaf 4")
l5 = mkTreeLeaf        False (text "leaf 5")
l6 = mkTreeLeaf        True  (text "leaf 6")
                           -- (boxed testcase `withFontSize_` (`div` 2))
tree = mkTreeNode True True (text "node") [l4, tr1, l6]
-}
-------------------------------------------------------------------------
mkTreeLeaf :: Bool -> Xprez -> Xprez
mkTreeLeaf isLast label = 
  row [ leafHandle isLast, empty {-hLine-} `withWidth` 12, leafImg
      , empty {-hLine-} `withWidth` 5, refHalf label ] 

mkTreeNode :: Bool -> Bool -> Xprez -> [ Xprez ] -> Xprez
mkTreeNode isLast isExp label children =
  rowR 1 [ nodeHandle isExp isLast, hLine `withWidth` 7
         , col $ [ row [ col [ nodeImg , if isExp then empty {-vLine-} else empty ]
                       , empty {-hLine-} `withWidth` 5,refHalf label 
                       ] 
                 ] ++ (if isExp then children else [] )
         ]

nodeHandle isExp isLast 
 = colR 1 ([ empty{-vLine-}, rowR 1 [ empty  `withWidth` 2, handleImg isExp] ]++ if isLast then [] else [empty{- vLine-}])

leafHandle isLast 
 = colR 1 ([empty{-vLine-}, empty `withSize` (9,9) `withRef` (4,4)]++ if isLast then [] else [empty{-vLine-}])

handleImg isExp = if isExp then minusImg else plusImg

nodeImg = img "img/folder.bmp" `withSize` (15,13) `withRef` (7,7)

leafImg = img "img/help.bmp" `withSize` (16,16) `withRef` (7,6)

plusImg = img "img/plus.bmp" `withSize` (9,9) `withRef` (4,4)

minusImg = img "img/minus.bmp" `withSize` (9,9) `withRef` (4,4)
