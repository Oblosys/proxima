module Presentation.XprezLib where

import Common.CommonTypes
import Evaluation.DocTypes
import Evaluation.DocumentEdit
import Presentation.PresTypes
import Presentation.PresentationParsing
import Maybe

-- switch href and vref href is y and vref is x, so (vref, href) is more logical

type Xprez doc node clip token = Presentation doc node clip token
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
--token = TokenP NoIDP
row = RowP NoIDP 0
rowR r = RowP NoIDP r
col = ColP NoIDP 0 NF
colR r = ColP NoIDP r NF
overlay = OverlayP NoIDP 
--matrix = Matrix 
rect w h = RectangleP NoIDP w h 1
ellipse w h = EllipseP NoIDP w h 1
img src = ImageP NoIDP src Tile
poly pts = PolyP NoIDP pts 1
polyW w pts = PolyP NoIDP pts w
formatter xps = FormatterP NoIDP xps
--alt xps = Alternative xps
with_ xp f = WithP f xp
structural xp = StructuralP NoIDP xp
parsing xp = ParsingP NoIDP Nothing LexInherited xp
parsing' l xp = ParsingP NoIDP Nothing l xp

parsingWithParser :: (Editable a doc node clip token, DocNode node, Ord token, Show token) =>
                     ListParser doc node clip token a -> a -> Presentation doc node clip token ->
                     Presentation doc node clip token
parsingWithParser parser self pres = ParsingP NoIDP (Just $ mkClipParser $ parser) LexInherited pres
loc l xp  = LocatorP l xp

graph :: Int -> Int -> [(Int,Int)] -> [Xprez doc node clip token] -> Xprez doc node clip token
graph width height edges vertexPress = 
  GraphP NoIDP Clean width height edges vertexPress
 
vertex :: Int -> Int -> Int -> Outline -> Xprez doc node clip token -> Xprez doc node clip token
vertex id x y outline pres = VertexP NoIDP id x y outline pres


-- used for putting structural presentations inside parsing presentations
structuralToken idp pres = TokenP idp (StructuralTk 0 Nothing (StructuralP idp pres) [] NoIDP)
-- we duplicate the idp: TokenP needs it, so detokenize can add the correct whitespace
-- And since TokenP and StructuralTk are removed by the layouter, we need it in the StructuralP
-- as well, so it can be recovered by the scanner
-- It is not added to StructuralTk, whose idp field is only used during scanning.

presHole focus typeStr nd pth = loc nd $
  structural $ row [text $ "{"++typeStr++"}"] `withColor` black `withbgColor` yellow `withFontFam` ("Courier New")

presParseErr node (StructuralParseErr pres) =
  loc node $ structural $ pres `withbgColor` whiteSmoke
presParseErr node (ParsingParseErr parseErrs tokens parser) =
  loc node $ ParsingP NoIDP (Just parser) LexInherited $ 
     row $ [ (case lookup i positionsAndErrors of
               Nothing -> id
               Just msg -> markError msg 
             )  $ presFromToken token
           | (i,token) <- zip [0..] tokens ]
           ++ if null finalErrors
              then []
              else [markError (unlines finalErrors) $ row [StringP NoIDP "" `withWidth` 8 ] ]
 where markError str pres = squiggly red pres `addPopupItems` [(str,id)] -- use popup until we have tooltips
       positionsAndErrors = catMaybes [ case mPos of
                                          Just p -> Just (p, msg)
                                          Nothing -> Nothing
                                      | (mPos, msg) <- parseErrs 
                                      ]
       finalErrors = map snd $ filter (isNothing . fst) parseErrs 
-- probably we need the lexer also in ParseErr


presFromToken (StructuralTk _ (Just node) pres _ idp) = loc node $ structuralToken idp $ pres
presFromToken token                                   = TokenP (tokenIDP token) token 




leftXp `beside` rightXp = row [leftXp, rightXp]

topXp `above` bottomXp = col [topXp, bottomXp]


withInh xp inf =  with_ xp (\(i,s) -> (inf i, s))
withSyn xp snf =  with_ xp (\(i,s) -> (i, snf s))

withColor :: Xprez doc node clip token -> Color -> Xprez doc node clip token
withColor xp c = withInh xp (\i -> i { textColor = c, lineColor = c})

withtxtColor :: Xprez doc node clip token -> Color -> Xprez doc node clip token
withtxtColor xp c = withInh xp (\i -> i { textColor = c})

withtxtColor_ :: Xprez doc node clip token -> (Color -> Color) -> Xprez doc node clip token
withtxtColor_ xp f = withInh xp (\i -> i { textColor = f (textColor i)})

withlnColor :: Xprez doc node clip token -> Color -> Xprez doc node clip token
withlnColor xp c = withInh xp (\i -> i { lineColor = c})

withfColor :: Xprez doc node clip token -> Color -> Xprez doc node clip token
withfColor xp c = withInh xp (\i -> i { fillColor = c})

withbgColor :: Xprez doc node clip token -> Color -> Xprez doc node clip token
withbgColor xp c = withInh xp (\i -> i { backgroundColor = c})

withbgColor_ :: Xprez doc node clip token -> (Color -> Color) -> Xprez doc node clip token
withbgColor_ xp f = withInh xp (\i -> i { backgroundColor = f (backgroundColor i)})

withFontSize :: Xprez doc node clip token -> Int -> Xprez doc node clip token
withFontSize xp fs = withInh xp (\i -> i { font = (font i) { fSize = fs }})

withFontFam :: Xprez doc node clip token -> String -> Xprez doc node clip token
withFontFam xp ff = withInh xp (\i -> i { font = (font i) { fFamily = ff }})

withFontSize_ :: Xprez doc node clip token -> (Int -> Int) -> Xprez doc node clip token
withFontSize_ xp ffs = withInh xp (\i -> i { font = (font i) { fSize = ffs (fSize (font i)) }})

withFont :: Xprez doc node clip token -> Font -> Xprez doc node clip token
withFont xp f = withInh xp (\i -> i { font = f })

withFont_ :: Xprez doc node clip token -> (Font -> Font) -> Xprez doc node clip token
withFont_ xp ff = withInh xp (\i -> i { font = ff (font i) })

-- just set Family and Size
withFont' :: Xprez doc node clip token -> (String, Int) -> Xprez doc node clip token
withFont' xp (ff,fs) = withFont_ xp (\f -> f { fFamily = ff, fSize = fs })

bold :: Xprez doc node clip token -> Xprez doc node clip token
bold xp = withFont_ xp (\f -> f { fBold = True })

italic :: Xprez doc node clip token -> Xprez doc node clip token
italic xp = withFont_ xp (\f -> f { fItalic = True })

underline :: Xprez doc node clip token -> Xprez doc node clip token
underline xp = withFont_ xp (\f -> f { fUnderline = True })

strikeOut :: Xprez doc node clip token -> Xprez doc node clip token
strikeOut xp = withFont_ xp (\f -> f { fStrikeOut = True })


withMouseDown :: Xprez doc node clip token -> UpdateDoc doc clip -> Xprez doc node clip token
withMouseDown xp upd = withInh xp (\i -> i { mouseDown = Just upd })

withPopupMenuItems :: Xprez doc node clip token -> [PopupMenuItem doc clip] -> Xprez doc node clip token
withPopupMenuItems xp mis = withInh xp (\i -> i { popupMenuItems = mis })

withPopupMenuItems_ :: Xprez doc node clip token -> ([PopupMenuItem doc clip] -> [PopupMenuItem doc clip]) -> Xprez doc node clip token
withPopupMenuItems_ xp fmis = withInh xp (\i -> i { popupMenuItems = fmis (popupMenuItems i) })

addPopupItems :: Xprez doc node clip token -> [PopupMenuItem doc clip] -> Xprez doc node clip token
addPopupItems xp mis = withPopupMenuItems_ xp (\pmis -> mis++pmis) 

withHRef :: Xprez doc node clip token -> Int -> Xprez doc node clip token
withHRef xp h = withSyn xp (\s -> s { hRef = h })

withHRef_ :: Xprez doc node clip token -> (Int -> Int) -> Xprez doc node clip token
withHRef_ xp fh = withSyn xp (\s -> s { hRef = fh (hRef s) })

withVRef :: Xprez doc node clip token -> Int -> Xprez doc node clip token
withVRef xp v = withSyn xp (\s -> s { vRef = v })

withVRef_ :: Xprez doc node clip token -> (Int -> Int) -> Xprez doc node clip token
withVRef_ xp fv = withSyn xp (\s -> s { vRef = fv (vRef s) })

withRef :: Xprez doc node clip token -> (Int, Int) -> Xprez doc node clip token
withRef xp (h,v) = withSyn xp (\s -> s { hRef = h, vRef = v })

withRef_ :: Xprez doc node clip token -> ((Int, Int) -> (Int, Int)) -> Xprez doc node clip token
withRef_ xp fhv = withSyn xp (\s -> let (h,v) = fhv (hRef s, vRef s)
                                    in  s { hRef = h, vRef = v })

withHStretch :: Xprez doc node clip token -> Bool -> Xprez doc node clip token
withHStretch xp hs = withSyn xp (\s -> s { hStretch = hs })

withVStretch :: Xprez doc node clip token -> Bool -> Xprez doc node clip token
withVStretch xp vs = withSyn xp (\s -> s { vStretch = vs })

withStretch :: Xprez doc node clip token -> Bool -> Xprez doc node clip token
withStretch xp str = withSyn xp (\s -> s { hStretch = str, vStretch = str })

withWidth :: Xprez doc node clip token -> Int -> Xprez doc node clip token
withWidth xp w = withSyn xp (\s -> s { minWidth = w, hStretch = False })

withHeight :: Xprez doc node clip token -> Int -> Xprez doc node clip token
withHeight xp h = withSyn xp (\s -> s { minHeight = h, vStretch = False })

withSize :: Xprez doc node clip token -> (Int, Int) -> Xprez doc node clip token
withSize xp (w,h) = withSyn xp (\s -> s { minWidth = w, hStretch = False 
                                        , minHeight = h, vStretch = False  })


move x y xp = xp `withRef_` (\(h,v)-> (h-x, v-y))
                 `with_` (\(i,s) -> let i' = i { assignedHRef = assignedHRef i + x
                                               , assignedVRef = assignedVRef i + y
                                               } 
                                        s' = s { finalHRef = finalHRef s -x
                                               , finalVRef = finalVRef s-y
                                               }
                                    in (i',s'))



hLine :: Xprez doc node clip token
hLine = poly [(0,0),(1.0,0)] Transparent `withHeight` 1 

vLine :: Xprez doc node clip token
vLine = poly [(0,0),(0,1.0)] Transparent `withWidth` 1

-- lineWidth should be an attribute, so we can use a with here
hLineW :: Int -> Xprez doc node clip token
hLineW lw = polyW lw [(0,0),(1.0,0)] Transparent `withHeight` 1

vLineW :: Int -> Xprez doc node clip token
vLineW lw = polyW lw [(0,0),(0,1.0)] Transparent `withWidth` 1

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

vRefHalf xp = xp `with_` (\(i,s) -> let refdif = vRef s - assignedHeight i `div` 2
                                        newI = i { assignedVRef = assignedVRef i + refdif
                                                 }
                                        newS = s { vRef = vRef s - refdif -- assignedHeight i `div` 2
                                                 , finalVRef = finalVRef s - refdif
                                                 }
                                                  
                                     in (newI,newS))

glue :: Xprez doc node clip token
glue = empty `withHStretch` True `withVStretch` True

boxed :: Xprez doc node clip token -> Xprez doc node clip token
boxed p = colR 1 [ hLine, rowR 1 [ vLine, p, vLine ], hLine ]

-- multiply x with a percentage
percent :: Int -> Int -> Int
percent a x = a * x `div` 100


presentFocus NoPathD     path pres = pres
presentFocus (PathD pth) path pres = if pth==path then pres `withbgColor` focusCol else pres

focusCol = lightBlue -- lightGrey

squiggly :: Color -> Xprez doc node clip token -> Xprez doc node clip token
squiggly c xp = overlay [xp, img "img/squiggly.png" `withHeight` 3 `withColor` c, empty]
-- png is the red one, only temporary

presentElementXML :: FocusDoc -> node -> [Int] -> String -> [Presentation doc node clip token] -> Presentation doc node clip token
presentElementXML focusD node path tag children =
  loc node $ parsing $ presentFocus focusD path $                  
    if null children
    then col [ text $ "<"++tag++"/>"]
    else col [ text  $ "<"++tag++">"
             , row [ text "  ", col children ]
             , text $ "</"++tag++">" ]      
    

presentElementTree :: FocusDoc -> node -> [Int] -> String -> [Presentation doc node clip token] -> Presentation doc node clip token
presentElementTree focusD node path tag children =
  loc node $ parsing $ presentFocus focusD path $                  
    if null children
    then mkTreeLeaf False $ text $ tag
    else mkTreeNode False True (text tag) children

-------------------------------------------------------------------------
-- change hLine and vLine to empty to get rid of lines
hLine' = hLine -- empty
vLine' = vLine -- empty


mkTreeLeaf :: Bool -> Xprez doc node clip token -> Xprez doc node clip token
mkTreeLeaf isLast label =
  row [ leafHandle isLast, hLine `withWidth` 12, leafImg
      , hLine `withWidth` 5, vRefHalf label ]

mkTreeNode :: Bool -> Bool -> Xprez doc node clip token -> [ Xprez doc node clip token ] -> Xprez doc node clip token
mkTreeNode isLast isExp label children =
  rowR 0 [ nodeHandle isExp isLast, hLine `withWidth` 7
         , col $ [ row [ col [ nodeImg , if isExp then vLine' else empty ]
                       , hLine `withWidth` 5, vRefHalf label
                       ]
                 ] ++ (if isExp then children else [] )
         ]

nodeHandle isExp isLast
 = colR 1 ([ vLine', handleImg isExp ]++ if isLast then [] else [vLine'])          -- old version
-- = colR 1 ([ vLine', rowR 1 [ empty  `withWidth` 2, handleImg isExp] ]++ if isLast then [] else [vLine'])

leafHandle isLast
 = colR 1 ([vLine', empty {-`withSize` (9,9) `withRef` (4,4)-}]++ if isLast then [] else [vLine'])

handleImg isExp = if isExp then minusImg else plusImg

nodeImg = img "img/folder.bmp" `withSize` (15,13) `withRef` (7,7)

leafImg = img "img/text.bmp" `withSize` (13,16) `withRef` (6,7)

plusImg = img "img/plus.bmp" `withSize` (9,9) `withRef` (4,4)

minusImg = img "img/minus.bmp" `withSize` (9,9) `withRef` (4,4)


{-  version that works for built-in tree presentation

hLine' = hLine -- empty
vLine' = vLine -- empty

mkTreeLeaf :: Bool -> Xprez doc node clip token -> Xprez doc node clip token
mkTreeLeaf isLast label =
  row [ leafHandle isLast, hLine `withWidth` 12, leafImg
      , hLine `withWidth` 5, refHalf label ]

mkTreeNode :: Bool -> Bool -> Xprez doc node clip token -> [ Xprez doc node clip token ] -> Xprez doc node clip token
mkTreeNode isLast isExp label children =
  rowR 1 [ nodeHandle isExp isLast, hLine `withWidth` 7
         , col $ [ row [ col [ nodeImg , if isExp then vLine' else empty ]
                       , hLine `withWidth` 5,refHalf label
                       ]
                 ] ++ (if isExp then children else [] )
         ]

nodeHandle isExp isLast
 = colR 1 ([ vLine', handleImg isExp ]++ if isLast then [] else [vLine'])
-- = colR 1 ([ vLine', rowR 1 [ empty  `withWidth` 2, handleImg isExp] ]++ if isLast then [] else [vLine'])

leafHandle isLast
 = colR 1 ([vLine', empty {-`withSize` (9,9) `withRef` (4,4)-}]++ if isLast then [] else [vLine'])

handleImg isExp = if isExp then minusImg else plusImg

nodeImg = img "img/folder.bmp" `withSize` (15,13) `withRef` (7,7)

leafImg = img "img/help.bmp" `withSize` (16,16) `withRef` (6,7)

plusImg = img "img/plus.bmp" `withSize` (9,9) `withRef` (4,4)

minusImg = img "img/minus.bmp" `withSize` (9,9) `withRef` (4,4)

-}
