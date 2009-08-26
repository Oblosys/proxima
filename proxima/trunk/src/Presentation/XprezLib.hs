module Presentation.XprezLib where

import Common.CommonTypes
import Evaluation.DocTypes
import Evaluation.DocumentEdit
import Presentation.PresTypes
import Presentation.PresentationParsing
import Layout.LayTypes
import Proxima.Wrap
import Maybe

-- switch href and vref href is y and vref is x, so (vref, href) is more logical

type Xprez doc enr node clip token = Presentation doc enr node clip token
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
overlay = OverlayP NoIDP HeadInFront
overlayReverse= OverlayP NoIDP HeadAtBack
-- overlayReverse puts the head of the list at the bottom of the stack. The head
-- is still the element that is scanned and parsed though. Hence overlay [ image, text ] is
-- different from overlayReverse [ text, image ]


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

parsingWithParser :: (Editable a doc enr node clip token, DocNode node, Ord token, Show token) =>
                     ListParser doc enr node clip token a -> a -> Presentation doc enr node clip token ->
                     Presentation doc enr node clip token
parsingWithParser parser self pres = ParsingP NoIDP (Just $ mkClipParser $ parser) LexInherited pres

parsingWithParserLexer :: (Editable a doc enr node clip token, DocNode node, Ord token, Show token) =>
                     ListParser doc enr node clip token a -> Lexer -> a -> Presentation doc enr node clip token ->
                     Presentation doc enr node clip token
parsingWithParserLexer parser lexer self pres = ParsingP NoIDP (Just $ mkClipParser $ parser) lexer pres

loc l xp  = LocatorP l xp
tag t xp = TagP t xp

dragSource xp = tag DragSourceTag xp
dropTarget or xp = tag (DropTargetTag or) xp

draggableCol xps = dropTarget Vertical $ col $ map dragSource xps
draggableRow xps = dropTarget Horizontal $ row $ map dragSource xps

graph :: Int -> Int -> [(Int,Int)] -> [Xprez doc enr node clip token] -> Xprez doc enr node clip token
graph width height edges vertexPress = 
  GraphP NoIDP Clean width height edges (map dragSource vertexPress)
 
vertex :: Int -> Int -> Int -> Outline -> Xprez doc enr node clip token -> Xprez doc enr node clip token
vertex id x y outline pres = VertexP NoIDP id x y outline pres


-- used for putting structural presentations inside parsing presentations
structuralToken idp pres = TokenP idp (StructuralTk 0 Nothing (StructuralP idp pres) [] NoIDP)
-- we duplicate the idp: TokenP needs it, so detokenize can add the correct whitespace
-- And since TokenP and StructuralTk are removed by the layouter, we need it in the StructuralP
-- as well, so it can be recovered by the scanner
-- It is not added to StructuralTk, whose idp field is only used during scanning.

-- presHole and presParseErr take care of adding the loc. This is to make explicitly specifying
-- .pres for hole and parse error alternatives a little less cumbersome

-- TODO: add presentFocus
presHole focus typeStr nd pth = loc nd $ 
--  structural $ row [text $ "{"++typeStr++"}"] `withColor` black `withbgColor` yellow -- `withFontFam` ("Courier New")
  structural $ overlay [text " ", poly [(l,u),(r,u),(r,d),(l,d),(l,u)] Transparent]
 where l = 0.2
       r = 0.8
       u = 0.2
       d = 0.8
       
presParseErr node (StructuralParseErr pres) =
  loc node $ structural $ pres `withbgColor` whiteSmoke
presParseErr node (ParsingParseErr idP parseErrs tokens parser) =
  loc node $ ParsingP idP (Just parser) LexInherited $ 
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
presFromToken token                                   = TokenP (getTokenIDP token) token 




leftXp `beside` rightXp = row [leftXp, rightXp]

topXp `above` bottomXp = col [topXp, bottomXp]


withInh xp inf =  with_ xp (\(i,s) -> (inf i, s))
withSyn xp snf =  with_ xp (\(i,s) -> (i, snf s))

withColor :: Xprez doc enr node clip token -> Color -> Xprez doc enr node clip token
withColor xp c = withInh xp (\i -> i { textColor = c, lineColor = c})

withtxtColor :: Xprez doc enr node clip token -> Color -> Xprez doc enr node clip token
withtxtColor xp c = withInh xp (\i -> i { textColor = c})

withtxtColor_ :: Xprez doc enr node clip token -> (Color -> Color) -> Xprez doc enr node clip token
withtxtColor_ xp f = withInh xp (\i -> i { textColor = f (textColor i)})

withlnColor :: Xprez doc enr node clip token -> Color -> Xprez doc enr node clip token
withlnColor xp c = withInh xp (\i -> i { lineColor = c})

withfColor :: Xprez doc enr node clip token -> Color -> Xprez doc enr node clip token
withfColor xp c = withInh xp (\i -> i { fillColor = c})

withbgColor :: Xprez doc enr node clip token -> Color -> Xprez doc enr node clip token
withbgColor xp c = withInh xp (\i -> i { backgroundColor = c})

withbgColor_ :: Xprez doc enr node clip token -> (Color -> Color) -> Xprez doc enr node clip token
withbgColor_ xp f = withInh xp (\i -> i { backgroundColor = f (backgroundColor i)})

withFontSize :: Xprez doc enr node clip token -> Int -> Xprez doc enr node clip token
withFontSize xp fs = withInh xp (\i -> i { font = (font i) { fSize = fs }})

withFontFam :: Xprez doc enr node clip token -> String -> Xprez doc enr node clip token
withFontFam xp ff = withInh xp (\i -> i { font = (font i) { fFamily = ff }})

withFontSize_ :: Xprez doc enr node clip token -> (Int -> Int) -> Xprez doc enr node clip token
withFontSize_ xp ffs = withInh xp (\i -> i { font = (font i) { fSize = ffs (fSize (font i)) }})

withFont :: Xprez doc enr node clip token -> Font -> Xprez doc enr node clip token
withFont xp f = withInh xp (\i -> i { font = f })

withFont_ :: Xprez doc enr node clip token -> (Font -> Font) -> Xprez doc enr node clip token
withFont_ xp ff = withInh xp (\i -> i { font = ff (font i) })

-- just set Family and Size
withFont' :: Xprez doc enr node clip token -> (String, Int) -> Xprez doc enr node clip token
withFont' xp (ff,fs) = withFont_ xp (\f -> f { fFamily = ff, fSize = fs })

bold :: Xprez doc enr node clip token -> Xprez doc enr node clip token
bold xp = withFont_ xp (\f -> f { fBold = True })

italic :: Xprez doc enr node clip token -> Xprez doc enr node clip token
italic xp = withFont_ xp (\f -> f { fItalic = True })

underline :: Xprez doc enr node clip token -> Xprez doc enr node clip token
underline xp = withFont_ xp (\f -> f { fUnderline = True })

strikeOut :: Xprez doc enr node clip token -> Xprez doc enr node clip token
strikeOut xp = withFont_ xp (\f -> f { fStrikeOut = True })


withMouseDown :: forall doc enr node clip token . 
                 Xprez doc enr node clip token -> UpdateDoc doc clip -> Xprez doc enr node clip token
withMouseDown xp upd = withInh xp (\i -> i { mouseDown = Just $ wrap (UpdateDoc' upd :: EditDocument' doc enr node clip token) })

withMouseDownBold :: forall doc enr node clip token . 
                 Xprez doc enr node clip token -> Xprez doc enr node clip token
withMouseDownBold xp = withInh xp (\i -> i { mouseDown = Just $ wrap (SetStyleLay Bold :: EditLayout doc enr node clip token) })

-- this one deletes all inherited popup items
withInheritablePopupMenuItems :: Xprez doc enr node clip token -> [PopupMenuItem doc clip] -> Xprez doc enr node clip token
withInheritablePopupMenuItems xp mis = withInh xp (\i -> i { inheritablePopupMenuItems = mis })

withInheritablePopupMenuItems_ :: Xprez doc enr node clip token -> ([PopupMenuItem doc clip] -> [PopupMenuItem doc clip]) -> Xprez doc enr node clip token
withInheritablePopupMenuItems_ xp fmis = withInh xp (\i -> i { inheritablePopupMenuItems = fmis (inheritablePopupMenuItems i) })

addPopupItems :: Xprez doc enr node clip token -> [PopupMenuItem doc clip] -> Xprez doc enr node clip token
addPopupItems xp mis = withInheritablePopupMenuItems_ xp (\pmis -> mis++pmis) 

withLocalPopupMenuItems :: Xprez doc enr node clip token -> [PopupMenuItem doc clip] -> Xprez doc enr node clip token
withLocalPopupMenuItems xp mis = withInh xp (\i -> i { localPopupMenuItems = mis })

withLocalPopupMenuItems_ :: Xprez doc enr node clip token -> ([PopupMenuItem doc clip] -> [PopupMenuItem doc clip]) -> Xprez doc enr node clip token
withLocalPopupMenuItems_ xp fmis = withInh xp (\i -> i { localPopupMenuItems = fmis (localPopupMenuItems i) })

addLocalPopupItems :: Xprez doc enr node clip token -> [PopupMenuItem doc clip] -> Xprez doc enr node clip token
addLocalPopupItems xp mis = withLocalPopupMenuItems_ xp (\pmis -> mis++pmis) 

withHRef :: Xprez doc enr node clip token -> Int -> Xprez doc enr node clip token
withHRef xp h = withSyn xp (\s -> s { hRef = h })

withHRef_ :: Xprez doc enr node clip token -> (Int -> Int) -> Xprez doc enr node clip token
withHRef_ xp fh = withSyn xp (\s -> s { hRef = fh (hRef s) })

withVRef :: Xprez doc enr node clip token -> Int -> Xprez doc enr node clip token
withVRef xp v = withSyn xp (\s -> s { vRef = v })

withVRef_ :: Xprez doc enr node clip token -> (Int -> Int) -> Xprez doc enr node clip token
withVRef_ xp fv = withSyn xp (\s -> s { vRef = fv (vRef s) })

withRef :: Xprez doc enr node clip token -> (Int, Int) -> Xprez doc enr node clip token
withRef xp (h,v) = withSyn xp (\s -> s { hRef = h, vRef = v })

withRef_ :: Xprez doc enr node clip token -> ((Int, Int) -> (Int, Int)) -> Xprez doc enr node clip token
withRef_ xp fhv = withSyn xp (\s -> let (h,v) = fhv (hRef s, vRef s)
                                    in  s { hRef = h, vRef = v })

withHStretch :: Xprez doc enr node clip token -> Bool -> Xprez doc enr node clip token
withHStretch xp hs = withSyn xp (\s -> s { hStretch = hs })

withVStretch :: Xprez doc enr node clip token -> Bool -> Xprez doc enr node clip token
withVStretch xp vs = withSyn xp (\s -> s { vStretch = vs })

withStretch :: Xprez doc enr node clip token -> Bool -> Xprez doc enr node clip token
withStretch xp str = withSyn xp (\s -> s { hStretch = str, vStretch = str })

withWidth :: Xprez doc enr node clip token -> Int -> Xprez doc enr node clip token
withWidth xp w = withSyn xp (\s -> s { minWidth = w, hStretch = False })

withHeight :: Xprez doc enr node clip token -> Int -> Xprez doc enr node clip token
withHeight xp h = withSyn xp (\s -> s { minHeight = h, vStretch = False })

withSize :: Xprez doc enr node clip token -> (Int, Int) -> Xprez doc enr node clip token
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


{-
Polies are not rendered accurately in Web browsers, so for Proxima 2.0, we need to use empties 
with background to create lines.
-}
hLinePoly :: Xprez doc enr node clip token
hLinePoly = poly [(0,0),(1.0,0)] Transparent `withHeight` 1 

hLine :: Xprez doc enr node clip token
hLine = lineColorAsBGColor $ empty `withHeight` 1 `withHStretch` True

vLinePoly :: Xprez doc enr node clip token
vLinePoly = poly [(0,0),(0,1.0)] Transparent `withWidth` 1

vLine :: Xprez doc enr node clip token
vLine = lineColorAsBGColor $ empty `withWidth` 1 `withVStretch` True

lineColorAsBGColor pres =  pres `with_` (\(i,s) -> let i' = i { backgroundColor = lineColor i
                                               } 
                                    in (i',s))


-- lineWidth should be an attribute, so we can use a with here
hLineW :: Int -> Xprez doc enr node clip token
hLineW lw = polyW lw [(0,0),(1.0,0)] Transparent `withHeight` 1

vLineW :: Int -> Xprez doc enr node clip token
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

glue :: Xprez doc enr node clip token
glue = empty `withHStretch` True `withVStretch` True

boxed :: Xprez doc enr node clip token -> Xprez doc enr node clip token
boxed p = colR 1 [ hLine, rowR 1 [ vLine, p, vLine ], hLine ]

-- multiply x with a percentage
percent :: Int -> Int -> Int
percent a x = a * x `div` 100


presentFocus NoPathD     path pres = pres
presentFocus (PathD pth) path pres = if pth==path then pres `withbgColor` focusCol else pres

focusCol = lightBlue -- lightGrey

squiggly :: Color -> Xprez doc enr node clip token -> Xprez doc enr node clip token
squiggly c xp = overlayReverse [xp, img imgFile `withHeight` 3 `withColor` c]
 where imgFile | c == red    = "img/redSquiggly.png" -- HACK. setting colors for images
               | c == green  = "img/greenSquiggly.png"  -- is a problem in gtk2HS
               | otherwise   = "img/squiggly.bmp" 

-- | add squiggly when condition holds
squiggle :: Color -> Bool -> Xprez doc enr node clip token -> Xprez doc enr node clip token
squiggle color condition pres = if condition then squiggly color pres else pres

               
presentElementXML :: FocusDoc -> node -> [Int] -> String -> [Presentation doc enr node clip token] -> Presentation doc enr node clip token
presentElementXML focusD node path tag children =
  loc node $ parsing $ presentFocus focusD path $                  
    if null children
    then col [ text $ "<"++tag++"/>"]
    else col [ text  $ "<"++tag++">"
             , row [ text "  ", col children ]
             , text $ "</"++tag++">" ]      
    

presentElementTree :: FocusDoc -> node -> [Int] -> String -> [Presentation doc enr node clip token] -> Presentation doc enr node clip token
presentElementTree focusD node path tag children =
  loc node $ parsing $ presentFocus focusD path $                  
    if null children
    then mkTreeLeaf False $ text $ tag
    else mkTreeNode False True (text tag) children

-------------------------------------------------------------------------
-- change hLine and vLine to empty to get rid of lines
hLine' = hLine -- empty
vLine' = vLine -- empty


mkTreeLeaf :: Bool -> Xprez doc enr node clip token -> Xprez doc enr node clip token
mkTreeLeaf isLast label =
  row [ leafHandle isLast, hLine `withWidth` 12, leafImg
      , hLine `withWidth` 5, vRefHalf label ]

mkTreeNode :: Bool -> Bool -> Xprez doc enr node clip token -> [ Xprez doc enr node clip token ] -> Xprez doc enr node clip token
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

mkTreeLeaf :: Bool -> Xprez doc enr node clip token -> Xprez doc enr node clip token
mkTreeLeaf isLast label =
  row [ leafHandle isLast, hLine `withWidth` 12, leafImg
      , hLine `withWidth` 5, refHalf label ]

mkTreeNode :: Bool -> Bool -> Xprez doc enr node clip token -> [ Xprez doc enr node clip token ] -> Xprez doc enr node clip token
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
