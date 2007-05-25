-- UUAGC 0.9.3 (../../proxima/src/arrangement/ArrangerAG.ag)
module ArrangerAG where

import CommonTypes
import ArrLayerTypes
import ArrLayerUtils
import FontLib

{-
TODO: 
bug: in (rowR 1 [ glue, pres, glue ]) left glue gets width 0, even when put in an overlay. (apparent in Dazzle network view)
bug: hline and vline implemented with polys are rendered with incorrect white space in boxed primitive

- why does Empty.arrangement have 0 for x and y?

-}


{- for AlternativeP
selectFirstFit :: Int -> [ Int ] -> Int       
selectFirstFit availableWidth widths = 
  if null widths 
  then error "Alternative presentation has no alternatives"
  else select widths 0
 where
  select [_] n = n
  select (w:ws) n = if w <= availableWidth then n else select ws (n+1)

-- maybe nicer as synth attribute for presentationList

-}


{- for MatrixP
divide n [] = []
divide n xs = let (line, rest) = splitAt n xs in line : divide n rest

getRows n [] = []
getRows n xs = let (line, rest) = splitAt n xs in line : divide n rest

getCols n xs = foldr (zipWith (:)) (repeat []) (getRows n xs)
-}



{- for FormatterP
unfoldAGFormatter w ws presentations = 
 let breaks = firstFit w ws
     dividedPresentations = linesFromBreaks breaks presentations
     rows = -- map (\lst -> LayedOutRowP lst 100 32 (repeat (0,0))) presentationLists
         map (RowP NoID 0) dividedPresentations
     unfoldedList = ColP NoID 0 (rows)
 in  --debug Arr ("formatter has width:"++show w++"\nkids have widths "++show ws) 
           unfoldedList



-- just add words until line overflow
firstFit lineWidth widths = buildFFBreaks lineWidth 0 0 widths

buildFFBreaks _ pos _ [] = [] -- add a breakpoint after last word? prob. not, otherwise return [pos]
buildFFBreaks maxLength pos currentLength (width:widths) =
  let space = if currentLength == 0 then 0 else 0 -- hard coded minimum space, and widths must be >= 0 
      newLength = currentLength + space + width
  in if currentLength == 0 && width > maxLength
     then if null widths 
          then []
          else (pos+1) : buildFFBreaks maxLength pos 0 (widths)
     else if newLength <= maxLength
          then buildFFBreaks maxLength (pos+1) newLength widths
          else pos : buildFFBreaks maxLength (pos) 0 (width:widths)

linesFromBreaks :: [Int] -> [a] -> [[a]]
linesFromBreaks brks wrds = tolines' brks 0 wrds 
   where
   tolines' [] _ wrds = [wrds]
   tolines' (brk:brks) lastbrk wrds = take (brk-lastbrk) wrds :
                                      tolines' brks brk (drop (brk-lastbrk) wrds)

-}


assign [] _ _ = []
assign (str:strs) (a:as) (o:os) = (if str then a else o) : assign strs as os


data Root doc node clip = Root (Presentation doc node clip)

type PresentationList doc node clip = [Presentation doc node clip]
                      
-- We don't want AG to generate the Presentation data types, so data type generation is turned off.
-- However, we do need Root and PresentationList
-- Presentation ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         assignedHRef         : Int
         assignedHeight       : Int
         assignedVRef         : Int
         assignedWidth        : Int
         backgroundColor      : Color
         fillColor            : Color
         font                 : Font
         fontMetrics          : FontMetrics
         lineColor            : Color
         mouseDown            : Maybe (UpdateDoc doc clip)
         oldArr               : Arrangement node
         popupMenuItems       : [ PopupMenuItem ]
         textColor            : Color
         x                    : Int
         y                    : Int
      chained attribute:
         allFonts             : [Font]
      synthesized attributes:
         arrangement          : Arrangement node
         finalHRef            : Int
         finalHeight          : Int
         finalVRef            : Int
         finalWidth           : Int
         hRf                  : Int
         hStretch             : Bool
         maxFormatterDepth    : Int
         minHeight            : Int
         minWidth             : Int
         unfoldedTree         : Presentation
         vRf                  : Int
         vStretch             : Bool
   alternatives:
      alternative ArrangedP:
         local minWidth       : _
         local minHeight      : _
         local hStretch       : _
         local vStretch       : _
         local hRf            : _
         local vRf            : _
         local finalWidth     : _
         local finalHeight    : _
         local finalHRef      : _
         local finalVRef      : _
         local unfoldedTree   : _
         local maxFormatterDepth : _
      alternative ColP:
         child id             : {IDP}
         child hRefNr         : {Int}
         child presentationList : PresentationList
         local minLeftWidth   : _
         local minRightWidth  : _
         local minWidth       : _
         local minHeight      : _
         local hStretch       : _
         local vStretch       : _
         local hRf            : _
         local vRf            : _
         local assignedHeights : _
         local stretchMinHeights : _
         local topCorrection  : _
         local bottomCorrection : _
         local topTotalSpace  : _
         local bottomTotalSpace : _
         local topVStretches  : _
         local bottomVStretches : _
         local topChildSpace  : _
         local bottomChildSpace : _
         local finalWidth     : _
         local finalHeight    : _
         local finalHRef      : _
         local finalVRef      : _
      alternative EllipseP:
         child id             : {IDP}
         child w              : {Int}
         child h              : {Int}
         child lineWidth      : {Int}
         local minWidth       : _
         local minHeight      : _
         local hStretch       : _
         local vStretch       : _
         local hRf            : _
         local vRf            : _
         local finalWidth     : _
         local finalHeight    : _
         local finalHRef      : _
         local finalVRef      : _
      alternative EmptyP:
         child id             : {IDP}
         local minWidth       : _
         local minHeight      : _
         local hStretch       : _
         local vStretch       : _
         local hRf            : _
         local vRf            : _
         local finalWidth     : _
         local finalHeight    : _
         local finalHRef      : _
         local finalVRef      : _
      alternative GraphP:
         child id             : {IDP}
         child w              : {Int}
         child h              : {Int}
         child edges          : {[(Int,Int)]}
         child presentationList : PresentationList
         local minWidth       : _
         local minHeight      : _
         local hStretch       : _
         local vStretch       : _
         local hRf            : _
         local vRf            : _
         local finalWidth     : _
         local finalHeight    : _
         local finalHRef      : _
         local finalVRef      : _
      alternative ImageP:
         child id             : {IDP}
         child src            : {String}
         local minWidth       : _
         local minHeight      : _
         local hStretch       : _
         local vStretch       : _
         local hRf            : _
         local vRf            : _
         local finalWidth     : _
         local finalHeight    : _
         local finalHRef      : _
         local finalVRef      : _
      alternative LocatorP:
         child location       : {node}
         child child          : Presentation
      alternative OverlayP:
         child id             : {IDP}
         child presentationList : PresentationList
         local minLeftWidth   : _
         local minRightWidth  : _
         local minWidth       : _
         local minTopHeight   : _
         local minBottomHeight : _
         local minHeight      : _
         local hStretch       : _
         local vStretch       : _
         local hRf            : _
         local vRf            : _
         local finalWidth     : _
         local finalHeight    : _
         local finalHRef      : _
         local finalVRef      : _
      alternative ParsingP:
         child id             : {IDP}
         child child          : Presentation
      alternative PolyP:
         child id             : {IDP}
         child pointList      : {[ (Float, Float) ]}
         child lineWidth      : {Int}
         local minWidth       : _
         local minHeight      : _
         local hStretch       : _
         local vStretch       : _
         local hRf            : _
         local vRf            : _
         local finalWidth     : _
         local finalHeight    : _
         local finalHRef      : _
         local finalVRef      : _
      alternative RectangleP:
         child id             : {IDP}
         child w              : {Int}
         child h              : {Int}
         child lineWidth      : {Int}
         local minWidth       : _
         local minHeight      : _
         local hStretch       : _
         local vStretch       : _
         local hRf            : _
         local vRf            : _
         local finalWidth     : _
         local finalHeight    : _
         local finalHRef      : _
         local finalVRef      : _
      alternative RowP:
         child id             : {IDP}
         child vRefNr         : {Int}
         child presentationList : PresentationList
         local minTopHeight   : _
         local minBottomHeight : _
         local minWidth       : _
         local minHeight      : _
         local hStretch       : _
         local vStretch       : _
         local hRf            : _
         local vRf            : _
         local assignedWidths : _
         local stretchMinWidths : _
         local leftCorrection : _
         local rightCorrection : _
         local leftTotalSpace : _
         local rightTotalSpace : _
         local leftHStretches : _
         local rightHStretches : _
         local leftChildSpace : _
         local rightChildSpace : _
         local finalWidth     : _
         local finalHeight    : _
         local finalHRef      : _
         local finalVRef      : _
      alternative StringP:
         child id             : {IDP}
         child text           : {String}
         local minWidth       : _
         local minHeight      : _
         local hStretch       : _
         local vStretch       : _
         local hRf            : _
         local vRf            : _
         local finalWidth     : _
         local finalHeight    : _
         local finalHRef      : _
         local finalVRef      : _
      alternative StructuralP:
         child id             : {IDP}
         child child          : Presentation
      alternative VertexP:
         child id             : {IDP}
         child x              : {Int}
         child y              : {Int}
         child outline        : {Outline}
         child child          : Presentation
         local finalWidth     : _
         local finalHeight    : _
         local finalHRef      : _
         local finalVRef      : _
      alternative WithP:
         child attrRule       : {AttrRule}
         child child          : Presentation
         local newAttrs       : _
         local newSyn         : _
         local newInh         : _
-}
-- cata
sem_Presentation (ArrangedP ) =
    (sem_Presentation_ArrangedP )
sem_Presentation (ColP _id _hRefNr _presentationList) =
    (sem_Presentation_ColP _id _hRefNr (sem_PresentationList _presentationList))
sem_Presentation (EllipseP _id _w _h _lineWidth) =
    (sem_Presentation_EllipseP _id _w _h _lineWidth)
sem_Presentation (EmptyP _id) =
    (sem_Presentation_EmptyP _id)
sem_Presentation (GraphP _id _w _h _edges _presentationList) =
    (sem_Presentation_GraphP _id _w _h _edges (sem_PresentationList _presentationList))
sem_Presentation (ImageP _id _src) =
    (sem_Presentation_ImageP _id _src)
sem_Presentation (LocatorP _location _child) =
    (sem_Presentation_LocatorP _location (sem_Presentation _child))
sem_Presentation (OverlayP _id _presentationList) =
    (sem_Presentation_OverlayP _id (sem_PresentationList _presentationList))
sem_Presentation (ParsingP _id _child) =
    (sem_Presentation_ParsingP _id (sem_Presentation _child))
sem_Presentation (PolyP _id _pointList _lineWidth) =
    (sem_Presentation_PolyP _id _pointList _lineWidth)
sem_Presentation (RectangleP _id _w _h _lineWidth) =
    (sem_Presentation_RectangleP _id _w _h _lineWidth)
sem_Presentation (RowP _id _vRefNr _presentationList) =
    (sem_Presentation_RowP _id _vRefNr (sem_PresentationList _presentationList))
sem_Presentation (StringP _id _text) =
    (sem_Presentation_StringP _id _text)
sem_Presentation (StructuralP _id _child) =
    (sem_Presentation_StructuralP _id (sem_Presentation _child))
sem_Presentation (VertexP _id _x _y _outline _child) =
    (sem_Presentation_VertexP _id _x _y _outline (sem_Presentation _child))
sem_Presentation (WithP _attrRule _child) =
    (sem_Presentation_WithP _attrRule (sem_Presentation _child))
sem_Presentation_ArrangedP  =
    (\ _lhsIallFonts
       _lhsIassignedHRef
       _lhsIassignedHeight
       _lhsIassignedVRef
       _lhsIassignedWidth
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIx
       _lhsIy ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 940, column 7)
              _lhsOarrangement =
                  setXYWHA _lhsIx _lhsIy _finalWidth _finalHeight _lhsIoldArr
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 946, column 7)
              _minWidth =
                  _finalWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 947, column 7)
              _minHeight =
                  _finalHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 948, column 7)
              _hStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 949, column 7)
              _vStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 950, column 7)
              _hRf =
                  hRefA _lhsIoldArr
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 951, column 7)
              _vRf =
                  vRefA _lhsIoldArr
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 952, column 7)
              _finalWidth =
                  widthA _lhsIoldArr
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 953, column 7)
              _finalHeight =
                  heightA _lhsIoldArr
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 954, column 7)
              _finalHRef =
                  hRefA _lhsIoldArr
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 955, column 7)
              _finalVRef =
                  vRefA _lhsIoldArr
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 956, column 7)
              _unfoldedTree =
                  ArrangedP
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 957, column 7)
              _maxFormatterDepth =
                  0
              -- copy rule (chain)
              _lhsOallFonts =
                  _lhsIallFonts
              -- copy rule (from local)
              _lhsOfinalHRef =
                  _finalHRef
              -- copy rule (from local)
              _lhsOfinalHeight =
                  _finalHeight
              -- copy rule (from local)
              _lhsOfinalVRef =
                  _finalVRef
              -- copy rule (from local)
              _lhsOfinalWidth =
                  _finalWidth
              -- copy rule (from local)
              _lhsOhRf =
                  _hRf
              -- copy rule (from local)
              _lhsOhStretch =
                  _hStretch
              -- copy rule (from local)
              _lhsOmaxFormatterDepth =
                  _maxFormatterDepth
              -- copy rule (from local)
              _lhsOminHeight =
                  _minHeight
              -- copy rule (from local)
              _lhsOminWidth =
                  _minWidth
              -- copy rule (from local)
              _lhsOunfoldedTree =
                  _unfoldedTree
              -- copy rule (from local)
              _lhsOvRf =
                  _vRf
              -- copy rule (from local)
              _lhsOvStretch =
                  _vStretch
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOfinalHRef,_lhsOfinalHeight,_lhsOfinalVRef,_lhsOfinalWidth,_lhsOhRf,_lhsOhStretch,_lhsOmaxFormatterDepth,_lhsOminHeight,_lhsOminWidth,_lhsOunfoldedTree,_lhsOvRf,_lhsOvStretch)))
sem_Presentation_ColP id_ hRefNr_ presentationList_ =
    (\ _lhsIallFonts
       _lhsIassignedHRef
       _lhsIassignedHeight
       _lhsIassignedVRef
       _lhsIassignedWidth
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIx
       _lhsIy ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 254, column 7)
              _lhsOmaxFormatterDepth =
                  maximum _presentationListImaxFormatterDepthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 304, column 7)
              _lhsOunfoldedTree =
                  ColP id_ hRefNr_ _presentationListIunfoldedTreeList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 419, column 7)
              _minLeftWidth =
                  if null _presentationListIvRfList then 0
                  else maximum _presentationListIvRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 421, column 7)
              _minRightWidth =
                  if null _presentationListIvRfList then 0
                  else maximum [ minWidth - vRf | (minWidth, vRf) <- zip _presentationListIminWidthList _presentationListIvRfList ]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 423, column 7)
              _minWidth =
                  _minLeftWidth + _minRightWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 424, column 7)
              _minHeight =
                  sum _presentationListIminHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 425, column 7)
              _hStretch =
                  and _presentationListIhStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 426, column 7)
              _vStretch =
                  or _presentationListIvStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 427, column 7)
              _hRf =
                  if null _presentationListIhRfList then 0
                  else sum (take hRefNr_ _presentationListIminHeightList) + _presentationListIhRfList !! hRefNr_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 429, column 7)
              _vRf =
                  _minLeftWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 575, column 7)
              _presentationListOassignedWidthList =
                  assign _presentationListIhStretchList
                  (repeat _lhsIassignedWidth) _presentationListIminWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 578, column 7)
              _presentationListOassignedVRefList =
                  assign _presentationListIhStretchList
                  (repeat _lhsIassignedVRef)
                  _presentationListIvRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 581, column 7)
              _assignedHeights =
                  if _presentationListIvStretchList!! hRefNr_
                  then assign _presentationListIvStretchList
                                     (repeat((_topTotalSpace+ _bottomTotalSpace)`div`(_topVStretches+1+ _bottomVStretches))
                                     )
                                     _presentationListIminHeightList
                  else assign _presentationListIvStretchList
                                     (  replicate hRefNr_ (_topChildSpace)
                                     ++ [0]
                                     ++ repeat _bottomChildSpace
                                     )
                                     _presentationListIminHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 593, column 7)
              _presentationListOassignedHeightList =
                  _assignedHeights
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 594, column 7)
              _presentationListOassignedHRefList =
                  _presentationListIhRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 604, column 7)
              _stretchMinHeights =
                  assign _presentationListIvStretchList _presentationListIminHeightList (repeat 0)
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 607, column 7)
              _topCorrection =
                  sum (take hRefNr_ _stretchMinHeights)
                      + if _presentationListIvStretchList!! hRefNr_ then _stretchMinHeights !! hRefNr_ else 0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 609, column 7)
              _bottomCorrection =
                  sum (drop (hRefNr_+1) _stretchMinHeights)
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 614, column 7)
              _topTotalSpace =
                  _lhsIassignedHRef - _hRf  + _topCorrection
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 615, column 7)
              _bottomTotalSpace =
                  _lhsIassignedHeight - _minHeight - _topTotalSpace   + _bottomCorrection
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 616, column 7)
              _topVStretches =
                  length . filter (==True) . take hRefNr_ $ _presentationListIvStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 617, column 7)
              _bottomVStretches =
                  length . filter (==True) . drop (hRefNr_+1) $ _presentationListIvStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 618, column 7)
              _topChildSpace =
                  round (fromIntegral _topTotalSpace / fromIntegral _topVStretches )
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 619, column 7)
              _bottomChildSpace =
                  round (fromIntegral _bottomTotalSpace / fromIntegral _bottomVStretches )
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 725, column 7)
              _finalWidth =
                  _lhsIassignedWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 726, column 7)
              _finalHeight =
                  sum _presentationListIfinalHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 727, column 7)
              _finalHRef =
                  if null _presentationListIhRfList then 0
                  else if _presentationListIvStretchList!! hRefNr_ then _lhsIassignedHRef
                          else sum (take hRefNr_ _assignedHeights) + (_presentationListIhRfList !! hRefNr_)
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 730, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 815, column 7)
              _presentationListOxList =
                  [ _finalVRef - cvRf | cvRf <- _presentationListIfinalVRefList ]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 816, column 7)
              _presentationListOyList =
                  init.scanl (+) 0 $ _presentationListIfinalHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 905, column 7)
              _lhsOarrangement =
                  ColA (idAFromP id_) _lhsIx _lhsIy _finalWidth _finalHeight _finalHRef _finalVRef _lhsIbackgroundColor _presentationListIarrangementList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 970, column 7)
              _presentationListOoldArrList =
                  case _lhsIoldArr of ColA _ _ _ _ _ _ _ _ arrs -> arrs
                                      _                     -> repeat $ EmptyA NoIDA 0 0 0 0 0 0
              -- copy rule (up)
              _lhsOallFonts =
                  _presentationListIallFonts
              -- copy rule (from local)
              _lhsOfinalHRef =
                  _finalHRef
              -- copy rule (from local)
              _lhsOfinalHeight =
                  _finalHeight
              -- copy rule (from local)
              _lhsOfinalVRef =
                  _finalVRef
              -- copy rule (from local)
              _lhsOfinalWidth =
                  _finalWidth
              -- copy rule (from local)
              _lhsOhRf =
                  _hRf
              -- copy rule (from local)
              _lhsOhStretch =
                  _hStretch
              -- copy rule (from local)
              _lhsOminHeight =
                  _minHeight
              -- copy rule (from local)
              _lhsOminWidth =
                  _minWidth
              -- copy rule (from local)
              _lhsOvRf =
                  _vRf
              -- copy rule (from local)
              _lhsOvStretch =
                  _vStretch
              -- copy rule (down)
              _presentationListOallFonts =
                  _lhsIallFonts
              -- copy rule (down)
              _presentationListObackgroundColor =
                  _lhsIbackgroundColor
              -- copy rule (down)
              _presentationListOfillColor =
                  _lhsIfillColor
              -- copy rule (down)
              _presentationListOfont =
                  _lhsIfont
              -- copy rule (down)
              _presentationListOfontMetrics =
                  _lhsIfontMetrics
              -- copy rule (down)
              _presentationListOlineColor =
                  _lhsIlineColor
              -- copy rule (down)
              _presentationListOmouseDown =
                  _lhsImouseDown
              -- copy rule (down)
              _presentationListOpopupMenuItems =
                  _lhsIpopupMenuItems
              -- copy rule (down)
              _presentationListOtextColor =
                  _lhsItextColor
              ( _presentationListIallFonts
               ,_presentationListIarrangementList
               ,_presentationListIfinalHRefList
               ,_presentationListIfinalHeightList
               ,_presentationListIfinalVRefList
               ,_presentationListIfinalWidthList
               ,_presentationListIhRfList
               ,_presentationListIhStretchList
               ,_presentationListImaxFormatterDepthList
               ,_presentationListIminHeightList
               ,_presentationListIminWidthList
               ,_presentationListIunfoldedTreeList
               ,_presentationListIvRfList
               ,_presentationListIvStretchList
               ) =
                  (presentationList_ _presentationListOallFonts
                                     _presentationListOassignedHRefList
                                     _presentationListOassignedHeightList
                                     _presentationListOassignedVRefList
                                     _presentationListOassignedWidthList
                                     _presentationListObackgroundColor
                                     _presentationListOfillColor
                                     _presentationListOfont
                                     _presentationListOfontMetrics
                                     _presentationListOlineColor
                                     _presentationListOmouseDown
                                     _presentationListOoldArrList
                                     _presentationListOpopupMenuItems
                                     _presentationListOtextColor
                                     _presentationListOxList
                                     _presentationListOyList)
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOfinalHRef,_lhsOfinalHeight,_lhsOfinalVRef,_lhsOfinalWidth,_lhsOhRf,_lhsOhStretch,_lhsOmaxFormatterDepth,_lhsOminHeight,_lhsOminWidth,_lhsOunfoldedTree,_lhsOvRf,_lhsOvStretch)))
sem_Presentation_EllipseP id_ w_ h_ lineWidth_ =
    (\ _lhsIallFonts
       _lhsIassignedHRef
       _lhsIassignedHeight
       _lhsIassignedVRef
       _lhsIassignedWidth
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIx
       _lhsIy ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 246, column 7)
              _lhsOmaxFormatterDepth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 296, column 7)
              _lhsOunfoldedTree =
                  EllipseP id_ w_ h_ lineWidth_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 378, column 7)
              _minWidth =
                  w_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 379, column 7)
              _minHeight =
                  h_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 380, column 7)
              _hStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 381, column 7)
              _vStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 382, column 7)
              _hRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 383, column 7)
              _vRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 703, column 7)
              _finalWidth =
                  _minWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 704, column 7)
              _finalHeight =
                  _minHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 705, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 706, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 880, column 7)
              _lhsOarrangement =
                  EllipseA (idAFromP id_) _lhsIx _lhsIy _finalWidth _finalHeight _finalHRef _finalVRef lineWidth_ Solid _lhsIlineColor _lhsIfillColor
              -- copy rule (chain)
              _lhsOallFonts =
                  _lhsIallFonts
              -- copy rule (from local)
              _lhsOfinalHRef =
                  _finalHRef
              -- copy rule (from local)
              _lhsOfinalHeight =
                  _finalHeight
              -- copy rule (from local)
              _lhsOfinalVRef =
                  _finalVRef
              -- copy rule (from local)
              _lhsOfinalWidth =
                  _finalWidth
              -- copy rule (from local)
              _lhsOhRf =
                  _hRf
              -- copy rule (from local)
              _lhsOhStretch =
                  _hStretch
              -- copy rule (from local)
              _lhsOminHeight =
                  _minHeight
              -- copy rule (from local)
              _lhsOminWidth =
                  _minWidth
              -- copy rule (from local)
              _lhsOvRf =
                  _vRf
              -- copy rule (from local)
              _lhsOvStretch =
                  _vStretch
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOfinalHRef,_lhsOfinalHeight,_lhsOfinalVRef,_lhsOfinalWidth,_lhsOhRf,_lhsOhStretch,_lhsOmaxFormatterDepth,_lhsOminHeight,_lhsOminWidth,_lhsOunfoldedTree,_lhsOvRf,_lhsOvStretch)))
sem_Presentation_EmptyP id_ =
    (\ _lhsIallFonts
       _lhsIassignedHRef
       _lhsIassignedHeight
       _lhsIassignedVRef
       _lhsIassignedWidth
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIx
       _lhsIy ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 240, column 7)
              _lhsOmaxFormatterDepth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 290, column 7)
              _lhsOunfoldedTree =
                  EmptyP id_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 357, column 7)
              _minWidth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 358, column 7)
              _minHeight =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 359, column 7)
              _hStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 360, column 7)
              _vStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 361, column 7)
              _hRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 362, column 7)
              _vRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 688, column 7)
              _finalWidth =
                  _lhsIassignedWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 689, column 7)
              _finalHeight =
                  _lhsIassignedHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 690, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 691, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 874, column 7)
              _lhsOarrangement =
                  EmptyA (idAFromP id_) 0 0 0 0 0 0
              -- copy rule (chain)
              _lhsOallFonts =
                  _lhsIallFonts
              -- copy rule (from local)
              _lhsOfinalHRef =
                  _finalHRef
              -- copy rule (from local)
              _lhsOfinalHeight =
                  _finalHeight
              -- copy rule (from local)
              _lhsOfinalVRef =
                  _finalVRef
              -- copy rule (from local)
              _lhsOfinalWidth =
                  _finalWidth
              -- copy rule (from local)
              _lhsOhRf =
                  _hRf
              -- copy rule (from local)
              _lhsOhStretch =
                  _hStretch
              -- copy rule (from local)
              _lhsOminHeight =
                  _minHeight
              -- copy rule (from local)
              _lhsOminWidth =
                  _minWidth
              -- copy rule (from local)
              _lhsOvRf =
                  _vRf
              -- copy rule (from local)
              _lhsOvStretch =
                  _vStretch
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOfinalHRef,_lhsOfinalHeight,_lhsOfinalVRef,_lhsOfinalWidth,_lhsOhRf,_lhsOhStretch,_lhsOmaxFormatterDepth,_lhsOminHeight,_lhsOminWidth,_lhsOunfoldedTree,_lhsOvRf,_lhsOvStretch)))
sem_Presentation_GraphP id_ w_ h_ edges_ presentationList_ =
    (\ _lhsIallFonts
       _lhsIassignedHRef
       _lhsIassignedHeight
       _lhsIassignedVRef
       _lhsIassignedWidth
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIx
       _lhsIy ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 258, column 7)
              _lhsOmaxFormatterDepth =
                  maximum _presentationListImaxFormatterDepthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 308, column 7)
              _lhsOunfoldedTree =
                  GraphP id_ w_ h_ edges_ _presentationListIunfoldedTreeList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 399, column 7)
              _minWidth =
                  w_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 400, column 7)
              _minHeight =
                  h_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 401, column 7)
              _hStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 402, column 7)
              _vStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 403, column 7)
              _hRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 404, column 7)
              _vRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 630, column 7)
              _presentationListOassignedHeightList =
                  _presentationListIminHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 631, column 7)
              _presentationListOassignedHRefList =
                  _presentationListIhRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 632, column 7)
              _presentationListOassignedWidthList =
                  _presentationListIminWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 633, column 7)
              _presentationListOassignedVRefList =
                  _presentationListIvRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 737, column 7)
              _finalWidth =
                  _minWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 738, column 7)
              _finalHeight =
                  _minHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 739, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 740, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 822, column 7)
              _presentationListOxList =
                  replicate (length _presentationListIfinalWidthList) 0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 823, column 7)
              _presentationListOyList =
                  replicate (length _presentationListIfinalWidthList) 0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 917, column 7)
              _lhsOarrangement =
                  GraphA (idAFromP id_) _lhsIx _lhsIy _finalWidth _finalHeight _finalHRef _finalVRef _lhsIbackgroundColor _presentationListIarrangementList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 976, column 7)
              _presentationListOoldArrList =
                  case _lhsIoldArr of GraphA _ _ _ _ _ _ _ _ arrs -> arrs
              -- copy rule (up)
              _lhsOallFonts =
                  _presentationListIallFonts
              -- copy rule (from local)
              _lhsOfinalHRef =
                  _finalHRef
              -- copy rule (from local)
              _lhsOfinalHeight =
                  _finalHeight
              -- copy rule (from local)
              _lhsOfinalVRef =
                  _finalVRef
              -- copy rule (from local)
              _lhsOfinalWidth =
                  _finalWidth
              -- copy rule (from local)
              _lhsOhRf =
                  _hRf
              -- copy rule (from local)
              _lhsOhStretch =
                  _hStretch
              -- copy rule (from local)
              _lhsOminHeight =
                  _minHeight
              -- copy rule (from local)
              _lhsOminWidth =
                  _minWidth
              -- copy rule (from local)
              _lhsOvRf =
                  _vRf
              -- copy rule (from local)
              _lhsOvStretch =
                  _vStretch
              -- copy rule (down)
              _presentationListOallFonts =
                  _lhsIallFonts
              -- copy rule (down)
              _presentationListObackgroundColor =
                  _lhsIbackgroundColor
              -- copy rule (down)
              _presentationListOfillColor =
                  _lhsIfillColor
              -- copy rule (down)
              _presentationListOfont =
                  _lhsIfont
              -- copy rule (down)
              _presentationListOfontMetrics =
                  _lhsIfontMetrics
              -- copy rule (down)
              _presentationListOlineColor =
                  _lhsIlineColor
              -- copy rule (down)
              _presentationListOmouseDown =
                  _lhsImouseDown
              -- copy rule (down)
              _presentationListOpopupMenuItems =
                  _lhsIpopupMenuItems
              -- copy rule (down)
              _presentationListOtextColor =
                  _lhsItextColor
              ( _presentationListIallFonts
               ,_presentationListIarrangementList
               ,_presentationListIfinalHRefList
               ,_presentationListIfinalHeightList
               ,_presentationListIfinalVRefList
               ,_presentationListIfinalWidthList
               ,_presentationListIhRfList
               ,_presentationListIhStretchList
               ,_presentationListImaxFormatterDepthList
               ,_presentationListIminHeightList
               ,_presentationListIminWidthList
               ,_presentationListIunfoldedTreeList
               ,_presentationListIvRfList
               ,_presentationListIvStretchList
               ) =
                  (presentationList_ _presentationListOallFonts
                                     _presentationListOassignedHRefList
                                     _presentationListOassignedHeightList
                                     _presentationListOassignedVRefList
                                     _presentationListOassignedWidthList
                                     _presentationListObackgroundColor
                                     _presentationListOfillColor
                                     _presentationListOfont
                                     _presentationListOfontMetrics
                                     _presentationListOlineColor
                                     _presentationListOmouseDown
                                     _presentationListOoldArrList
                                     _presentationListOpopupMenuItems
                                     _presentationListOtextColor
                                     _presentationListOxList
                                     _presentationListOyList)
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOfinalHRef,_lhsOfinalHeight,_lhsOfinalVRef,_lhsOfinalWidth,_lhsOhRf,_lhsOhStretch,_lhsOmaxFormatterDepth,_lhsOminHeight,_lhsOminWidth,_lhsOunfoldedTree,_lhsOvRf,_lhsOvStretch)))
sem_Presentation_ImageP id_ src_ =
    (\ _lhsIallFonts
       _lhsIassignedHRef
       _lhsIassignedHeight
       _lhsIassignedVRef
       _lhsIassignedWidth
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIx
       _lhsIy ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 248, column 7)
              _lhsOmaxFormatterDepth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 298, column 7)
              _lhsOunfoldedTree =
                  ImageP id_ src_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 385, column 7)
              _minWidth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 386, column 7)
              _minHeight =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 387, column 7)
              _hStretch =
                  True
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 388, column 7)
              _vStretch =
                  True
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 389, column 7)
              _hRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 390, column 7)
              _vRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 708, column 7)
              _finalWidth =
                  _lhsIassignedWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 709, column 7)
              _finalHeight =
                  _lhsIassignedHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 710, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 711, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 882, column 7)
              _lhsOarrangement =
                  ImageA (idAFromP id_) _lhsIx _lhsIy _finalWidth _finalHeight _finalHRef _finalVRef src_ Tile _lhsIlineColor _lhsIbackgroundColor
              -- copy rule (chain)
              _lhsOallFonts =
                  _lhsIallFonts
              -- copy rule (from local)
              _lhsOfinalHRef =
                  _finalHRef
              -- copy rule (from local)
              _lhsOfinalHeight =
                  _finalHeight
              -- copy rule (from local)
              _lhsOfinalVRef =
                  _finalVRef
              -- copy rule (from local)
              _lhsOfinalWidth =
                  _finalWidth
              -- copy rule (from local)
              _lhsOhRf =
                  _hRf
              -- copy rule (from local)
              _lhsOhStretch =
                  _hStretch
              -- copy rule (from local)
              _lhsOminHeight =
                  _minHeight
              -- copy rule (from local)
              _lhsOminWidth =
                  _minWidth
              -- copy rule (from local)
              _lhsOvRf =
                  _vRf
              -- copy rule (from local)
              _lhsOvStretch =
                  _vStretch
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOfinalHRef,_lhsOfinalHeight,_lhsOfinalVRef,_lhsOfinalWidth,_lhsOhRf,_lhsOhStretch,_lhsOmaxFormatterDepth,_lhsOminHeight,_lhsOminWidth,_lhsOunfoldedTree,_lhsOvRf,_lhsOvStretch)))
sem_Presentation_LocatorP location_ child_ =
    (\ _lhsIallFonts
       _lhsIassignedHRef
       _lhsIassignedHeight
       _lhsIassignedVRef
       _lhsIassignedWidth
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIx
       _lhsIy ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 330, column 7)
              _lhsOunfoldedTree =
                  LocatorP location_ _childIunfoldedTree
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 938, column 7)
              _lhsOarrangement =
                  LocatorA location_ _childIarrangement
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 987, column 7)
              _childOoldArr =
                  case _lhsIoldArr of LocatorA _ arr -> arr
                                      _              -> debug Arr "" EmptyA NoIDA 0 0 0 0 0 0
              -- copy rule (up)
              _lhsOallFonts =
                  _childIallFonts
              -- copy rule (up)
              _lhsOfinalHRef =
                  _childIfinalHRef
              -- copy rule (up)
              _lhsOfinalHeight =
                  _childIfinalHeight
              -- copy rule (up)
              _lhsOfinalVRef =
                  _childIfinalVRef
              -- copy rule (up)
              _lhsOfinalWidth =
                  _childIfinalWidth
              -- copy rule (up)
              _lhsOhRf =
                  _childIhRf
              -- copy rule (up)
              _lhsOhStretch =
                  _childIhStretch
              -- copy rule (up)
              _lhsOmaxFormatterDepth =
                  _childImaxFormatterDepth
              -- copy rule (up)
              _lhsOminHeight =
                  _childIminHeight
              -- copy rule (up)
              _lhsOminWidth =
                  _childIminWidth
              -- copy rule (up)
              _lhsOvRf =
                  _childIvRf
              -- copy rule (up)
              _lhsOvStretch =
                  _childIvStretch
              -- copy rule (down)
              _childOallFonts =
                  _lhsIallFonts
              -- copy rule (down)
              _childOassignedHRef =
                  _lhsIassignedHRef
              -- copy rule (down)
              _childOassignedHeight =
                  _lhsIassignedHeight
              -- copy rule (down)
              _childOassignedVRef =
                  _lhsIassignedVRef
              -- copy rule (down)
              _childOassignedWidth =
                  _lhsIassignedWidth
              -- copy rule (down)
              _childObackgroundColor =
                  _lhsIbackgroundColor
              -- copy rule (down)
              _childOfillColor =
                  _lhsIfillColor
              -- copy rule (down)
              _childOfont =
                  _lhsIfont
              -- copy rule (down)
              _childOfontMetrics =
                  _lhsIfontMetrics
              -- copy rule (down)
              _childOlineColor =
                  _lhsIlineColor
              -- copy rule (down)
              _childOmouseDown =
                  _lhsImouseDown
              -- copy rule (down)
              _childOpopupMenuItems =
                  _lhsIpopupMenuItems
              -- copy rule (down)
              _childOtextColor =
                  _lhsItextColor
              -- copy rule (down)
              _childOx =
                  _lhsIx
              -- copy rule (down)
              _childOy =
                  _lhsIy
              ( _childIallFonts,_childIarrangement,_childIfinalHRef,_childIfinalHeight,_childIfinalVRef,_childIfinalWidth,_childIhRf,_childIhStretch,_childImaxFormatterDepth,_childIminHeight,_childIminWidth,_childIunfoldedTree,_childIvRf,_childIvStretch) =
                  (child_ _childOallFonts _childOassignedHRef _childOassignedHeight _childOassignedVRef _childOassignedWidth _childObackgroundColor _childOfillColor _childOfont _childOfontMetrics _childOlineColor _childOmouseDown _childOoldArr _childOpopupMenuItems _childOtextColor _childOx _childOy)
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOfinalHRef,_lhsOfinalHeight,_lhsOfinalVRef,_lhsOfinalWidth,_lhsOhRf,_lhsOhStretch,_lhsOmaxFormatterDepth,_lhsOminHeight,_lhsOminWidth,_lhsOunfoldedTree,_lhsOvRf,_lhsOvStretch)))
sem_Presentation_OverlayP id_ presentationList_ =
    (\ _lhsIallFonts
       _lhsIassignedHRef
       _lhsIassignedHeight
       _lhsIassignedVRef
       _lhsIassignedWidth
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIx
       _lhsIy ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 256, column 7)
              _lhsOmaxFormatterDepth =
                  maximum _presentationListImaxFormatterDepthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 306, column 7)
              _lhsOunfoldedTree =
                  OverlayP id_ _presentationListIunfoldedTreeList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 431, column 7)
              _minLeftWidth =
                  maximum _presentationListIvRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 432, column 7)
              _minRightWidth =
                  maximum [ minWidth - vRf | (minWidth, vRf) <- zip _presentationListIminWidthList _presentationListIvRfList ]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 433, column 7)
              _minWidth =
                  _minLeftWidth + _minRightWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 434, column 7)
              _minTopHeight =
                  maximum _presentationListIhRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 435, column 7)
              _minBottomHeight =
                  maximum [ minHeight - hRf | (minHeight, hRf) <- zip _presentationListIminHeightList _presentationListIhRfList ]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 436, column 7)
              _minHeight =
                  _minTopHeight + _minBottomHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 437, column 7)
              _hStretch =
                  and _presentationListIhStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 438, column 7)
              _vStretch =
                  and _presentationListIvStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 439, column 7)
              _hRf =
                  _minTopHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 440, column 7)
              _vRf =
                  _minLeftWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 621, column 7)
              _presentationListOassignedHeightList =
                  assign _presentationListIvStretchList
                  (repeat _lhsIassignedHeight) _presentationListIminHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 623, column 7)
              _presentationListOassignedHRefList =
                  assign _presentationListIvStretchList
                  (repeat _lhsIassignedHRef) _presentationListIhRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 625, column 7)
              _presentationListOassignedWidthList =
                  assign _presentationListIhStretchList
                  (repeat _lhsIassignedWidth) _presentationListIminWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 627, column 7)
              _presentationListOassignedVRefList =
                  assign _presentationListIhStretchList
                  (repeat _lhsIassignedVRef) _presentationListIvRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 732, column 7)
              _finalWidth =
                  _lhsIassignedWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 733, column 7)
              _finalHeight =
                  _lhsIassignedHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 734, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 735, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 818, column 7)
              _presentationListOxList =
                  [ _finalVRef - cvRf | cvRf <- _presentationListIfinalVRefList ]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 819, column 7)
              _presentationListOyList =
                  [ _finalHRef - chRf | chRf <- _presentationListIfinalHRefList ]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 915, column 7)
              _lhsOarrangement =
                  OverlayA (idAFromP id_) _lhsIx _lhsIy _finalWidth _finalHeight _finalHRef _finalVRef _lhsIbackgroundColor _presentationListIarrangementList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 973, column 7)
              _presentationListOoldArrList =
                  case _lhsIoldArr of OverlayA _ _ _ _ _ _ _ _ arrs -> arrs
                                      _                         -> repeat $ EmptyA NoIDA 0 0 0 0 0 0
              -- copy rule (up)
              _lhsOallFonts =
                  _presentationListIallFonts
              -- copy rule (from local)
              _lhsOfinalHRef =
                  _finalHRef
              -- copy rule (from local)
              _lhsOfinalHeight =
                  _finalHeight
              -- copy rule (from local)
              _lhsOfinalVRef =
                  _finalVRef
              -- copy rule (from local)
              _lhsOfinalWidth =
                  _finalWidth
              -- copy rule (from local)
              _lhsOhRf =
                  _hRf
              -- copy rule (from local)
              _lhsOhStretch =
                  _hStretch
              -- copy rule (from local)
              _lhsOminHeight =
                  _minHeight
              -- copy rule (from local)
              _lhsOminWidth =
                  _minWidth
              -- copy rule (from local)
              _lhsOvRf =
                  _vRf
              -- copy rule (from local)
              _lhsOvStretch =
                  _vStretch
              -- copy rule (down)
              _presentationListOallFonts =
                  _lhsIallFonts
              -- copy rule (down)
              _presentationListObackgroundColor =
                  _lhsIbackgroundColor
              -- copy rule (down)
              _presentationListOfillColor =
                  _lhsIfillColor
              -- copy rule (down)
              _presentationListOfont =
                  _lhsIfont
              -- copy rule (down)
              _presentationListOfontMetrics =
                  _lhsIfontMetrics
              -- copy rule (down)
              _presentationListOlineColor =
                  _lhsIlineColor
              -- copy rule (down)
              _presentationListOmouseDown =
                  _lhsImouseDown
              -- copy rule (down)
              _presentationListOpopupMenuItems =
                  _lhsIpopupMenuItems
              -- copy rule (down)
              _presentationListOtextColor =
                  _lhsItextColor
              ( _presentationListIallFonts
               ,_presentationListIarrangementList
               ,_presentationListIfinalHRefList
               ,_presentationListIfinalHeightList
               ,_presentationListIfinalVRefList
               ,_presentationListIfinalWidthList
               ,_presentationListIhRfList
               ,_presentationListIhStretchList
               ,_presentationListImaxFormatterDepthList
               ,_presentationListIminHeightList
               ,_presentationListIminWidthList
               ,_presentationListIunfoldedTreeList
               ,_presentationListIvRfList
               ,_presentationListIvStretchList
               ) =
                  (presentationList_ _presentationListOallFonts
                                     _presentationListOassignedHRefList
                                     _presentationListOassignedHeightList
                                     _presentationListOassignedVRefList
                                     _presentationListOassignedWidthList
                                     _presentationListObackgroundColor
                                     _presentationListOfillColor
                                     _presentationListOfont
                                     _presentationListOfontMetrics
                                     _presentationListOlineColor
                                     _presentationListOmouseDown
                                     _presentationListOoldArrList
                                     _presentationListOpopupMenuItems
                                     _presentationListOtextColor
                                     _presentationListOxList
                                     _presentationListOyList)
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOfinalHRef,_lhsOfinalHeight,_lhsOfinalVRef,_lhsOfinalWidth,_lhsOhRf,_lhsOhStretch,_lhsOmaxFormatterDepth,_lhsOminHeight,_lhsOminWidth,_lhsOunfoldedTree,_lhsOvRf,_lhsOvStretch)))
sem_Presentation_ParsingP id_ child_ =
    (\ _lhsIallFonts
       _lhsIassignedHRef
       _lhsIassignedHeight
       _lhsIassignedVRef
       _lhsIassignedWidth
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIx
       _lhsIy ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 328, column 7)
              _lhsOunfoldedTree =
                  ParsingP id_  _childIunfoldedTree
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 936, column 7)
              _lhsOarrangement =
                  ParsingA (idAFromP id_) _childIarrangement
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 984, column 7)
              _childOoldArr =
                  case _lhsIoldArr of ParsingA _ arr -> arr
                                      _              -> debug Arr "" EmptyA NoIDA 0 0 0 0 0 0
              -- copy rule (up)
              _lhsOallFonts =
                  _childIallFonts
              -- copy rule (up)
              _lhsOfinalHRef =
                  _childIfinalHRef
              -- copy rule (up)
              _lhsOfinalHeight =
                  _childIfinalHeight
              -- copy rule (up)
              _lhsOfinalVRef =
                  _childIfinalVRef
              -- copy rule (up)
              _lhsOfinalWidth =
                  _childIfinalWidth
              -- copy rule (up)
              _lhsOhRf =
                  _childIhRf
              -- copy rule (up)
              _lhsOhStretch =
                  _childIhStretch
              -- copy rule (up)
              _lhsOmaxFormatterDepth =
                  _childImaxFormatterDepth
              -- copy rule (up)
              _lhsOminHeight =
                  _childIminHeight
              -- copy rule (up)
              _lhsOminWidth =
                  _childIminWidth
              -- copy rule (up)
              _lhsOvRf =
                  _childIvRf
              -- copy rule (up)
              _lhsOvStretch =
                  _childIvStretch
              -- copy rule (down)
              _childOallFonts =
                  _lhsIallFonts
              -- copy rule (down)
              _childOassignedHRef =
                  _lhsIassignedHRef
              -- copy rule (down)
              _childOassignedHeight =
                  _lhsIassignedHeight
              -- copy rule (down)
              _childOassignedVRef =
                  _lhsIassignedVRef
              -- copy rule (down)
              _childOassignedWidth =
                  _lhsIassignedWidth
              -- copy rule (down)
              _childObackgroundColor =
                  _lhsIbackgroundColor
              -- copy rule (down)
              _childOfillColor =
                  _lhsIfillColor
              -- copy rule (down)
              _childOfont =
                  _lhsIfont
              -- copy rule (down)
              _childOfontMetrics =
                  _lhsIfontMetrics
              -- copy rule (down)
              _childOlineColor =
                  _lhsIlineColor
              -- copy rule (down)
              _childOmouseDown =
                  _lhsImouseDown
              -- copy rule (down)
              _childOpopupMenuItems =
                  _lhsIpopupMenuItems
              -- copy rule (down)
              _childOtextColor =
                  _lhsItextColor
              -- copy rule (down)
              _childOx =
                  _lhsIx
              -- copy rule (down)
              _childOy =
                  _lhsIy
              ( _childIallFonts,_childIarrangement,_childIfinalHRef,_childIfinalHeight,_childIfinalVRef,_childIfinalWidth,_childIhRf,_childIhStretch,_childImaxFormatterDepth,_childIminHeight,_childIminWidth,_childIunfoldedTree,_childIvRf,_childIvStretch) =
                  (child_ _childOallFonts _childOassignedHRef _childOassignedHeight _childOassignedVRef _childOassignedWidth _childObackgroundColor _childOfillColor _childOfont _childOfontMetrics _childOlineColor _childOmouseDown _childOoldArr _childOpopupMenuItems _childOtextColor _childOx _childOy)
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOfinalHRef,_lhsOfinalHeight,_lhsOfinalVRef,_lhsOfinalWidth,_lhsOhRf,_lhsOhStretch,_lhsOmaxFormatterDepth,_lhsOminHeight,_lhsOminWidth,_lhsOunfoldedTree,_lhsOvRf,_lhsOvStretch)))
sem_Presentation_PolyP id_ pointList_ lineWidth_ =
    (\ _lhsIallFonts
       _lhsIassignedHRef
       _lhsIassignedHeight
       _lhsIassignedVRef
       _lhsIassignedWidth
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIx
       _lhsIy ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 250, column 7)
              _lhsOmaxFormatterDepth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 300, column 7)
              _lhsOunfoldedTree =
                  PolyP id_ pointList_ lineWidth_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 392, column 7)
              _minWidth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 393, column 7)
              _minHeight =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 394, column 7)
              _hStretch =
                  True
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 395, column 7)
              _vStretch =
                  True
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 396, column 7)
              _hRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 397, column 7)
              _vRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 713, column 7)
              _finalWidth =
                  _lhsIassignedWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 714, column 7)
              _finalHeight =
                  _lhsIassignedHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 715, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 716, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 884, column 7)
              _lhsOarrangement =
                  let mkPoint (rx, ry) = ( round (rx * fromIntegral (_finalWidth-1))
                                                     , round (ry * fromIntegral (_finalHeight-1)) )
                  in  PolyA (idAFromP id_) _lhsIx _lhsIy _finalWidth _finalHeight _finalHRef _finalVRef (map mkPoint pointList_) lineWidth_ _lhsIlineColor _lhsIbackgroundColor
              -- copy rule (chain)
              _lhsOallFonts =
                  _lhsIallFonts
              -- copy rule (from local)
              _lhsOfinalHRef =
                  _finalHRef
              -- copy rule (from local)
              _lhsOfinalHeight =
                  _finalHeight
              -- copy rule (from local)
              _lhsOfinalVRef =
                  _finalVRef
              -- copy rule (from local)
              _lhsOfinalWidth =
                  _finalWidth
              -- copy rule (from local)
              _lhsOhRf =
                  _hRf
              -- copy rule (from local)
              _lhsOhStretch =
                  _hStretch
              -- copy rule (from local)
              _lhsOminHeight =
                  _minHeight
              -- copy rule (from local)
              _lhsOminWidth =
                  _minWidth
              -- copy rule (from local)
              _lhsOvRf =
                  _vRf
              -- copy rule (from local)
              _lhsOvStretch =
                  _vStretch
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOfinalHRef,_lhsOfinalHeight,_lhsOfinalVRef,_lhsOfinalWidth,_lhsOhRf,_lhsOhStretch,_lhsOmaxFormatterDepth,_lhsOminHeight,_lhsOminWidth,_lhsOunfoldedTree,_lhsOvRf,_lhsOvStretch)))
sem_Presentation_RectangleP id_ w_ h_ lineWidth_ =
    (\ _lhsIallFonts
       _lhsIassignedHRef
       _lhsIassignedHeight
       _lhsIassignedVRef
       _lhsIassignedWidth
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIx
       _lhsIy ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 244, column 7)
              _lhsOmaxFormatterDepth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 294, column 7)
              _lhsOunfoldedTree =
                  RectangleP id_ w_ h_ lineWidth_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 371, column 7)
              _minWidth =
                  w_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 372, column 7)
              _minHeight =
                  h_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 373, column 7)
              _hStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 374, column 7)
              _vStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 375, column 7)
              _hRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 376, column 7)
              _vRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 698, column 7)
              _finalWidth =
                  _minWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 699, column 7)
              _finalHeight =
                  _minHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 700, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 701, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 878, column 7)
              _lhsOarrangement =
                  RectangleA (idAFromP id_) _lhsIx _lhsIy _finalWidth _finalHeight _finalHRef _finalVRef lineWidth_ Solid _lhsIlineColor _lhsIfillColor
              -- copy rule (chain)
              _lhsOallFonts =
                  _lhsIallFonts
              -- copy rule (from local)
              _lhsOfinalHRef =
                  _finalHRef
              -- copy rule (from local)
              _lhsOfinalHeight =
                  _finalHeight
              -- copy rule (from local)
              _lhsOfinalVRef =
                  _finalVRef
              -- copy rule (from local)
              _lhsOfinalWidth =
                  _finalWidth
              -- copy rule (from local)
              _lhsOhRf =
                  _hRf
              -- copy rule (from local)
              _lhsOhStretch =
                  _hStretch
              -- copy rule (from local)
              _lhsOminHeight =
                  _minHeight
              -- copy rule (from local)
              _lhsOminWidth =
                  _minWidth
              -- copy rule (from local)
              _lhsOvRf =
                  _vRf
              -- copy rule (from local)
              _lhsOvStretch =
                  _vStretch
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOfinalHRef,_lhsOfinalHeight,_lhsOfinalVRef,_lhsOfinalWidth,_lhsOhRf,_lhsOhStretch,_lhsOmaxFormatterDepth,_lhsOminHeight,_lhsOminWidth,_lhsOunfoldedTree,_lhsOvRf,_lhsOvStretch)))
sem_Presentation_RowP id_ vRefNr_ presentationList_ =
    (\ _lhsIallFonts
       _lhsIassignedHRef
       _lhsIassignedHeight
       _lhsIassignedVRef
       _lhsIassignedWidth
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIx
       _lhsIy ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 252, column 7)
              _lhsOmaxFormatterDepth =
                  maximum _presentationListImaxFormatterDepthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 302, column 7)
              _lhsOunfoldedTree =
                  RowP id_ vRefNr_ _presentationListIunfoldedTreeList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 407, column 7)
              _minTopHeight =
                  if null _presentationListIhRfList then 0
                     else maximum _presentationListIhRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 409, column 7)
              _minBottomHeight =
                  if null _presentationListIhRfList then 0
                  else maximum [ minHeight - hRf | (minHeight, hRf) <- zip _presentationListIminHeightList _presentationListIhRfList ]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 411, column 7)
              _minWidth =
                  sum _presentationListIminWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 412, column 7)
              _minHeight =
                  _minTopHeight + _minBottomHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 413, column 7)
              _hStretch =
                  or _presentationListIhStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 414, column 7)
              _vStretch =
                  and _presentationListIvStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 415, column 7)
              _hRf =
                  _minTopHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 416, column 7)
              _vRf =
                  if null _presentationListIvRfList then 0
                  else sum (take vRefNr_ _presentationListIminWidthList) + _presentationListIvRfList !! vRefNr_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 535, column 7)
              _assignedWidths =
                  if _presentationListIhStretchList!! vRefNr_
                        then assign _presentationListIhStretchList
                                      (repeat((_leftTotalSpace+ _rightTotalSpace)`div`(_leftHStretches+1+ _rightHStretches))
                                      )
                                      _presentationListIminWidthList
                        else assign _presentationListIhStretchList
                                      (  replicate vRefNr_ (_leftChildSpace)
                                      ++ [0]
                                      ++ repeat _rightChildSpace
                                      )
                                      _presentationListIminWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 547, column 7)
              _presentationListOassignedWidthList =
                  _assignedWidths
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 548, column 7)
              _presentationListOassignedVRefList =
                  _presentationListIvRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 550, column 7)
              _presentationListOassignedHeightList =
                  assign _presentationListIvStretchList
                  (repeat _lhsIassignedHeight) _presentationListIminHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 552, column 7)
              _presentationListOassignedHRefList =
                  assign _presentationListIvStretchList
                  (repeat _hRf)
                                       _presentationListIhRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 558, column 7)
              _stretchMinWidths =
                  assign _presentationListIhStretchList _presentationListIminWidthList (repeat 0)
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 561, column 7)
              _leftCorrection =
                  sum (take vRefNr_ _stretchMinWidths)
                     + if _presentationListIhStretchList!! vRefNr_ then _stretchMinWidths !! vRefNr_ else 0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 563, column 7)
              _rightCorrection =
                  sum (drop (vRefNr_+1) _stretchMinWidths)
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 565, column 7)
              _leftTotalSpace =
                  _lhsIassignedVRef - _vRf    + _leftCorrection
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 566, column 7)
              _rightTotalSpace =
                  _lhsIassignedWidth - _minWidth - _leftTotalSpace  + _rightCorrection
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 567, column 7)
              _leftHStretches =
                  length . filter (==True) . take vRefNr_ $ _presentationListIhStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 568, column 7)
              _rightHStretches =
                  length . filter (==True) . drop (vRefNr_+1) $ _presentationListIhStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 569, column 7)
              _leftChildSpace =
                  round (fromIntegral _leftTotalSpace / fromIntegral _leftHStretches )
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 570, column 7)
              _rightChildSpace =
                  round (fromIntegral _rightTotalSpace / fromIntegral _rightHStretches )
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 718, column 7)
              _finalWidth =
                  sum _presentationListIfinalWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 719, column 7)
              _finalHeight =
                  _lhsIassignedHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 720, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 721, column 7)
              _finalVRef =
                  if null _presentationListIvRfList then 0
                  else if _presentationListIhStretchList!! vRefNr_ then _lhsIassignedVRef
                          else sum (take vRefNr_ _assignedWidths) + (_presentationListIvRfList !! vRefNr_)
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 812, column 7)
              _presentationListOxList =
                  init.scanl (+) 0 $ _presentationListIfinalWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 813, column 7)
              _presentationListOyList =
                  [ _finalHRef - chRf | chRf <- _presentationListIfinalHRefList ]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 893, column 7)
              _lhsOarrangement =
                  RowA (idAFromP id_) _lhsIx _lhsIy _finalWidth _finalHeight _finalHRef _finalVRef _lhsIbackgroundColor _presentationListIarrangementList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 967, column 7)
              _presentationListOoldArrList =
                  case _lhsIoldArr of RowA _ _ _ _ _ _ _ _ arrs -> arrs
                                      _                     -> repeat $ EmptyA NoIDA 0 0 0 0 0 0
              -- copy rule (up)
              _lhsOallFonts =
                  _presentationListIallFonts
              -- copy rule (from local)
              _lhsOfinalHRef =
                  _finalHRef
              -- copy rule (from local)
              _lhsOfinalHeight =
                  _finalHeight
              -- copy rule (from local)
              _lhsOfinalVRef =
                  _finalVRef
              -- copy rule (from local)
              _lhsOfinalWidth =
                  _finalWidth
              -- copy rule (from local)
              _lhsOhRf =
                  _hRf
              -- copy rule (from local)
              _lhsOhStretch =
                  _hStretch
              -- copy rule (from local)
              _lhsOminHeight =
                  _minHeight
              -- copy rule (from local)
              _lhsOminWidth =
                  _minWidth
              -- copy rule (from local)
              _lhsOvRf =
                  _vRf
              -- copy rule (from local)
              _lhsOvStretch =
                  _vStretch
              -- copy rule (down)
              _presentationListOallFonts =
                  _lhsIallFonts
              -- copy rule (down)
              _presentationListObackgroundColor =
                  _lhsIbackgroundColor
              -- copy rule (down)
              _presentationListOfillColor =
                  _lhsIfillColor
              -- copy rule (down)
              _presentationListOfont =
                  _lhsIfont
              -- copy rule (down)
              _presentationListOfontMetrics =
                  _lhsIfontMetrics
              -- copy rule (down)
              _presentationListOlineColor =
                  _lhsIlineColor
              -- copy rule (down)
              _presentationListOmouseDown =
                  _lhsImouseDown
              -- copy rule (down)
              _presentationListOpopupMenuItems =
                  _lhsIpopupMenuItems
              -- copy rule (down)
              _presentationListOtextColor =
                  _lhsItextColor
              ( _presentationListIallFonts
               ,_presentationListIarrangementList
               ,_presentationListIfinalHRefList
               ,_presentationListIfinalHeightList
               ,_presentationListIfinalVRefList
               ,_presentationListIfinalWidthList
               ,_presentationListIhRfList
               ,_presentationListIhStretchList
               ,_presentationListImaxFormatterDepthList
               ,_presentationListIminHeightList
               ,_presentationListIminWidthList
               ,_presentationListIunfoldedTreeList
               ,_presentationListIvRfList
               ,_presentationListIvStretchList
               ) =
                  (presentationList_ _presentationListOallFonts
                                     _presentationListOassignedHRefList
                                     _presentationListOassignedHeightList
                                     _presentationListOassignedVRefList
                                     _presentationListOassignedWidthList
                                     _presentationListObackgroundColor
                                     _presentationListOfillColor
                                     _presentationListOfont
                                     _presentationListOfontMetrics
                                     _presentationListOlineColor
                                     _presentationListOmouseDown
                                     _presentationListOoldArrList
                                     _presentationListOpopupMenuItems
                                     _presentationListOtextColor
                                     _presentationListOxList
                                     _presentationListOyList)
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOfinalHRef,_lhsOfinalHeight,_lhsOfinalVRef,_lhsOfinalWidth,_lhsOhRf,_lhsOhStretch,_lhsOmaxFormatterDepth,_lhsOminHeight,_lhsOminWidth,_lhsOunfoldedTree,_lhsOvRf,_lhsOvStretch)))
sem_Presentation_StringP id_ text_ =
    (\ _lhsIallFonts
       _lhsIassignedHRef
       _lhsIassignedHeight
       _lhsIassignedVRef
       _lhsIassignedWidth
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIx
       _lhsIy ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 242, column 7)
              _lhsOmaxFormatterDepth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 292, column 7)
              _lhsOunfoldedTree =
                  StringP id_ text_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 364, column 7)
              _minWidth =
                  textWidth _lhsIfontMetrics _lhsIfont text_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 365, column 7)
              _minHeight =
                  charHeight _lhsIfontMetrics _lhsIfont
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 366, column 7)
              _hStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 367, column 7)
              _vStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 368, column 7)
              _hRf =
                  baseLine _lhsIfontMetrics _lhsIfont
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 369, column 7)
              _vRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 693, column 7)
              _finalWidth =
                  _minWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 694, column 7)
              _finalHeight =
                  _minHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 695, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 696, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 876, column 7)
              _lhsOarrangement =
                  StringA (idAFromP id_) _lhsIx _lhsIy _finalWidth _finalHeight _finalHRef _finalVRef text_ _lhsItextColor _lhsIfont (cumulativeCharWidths _lhsIfontMetrics _lhsIfont text_)
              -- copy rule (chain)
              _lhsOallFonts =
                  _lhsIallFonts
              -- copy rule (from local)
              _lhsOfinalHRef =
                  _finalHRef
              -- copy rule (from local)
              _lhsOfinalHeight =
                  _finalHeight
              -- copy rule (from local)
              _lhsOfinalVRef =
                  _finalVRef
              -- copy rule (from local)
              _lhsOfinalWidth =
                  _finalWidth
              -- copy rule (from local)
              _lhsOhRf =
                  _hRf
              -- copy rule (from local)
              _lhsOhStretch =
                  _hStretch
              -- copy rule (from local)
              _lhsOminHeight =
                  _minHeight
              -- copy rule (from local)
              _lhsOminWidth =
                  _minWidth
              -- copy rule (from local)
              _lhsOvRf =
                  _vRf
              -- copy rule (from local)
              _lhsOvStretch =
                  _vStretch
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOfinalHRef,_lhsOfinalHeight,_lhsOfinalVRef,_lhsOfinalWidth,_lhsOhRf,_lhsOhStretch,_lhsOmaxFormatterDepth,_lhsOminHeight,_lhsOminWidth,_lhsOunfoldedTree,_lhsOvRf,_lhsOvStretch)))
sem_Presentation_StructuralP id_ child_ =
    (\ _lhsIallFonts
       _lhsIassignedHRef
       _lhsIassignedHeight
       _lhsIassignedVRef
       _lhsIassignedWidth
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIx
       _lhsIy ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 326, column 7)
              _lhsOunfoldedTree =
                  StructuralP id_  _childIunfoldedTree
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 934, column 7)
              _lhsOarrangement =
                  StructuralA (idAFromP id_) _childIarrangement
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 981, column 7)
              _childOoldArr =
                  case _lhsIoldArr of StructuralA _ arr -> arr
                                      _                 -> debug Arr "" EmptyA NoIDA 0 0 0 0 0 0
              -- copy rule (up)
              _lhsOallFonts =
                  _childIallFonts
              -- copy rule (up)
              _lhsOfinalHRef =
                  _childIfinalHRef
              -- copy rule (up)
              _lhsOfinalHeight =
                  _childIfinalHeight
              -- copy rule (up)
              _lhsOfinalVRef =
                  _childIfinalVRef
              -- copy rule (up)
              _lhsOfinalWidth =
                  _childIfinalWidth
              -- copy rule (up)
              _lhsOhRf =
                  _childIhRf
              -- copy rule (up)
              _lhsOhStretch =
                  _childIhStretch
              -- copy rule (up)
              _lhsOmaxFormatterDepth =
                  _childImaxFormatterDepth
              -- copy rule (up)
              _lhsOminHeight =
                  _childIminHeight
              -- copy rule (up)
              _lhsOminWidth =
                  _childIminWidth
              -- copy rule (up)
              _lhsOvRf =
                  _childIvRf
              -- copy rule (up)
              _lhsOvStretch =
                  _childIvStretch
              -- copy rule (down)
              _childOallFonts =
                  _lhsIallFonts
              -- copy rule (down)
              _childOassignedHRef =
                  _lhsIassignedHRef
              -- copy rule (down)
              _childOassignedHeight =
                  _lhsIassignedHeight
              -- copy rule (down)
              _childOassignedVRef =
                  _lhsIassignedVRef
              -- copy rule (down)
              _childOassignedWidth =
                  _lhsIassignedWidth
              -- copy rule (down)
              _childObackgroundColor =
                  _lhsIbackgroundColor
              -- copy rule (down)
              _childOfillColor =
                  _lhsIfillColor
              -- copy rule (down)
              _childOfont =
                  _lhsIfont
              -- copy rule (down)
              _childOfontMetrics =
                  _lhsIfontMetrics
              -- copy rule (down)
              _childOlineColor =
                  _lhsIlineColor
              -- copy rule (down)
              _childOmouseDown =
                  _lhsImouseDown
              -- copy rule (down)
              _childOpopupMenuItems =
                  _lhsIpopupMenuItems
              -- copy rule (down)
              _childOtextColor =
                  _lhsItextColor
              -- copy rule (down)
              _childOx =
                  _lhsIx
              -- copy rule (down)
              _childOy =
                  _lhsIy
              ( _childIallFonts,_childIarrangement,_childIfinalHRef,_childIfinalHeight,_childIfinalVRef,_childIfinalWidth,_childIhRf,_childIhStretch,_childImaxFormatterDepth,_childIminHeight,_childIminWidth,_childIunfoldedTree,_childIvRf,_childIvStretch) =
                  (child_ _childOallFonts _childOassignedHRef _childOassignedHeight _childOassignedVRef _childOassignedWidth _childObackgroundColor _childOfillColor _childOfont _childOfontMetrics _childOlineColor _childOmouseDown _childOoldArr _childOpopupMenuItems _childOtextColor _childOx _childOy)
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOfinalHRef,_lhsOfinalHeight,_lhsOfinalVRef,_lhsOfinalWidth,_lhsOhRf,_lhsOhStretch,_lhsOmaxFormatterDepth,_lhsOminHeight,_lhsOminWidth,_lhsOunfoldedTree,_lhsOvRf,_lhsOvStretch)))
sem_Presentation_VertexP id_ x_ y_ outline_ child_ =
    (\ _lhsIallFonts
       _lhsIassignedHRef
       _lhsIassignedHeight
       _lhsIassignedVRef
       _lhsIassignedWidth
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIx
       _lhsIy ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 310, column 7)
              _lhsOunfoldedTree =
                  VertexP id_ x_ y_ outline_ _childIunfoldedTree
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 742, column 7)
              _finalWidth =
                  _lhsIassignedWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 743, column 7)
              _finalHeight =
                  _lhsIassignedHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 744, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 745, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 825, column 7)
              _childOx =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 826, column 7)
              _childOy =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 919, column 7)
              _lhsOarrangement =
                  VertexA (idAFromP id_) (_lhsIx + x_ - _finalHRef) (_lhsIy + y_ - _finalVRef)
                          _finalWidth _finalHeight _finalHRef _finalVRef _lhsIbackgroundColor _childIarrangement
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 978, column 7)
              _childOoldArr =
                  case _lhsIoldArr of VertexA _ _ _ _ _ _ _ _ arr -> arr
                                      _                           -> debug Arr "" EmptyA NoIDA 0 0 0 0 0 0
              -- copy rule (up)
              _lhsOallFonts =
                  _childIallFonts
              -- copy rule (from local)
              _lhsOfinalHRef =
                  _finalHRef
              -- copy rule (from local)
              _lhsOfinalHeight =
                  _finalHeight
              -- copy rule (from local)
              _lhsOfinalVRef =
                  _finalVRef
              -- copy rule (from local)
              _lhsOfinalWidth =
                  _finalWidth
              -- copy rule (up)
              _lhsOhRf =
                  _childIhRf
              -- copy rule (up)
              _lhsOhStretch =
                  _childIhStretch
              -- copy rule (up)
              _lhsOmaxFormatterDepth =
                  _childImaxFormatterDepth
              -- copy rule (up)
              _lhsOminHeight =
                  _childIminHeight
              -- copy rule (up)
              _lhsOminWidth =
                  _childIminWidth
              -- copy rule (up)
              _lhsOvRf =
                  _childIvRf
              -- copy rule (up)
              _lhsOvStretch =
                  _childIvStretch
              -- copy rule (down)
              _childOallFonts =
                  _lhsIallFonts
              -- copy rule (down)
              _childOassignedHRef =
                  _lhsIassignedHRef
              -- copy rule (down)
              _childOassignedHeight =
                  _lhsIassignedHeight
              -- copy rule (down)
              _childOassignedVRef =
                  _lhsIassignedVRef
              -- copy rule (down)
              _childOassignedWidth =
                  _lhsIassignedWidth
              -- copy rule (down)
              _childObackgroundColor =
                  _lhsIbackgroundColor
              -- copy rule (down)
              _childOfillColor =
                  _lhsIfillColor
              -- copy rule (down)
              _childOfont =
                  _lhsIfont
              -- copy rule (down)
              _childOfontMetrics =
                  _lhsIfontMetrics
              -- copy rule (down)
              _childOlineColor =
                  _lhsIlineColor
              -- copy rule (down)
              _childOmouseDown =
                  _lhsImouseDown
              -- copy rule (down)
              _childOpopupMenuItems =
                  _lhsIpopupMenuItems
              -- copy rule (down)
              _childOtextColor =
                  _lhsItextColor
              ( _childIallFonts,_childIarrangement,_childIfinalHRef,_childIfinalHeight,_childIfinalVRef,_childIfinalWidth,_childIhRf,_childIhStretch,_childImaxFormatterDepth,_childIminHeight,_childIminWidth,_childIunfoldedTree,_childIvRf,_childIvStretch) =
                  (child_ _childOallFonts _childOassignedHRef _childOassignedHeight _childOassignedVRef _childOassignedWidth _childObackgroundColor _childOfillColor _childOfont _childOfontMetrics _childOlineColor _childOmouseDown _childOoldArr _childOpopupMenuItems _childOtextColor _childOx _childOy)
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOfinalHRef,_lhsOfinalHeight,_lhsOfinalVRef,_lhsOfinalWidth,_lhsOhRf,_lhsOhStretch,_lhsOmaxFormatterDepth,_lhsOminHeight,_lhsOminWidth,_lhsOunfoldedTree,_lhsOvRf,_lhsOvStretch)))
sem_Presentation_WithP attrRule_ child_ =
    (\ _lhsIallFonts
       _lhsIassignedHRef
       _lhsIassignedHeight
       _lhsIassignedVRef
       _lhsIassignedWidth
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIx
       _lhsIy ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 160, column 7)
              _newAttrs =
                  attrRule_ ( Inh _lhsIfont
                                 _lhsItextColor _lhsIlineColor _lhsIfillColor _lhsIbackgroundColor
                                 _lhsImouseDown _lhsIpopupMenuItems
                                 _lhsIassignedWidth _lhsIassignedHeight _lhsIassignedHRef _lhsIassignedVRef
                           , Syn _childIhRf _childIvRf _childIminWidth _childIminHeight
                                 _childIhStretch _childIvStretch
                                 _childIfinalWidth _childIfinalHeight _childIfinalHRef _childIfinalVRef)
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 168, column 7)
              _newSyn =
                  fst _newAttrs
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 169, column 7)
              _newInh =
                  snd _newAttrs
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 170, column 7)
              _childOfont =
                  font _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 171, column 7)
              _childOtextColor =
                  textColor _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 172, column 7)
              _childOlineColor =
                  lineColor _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 173, column 7)
              _childOfillColor =
                  fillColor _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 174, column 7)
              _childObackgroundColor =
                  backgroundColor _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 175, column 7)
              _childOmouseDown =
                  mouseDown _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 176, column 7)
              _childOpopupMenuItems =
                  popupMenuItems _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 177, column 7)
              _childOassignedWidth =
                  assignedWidth _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 178, column 7)
              _childOassignedHeight =
                  assignedHeight _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 179, column 7)
              _childOassignedHRef =
                  assignedHRef _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 180, column 7)
              _childOassignedVRef =
                  assignedVRef _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 181, column 7)
              _lhsOhRf =
                  hRef _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 182, column 7)
              _lhsOvRf =
                  vRef _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 183, column 7)
              _lhsOminWidth =
                  minWidth _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 184, column 7)
              _lhsOminHeight =
                  minHeight _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 185, column 7)
              _lhsOhStretch =
                  hStretch _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 186, column 7)
              _lhsOvStretch =
                  vStretch _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 187, column 7)
              _lhsOfinalWidth =
                  finalWidth _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 188, column 7)
              _lhsOfinalHeight =
                  finalHeight _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 189, column 7)
              _lhsOfinalHRef =
                  finalHRef _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 190, column 7)
              _lhsOfinalVRef =
                  finalVRef _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 210, column 7)
              _childOallFonts =
                  let inhs = _newSyn
                  in  font inhs : _lhsIallFonts
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 324, column 7)
              _lhsOunfoldedTree =
                  WithP attrRule_ _childIunfoldedTree
              -- copy rule (up)
              _lhsOallFonts =
                  _childIallFonts
              -- copy rule (up)
              _lhsOarrangement =
                  _childIarrangement
              -- copy rule (up)
              _lhsOmaxFormatterDepth =
                  _childImaxFormatterDepth
              -- copy rule (down)
              _childOfontMetrics =
                  _lhsIfontMetrics
              -- copy rule (down)
              _childOoldArr =
                  _lhsIoldArr
              -- copy rule (down)
              _childOx =
                  _lhsIx
              -- copy rule (down)
              _childOy =
                  _lhsIy
              ( _childIallFonts,_childIarrangement,_childIfinalHRef,_childIfinalHeight,_childIfinalVRef,_childIfinalWidth,_childIhRf,_childIhStretch,_childImaxFormatterDepth,_childIminHeight,_childIminWidth,_childIunfoldedTree,_childIvRf,_childIvStretch) =
                  (child_ _childOallFonts _childOassignedHRef _childOassignedHeight _childOassignedVRef _childOassignedWidth _childObackgroundColor _childOfillColor _childOfont _childOfontMetrics _childOlineColor _childOmouseDown _childOoldArr _childOpopupMenuItems _childOtextColor _childOx _childOy)
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOfinalHRef,_lhsOfinalHeight,_lhsOfinalVRef,_lhsOfinalWidth,_lhsOhRf,_lhsOhStretch,_lhsOmaxFormatterDepth,_lhsOminHeight,_lhsOminWidth,_lhsOunfoldedTree,_lhsOvRf,_lhsOvStretch)))
-- PresentationList --------------------------------------------
{-
   visit 0:
      inherited attributes:
         assignedHRefList     : [Int]
         assignedHeightList   : [Int]
         assignedVRefList     : [Int]
         assignedWidthList    : [Int]
         backgroundColor      : Color
         fillColor            : Color
         font                 : Font
         fontMetrics          : FontMetrics
         lineColor            : Color
         mouseDown            : Maybe (UpdateDoc doc clip)
         oldArrList           : [Arrangement node]
         popupMenuItems       : [ PopupMenuItem ]
         textColor            : Color
         xList                : [Int]
         yList                : [Int]
      chained attribute:
         allFonts             : [Font]
      synthesized attributes:
         arrangementList      : [Arrangement node]
         finalHRefList        : [Int]
         finalHeightList      : [Int]
         finalVRefList        : [Int]
         finalWidthList       : [Int]
         hRfList              : [Int]
         hStretchList         : [Bool]
         maxFormatterDepthList : [Int]
         minHeightList        : [Int]
         minWidthList         : [Int]
         unfoldedTreeList     : [Presentation]
         vRfList              : [Int]
         vStretchList         : [Bool]
   alternatives:
      alternative Cons:
         child hd             : Presentation
         child tl             : PresentationList
      alternative Nil:
-}
-- cata
sem_PresentationList list =
    (Prelude.foldr sem_PresentationList_Cons sem_PresentationList_Nil (Prelude.map sem_Presentation list))
sem_PresentationList_Cons hd_ tl_ =
    (\ _lhsIallFonts
       _lhsIassignedHRefList
       _lhsIassignedHeightList
       _lhsIassignedVRefList
       _lhsIassignedWidthList
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArrList
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIxList
       _lhsIyList ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 283, column 7)
              _lhsOmaxFormatterDepthList =
                  _hdImaxFormatterDepth : _tlImaxFormatterDepthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 339, column 7)
              _lhsOunfoldedTreeList =
                  _hdIunfoldedTree : _tlIunfoldedTreeList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 516, column 7)
              _lhsOminWidthList =
                  _hdIminWidth : _tlIminWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 517, column 7)
              _lhsOminHeightList =
                  _hdIminHeight : _tlIminHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 518, column 7)
              _lhsOhStretchList =
                  _hdIhStretch : _tlIhStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 519, column 7)
              _lhsOvStretchList =
                  _hdIvStretch : _tlIvStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 520, column 7)
              _lhsOhRfList =
                  _hdIhRf : _tlIhRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 521, column 7)
              _lhsOvRfList =
                  _hdIvRf : _tlIvRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 673, column 7)
              _hdOassignedWidth =
                  head _lhsIassignedWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 674, column 7)
              _tlOassignedWidthList =
                  tail _lhsIassignedWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 675, column 7)
              _hdOassignedHeight =
                  head _lhsIassignedHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 676, column 7)
              _tlOassignedHeightList =
                  tail _lhsIassignedHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 677, column 7)
              _hdOassignedHRef =
                  head _lhsIassignedHRefList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 678, column 7)
              _tlOassignedHRefList =
                  tail _lhsIassignedHRefList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 679, column 7)
              _hdOassignedVRef =
                  head _lhsIassignedVRefList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 680, column 7)
              _tlOassignedVRefList =
                  tail _lhsIassignedVRefList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 793, column 7)
              _lhsOfinalWidthList =
                  _hdIfinalWidth  : _tlIfinalWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 794, column 7)
              _lhsOfinalHeightList =
                  _hdIfinalHeight : _tlIfinalHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 795, column 7)
              _lhsOfinalHRefList =
                  _hdIfinalHRef   : _tlIfinalHRefList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 796, column 7)
              _lhsOfinalVRefList =
                  _hdIfinalVRef   : _tlIfinalVRefList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 849, column 7)
              _hdOx =
                  head _lhsIxList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 850, column 7)
              _hdOy =
                  head _lhsIyList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 851, column 7)
              _tlOxList =
                  tail _lhsIxList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 852, column 7)
              _tlOyList =
                  tail _lhsIyList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 993, column 7)
              _hdOoldArr =
                  head _lhsIoldArrList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 994, column 7)
              _tlOoldArrList =
                  tail _lhsIoldArrList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 1002, column 7)
              _lhsOarrangementList =
                  _hdIarrangement : _tlIarrangementList
              -- copy rule (up)
              _lhsOallFonts =
                  _tlIallFonts
              -- copy rule (down)
              _hdOallFonts =
                  _lhsIallFonts
              -- copy rule (down)
              _hdObackgroundColor =
                  _lhsIbackgroundColor
              -- copy rule (down)
              _hdOfillColor =
                  _lhsIfillColor
              -- copy rule (down)
              _hdOfont =
                  _lhsIfont
              -- copy rule (down)
              _hdOfontMetrics =
                  _lhsIfontMetrics
              -- copy rule (down)
              _hdOlineColor =
                  _lhsIlineColor
              -- copy rule (down)
              _hdOmouseDown =
                  _lhsImouseDown
              -- copy rule (down)
              _hdOpopupMenuItems =
                  _lhsIpopupMenuItems
              -- copy rule (down)
              _hdOtextColor =
                  _lhsItextColor
              -- copy rule (chain)
              _tlOallFonts =
                  _hdIallFonts
              -- copy rule (down)
              _tlObackgroundColor =
                  _lhsIbackgroundColor
              -- copy rule (down)
              _tlOfillColor =
                  _lhsIfillColor
              -- copy rule (down)
              _tlOfont =
                  _lhsIfont
              -- copy rule (down)
              _tlOfontMetrics =
                  _lhsIfontMetrics
              -- copy rule (down)
              _tlOlineColor =
                  _lhsIlineColor
              -- copy rule (down)
              _tlOmouseDown =
                  _lhsImouseDown
              -- copy rule (down)
              _tlOpopupMenuItems =
                  _lhsIpopupMenuItems
              -- copy rule (down)
              _tlOtextColor =
                  _lhsItextColor
              ( _hdIallFonts,_hdIarrangement,_hdIfinalHRef,_hdIfinalHeight,_hdIfinalVRef,_hdIfinalWidth,_hdIhRf,_hdIhStretch,_hdImaxFormatterDepth,_hdIminHeight,_hdIminWidth,_hdIunfoldedTree,_hdIvRf,_hdIvStretch) =
                  (hd_ _hdOallFonts _hdOassignedHRef _hdOassignedHeight _hdOassignedVRef _hdOassignedWidth _hdObackgroundColor _hdOfillColor _hdOfont _hdOfontMetrics _hdOlineColor _hdOmouseDown _hdOoldArr _hdOpopupMenuItems _hdOtextColor _hdOx _hdOy)
              ( _tlIallFonts,_tlIarrangementList,_tlIfinalHRefList,_tlIfinalHeightList,_tlIfinalVRefList,_tlIfinalWidthList,_tlIhRfList,_tlIhStretchList,_tlImaxFormatterDepthList,_tlIminHeightList,_tlIminWidthList,_tlIunfoldedTreeList,_tlIvRfList,_tlIvStretchList) =
                  (tl_ _tlOallFonts _tlOassignedHRefList _tlOassignedHeightList _tlOassignedVRefList _tlOassignedWidthList _tlObackgroundColor _tlOfillColor _tlOfont _tlOfontMetrics _tlOlineColor _tlOmouseDown _tlOoldArrList _tlOpopupMenuItems _tlOtextColor _tlOxList _tlOyList)
          in  ( _lhsOallFonts,_lhsOarrangementList,_lhsOfinalHRefList,_lhsOfinalHeightList,_lhsOfinalVRefList,_lhsOfinalWidthList,_lhsOhRfList,_lhsOhStretchList,_lhsOmaxFormatterDepthList,_lhsOminHeightList,_lhsOminWidthList,_lhsOunfoldedTreeList,_lhsOvRfList,_lhsOvStretchList)))
sem_PresentationList_Nil  =
    (\ _lhsIallFonts
       _lhsIassignedHRefList
       _lhsIassignedHeightList
       _lhsIassignedVRefList
       _lhsIassignedWidthList
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArrList
       _lhsIpopupMenuItems
       _lhsItextColor
       _lhsIxList
       _lhsIyList ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 281, column 7)
              _lhsOmaxFormatterDepthList =
                  [0]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 337, column 7)
              _lhsOunfoldedTreeList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 509, column 7)
              _lhsOminWidthList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 510, column 7)
              _lhsOminHeightList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 511, column 7)
              _lhsOhStretchList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 512, column 7)
              _lhsOvStretchList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 513, column 7)
              _lhsOhRfList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 514, column 7)
              _lhsOvRfList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 788, column 7)
              _lhsOfinalWidthList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 789, column 7)
              _lhsOfinalHeightList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 790, column 7)
              _lhsOfinalHRefList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 791, column 7)
              _lhsOfinalVRefList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 1000, column 7)
              _lhsOarrangementList =
                  []
              -- copy rule (chain)
              _lhsOallFonts =
                  _lhsIallFonts
          in  ( _lhsOallFonts,_lhsOarrangementList,_lhsOfinalHRefList,_lhsOfinalHeightList,_lhsOfinalVRefList,_lhsOfinalWidthList,_lhsOhRfList,_lhsOhStretchList,_lhsOmaxFormatterDepthList,_lhsOminHeightList,_lhsOminWidthList,_lhsOunfoldedTreeList,_lhsOvRfList,_lhsOvStretchList)))
-- Root --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         backgroundColor      : Color
         fillColor            : Color
         focus                : FocusPres
         font                 : Font
         fontMetrics          : FontMetrics
         lineColor            : Color
         mouseDown            : Maybe (UpdateDoc doc clip)
         oldArr               : Arrangement node
         popupMenuItems       : [ PopupMenuItem ]
         screenWidth          : Int
         textColor            : Color
      chained attribute:
         allFonts             : [Font]
      synthesized attributes:
         arrangement          : Arrangement node
         maxFormatterDepth    : Int
         unfoldedTree         : Presentation
   alternatives:
      alternative Root:
         child presentation   : Presentation
-}
-- cata
sem_Root (Root _presentation) =
    (sem_Root_Root (sem_Presentation _presentation))
sem_Root_Root presentation_ =
    (\ _lhsIallFonts
       _lhsIbackgroundColor
       _lhsIfillColor
       _lhsIfocus
       _lhsIfont
       _lhsIfontMetrics
       _lhsIlineColor
       _lhsImouseDown
       _lhsIoldArr
       _lhsIpopupMenuItems
       _lhsIscreenWidth
       _lhsItextColor ->
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 145, column 7)
              _lhsOmaxFormatterDepth =
                  _presentationImaxFormatterDepth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 146, column 7)
              _lhsOunfoldedTree =
                  _presentationIunfoldedTree
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 147, column 7)
              _lhsOarrangement =
                  _presentationIarrangement
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 528, column 7)
              _presentationOassignedWidth =
                  if _presentationIhStretch then _lhsIscreenWidth else _presentationIminWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 529, column 7)
              _presentationOassignedHeight =
                  if _presentationIvStretch then 300 else _presentationIminHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 530, column 7)
              _presentationOassignedHRef =
                  _presentationIhRf
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 531, column 7)
              _presentationOassignedVRef =
                  _presentationIvRf
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 806, column 7)
              _presentationOx =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 807, column 7)
              _presentationOy =
                  0
              -- copy rule (up)
              _lhsOallFonts =
                  _presentationIallFonts
              -- copy rule (down)
              _presentationOallFonts =
                  _lhsIallFonts
              -- copy rule (down)
              _presentationObackgroundColor =
                  _lhsIbackgroundColor
              -- copy rule (down)
              _presentationOfillColor =
                  _lhsIfillColor
              -- copy rule (down)
              _presentationOfont =
                  _lhsIfont
              -- copy rule (down)
              _presentationOfontMetrics =
                  _lhsIfontMetrics
              -- copy rule (down)
              _presentationOlineColor =
                  _lhsIlineColor
              -- copy rule (down)
              _presentationOmouseDown =
                  _lhsImouseDown
              -- copy rule (down)
              _presentationOoldArr =
                  _lhsIoldArr
              -- copy rule (down)
              _presentationOpopupMenuItems =
                  _lhsIpopupMenuItems
              -- copy rule (down)
              _presentationOtextColor =
                  _lhsItextColor
              ( _presentationIallFonts,_presentationIarrangement,_presentationIfinalHRef,_presentationIfinalHeight,_presentationIfinalVRef,_presentationIfinalWidth,_presentationIhRf,_presentationIhStretch,_presentationImaxFormatterDepth,_presentationIminHeight,_presentationIminWidth,_presentationIunfoldedTree,_presentationIvRf,_presentationIvStretch) =
                  (presentation_ _presentationOallFonts _presentationOassignedHRef _presentationOassignedHeight _presentationOassignedVRef _presentationOassignedWidth _presentationObackgroundColor _presentationOfillColor _presentationOfont _presentationOfontMetrics _presentationOlineColor _presentationOmouseDown _presentationOoldArr _presentationOpopupMenuItems _presentationOtextColor _presentationOx _presentationOy)
          in  ( _lhsOallFonts,_lhsOarrangement,_lhsOmaxFormatterDepth,_lhsOunfoldedTree)))
