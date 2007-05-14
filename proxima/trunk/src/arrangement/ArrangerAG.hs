-- UUAGC 0.9.3 (../../proxima/src/arrangement/ArrangerAG.ag)
module ArrangerAG where

import CommonTypes
import ArrLayerTypes
import ArrLayerUtils
import FontLib


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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 885, column 7)
              _lhsOarrangement =
                  setXYWHA _lhsIx _lhsIy _finalWidth _finalHeight _lhsIoldArr
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 891, column 7)
              _minWidth =
                  _finalWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 892, column 7)
              _minHeight =
                  _finalHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 893, column 7)
              _hStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 894, column 7)
              _vStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 895, column 7)
              _hRf =
                  hRefA _lhsIoldArr
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 896, column 7)
              _vRf =
                  vRefA _lhsIoldArr
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 897, column 7)
              _finalWidth =
                  widthA _lhsIoldArr
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 898, column 7)
              _finalHeight =
                  heightA _lhsIoldArr
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 899, column 7)
              _finalHRef =
                  hRefA _lhsIoldArr
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 900, column 7)
              _finalVRef =
                  vRefA _lhsIoldArr
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 901, column 7)
              _unfoldedTree =
                  ArrangedP
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 902, column 7)
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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 243, column 7)
              _lhsOmaxFormatterDepth =
                  maximum _presentationListImaxFormatterDepthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 290, column 7)
              _lhsOunfoldedTree =
                  ColP id_ hRefNr_ _presentationListIunfoldedTreeList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 393, column 7)
              _minLeftWidth =
                  if null _presentationListIvRfList then 0
                  else maximum _presentationListIvRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 395, column 7)
              _minRightWidth =
                  if null _presentationListIvRfList then 0
                  else maximum [ minWidth - vRf | (minWidth, vRf) <- zip _presentationListIminWidthList _presentationListIvRfList ]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 397, column 7)
              _minWidth =
                  _minLeftWidth + _minRightWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 398, column 7)
              _minHeight =
                  sum _presentationListIminHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 399, column 7)
              _hStretch =
                  and _presentationListIhStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 400, column 7)
              _vStretch =
                  or _presentationListIvStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 401, column 7)
              _hRf =
                  if null _presentationListIhRfList then 0
                  else sum (take hRefNr_ _presentationListIminHeightList) + _presentationListIhRfList !! hRefNr_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 403, column 7)
              _vRf =
                  _minLeftWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 549, column 7)
              _presentationListOassignedWidthList =
                  assign _presentationListIhStretchList
                  (repeat _lhsIassignedWidth) _presentationListIminWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 552, column 7)
              _presentationListOassignedVRefList =
                  assign _presentationListIhStretchList
                  (repeat _lhsIassignedVRef)
                  _presentationListIvRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 555, column 7)
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
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 567, column 7)
              _presentationListOassignedHeightList =
                  _assignedHeights
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 568, column 7)
              _presentationListOassignedHRefList =
                  _presentationListIhRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 578, column 7)
              _stretchMinHeights =
                  assign _presentationListIvStretchList _presentationListIminHeightList (repeat 0)
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 581, column 7)
              _topCorrection =
                  sum (take hRefNr_ _stretchMinHeights)
                      + if _presentationListIvStretchList!! hRefNr_ then _stretchMinHeights !! hRefNr_ else 0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 583, column 7)
              _bottomCorrection =
                  sum (drop (hRefNr_+1) _stretchMinHeights)
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 588, column 7)
              _topTotalSpace =
                  _lhsIassignedHRef - _hRf  + _topCorrection
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 589, column 7)
              _bottomTotalSpace =
                  _lhsIassignedHeight - _minHeight - _topTotalSpace   + _bottomCorrection
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 590, column 7)
              _topVStretches =
                  length . filter (==True) . take hRefNr_ $ _presentationListIvStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 591, column 7)
              _bottomVStretches =
                  length . filter (==True) . drop (hRefNr_+1) $ _presentationListIvStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 592, column 7)
              _topChildSpace =
                  round (fromIntegral _topTotalSpace / fromIntegral _topVStretches )
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 593, column 7)
              _bottomChildSpace =
                  round (fromIntegral _bottomTotalSpace / fromIntegral _bottomVStretches )
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 694, column 7)
              _finalWidth =
                  _lhsIassignedWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 695, column 7)
              _finalHeight =
                  sum _presentationListIfinalHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 696, column 7)
              _finalHRef =
                  if null _presentationListIhRfList then 0
                  else if _presentationListIvStretchList!! hRefNr_ then _lhsIassignedHRef
                          else sum (take hRefNr_ _assignedHeights) + (_presentationListIhRfList !! hRefNr_)
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 699, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 774, column 7)
              _presentationListOxList =
                  [ _finalVRef - cvRf | cvRf <- _presentationListIfinalVRefList ]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 775, column 7)
              _presentationListOyList =
                  init.scanl (+) 0 $ _presentationListIfinalHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 855, column 7)
              _lhsOarrangement =
                  ColA (idAFromP id_) _lhsIx _lhsIy _finalWidth _finalHeight _finalHRef _finalVRef _lhsIbackgroundColor _presentationListIarrangementList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 917, column 7)
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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 235, column 7)
              _lhsOmaxFormatterDepth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 282, column 7)
              _lhsOunfoldedTree =
                  EllipseP id_ w_ h_ lineWidth_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 360, column 7)
              _minWidth =
                  w_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 361, column 7)
              _minHeight =
                  h_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 362, column 7)
              _hStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 363, column 7)
              _vStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 364, column 7)
              _hRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 365, column 7)
              _vRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 672, column 7)
              _finalWidth =
                  _minWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 673, column 7)
              _finalHeight =
                  _minHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 674, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 675, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 830, column 7)
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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 229, column 7)
              _lhsOmaxFormatterDepth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 276, column 7)
              _lhsOunfoldedTree =
                  EmptyP id_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 339, column 7)
              _minWidth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 340, column 7)
              _minHeight =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 341, column 7)
              _hStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 342, column 7)
              _vStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 343, column 7)
              _hRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 344, column 7)
              _vRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 657, column 7)
              _finalWidth =
                  _lhsIassignedWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 658, column 7)
              _finalHeight =
                  _lhsIassignedHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 659, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 660, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 824, column 7)
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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 237, column 7)
              _lhsOmaxFormatterDepth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 284, column 7)
              _lhsOunfoldedTree =
                  ImageP id_ src_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 367, column 7)
              _minWidth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 368, column 7)
              _minHeight =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 369, column 7)
              _hStretch =
                  True
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 370, column 7)
              _vStretch =
                  True
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 371, column 7)
              _hRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 372, column 7)
              _vRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 677, column 7)
              _finalWidth =
                  _lhsIassignedWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 678, column 7)
              _finalHeight =
                  _lhsIassignedHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 679, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 680, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 832, column 7)
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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 312, column 7)
              _lhsOunfoldedTree =
                  LocatorP location_ _childIunfoldedTree
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 883, column 7)
              _lhsOarrangement =
                  LocatorA location_ _childIarrangement
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 937, column 7)
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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 245, column 7)
              _lhsOmaxFormatterDepth =
                  maximum _presentationListImaxFormatterDepthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 292, column 7)
              _lhsOunfoldedTree =
                  OverlayP id_ _presentationListIunfoldedTreeList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 405, column 7)
              _minLeftWidth =
                  maximum _presentationListIvRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 406, column 7)
              _minRightWidth =
                  maximum [ minWidth - vRf | (minWidth, vRf) <- zip _presentationListIminWidthList _presentationListIvRfList ]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 407, column 7)
              _minWidth =
                  _minLeftWidth + _minRightWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 408, column 7)
              _minTopHeight =
                  maximum _presentationListIhRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 409, column 7)
              _minBottomHeight =
                  maximum [ minHeight - hRf | (minHeight, hRf) <- zip _presentationListIminHeightList _presentationListIhRfList ]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 410, column 7)
              _minHeight =
                  _minTopHeight + _minBottomHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 411, column 7)
              _hStretch =
                  and _presentationListIhStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 412, column 7)
              _vStretch =
                  and _presentationListIvStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 413, column 7)
              _hRf =
                  _minTopHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 414, column 7)
              _vRf =
                  _minLeftWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 595, column 7)
              _presentationListOassignedHeightList =
                  assign _presentationListIvStretchList
                  (repeat _lhsIassignedHeight) _presentationListIminHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 597, column 7)
              _presentationListOassignedHRefList =
                  assign _presentationListIvStretchList
                  (repeat _lhsIassignedHRef) _presentationListIhRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 599, column 7)
              _presentationListOassignedWidthList =
                  assign _presentationListIhStretchList
                  (repeat _lhsIassignedWidth) _presentationListIminWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 601, column 7)
              _presentationListOassignedVRefList =
                  assign _presentationListIhStretchList
                  (repeat _lhsIassignedVRef) _presentationListIvRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 701, column 7)
              _finalWidth =
                  _lhsIassignedWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 702, column 7)
              _finalHeight =
                  _lhsIassignedHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 703, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 704, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 777, column 7)
              _presentationListOxList =
                  [ _finalVRef - cvRf | cvRf <- _presentationListIfinalVRefList ]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 778, column 7)
              _presentationListOyList =
                  [ _finalHRef - chRf | chRf <- _presentationListIfinalHRefList ]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 865, column 7)
              _lhsOarrangement =
                  OverlayA (idAFromP id_) _lhsIx _lhsIy _finalWidth _finalHeight _finalHRef _finalVRef _lhsIbackgroundColor _presentationListIarrangementList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 922, column 7)
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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 310, column 7)
              _lhsOunfoldedTree =
                  ParsingP id_  _childIunfoldedTree
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 881, column 7)
              _lhsOarrangement =
                  ParsingA (idAFromP id_) _childIarrangement
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 932, column 7)
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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 239, column 7)
              _lhsOmaxFormatterDepth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 286, column 7)
              _lhsOunfoldedTree =
                  PolyP id_ pointList_ lineWidth_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 374, column 7)
              _minWidth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 375, column 7)
              _minHeight =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 376, column 7)
              _hStretch =
                  True
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 377, column 7)
              _vStretch =
                  True
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 378, column 7)
              _hRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 379, column 7)
              _vRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 682, column 7)
              _finalWidth =
                  _lhsIassignedWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 683, column 7)
              _finalHeight =
                  _lhsIassignedHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 684, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 685, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 834, column 7)
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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 233, column 7)
              _lhsOmaxFormatterDepth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 280, column 7)
              _lhsOunfoldedTree =
                  RectangleP id_ w_ h_ lineWidth_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 353, column 7)
              _minWidth =
                  w_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 354, column 7)
              _minHeight =
                  h_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 355, column 7)
              _hStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 356, column 7)
              _vStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 357, column 7)
              _hRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 358, column 7)
              _vRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 667, column 7)
              _finalWidth =
                  _minWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 668, column 7)
              _finalHeight =
                  _minHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 669, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 670, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 828, column 7)
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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 241, column 7)
              _lhsOmaxFormatterDepth =
                  maximum _presentationListImaxFormatterDepthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 288, column 7)
              _lhsOunfoldedTree =
                  RowP id_ vRefNr_ _presentationListIunfoldedTreeList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 381, column 7)
              _minTopHeight =
                  if null _presentationListIhRfList then 0
                     else maximum _presentationListIhRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 383, column 7)
              _minBottomHeight =
                  if null _presentationListIhRfList then 0
                  else maximum [ minHeight - hRf | (minHeight, hRf) <- zip _presentationListIminHeightList _presentationListIhRfList ]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 385, column 7)
              _minWidth =
                  sum _presentationListIminWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 386, column 7)
              _minHeight =
                  _minTopHeight + _minBottomHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 387, column 7)
              _hStretch =
                  or _presentationListIhStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 388, column 7)
              _vStretch =
                  and _presentationListIvStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 389, column 7)
              _hRf =
                  _minTopHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 390, column 7)
              _vRf =
                  if null _presentationListIvRfList then 0
                  else sum (take vRefNr_ _presentationListIminWidthList) + _presentationListIvRfList !! vRefNr_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 509, column 7)
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
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 521, column 7)
              _presentationListOassignedWidthList =
                  _assignedWidths
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 522, column 7)
              _presentationListOassignedVRefList =
                  _presentationListIvRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 524, column 7)
              _presentationListOassignedHeightList =
                  assign _presentationListIvStretchList
                  (repeat _lhsIassignedHeight) _presentationListIminHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 526, column 7)
              _presentationListOassignedHRefList =
                  assign _presentationListIvStretchList
                  (repeat _hRf)
                                       _presentationListIhRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 532, column 7)
              _stretchMinWidths =
                  assign _presentationListIhStretchList _presentationListIminWidthList (repeat 0)
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 535, column 7)
              _leftCorrection =
                  sum (take vRefNr_ _stretchMinWidths)
                     + if _presentationListIhStretchList!! vRefNr_ then _stretchMinWidths !! vRefNr_ else 0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 537, column 7)
              _rightCorrection =
                  sum (drop (vRefNr_+1) _stretchMinWidths)
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 539, column 7)
              _leftTotalSpace =
                  _lhsIassignedVRef - _vRf    + _leftCorrection
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 540, column 7)
              _rightTotalSpace =
                  _lhsIassignedWidth - _minWidth - _leftTotalSpace  + _rightCorrection
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 541, column 7)
              _leftHStretches =
                  length . filter (==True) . take vRefNr_ $ _presentationListIhStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 542, column 7)
              _rightHStretches =
                  length . filter (==True) . drop (vRefNr_+1) $ _presentationListIhStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 543, column 7)
              _leftChildSpace =
                  round (fromIntegral _leftTotalSpace / fromIntegral _leftHStretches )
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 544, column 7)
              _rightChildSpace =
                  round (fromIntegral _rightTotalSpace / fromIntegral _rightHStretches )
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 687, column 7)
              _finalWidth =
                  sum _presentationListIfinalWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 688, column 7)
              _finalHeight =
                  _lhsIassignedHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 689, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 690, column 7)
              _finalVRef =
                  if null _presentationListIvRfList then 0
                  else if _presentationListIhStretchList!! vRefNr_ then _lhsIassignedVRef
                          else sum (take vRefNr_ _assignedWidths) + (_presentationListIvRfList !! vRefNr_)
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 771, column 7)
              _presentationListOxList =
                  init.scanl (+) 0 $ _presentationListIfinalWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 772, column 7)
              _presentationListOyList =
                  [ _finalHRef - chRf | chRf <- _presentationListIfinalHRefList ]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 843, column 7)
              _lhsOarrangement =
                  RowA (idAFromP id_) _lhsIx _lhsIy _finalWidth _finalHeight _finalHRef _finalVRef _lhsIbackgroundColor _presentationListIarrangementList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 912, column 7)
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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 231, column 7)
              _lhsOmaxFormatterDepth =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 278, column 7)
              _lhsOunfoldedTree =
                  StringP id_ text_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 346, column 7)
              _minWidth =
                  textWidth _lhsIfontMetrics _lhsIfont text_
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 347, column 7)
              _minHeight =
                  charHeight _lhsIfontMetrics _lhsIfont
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 348, column 7)
              _hStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 349, column 7)
              _vStretch =
                  False
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 350, column 7)
              _hRf =
                  baseLine _lhsIfontMetrics _lhsIfont
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 351, column 7)
              _vRf =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 662, column 7)
              _finalWidth =
                  _minWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 663, column 7)
              _finalHeight =
                  _minHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 664, column 7)
              _finalHRef =
                  _lhsIassignedHRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 665, column 7)
              _finalVRef =
                  _lhsIassignedVRef
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 826, column 7)
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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 308, column 7)
              _lhsOunfoldedTree =
                  StructuralP id_  _childIunfoldedTree
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 879, column 7)
              _lhsOarrangement =
                  StructuralA (idAFromP id_) _childIarrangement
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 927, column 7)
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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 149, column 7)
              _newAttrs =
                  attrRule_ ( Inh _lhsIfont
                                 _lhsItextColor _lhsIlineColor _lhsIfillColor _lhsIbackgroundColor
                                 _lhsImouseDown _lhsIpopupMenuItems
                                 _lhsIassignedWidth _lhsIassignedHeight _lhsIassignedHRef _lhsIassignedVRef
                           , Syn _childIhRf _childIvRf _childIminWidth _childIminHeight
                                 _childIhStretch _childIvStretch
                                 _childIfinalWidth _childIfinalHeight _childIfinalHRef _childIfinalVRef)
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 157, column 7)
              _newSyn =
                  fst _newAttrs
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 158, column 7)
              _newInh =
                  snd _newAttrs
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 159, column 7)
              _childOfont =
                  font _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 160, column 7)
              _childOtextColor =
                  textColor _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 161, column 7)
              _childOlineColor =
                  lineColor _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 162, column 7)
              _childOfillColor =
                  fillColor _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 163, column 7)
              _childObackgroundColor =
                  backgroundColor _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 164, column 7)
              _childOmouseDown =
                  mouseDown _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 165, column 7)
              _childOpopupMenuItems =
                  popupMenuItems _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 166, column 7)
              _childOassignedWidth =
                  assignedWidth _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 167, column 7)
              _childOassignedHeight =
                  assignedHeight _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 168, column 7)
              _childOassignedHRef =
                  assignedHRef _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 169, column 7)
              _childOassignedVRef =
                  assignedVRef _newSyn
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 170, column 7)
              _lhsOhRf =
                  hRef _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 171, column 7)
              _lhsOvRf =
                  vRef _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 172, column 7)
              _lhsOminWidth =
                  minWidth _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 173, column 7)
              _lhsOminHeight =
                  minHeight _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 174, column 7)
              _lhsOhStretch =
                  hStretch _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 175, column 7)
              _lhsOvStretch =
                  vStretch _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 176, column 7)
              _lhsOfinalWidth =
                  finalWidth _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 177, column 7)
              _lhsOfinalHeight =
                  finalHeight _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 178, column 7)
              _lhsOfinalHRef =
                  finalHRef _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 179, column 7)
              _lhsOfinalVRef =
                  finalVRef _newInh
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 199, column 7)
              _childOallFonts =
                  let inhs = _newSyn
                  in  font inhs : _lhsIallFonts
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 306, column 7)
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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 269, column 7)
              _lhsOmaxFormatterDepthList =
                  _hdImaxFormatterDepth : _tlImaxFormatterDepthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 321, column 7)
              _lhsOunfoldedTreeList =
                  _hdIunfoldedTree : _tlIunfoldedTreeList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 490, column 7)
              _lhsOminWidthList =
                  _hdIminWidth : _tlIminWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 491, column 7)
              _lhsOminHeightList =
                  _hdIminHeight : _tlIminHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 492, column 7)
              _lhsOhStretchList =
                  _hdIhStretch : _tlIhStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 493, column 7)
              _lhsOvStretchList =
                  _hdIvStretch : _tlIvStretchList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 494, column 7)
              _lhsOhRfList =
                  _hdIhRf : _tlIhRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 495, column 7)
              _lhsOvRfList =
                  _hdIvRf : _tlIvRfList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 642, column 7)
              _hdOassignedWidth =
                  head _lhsIassignedWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 643, column 7)
              _tlOassignedWidthList =
                  tail _lhsIassignedWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 644, column 7)
              _hdOassignedHeight =
                  head _lhsIassignedHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 645, column 7)
              _tlOassignedHeightList =
                  tail _lhsIassignedHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 646, column 7)
              _hdOassignedHRef =
                  head _lhsIassignedHRefList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 647, column 7)
              _tlOassignedHRefList =
                  tail _lhsIassignedHRefList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 648, column 7)
              _hdOassignedVRef =
                  head _lhsIassignedVRefList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 649, column 7)
              _tlOassignedVRefList =
                  tail _lhsIassignedVRefList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 752, column 7)
              _lhsOfinalWidthList =
                  _hdIfinalWidth : _tlIfinalWidthList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 753, column 7)
              _lhsOfinalHeightList =
                  _hdIfinalHeight : _tlIfinalHeightList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 754, column 7)
              _lhsOfinalHRefList =
                  _hdIfinalHRef : _tlIfinalHRefList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 755, column 7)
              _lhsOfinalVRefList =
                  _hdIfinalVRef : _tlIfinalVRefList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 801, column 7)
              _hdOx =
                  head _lhsIxList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 802, column 7)
              _hdOy =
                  head _lhsIyList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 803, column 7)
              _tlOxList =
                  tail _lhsIxList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 804, column 7)
              _tlOyList =
                  tail _lhsIyList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 945, column 7)
              _hdOoldArr =
                  head _lhsIoldArrList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 946, column 7)
              _tlOoldArrList =
                  tail _lhsIoldArrList
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 956, column 7)
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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 267, column 7)
              _lhsOmaxFormatterDepthList =
                  [0]
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 319, column 7)
              _lhsOunfoldedTreeList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 483, column 7)
              _lhsOminWidthList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 484, column 7)
              _lhsOminHeightList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 485, column 7)
              _lhsOhStretchList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 486, column 7)
              _lhsOvStretchList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 487, column 7)
              _lhsOhRfList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 488, column 7)
              _lhsOvRfList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 747, column 7)
              _lhsOfinalWidthList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 748, column 7)
              _lhsOfinalHeightList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 749, column 7)
              _lhsOfinalHRefList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 750, column 7)
              _lhsOfinalVRefList =
                  []
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 954, column 7)
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
         (let -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 134, column 7)
              _lhsOmaxFormatterDepth =
                  _presentationImaxFormatterDepth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 135, column 7)
              _lhsOunfoldedTree =
                  _presentationIunfoldedTree
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 136, column 7)
              _lhsOarrangement =
                  _presentationIarrangement
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 502, column 7)
              _presentationOassignedWidth =
                  if _presentationIhStretch then _lhsIscreenWidth else _presentationIminWidth
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 503, column 7)
              _presentationOassignedHeight =
                  if _presentationIvStretch then 300 else _presentationIminHeight
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 504, column 7)
              _presentationOassignedHRef =
                  _presentationIhRf
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 505, column 7)
              _presentationOassignedVRef =
                  _presentationIvRf
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 765, column 7)
              _presentationOx =
                  0
              -- "../../proxima/src/arrangement/ArrangerAG.ag"(line 766, column 7)
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
