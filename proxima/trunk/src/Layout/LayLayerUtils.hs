module Layout.LayLayerUtils ( module Layout.LayUtils
                     , module Layout.LayLayerUtils    ) where


import Common.CommonTypes
import Layout.LayLayerTypes

import Common.CommonUtils
--import PresUtils imported via LayUtils
import Layout.LayUtils


castLayToPres :: Layout doc enr node clip token -> Presentation doc enr node clip token
castLayToPres = cast

castPresToLay :: Presentation doc enr node clip token -> Layout doc enr node clip token
castPresToLay = cast

cast :: PresentationBase doc enr node clip token level -> PresentationBase doc enr node clip token level'
cast (EmptyP id)                = EmptyP id
cast (StringP id str)           = StringP id str
cast (ImageP id str st)         = ImageP id str st
cast (PolyP id pts w st)        = PolyP id pts w st
cast (RectangleP id w h lw st)  = RectangleP id w h lw st
cast (EllipseP id w h lw st)    = EllipseP id w h lw st
cast (RowP id rf press)         = RowP id rf $ map cast press
cast (ColP id rf f press)       = ColP id rf f $ map cast press
cast (OverlayP id d press)      = OverlayP id d $ map cast press
cast (WithP ar pres)            = WithP ar $ cast pres
cast (ParsingP id p l pres)     = ParsingP id p l $ cast pres
cast (StructuralP id pres)      = StructuralP id $ cast pres
cast (LocatorP l pres)          = LocatorP l $ cast pres
cast (TagP t pres)          = TagP t $ cast pres
cast (GraphP id d w h es press) = GraphP id d w h es $ map cast press
cast (VertexP id v x y o pres)  = VertexP id v x y o $ cast pres
cast (FormatterP id press)      = FormatterP id $ map cast press
cast (TokenP _ _)               = debug Err "LayLayerUtils.castPresToLay: presentation contains tokens" $ EmptyP NoIDP


stringFromScanChars :: [ScanChar doc enr node clip token] -> String
stringFromScanChars scs = 
  [ case sc of Char _ _ _ _ c           -> c
               Structural _ _ _ _ _ _ -> '@' -- in the Alex scanner, this is \255, this output is only for show
               Style (ScannedStyleTag _ Start) -> '<' -- in the Alex scanner, this is \255, this output is only for show
               Style (ScannedStyleTag _ End) -> '>' -- in the Alex scanner, this is \255, this output is only for show
  
  | sc <- scs
  ]

-- Retrieve the idp from a list of ScanChars. The returned idp is the idp of the first character that
-- has an idp, or NoIDP if no character has an idp.
idPFromScanChars :: [ScanChar doc enr node clip token] -> IDP
idPFromScanChars [] = NoIDP
idPFromScanChars (Char (IDP idp) _ _ _ _ : scs) = IDP idp
idPFromScanChars (Char NoIDP     _ _ _ _ : scs) = idPFromScanChars scs
idPFromScanChars (Structural _ _ _ _ _ _ : scs) = debug Err "LayLayerUtils.idPFromScanChars called on Structural" $ 
                                                    idPFromScanChars scs
idPFromScanChars (Style _ : scs)                = debug Err "LayLayerUtils.idPFromScanChars called on Style" $
                                                    idPFromScanChars scs

-- Retrieve the first locator node from a list of scanChars. The returned locator is the locator of the first
-- character that has one, or Nothing if no character has a locator.
locFromScanChars :: [ScanChar doc enr node clip token] -> Maybe node
locFromScanChars [] = Nothing
locFromScanChars (Char _ _ _ (Just loc) _ : scs) = Just loc
locFromScanChars (Char _ _ _ Nothing    _ : scs) = locFromScanChars scs
locFromScanChars (Structural _ _ _ _ _ _ : scs)  = debug Err "LayLayerUtils.locFromScanChars called on Structural" $ 
                                                     locFromScanChars scs
locFromScanChars (Style _ : scs)                 = debug Err "LayLayerUtils.locFromScanChars called on Style" $ 
                                                     locFromScanChars scs

markFocusStart sc@(Style _) = debug Err "illegal operation on Style scan char" sc
markFocusStart scanChar = scanChar { startFocusMark = FocusMark }

markFocusEnd sc@(Style _) = debug Err "illegal operation on Style scan char" sc
markFocusEnd scanChar = scanChar { endFocusMark = FocusMark }

hasFocusStartMark sc@(Style _) = debug Err "illegal operation on Style scan char" False
hasFocusStartMark scanChar = startFocusMark scanChar == FocusMark

hasFocusEndMark sc@(Style _) = debug Err "illegal operation on Style scan char" False
hasFocusEndMark scanChar = endFocusMark scanChar == FocusMark

-- TODO handle pattern match failures with internal errors

getFocusStartEnd scs = updateFocusStartEnd 0 (Nothing, Nothing) scs

updateFocusStartEnd :: Int -> FocusStartEnd -> [ScanChar doc enr node clip userToken] -> FocusStartEnd
updateFocusStartEnd i (oldFocusStart, oldFocusEnd) cs =
  (getFocusStart i oldFocusStart cs, getFocusEnd i oldFocusEnd cs) 
  
getFocusStart i oldFocusStart []     = Nothing
getFocusStart i oldFocusStart (c:cs) = if hasFocusStartMark c then Just i else getFocusStart (i+1) oldFocusStart cs

getFocusEnd i oldFocusEnd []     = Nothing
getFocusEnd i oldFocusEnd (c:cs) = if hasFocusEndMark c then Just i else getFocusEnd (i+1) oldFocusEnd cs


scannedStyleFromStyle Bold = ScannedBold
scannedStyleFromStyle Italic = ScannedItalic
scannedStyleFromStyle (FontSize s) = ScannedFontSize s
scannedStyleFromStyle (Color c) = ScannedColor c

