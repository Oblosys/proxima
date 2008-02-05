module LayLayerUtils ( module LayUtils
                     , module LayLayerUtils    ) where


import CommonTypes
import LayLayerTypes

import CommonUtils
--import PresUtils imported via LayUtils
import LayUtils


castLayToPres :: Layout doc node clip -> Presentation doc node clip token
castLayToPres = cast

castPresToLay :: Presentation doc node clip token -> Layout doc node clip
castPresToLay (TokenP _ _) = debug Err "LayLayerUtils.castPresToLay: presentation contains tokens" $ EmptyP NoIDP
castPresToLay pres         = cast pres

cast :: Presentation doc node clip token -> Presentation doc node clip token'
cast (EmptyP id)                = EmptyP id
cast (StringP id str)           = StringP id str
cast (ImageP id str st)         = ImageP id str st
cast (PolyP id pts w st)        = PolyP id pts w st
cast (RectangleP id w h lw st)  = RectangleP id w h lw st
cast (EllipseP id w h lw st)    = EllipseP id w h lw st
cast (RowP id rf press)         = RowP id rf $ map cast press
cast (ColP id rf f press)       = ColP id rf f $ map cast press
cast (OverlayP id press)        = OverlayP id $ map cast press
cast (WithP ar pres)            = WithP ar $ cast pres
cast (ParsingP id l pres)       = ParsingP id l $ cast pres
cast (StructuralP id pres)      = StructuralP id $ cast pres
cast (LocatorP l pres)          = LocatorP l $ cast pres
cast (GraphP id d w h es press) = GraphP id d w h es $ map cast press
cast (VertexP id v x y o pres)  = VertexP id v x y o $ cast pres
cast (FormatterP id press)      = FormatterP id $ map cast press

stringFromScanChars :: [ScanChar doc node clip token] -> String
stringFromScanChars [] = ""
stringFromScanChars (sc : scs) =
  (case sc of Char _ c           -> c
              Structural _ _ _ _ -> '@') -- in the Alex scanner, this is \255, this output is only for show
   : stringFromScanChars scs                                    

idPFromScanChars :: [ScanChar doc node clip token] -> IDP
idPFromScanChars [] = NoIDP
idPFromScanChars (Char (IDP idp) _ : scs) = IDP idp
idPFromScanChars (Char NoIDP     _ : scs) = idPFromScanChars scs
idPFromScanChars (Structural (IDP idp) _ _ _ : scs) = IDP idp
idPFromScanChars (Structural NoIDP     _ _ _ : scs) = idPFromScanChars scs

