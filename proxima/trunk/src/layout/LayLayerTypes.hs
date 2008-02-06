module LayLayerTypes ( module LayTypes
                     , module LayLayerTypes    ) where


import CommonTypes
--import PresTypes -- already imported via LayoutTypes
import LayTypes


type LayerStateLay doc node clip = Layout doc node clip -- clipboard
-- is this  layer local state, instead of level local state? It is focus related, so probably, it should be 
-- local to LayoutLevel, just like the focus

type ScannerSheet doc node clip token = 
      ( IDPCounter -> Layout doc node clip ->
                             (Presentation doc node clip token, WhitespaceMap, IDPCounter)
      , IDPCounter -> [ScanChar doc node clip token] -> ([Token doc node clip token], IDPCounter, WhitespaceMap)
      )
      
data ScanChar doc node clip token = 
       Char IDP Char 
     | Structural IDP (Maybe node) [Token doc node clip token] (Presentation doc node clip token) -- original pres to show in case of parse/scan errors

