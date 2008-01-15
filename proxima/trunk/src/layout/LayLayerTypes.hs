module LayLayerTypes ( module LayTypes
                     , module LayLayerTypes    ) where


import CommonTypes
--import PresTypes -- already imported via LayoutTypes
import LayTypes


type LayerStateLay doc node clip = Layout doc node clip -- clipboard
-- is this  layer local state, instead of level local state? It is focus related, so probably, it should be 
-- local to LayoutLevel, just like the focus

type ScannerSheet doc node clip token = IDPCounter -> Maybe node -> Layout doc node clip ->
                             (Presentation doc node clip token, WhitespaceMap, IDPCounter)