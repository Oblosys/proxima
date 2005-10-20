module LayLayerTypes ( module LayTypes
                     , module LayLayerTypes    ) where


import CommonTypes
--import PresTypes -- already imported via LayoutTypes
import LayTypes


type LayerStateLay = Presentation -- clipboard
-- is this  layer local state, instead of level local state? It is focus related, so probably, it should be 
-- local to LayoutLevel, just like the focus

type ScannerSheet doc node = IDPCounter -> Maybe node -> Presentation doc node ->
                             (Presentation doc node, LayoutMap, IDPCounter)