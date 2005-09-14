module PresLayerTypes ( module EnrTypes
                      , module PresTypes
                      , module PresLayerTypes    ) where


import CommonTypes
import EnrTypes
import PresTypes


type LayerStatePres = ()

type PresentationSheet node = EnrichedDoc -> FocusDoc -> LayoutMap -> Int -> 
                         (LayoutMap, Int, Presentation node, EnrichedDoc)
-- both Ints are Id counters
                         
type ParseSheet node = Presentation node -> Maybe EnrichedDoc
