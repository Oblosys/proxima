module PresLayerTypes ( module EnrTypes
                      , module PresTypes
                      , module PresLayerTypes    ) where


import CommonTypes
import EnrTypes
import PresTypes


type LayerStatePres = ()

type PresentationSheet = EnrichedDoc -> FocusDoc -> LayoutMap -> Int -> 
                         (LayoutMap, Int, Presentation, EnrichedDoc)
-- both Ints are Id counters
                         
type ParseSheet = Presentation -> Maybe EnrichedDoc
