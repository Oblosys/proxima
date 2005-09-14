module PresLayerTypes ( module EnrTypes
                      , module PresTypes
                      , module PresLayerTypes    ) where


import CommonTypes
import EnrTypes hiding (DocumentLevel, Node)
import PresTypes


type LayerStatePres = ()

type PresentationSheet node = EnrichedDoc -> FocusDoc -> LayoutMap -> IDPCounter -> 
                         (LayoutMap, IDPCounter, Presentation node, EnrichedDoc)
                         
type ParseSheet node = Presentation node -> Maybe EnrichedDoc
