module PresLayerTypes ( module EnrTypes
                      , module PresTypes
                      , module PresLayerTypes    ) where


import CommonTypes
import EnrTypes hiding (DocumentLevel, Node)
import PresTypes


type LayerStatePres = ()

type PresentationSheet doc node = EnrichedDoc -> FocusDoc -> LayoutMap -> IDPCounter -> 
                         (LayoutMap, IDPCounter, Presentation doc node, EnrichedDoc)
                         
type ParseSheet doc node = Presentation doc node -> Maybe EnrichedDoc
