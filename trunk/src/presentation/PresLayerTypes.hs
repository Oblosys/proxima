module PresLayerTypes ( module EnrTypes
                      , module PresTypes
                      , module PresLayerTypes    ) where


import CommonTypes
import EnrTypes hiding (DocumentLevel)
import PresTypes


type LayerStatePres = ()

type PresentationSheet doc enr node = enr -> FocusDoc -> LayoutMap -> IDPCounter -> 
                         (LayoutMap, IDPCounter, Presentation doc node, enr)
                         
type ParseSheet doc enr node = Presentation doc node -> Maybe enr
