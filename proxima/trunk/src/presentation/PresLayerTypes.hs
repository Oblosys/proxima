module PresLayerTypes ( module EnrTypes
                      , module PresTypes
                      , module PresLayerTypes    ) where


import CommonTypes
import EnrTypes hiding (DocumentLevel)
import PresTypes


type LayerStatePres = ()

type PresentationSheet doc enr node clip token = enr -> FocusDoc -> WhitespaceMap -> IDPCounter -> 
                         (WhitespaceMap, IDPCounter, Presentation doc node clip token, enr)
                         
type ParseSheet doc enr node clip token = Presentation doc node clip token -> Maybe enr
