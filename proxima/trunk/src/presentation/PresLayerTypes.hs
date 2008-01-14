module PresLayerTypes ( module EnrTypes
                      , module PresTypes
                      , module PresLayerTypes    ) where


import CommonTypes
import EnrTypes hiding (DocumentLevel)
import PresTypes


type LayerStatePres = ()

type PresentationSheet doc enr node clip = enr -> FocusDoc -> WhitespaceMap -> IDPCounter -> 
                         (WhitespaceMap, IDPCounter, Presentation doc node clip, enr)
                         
type ParseSheet doc enr node clip = Presentation doc node clip -> Maybe enr
