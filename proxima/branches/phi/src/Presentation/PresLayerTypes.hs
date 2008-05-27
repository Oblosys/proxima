module Presentation.PresLayerTypes ( module Evaluation.EnrTypes
                      , module Presentation.PresTypes
                      , module Presentation.PresLayerTypes    ) where


import Common.CommonTypes
import Evaluation.EnrTypes hiding (DocumentLevel)
import Presentation.PresTypes

type LayerStatePres = ()

type PresentationSheet doc enr node clip token = enr -> doc -> FocusDoc -> WhitespaceMap -> IDPCounter -> 
                         (WhitespaceMap, IDPCounter, Presentation doc node clip token,  enr)
                         
type ParseSheet doc enr node clip token = ListParser doc node clip token (enr,WhitespaceMap)
