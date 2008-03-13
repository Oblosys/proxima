module Presentation.PresLayerTypes ( module Evaluation.EnrTypes
                      , module Presentation.PresTypes
                      , module Presentation.PresLayerTypes    ) where


import Common.CommonTypes
import Evaluation.EnrTypes hiding (DocumentLevel)
import Presentation.PresTypes

import Common.UU_Parsing

type LayerStatePres = ()

type PresentationSheet doc enr node clip token = enr -> FocusDoc -> WhitespaceMap -> IDPCounter -> Path -> 
                         (WhitespaceMap, IDPCounter, Presentation doc node clip token)
                         
type ListParser doc node clip token a = AnaParser [] Pair  (Token doc node clip token) a 

type ParseSheet doc enr node clip token = ListParser doc node clip token enr
