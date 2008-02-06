module PresLayerTypes ( module EnrTypes
                      , module PresTypes
                      , module PresLayerTypes    ) where


import CommonTypes
import EnrTypes hiding (DocumentLevel)
import PresTypes

import UU_Parsing

type LayerStatePres = ()

type PresentationSheet doc enr node clip token = enr -> FocusDoc -> WhitespaceMap -> IDPCounter -> 
                         (WhitespaceMap, IDPCounter, Presentation doc node clip token, enr)
                         




type ListParser doc node clip token a = AnaParser [] Pair  (Token doc node clip token) a 

type ParseSheet doc enr node clip token = ListParser doc node clip token enr
