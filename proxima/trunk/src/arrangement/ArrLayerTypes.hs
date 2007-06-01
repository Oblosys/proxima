module ArrLayerTypes ( module LayTypes
                     , module ArrTypes
                     , module ArrLayerTypes    ) where


import CommonTypes
import LayTypes
import ArrTypes

import FontLib

data LocalStateArr = LocalStateArr { getFontMetricsRef :: FontMetricsRef
                                   , getLastMousePress :: Maybe (Int,Int)
                                   }
