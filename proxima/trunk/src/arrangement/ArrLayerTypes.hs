module ArrLayerTypes ( module LayTypes
                     , module ArrTypes
                     , module ArrLayerTypes    ) where


import CommonTypes
import LayTypes
import ArrTypes
import Data.IORef

--import FontLibGTK
import FontLib

data LocalStateArr = LocalStateArr { getFontMetricsRef :: FontMetricsRef
                                   , getLastMousePress :: Maybe (Int,Int)
                                   , getViewedAreaRef :: IORef Rectangle
                                   }
