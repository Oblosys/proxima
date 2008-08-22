module Arrangement.ArrLayerTypes ( module Layout.LayTypes
                     , module Arrangement.ArrTypes
                     , module Arrangement.ArrLayerTypes    ) where


import Common.CommonTypes
import Layout.LayTypes
import Arrangement.ArrTypes
import Data.IORef

--import FontLibGTK
import Arrangement.FontLib

data LocalStateArr = LocalStateArr { getFontMetricsRef :: FontMetricsRef
                                   , getLastMousePress :: Maybe (Int,Int)
                                   , getViewedAreaRef :: IORef Rectangle
                                   , getLastViewedArea :: Rectangle
                                   , getIDACounter :: Int
                                   }
