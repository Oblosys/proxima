module Layers where

import DPPClass_Lib


data Simple' state mapping doc pres gest upd =
       Simple' { present' ::   LayerFunction state doc mapping pres
               , interpret' :: LayerFunction (mapping, state) gest state upd
               }

data Simple state mapping doc pres gest upd =
       Simple { present ::   LayerFunction state doc (mapping, state) pres
              , interpret :: LayerFunction (mapping, state) gest state upd
              }

wrap :: Simple' state mapping doc pres gest upd -> Simple state mapping doc pres gest upd 
wrap (Simple' present' interpret') =
  Simple { present = \state doc -> let (mapping, pres) = present' state doc
                                   in  ((mapping, state), pres)
         , interpret = interpret'
         }


data Document = Document String deriving Show
data Presentation = Presentation String deriving Show
data Arrangement = Arrangement String deriving Show
data Rendering = Rendering String deriving Show


data Mapping0
data Mapping1
data Mapping2

data State0
data State1
data State2

data EditDocument
data EditPresentation
data EditArrangement
data EditRendering


data Gesture
 
initDoc :: IO Document
initDoc = undefined

initStates :: IO (State0, State1, State2)
initStates = undefined

layer0 :: Simple State0 Mapping0 Document Presentation EditPresentation EditDocument
layer0 = undefined

layer1 :: Simple State1 Mapping1 Presentation Arrangement EditArrangement EditPresentation
layer1 = undefined

layer2 :: Simple State2 Mapping2 Arrangement Rendering EditRendering EditArrangement
layer2 = undefined

updateDocument :: EditDocument -> Document -> Document
updateDocument = undefined

getGesture :: IO EditRendering
getGesture = undefined

showRendering :: Rendering -> IO () 
showRendering = undefined

