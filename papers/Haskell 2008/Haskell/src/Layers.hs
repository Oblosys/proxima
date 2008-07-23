{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module Layers where

type LayerFn horArgs vertArg horRess vertRes =
       horArgs -> vertArg -> (vertRes, horRess)


data Simple' state map doc pres gest upd =
       Simple' { present' ::   LayerFn state doc map pres
              , interpret' :: LayerFn (map, state) gest state upd
              }

data Simple state map doc pres gest upd =
       Simple { present ::   LayerFn state doc (map, state) pres
               , interpret :: LayerFn (map, state) gest state upd
               }

wrap :: Simple' state map doc pres gest upd -> Simple state map doc pres gest upd 
wrap (Simple' present' interpret') =
  Simple { present = \state doc -> let (pres, map) = present' state doc
                                     in (pres, (map, state))
         , interpret = interpret'
         }


data Document  = Document String deriving Show
data Presentation  = Presentation String deriving Show
data Arrangement  = Arrangement String deriving Show
data Rendering  = Rendering String deriving Show


data Mapping0
data Mapping1
data Mapping2

data State0
data State1
data State2

data EditDocument = EditDocument deriving Show
data EditPresentation = EditPresentation deriving Show
data EditArrangement = EditArrangement deriving Show
data EditRendering = EditRendering deriving Show


data Gesture
 
initDoc :: IO Document
initDoc = undefined

initStates :: IO (State0, State1, State2)
initStates = undefined

layer0 :: Simple State0 Mapping0 Document Presentation (EditPresentation) (EditDocument)
layer0 = undefined

layer1 :: Simple State1 Mapping1 Presentation Arrangement (EditArrangement) (EditPresentation)
layer1 = undefined

layer2 :: Simple State2 Mapping2 Arrangement Rendering (EditRendering) (EditArrangement)
layer2 = undefined

updateDocument :: EditDocument -> Document -> Document
updateDocument = undefined

getGesture :: IO EditRendering
getGesture = undefined

showRendering :: Rendering  -> IO () 
showRendering = undefined

