module ProximaLayers where

-- not yet adapted to different param order

import DPP_Lib


data Document = Document String deriving Show
data EnrDocument = EntDocument String deriving Show
data Presentation = Presentation String deriving Show
data Arrangement = Arrangement String deriving Show
data Rendering = Rendering String deriving Show

data Mapping0
data Mapping1
data Mapping2
data Mapping3

type Intermediate0 = EditDocument
data Intermediate1
data Intermediate2
data Intermediate3
data Intermediate4

data State0
data State1
data State2
data State3

data Sheet0
data Sheet1
data Sheet2
data Sheet3


data EditDocument
data EditEnrDocument
data EditPresentation
data EditArrangement
data EditRendering

data EditState0
data EditState1
data EditState2
data EditState3

data Gesture

class Edits doc delta where
  update :: delta -> doc -> doc

instance (Edits doc1 delta1, Edits doc2 delta2) => Edits (doc1, doc2) (delta1, delta2) where
  update (doc1, doc2) (delta1, delta2) = (update doc1 delta1, update doc2 delta2)

instance Edits Document EditDocument where
  update = const id

instance Edits EnrDocument EditEnrDocument where
  update = const id

instance Edits Presentation EditPresentation where
  update = const id

instance Edits Arrangement EditArrangement where
  update = const id

instance Edits Rendering EditRendering where
  update = const id

instance Edits State0 EditState0 where
  update = const id

instance Edits State1 EditState1 where
  update = const id

instance Edits State2 EditState2 where
  update = const id

instance Edits State3 EditState3 where
  update = const id


present0 :: Sheet0 -> State0 -> Document -> (EnrDocument, Mapping0)
present0 = undefined

translate0 :: Sheet0 -> State0 -> Mapping0 -> EnrDocument -> Document ->
              Either Gesture EditEnrDocument -> (Either Gesture EditDocument, EditState0)
translate0 = undefined

h0 :: EditState0 -> Intermediate0 -> (Intermediate1, EditState0)
h0 = undefined

present1 :: Sheet1 -> State1 -> EnrDocument -> (Presentation, Mapping1)
present1 = undefined

translate1 :: Sheet1 -> State1 -> Mapping1 -> Presentation -> EnrDocument ->
              Either Gesture EditPresentation -> (Either Gesture EditEnrDocument, EditState1)
translate1 = undefined

h1 :: EditState1 -> Intermediate1 -> (Intermediate2, EditState1)
h1 = undefined

present2 :: Sheet2 -> State2 -> Presentation -> (Arrangement, Mapping2)
present2 = undefined

translate2 :: Sheet2 -> State2 -> Mapping2 -> Arrangement -> Presentation ->
              Either Gesture EditArrangement -> (Either Gesture EditPresentation, EditState2)
translate2 = undefined

h2 :: EditState2 -> Intermediate2 -> (Intermediate3, EditState2)
h2 = undefined

present3 :: Sheet3 -> State3 -> Arrangement -> (Rendering, Mapping3)
present3 = undefined

translate3 :: Sheet3 -> State3 -> Mapping3 -> Rendering -> Arrangement ->
              Either Gesture EditRendering -> (Either Gesture EditArrangement, EditState3)
translate3 = undefined

h3 :: EditState3 -> Intermediate3 -> (Intermediate4, EditState3)
h3 = undefined

sheet0 :: Sheet0
sheet0 = undefined

sheet1 :: Sheet1
sheet1 = undefined

sheet2 :: Sheet2
sheet2 = undefined

sheet3 :: Sheet3
sheet3 = undefined

initSheets :: IO (Sheet0, Sheet1, Sheet2, Sheet3)
initSheets = undefined


initDoc :: IO Document
initDoc = undefined

initState0 :: IO State0
initState0 = undefined

initState1 :: IO State1
initState1 = undefined

initState2 :: IO State2
initState2 = undefined

initState3 :: IO State3
initState3 = undefined

initStates :: IO (State0, State1, State2, State3)
initStates = undefined

showRendering :: Rendering -> IO ()
showRendering = undefined

getGesture :: IO Gesture
getGesture = undefined

updateDoc :: Edits Document editop => editop -> Document -> Document
updateDoc editop doc = update editop doc

-- Proxima layers

data EditLayer sheet state editState intermedHigh intermedLow editHigh editLow high low mapping =
       EditLayer { present :: sheet -> state -> high -> (low, mapping)
                  , translate :: sheet -> state -> mapping -> low -> high ->
                    Either Gesture editLow -> (Either Gesture editHigh, editState)
                  , h :: editState -> intermedHigh -> (intermedLow, editState)
                  , updateState :: editState -> state -> state }


data EditLayer' sheet state editState intermedHigh intermedLow editHigh editLow high low mapping =
       EditLayer' { present' :: LayerFunction (sheet, state) high 
                                            (mapping, low, high, sheet, state) low
                 , translate' :: LayerFunction (mapping, low, high, sheet, state)
                                              (Either Gesture editLow)
                                              (editState, sheet, state)
                                              (Either Gesture editHigh)
                 , h' :: LayerFunction (editState, sheet, state) intermedHigh 
                                       (sheet, state) intermedLow }



wrap :: Edits st es => EditLayer sh st es ih il eh el h l m -> EditLayer' sh st es ih il eh el h l m
wrap (EditLayer {present = p, translate = t, h = h}) = 
  EditLayer' { present' = \(sh, st) h -> 
                           let (l,m) = p sh st h 
                           in  (l,(m,l, h, sh, st))
            , translate' = \(m, l, h, sh, st) gl ->
                           let (gh, es) = t sh st m l h gl
                           in  (gh, (es, sh, st))
            , h' = \(es, sh, st) ih ->
                           let (il, es') = h es ih
                           in  (il, (sh, update es' st)) }


evaluationLayer = EditLayer present0 translate0 h0 undefined
presentationLayer = EditLayer present1 translate1 h1 undefined
arrangementLayer = EditLayer present2 translate2 h2 undefined
renderingLayer = EditLayer present3 translate3 h3 undefined

