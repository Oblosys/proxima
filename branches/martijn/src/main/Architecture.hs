module Architecture where

import ArchitectureLibM

import qualified EvalPresent
import qualified EvalTranslate
import qualified PresPresent
import qualified PresTranslate
import qualified LayPresent
import qualified LayTranslate
import qualified ArrPresent
import qualified ArrTranslate
import qualified RenPresent
import qualified RenTranslate

type Step m nextstep a b c d = (a -> m (b, nextstep m c d a b))

newtype TransStep m a b c d = 
            TransStep {transStep :: Step m PresStep a b c d}

newtype PresStep m a b c d = 
            PresStep {presStep :: Step m TransStep a b c d}


instance Pack m (TransStep m a b c d) a b (PresStep m c d a b) where
  pack = TransStep
  unpack = transStep

instance Pack m (PresStep m a b c d) a b (TransStep m c d a b) where
  pack = PresStep
  unpack = presStep




type LayerFunction m horArgs vertArg horRess vertRes = 
       (horArgs -> vertArg -> m (vertRes, horRess))

-- The functions as they are defined in the layer modules

data ProximaLayer m state doc pres editDoc editPres editDoc' editPres' =
       ProximaLayer { translate' :: state -> pres -> doc -> editPres -> m (editDoc, state, pres)
                    , present' ::   state -> doc -> pres -> editDoc' -> m (editPres', state, doc)
                    }


-- The data type used by the combinators

data Layer m state doc pres editDoc editPres editDoc' editPres' =
       Layer { translate :: LayerFunction m (state, doc) (pres, editPres) (state, pres) (doc, editDoc)
              , present ::   LayerFunction m (state, pres) (doc, editDoc') (state, doc) (pres, editPres')
              }


{-
(Monad m,
 ArchitectureLibM.Pack m step (pres, editPres) (doc, editDoc) step1,
 ArchitectureLibM.Pack m
                       step1
                       (doc, editDoc')
                       (pres, editPres')
                       step) =>
Layer m state doc pres editDoc editPres editDoc' editPres'
-> (state, doc) -> step
-}


-- the edit ops may be of equal type, but for now we use two different types
lift :: Monad m => Layer m state doc pres editDoc editPres editDoc' editPres' -> (state,doc) -> TransStep m (pres,editPres) (doc,editDoc) (doc, editDoc') (pres, editPres')
lift layer =
  fix $ liftStep (translate layer) 
      . liftStep (present layer) 

combine :: Monad m => TransStep m b c d e -> TransStep m a b e f -> TransStep m a c d f
combine upr lwr =  
  fix (combineStepUp . combineStepDown) upr lwr


-- 
wrap :: Monad m => ProximaLayer m state doc pres editDoc editPres editDoc' editPres'
                -> Layer m state doc pres editDoc editPres editDoc' editPres'
wrap (ProximaLayer translate' present') = 
  Layer { translate = \(state, doc) (pres, editPres) ->
                         do { (editDoc, state, pres) <- translate' state pres doc editPres
                            ; return ((doc, editDoc), (state, pres))
                            }
         , present   = \(state, pres) (doc, editDoc') ->
                         do { (editPres', state, doc) <- present' state doc pres editDoc'
                            ; return ((pres, editPres'), (state, doc))
                            }
         }


{-
-- The data type used by the combinators
data Layer m state doc pres editDoc editPres editDoc' editPres' =
       Layer { translate :: LayerFunction m (state, doc) (pres, editPres) (state, pres) (doc, editDoc)
              , present ::   LayerFunction m (state, pres) (doc, editDoc') (state, doc) (pres, editPres')
              }


-- 
wrap :: Monad m => ProximaLayer m state doc pres editDoc editPres  editDoc' editPres'
                -> Layer m state doc pres editDoc editPres editDoc' editPres'
wrap (ProximaLayer translate' present') = 
  Layer { translate = \(state, doc) (pres, editPres) ->
                         do { (editDoc, state, pres) <- translate' state pres doc editPres
                            ; return ((doc, editDoc), (state, pres))
                            }
         , present   = \(state, pres) (doc, editDoc') ->
                         do { (editPres', state, doc) <- present' state doc pres editDoc'
                            ; return ((pres, editPres'), (state, doc))
                            }
         }


data ProximaLayer m state doc pres editDoc editPres editDoc' editPres' =
       ProximaLayer { translate' :: state -> pres -> doc -> editPres -> m (editDoc, state, pres)
                    , present' ::   state -> doc -> pres -> editDoc' -> m (editPres', state, doc)
                    }
-}



evaluationLayer   = ProximaLayer      EvalTranslate.translateIO EvalPresent.presentIO
presentationLayer = ProximaLayer      PresTranslate.translateIO PresPresent.presentIO
layoutLayer       = ProximaLayer      LayTranslate.translateIO  LayPresent.presentIO
arrangementLayer  = ProximaLayer      ArrTranslate.translateIO  ArrPresent.presentIO
renderingLayer    = ProximaLayer      RenTranslate.translateIO  RenPresent.presentIO


proximaLayers evaluationLS presentationLS layoutLS arrangementLS rendererLS =
            lift (wrap evaluationLayer)   evaluationLS
  `combine` lift (wrap presentationLayer) presentationLS
  `combine` lift (wrap layoutLayer)       layoutLS
  `combine` lift (wrap arrangementLayer)  arrangementLS
  `combine` lift (wrap renderingLayer)    rendererLS

