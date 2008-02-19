module Proxima.Architecture where

import Proxima.ArchitectureLibM

import qualified Evaluation.EvalPresent     as EvalPresent
import qualified Evaluation.EvalTranslate   as EvalTranslate
import qualified Presentation.PresPresent   as PresPresent
import qualified Presentation.PresTranslate as PresTranslate
import qualified Layout.LayPresent          as LayPresent
import qualified Layout.LayTranslate        as LayTranslate
import qualified Arrangement.ArrPresent     as ArrPresent
import qualified Arrangement.ArrTranslate   as ArrTranslate
import qualified Rendering.RenPresent       as RenPresent
import qualified Rendering.RenTranslate     as RenTranslate

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

{-
data Layer m state doc pres editDoc editPres editDoc' editPres' =
       Layer { translate :: LayerFunction m (state, doc) (pres, editPres) (state, pres) (doc, editDoc)
              , present ::   LayerFunction m (state, pres) (doc, editDoc') (state, doc) (pres, editPres')
              }
-}
-- thesis version with better names
data Layer m state high low editH editL editH' editL' =
       Layer { translate :: LayerFunction m (state, high) (low, editL) (state, low) (high, editH)
              , present ::   LayerFunction m (state, low) (high, editH') (state, high) (low, editL')
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



evaluationLayer   = ProximaLayer EvalTranslate.translateIO EvalPresent.presentIO
presentationLayer presentationSheet parseSheet = ProximaLayer 
                                                   (PresTranslate.translateIO parseSheet)
                                                   (PresPresent.presentIO presentationSheet)
layoutLayer       scannerSheet = ProximaLayer 
                                   (LayTranslate.translateIO scannerSheet) LayPresent.presentIO
arrangementLayer  = ProximaLayer ArrTranslate.translateIO ArrPresent.presentIO
renderingLayer    = ProximaLayer RenTranslate.translateIO RenPresent.presentIO


proximaLayers presentationSheet parseSheet scannerSheet
              evaluationLS presentationLS layoutLS arrangementLS rendererLS =
            lift (wrap evaluationLayer)   evaluationLS
  `combine` lift (wrap (presentationLayer presentationSheet parseSheet)) presentationLS
  `combine` lift (wrap (layoutLayer scannerSheet))       layoutLS
  `combine` lift (wrap arrangementLayer)  arrangementLS
  `combine` lift (wrap renderingLayer)    rendererLS

