module Proxima.Architecture where

import Proxima.ArchitectureLibM

import qualified Evaluation.EvalPresent     as EvalPresent
import qualified Evaluation.EvalInterpret   as EvalInterpret
import qualified Presentation.PresPresent   as PresPresent
import qualified Presentation.PresInterpret as PresInterpret
import qualified Layout.LayPresent          as LayPresent
import qualified Layout.LayInterpret        as LayInterpret
import qualified Arrangement.ArrPresent     as ArrPresent
import qualified Arrangement.ArrInterpret   as ArrInterpret
import qualified Rendering.RenPresent       as RenPresent
import qualified Rendering.RenInterpret     as RenInterpret

type Step m nextstep a b c d = (a -> m (b, nextstep m c d a b))

newtype IntrpStep m a b c d = 
            IntrpStep {intrpStep :: Step m PresStep a b c d}

newtype PresStep m a b c d = 
            PresStep {presStep :: Step m IntrpStep a b c d}


instance Pack m (IntrpStep m a b c d) a b (PresStep m c d a b) where
  pack = IntrpStep
  unpack = intrpStep

instance Pack m (PresStep m a b c d) a b (IntrpStep m c d a b) where
  pack = PresStep
  unpack = presStep




type LayerFunction m horArgs vertArg horRess vertRes = 
       (horArgs -> vertArg -> m (vertRes, horRess))

-- The functions as they are defined in the layer modules

data ProximaLayer m state doc pres editsDoc editsPres editsDoc' editsPres' =
       ProximaLayer { interpret' :: state -> pres -> doc -> editsPres -> m (editsDoc, state, pres)
                    , present' ::   state -> doc -> pres -> editsDoc' -> m (editsPres', state, doc)
                    }


-- The data type used by the combinators

data Layer m state high low editsH editsL editsH' editsL' =
       Layer { interpret :: LayerFunction m (state, high) (low, editsL) (state, low) (high, editsH)
             , present ::   LayerFunction m (state, low) (high, editsH') (state, high) (low, editsL')
             }


lift :: Monad m => Layer m state doc pres editsDoc editsPres editsDoc' editsPres' -> (state,doc) -> IntrpStep m (pres,editsPres) (doc,editsDoc) (doc, editsDoc') (pres, editsPres')
lift layer =
  fix $ liftStep (interpret layer) 
      . liftStep (present layer) 

combine :: Monad m => IntrpStep m b c d e -> IntrpStep m a b e f -> IntrpStep m a c d f
combine upr lwr =  
  fix (combineStepUp . combineStepDown) upr lwr


 
wrap :: Monad m => ProximaLayer m state doc pres editsDoc editsPres editsDoc' editsPres'
                -> Layer m state doc pres editsDoc editsPres editsDoc' editsPres'
wrap (ProximaLayer interpret' present') = 
  Layer { interpret = \(state, doc) (pres, editsPres) ->
                         do { (editsDoc, state, pres) <- interpret' state pres doc editsPres
                            ; return ((doc, editsDoc), (state, pres))
                            }
        , present   = \(state, pres) (doc, editsDoc') ->
                         do { (editsPres', state, doc) <- present' state doc pres editsDoc'
                            ; return ((pres, editsPres'), (state, doc))
                            }
        }


evaluationLayer   = ProximaLayer EvalInterpret.interpretIO EvalPresent.presentIO
presentationLayer presentationSheet parseSheet = ProximaLayer 
                                                   (PresInterpret.interpretIO parseSheet)
                                                   (PresPresent.presentIO presentationSheet)
layoutLayer       scannerSheet = ProximaLayer 
                                   (LayInterpret.interpretIO scannerSheet) LayPresent.presentIO
arrangementLayer  settings     = ProximaLayer 
                                   ArrInterpret.interpretIO (ArrPresent.presentIO settings)
renderingLayer    settings     = ProximaLayer
                                   RenInterpret.interpretIO (RenPresent.presentIO settings)


proximaLayers settings presentationSheet parseSheet scannerSheet
              evaluationLS presentationLS layoutLS arrangementLS rendererLS =
            lift (wrap evaluationLayer)   evaluationLS
  `combine` lift (wrap (presentationLayer presentationSheet parseSheet)) presentationLS
  `combine` lift (wrap (layoutLayer scannerSheet))       layoutLS
  `combine` lift (wrap (arrangementLayer settings))  arrangementLS
  `combine` lift (wrap (renderingLayer settings))    rendererLS

