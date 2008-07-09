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

import Evaluation.DocTypes (DocumentLevel)
import Proxima.Wrap (EditDocument, EditDocument', RenderingLevel, EditRendering, EditRendering')

-- all for magic :-(
import Rendering.RenTypes
import Arrangement.ArrTypes 
import Arrangement.ArrLayerTypes 
import Layout.LayTypes 
import Layout.LayLayerTypes 
import Presentation.PresTypes 
import Presentation.PresLayerTypes 
import Common.CommonTypes
import Evaluation.DocumentEdit
import Evaluation.EnrTypes
import Evaluation.DocTypes
import Evaluation.EvalLayerTypes
{-
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
-}



type LayerFunction m horArgs vertArg horRess vertRes = 
       (horArgs -> vertArg -> m (vertRes, horRess))

-- The functions as they are defined in the layer modules

data ProximaLayer m state doc pres editsDoc editsPres editsDoc' editsPres' =
       ProximaLayer { translate' :: state -> pres -> doc -> editsPres -> m (editsDoc, state, pres)
                    , present' ::   state -> doc -> pres -> editsDoc' -> m (editsPres', state, doc)
                    }


-- The data type used by the combinators

data Layer m state high low editsH editsL editsH' editsL' =
       Layer { translate :: LayerFunction m (state, high) (low, editsL) (state, low) (high, editsH)
             , present ::   LayerFunction m (state, low) (high, editsH') (state, high) (low, editsL')
             }

{-
lift :: Monad m => Layer m state doc pres editsDoc editsPres editsDoc' editsPres' -> (state,doc) -> TransStep m (pres,editsPres) (doc,editsDoc) (doc, editsDoc') (pres, editsPres')
lift layer =
  fix $ liftStep (translate layer) 
      . liftStep (present layer) 

combine :: Monad m => TransStep m b c d e -> TransStep m a b e f -> TransStep m a c d f
combine upr lwr =  
  fix (combineStepUp . combineStepDown) upr lwr

-}
 
wrap :: Monad m => ProximaLayer m state doc pres editsDoc editsPres editsDoc' editsPres'
                -> Layer m state doc pres editsDoc editsPres editsDoc' editsPres'
wrap (ProximaLayer translate' present') = 
  Layer { translate = \(state, doc) (pres, editsPres) ->
                         do { (editsDoc, state, pres) <- translate' state pres doc editsPres
                            ; return ((doc, editsDoc), (state, pres))
                            }
        , present   = \(state, pres) (doc, editsDoc') ->
                         do { (editsPres', state, doc) <- present' state doc pres editsDoc'
                            ; return ((pres, editsPres'), (state, doc))
                            }
        }


evaluationLayer   = ProximaLayer EvalTranslate.translateIO EvalPresent.presentIO
presentationLayer presentationSheet parseSheet = ProximaLayer 
                                                   (PresTranslate.translateIO parseSheet)
                                                   (PresPresent.presentIO presentationSheet)
layoutLayer       scannerSheet = ProximaLayer 
                                   (LayTranslate.translateIO scannerSheet) LayPresent.presentIO
arrangementLayer  = ProximaLayer ArrTranslate.translateIO ArrPresent.presentIO
renderingLayer    = ProximaLayer RenTranslate.translateIO RenPresent.presentIO


 
lift :: Monad m => Layer m state doc pres editsDoc editsPres editsDoc' editsPres' -> (state,doc) -> 
        TLayer m (pres,editsPres) (doc,editsDoc) (doc, editsDoc') (pres, editsPres')
lift layer = genericLift (translate layer) (present layer)
--lift layer = lfix $ liftStep (translate layer) . liftStep (present layer)
--genericCombine = cfix $ combineStepUp . combineStepDown


proximaLayers :: forall doc t6 node clip token.
 (Show t6,
 Evaluation.DocTypes.Doc doc,
 Show (Arrangement.ArrTypes.Arrangement node),
 Show token,
 Common.CommonTypes.DocNode node,
 Ord token,
 Evaluation.EvalLayerTypes.ReductionSheet doc t6 clip,
 Evaluation.EvalLayerTypes.EvaluationSheet doc t6 clip,
 Evaluation.DocumentEdit.Editable doc doc node clip token,
 Evaluation.DocumentEdit.Clip clip) =>
 Presentation.PresLayerTypes.PresentationSheet doc
                                              t6
                                              node
                                              clip
                                              token
 -> Presentation.PresLayerTypes.ParseSheet doc t6 node clip token
 -> Layout.LayLayerTypes.ScannerSheet doc node clip token
 -> (Evaluation.EvalLayerTypes.LayerStateEval doc clip,
    Evaluation.DocTypes.DocumentLevel doc clip)
 -> (Presentation.PresLayerTypes.LayerStatePres,
    Evaluation.EnrTypes.EnrichedDocLevel t6 doc)
 -> (Layout.LayLayerTypes.LayerStateLay doc node clip token,
    Presentation.PresTypes.PresentationLevel doc node clip token)
 -> (Arrangement.ArrLayerTypes.LocalStateArr,
    Layout.LayTypes.LayoutLevel doc node clip token)
 -> (Rendering.RenTypes.LocalStateRen,
    Arrangement.ArrTypes.ArrangementLevel doc node clip token)
 -> TLayer IO (RenderingLevel doc t6 node clip token, [EditRendering doc t6 node clip token])
               (DocumentLevel doc clip, [EditDocument doc t6 node clip token])
               (DocumentLevel doc clip, [EditDocument' doc t6 node clip token])
               (RenderingLevel doc t6 node clip token, [EditRendering' doc t6 node clip token])
     

proximaLayers presentationSheet parseSheet scannerSheet
              evaluationLS presentationLS layoutLS arrangementLS rendererLS =
                   lift (wrap evaluationLayer)   evaluationLS
  `genericCombine` lift (wrap (presentationLayer presentationSheet parseSheet)) presentationLS
  `genericCombine` lift (wrap (layoutLayer scannerSheet))       layoutLS
  `genericCombine` lift (wrap arrangementLayer)  arrangementLS
  `genericCombine` lift (wrap renderingLayer)    rendererLS
 {- :: --(Show doc, Show enr, Show token, Show node) =>
     TLayer IO (RenderingLevel doc enr node clip token, [EditRendering doc enr node clip token])
               (RenderingLevel doc enr node clip token, [EditRendering' doc enr node clip token])
               (DocumentLevel doc clip, [EditDocument doc enr node clip token])
               (DocumentLevel doc clip, [EditDocument' doc enr node clip token])
               
               -}
               
               
{-
Proxima.ArchitectureLibM.Fix IO
                                ((Proxima.ArchitectureLibM.:.:) (Proxima.Archite
ctureLibM.Step (Proxima.Wrap.RenderingLevel doc

                                            t6

                                            node

                                            clip

                                            token,

                [Proxima.Wrap.EditRendering doc

                                            t6

                                            node

                                            clip

                                            token])

               (Evaluation.DocTypes.DocumentLevel doc

                                                  clip,

                [Proxima.Wrap.EditDocument doc

                                           t6

                                           node

                                           clip

                                           token]))
                                                                ((Proxima.Archit
ectureLibM.:.:) (Proxima.ArchitectureLibM.Step (Evaluation.DocTypes.DocumentLeve
l doc


  clip,

                                                [Proxima.Wrap.EditDocument' doc

                                                                            t6

                                                                            node


                                                                            clip


                                                                            toke
n])

                                               (Proxima.Wrap.RenderingLevel doc

                                                                            t6

                                                                            node


                                                                            clip


                                                                            toke
n,

                                                [Rendering.RenTypes.EditRenderin
g'_ (Proxima.Wrap.Wrapped doc


                          t6


                          node


                          clip


                          token)


    doc


    t6


    node


    clip


    token]))

                Proxima.ArchitectureLibM.NilStep))
-}               