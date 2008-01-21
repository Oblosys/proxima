module EvalLayerTypes ( module EnrTypes
                      , module EvalLayerTypes    ) where


import CommonTypes
import EnrTypes 
import DocTypes

data LayerStateEval = LayerStateEval

type EvaluationSheet doc enr clip = LayerStateEval -> DocumentLevel doc clip -> EnrichedDocLevel enr ->
             EditDocument' (DocumentLevel doc clip) doc ->
             IO (EditEnrichedDoc' enr, LayerStateEval, DocumentLevel doc clip)
             
       
-- instantiating only one of these is enough
class ReductionSheet doc enr clip | doc -> clip where
  reductionSheet ::
               LayerStateEval -> DocumentLevel doc clip ->
               EnrichedDocLevel enr ->
               IO (EditDocument (DocumentLevel doc clip) doc, LayerStateEval, EnrichedDocLevel enr)


-- The parameters to the sheet are the old enriched doc and doc, and the new enriched doc.
-- the result is an edit operation on the document, a new state, and a possibly updated enriched doc
-- If necessary, the old enriched document could also be provided in an interface
  reductionSheet state (DocumentLevel oldDoc oldDocFocus oldClip) (EnrichedDocLevel enr enrFocus) = 
   do { (newDoc, newState, newEnr) <- reductionSheetSimpleIO state oldDoc enr
      ; return (SetDoc newDoc, newState, EnrichedDocLevel newEnr enrFocus)
      }
      
  reductionSheetSimpleIO :: LayerStateEval -> doc -> enr -> IO (doc, LayerStateEval, enr)
  reductionSheetSimpleIO state doc enr = return $ reductionSheetSimple state doc enr
  
  reductionSheetSimple :: LayerStateEval -> doc -> enr -> (doc, LayerStateEval, enr)
  reductionSheetSimple state doc enr =  (reductionSheetSimplest enr, state, enr)
  
  reductionSheetSimplest :: enr -> doc
         