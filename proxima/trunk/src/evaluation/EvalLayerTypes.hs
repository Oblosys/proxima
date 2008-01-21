module EvalLayerTypes ( module EnrTypes
                      , module EvalLayerTypes    ) where


import CommonTypes
import EnrTypes 
import DocTypes

data LayerStateEval = LayerStateEval


-- instantiating only one of these is enough
class EvaluationSheet doc enr clip | doc -> clip where
  evaluationSheet ::
                LayerStateEval -> EnrichedDocLevel enr -> DocumentLevel doc clip -> 
                IO (EditEnrichedDoc' enr, LayerStateEval, DocumentLevel doc clip)
-- The parameters to the sheet are the old enriched doc and doc, and the new enriched doc.
-- the result is an edit operation on the document, a new state, and a possibly updated enriched doc
-- If necessary, the old enriched document could also be provided in an interface
     
  evaluationSheetSimpleIO :: LayerStateEval -> enr -> doc -> IO (enr, LayerStateEval, doc)
  evaluationSheetSimple :: LayerStateEval -> enr -> doc -> (enr, LayerStateEval, doc)
  evaluationSheetSimplest :: doc -> enr

  evaluationSheet state (EnrichedDocLevel oldEnr oldEnrFocus) (DocumentLevel doc docFocus clip) = 
     do { (newEnr, newState, newDoc) <- evaluationSheetSimpleIO state oldEnr doc
        ; return (SetEnr' (EnrichedDocLevel newEnr oldEnrFocus), newState, DocumentLevel newDoc docFocus clip)
        }
        
  evaluationSheetSimpleIO state enr doc = return $ evaluationSheetSimple state enr doc
  
  evaluationSheetSimple state enr doc   =  (evaluationSheetSimplest doc, state, doc)


       
-- instantiating only one of these is enough
class ReductionSheet doc enr clip | doc -> clip where
  reductionSheet ::
               LayerStateEval -> DocumentLevel doc clip -> EnrichedDocLevel enr ->
               IO (EditDocument (DocumentLevel doc clip) doc, LayerStateEval, EnrichedDocLevel enr)
-- The parameters to the sheet are the old enriched doc and doc, and the new enriched doc.
-- the result is an edit operation on the document, a new state, and a possibly updated enriched doc
-- If necessary, the old enriched document could also be provided in an interface

  reductionSheetSimpleIO :: LayerStateEval -> doc -> enr -> IO (doc, LayerStateEval, enr)
  reductionSheetSimple   :: LayerStateEval -> doc -> enr -> (doc, LayerStateEval, enr)
  reductionSheetSimplest :: enr -> doc


  reductionSheet state (DocumentLevel oldDoc oldDocFocus oldClip) (EnrichedDocLevel enr enrFocus) = 
   do { (newDoc, newState, newEnr) <- reductionSheetSimpleIO state oldDoc enr
      ; return (SetDoc newDoc, newState, EnrichedDocLevel newEnr enrFocus)
      }
      
  reductionSheetSimpleIO state doc enr = return $ reductionSheetSimple state doc enr
  
  reductionSheetSimple state doc enr   = (reductionSheetSimplest enr, state, enr)
  
         