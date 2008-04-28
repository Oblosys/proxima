module Evaluation.EvalLayerTypes ( module Evaluation.EnrTypes
                      , module Evaluation.EvalLayerTypes    ) where


import Common.CommonTypes
import Evaluation.EnrTypes 
import Evaluation.DocTypes

data LayerStateEval doc clip = LayerStateEval [DocumentLevel doc clip] [DocumentLevel doc clip]


-- instantiating only one of these is enough
class EvaluationSheet doc enr clip | doc -> clip where
  evaluationSheet ::
                LayerStateEval doc clip -> DocumentLevel doc clip -> EnrichedDocLevel enr doc -> EditDocument' doc clip -> DocumentLevel doc clip -> 
                IO (EditEnrichedDoc' enr doc, LayerStateEval doc clip, DocumentLevel doc clip)
-- The parameters to the sheet are the old document, the old enriched doc, and the new enriched doc.
-- also the edit operation on the document is passed, but this has already been applied to old
-- doc, yielding the new document. It is only present to implement special behavior for certain
-- edit operations. (such doing an expensive type computation only for an EvaluateDoc edit)
-- the result is an edit operation on the document, a new state, and a possibly updated enriched doc
-- If necessary, the old enriched document could also be provided in an interface
     
  evaluationSheetSimpleIO :: LayerStateEval doc clip -> doc -> enr -> doc -> IO (enr, LayerStateEval doc clip, doc)
  evaluationSheetSimple   :: LayerStateEval doc clip -> doc -> enr -> doc -> (enr, LayerStateEval doc clip, doc)
  evaluationSheetSimplest :: doc -> enr

  evaluationSheet state (DocumentLevel oldDoc oldDocFocus oldClip) (EnrichedDocLevel oldEnr oldEnrFocus _)
                        docEdit (DocumentLevel doc docFocus clip) = 
     do { (newEnr, newState, newDoc) <- evaluationSheetSimpleIO state oldDoc oldEnr doc
        ; return (SetEnr' (EnrichedDocLevel newEnr docFocus newDoc), newState, DocumentLevel newDoc docFocus clip)
        } -- copy the doc focus to the enriched focus
        
  evaluationSheetSimpleIO state oldDoc enr doc = return $ evaluationSheetSimple state oldDoc enr doc
  
  evaluationSheetSimple state oldDoc enr doc   =  (evaluationSheetSimplest doc, state, doc)


       
-- instantiating only one of these is enough
class ReductionSheet doc enr clip | doc -> clip where
  reductionSheet :: LayerStateEval doc clip -> EnrichedDocLevel enr doc -> DocumentLevel doc clip ->
               EnrichedDocLevel enr doc ->
               IO (EditDocument doc clip, LayerStateEval doc clip, EnrichedDocLevel enr doc)
-- The parameters to the sheet are the old enriched doc, the old document, and the new enriched doc.
-- the result is an edit operation on the document, a new state, and a possibly updated enriched doc
-- If necessary, the old enriched document could also be provided in an interface

  reductionSheetSimpleIO :: LayerStateEval doc clip -> enr -> doc -> enr -> IO (doc, LayerStateEval doc clip, enr)
  reductionSheetSimple   :: LayerStateEval doc clip -> enr -> doc -> enr -> (doc, LayerStateEval doc clip, enr)
  reductionSheetSimplest :: enr -> doc

  reductionSheet state (EnrichedDocLevel oldEnr oldEnrFocus _) (DocumentLevel oldDoc oldDocFocus oldClip)
                       (EnrichedDocLevel enr enrFocus doc) = 
   do { (newDoc, newState, newEnr) <- reductionSheetSimpleIO state oldEnr oldDoc enr
      ; return (SetDoc (DocumentLevel newDoc enrFocus oldClip), newState, EnrichedDocLevel newEnr enrFocus newDoc) -- popupHack arg, but it is only used on presentation
      } -- copy the enriched focus to the doc focus         
           
  reductionSheetSimpleIO state oldEnr oldDoc enr = return $ reductionSheetSimple state oldEnr oldDoc enr
  
  reductionSheetSimple state oldEnr oldDoc enr   = (reductionSheetSimplest enr, state, enr)
  
-- Maybe we also want the focus in the simpler interfaces in a future version.
