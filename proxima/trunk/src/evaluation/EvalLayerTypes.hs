module EvalLayerTypes ( module EnrTypes
                      , module EvalLayerTypes    ) where


import CommonTypes
import EnrTypes 
import DocTypes

data LayerStateEval = LayerStateEval


-- instantiating only one of these is enough
class EvaluationSheet doc enr clip | doc -> clip where
  evaluationSheet ::
                LayerStateEval -> DocumentLevel doc clip -> EnrichedDocLevel enr -> EditDocument' docLevel doc -> DocumentLevel doc clip -> 
                IO (EditEnrichedDoc' enr, LayerStateEval, DocumentLevel doc clip)
-- The parameters to the sheet are the old document, the old enriched doc, and the new enriched doc.
-- also the edit operation on the document is passed, but this has already been applied to old
-- doc, yielding the new document. It is only present to implement special behavior for certain
-- edit operations. (such doing an expensive type computation only for an EvaluateDoc edit)
-- the result is an edit operation on the document, a new state, and a possibly updated enriched doc
-- If necessary, the old enriched document could also be provided in an interface
     
  evaluationSheetSimpleIO :: LayerStateEval -> doc -> enr -> doc -> IO (enr, LayerStateEval, doc)
  evaluationSheetSimple   :: LayerStateEval -> doc -> enr -> doc -> (enr, LayerStateEval, doc)
  evaluationSheetSimplest :: doc -> enr

  evaluationSheet state (DocumentLevel oldDoc oldDocFocus oldClip) (EnrichedDocLevel oldEnr oldEnrFocus)
                        docEdit (DocumentLevel doc docFocus clip) = 
     do { (newEnr, newState, newDoc) <- evaluationSheetSimpleIO state oldDoc oldEnr doc
        ; return (SetEnr' (EnrichedDocLevel newEnr docFocus), newState, DocumentLevel newDoc docFocus clip)
        } -- copy the doc focus to the enriched focus
        
  evaluationSheetSimpleIO state oldDoc enr doc = return $ evaluationSheetSimple state oldDoc enr doc
  
  evaluationSheetSimple state oldDoc enr doc   =  (evaluationSheetSimplest doc, state, doc)


       
-- instantiating only one of these is enough
class ReductionSheet doc enr clip | doc -> clip where
  reductionSheet :: LayerStateEval -> EnrichedDocLevel enr -> DocumentLevel doc clip ->
               EnrichedDocLevel enr ->
               IO (EditDocument (DocumentLevel doc clip) doc, LayerStateEval, EnrichedDocLevel enr)
-- The parameters to the sheet are the old enriched doc, the old document, and the new enriched doc.
-- the result is an edit operation on the document, a new state, and a possibly updated enriched doc
-- If necessary, the old enriched document could also be provided in an interface

  reductionSheetSimpleIO :: LayerStateEval -> enr -> doc -> enr -> IO (doc, LayerStateEval, enr)
  reductionSheetSimple   :: LayerStateEval -> enr -> doc -> enr -> (doc, LayerStateEval, enr)
  reductionSheetSimplest :: enr -> doc

  reductionSheet state (EnrichedDocLevel oldEnr oldEnrFocus) (DocumentLevel oldDoc oldDocFocus oldClip)
                       (EnrichedDocLevel enr enrFocus) = 
   do { (newDoc, newState, newEnr) <- reductionSheetSimpleIO state oldEnr oldDoc enr
      ; return (SetDoc newDoc, newState, EnrichedDocLevel newEnr enrFocus)
      } -- reduction does not copy the enriched focus to the doc focus, because enriched focus is 
        -- never edited
      
  reductionSheetSimpleIO state oldEnr oldDoc enr = return $ reductionSheetSimple state oldEnr oldDoc enr
  
  reductionSheetSimple state oldEnr oldDoc enr   = (reductionSheetSimplest enr, state, enr)
  
-- Maybe we also want the focus in the simpler interfaces in a future version.