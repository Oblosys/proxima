module EvalLayerTypes ( module EnrTypes
                      , module EvalLayerTypes    ) where


import CommonTypes
import EnrTypes 
import DocTypes

data LayerStateEval = LayerStateEval

type EvaluationSheet doc enr clip = LayerStateEval -> DocumentLevel doc clip -> EnrichedDocLevel enr ->
             EditDocument' (DocumentLevel doc clip) doc ->
             IO (EditEnrichedDoc' enr, LayerStateEval, DocumentLevel doc clip)
             
-- The parameters to the sheet are the old enriched doc and doc, and the new enriched doc.
-- the result is an edit operation on the document, a new state, and a possibly updated enriched doc
type ReductionSheet doc enr clip = 
       LayerStateEval -> EnrichedDocLevel enr -> DocumentLevel doc clip ->
       EnrichedDocLevel enr ->
       IO (EditDocument doc clip, LayerStateEval, EnrichedDocLevel enr)