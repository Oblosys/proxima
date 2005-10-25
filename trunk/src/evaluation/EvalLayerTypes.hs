module EvalLayerTypes ( module EnrTypes
                      , module EvalLayerTypes    ) where


import CommonTypes
import EnrTypes 
import DocTypes

data LayerStateEval = LayerStateEval

type EvaluationSheet doc enr clip = LayerStateEval -> DocumentLevel doc clip -> EnrichedDocLevel enr ->
             EditDocument' (DocumentLevel doc clip) doc ->
             IO (EditEnrichedDoc' enr, LayerStateEval, DocumentLevel doc clip)
             
type ReductionSheet doc enr clip = LayerStateEval -> EnrichedDocLevel enr -> DocumentLevel doc clip ->
               EditEnrichedDoc (DocumentLevel doc clip) enr -> 
               IO (EditDocument (DocumentLevel doc clip) doc, LayerStateEval, EnrichedDocLevel enr)