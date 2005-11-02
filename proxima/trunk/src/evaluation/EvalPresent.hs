module EvalPresent where

import CommonTypes
import EvalLayerTypes
import EvalLayerUtils

-- For now, the entire functionality of this layer is in the evaluationSheet parameter
-- TODO: factorize out common functionality

presentIO :: EvaluationSheet doc enr clip -> 
             LayerStateEval -> DocumentLevel doc clip -> EnrichedDocLevel enr ->
             EditDocument' (DocumentLevel doc clip) doc ->
             IO (EditEnrichedDoc' enr, LayerStateEval, DocumentLevel doc clip)
presentIO  evaluationSheet = evaluationSheet
