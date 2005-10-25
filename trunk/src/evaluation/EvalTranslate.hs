module EvalTranslate where

import CommonTypes
import EvalLayerTypes

translateIO :: ReductionSheet doc enr clip ->
               LayerStateEval -> EnrichedDocLevel enr -> DocumentLevel doc clip ->
               EditEnrichedDoc (DocumentLevel doc clip) enr -> 
               IO (EditDocument (DocumentLevel doc clip) doc, LayerStateEval, EnrichedDocLevel enr)
translateIO reductionSheet = reductionSheet
