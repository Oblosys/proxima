module Main where

import DPP_Lib
import ProximaLayers

type Step nextstep a b c d e f = (a ->(b, nextstep c d e f a b))

newtype PresStep a b c d e f = PresStep (Step TransStep a b c d e f)
newtype TransStep a b c d e f = TransStep(Step HStep a b c d e f)
newtype HStep a b c d e f = HStep (Step PresStep a b c d e f)

dictPresStep =  (PresStep,  \(PresStep p)  -> p, \(PresStep p) -> p)
dictTransStep = (TransStep, \(TransStep p) -> p, \(TransStep p) -> p)
dictHStep =     (HStep,     \(HStep p)     -> p, \(HStep p) -> p)

lift layer =
  fix $ f dictPresStep  (present' layer) 
      . f dictTransStep (translate' layer) 
      . f dictHStep     (h' layer)

combine = 
  fix $ g dictPresStep  combineDown 
      . g dictTransStep combineUp
      . g dictHStep     combineDown 


-- type not right, first PresStep already 'unpacked'
--editLoop :: PresStep Document Rendering (Either Gesture EditRendering) (Either Gesture EditDocument)
--            Intermediate0 Intermediate4 -> Document -> IO ()
editLoop present doc =  
 do { let (pres, TransStep translate) = present doc

    ; showRendering pres
    ; gesture <- getGesture
    
    ; let ((Right upd), HStep h) = translate (Left gesture)
    
    ; let (_ , PresStep present') = h upd

    ; let doc' = update upd doc
    
    ; editLoop present' doc'
    }

main :: IO ()
main =
 do { (sheet0, sheet1, sheet2, sheet3) <- initSheets
    ; (state0, state1, state2, state3) <- initStates
    ; doc <- initDoc 
    ; let PresStep present =           lift (wrap evaluationLayer)   (sheet0, state0) 
                             `combine` lift (wrap presentationLayer) (sheet1, state1)
                             `combine` lift (wrap arrangementLayer)  (sheet2, state2)
                             `combine` lift (wrap renderingLayer)    (sheet3, state3)
    ; doc <- initDoc
    ; editLoop present doc
    }
