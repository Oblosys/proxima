module Main where
import Layers

main' layer0 layer1 layer2 = 
 do { states <- initStates
    ; doc <- initDoc 
    ; editLoop (layer0, layer1, layer2) states doc
    }

editLoop (layer0, layer1, layer2) states doc = loop states doc
 where loop (state0, state1, state2) doc = 
        do { -- Presentation process:
             let ((mapping0,state0'), pres1) = (present layer0) state0 doc
           ; let ((mapping1,state1'), pres2) = (present layer1) state1 pres1
           ; let ((mapping2,state2'), pres3) = (present layer2) state2 pres2

           ; showRendering pres3
           ; gest3 <- getGesture
 
             -- Interpretation process:
           ; let (state2'', gest2)  = (interpret layer2) (mapping2, state2') gest3
           ; let (state1'', gest1)  = (interpret layer1) (mapping1, state1') gest2
           ; let (state0'', update) = (interpret layer0) (mapping0, state0') gest1
       
           ; let doc' = updateDocument update doc
           ; loop (state0'', state1'', state2'') doc'
           }