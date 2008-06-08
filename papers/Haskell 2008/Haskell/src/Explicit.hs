module Explicit where
import Layers

main layer0 layer1 layer2 = 
 do { states <- initStates
    ; doc <- initDoc 
    ; editLoop (layer0, layer1, layer2) states doc
    }

editLoop (layer0, layer1, layer2) states doc = 
  loop states doc where
  loop (state0, state1, state2) doc = 
   do { let (pres1,(map0,state0')) = 
              present layer0 state0 doc
      ; let (pres2,(map1,state1')) = 
              present layer1 state1 pres1
      ; let (pres3,(map2,state2')) = 
              present layer2 state2 pres2

      ; showRendering pres3
      ; gest3 <- getGesture

      ; let (gest2,state2'')  = 
              interpret layer2 (map2, state2') gest3
      ; let (gest1,state1'')  =
              interpret layer1 (map1, state1') gest2
      ; let (update,state0'') = 
              interpret layer0 (map0, state0') gest1
      
      ; let doc' = updateDocument update doc
      ; loop (state0'', state1'', state2'') doc'
      }
