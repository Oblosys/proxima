module NCP where

import Layers


data LayerC h0 h1 doc pres gest upd =
       LayerC { presentC ::   LayerFn h0 doc h1 pres
              , interpretC :: LayerFn h1 gest h0 upd
              }
--------------------------------------------------------
composeDown :: LayerFn horArgU arg horResU intr ->
               LayerFn horArgL intr horResL res ->
               LayerFn (horArgU, horArgL) arg 
                             (horResU, horResL) res
composeDown upper lower = 
  \(horArgU, horArgL) arg ->                                           
    let (interm, horResU) = upper horArgU arg
        (res, horResL)    = lower horArgL interm            
    in  (res, (horResU,horResL))


composeUp :: LayerFn horArgU intr horResU res ->
             LayerFn horArgL arg horResL intr ->
             LayerFn (horArgU, horArgL) arg
                           (horResU, horResL) res
composeUp upper lower = 
  \(horArgU, horArgL) arg ->
    let (interm, horResL) = lower horArgL arg
        (res, horResU)    = upper horArgU interm            
    in  (res, (horResU,horResL))

-- problem, when we combine, the thing is no longer a Simple'.

lift :: Simple a b c d e f -> LayerC a (b,a) c d e f
lift simple = 
  LayerC { presentC = present simple
         , interpretC = interpret simple
         }

combine :: LayerC a b c d e f -> LayerC g h d i j e -> 
           LayerC (a,g) (b,h) c i j f
combine upper lower =
  LayerC { presentC = composeDown (presentC upper) 
                                  (presentC lower)
         , interpretC = composeUp (interpretC upper) 
                                  (interpretC lower)
         }

combine' :: Simple' a b c d e f -> Simple' g a d h i e -> 
           Simple' (a,g) (b,a) c h i f
combine' upper lower =
  Simple' { present' = composeDown (present' upper) (present' lower)
          , interpret' = composeUp (interpret' upper) (interpret' lower)
          }
{-data Simple' state map doc pres gest upd =
       Simple' { present' ::   LayerFn state doc (map, state) pres
               , interpret' :: LayerFn (map, state) gest state upd
               }
-}

{-

result is (map, (map,...)), but arg is ((map, state), ((map,state), ...))))  

handling the h params at loop level is only possible when they are not connected like this.

Solution: special combinator combUp and combDown, specific to h params and h args
Or: wrap simples so htypes match. 
-}


main layer1 layer2 layer3 = 
 do { (state1, state2, state3) <- initStates
    ; doc <- initDoc 
    ; let layers = lift layer1 `combine` lift layer2 
                               `combine` lift  layer3
    ; editLoop layers ((state1, state2), state3) doc
    }


editLoop layers states doc = loop states doc
 where loop states doc = 
        do { let (pres, mapsStates) = 
                   presentC layers states doc
           
           ; showRendering pres
           ; gest <- getGesture
 
           ; let (update, states') = 
                   interpretC layers mapsStates gest
       
           ; let doc' = updateDocument update doc
           ; loop states' doc'
           }

