module Main where

import Layers


type LayerFunction horArg vertArg horRes vertRes = 
       horArg -> vertArg -> (horRes, vertRes)  --

-- code for haskell.xml
data LayerC h0 h1 doc pres gest upd =
       LayerC { presentC ::   LayerFunction h0 doc h1 pres
              , interpretC :: LayerFunction h1 gest h0 upd
              }

composeDown :: LayerFunction horArgH arg horResH interm ->
               LayerFunction horArgL interm horResL res ->
               LayerFunction (horArgH, horArgL) arg (horResH,horResL) res
composeDown higher lower = 
  \(horArgH, horArgL) arg ->                                           
    let (horResH, interm) = higher horArgH arg
        (horResL, res)    = lower horArgL interm
    in  ((horResH,horResL), res)


composeUp :: LayerFunction horArgH interm horResH res ->
             LayerFunction horArgL arg horResL interm ->
             LayerFunction (horArgH, horArgL) arg (horResH,horResL) res
composeUp higher lower = 
  \(horArgH, horArgL) arg ->
    let (horResL, interm) = lower horArgL arg
        (horResH, res)    = higher horArgH interm            
    in  ((horResH,horResL), res)

-- problem, when we combine, the thing is no longer a Simple'.

lift :: Simple a b c d e f -> LayerC a (b,a) c d e f
lift simple = 
  LayerC { presentC = present simple
         , interpretC = interpret simple
         }

combine :: LayerC a b c d e f -> LayerC g h d i j e -> 
           LayerC (a,g) (b,h) c i j f
combine higher lower =
  LayerC { presentC = composeDown (presentC higher) (presentC lower)
         , interpretC = composeUp (interpretC higher) (interpretC lower)
         }

combine' :: Simple' a b c d e f -> Simple' g a d h i e -> 
           Simple' (a,g) (b,a) c h i f
combine' higher lower =
  Simple' { present' = composeDown (present' higher) (present' lower)
          , interpret' = composeUp (interpret' higher) (interpret' lower)
          }
{-data Simple' state mapping doc pres gest upd =
       Simple' { present' ::   LayerFunction state doc (mapping, state) pres
               , interpret' :: LayerFunction (mapping, state) gest state upd
               }
-}

{-

result is (mapping, (mapping,...)), but arg is ((mapping, state), ((mapping,state), ...))))  

handling the h params at loop level is only possible when they are not connected like this.

Solution: special combinator combUp and combDown, specific to h params and h args
Or: wrap simples so htypes match. 
-}


main' layer0 layer1 layer2 = 
 do { (state0, state1, state2) <- initStates
    ; doc <- initDoc 
    ; let layers = lift layer0 `combine` lift layer1 `combine` lift  layer2
    ; editLoop layers ((state0, state1), state2) doc
    }

editLoop layers states doc = loop states doc
 where loop states doc = 
        do { -- Presentation process:
             let (mappingsStates, pres) = (presentC layers) states doc
           
           ; showRendering pres
           ; gest <- getGesture
 
             -- Interpretation process:
           ; let (states', update) = (interpretC layers) mappingsStates gest
       
           ; let doc' = updateDocument update doc
           ; loop states' doc'
           }

