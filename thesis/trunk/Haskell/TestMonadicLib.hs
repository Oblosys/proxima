module TestMonadicLib where

import ArchitectureLibM


-- Simple test module for ArchitectureLibM

--------------------

type Step nextstep a b c d = (a -> IO (b, nextstep c d a b))

newtype PresStep doc pres gest upd = 
            PresStep {presStep :: Step TransStep doc pres gest upd}
newtype TransStep gest upd doc pres = 
            TransStep {transStep :: Step PresStep gest upd doc pres}

instance Pack IO (PresStep a b c d) a b (TransStep c d a b) where
  pack = PresStep
  unpack = presStep

instance Pack IO (TransStep a b c d) a b (PresStep c d a b) where
  pack = TransStep
  unpack = transStep



lift :: Simple a b c d e f  -> a -> (PresStep c d e f)
lift simple =
  fix $ liftStep (present simple) 
      . liftStep (translate simple) 

combine :: PresStep a b e f -> PresStep b c d e -> PresStep a c d f
combine upr lwr =  
  fix (combineStepDown . combineStepUp) upr lwr


data Simple state mapping doc pres gest upd =
       Simple { present ::   LayerFunction IO state doc (mapping, state) pres
               , translate :: LayerFunction IO (mapping, state) gest state upd
               }


--------------------


mylayerIO0 = lift (Simple presIO0 transIO0) 0
presIO0 state doc = 
 do { putStr "presIO0"
    ; return (("map for state "++ show state, state), "<0 "++show state++","++doc++" 0>")
    }

transIO0 (map ,state) gest =
 do { putStr "transIO0"
    ; return . seq2 $ (state+1, map++","++gest)                   
    }

mylayerIO1 = lift (Simple presIO1 transIO1) 0.0
presIO1 state doc = return (("map for state "++ show state, state), "<1 "++show state++","++doc++" 1>")
transIO1 (map,state) gest = return (seq2 (state+1.0, map++","++gest))                   


seq2 (a,b) = seq a (seq b (a,b))

testIO :: IO ()
testIO =
 do { loop (mylayerIO0 `combine` mylayerIO1) 10 
    }
 where loop layer n =
        do { let PresStep f = layer
           ; (rend, TransStep f') <- f "doc"
           ; putStr rend
           ; (upd, layer') <- f' "gest"
           ; putStr upd
           ; if n > 0 then loop layer' (n-1) else return ()
           }
