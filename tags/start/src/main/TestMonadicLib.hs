module TestMonadicLib where

import ArchitectureLibM


-- Simple test module for ArchitectureLibM

--------------------

type Step nextstep a b c d = (a -> IO (b, nextstep c d a b))

newtype PresStep doc pres gest upd = 
            PresStep {presStep :: Step TransStep doc pres gest upd}
newtype TransStep gest upd doc pres = 
            TransStep {transStep :: Step PresStep gest upd doc pres}

instance Pack (PresStep a b c d) a b (TransStep c d a b) where
  pack = PresStep
  unpack = presStep

instance Pack (TransStep a b c d) a b (PresStep c d a b) where
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
       Simple { present ::   LayerFunction state doc (mapping, state) pres
               , translate :: LayerFunction (mapping, state) gest state upd
               }
type LayerFunction horArgs vertArg horRess vertRes = 
       (horArgs -> vertArg -> IO (vertRes, horRess))


--------------------


mylayerIO0 = lift (Simple presIO0 transIO0) 0
presIO0 state doc = 
 do { putStr "presIO0"
    ; return ("<0 "++show state++","++doc++" 0>", ("map for state "++ show state, state))
    }

transIO0 (map ,state) gest =
 do { putStr "transIO0"
    ; return . seq2 $ (map++","++gest, state+1 )                   
    }

mylayerIO1 = lift (Simple presIO1 transIO1) 0.0
presIO1 state doc = return ("<1 "++show state++","++doc++" 1>", ("map for state "++ show state, state))
transIO1 (map,state) gest = return (seq2 (map++","++gest, state+1.0 ))                   


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
