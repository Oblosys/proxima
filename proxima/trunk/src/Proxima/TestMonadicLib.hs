module Main.TestMonadicLib where

import Main.ArchitectureLibM


-- Simple test module for ArchitectureLibM

--------------------

type Step nStep a b c d = (a -> IO (nStep c d a b, b))

newtype PresStep doc pres gest upd = 
            PresStep {presStep :: Step IntrpStep doc pres gest upd}
newtype IntrpStep gest upd doc pres = 
            IntrpStep {intrpStep :: Step PresStep gest upd doc pres}

instance Pack IO (PresStep a b c d) a b (IntrpStep c d a b) where
  pack = PresStep
  unpack = presStep

instance Pack IO (IntrpStep a b c d) a b (PresStep c d a b) where
  pack = IntrpStep
  unpack = intrpStep



lift :: Simple a b c d e f  -> a -> (PresStep c d e f)
lift simple =
  fix $ liftStep (present simple) 
      . liftStep (interpret simple) 

combine :: PresStep a b e f -> PresStep b c d e -> PresStep a c d f
combine hghr lwr =  
  fix (combineStepDown . combineStepUp) hghr lwr


data Simple state mapping doc pres gest upd =
       Simple { present ::   LayerFunction IO state doc (mapping, state) pres
              , interpret :: LayerFunction IO (mapping, state) gest state upd
              }


--------------------


mylayerIO0 = lift (Simple presIO0 intrpIO0) 0
presIO0 state doc = 
 do { putStr "presIO0"
    ; return (("map for state "++ show state, state), "<0 "++show state++","++doc++" 0>")
    }

intrpIO0 (map ,state) gest =
 do { putStr "intrpIO0"
    ; return . seq2 $ (state+1, map++","++gest)                   
    }

mylayerIO1 = lift (Simple presIO1 intrpIO1) 0.0
presIO1 state doc = return (("map for state "++ show state, state), "<1 "++show state++","++doc++" 1>")
intrpIO1 (map,state) gest = return (seq2 (state+1.0, map++","++gest))                   


seq2 (a,b) = seq a (seq b (a,b))

testIO :: IO ()
testIO =
 do { loop (mylayerIO0 `combine` mylayerIO1) 10 
    }
 where loop layer n =
        do { let PresStep f = layer
           ; (IntrpStep f', rend) <- f "doc"
           ; putStr rend
           ; (layer', upd) <- f' "gest"
           ; putStr upd
           ; if n > 0 then loop layer' (n-1) else return ()
           }
