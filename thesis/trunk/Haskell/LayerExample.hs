module Example where

import DPPClass_Lib
import Char

-- uncomment one of the two main functions.

--main = run' layers
main = run'' layersAB


-- The run function can be modified to create a continuing computation



-- a layer has two functions one going up, and one going down. No horizontal data flow
data Layer = Layer (String -> String) (String -> String)


-- because the combinators require functions of type LayerFunction in the layer, we make the following data type

data Layer' = Layer' (LayerFunction () String () String)
                     (LayerFunction () String () String)


-- wrap is used to create the Layer' values. The horizontal dataflow is () everywhere

wrap :: Layer -> Layer'
wrap (Layer f1 f2) = Layer' (\() str -> ((), f1 str) )
                            (\() str -> ((), f2 str) )                            


-- Almost automaticallly we can define the step type and Pack instances
-- step type for two layer steps:

type Step nextstep a b c d = (a ->(b, nextstep c d a b))

newtype Step1 l h h' l' = 
            Step1 {step1 :: Step Step2 l h h' l'}
newtype Step2 h l l' h' = 
            Step2 {step2 :: Step Step1 h l l' h'}

-- Pack instances:

instance Pack (Step1 l h h' l') l h (Step2  h' l' l h)
  where pack = Step1
        unpack = step1

instance Pack (Step2  h' l' l h) h' l' (Step1 l h h' l')
  where pack = Step2
        unpack = step2



-- Combinator definitions

lift (Layer' f1 f2) = fix $ liftStep f1
                          . liftStep f2


-- for combine we have to write down the type. It cannot be inferred
combine :: Step1 b c d e -> Step1 a b e f -> Step1 a c d f
combine upper lower = (fix $ combineStepUp 
                           . combineStepDown) upper lower

  


-- two example layers:
layer1 = Layer (map toUpper) (map toLower)
layer2 = Layer reverse       reverse


-- create an architecture:
layers =           lift (wrap layer1) () 
         `combine` lift (wrap layer2) ()



{-

general idea of 'run layers':
              
              hOut -------------------------|                    hOut' ------------------------|
               ^                            v                      ^                           v
layer1:   (map toUpper) ---> () --> (map toLower) ---> () --> (map toUpper) --> () ----> (map toLower)
               ^                            v                      ^                           v
layer2:    (reverse) ------> () ------> (reverse) ---> () ---> (reverse) ---> () --------> (reverse)     
               ^                            v                      ^                           v
     str   ----|                          lOut --------------------|                         lOut'

-}


run :: Step1 String String String String -> IO ()
run layers =
 do { let (Step1 fUp) = layers
    
    ; putStrLn $ "Enter a string:"
    ; str <- getLine
   
    ; putStrLn $ "Inserting "++str++" at bottom of layers"

    ; let (hOut, Step2 fDown) = fUp str
    
    ; putStrLn $ "Result coming from the top, hOut: "++ hOut

    ; let (lOut, Step1 fUp') = fDown hOut

    ; putStrLn $ "Result coming from the bottom, lOut: "++ lOut

    ; let (hOut', Step2 fDown') = fUp' lOut      -- remember not to use fUp, but fUp'
    
    ; putStrLn $ "Result coming from the top, hOut': "++ hOut'

    ; let (lOut', Step1 fUp') = fDown' hOut'    -- same here

    ; putStrLn $ "Result coming from the bottom, lOut': "++ lOut'
    
    ; return () 
    --; run (Step1 fUp')  -- instead of return ()      causes a continuing computation    
    }



-- with the top combinator, a monadic topLayer can be combined with the layers

top :: Monad m => (b -> m c) -> Step1 a b c d -> a -> m (d,Step1 a b c d)
top topLayer (Step1 step) a =
 do { let (b, Step2 step') = step a
    ; c <- topLayer b
    ; return $ step' c
    }

topLayer :: String -> IO String
topLayer hOut =
  do { putStrLn $ "Result coming from the top, hOut: "++ hOut
     ; return hOut
     }

run' :: Step1 String String String String -> IO ()
run' layers =
 do { putStrLn $ "Enter a string:"
    ; str <- getLine
   
    ; putStrLn $ "Inserting "++str++" at bottom of layers"
    
    ; (lOut, layers') <- top topLayer layers str

    ; putStrLn $ "Result coming from the bottom, lOut: "++ lOut

    ; (lOut', layers'') <- top topLayer layers' lOut
   
    ; putStrLn $ "Result coming from the bottom, lOut': "++ lOut'
    
    ; return () 
    --; run (layers'')  -- instead of return ()      causes a continuing computation    
    }


-- The problem is that we have to combine each time with the latest step function, if we do this in the top
-- combinator, then we need a new Step type to get around the recursive type problem

-- If we call the newtype Ztep

newtype Ztep m a b = Ztep (a -> m (b, (Ztep m a b)))

-- then the following top' combinator combines the layers with a top layer:

top' :: Monad m => (b -> m c) -> Step1 a b c d -> (Ztep m a d)
top' topLayer (Step1 step) = Ztep $ \a ->
 do { let (b, Step2 step') = step a
    ; c <- topLayer b
    ; let (d, Step1 step'') = step' c
    ; return $ (d, top' topLayer (Step1 step''))
    }

run'' :: Step1 String String String String -> IO ()
run'' openLayers =
 do { let (Ztep layers) = top' topLayer openLayers
    ; putStrLn $ "Enter a string:"
    ; str <- getLine
   
    ; putStrLn $ "Inserting "++str++" at bottom of layers"
    
    ; (lOut, Ztep layers') <- layers str

    ; putStrLn $ "Result coming from the bottom, lOut: "++ lOut

    ; (lOut', Ztep layers'') <- layers' lOut
   
    ; putStrLn $ "Result coming from the bottom, lOut': "++ lOut'
    
    ; return () 
    --; run (layers'')  -- instead of return ()      causes a continuing computation    
    }


{-
-- a different Ztep type that leads to simpler combinator and maybe even nicer code. 
-- Is this applicable to the meta combinator step types as well?

newtype Ztep m a b = Ztep (a, b -> m (Ztep m a b))

-- then the following top' combinator combines the layers with a top layer:

top' :: Monad m => (b -> m c) -> Step1 a b c d -> a -> m (Ztep m d a)
top' topLayer (Step1 step) a =
 do { let (b, Step2 step') = step a
    ; c <- topLayer b
    ; let (d, Step1 step'') = step' c
    ; return $ Ztep (d, top' topLayer (Step1 step''))
    }

run'' :: Step1 String String String String -> IO ()
run'' openLayers =
 do { let layers = top' topLayer openLayers
    ; putStrLn $ "Enter a string:"
    ; str <- getLine
   
    ; putStrLn $ "Inserting "++str++" at bottom of layers"
    
    ; Ztep (lOut, layers') <- layers str

    ; putStrLn $ "Result coming from the bottom, lOut: "++ lOut

    ; Ztep (lOut', layers'') <- layers' lOut
   
    ; putStrLn $ "Result coming from the bottom, lOut': "++ lOut'
    
    ; return () 
    --; run (layers'')  -- instead of return ()      causes a continuing computation    
    }

-}

-- More interesting layers with horizontal dataFlow. We directly put LayerFunctions in the layer, so no wrap is needed
-- The LayersAB datatype is parameterized with the type of the horizontal parameter

data LayerAB a = LayerAB (LayerFunction a String a String)
                         (LayerFunction a String a String)

layerA :: LayerAB Int
layerA = LayerAB (\h v -> (h+1, map toUpper v++show h))
                 (\h v -> (h+1, map toLower v++show h))

-- each layer step concatenates the showed horizontal int to right of the vertical result and
-- increments the horizontal int

layerB :: LayerAB Char
layerB = LayerAB (\h v -> (succ h, h : reverse v)) 
                 (\h v -> (succ h, h : reverse v))

-- each layer step concatenates the horizontal character to the left of the vertical result and
-- applies succ to the horizontal character


-- we only need a new lift
liftAB (LayerAB f1 f2) = fix $ liftStep f1
                             . liftStep f2


-- The second architecture:
layersAB =           liftAB layerA 0 
           `combine` liftAB layerB 'a'

{-

general idea of run layers:

(map toUpper) and (reverse) now also add the horizontal values to the vertical values


                  hOut -------------------------|                    hOut' ----------------------|
                   ^                            v                      ^                         v
                 "ARTS0"                     "ARTS0"               "CARTS01B2"              "CARTS01B2"
                   ^                            v                      ^                         v
layerA: 0 -> (map toUpper) ----> 1 ----> (map toLower) --> 2 --> (map toUpper) --> 3 ---> (map toLower) -> 4
                   ^                            v                      ^                         v
                 "arts"                      "arts01"              "carts01b"               "carts012b3"
                   ^                            v                      ^                         v
layerB: 'a' -> (reverse) -----> 'b' ------> (reverse) --> 'c' ---> (reverse) ---> 'd' ------> (reverse) -> 'e'
                   ^                            v                      ^                         v
                 "str"                      "b10stra"              "b10stra"               "d32b10strac"
                   ^                            v                      ^                         v
     str   --------|                          lOut --------------------|                       lOut'

-}
