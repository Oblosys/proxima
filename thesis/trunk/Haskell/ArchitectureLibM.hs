module ArchitectureLibM where

fix :: (a->a) -> a
fix a = let fixa = a fixa
         in fixa

type LayerFunction m horArg vertArg horRes vertRes = 
       (horArg -> vertArg -> m (horRes, vertRes))


class Pack m step arg res nStep | step -> m arg res nStep where
  pack :: (arg -> m (nStep, res)) -> step
  unpack :: step -> arg -> m (nStep, res)

liftStep :: (Monad m, Pack m step vArg vRes nStep) => 
            (hArg -> vArg -> m (hRes,vRes)) -> (hRes -> nStep) -> hArg -> step
liftStep layerF nStep horArg = pack $ 
    \vertArg -> do {(horRes, vertRes) <- layerF horArg vertArg
                   ; return (nStep horRes, vertRes)
                   }
                   

combineStepDown :: ( Monad md
                   , Pack md stepC h l nStepC
                   , Pack md stepH h m nStepH
                   , Pack md stepL m l nStepL ) => 
                   (nStepH -> nStepL -> nStepC) -> stepH -> stepL -> stepC
combineStepDown nStep hghr lwr = pack $
    \high -> do { (nextHghr, med) <- (unpack hghr) high
                ; (nextLwr, low) <- (unpack lwr) med
                ; return (nStep nextHghr nextLwr, low)
                }

combineStepUp :: ( Monad md
                 , Pack md stepC l h nStepC
                 , Pack md stepH m h nStepH
                 , Pack md stepL l m nStepL ) => 
                 (nStepH -> nStepL -> nStepC) -> stepH -> stepL -> stepC
combineStepUp nStep hghr lwr = pack $
    \low -> do { (nextLwr, med) <- (unpack lwr) low
               ; (nextHghr, high) <- (unpack hghr) med
               ; return (nStep nextHghr nextLwr, high)
               }


