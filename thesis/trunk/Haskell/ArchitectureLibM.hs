module ArchitectureLibM where


fix :: (a->a) -> a
fix a = let fixa = a fixa
         in fixa

type LayerFunction m horArgs vertArg horRess vertRes = 
       (horArgs -> vertArg -> m (vertRes, horRess))

class Pack m step arg res nStep | step -> m arg res nStep where
  pack :: (arg -> m (res, nStep)) -> step
  unpack :: step -> arg -> m (res, nStep)

liftStep :: (Monad m, Pack m step vArg vRes nStep) => 
            (hArgs -> vArg -> m (vRes,hRess)) -> (hRess -> nStep) -> hArgs -> step
liftStep layerF next horArgs = pack $ 
    \vertArg -> do {(vertRes, horRess) <- layerF horArgs vertArg
                   ; return (vertRes, next horRess)
                   }
                   

combineStepDown :: ( Monad md
                   , Pack md stepC h l nStepC
                   , Pack md stepU h m nStepU
                   , Pack md stepL m l nStepL ) => 
                   (nStepU -> nStepL -> nStepC) -> stepU -> stepL -> stepC
combineStepDown nextStep upr lwr = pack $
    \high -> do { (med, nextUpr) <- (unpack upr) high
                ; (low, nextLwr) <- (unpack lwr) med
                ; return (low, nextStep nextUpr nextLwr)
                }

combineStepUp :: ( Monad md
                 , Pack md stepC l h nStepC
                 , Pack md stepU m h nStepU
                 , Pack md stepL l m nStepL ) => 
                 (nStepU -> nStepL -> nStepC) -> stepU -> stepL -> stepC
combineStepUp nextStep upr lwr = pack $
    \low -> do { (med, nextLwr) <- (unpack lwr) low
               ; (high, nextUpr) <- (unpack upr) med
               ; return (high, nextStep nextUpr nextLwr)
               }


