module ArchitectureLib where

fix :: (a->a) -> a
fix a = let fixa = a fixa
         in fixa

class Pack step arg res nStep | step -> arg res nStep where
  pack :: (arg -> (res, nStep)) -> step
  unpack :: step -> arg -> (res, nStep)

liftStep :: Pack step vArg vRes nStep => 
            (hArgs -> vArg -> (vRes,hRess)) -> (hRess -> nStep) -> hArgs -> step
liftStep layerF next horArgs = pack $
    \vertArg -> let (vertRes, horRess) = layerF horArgs vertArg                     
                in  (vertRes, next horRess)

combineStepDown :: ( Pack stepC h l nStepC 
                   , Pack stepU h m nStepU
                   , Pack stepL m l nStepL ) => 
                   (nStepU -> nStepL -> nStepC) -> stepU -> stepL -> stepC
combineStepDown nextStep upr lwr = pack $
    \high -> let (med, nextUpr) = (unpack upr) high
                 (low, nextLwr) = (unpack lwr) med
             in  (low, nextStep nextUpr nextLwr)

combineStepUp :: ( Pack stepC l h nStepC 
                 , Pack stepU m h nStepU
                 , Pack stepL l m nStepL ) => 
                 (nStepU -> nStepL -> nStepC) -> stepU -> stepL -> stepC
combineStepUp nextStep upr lwr = pack $
    \low -> let (med, nextLwr) = (unpack lwr) low
                (high, nextUpr) = (unpack upr) med
            in  (high, nextStep nextUpr nextLwr)
