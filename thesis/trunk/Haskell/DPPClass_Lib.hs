module DPPClass_Lib where

-- param order


fix :: (a->a) -> a
fix a = let fixa = a fixa
         in fixa

type LayerFunction horArg vertArg horRes vertRes =
       horArg -> vertArg -> (horRes, vertRes)  --


class Pack step arg res nStep | step -> arg res nStep where
  pack :: (arg -> (res, nStep)) -> step
  unpack :: step -> arg -> (res, nStep)

liftStep :: Pack step vArg vRes nStep => 
            (hArg -> vArg -> (hRes,vRes)) -> (hRes -> nStep) -> hArg -> step
liftStep layerF next horArg = pack $
    \vertArg -> let (horRes, vertRes) = layerF horArg vertArg --
                in  (vertRes, next horRes)                     --

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


{- Different version (does not type check). Probably for Jurre Laven's MSc thesis

class Pack step a b c d nStep where
  pack :: (arg -> (res, nStep c d a b)) -> step a b c d
  unpack :: step a b c d -> arg -> (res, nStep c d a b)

liftStep :: Pack step vArg vRes c d nStep => 
            (hArg -> vArg -> (vRes,hRes)) -> (hRes -> nStep c d vArg vRes) -> hArg -> step vArg vRes c d
liftStep layerF next horArg = pack $
    \vertArg -> let (vertRes, horRes) = layerF horArg vertArg                     
                in  (vertRes, next horRes)


combineStepDown :: ( Pack stepC h l c d nStepC 
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
-}