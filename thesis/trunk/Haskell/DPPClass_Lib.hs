module DPPClass_Lib where

-- param order


fix :: (a->a) -> a
fix a = let fixa = a fixa
         in fixa

type LayerFunction horArg vertArg horRes vertRes =
       horArg -> vertArg -> (horRes, vertRes)

class Pack step arg res nStep | step -> arg res nStep, nStep -> step where
  pack :: (arg -> (nStep, res)) -> step
  unpack :: step -> arg -> (nStep, res)

liftStep :: Pack step vArg vRes nStep => 
            (hArg -> vArg -> (hRes,vRes)) ->
            (hRes -> nStep) ->
            hArg -> step
liftStep layerF next horArg = pack $
    \vertArg -> let (horRes, vertRes) = layerF horArg vertArg
                in  (next horRes, vertRes)                    

combineStepDown :: ( Pack stepC h l nStepC 
                   , Pack stepH h m nStepH
                   , Pack stepL m l nStepL ) => 
                   (nStepH -> nStepL -> nStepC) -> stepH -> stepL -> stepC
combineStepDown nStep hghr lwr = pack $
    \high -> let (nextHghr, med) = (unpack hghr) high
                 (nextLwr, low) = (unpack lwr) med
             in  (nStep nextHghr nextLwr, low)

combineStepUp :: ( Pack stepC l h nStepC 
                 , Pack stepH m h nStepH
                 , Pack stepL l m nStepL ) => 
                 (nStepH -> nStepL -> nStepC) -> stepH -> stepL -> stepC
combineStepUp nStep hghr lwr = pack $
    \low -> let (nextLwr, med) = (unpack lwr) low
                (nextHghr, high) = (unpack hghr) med
            in  (nStep nextHghr nextLwr, high)


{- Different version (does not type check). Is this for Jurre Laven's MSc thesis?

class Pack step a b c d nStep where
  pack :: (arg -> (res, nStep c d a b)) -> step a b c d
  unpack :: step a b c d -> arg -> (res, nStep c d a b)

liftStep :: Pack step vArg vRes c d nStep => 
            (hArg -> vArg -> (vRes,hRes)) -> (hRes -> nStep c d vArg vRes) -> hArg -> step vArg vRes c d
liftStep layerF next horArg = pack $
    \vertArg -> let (vertRes, horRes) = layerF horArg vertArg                     
                in  (vertRes, next horRes)


combineStepDown :: ( Pack stepC h l c d nStepC 
                   , Pack stepU h m nStepH
                   , Pack stepL m l nStepL ) => 
                   (nStepH -> nStepL -> nStepC) -> stepH -> stepL -> stepC
combineStepDown nStep hghr lwr = pack $
    \high -> let (med, nextHghr) = (unpack hghr) high
                 (low, nextLwr) = (unpack lwr) med
             in  (low, nStep nextHghr nextLwr)

combineStepUp :: ( Pack stepC l h nStepC 
                 , Pack stepH m h nStepH
                 , Pack stepL l m nStepL ) => 
                 (nStepH -> nStepL -> nStepC) -> stepH -> stepL -> stepC
combineStepUp nStep hghr lwr = pack $
    \low -> let (med, nextLwr) = (unpack lwr) low
                (high, nextHghr) = (unpack hghr) med
            in  (high, nStep nextHghr nextLwr)
-}