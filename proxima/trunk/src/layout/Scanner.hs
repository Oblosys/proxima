-----------------------------------------------------------------------------------------
{-| Module      : Scanner
    Copyright   : (c) 2007 Martijn Schrage
    License     : All Rights Reserved

    Maintainer  : martijn@cs.uu.nl
    Stability   : experimental
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Scanner where

import CommonTypes
import LayLayerTypes
import LayLayerUtils

import ScannerAG

tokenizeLay scannerSheet state layLvl@(LayoutLevel pres focus dt) (PresentationLevel _ (layout, idCounter)) = 
 let (pres', layout', idCounter') = scannerSheet idCounter pres
--     i = wrapLayout pres
     presLvl'                     = PresentationLevel pres' (layout',idCounter')
 in  (SetPres presLvl', state, layLvl) --LayoutLevel (markUnparsed pres') (markUnparsedF pres' focus'))




testNewScanner pres =
  let attrs = sem_Layout pres 
  in  ()