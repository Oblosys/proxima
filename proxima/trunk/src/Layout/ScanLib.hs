-----------------------------------------------------------------------------------------
{-| Module      : Layout.ScanLib
    Copyright   : (c) 2007 Martijn Schrage
    License     : All Rights Reserved

    Maintainer  : martijn@cs.uu.nl
    Stability   : experimental
    Portability : 

This module is imported by the Alex scanner. Because of the module exports, the scanner
only needs to import this module (and DocTypes_Generated for the token data type).

Some scanner functionality could not be factorized and can be found in AlexTemplate-ghc

-}
-----------------------------------------------------------------------------------------

module Layout.ScanLib ( module Layout.ScanLib
                      , module Maybe
                      , module Layout.LayLayerTypes
                      , module Layout.LayLayerUtils
                      , module Map
                      )  where

import Common.CommonTypes
import Layout.LayLayerTypes
import Layout.LayLayerUtils hiding (empty)
import qualified Data.Map as Map hiding (mapMaybe, (!))
import Maybe

alexGetChar (_, [])   = Nothing
alexGetChar (_, Char _ _ _ _ c : cs) = Just (c, (c,cs))
alexGetChar (_, Structural _ _ _ _ _ _ : cs) = Just ('\255', ('\255', cs))

alexInputPrevChar (c,_) = c

type ScannerState = Position
                 -- token position

initScannerState :: ScannerState
initScannerState = 0

mkToken = mkTokenEx id

-- the first strf is for manipulating the string that is stored in the token
mkTokenEx :: (String->String) -> (String -> userToken) -> ScannerState -> [ScanChar doc node clip userToken] -> 
           (ScannedToken doc node clip userToken, ScannerState)
mkTokenEx strf tokf tokenPos scs = 
  let str = strf $ stringFromScanChars scs
      idp = idPFromScanChars scs
      loc = locFromScanChars scs
      userToken = tokf str
                                                   
  in  ( ScannedToken (getFocusStartEnd scs) $ UserTk tokenPos userToken str loc idp
      , tokenPos + 1
      )


mkStructuralToken :: ScannerState -> [ScanChar doc node clip userToken] -> 
                     (ScannedToken doc node clip userToken, ScannerState)
mkStructuralToken tokenPos
                  scs@[Structural idp _ _ loc tokens lay] = 
      ( ScannedToken (getFocusStartEnd scs) $ StructuralTk tokenPos loc lay tokens idp
      , tokenPos + 1
      )

collectWhitespace :: ScannerState -> [ScanChar doc node clip userToken] -> 
                     (ScannedToken doc node clip userToken, ScannerState)
collectWhitespace tokenPos scs =
  let whitespaceChars = stringFromScanChars scs
      scannedWhitespace = ( length (filter (=='\n') whitespaceChars)
                          , length (takeWhile (==' ') (reverse whitespaceChars))
                          )
  in  ( ScannedWhitespace (getFocusStartEnd scs) scannedWhitespace
      , tokenPos
      )

                         
-- TODO handle pattern match failures with internal errors


getFocusStartEnd scs = updateFocusStartEnd 0 (Nothing, Nothing) scs

updateFocusStartEnd :: Int -> FocusStartEnd -> [ScanChar doc node clip userToken] -> FocusStartEnd
updateFocusStartEnd i (oldFocusStart, oldFocusEnd) cs =
  (getFocusStart i oldFocusStart cs, getFocusEnd i oldFocusEnd cs) 
  
getFocusStart i oldFocusStart []     = Nothing
getFocusStart i oldFocusStart (c:cs) = if hasFocusStartMark c then Just i else getFocusStart (i+1) oldFocusStart cs

getFocusEnd i oldFocusEnd []     = Nothing
getFocusEnd i oldFocusEnd (c:cs) = if hasFocusEndMark c then Just i else getFocusEnd (i+1) oldFocusEnd cs
