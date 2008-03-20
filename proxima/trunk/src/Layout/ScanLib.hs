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
import Data.Map as Map hiding (mapMaybe, (!))
import Maybe

alexGetChar (_, [])   = Nothing
alexGetChar (_, Char _ _ _ _ c : cs) = Just (c, (c,cs))
alexGetChar (_, Structural _ _ _ _ _ _ : cs) = Just ('\255', ('\255', cs))

alexInputPrevChar (c,_) = c

type ScannerState = (Position,       Int,         WhitespaceMap,  WhitespaceFocus) 
                 -- (token position, idP counter, whitespace map, collected (newlines, spaces))

mkToken = mkTokenEx id



initWhitespaceFocus :: WhitespaceFocus
initWhitespaceFocus = ((0,0),(Nothing,Nothing))


-- the first strf is for manipulating the string that is stored in the token
mkTokenEx :: (String->String) -> (String -> userToken) -> ScannerState -> [ScanChar doc node clip userToken] -> 
           (Maybe (Token doc node clip userToken), ScannerState)
mkTokenEx strf tokf (tokenPos, idPCounter, whitespaceMap, (collectedWhitespace, whitespaceFocus)) scs = 
  let tokenLayout = TokenLayout collectedWhitespace
                                whitespaceFocus
                                (getFocusStartEnd scs)
      str = strf $ stringFromScanChars scs
      idp = idPFromScanChars scs
      loc = locFromScanChars scs
      userToken = tokf str
      (idp', idPCounter') = case idp of NoIDP -> (IDP idPCounter, idPCounter + 1)
                                        _     -> case Map.lookup idp whitespaceMap of
                                                   Nothing -> (idp,            idPCounter    )
                                                   _       -> (IDP idPCounter, idPCounter + 1)
                           -- if there is no idp, we create a new one. If there is an idp, but
                           -- it is already in the whitespaceMap, we also create a new one.                                                   
                                                   
  in  ( Just $ UserTk tokenPos userToken str loc idp'
      , (tokenPos + 1, idPCounter', Map.insert idp' tokenLayout whitespaceMap, initWhitespaceFocus) 
      )


mkStructuralToken :: ScannerState -> [ScanChar doc node clip userToken] -> (Maybe (Token doc node clip userToken), ScannerState)
mkStructuralToken (tokenPos, idPCounter, whitespaceMap, (collectedWhitespace, whitespaceFocus)) 
                  scs@[Structural idp _ _ loc tokens lay] = 
  let tokenLayout = TokenLayout collectedWhitespace
                                whitespaceFocus
                                (getFocusStartEnd scs)
      (idp', idPCounter') = case idp of NoIDP -> (IDP idPCounter, idPCounter + 1)
                                        _     -> (idp,            idPCounter    )
  in  debug Lay ("Structural " ++ show tokenLayout) $
      ( Just $ StructuralTk tokenPos loc lay tokens idp'
      , (tokenPos + 1, idPCounter', Map.insert idp' tokenLayout whitespaceMap, initWhitespaceFocus)
      )


collectWhitespace :: ScannerState -> [ScanChar doc node clip userToken] -> (Maybe a, ScannerState)
collectWhitespace (tokenPos, idPCounter, whitespaceMap, ((newlines, spaces), focusStartEnd)) 
                  (sc@(Char _ _ _ _ c):scs) = -- will always be a Char
  let newWhitespace = case c of                                                
                        '\n' -> (newlines + 1 + length scs, 0                      ) -- newline resets spaces
                        ' '  -> (newlines                 , spaces + 1 + length scs)
  in (Nothing, ( tokenPos, idPCounter, whitespaceMap, 
                 (newWhitespace, updateFocusStartEnd (newlines+spaces) focusStartEnd  $ sc:scs)
               )
     )
       -- The preceding newlines and spaces are the offset for the focus

-- TODO handle pattern match failures with internal errors


getFocusStartEnd scs = updateFocusStartEnd 0 (Nothing, Nothing) scs

updateFocusStartEnd :: Int -> FocusStartEnd -> [ScanChar doc node clip userToken] -> FocusStartEnd
updateFocusStartEnd i (oldFocusStart, oldFocusEnd) cs =
  (getFocusStart i oldFocusStart cs, getFocusEnd i oldFocusEnd cs) 
  
getFocusStart i oldFocusStart []     = Nothing
getFocusStart i oldFocusStart (c:cs) = if hasFocusStartMark c then Just i else getFocusStart (i+1) oldFocusStart cs

getFocusEnd i oldFocusEnd []     = Nothing
getFocusEnd i oldFocusEnd (c:cs) = if hasFocusEndMark c then Just i else getFocusEnd (i+1) oldFocusEnd cs
