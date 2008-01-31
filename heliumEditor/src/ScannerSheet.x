{
module ScannerSheet (alexScanTokenz) where

import Maybe
import qualified Data.Map as Map
import DocTypes_Generated
import PresTypes

}

$digit = 0-9            -- digits
$alpha = [a-zA-Z]        -- alphabetic characters

tokens :-
    

  \n+              { collectWhitespace }
  \ +              { collectWhitespace }
  module           { mkToken $ \s -> StrTk s }
  let              { mkToken $ \s -> StrTk s }
  in               { mkToken $ \s -> StrTk s }
  False            { mkToken $ \s -> StrTk s }
  True             { mkToken $ \s -> StrTk s }
  if               { mkToken $ \s -> StrTk s }
  then             { mkToken $ \s -> StrTk s }
  else             { mkToken $ \s -> StrTk s }
  let              { mkToken $ \s -> StrTk s }
  in               { mkToken $ \s -> StrTk s }
  case             { mkToken $ \s -> StrTk s }
  of               { mkToken $ \s -> StrTk s }
  Chess            { mkToken $ \s -> StrTk s }
  board            { mkToken $ \s -> StrTk s }
  Slides           { mkToken $ \s -> StrTk s }
  pres             { mkToken $ \s -> StrTk s }
  \+               { mkToken $ \s -> StrTk s }
  \-               { mkToken $ \s -> StrTk s }
  \*               { mkToken $ \s -> StrTk s }
  \%               { mkToken $ \s -> StrTk s }
  \/               { mkToken $ \s -> StrTk s }
  \^               { mkToken $ \s -> StrTk s }
  \-\>             { mkToken $ \s -> StrTk s }
  \(               { mkToken $ \s -> StrTk s }
  \)               { mkToken $ \s -> StrTk s }
  \{               { mkToken $ \s -> StrTk s }
  \}               { mkToken $ \s -> StrTk s }
  \[               { mkToken $ \s -> StrTk s }
  \]               { mkToken $ \s -> StrTk s }
  \,               { mkToken $ \s -> StrTk s }
  \;               { mkToken $ \s -> StrTk s }
  \\               { mkToken $ \s -> StrTk s }
  \=               { mkToken $ \s -> StrTk s }
  $digit+          { mkToken $ \s -> IntTk }
  $alpha [$alpha $digit \_ \']*        { mkToken $ \s -> LIdentTk }

{

{-
Old helium scanner seemed to have only keywords, no symbols




-}
-- -----------------------------------------------------------------------------
-- Basic wrapper


type AlexInput = (Char,String)

alexGetChar (_, [])   = Nothing
alexGetChar (_, c:cs) = Just (c, (c,cs))

alexInputPrevChar (c,_) = c

-- TODO final whitespace?
alexScanTokenz str = 
  let (mTokens, (_, whitespaceMap, _)) = alexScanTokenzz initScannerState str
  in  (catMaybes mTokens, whitespaceMap)
  
alexScanTokenzz :: ScannerState -> String -> 
                   ([Maybe (Token doc node clip UserToken)], ScannerState)
alexScanTokenzz initState str = go initState ('\n',str)
  where go :: ScannerState -> (Char, String) -> ([Maybe (Token doc node clip UserToken)], ScannerState)
        go state inp@(_,str) =
	  case alexScan inp 0 of
		AlexEOF -> ([], state)
		AlexError (_,remaining) -> error ("lexical error at "++show (take 10 remaining))
		AlexSkip  inp' len     -> go state inp'
		AlexToken inp' len act -> let (mToken, state') = act state (take len str)
		                              (mTokens, state'') = go state' inp'
		                          in  (mToken : mTokens, state'') 


type ScannerState = (Int, WhitespaceMap, (Int, Int)) -- (idP counter, (newlines, spaces))

initScannerState :: ScannerState
initScannerState = (0, Map.empty,(0,0))

mkToken :: (String -> UserToken) -> ScannerState -> String -> 
           (Maybe (Token doc node clip UserToken), ScannerState)
mkToken tokf (idpCounter, whitespaceMap, collectedWhitespace) str = 
  let userToken = tokf str
      idp = IDP idpCounter
  in  (Just $ UserTk userToken str Nothing idp, (idpCounter + 1, Map.insert idp collectedWhitespace whitespaceMap, (0,0)) )

-- TODO: factorize
collectWhitespace :: ScannerState -> String -> (Maybe a, ScannerState)
collectWhitespace (idpCounter, whitespaceMap, (newlines, spaces)) ('\n':newlineStr) = (Nothing, (idpCounter, whitespaceMap, (newlines + 1 + length newlineStr, spaces)))
collectWhitespace (idpCounter, whitespaceMap, (newlines, spaces)) (' ':spaceStr)    = (Nothing, (idpCounter, whitespaceMap, (newlines, spaces + 1 + length spaceStr)))
}