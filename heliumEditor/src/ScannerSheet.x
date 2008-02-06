{
module ScannerSheet (scanner) where

import Maybe
import qualified Data.Map as Map
import DocTypes_Generated
import PresTypes
import LayLayerTypes
import LayLayerUtils

import Scanner
}

$digit = 0-9            -- digits
$lower = [a-z]
$upper = [A-Z]
$alpha = [$lower $upper]        -- alphabetic characters

tokens :-
    

  \n+              { collectWhitespace }
  \ +              { collectWhitespace }
  \255             { mkStructuralToken }
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
  \:               { mkToken $ \s -> StrTk s }
  \;               { mkToken $ \s -> StrTk s }
  \\               { mkToken $ \s -> StrTk s }
  \=               { mkToken $ \s -> StrTk s }
  $digit+          { mkToken $ \s -> IntTk }
  $lower [$alpha $digit \_ \']* { mkToken $ \s -> LIdentTk }
  $upper [$alpha $digit \_ \']* { mkToken $ \s -> UIdentTk }

{

{-
Old helium scanner seemed to have only keywords, no symbols



-}
-- -----------------------------------------------------------------------------
-- Basic wrapper

type ScanChar_ = ScanChar Document Node ClipDoc UserToken
-- ScanChar_ is ScanChar applied to its parameter types. This is necessary, because Alex
-- cannot handle paramters in the AlexInput type.


type AlexInput  = (Char, [ScanChar_])

alexGetChar (_, [])   = Nothing
alexGetChar (_, Char _ c : cs) = Just (c, (c,cs))
alexGetChar (_, Structural _ _ _ _ : cs) = Just ('\255', ('\255',cs))

alexInputPrevChar (c,_) = c

-- TODO final whitespace?
scanner :: IDPCounter -> [ScanChar_] -> ([Token Document Node ClipDoc UserToken], IDPCounter, WhitespaceMap)
scanner idPCounter scs = 
  let (mTokens, (idPCounter', whitespaceMap, _)) = alexScanTokenzz initScannerState scs
  in  (catMaybes mTokens, idPCounter', whitespaceMap)
 where initScannerState :: ScannerState
       initScannerState = (idPCounter, Map.empty,(0,0))

   
alexScanTokenzz :: ScannerState -> [ScanChar_] -> 
                   ([Maybe (Token Document Node ClipDoc UserToken)], ScannerState)
alexScanTokenzz initState scs = go initState ('\n',scs)
  where go :: ScannerState -> (Char, [ScanChar_]) -> ([Maybe (Token Document Node ClipDoc UserToken)], ScannerState)
        go state inp@(_,str) =
	  case alexScan inp 0 of
		AlexEOF -> ([], state)
		AlexError (_,remaining) -> error ("lexical error at "++show (take 10 $ stringFromScanChars remaining))
		AlexSkip  inp' len     -> go state inp'
		AlexToken inp' len act -> let (mToken, state') = act state (take len str)
		                              (mTokens, state'') = go state' inp'
		                          in  (mToken : mTokens, state'') 


f = 'x' -- removing this line causes Alex to produce a lexical error.. ???
}