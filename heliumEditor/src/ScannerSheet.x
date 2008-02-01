{
module ScannerSheet (alexScanTokenz) where

import Maybe
import qualified Data.Map as Map
import DocTypes_Generated
import PresTypes
import LayLayerTypes
import LayLayerUtils
}

$digit = 0-9            -- digits
$alpha = [a-zA-Z]        -- alphabetic characters

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
  \;               { mkToken $ \s -> StrTk s }
  \\               { mkToken $ \s -> StrTk s }
  \=               { mkToken $ \s -> StrTk s }
  $digit+          { mkToken $ \s -> IntTk }
  $alpha [$alpha $digit \_ \']*        { mkToken $ \s -> LIdentTk }

{

{-
Old helium scanner seemed to have only keywords, no symbols

Why does \xffa


-}
-- -----------------------------------------------------------------------------
-- Basic wrapper

type ScanChar_ = ScanChar Document Node ClipDoc UserToken
-- ScanChar_ is ScanChar applied to its parameter types. This is necessary, because Alex
-- cannot handle paramters in the AlexInput type.


type AlexInput  = (Char, [ScanChar_])

alexGetChar (_, [])   = Nothing
alexGetChar (_, Char c : cs) = Just (c, (c,cs))
alexGetChar (_, Structural _ _ : cs) = Just ('\255', ('\255',cs))

alexInputPrevChar (c,_) = c

-- TODO final whitespace?
alexScanTokenz :: [ScanChar_] -> ([Token Document Node ClipDoc UserToken], WhitespaceMap)
alexScanTokenz scs = 
  let (mTokens, (_, whitespaceMap, _)) = alexScanTokenzz initScannerState scs
  in  (catMaybes mTokens, whitespaceMap)
  
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


type ScannerState = (Int, WhitespaceMap, (Int, Int)) -- (idP counter, (newlines, spaces))

initScannerState :: ScannerState
initScannerState = (0, Map.empty,(0,0))

mkToken :: (String -> UserToken) -> ScannerState -> [ScanChar_] -> 
           (Maybe (Token doc node clip UserToken), ScannerState)
mkToken tokf (idpCounter, whitespaceMap, collectedWhitespace) scs = 
  let str = stringFromScanChars scs
      userToken = tokf str
      idp = IDP idpCounter
  in  ( Just $ UserTk userToken str Nothing idp
      , (idpCounter + 1, Map.insert idp collectedWhitespace whitespaceMap, (0,0)) 
      )

collectWhitespace :: ScannerState -> [ScanChar_] -> (Maybe a, ScannerState)
collectWhitespace (idpCounter, whitespaceMap, (newlines, spaces)) (c:cs) =
  let newWhitespace = case c of
                        Char '\n' -> (newlines + 1 + length cs, spaces                )
                        Char ' '  -> (newlines                , spaces + 1 + length cs)
                        -- will always be a Char
  in (Nothing, (idpCounter, whitespaceMap, newWhitespace))

mkStructuralToken :: ScannerState -> [ScanChar_] -> (Maybe (Token Document Node ClipDoc UserToken), ScannerState)
mkStructuralToken (idpCounter, whitespaceMap, collectedWhitespace) scs = 
  let idp = IDP idpCounter
      Structural loc pres = head scs
  in  ( Just $ StructuralTk loc pres [] idp
      , (idpCounter, whitespaceMap, (0,0))
      ) -- TODO handle whitespace for structurals

-- TODO handle pattern match failures with internal errors

}