{
module ScannerSheet where

import DocTypes_Generated
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
-- Old helium scanner seemed to have only keywords, no symbols

  $digit+          { mkToken $ \s -> IntTk }
  $lower [$alpha $digit \_ \']* { mkToken $ \s -> LIdentTk }
  $upper [$alpha $digit \_ \']* { mkToken $ \s -> UIdentTk }


-- in case of a lexical error from Alex, just add a single quote here. (total should be even)