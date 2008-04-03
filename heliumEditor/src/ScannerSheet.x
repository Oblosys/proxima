{
module ScannerSheet where

import DocTypes_Generated
import Layout.ScanLib
}

$digit = 0-9            -- digits
$lower = [a-z]
$upper = [A-Z]
$alpha = [$lower $upper]        -- alphabetic characters

tokens :-
    

  [\n \ ]+         { collectWhitespace }
  module           { mkToken $ \s -> KeyTk s }
  let              { mkToken $ \s -> KeyTk s }
  in               { mkToken $ \s -> KeyTk s }
  False            { mkToken $ \s -> KeyTk s }
  True             { mkToken $ \s -> KeyTk s }
  if               { mkToken $ \s -> KeyTk s }
  then             { mkToken $ \s -> KeyTk s }
  else             { mkToken $ \s -> KeyTk s }
  let              { mkToken $ \s -> KeyTk s }
  in               { mkToken $ \s -> KeyTk s }
  case             { mkToken $ \s -> KeyTk s }
  of               { mkToken $ \s -> KeyTk s }
  Chess            { mkToken $ \s -> KeyTk s }
  board            { mkToken $ \s -> KeyTk s }
  Slides           { mkToken $ \s -> KeyTk s }
  pres             { mkToken $ \s -> KeyTk s }
  \+               { mkToken $ \s -> KeyTk s }
  \-               { mkToken $ \s -> KeyTk s }
  \*               { mkToken $ \s -> KeyTk s }
  \%               { mkToken $ \s -> KeyTk s }
  \/               { mkToken $ \s -> KeyTk s }
  \^               { mkToken $ \s -> KeyTk s }
  \-\>             { mkToken $ \s -> KeyTk s }
  \(               { mkToken $ \s -> KeyTk s }
  \)               { mkToken $ \s -> KeyTk s }
  \{               { mkToken $ \s -> KeyTk s }
  \}               { mkToken $ \s -> KeyTk s }
  \[               { mkToken $ \s -> KeyTk s }
  \]               { mkToken $ \s -> KeyTk s }
  \,               { mkToken $ \s -> KeyTk s }
  \:               { mkToken $ \s -> KeyTk s }
  \;               { mkToken $ \s -> KeyTk s }
  \\               { mkToken $ \s -> KeyTk s }
  \=               { mkToken $ \s -> KeyTk s }
  \.\.\.           { mkToken $ \s -> KeyTk s }
-- Old helium scanner seemed to have only keywords, no symbols

  $digit+          { mkToken $ \s -> IntTk }
  $lower [$alpha $digit \_ \']* { mkToken $ \s -> LIdentTk }
  $upper [$alpha $digit \_ \']* { mkToken $ \s -> UIdentTk }


-- in case of a lexical error from Alex, just add a single quote here. (total should be even)
