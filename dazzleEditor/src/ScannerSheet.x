{
module ScannerSheet where

import DocTypes_Generated
import Scanner
}

$char = .#[\n\ \255]
$nobrace = .#[\{]

tokens :-
  \255                { mkStructuralToken }
  \n                  { mkToken $ \s -> KeyTk s }
  \                   { mkToken $ \s -> KeyTk s }
  \\graph             { mkToken $ \s -> KeyTk s }
  \\node\{$nobrace*\} { mkTokenEx (drop 6 . init) $ \s -> NodeRefTk }
  $char+              { mkToken $ \s -> WordTk }
{
}
--  in case of a lexical error from Alex, just add a single quote here. (total should be even)