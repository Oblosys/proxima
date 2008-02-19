{
module ScannerSheet where

import DocTypes_Generated
import Layout.Scanner
}

$char = .#[\n\ \255]
$nobrace = .#[\{]

tokens :-
  \255                            { mkStructuralToken }
  \n                              { mkToken $ \s -> KeyTk s }
  \                               { mkToken $ \s -> KeyTk s }
  \\graph                         { mkToken $ \s -> KeyTk s }
  \\node\{$nobrace*\}             { mkTokenEx (init . drop 6) $ \s -> NodeRefTk }
  \\label\{$nobrace*\}            { mkTokenEx (init . drop 7) $ \s -> LabelTk }
  \\ref\{$nobrace*\}\{$nobrace*\} { mkTokenEx (takeWhile (/='}') . drop 5) $ \s -> LabelRefTk }
  $char+                          { mkToken $ \s -> WordTk }
{
}
--  in case of a lexical error from Alex, just add a single quote here. (total should be even)
