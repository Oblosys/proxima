{
module ScannerSheet where

import DocTypes_Generated
import Layout.ScanLib
}

$char = .#[\n\ ]
$digit = 0-9
$nobrace = .#[\{]

tokens :-
  \n                              { mkToken $ \s -> KeyTk s }
  \                               { mkToken $ \s -> KeyTk s }
  \\graph                         { mkToken $ \s -> KeyTk s }
  \\node\{$nobrace*\}             { mkTokenEx (init . drop 6) $ \s -> NodeRefTk }
  \\label\{$nobrace*\}            { mkTokenEx (init . drop 7) $ \s -> LabelTk }
  \\ref\{$nobrace*\}              { mkTokenEx (takeWhile (/='}') . drop 5) $ \s -> LabelRefTk }
--  $digit+                       { mkToken $ \s -> IntTk }
  $char+                          { mkToken $ \s -> WordTk }
{
}
--  in case of a lexical error from Alex, just add a single quote here. (total should be even)
