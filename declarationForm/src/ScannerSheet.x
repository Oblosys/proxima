{
module ScannerSheetHS where


import DocTypes_Generated
import Layout.ScanLib
}

$digit = 0-9
$char = .#[\n\ ]

tokens :-
<0> \n                                  { mkToken $ \s -> KeyTk " " } -- at least one rule for 0 is required by Alex
<float> \n                              { mkToken $ \s -> KeyTk s }
<float> \                               { mkToken $ \s -> KeyTk s }
<float> $digit+(\.$digit+)?             { mkToken $ \s -> FloatTk }
<descr> \n                              { mkToken $ \s -> KeyTk " " }
<descr> \                               { mkToken $ \s -> KeyTk s }
<descr> $char+                          { mkToken $ \s -> WordTk }
{
}
--  in case of a lexical error from Alex, just add a single quote here. (total should be even)
