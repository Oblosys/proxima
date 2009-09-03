{
module ScannerSheetHS where


import DocTypes_Generated
import Layout.ScanLib
}

$digit = 0-9
$char = .#[\n\ ]

tokens :-
<0> .*                                  { mkToken $ \s -> KeyTk s } -- at least one rule for 0 is required by Alex
<int>   \                               ;
<int>   \n                              ;
<int>   $digit+                         { mkToken $ \s -> IntTk }
<float> \n                              ;
<float> \                               ;
<float> $digit+(\.$digit+)?             { mkToken $ \s -> FloatTk }
<descr> \n                              { mkToken $ \s -> KeyTk " " }
<descr> \                               { mkToken $ \s -> KeyTk s }
<descr> $char+                          { mkToken $ \s -> WordTk }
{
}
--  in case of a lexical error from Alex, just add a single quote here. (total should be even)
