{
module ScannerSheet where


import DocTypes_Generated
import Layout.ScanLib
}

$char = .#[\n\ ]

tokens :-
  \n+                             { collectWhitespace }
  \ +                             { collectWhitespace }
  Leaf                            { mkToken $ \s -> LeafToken }
  Bin                             { mkToken $ \s -> BinToken }
  [0-9]+                          { mkToken $ \s -> IntToken }
  [\(\)]                          { mkToken $ \s -> SymToken s }

{
}
--  in case of a lexical error from Alex, just add a single quote here. (total should be even)
