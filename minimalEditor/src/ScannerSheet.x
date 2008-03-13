{
module ScannerSheet where


import DocTypes_Generated
import Layout.Scanner
}

$char = .#[\n\ \255]  -- we need to exclude the proxima specific stuff (can be reduced to only \255)

tokens :-
  \255                            { mkStructuralToken }
  \n+                             { collectWhitespace }
  \ +                             { collectWhitespace }
  
-- These three rules are required by Proxima and should not be removed in most cases

  Leaf                            { mkToken $ \s -> LeafToken }
  Bin                             { mkToken $ \s -> BinToken }
  [\(\)]                          { mkToken $ \s -> SymToken s }

{
}
--  in case of a lexical error from Alex, just add a single quote here. (total should be even)
