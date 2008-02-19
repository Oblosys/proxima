module LayLayerTypes ( module LayTypes
                     , module LayLayerTypes    ) where


import CommonTypes
--import PresTypes -- already imported via LayoutTypes
import LayTypes


type LayerStateLay doc node clip = Layout doc node clip -- clipboard
-- is this  layer local state, instead of level local state? It is focus related, so probably, it should be 
-- local to LayoutLevel, just like the focus

type ScannerSheet doc node clip token = 
      (IDPCounter -> [ScanChar doc node clip token] -> ([Token doc node clip token], IDPCounter, WhitespaceMap))
      
data ScanChar doc node clip token = 
       Char         { idp :: IDP, startFocusMark :: FocusMark, endFocusMark :: FocusMark
                    , char :: Char
                    }
     | Structural   { idp :: IDP, startFocusMark :: FocusMark, endFocusMark :: FocusMark
                    , locator :: (Maybe node), tokens :: [Token doc node clip token]
                    , prs :: (Presentation doc node clip token) -- original pres to show in case of parse/scan errors
                    }
     | EndOfParsing { startFocusMark :: FocusMark, endFocusMark :: FocusMark
                    }
     
data FocusMark = FocusMark | NoFocusMark deriving Eq