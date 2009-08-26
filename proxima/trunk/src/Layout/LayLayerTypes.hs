module Layout.LayLayerTypes ( module Layout.LayTypes
                     , module Layout.LayLayerTypes    ) where


import Common.CommonTypes
--import PresTypes -- already imported via LayoutTypes
import Layout.LayTypes


data LayerStateLay doc enr node clip token = LayerStateLay { getClipboard :: (Layout doc enr node clip token) -- clipboard
                                                       , getLastSearchTerm :: (Maybe String)
                                                       }
-- is this  layer local state, instead of level local state? It is focus related, so probably, it should be 
-- local to LayoutLevel, just like the focus

type ScannerSheet doc enr node clip token = 
      (Int -> [ScanChar doc enr node clip token] -> ([ScannedToken doc enr node clip token], Int))
      
data ScanChar doc enr node clip token = 
       Char         { idp :: IDP, startFocusMark :: FocusMark, endFocusMark :: FocusMark
                    , locator :: (Maybe node)
                    , char :: Char
                    }
     | Structural   { idp :: IDP, startFocusMark :: FocusMark, endFocusMark :: FocusMark
                    , locator :: (Maybe node), tokens :: [Token doc enr node clip token]
                    , prs :: (Presentation doc enr node clip token) -- original pres to show in case of parse/scan errors
                    } 
     | Style        { styleTag :: ScannedStyleTag
                    }
     deriving Show

isCharScanChar (Char _ _ _ _ _) = True
isCharScanChar _            = False

isStructuralScanChar (Structural _ _ _ _ _ _) = True
isStructuralScanChar _            = False
 
isStyleScanChar (Style _ ) = True
isStyleScanChar _          = False

-- Scanned token is used as the return type for the alex scanner. Actual tokens are put
-- in ScannedToken, and all whitespace is put in ScannedWhitespace. It is related to the preceding
-- token in the whitespace map. 
data ScannedToken doc enr node clip token =
       ScannedWhitespace FocusStartEnd Whitespace
     | ScannedToken      FocusStartEnd (Token doc enr node clip token) deriving Show

showScannedTokens scannedTokens = "Scanned tokens:\n" ++ concat [ "  " ++ show st ++ "\n" | st <- scannedTokens ]
   
data FocusMark = FocusMark | NoFocusMark deriving (Eq, Show)

data StyleTag = StyleTag Style StartOrEnd deriving (Show, Eq, Ord)
