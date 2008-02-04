-----------------------------------------------------------------------------------------
{-| Module      : Scanner
    Copyright   : (c) 2007 Martijn Schrage
    License     : All Rights Reserved

    Maintainer  : martijn@cs.uu.nl
    Stability   : experimental
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Scanner where

import CommonTypes
import LayLayerTypes
import LayLayerUtils
import qualified Data.Map as Map

import ScannerAG


{-
Challenges:

passing several Alex scanners (probably solved by Alex itself)

maybe presentation id's need to be kept. This could be what causes the problem when identifier list is edited


fix: Parse errors empty the whole presentation
-}





tokenizeLay :: (Show token) =>
               ScannerSheet doc node clip token -> state -> LayoutLevel doc node clip ->
               PresentationLevel doc node clip token -> (EditPresentation docLvl doc node clip token , state, LayoutLevel doc node clip)
tokenizeLay sheet@(scannerSheet, alexScanner) state layLvl@(LayoutLevel lay focus dt) (PresentationLevel _ (layout, idPCounter)) = 
 let --(pres, layout', idCounter') = scannerSheet idCounter lay
     (pres', idPCounter', whitespaceMap) = scanStructural sheet LexHaskell Nothing idPCounter Map.empty lay 
     presLvl' = PresentationLevel pres' (whitespaceMap,idPCounter')
 in  debug Lay ("\n\n\n"++ show whitespaceMap) (SetPres presLvl', state, layLvl)

{-
tokenize traverses the structural parts of the tree, calling scanPresentation on Parsing subtrees.
It is a simple AG:

inherited attribute   lx: the lexer of                   (simply copied down)
                      loc: the nearest parent locator    (assigned at LocatorP case)
inh & synthesized     idP: the presentation id counter   (threaded)
                      wm: the whitespace map             (threaded)
synthesized attribute pres: the tokenized presentation   (constructed at every case because of cast from Layout to Presentation)
                      
-}
scanStructural :: Show token => ScannerSheet doc node clip token ->
                  Lexer -> Maybe node -> IDPCounter -> WhitespaceMap -> Layout doc node clip ->
                  (Presentation doc node clip token, IDPCounter, WhitespaceMap)
scanStructural sheet lx loc idpc wm pres =
  case pres of
    ParsingP idP lx' pres'      -> scanPresentation sheet lx loc idpc wm idP lx' pres'
    EmptyP idd                  -> (EmptyP idd,               idpc, wm)
    StringP idd str             -> (StringP idd str,          idpc, wm)
    ImageP idd istr st          -> (ImageP idd istr st,       idpc, wm)
    PolyP idd pts w st          -> (PolyP idd pts w st,       idpc, wm)
    RectangleP idd w h lw st    -> (RectangleP idd w h lw st, idpc, wm)
    EllipseP idd w h lw st      -> (EllipseP idd w h lw st,   idpc, wm)
    RowP id rf press            -> let (press', idpc', wm') = scanStructuralList sheet lx loc idpc wm press
                                   in  (RowP id rf press',    idpc', wm')
    ColP id rf f press          -> let (press', idpc', wm') = scanStructuralList sheet lx loc idpc wm press
                                   in  (ColP id rf f press',  idpc', wm')
    OverlayP id press           -> let (press', idpc', wm') = scanStructuralList sheet lx loc idpc wm press
                                   in  (OverlayP id press',   idpc', wm')
    GraphP id d w h edges press -> let (press', idpc', wm') = scanStructuralList sheet lx loc idpc wm press
                                   in  (GraphP id d w h edges press', idpc, wm')
    FormatterP id press         -> let (press', idpc', wm') = scanStructuralList sheet lx loc idpc wm press
                                   in  (FormatterP id press', idpc', wm')
    WithP ar pres               -> let (pres', idpc', wm') = scanStructural sheet lx loc idpc wm pres
                                   in  (WithP ar pres',       idpc', wm')
    StructuralP id pres         -> let (pres', idpc', wm') = scanStructural sheet lx loc idpc wm pres
                                   in  (StructuralP id pres', idpc', wm')
    VertexP id v x y o pres     -> let (pres', idpc', wm') = scanStructural sheet lx loc idpc wm pres
                                   in  (VertexP id v x y o pres', idpc', wm')
    LocatorP newLoc pres        -> let (pres', idpc', wm') = scanStructural sheet lx (Just newLoc) idpc wm pres
                                   in  (LocatorP newLoc pres', idpc', wm')
    pr -> debug Err ("Scanner.scanStructural: can't handle "++ show pr) (castLayToPres pr, idpc, wm)


scanStructuralList :: Show token => 
                      ScannerSheet doc node clip token -> Lexer -> Maybe node -> 
                      IDPCounter -> WhitespaceMap -> [Layout doc node clip] ->
                      ([Presentation doc node clip token], IDPCounter, WhitespaceMap)
scanStructuralList sheet lx loc idpc wm []           = ([], idpc, wm)
scanStructuralList sheet lx loc idpc wm (pres:press) = 
  let (pres',  idpc', wm') = scanStructural sheet lx loc idpc wm pres
      (press', idpc'', wm'') = scanStructuralList sheet lx loc idpc' wm' press
  in  (pres':press', idpc'', wm'')

scanPresentation :: Show token => ScannerSheet doc node clip token -> 
                    Lexer -> Maybe node -> IDPCounter -> WhitespaceMap ->
                    IDP -> Lexer -> Layout doc node clip ->
                    (Presentation doc node clip token, IDPCounter, WhitespaceMap)
scanPresentation sheet inheritedLex loc idPCounter whitespaceMap idP presentationLex pres =
 let lex = case  presentationLex of
             LexInherited -> inheritedLex
             _            -> presentationLex
     (idPCounter', scanChars, self, whitespaceMap') = sem_Layout pres idPCounter lex loc (scanStructural sheet) whitespaceMap
     (_,alexScanner) = sheet
     (tokens, idPCounter'', whitespaceMap'') = alexScanner idPCounter' scanChars
 in  debug Lay ("Alex scanner:\n" ++ stringFromScanChars scanChars ++ "\n" ++ (show tokens)) $
     ( ParsingP idP presentationLex $ row $ map (TokenP NoIDP) tokens
     , idPCounter'', whitespaceMap' `Map.union` whitespaceMap'')




type ScannerState = (Int, WhitespaceMap, (Int, Int)) -- (idP counter, whitespace map, (newlines, spaces))


mkToken :: (String -> userToken) -> ScannerState -> [ScanChar doc node clip userToken] -> 
           (Maybe (Token doc node clip userToken), ScannerState)
mkToken tokf (idPCounter, whitespaceMap, collectedWhitespace) scs = 
  let str = stringFromScanChars scs
      idp = idPFromScanChars scs
      userToken = tokf str
      (idp', idPCounter') = case idp of NoIDP -> (IDP idPCounter, idPCounter + 1)
                                        _     -> (idp,          idPCounter    )
  in  ( Just $ UserTk userToken str Nothing idp'
      , (idPCounter', Map.insert idp' collectedWhitespace whitespaceMap, (0,0)) 
      )


mkStructuralToken :: ScannerState -> [ScanChar doc node clip userToken] -> (Maybe (Token doc node clip userToken), ScannerState)
mkStructuralToken (idPCounter, whitespaceMap, collectedWhitespace) scs = 
  let Structural idp loc pres = head scs
      (idp', idPCounter') = case idp of NoIDP -> (IDP idPCounter, idPCounter + 1)
                                        _     -> (idp,          idPCounter    )
  in  ( Just $ StructuralTk loc pres [] idp'
      , (idPCounter', Map.insert idp' collectedWhitespace whitespaceMap, (0,0))
      ) -- TODO handle whitespace for structurals


collectWhitespace :: ScannerState -> [ScanChar doc node clip userToken] -> (Maybe a, ScannerState)
collectWhitespace (idPCounter, whitespaceMap, (newlines, spaces)) (c:cs) =
  let newWhitespace = case c of
                        Char _ '\n' -> (newlines + 1 + length cs, spaces                )
                        Char _ ' '  -> (newlines                , spaces + 1 + length cs)
                        -- will always be a Char
  in (Nothing, (idPCounter, whitespaceMap, newWhitespace))


-- TODO handle pattern match failures with internal errors
