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
Challenges/todo:

passing several Alex scanners (probably solved by Alex itself)

recover locators for parsing presentations

scanner errors should not crash Proxima. Maybe make an error token?

make sure that pres args to ParsingTk and StructuralTk are lazy
!! and should these pres args include the ParsingP/StructuralP  nodes? (seems that they should)


If we allow autowhitespace for strings and structural presentations
then Layout.hs adds too much whitespace in case of a parseErr presentation
However, enforcing the use of tokens in the presentationAG is tricky.

This is related to the error pres in the tokens. This is now not processed (so it is a layout instead of 
a presentation). Processing it recursively is not a problem for Structural presentations, but
for parsing presentations the lexer can fail on a lexer error, in which case no information is returned
and we cannot tokenize any structural token children.

-}





tokenizeLay :: (Show token) =>
               ScannerSheet doc node clip token -> state -> LayoutLevel doc node clip ->
               PresentationLevel doc node clip token -> (EditPresentation docLvl doc node clip token , state, LayoutLevel doc node clip)
tokenizeLay sheet state layLvl@(LayoutLevel lay focus dt) (PresentationLevel _ (_, idPCounter)) = 
 let (tokens, idPCounter', whitespaceMap) = scanStructural sheet LexHaskell Nothing idPCounter Map.empty lay 
     presLvl' = PresentationLevel (TokenP NoIDP (StructuralTk Nothing (castLayToPres lay) tokens NoIDP)) (whitespaceMap,idPCounter')
 in  debug Lay ("Scanned tokens:"++show tokens++"\n"++ show whitespaceMap) $
     (SetPres presLvl', state, layLvl)

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
                  ([Token doc node clip token], IDPCounter, WhitespaceMap)
scanStructural sheet lx loc idpc wm presentation =
  case presentation of
    ParsingP idP lx' pres'      -> scanPresentation sheet lx loc idpc wm idP lx' pres'
    EmptyP idd                  -> ([], idpc, wm)
    StringP idd str             -> ([], idpc, wm)
    ImageP idd istr st          -> ([], idpc, wm)
    PolyP idd pts w st          -> ([], idpc, wm)
    RectangleP idd w h lw st    -> ([], idpc, wm)
    EllipseP idd w h lw st      -> ([], idpc, wm)
    RowP id rf press            -> scanStructuralList sheet lx loc idpc wm press
    ColP id rf f press          -> scanStructuralList sheet lx loc idpc wm press
    OverlayP id press           -> scanStructuralList sheet lx loc idpc wm press
    FormatterP id press         -> scanStructuralList sheet lx loc idpc wm press
    GraphP id d w h edges press -> let (tokens, idpc', wm') = scanStructuralList sheet lx loc idpc wm press
                                   in  (GraphTk d edges loc id : tokens, idpc', wm')
    VertexP id v x y o pres     -> let (tokens, idpc', wm') = scanStructural sheet lx loc idpc wm pres
                                   in  (VertexTk v (x,y) loc id : tokens, idpc', wm')
    WithP ar pres               -> scanStructural sheet lx loc idpc wm pres
    StructuralP id pres         -> let (tokens, idpc', wm') = scanStructural sheet lx loc idpc wm pres
                                   in  ([StructuralTk loc (castLayToPres presentation) tokens id], idpc', wm')
    LocatorP newLoc pres        -> scanStructural sheet lx (Just newLoc) idpc wm pres
    pr -> debug Err ("Scanner.scanStructural: can't handle "++ show pr) ([], idpc, wm)


scanStructuralList :: Show token => 
                      ScannerSheet doc node clip token -> Lexer -> Maybe node -> 
                      IDPCounter -> WhitespaceMap -> [Layout doc node clip] ->
                      ([Token doc node clip token], IDPCounter, WhitespaceMap)
scanStructuralList sheet lx loc idpc wm []           = ([], idpc, wm)
scanStructuralList sheet lx loc idpc wm (pres:press) = 
  let (tokens,  idpc',  wm')   = scanStructural sheet lx loc idpc wm pres
      (tokenss, idpc'', wm'') = scanStructuralList sheet lx loc idpc' wm' press
  in  (tokens ++ tokenss, idpc'', wm'')

scanPresentation :: Show token => ScannerSheet doc node clip token -> 
                    Lexer -> Maybe node -> IDPCounter -> WhitespaceMap ->
                    IDP -> Lexer -> Layout doc node clip ->
                    ([Token doc node clip token], IDPCounter, WhitespaceMap)
scanPresentation sheet inheritedLex loc idPCounter whitespaceMap idP presentationLex lay =
 let lex = case  presentationLex of
             LexInherited -> inheritedLex
             _            -> presentationLex
     (idPCounter', scanChars, self, whitespaceMap') = sem_Layout lay idPCounter lex loc (scanStructural sheet) whitespaceMap
     (tokens, idPCounter'', whitespaceMap'') = sheet idPCounter' scanChars
 in  -- debug Lay ("Alex scanner:\n" ++ stringFromScanChars scanChars ++ "\n" ++ (show tokens)) $
     ( [ParsingTk (castLayToPres lay) tokens idP]
     , idPCounter'', whitespaceMap' `Map.union` whitespaceMap'')




type ScannerState = (Int, WhitespaceMap, (Int, Int)) -- (idP counter, whitespace map, (newlines, spaces))


mkToken :: (String -> userToken) -> ScannerState -> [ScanChar doc node clip userToken] -> 
           (Maybe (Token doc node clip userToken), ScannerState)
mkToken tokf (idPCounter, whitespaceMap, collectedWhitespace) scs = 
  let str = stringFromScanChars scs
      idp = idPFromScanChars scs
      userToken = tokf str
      (idp', idPCounter') = case idp of NoIDP -> (IDP idPCounter, idPCounter + 1)
                                        _     -> (idp,            idPCounter    )
  in  ( Just $ UserTk userToken str Nothing idp'
      , (idPCounter', Map.insert idp' collectedWhitespace whitespaceMap, (0,0)) 
      )


mkStructuralToken :: ScannerState -> [ScanChar doc node clip userToken] -> (Maybe (Token doc node clip userToken), ScannerState)
mkStructuralToken (idPCounter, whitespaceMap, collectedWhitespace) scs = 
  let Structural idp loc tokens lay = head scs
      (idp', idPCounter') = case idp of NoIDP -> (IDP idPCounter, idPCounter + 1)
                                        _     -> (idp,            idPCounter    )
  in  ( Just $ StructuralTk loc lay tokens idp'
      , (idPCounter', Map.insert idp' collectedWhitespace whitespaceMap, (0,0))
      )


collectWhitespace :: ScannerState -> [ScanChar doc node clip userToken] -> (Maybe a, ScannerState)
collectWhitespace (idPCounter, whitespaceMap, (newlines, spaces)) (c:cs) =
  let newWhitespace = case c of
                        Char _ '\n' -> (newlines + 1 + length cs, spaces                )
                        Char _ ' '  -> (newlines                , spaces + 1 + length cs)
                        -- will always be a Char
  in (Nothing, (idPCounter, whitespaceMap, newWhitespace))


-- TODO handle pattern match failures with internal errors
