-----------------------------------------------------------------------------------------
{-| Module      : Scanner
    Copyright   : (c) 2007 Martijn Schrage
    License     : All Rights Reserved

    Maintainer  : martijn@cs.uu.nl
    Stability   : experimental
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Layout.Scanner where

import Common.CommonTypes
import Layout.LayLayerTypes
import Layout.LayLayerUtils hiding (empty)
import Data.Map as Map hiding (mapMaybe, (!))
import Maybe

import Layout.ScannerAG

{-

The scanner converts the Layout level to: TokenP (StructuralTk [..])

locators for tokens are taken from its string. The first character that has a locator determines
the locator for the entire string.

Same thing for idps



Unclear:
what happens if  string has no idp? What is the problem if idp's are created new?

The layouter adds whitespace according to whitespace map.
Only the tokens of a PresentationTk.
  These are put in (possibly nested) rows, since we don't want another attribute tokens.
  Now we can use the presentation.
  
  Non tokens:
  StringP?
  If an overlay is used ?
  Column?
  Formatter?


Challenges/todo:

Lexical errors create one big token, which discards structural children and ruins the presentation.
  after lexical error, just split remaining input into error tokens and structural tokens
  and represent all whitespace in the error tokens. Layout should make sure that whitespace in 
  error tokens is converted to breaks and spaces.
  Probably also the whitespace in the state before the error occurred should be put in the error token.

  check layout what Layouter does now with whitespace for error tokens

  Instead of using Alex for structurals, we could call Alex several times for the string parts of the
  layout. This way, we keep the structurals out of the user specified scanner. The whitespace in the
  state can be put into the structural token.
  
Store trailing whitespace. Maybe Alex can emit whitespace tokens which are put in the whitespace map
after the whole layout has been scanned. 

passing several Alex scanners (probably solved by Alex itself)

focus for parse errors?

what to do with columns and formatters containing tokens?


if formatters have linebreak whitespace, layouter will probably go wrong.

whitespace if no tokens are present? (example: decl* with empty pres)

scanner duplicates idp's

handle focus at the right of the last char (and whitespace to the right of the last char)
we could put an extra token there, which is parsed automatically.

problem with structurals. focus in auto type sig is not recognized properly



If we allow autowhitespace for strings and structural presentations
then Layout.hs adds too much whitespace in case of a parseErr presentation
However, enforcing the use of tokens in the presentationAG is tricky.

This is related to the error pres in the tokens. This is now not processed (so it is a layout instead of 
a presentation). Processing it recursively is not a problem for Structural presentations, but
for parsing presentations the lexer can fail on a lexer error, in which case no information is returned
and we cannot tokenize any structural token children.


Longer term: 
  recover locators for parsing presentations. This way, a presentation may contain extra state not in
  the presentation. It is tricky however when this information can be recovered.

-}





tokenizeLay :: (DocNode node, Show token) =>
               ScannerSheet doc node clip token -> state -> LayoutLevel doc node clip token ->
               PresentationLevel doc node clip token -> (EditPresentation docLvl doc node clip token , state, LayoutLevel doc node clip token)
tokenizeLay sheet state layLvl@(LayoutLevel lay focus dt) (PresentationLevel _ (_, idPCounter)) = 
 let (tokens, idPCounter', whitespaceMap) = scanStructural sheet (fixFocus focus) LexHaskell Nothing [] idPCounter Map.empty lay 
     presLvl' = PresentationLevel (TokenP NoIDP (StructuralTk 0 Nothing (castLayToPres lay) tokens NoIDP)) (whitespaceMap,idPCounter')
 in  (case focus of FocusP (PathP sf si) (PathP ef ei) -> debug Lay ("focus start\n"++ show sf++ show si ++ "focus end\n"++ show ef++ show ei ++"\n")
                    _ -> id
     ) 
--     debug Lay ("Scanned tokens:"++show tokens++"\n"++ show whitespaceMap) $
     (SetPres presLvl', state, layLvl)

fixFocus (FocusP (PathP sp si) (PathP ep ei)) = ((sp,si),(ep,ei))
fixFocus (FocusP (PathP sp si) NoPathP)       = ((sp,si),(sp,si))
fixFocus (FocusP NoPathP       (PathP ep ei)) = ((ep,ei),(ep,ei))
fixFocus _                                    = (([],0),([],0)) -- not that clean, altough this will never match with any focus

{-
tokenize traverses the structural parts of the tree, calling scanPresentation on Parsing subtrees.
It is a simple AG:

inherited attribute   sheet the scanner sheet            (simply copied down)
                      focus: the presentation focus      (simply copied down)
                      lx: the lexer being used #currently not in use# (assigned at ParsingP in structural)
                      loc: the nearest parent locator    (assigned at LocatorP case)
                      pth: the current path              (parent adds child nr to path)
inh & synthesized     idP: the presentation id counter   (threaded)
                      wm: the whitespace map             (threaded)
synthesized attribute pres: the tokenized presentation   (constructed at every case because of cast from Layout to Presentation)
                      
-}
scanStructural :: (DocNode node, Show token) => ScannerSheet doc node clip token -> ((Path,Int),(Path,Int)) ->
                  Lexer -> Maybe node -> Path -> IDPCounter -> WhitespaceMap -> Layout doc node clip token ->
                  ([Token doc node clip token], IDPCounter, WhitespaceMap)
scanStructural sheet foc lx loc pth idpc wm presentation =
  case presentation of
    ParsingP idP p lx' pres'    -> scanPresentation sheet foc lx loc (pth++[0]) idpc wm idP p lx' pres'
    EmptyP idd                  -> ([], idpc, wm)
    StringP idd str             -> ([], idpc, wm)
    ImageP idd istr st          -> ([], idpc, wm)
    PolyP idd pts w st          -> ([], idpc, wm)
    RectangleP idd w h lw st    -> ([], idpc, wm)
    EllipseP idd w h lw st      -> ([], idpc, wm)
    RowP id rf press            -> scanStructuralList sheet foc lx loc pth idpc wm press
    ColP id rf f press          -> scanStructuralList sheet foc lx loc pth idpc wm press
    OverlayP id press           -> scanStructuralList sheet foc lx loc pth idpc wm press
    FormatterP id press         -> scanStructuralList sheet foc lx loc pth idpc wm press
    GraphP id d w h edges press -> let (tokens, idpc', wm') = scanStructuralList sheet foc lx loc pth idpc wm press
                                   in  (GraphTk d edges loc id : tokens, idpc', wm')
    VertexP id v x y o pres     -> let (tokens, idpc', wm') = scanStructural sheet foc lx loc (pth++[0]) idpc wm pres
                                   in  (VertexTk v (x,y) loc id : tokens, idpc', wm')
    WithP ar pres               -> scanStructural sheet foc lx loc (pth++[0]) idpc wm pres
    StructuralP id pres         -> let (tokens, idpc', wm') = scanStructural sheet foc lx loc (pth++[0]) idpc wm pres
                                   in  ([StructuralTk 0 loc (castLayToPres pres) tokens id], idpc', wm')
                                                      -- position is not used, so set to 0
    LocatorP newLoc pres        -> scanStructural sheet foc lx (Just newLoc) (pth++[0]) idpc wm pres
    pr -> debug Err ("Scanner.scanStructural: can't handle "++ show pr) ([], idpc, wm)


scanStructuralList :: (DocNode node, Show token) => 
                      ScannerSheet doc node clip token -> ((Path,Int),(Path,Int)) -> Lexer ->
                      Maybe node -> Path ->
                      IDPCounter -> WhitespaceMap -> [Layout doc node clip token] ->
                      ([Token doc node clip token], IDPCounter, WhitespaceMap)
scanStructuralList sheet foc lx loc pth idpc wm press = scanStructuralList' sheet foc lx loc pth idpc wm 0 press
 where scanStructuralList' sheet foc lx loc pth idpc wm i []           = ([], idpc, wm)
       scanStructuralList' sheet foc lx loc pth idpc wm i (pres:press) = 
         let (tokens,  idpc',  wm')  = scanStructural sheet foc lx loc (pth++[i]) idpc wm pres
             (tokenss, idpc'', wm'') = scanStructuralList' sheet foc lx loc pth idpc' wm' (i+1) press
         in  (tokens ++ tokenss, idpc'', wm'')



scanPresentation :: (DocNode node, Show token) => ScannerSheet doc node clip token -> ((Path,Int),(Path,Int)) -> 
                    Lexer -> Maybe node -> Path -> IDPCounter -> WhitespaceMap ->
                    IDP -> Maybe (ClipParser doc node clip token) -> Lexer -> Layout doc node clip token ->
                    ([Token doc node clip token], IDPCounter, WhitespaceMap)
scanPresentation sheet foc inheritedLex loc pth idPCounter whitespaceMap idP
                 parser presentationLex lay =
 let --lay = parsing (row [ structural (row [ text "<", parsing (text "blaa"), text ">"]), text "   " ])
     lex = case  presentationLex of
             LexInherited -> inheritedLex
             _            -> presentationLex
     (idPCounter', pos, scanChars, scannedFocusEnd, scannedFocusStart, self, whitespaceMap') = sem_Layout lay foc idPCounter lex loc pth 0 (scanStructural sheet) Nothing Nothing whitespaceMap
       -- sheet is not used by the AG, so we already pass it to scanStructural, saving an extra attribute
     afterLastCharFocusStart = focusAfterLastChar scanChars scannedFocusStart
     afterLastCharFocusEnd   = focusAfterLastChar scanChars scannedFocusEnd
     focusedScanChars = markFocus markFocusStart scannedFocusStart $
                        markFocus markFocusEnd   scannedFocusEnd 
                                  scanChars 
     (tokens, idPCounter'', scannedWhitespaceMap, lastWhitespaceFocus) = sheet idPCounter' focusedScanChars
     lastWhitespaceFocus' = markFocusInLastWhitespaceFocus afterLastCharFocusStart afterLastCharFocusEnd lastWhitespaceFocus
 in  --debug Lay ("Last whitespaceFocus':" ++ show lastWhitespaceFocus') $
     --debug Lay ("whitespaceMap" ++ show scannedWhitespaceMap ) $
     --debug Lay ("Alex scanner:\n" ++ show (scannedFocusStart,scannedFocusEnd)++ stringFromScanChars scanChars) $
     ( [ParsingTk parser loc (castLayToPres lay) tokens idP]
     , idPCounter'', scannedWhitespaceMap `Map.union` whitespaceMap')

-- when there are no tokens, no whitespace is saved
updateScannedWhitespaceMap []     lastWhitespaceFocus scannedWhitespaceMap = scannedWhitespaceMap
updateScannedWhitespaceMap tokens lastWhitespaceFocus scannedWhitespaceMap =
  let f = Map.update (\a -> Just a) (tokenIDP (last tokens)) scannedWhitespaceMap
  in undefined

focusAfterLastChar scs Nothing    = False
focusAfterLastChar scs (Just pos) = pos == length scs

-- if the focus is after last char, we cannot encode it in the scanChars (it is handled separately) 
markFocus f Nothing          scs = scs
markFocus f focus@(Just pos) scs = if not $ focusAfterLastChar scs focus 
                                   then case splitAt pos scs of
                                          (left, focusedChar:right) -> left ++ f focusedChar : right
                                          _                         -> debug Err "markFocus: problem, cannot record focus" scs
                                                                             else scs


markFocusInLastWhitespaceFocus afterLastCharFocusStart afterLastCharFocusEnd 
                               ((newlines, spaces), (focusStart, focusEnd)) =
  let focusStart' = if afterLastCharFocusStart then Just (newlines+spaces) else focusStart
      focusEnd'   = if afterLastCharFocusEnd   then Just (newlines+spaces) else focusEnd
  in  ((newlines, spaces), (focusStart', focusEnd'))
