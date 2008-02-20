-----------------------------------------------------------------------------------------
{-| Module      : Scanner
    Copyright   : (c) 2007 Martijn Schrage
    License     : All Rights Reserved

    Maintainer  : martijn@cs.uu.nl
    Stability   : experimental
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Layout.Scanner ( module Layout.Scanner
               , module Maybe
               , module Layout.LayLayerTypes
               , module Layout.LayLayerUtils
               , module Map
               ) where
-- we export a lot of stuff here, so the Alex sheet only has to import Scanner.hs
-- unfortunately Haskell does not allow us to export the qualified Map
import Common.CommonTypes
import Layout.LayLayerTypes
import Layout.LayLayerUtils hiding (empty)
import Data.Map as Map hiding (mapMaybe, (!))
import Maybe

import Layout.ScannerAG

{-
Challenges/todo:

passing several Alex scanners (probably solved by Alex itself)

focus for parse errors?

recover locators for parsing presentations

handle focus at the right of the last char (and whitespace to the right of the last char)
we could put an extra token there, which is parsed automatically.

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
 let (tokens, idPCounter', whitespaceMap) = scanStructural sheet (fixFocus focus) LexHaskell Nothing [] idPCounter Map.empty lay 
     presLvl' = PresentationLevel (TokenP NoIDP (StructuralTk Nothing (castLayToPres lay) tokens NoIDP)) (whitespaceMap,idPCounter')
 in  (case focus of FocusP (PathP sf si) (PathP ef ei) -> debug Lay ("focus start\n"++ show sf++ show si ++ "focus end\n"++ show ef++ show ei ++"\n")
                    _ -> id
     ) 
     debug Lay ("Scanned tokens:"++show tokens++"\n"++ show whitespaceMap) $
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
scanStructural :: Show token => ScannerSheet doc node clip token -> ((Path,Int),(Path,Int)) ->
                  Lexer -> Maybe node -> Path -> IDPCounter -> WhitespaceMap -> Layout doc node clip ->
                  ([Token doc node clip token], IDPCounter, WhitespaceMap)
scanStructural sheet foc lx loc pth idpc wm presentation =
  case presentation of
    ParsingP idP lx' pres'      -> scanPresentation sheet foc lx loc (pth++[0]) idpc wm idP lx' pres'
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
                                   in  ([StructuralTk loc (castLayToPres presentation) tokens id], idpc', wm')
    LocatorP newLoc pres        -> scanStructural sheet foc lx (Just newLoc) (pth++[0]) idpc wm pres
    pr -> debug Err ("Scanner.scanStructural: can't handle "++ show pr) ([], idpc, wm)


scanStructuralList :: Show token => 
                      ScannerSheet doc node clip token -> ((Path,Int),(Path,Int)) -> Lexer ->
                      Maybe node -> Path ->
                      IDPCounter -> WhitespaceMap -> [Layout doc node clip] ->
                      ([Token doc node clip token], IDPCounter, WhitespaceMap)
scanStructuralList sheet foc lx loc pth idpc wm press = scanStructuralList' sheet foc lx loc pth idpc wm 0 press
 where scanStructuralList' sheet foc lx loc pth idpc wm i []           = ([], idpc, wm)
       scanStructuralList' sheet foc lx loc pth idpc wm i (pres:press) = 
         let (tokens,  idpc',  wm')  = scanStructural sheet foc lx loc (pth++[i]) idpc wm pres
             (tokenss, idpc'', wm'') = scanStructuralList' sheet foc lx loc pth idpc' wm' (i+1) press
         in  (tokens ++ tokenss, idpc'', wm'')



scanPresentation :: Show token => ScannerSheet doc node clip token -> ((Path,Int),(Path,Int)) -> 
                    Lexer -> Maybe node -> Path -> IDPCounter -> WhitespaceMap ->
                    IDP -> Lexer -> Layout doc node clip ->
                    ([Token doc node clip token], IDPCounter, WhitespaceMap)
scanPresentation sheet foc inheritedLex loc pth idPCounter whitespaceMap idP presentationLex lay =
 let lex = case  presentationLex of
             LexInherited -> inheritedLex
             _            -> presentationLex
     (idPCounter', pos, scanChars, scannedFocusStart, scannedFocusEnd, self, whitespaceMap') =
       sem_Layout lay foc idPCounter lex loc pth 0 (scanStructural sheet) Nothing Nothing whitespaceMap
       -- sheet is not used by the AG, so we already pass it to scanStructural, saving an extra attribute
     afterLastCharFocusStart = focusAfterLastChar scanChars scannedFocusStart
     afterLastCharFocusEnd   = focusAfterLastChar scanChars scannedFocusEnd
     focusedScanChars = markFocus markFocusStart scannedFocusStart $
                        markFocus markFocusEnd   scannedFocusEnd 
                                  scanChars 
     (tokens, idPCounter'', scannedWhitespaceMap, lastWhitespaceFocus) = sheet idPCounter' focusedScanChars
     lastWhitespaceFocus' = markFocusInLastWhitespaceFocus afterLastCharFocusStart afterLastCharFocusEnd lastWhitespaceFocus
     whitespaceMapWithLastWhitespace = scannedWhitespaceMap
 in  debug Lay ("Last whitespaceFocus':" ++ show lastWhitespaceFocus') $
     debug Lay ("whitespaceMap" ++ show scannedWhitespaceMap ) $
     -- debug Lay ("Alex scanner:\n" ++ stringFromScanChars scanChars ++ "\n" ++ (show tokens)) $
     ( [ParsingTk (castLayToPres lay) tokens idP]
     , idPCounter'', whitespaceMapWithLastWhitespace `Map.union` whitespaceMap')

focusAfterLastChar scs Nothing    = False
focusAfterLastChar scs (Just pos) = pos == length scs

-- if the focus is after last char, we cannot encode it in the scanChars (it is handled separately) 
markFocus f Nothing          scs = scs
markFocus f focus@(Just pos) scs = if not $ focusAfterLastChar scs focus 
                                   then let (left, focusedChar:right) = splitAt pos scs
                                        in  left ++ f focusedChar : right
                                   else scs


markFocusInLastWhitespaceFocus afterLastCharFocusStart afterLastCharFocusEnd 
                               ((newlines, spaces), (focusStart, focusEnd)) =
  let focusStart' = if afterLastCharFocusStart then Just (newlines+spaces) else focusStart
      focusEnd'   = if afterLastCharFocusEnd   then Just (newlines+spaces) else focusEnd
  in  ((newlines, spaces), (focusStart', focusEnd'))

-- The functions below are used by the Alex Scanner, which imports Scanner.hs
-- Some scanner functionality could not be factorized and can be found in AlexTemplate-ghc

alexGetChar (_, [])   = Nothing
alexGetChar (_, Char _ _ _ c : cs) = Just (c, (c,cs))
alexGetChar (_, Structural _ _ _ _ _ _ : cs) = Just ('\255', ('\255', cs))

alexInputPrevChar (c,_) = c

type ScannerState = (Int, WhitespaceMap, WhitespaceFocus) -- (idP counter, whitespace map, (newlines, spaces))

mkToken = mkTokenEx id



initWhitespaceFocus :: WhitespaceFocus
initWhitespaceFocus = ((0,0),(Nothing,Nothing))

data TokenLayout = TokenLayout WhitespaceFocus         -- preceding whitespace & focus
                               FocusStartEnd           -- focus in token 
                               (Maybe WhitespaceFocus) -- for the last token only: whitespace &B focus
                               deriving Show

-- the first strf is for manipulating the string that is stored in the token
mkTokenEx :: (String->String) -> (String -> userToken) -> ScannerState -> [ScanChar doc node clip userToken] -> 
           (Maybe (Token doc node clip userToken), ScannerState)
mkTokenEx strf tokf (idPCounter, whitespaceMap, collectedWhitespaceFocus) scs = 
  let tokenLayout = TokenLayout collectedWhitespaceFocus
                                (getFocusStartEnd scs)
                                Nothing -- will be added by Scanner.scanPresentation (if it is the last token)
      str = strf $ stringFromScanChars scs
      idp = idPFromScanChars scs
      userToken = tokf str
      (idp', idPCounter') = case idp of NoIDP -> (IDP idPCounter, idPCounter + 1)
                                        _     -> (idp,            idPCounter    )
  in  debug Lay (show idp ++ show str ++ " " ++ show tokenLayout) $
      ( Just $ UserTk userToken str Nothing idp'
      , (idPCounter', Map.insert idp' collectedWhitespaceFocus whitespaceMap, initWhitespaceFocus) 
      )


mkStructuralToken :: ScannerState -> [ScanChar doc node clip userToken] -> (Maybe (Token doc node clip userToken), ScannerState)
mkStructuralToken (idPCounter, whitespaceMap, collectedWhitespaceFocus) 
                  scs@[Structural idp _ _ loc tokens lay] = 
  let tokenLayout = TokenLayout collectedWhitespaceFocus
                                (getFocusStartEnd scs)
                                Nothing -- will be added by Scanner.scanPresentation (if it is the last token)
      (idp', idPCounter') = case idp of NoIDP -> (IDP idPCounter, idPCounter + 1)
                                        _     -> (idp,            idPCounter    )
  in  debug Lay ("Structural " ++ show tokenLayout) $
      ( Just $ StructuralTk loc lay tokens idp'
      , (idPCounter', Map.insert idp' collectedWhitespaceFocus whitespaceMap, initWhitespaceFocus)
      )


collectWhitespace :: ScannerState -> [ScanChar doc node clip userToken] -> (Maybe a, ScannerState)
collectWhitespace (idPCounter, whitespaceMap, ((newlines, spaces), focusStartEnd)) 
                  (sc@(Char _ _ _ c):scs) = -- will always be a Char
  let newWhitespace = case c of                                                
                        '\n' -> (newlines + 1 + length scs, spaces                 )
                        ' '  -> (newlines                 , spaces + 1 + length scs)
  in (Nothing, (idPCounter, whitespaceMap, ( newWhitespace
                                           , updateFocusStartEnd (newlines+spaces) focusStartEnd (sc:scs) )))
       -- we add

-- TODO handle pattern match failures with internal errors


getFocusStartEnd scs = updateFocusStartEnd 0 (Nothing, Nothing) scs

updateFocusStartEnd :: Int -> FocusStartEnd -> [ScanChar doc node clip userToken] -> FocusStartEnd
updateFocusStartEnd i (oldFocusStart, oldFocusEnd) cs =
  (getFocusStart i oldFocusStart cs, getFocusEnd i oldFocusEnd cs) 
  
getFocusStart i oldFocusStart []     = Nothing
getFocusStart i oldFocusStart (c:cs) = if hasFocusStartMark c then Just i else getFocusStart (i+1) oldFocusStart cs

getFocusEnd i oldFocusEnd []     = Nothing
getFocusEnd i oldFocusEnd (c:cs) = if hasFocusEndMark c then Just i else getFocusEnd (i+1) oldFocusEnd cs
