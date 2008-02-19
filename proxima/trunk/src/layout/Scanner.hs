-----------------------------------------------------------------------------------------
{-| Module      : Scanner
    Copyright   : (c) 2007 Martijn Schrage
    License     : All Rights Reserved

    Maintainer  : martijn@cs.uu.nl
    Stability   : experimental
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Scanner ( module Scanner
               , module Maybe
               , module LayLayerTypes
               , module LayLayerUtils
               , module Map
               ) where
-- we export a lot of stuff here, so the Alex sheet only has to import Scanner.hs
-- unfortunately Haskell does not allow us to export the qualified Map
import CommonTypes
import LayLayerTypes
import LayLayerUtils hiding (empty)
import Data.Map as Map hiding (mapMaybe, (!))
import Maybe

import ScannerAG

{-
Challenges/todo:

passing several Alex scanners (probably solved by Alex itself)

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
     -- debug Lay ("Scanned tokens:"++show tokens++"\n"++ show whitespaceMap) $
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
     focusedScanChars = markFocus markFocusStart scannedFocusStart $
                        markFocus markFocusEnd scannedFocusEnd 
                          (scanChars) -- ++ [EndOfParsing NoFocusMark NoFocusMark]) 
     (tokens, idPCounter'', whitespaceMap'') = sheet idPCounter' focusedScanChars
 in  -- debug Lay ("Alex scanner:\n" ++ stringFromScanChars scanChars ++ "\n" ++ (show tokens)) $
     ( [ParsingTk (castLayToPres lay) tokens idP]
     , idPCounter'', whitespaceMap' `Map.union` whitespaceMap'')


markFocus f Nothing    scs = scs
markFocus f (Just pos) scs = let (left, focusedChar:right) = splitAt pos scs
                             in  left ++ f focusedChar : right




-- The functions below are used by the Alex Scanner, which imports Scanner.hs
-- Some scanner functionality could not be factorized and can be found in AlexTemplate-ghc

alexGetChar (_, [])   = Nothing
alexGetChar (_, Char _ _ _ c : cs) = Just (c, (c,cs))
alexGetChar (_, Structural _ _ _ _ _ _ : cs) = Just ('\255', ('\255', cs))
alexGetChar (_, EndOfParsing _ _ : cs)       = Just ('\254', ('\254', cs))

alexInputPrevChar (c,_) = c

type ScannerState = (Int, WhitespaceMap, (Int, Int)) -- (idP counter, whitespace map, (newlines, spaces))

mkToken = mkTokenEx id

-- the first strf is for manipulating the string that is stored in the token
mkTokenEx :: (String->String) -> (String -> userToken) -> ScannerState -> [ScanChar doc node clip userToken] -> 
           (Maybe (Token doc node clip userToken), ScannerState)
mkTokenEx strf tokf (idPCounter, whitespaceMap, collectedWhitespace) scs = 
  let str = debug Lay ((case hasStart 0 scs of 
                        Just i  -> "Focus start on "++ show (stringFromScanChars scs)++" at "++show i
                        Nothing -> "") ++
                       (case hasEnd 0 scs of 
                        Just i  -> "Focus end on "++ show (stringFromScanChars scs)++" at "++show i
                        Nothing -> "") 
                       ) $
            strf $ stringFromScanChars scs
      idp = idPFromScanChars scs
      userToken = tokf str
      (idp', idPCounter') = case idp of NoIDP -> (IDP idPCounter, idPCounter + 1)
                                        _     -> (idp,            idPCounter    )
  in  ( Just $ UserTk userToken str Nothing idp'
      , (idPCounter', Map.insert idp' collectedWhitespace whitespaceMap, (0,0)) 
      )
      
hasStart i [] = Nothing
hasStart i (c:cs) = if hasFocusStartMark c then Just i else hasStart (i+1) cs

hasEnd i [] = Nothing
hasEnd i (c:cs) = if hasFocusEndMark c then Just i else hasEnd (i+1) cs

mkStructuralToken :: ScannerState -> [ScanChar doc node clip userToken] -> (Maybe (Token doc node clip userToken), ScannerState)
mkStructuralToken (idPCounter, whitespaceMap, collectedWhitespace) scs = 
  let Structural idp _ _ loc tokens lay = head scs
      (idp', idPCounter') = case idp of NoIDP -> (IDP idPCounter, idPCounter + 1)
                                        _     -> (idp,            idPCounter    )
  in  ( Just $ StructuralTk loc lay tokens idp'
      , (idPCounter', Map.insert idp' collectedWhitespace whitespaceMap, (0,0))
      )


collectWhitespace :: ScannerState -> [ScanChar doc node clip userToken] -> (Maybe a, ScannerState)
collectWhitespace (idPCounter, whitespaceMap, (newlines, spaces)) (c:cs) =
  let newWhitespace = case c of
                        Char _ _ _ '\n' -> (newlines + 1 + length cs, spaces                )
                        Char _ _ _ ' '  -> (newlines                , spaces + 1 + length cs)
                        -- will always be a Char
  in (Nothing, (idPCounter, whitespaceMap, newWhitespace))


-- TODO handle pattern match failures with internal errors
