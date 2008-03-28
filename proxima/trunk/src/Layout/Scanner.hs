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
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Maybe

import Layout.ScannerAG

{-

The scanner converts the Layout level to: TokenP (StructuralTk [..])

locators for tokens are taken from its string. The first character that has a locator determines
the locator for the entire string.

Same thing for idps



Unclear:
What is the problem if idp's are created new?
what happens if token has different string than scanned (eg 0123 -> 123) how does this affect focus?

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

TODO: 
- clear IDPS in pastePres
- add special handling of error token in Layout, so it adds the correct whitespace.
- put scanchars in error token, so idp's and locators are not cleared on a lexical error.
- take structurals out of scanner (otherwise lexical error kills them)
   (make sure that the list of new idps is used in all segments when we split at structurals)
- use tokenIDP from presTypes
- In LayPresent: enable fall back when focus was not scanned (disabled for testing)

- Maybe we can store structural focus too.



Lexical errors create one big token, which discards structural children and ruins the presentation.
  after lexical error, just split remaining input into error tokens and structural tokens
  and represent all whitespace in the error tokens. Layout should make sure that whitespace in 
  error tokens is converted to breaks and spaces.
  Probably also the whitespace in the state before the error occurred should be put in the error token.


  Instead of using Alex for structurals, we could call Alex several times for the string parts of the
  layout. This way, we keep the structurals out of the user specified scanner. The whitespace in the
  state can be put into the structural token.
  

passing several Alex scanners (probably solved by Alex itself)


what to do with columns and formatters containing tokens?


if formatters have linebreak whitespace, layouter will probably go wrong.

whitespace in front of first token?
can be solved by having optional preceding whitespace, but this does not work if there are no
tokens at all. If parser fails, it is okay, the id's are kept by the parseErr, but if empty pres
is accepted then we need an initial token.



problem with structurals. focus in auto type sig is not recognized properly


If we allow autowhitespace for strings and structural presentations
then Layout.hs adds too much whitespace in case of a parseErr presentation
 (is this still the case after all the changes to presParseErr?)
However, enforcing the use of tokens in the presentationAG is tricky.

-- unclear whether this comment still holds.
This is related to the error pres in the tokens. This is now not processed (so it is a layout instead of 
a presentation). Processing it recursively is not a problem for Structural presentations, but
for parsing presentations the lexer can fail on a lexer error, in which case no information is returned
and we cannot tokenize any structural token children.


-}





tokenizeLay :: (DocNode node, Show token) =>
               ScannerSheet doc node clip token -> state -> LayoutLevel doc node clip token ->
               PresentationLevel doc node clip token -> (EditPresentation docLvl doc node clip token , state, LayoutLevel doc node clip token)
tokenizeLay sheet state layLvl@(LayoutLevel lay focus dt) (PresentationLevel _ (_, idPCounter)) = 
 let (tokens, idPCounter', whitespaceMap, tokenizedPres) = scanStructural sheet (fixFocus focus) LexHaskell Nothing [] idPCounter Map.empty lay 
     presLvl' = PresentationLevel (TokenP NoIDP (StructuralTk 0 Nothing tokenizedPres tokens NoIDP)) (whitespaceMap,idPCounter')
 in  (case focus of FocusP (PathP sf si) (PathP ef ei) -> debug Lay ("focus start\n"++ show sf++ show si ++ "focus end\n"++ show ef++ show ei ++"\n")
                    _ -> id
     ) 
--     debug Lay ("Scanned tokens:"++show tokens++"\n"++ show whitespaceMap) $
     (SetPres presLvl', state, layLvl)

-- convert focus with missing paths to focus with the same start and end paths
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
synthesized attribute tokens: the list of tokens         (constructed at every case because of cast from Layout to Presentation)
                      pres: the presentation in which parsing parts are tokenized (put in the presentation field of StructuralTk, which is used in parse errors
                                                                                   if we simply used the presentation, then whitespace/focus for inner parsing
                                                                                   parts could not be reused.)
                      
-}
scanStructural :: (DocNode node, Show token) => ScannerSheet doc node clip token -> ((Path,Int),(Path,Int)) ->
                  Lexer -> Maybe node -> Path -> IDPCounter -> WhitespaceMap -> Layout doc node clip token ->
                  ([Token doc node clip token], IDPCounter, WhitespaceMap, Presentation doc node clip token)
scanStructural sheet foc lx loc pth idpc wm presentation =
  case presentation of
    ParsingP idP p lx' pres'    -> scanPresentation sheet foc lx loc (pth++[0]) idpc wm idP p lx' pres'
    EmptyP idd                  -> ([], idpc, wm, EmptyP idd)
    StringP idd str             -> ([], idpc, wm, StringP idd str)
    ImageP idd istr st          -> ([], idpc, wm, ImageP idd istr st)
    PolyP idd pts w st          -> ([], idpc, wm, PolyP idd pts w st)
    RectangleP idd w h lw st    -> ([], idpc, wm, RectangleP idd w h lw st)
    EllipseP idd w h lw st      -> ([], idpc, wm, EllipseP idd w h lw st)
    RowP id rf press            -> let (tokens, idpc', wm', press') = scanStructuralList sheet foc lx loc pth idpc wm press
                                   in  (tokens, idpc', wm', RowP id rf press')
    ColP id rf f press          -> let (tokens, idpc', wm', press') = scanStructuralList sheet foc lx loc pth idpc wm press
                                   in  (tokens, idpc', wm', ColP id rf f press')
    OverlayP id press           -> let (tokens, idpc', wm', press') = scanStructuralList sheet foc lx loc pth idpc wm press
                                   in  (tokens, idpc', wm', OverlayP id press')
    FormatterP id press         -> let (tokens, idpc', wm', press') = scanStructuralList sheet foc lx loc pth idpc wm press
                                   in  (tokens, idpc', wm', FormatterP id press')
    GraphP id d w h edges press -> let (tokens, idpc', wm', press') = scanStructuralList sheet foc lx loc pth idpc wm press
                                   in  (GraphTk d edges loc id : tokens, idpc', wm', GraphP id d w h edges press')
    VertexP id v x y o pres     -> let (tokens, idpc', wm', pres') = scanStructural sheet foc lx loc (pth++[0]) idpc wm pres
                                   in  (VertexTk v (x,y) loc id : tokens, idpc', wm', VertexP id v x y o pres')
    WithP ar pres               -> let (tokens, idpc', wm', pres') = scanStructural sheet foc lx loc (pth++[0]) idpc wm pres
                                   in  (tokens, idpc', wm', WithP ar pres')
    StructuralP id pres         -> let (tokens, idpc', wm', pres') = scanStructural sheet foc lx loc (pth++[0]) idpc wm pres
                                   in  ([StructuralTk 0 loc pres' tokens id], idpc', wm', StructuralP id pres')
                                                      -- position is not used, so set to 0
    LocatorP newLoc pres        -> let (tokens, idpc', wm', pres') = scanStructural sheet foc lx (Just newLoc) (pth++[0]) idpc wm pres
                                   in  (tokens, idpc', wm', LocatorP newLoc pres')
    pr -> debug Err ("Scanner.scanStructural: can't handle "++ show pr) ([], idpc, wm, castLayToPres pr)


scanStructuralList :: (DocNode node, Show token) => 
                      ScannerSheet doc node clip token -> ((Path,Int),(Path,Int)) -> Lexer ->
                      Maybe node -> Path ->
                      IDPCounter -> WhitespaceMap -> [Layout doc node clip token] ->
                      ([Token doc node clip token], IDPCounter, WhitespaceMap, [Presentation doc node clip token])
scanStructuralList sheet foc lx loc pth idpc wm press = scanStructuralList' sheet foc lx loc pth idpc wm 0 press
 where scanStructuralList' sheet foc lx loc pth idpc wm i []           = ([], idpc, wm, [])
       scanStructuralList' sheet foc lx loc pth idpc wm i (pres:press) = 
         let (tokens,  idpc',  wm', pres')  = scanStructural sheet foc lx loc (pth++[i]) idpc wm pres
             (tokenss, idpc'', wm'', press') = scanStructuralList' sheet foc lx loc pth idpc' wm' (i+1) press
         in  (tokens ++ tokenss, idpc'', wm'', pres':press')



scanPresentation :: (DocNode node, Show token) => ScannerSheet doc node clip token -> ((Path,Int),(Path,Int)) -> 
                    Lexer -> Maybe node -> Path -> IDPCounter -> WhitespaceMap ->
                    IDP -> Maybe (ClipParser doc node clip token) -> Lexer -> Layout doc node clip token ->
                    ([Token doc node clip token], IDPCounter, WhitespaceMap, Presentation doc node clip token)
scanPresentation sheet foc inheritedLex mNode pth idPCounter whitespaceMap idP
                 parser presentationLex lay =
 let --lay = parsing (row [ structural (row [ text "<", parsing (text "blaa"), text ">"]), text "   " ])
     lex = case  presentationLex of
             LexInherited -> inheritedLex
             _            -> presentationLex
     (idPCounter', pos, scanChars, scannedFocusEnd, scannedFocusStart, self, whitespaceMap') = sem_Layout lay foc idPCounter lex mNode pth 0 (scanStructural sheet) Nothing Nothing whitespaceMap
       -- sheet is not used by the AG, so we already pass it to scanStructural, saving an extra attribute


     focusedScanChars = markFocusStartAndEnd scannedFocusStart scannedFocusEnd scanChars 
     -- first, we store the focus in the scanned characters (except if focus is after last char)

     scannedTokens = addLastCharFocusStartAndEnd scanChars scannedFocusStart scannedFocusEnd $
                     sheet focusedScanChars
     -- scan the characters, and in the resulting tokens, store 
                     
     (scannedTokensIDPs, addedIdPs, idPCounter'') = addIdPs (IntSet.empty,idPCounter') scannedTokens
     -- the scanned tokens may have double or missing idp's. addIdPs creates idps for all tokens.
     
     scannedWhitespaceMap = retrieveTokenWhitespace Map.empty scannedTokensIDPs
     -- retrieve whitespace and focus info from each token and store it in the WhitespaceMap
     
     tokens = catMaybes $ map f scannedTokensIDPs
                where f (ScannedToken _ t) = Just t
                      f _                = Nothing
     -- finally, remove all whitespace tokens.
     
 in  debug Lay ("Alex scanner:\n" ++ show (scannedFocusStart,scannedFocusEnd)++ stringFromScanChars scanChars) $
     --debug Lay ("Last whitespaceFocus':" ++ show (afterLastCharFocusStart)) $
     --debug Lay ("whitespaceMap" ++ show scannedWhitespaceMap ) $
     debug Lay (showScannedTokens scannedTokensIDPs) $
     
     ( [ParsingTk parser mNode tokens idP]
     , idPCounter'', scannedWhitespaceMap `Map.union` whitespaceMap'
     , loc (maybe noNode id mNode) $ ParsingP NoIDP parser LexInherited $ RowP NoIDP 0 $ map presFromToken tokens
     )

-- If the focus is after last char, we cannot encode it in the scanChars. In this case, it is added
-- to the scanned tokens by addLastCharFocusStartAndEnd.
markFocusStartAndEnd :: (Maybe Int) -> (Maybe Int) -> [ScanChar doc node clip token] -> [ScanChar doc node clip token]
markFocusStartAndEnd scannedFocusStart scannedFocusEnd scanChars =
  markFocus markFocusStart scannedFocusStart $
  markFocus markFocusEnd   scannedFocusEnd 
  scanChars

-- markFocus is parameterized with a function that marks either the focus start or the end in a ScanChar.
markFocus :: (ScanChar doc node clip token -> ScanChar doc node clip token) -> (Maybe Int) ->
             [ScanChar doc node clip token] -> [ScanChar doc node clip token]
markFocus setFocusStartOrEnd Nothing          scs = scs
markFocus setFocusStartOrEnd focus@(Just pos) scs = 
  if not $ focusAfterLastChar scs focus 
  then case splitAt pos scs of
         (left, focusedChar:right) -> left ++ setFocusStartOrEnd focusedChar : right
         _                         -> debug Err "PresentationParsing.markFocus: incorrect position" scs
  else scs

focusAfterLastChar scs Nothing    = False
focusAfterLastChar scs (Just pos) = pos == length scs

-- If ScannedFocusStart or ScannedFocusEnd is after the last character, it was not recorded in the
-- scanChars. This function adds it directly to the scanned tokens.
addLastCharFocusStartAndEnd :: [ScanChar doc node clip token] -> Maybe Int ->  Maybe Int ->
                               [ ScannedToken doc node clip token ]  -> [ ScannedToken doc node clip token ] 
addLastCharFocusStartAndEnd scanChars scannedFocusStart scannedFocusEnd tokens =
  addLastCharFocus updateFocusStart scanChars scannedFocusStart $
  addLastCharFocus updateFocusEnd scanChars scannedFocusEnd $
  tokens
 where updateFocusStart :: Int -> FocusStartEnd -> FocusStartEnd
       updateFocusStart p (_, focusEnd) = (Just p, focusEnd)

       updateFocusEnd :: Int -> FocusStartEnd -> FocusStartEnd
       updateFocusEnd p (focusStart, _) = (focusStart, Just p)
 
-- addLastCharFocus is parameterized with a function updateStartOrEnd, which takes a focus position and
-- updates either the start or the end field in FocusStartEnd.
addLastCharFocus :: (Int -> FocusStartEnd -> FocusStartEnd) -> [ScanChar doc node clip token] -> Maybe Int ->
                    [ ScannedToken doc node clip token ] -> [ ScannedToken doc node clip token ] 
addLastCharFocus updateStartOrEnd scanChars scannedFocus tokens =
  if focusAfterLastChar scanChars scannedFocus
  then case tokens of
         [] -> []
         (_:_) -> init tokens ++ [updateFocus (last tokens)]
  else tokens
  where updateFocus (ScannedWhitespace focusStartEnd ws@(bs,sps)) = 
                     ScannedWhitespace (updateStartOrEnd (bs+sps) focusStartEnd) ws
        updateFocus (ScannedToken focusStartEnd token) =
          case token of
            UserTk tokenPos userToken str loc idp -> 
                     ScannedToken (updateStartOrEnd (length str) focusStartEnd) token
            StructuralTk _ _ _ _ _ -> 
                     ScannedToken (updateStartOrEnd 1 focusStartEnd) token
            ErrorTk _ str _ -> 
                     ScannedToken (updateStartOrEnd (length str) focusStartEnd) token
            


{-
addIdPs sets the IdP field for tokens that have NoIdP, and removes duplicates.

Scanned tokens may have the same IdP if they arise from splitting a token. To guarantee
uniqueness, we keep a list of idPs that were encountered in the new tokens, and create
new ones if not unique. Newly generated idP's themselves will always be unique, so they
are not kept in the list. Furthermore, since paste operations clear all idP's we only
need to take into account the idP's of one parsing presentation (rather than the global
set of idP's)
-}
addIdPs :: (Show node, Show token) => (IntSet, Int) -> [ScannedToken doc node clip token] ->
           ([ScannedToken doc node clip token], IntSet, Int)
addIdPs (addedIdPs,idPCounter) [] = ([], addedIdPs, idPCounter)
addIdPs (addedIdPs,idPCounter) (st: sts) = 
  let (st', addedIdPs', idPCounter') = case st of
                             (ScannedWhitespace _ _) -> (st, addedIdPs, idPCounter)
                             (ScannedToken f tk) -> 
                                case getTokenIDP tk of
                                  NoIDP -> ( ScannedToken f (setTokenIDP (IDP idPCounter) tk)
                                           , addedIdPs -- new idP is unique, no need to add
                                           , idPCounter+1
                                           )
                                  IDP idP -> 
                                    if idP `IntSet.member` addedIdPs
                                    then ( ScannedToken f (setTokenIDP (IDP idPCounter) tk)
                                         , addedIdPs -- new idP is unique, no need to add
                                         , idPCounter+1
                                         )
                                    else ( ScannedToken f tk
                                         , IntSet.insert idP addedIdPs -- first time encountering idP, so add
                                         , idPCounter
                                         )
      (sts', addedIdPs'', idPCounter'') = addIdPs (addedIdPs',idPCounter') sts 
  in  (st':sts', addedIdPs'', idPCounter'') 



-- For each scannedToken, store its focus and its trailing whitespace and whitespacefocus in the 
-- WhitespaceMap.
retrieveTokenWhitespace whitespaceMap scannedTokens = 
  case scannedTokens of 
    [] -> 
      whitespaceMap
    (ScannedToken f t: ScannedWhitespace wf ws : sts) ->  -- token with trailing whitespace
      let tokenLayout = TokenLayout ws wf f
          whitespaceMap' = Map.insert (getTokenIDP t) tokenLayout whitespaceMap
      in  retrieveTokenWhitespace whitespaceMap' sts
    (ScannedToken f t: sts) ->                            -- token without trailing whitespace
      let tokenLayout = TokenLayout (0,0) (Nothing,Nothing) f
          whitespaceMap' = Map.insert (getTokenIDP t) tokenLayout whitespaceMap
      in  retrieveTokenWhitespace whitespaceMap' sts
    (ScannedWhitespace _ _ : sts) ->            
      retrieveTokenWhitespace whitespaceMap sts -- leading whitespace of token list (or whitespace 
                                                -- after whitespace, but the scanner does not produce this)
    -- currently, we ignore this one.
