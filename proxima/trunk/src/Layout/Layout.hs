module Layout.Layout where

import Common.CommonTypes
import Common.CommonUtils
import Layout.LayLayerTypes
import Layout.LayLayerUtils

import Layout.TreeEditPres

import qualified Data.Map as Map
import Data.Map (Map)


-- add layout from local state to all descendents of ParsingP node, until StructureP node is encountered

-- mutually recursive functions: detokenize for walking the tree until ParsingP node
--                               detokenize' for adding whitespace until StructureP node

-- Local State is layout

testPres :: Presentation doc node clip ()
testPres =     
               ParsingP NoIDP Nothing LexInherited $
           RowP NoIDP 0 [  
                         RowP NoIDP 0 [ 
                            TokenP (IDP 1) (UserTk 0 () "een" Nothing NoIDP)
                          , TokenP (IDP 2) (UserTk 1 () "twee" Nothing NoIDP)
                          , TokenP (IDP 3) (UserTk 2 () "drie" Nothing NoIDP)
                        ]
                        ]
                        
testWM = Map.fromList [(IDP 1, emptyTokenLayout { whitespace = (1,0)} )
                      ,(IDP 2, emptyTokenLayout { whitespace = (0,1)} )
                      ,(IDP 3, emptyTokenLayout { whitespace = (0,1), tokenFocus = (Just 0, Just 0)} )
                      ]
emptyTokenLayout = TokenLayout { whitespace = (0,0)
                               , whitespaceFocus = (Nothing,Nothing)
                               , tokenFocus = (Nothing,Nothing)
                               }

-- detokenize ignores Lexer information, since all tokens can be treated the same when layouting.
detokenizer wm pres = {- let (l',focusp) = detokenize testWM testPres
                      in  debug Lay ("\n\n\n\ndetokenize test\n"++show (l',focusp)++"\nfocus is on "++case fromP focusp of
                                                                                          NoPathP    -> ""
                                                                                          PathP pth _ -> show(selectTree pth l')
                                                                                          ) $
                      -} detokenize wm pres

detokenize :: (DocNode node, Show token) => WhitespaceMap -> Presentation doc node clip token ->
              (Layout doc node clip token, FocusPres)
detokenize wm pres@(ParsingP idp pr l _)  = detokenizeParsing wm pres
                                                  
detokenize wm (EmptyP idp)                = (EmptyP idp, noFocus) 
detokenize wm (StringP idp str)           = (StringP idp str,          noFocus)
detokenize wm (ImageP idp str st)         = (ImageP idp str st,        noFocus)
detokenize wm (PolyP idp pts w st)        = (PolyP idp pts w st,       noFocus)
detokenize wm (RectangleP idp w h lw st)  = (RectangleP idp w h lw st, noFocus)
detokenize wm (EllipseP idp w h lw st)    = (EllipseP idp w h lw st,   noFocus)
detokenize wm (RowP idp rf press)         = let (press', f) = detokenizeList wm 0 press
                                            in  (RowP idp rf press', f)
detokenize wm (ColP idp rf fm press)      = let (press', f) = detokenizeList wm 0 press
                                           in   (ColP idp rf fm press', f)
detokenize wm (OverlayP idp (pres:press)) = let (pres', f) = detokenize wm pres
                                            in  ( OverlayP idp (pres' : (map castPresToLay press))
                                                , prependToFocus 0 f) -- cast is safe, no tokens in press
detokenize wm (WithP ar pres)            = let (pres', f) = detokenize wm pres
                                           in  (WithP ar pres', prependToFocus 0 f) 
detokenize wm (StructuralP idp pres)      = let (pres', f) = detokenize wm pres
                                           in  (StructuralP idp pres', prependToFocus 0 f)
detokenize wm (LocatorP l pres)          = let (pres', f) = detokenize wm pres
                                           in  (LocatorP l pres', prependToFocus 0 f)
detokenize wm (GraphP idp d w h es press) = let (press', f) = detokenizeList wm 0 press
                                           in  (GraphP idp d w h es press', f)
detokenize wm (VertexP idp v x y o pres)  = let (pres', f) = detokenize wm pres
                                           in  (VertexP idp v x y o pres', prependToFocus 0 f)
detokenize wm (FormatterP idp press)      = let (press', f) = detokenizeList wm 0 press
                                           in  (FormatterP idp press', f)
detokenize wm pr                         = debug Err ("Layout.detokenize: can't handle "++ show pr) $ (castPresToLay pr, noFocus)

detokenizeList wm i []           = ([], noFocus)
detokenizeList wm i (pres:press) = let (pres',  f1) = detokenize wm pres 
                                       (press', f2) = detokenizeList wm (i+1) press
                                   in  (pres' : press', combineFocus (prependToFocus i f1) f2)


detokenizeParsing wm (ParsingP idp pr l pres) =
  let rows = detokenize' wm False pres
      presss = map (map fst) $ rows
      focusss = [ prependToFocus y $ prependToFocus x $ focus
                | (y,row) <- zip [0..] rows, (x,(_,focus)) <- zip [0..] row
                ]
      f = foldl combineFocus  noFocus focusss
  in -- debug Lay ("\n\n\ndetokenizeParsingP: "++show pres++" yields "++show presss) $
     ( ParsingP idp pr l $ ColP NoIDP 0 NF $ if null presss 
                                             then [RowP NoIDP 0 [StringP NoIDP ""]]
                                             else (map (RowP NoIDP 0)) presss
     , prependToFocus 0 $ f
     )
          

                                     
-- for overlay, we descend into the first element, and make an overlay with the first element of the first
-- row of the result of the detokenization (which may be recursively detokenized).

-- for col, we do the same as for row. This is because a col may be present in a structuralTk coming
-- from a presented parse error
-- TODO: is this ok?
detokenize' :: (DocNode node, Show token) => WhitespaceMap -> Bool -> Presentation doc node clip token -> 
               [[(Layout doc node clip token, FocusPres)]]
detokenize' wm t (StructuralP idp pres)      = let (pres', f) = detokenize wm pres
                                            in  [[(StructuralP idp pres', prependToFocus 0 f)]]
detokenize' wm t (EmptyP idp)                = [[(EmptyP idp, noFocus)]]
            
detokenize' wm t (StringP idp str)           = [[(StringP idp str, noFocus)]]
detokenize' wm t (TokenP idp token)          = let res = addWhitespaceToken wm idp token
                                               in  -- debug Lay ("Token:"++show res ) $
                                                   res
detokenize' wm t (ImageP idp str st)         = [[(ImageP idp str st, noFocus)]]
detokenize' wm t (PolyP idp pts w st)        = [[(PolyP idp pts w st, noFocus)]]
detokenize' wm t (RectangleP idp w h lw st)  = [[(RectangleP idp w h lw st, noFocus)]]
detokenize' wm t (EllipseP idp w h lw st)    = [[(EllipseP idp w h lw st, noFocus)]]

detokenize' wm t (RowP idp rf press)         = detokenizeRow' wm t press
--detokenize' wm t (ColP idp rf fm press)      = detokenizeRow' wm t press
detokenize' wm t (OverlayP idp (pres:press)) = let (((pres',f):row):rows) = detokenize' wm t pres -- cast is safe, no tokens in press
                                               in  (( OverlayP idp $ pres' : map castPresToLay press
                                                    , prependToFocus 0 f
                                                    ):row)
                                                   : rows
detokenize' wm t (WithP ar pres)            = map (map (\(pres',f) -> (WithP ar pres', prependToFocus 0 f))) (detokenize' wm t pres)
detokenize' wm t (ParsingP idp pr l pres)   = map (map (\(pres',f) -> (ParsingP idp pr l pres', prependToFocus 0 f))) (detokenize' wm t pres)
detokenize' wm t (LocatorP l pres)          = map (map (\(pres',f) -> (LocatorP l pres', prependToFocus 0 f))) (detokenize' wm t pres)
--detokenize' wm t (FormatterP idp press)      = let (press', f) = detokenizeList' wm p t 0 press
--                                              in  ([FormatterP idp press'], f)
-- graph and vertex are not assumed to be in parsing presentations
detokenize' wm t pr                         = debug Err ("\n\n\nLayout.detokenize': can't handle "++ show pr) [[(castPresToLay pr, noFocus)]]


detokenizeRow' :: (DocNode node, Show token) => WhitespaceMap -> Bool -> [Presentation doc node clip token] -> 
                  [[(Layout doc node clip token, FocusPres)]]
detokenizeRow' wm t [] = []
detokenizeRow' wm t (pres:press) =
   combine (detokenize' wm True pres) (detokenizeRow' wm t press)
  


combine :: [[(Layout doc node clip token,FocusPres)]] -> [[(Layout doc node clip token,FocusPres)]] ->
           [[(Layout doc node clip token, FocusPres)]]
combine [] l2 = l2 -- in this case f1 will always be noFocus, so we take f2
combine l1 [] = l1 -- in this case f2 will always be noFocus, so we take f1
combine l1 l2 = ( init l1 ++ 
                 [let lastR = last l1
                      firstR = head' "Layout.combine" l2
                  in  lastR ++ firstR -- TODO: add nr of elts lastR to firstR
                 ] 
                 ++ tail l2
                            )
-- use the paths from the argument in which they are defined. (each should be defined in only one arg)
combineFocus (FocusP (PathP sp si) (PathP ep ei)) _                = (FocusP (PathP sp si) (PathP ep ei))
combineFocus (FocusP (PathP sp si) _            ) (FocusP _   ep2) = (FocusP (PathP sp si) ep2          )
combineFocus (FocusP _             (PathP ep ei)) (FocusP sp2 _)   = (FocusP sp2           (PathP ep ei))
combineFocus (FocusP _             _            ) (FocusP sp2 ep2) = (FocusP sp2 ep2)

noFocus = FocusP NoPathP NoPathP


prependToFocus i focus = mapFocusPath (i:) focus


mapFocusPath :: (Path -> Path) -> FocusPres -> FocusPres
mapFocusPath f NoFocusP = NoFocusP
mapFocusPath f (FocusP p1 p2) = FocusP (mapPath f p1) (mapPath f p2)

mapPath f NoPathP = NoPathP
mapPath f (PathP p i) = PathP (f p) i


addWhitespaceToken :: (DocNode node, Show token) => WhitespaceMap -> IDP -> Token doc node clip token -> 
                      [[(Layout doc node clip token, FocusPres)]]
addWhitespaceToken wm idp (UserTk _ _ str _ _)        = --debug Lay ("Adding whitespace to UserTk "++show idp++":"++show str) $
                                                        addWhitespace False wm Nothing idp (StringP idp str)
addWhitespaceToken wm idp (StructuralTk _ _ pres _ _) = --debug Lay ("Adding whitespace to StructuralTk "++show idp) $
                                                        let (pres', f) = detokenize wm pres
                                                        in  addWhitespace True wm (Just f) idp pres'
addWhitespaceToken wm idp (ErrorTk _ str _)           = --debug Lay ("Adding whitespace to ErrorTk "++show idp) $
                                                          addWhitespaceErrorToken wm idp str

-- if pres is a structural, we add a "" before and after it, to handle focus. (after is only necessary
-- if it is the last token and there is no whitespace behind it)                   
addWhitespace :: Show node => Bool -> WhitespaceMap -> Maybe FocusPres -> IDP -> Layout doc node clip token -> [[(Layout doc node clip token, FocusPres)]]
addWhitespace isStructural wm mStrFocus NoIDP pres = [surroundWithEmpties isStructural noFocus noFocus $ (pres,noFocus)]
addWhitespace isStructural wm mStrFocus idp pres = 
  case Map.lookup idp wm  of
    Nothing -> [surroundWithEmpties isStructural noFocus noFocus $ (pres,noFocus)]
    Just tLayout@(TokenLayout (breaks, spaces) wsFocus  tFocus)  ->
      let surroundedPres = surroundWithEmpties isStructural beforeTokenFocus afterTokenFocus (pres,tokenFocus) 
          rows =  if breaks ==  0 
                  then [ surroundedPres ++ [(StringP NoIDP (replicate spaces ' '), spacesFocus)]]
                  else [ surroundedPres ++ [(StringP NoIDP "", firstBreakFocus)]]
                    ++ map (\x -> [x]) (zip (replicate (breaks-1) (StringP NoIDP "")) breaksFocuss)
                    ++ [[(StringP NoIDP (replicate spaces ' '), spacesFocus)]]
                  
          -- take care that before and afterTokenFocus are not used if pres is not structural
          (beforeTokenFocus, tokenFocus, afterTokenFocus,firstBreakFocus, breaksFocuss, spacesFocus) =
            mkFocuss isStructural tLayout (case mStrFocus of Just strFocus -> strFocus
                                                             Nothing       -> noFocus)
      in -- debug Lay ("Whitespace for "++shallowShowPres pres++"\n"++show (map (map (\(p,f) -> "("++shallowShowPres p++","++show f++")")) rows)) $
           rows 
           
surroundWithEmpties False _ _ presAndFocus = [presAndFocus]
surroundWithEmpties True beforeFocus afterFocus presAndFocus = 
  [(StringP NoIDP "", beforeFocus), presAndFocus, (StringP NoIDP "", afterFocus)]
           
           
{-
[[pres]]

breaks == 0

[[pres, spaces]]

breaks > 0:

[[pres, ""] 
. 
. breaks-1 times [""] 
. 
,[spaces]
]


-}
mkFocuss isStructural (TokenLayout (breaks, spaces) (wsStartFocus,wsEndFocus) (tStartFocus,tEndFocus)) strFocus = 
  let (btsf, tsf, atsf, fbsf, bsfs, ssf) = mkStartOrEndFocuss isStructural (breaks, spaces) wsStartFocus tStartFocus (fromP strFocus)
      (btef, tef, atef, fbef, befs, sef) = mkStartOrEndFocuss isStructural (breaks, spaces) wsEndFocus   tEndFocus   (toP strFocus)
  in (FocusP btsf btef, FocusP tsf tef, FocusP atsf atef, FocusP fbsf fbef, zipWith FocusP bsfs befs, FocusP ssf sef)
-- because these focuses are either both start or both end focus, we know that only one can be a (Just i)
mkStartOrEndFocuss isStructural (breaks, spaces) wFocus tFocus sFocus = -- sFocus is focus inside the structural token
       let beforeTokenFocus = case tFocus of   -- only used for structural, so isStructural check is not necessary
                                Just 0 -> PathP [] 0
                                _      -> NoPathP
           tokenFocus = case tFocus of  
                          Just i  -> if isStructural -- structural leaves focus to before/afterTokenFocus
                                     then NoPathP
                                     else PathP [] i
                          Nothing -> sFocus 
           afterTokenFocus = case tFocus of -- check not necessary, see above
                               Just 1 -> PathP [] 0
                               _      -> NoPathP
           firstBreakFocus = case wFocus of
                               Just i -> if i == 0 then PathP [] 0 else NoPathP
                               Nothing -> NoPathP
           breaksFocuss = case wFocus of 
                                 Just i  -> if i > 0 
                                            then take (breaks-1) $ (replicate (i-1) NoPathP) ++ [PathP [] 0] ++ repeat NoPathP
                                            else take (breaks-1) $ repeat NoPathP
                                 Nothing -> take (breaks-1) $ repeat NoPathP
           spacesFocus     = case wFocus of 
                              Just i -> if i >= breaks -- then focus is in the spaces
                                      then PathP [] (i-breaks)
                                      else NoPathP
                              Nothing -> NoPathP
       in (beforeTokenFocus, tokenFocus, afterTokenFocus, firstBreakFocus, breaksFocuss, spacesFocus)
  


-- an error token is treated specially, since its whitespace is stored in the string (the scanner stops
-- producing whitespace tokens after a lexical error). Hence there will not be any trailing whitespace, but
-- we do need the whitespacemap for the focus. 
addWhitespaceErrorToken :: Show node => WhitespaceMap -> IDP -> String ->
                           [[(Layout doc node clip token, FocusPres)]]
addWhitespaceErrorToken wm idp str = 
  let lines =  splitAtNewlines str
      focuss = case Map.lookup idp wm  of
                     Nothing -> repeat noFocus
                     Just tLayout@(TokenLayout _ _ (focusStart, focusEnd))  ->
                        zipWith FocusP (mkLineFocus focusStart lines) (mkLineFocus focusEnd lines)
  in  [ [(StringP NoIDP ln,f)] | (ln,f) <- zip lines focuss ]

 
-- this is almost Prelude.lines, but it doesn't remove the last '\n'. (And returns [""] for "", but it
-- will never be called on "".
splitAtNewlines []        = [[]]
splitAtNewlines ('\n':cs) = [] : splitAtNewlines cs
splitAtNewlines (c:cs)    = let (line:lines) = splitAtNewlines cs -- safe: result is always at least a singleton
                            in  (c:line):lines

mkLineFocus Nothing lines  = repeat NoPathP
mkLineFocus (Just f) lines = mkLineFocus' f lines

mkLineFocus' f lines = 
 case lines of (line:lines) -> if f <= length line
                               then PathP [] f : repeat NoPathP 
                               else NoPathP : mkLineFocus' (f-length line-1) lines
               []           -> if f < 0 then []
                               else debug Err "Layout.mkLineFocus: focus index too large" []
