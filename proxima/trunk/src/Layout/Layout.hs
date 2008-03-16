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
               ParsingP NoIDP LexInherited $
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

-- maybe split whitespacefocus
-- detokenize ignores Lexer information, since alle tokens can be treated the same when layouting.
detokenizer wm pres = {- let (l',focusp) = detokenize testWM testPres
                      in  debug Lay ("\n\n\n\ndetokenize test\n"++show (l',focusp)++"\nfocus is on "++case fromP focusp of
                                                                                          NoPathP    -> ""
                                                                                          PathP pth _ -> show(selectTree pth l')
                                                                                          ) $
                      -} detokenize wm pres

detokenize :: (DocNode node, Show token) => WhitespaceMap -> Presentation doc node clip token ->
              (Layout doc node clip, FocusPres)
detokenize wm pres@(ParsingP idp l _)     = detokenizeParsing wm pres
                                                  
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


detokenizeParsing wm (ParsingP idp l pres) =
  let (press, fs) = unzip [ (pres', prependToFocus i f') | (i,(pres',f')) <- zip [0..] $ detokenize' wm False pres ]
      
      f = foldl combineFocus  noFocus fs
  in debug Lay ("focusses:"++show fs) $
     if null press 
     then debug Err ("Layout.detokenize empty token list") 
          ( StringP NoIDP "", noFocus)
     else ( ParsingP idp l $ ColP NoIDP 0 NF press
          , prependToFocus 0 $ f)
          
-- use the paths from the argument in which they are defined. (each should be defined in only one arg)
combineFocus (FocusP (PathP sp si) (PathP ep ei)) _                = (FocusP (PathP sp si) (PathP ep ei))
combineFocus (FocusP (PathP sp si) _            ) (FocusP _   ep2) = (FocusP (PathP sp si) ep2          )
combineFocus (FocusP _             (PathP ep ei)) (FocusP sp2 _)   = (FocusP sp2           (PathP ep ei))
combineFocus (FocusP _             _            ) (FocusP sp2 ep2) = (FocusP sp2 ep2)

noFocus = FocusP NoPathP NoPathP

                                     

-- find out semantics of this one        What about Refs?
-- incomplete, only for strings
detokenize' :: (DocNode node, Show token) => WhitespaceMap -> Bool -> Presentation doc node clip token -> 
               [(Layout doc node clip, FocusPres)]
detokenize' wm t (StructuralP idp pres)      = let (pres', f) = detokenize wm pres
                                            in  [(StructuralP idp pres', prependToFocus 0 f)]
detokenize' wm t (EmptyP idp)                = [(EmptyP idp, noFocus)]
            
detokenize' wm t (StringP idp str)           = [(StringP idp str, noFocus)]
detokenize' wm t (TokenP idp token)          = let res = addWhitespaceToken wm idp token
                                               in  -- debug Lay ("Token:"++show res ) $
                                                   res
detokenize' wm t (ImageP idp str st)         = [(ImageP idp str st, noFocus)]
detokenize' wm t (PolyP idp pts w st)        = [(PolyP idp pts w st, noFocus)]
detokenize' wm t (RectangleP idp w h lw st)  = [(RectangleP idp w h lw st, noFocus)]
detokenize' wm t (EllipseP idp w h lw st)    = [(EllipseP idp w h lw st, noFocus)]

detokenize' wm t (RowP idp rf press)         = detokenizeRow' wm t 0 press -- ref gets lost
--detokenize' wm t (ColP idp rf fm press)      = let (press',f) = detokenizeList' wm t 0 press
--                                               in  ([ColP idp rf fm press'], f)
--detokenize' wm t (OverlayP idp (pres:press)) = let (press',f) = detokenize' wm t pres -- cast is safe, no tokens in press
--                                              in  ([ OverlayP idp (pres' : map castPresToLay press) | pres' <- press' ], f)
detokenize' wm t (WithP ar pres)            = [(WithP ar pres', prependToFocus 0 f)       | (pres',f) <- detokenize' wm t pres ]
detokenize' wm t (ParsingP idp l pres)       =[(ParsingP idp l pres', prependToFocus 0 f) | (pres',f) <- detokenize' wm t pres ]
detokenize' wm t (LocatorP l pres)          = [(LocatorP l pres', prependToFocus 0 f)     | (pres',f) <- detokenize' wm t pres ]
--detokenize' wm t (FormatterP idp press)      = let (press', f) = detokenizeList' wm p t 0 press
--                                              in  ([FormatterP idp press'], f)
-- graph and vertex are not assumed to be in parsing presentations
detokenize' wm t pr                         = debug Err ("\n\n\nLayout.detokenize': can't handle "++ show pr) [(castPresToLay pr, noFocus)]


{-
detokenizeList' wm t i []           = []
detokenizeList' wm t i (pres:press) = let (press',  f1) = detokenize' wm  t pres 
                                          (presss', f2) = detokenizeList' wm t (i+length press') press
                                      in  (press' ++ presss', combineFocus f1 f2)
-}

-- recursive rows cause problems. we cannot add to the path for every row (only the topmost), but if deeper rows
-- cause the creation of more rows in the result, then this should be taken into account at the top-level.
-- in order to do so, we would need some threaded attribute, which will complicate the code even more.

detokenizeRow' :: (DocNode node, Show token) => WhitespaceMap -> Bool -> Int -> [Presentation doc node clip token] -> 
                  [(Layout doc node clip, FocusPres)]
detokenizeRow' wm t i [] = []
detokenizeRow' wm t i (pres:press) =
   combine (detokenize' wm True pres) (detokenizeRow' wm t i press)
  -- the last and first lines are merged, so if press' has more than 0 lines, we decrement i with 1
  

singleton []       = debug Err ("Layout.detokenize': graph child without singleton token (add row to presentation)") $ EmptyP NoIDP
singleton [pres]   = pres
singleton (pres:_) = debug Err ("Layout.detokenize': graph child without singleton token (add row to presentation)") $ pres

combine :: [(Presentation doc node clip token,FocusPres)] -> [(Presentation doc node clip token,FocusPres)] ->
           [(Presentation doc node clip token, FocusPres)]
combine [] l2 = l2 -- in this case f1 will always be noFocus, so we take f2
combine l1 [] = l1 -- in this case f2 will always be noFocus, so we take f1
combine l1 l2 = ( init l1 ++ 
                 [let (lastR, lastF) = last l1
                      (firstR, firstF) = head' "Layout.combine" l2
                  in (RowP NoIDP 0 $ [lastR,firstR], prependToFocus 0 lastF `combineFocus` prependToFocus 1 firstF) 
                 ] 
                 ++ tail l2
                            )

prependToFocus i focus = mapFocusPath (i:) focus


mapFocusPath :: (Path -> Path) -> FocusPres -> FocusPres
mapFocusPath f NoFocusP = NoFocusP
mapFocusPath f (FocusP p1 p2) = FocusP (mapPath f p1) (mapPath f p2)

mapPath f NoPathP = NoPathP
mapPath f (PathP p i) = PathP (f p) i


addWhitespaceToken :: (DocNode node, Show token) => WhitespaceMap -> IDP -> Token doc node clip token -> 
                      [(Layout doc node clip, FocusPres)]
addWhitespaceToken wm idp (UserTk _ _ str _ _)        = addWhitespace wm Nothing idp (StringP idp str)
addWhitespaceToken wm idp (StructuralTk _ _ pres _ _) = let (pres', f) = detokenize wm pres
                                                        in  addWhitespace wm (Just f) idp pres'
                                                      

addWhitespace :: WhitespaceMap -> Maybe FocusPres -> IDP -> Layout doc node clip -> [(Layout doc node clip, FocusPres)]
addWhitespace wm mStrFocus NoIDP pres = [(pres,noFocus)]
addWhitespace wm mStrFocus idp pres = 
  case Map.lookup idp wm  of
    Nothing -> [(pres, noFocus)]
    Just tLayout@(TokenLayout (breaks, spaces) wsFocus  tFocus)  ->
      let rows =  replicate breaks (StringP NoIDP "") 
                  ++ [row [StringP NoIDP (replicate spaces ' '), pres]]
          focuss = mkFocuss tLayout (case mStrFocus of Just strFocus -> strFocus
                                                       Nothing       -> noFocus)
      in zip rows focuss

mkFocuss (TokenLayout (breaks, spaces) (wsStartFocus,wsEndFocus) (tStartFocus,tEndFocus)) strFocus = 
  zipWith FocusP
      (mkStartOrEndFocuss (breaks, spaces) wsStartFocus tStartFocus (fromP strFocus))
      (mkStartOrEndFocuss (breaks, spaces) wsEndFocus   tEndFocus (toP strFocus))

-- because these focuses are either both start or both end focus, we know that only one can be a (Just i)
mkStartOrEndFocuss (breaks, spaces) wFocus tFocus sFocus = -- sFocus is focus inside structural token
  let breaksFocuss = case wFocus of 
                       Just i  -> take breaks $ (replicate i NoPathP) ++ [PathP [] 0] ++ repeat NoPathP
                       Nothing -> take breaks $ repeat NoPathP
      rowFocus     = case wFocus of 
                       Just i -> if i >= breaks -- then focus is in the spaces
                                 then PathP [0] (i-breaks)
                                 else NoPathP
                       Nothing -> case tFocus of 
                                    Just i  -> PathP [1] i   -- [1] for selecting the second elt of the added row
                                    Nothing -> mapPath (1:) sFocus
                                    -- if nothing else has focus, use the structural focus
  in breaksFocuss ++ [rowFocus]
--instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a,b,c,d,e,f) where -- why isn't this a standard instance?
--  show (a,b,c,d,e,f) = "("++show a++","++show b++","++show c++","++show d++","++show e++","++show f++")"

-- row and column mappings get lost in a parsing structure. What are the consequences? And can we do something
-- about it?

-- !!(Look further at this:)last bit of layout in an empty token? for now we put it in an empty string token. 


-- white space in front of images not correct now

-- new anonymous character in front of token, leads to anonymous token. Maybe take idp from the first non anonymous
-- character.

-- structurals should have whitespace
