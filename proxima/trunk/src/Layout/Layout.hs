module Layout.Layout where

import Common.CommonTypes
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
testPres = ParsingP NoIDP LexInherited $
           RowP NoIDP 0 [  
                         RowP NoIDP 0 [ 
                            TokenP (IDP 1) (UserTk () "een" Nothing NoIDP)
                          , TokenP (IDP 2) (UserTk () "twee" Nothing NoIDP)
                          , TokenP (IDP 3) (UserTk () "drie" Nothing NoIDP)
                        ]
                        ]
testWM = Map.fromList [(IDP 1, emptyTokenLayout { whitespaceFocus = ((1,0),(Nothing,Nothing))} )
                      ,(IDP 2, emptyTokenLayout { whitespaceFocus = ((0,1),(Nothing,Nothing))} )
                      ,(IDP 3, emptyTokenLayout { whitespaceFocus = ((0,1),(Nothing,Nothing)), tokenFocus = (Just 0, Just 0)} )
                      ]
emptyTokenLayout = TokenLayout { whitespaceFocus = ((0,0),(Nothing,Nothing))
                               , tokenFocus = (Nothing,Nothing)
                               }

-- maybe split whitespacefocus
-- detokenize ignores Lexer information, since alle tokens can be treated the same when layouting.
detokenizer wm pres = let (l',focusp) = detokenize testWM [] testPres
                      in  debug Lay ("\n\n\n\ndetokenize test\n"++show (l',focusp)++"\nfocus is on "++case fromP focusp of
                                                                                          NoPathP    -> ""
                                                                                          PathP pth _ -> show(selectTree pth l')
                                                                                          ) $
                          detokenize wm [] pres

detokenize :: Show token => WhitespaceMap -> Path -> Presentation doc node clip token ->
              (Layout doc node clip, FocusPres)
detokenize wm p (ParsingP idp l pres)       = let (press, f) = detokenize' wm (p++[0]) False pres
                                               in if null press 
                                                  then debug Err ("Layout.detokenize empty token list") (StringP NoIDP "", noFocus)
                                                  else (ParsingP idp l $ ColP NoIDP 0 NF press, f)
                                                  
detokenize wm p (EmptyP idp)                = (EmptyP idp, noFocus) 
detokenize wm p (StringP idp str)           = (StringP idp str,          noFocus)
detokenize wm p (ImageP idp str st)         = (ImageP idp str st,        noFocus)
detokenize wm p (PolyP idp pts w st)        = (PolyP idp pts w st,       noFocus)
detokenize wm p (RectangleP idp w h lw st)  = (RectangleP idp w h lw st, noFocus)
detokenize wm p (EllipseP idp w h lw st)    = (EllipseP idp w h lw st,   noFocus)
detokenize wm p (RowP idp rf press)         = let (press', f) = detokenizeList wm p 0 press
                                             in  (RowP idp rf press', f)
detokenize wm p (ColP idp rf fm press)      = let (press', f) = detokenizeList wm p 0 press
                                             in  debug Lay ("At col"++show p) (ColP idp rf fm press', f)
detokenize wm p (OverlayP idp (pres:press)) = let (pres', f) = detokenize wm (p++[0]) pres
                                             in  (OverlayP idp (pres' : (map castPresToLay press)), f) -- cast is safe, no tokens in press
detokenize wm p (WithP ar pres)            = let (pres', f) = detokenize wm (p++[0]) pres
                                             in  (WithP ar pres', f) 
detokenize wm p (StructuralP idp pres)      = let (pres', f) = detokenize wm (p++[0]) pres
                                             in  (StructuralP idp pres', f)
detokenize wm p (LocatorP l pres)          = let (pres', f) = detokenize wm (p++[0]) pres
                                             in  (LocatorP l pres', f)
detokenize wm p (GraphP idp d w h es press) = let (press', f) = detokenizeList wm p 0 press
                                             in  (GraphP idp d w h es press', f)
detokenize wm p (VertexP idp v x y o pres)  = let (pres', f) = detokenize wm (p++[0]) pres
                                             in  (VertexP idp v x y o pres', f)
detokenize wm p (FormatterP idp press)      = let (press', f) = detokenizeList wm p 0 press
                                             in  (FormatterP idp press', f)
detokenize wm p pr                         = debug Err ("Layout.detokenize: can't handle "++ show pr) $ (castPresToLay pr, noFocus)

detokenizeList wm p i []           = ([], noFocus)
detokenizeList wm p i (pres:press) = let (pres',  f1) = detokenize wm (p++[i]) pres 
                                         (press', f2) = detokenizeList wm p (i+1) press
                                     in  (pres' : press', combineFocus f1 f2)


-- use the paths from the argument in which they are defined. (each should be defined in only one arg)
combineFocus (FocusP (PathP sp si) (PathP ep ei)) _                = (FocusP (PathP sp si) (PathP ep ei))
combineFocus (FocusP (PathP sp si) _            ) (FocusP _   ep2) = (FocusP (PathP sp si) ep2          )
combineFocus (FocusP _             (PathP ep ei)) (FocusP sp2 _)   = (FocusP sp2           (PathP ep ei))
combineFocus (FocusP _             _            ) (FocusP sp2 ep2) = (FocusP sp2 ep2)

noFocus = FocusP NoPathP NoPathP

prependToFocus pth (FocusP sp ep) pre = FocusP (prependToPath sp) (prependToPath ep)
 where prependToPath NoPathP     = NoPathP
       prependToPath (PathP p i) = let (left, right) = splitAt (length pth) p 
                                   in  PathP (left ++ pre ++right) i
                                     

-- find out semantics of this one        What about Refs?
-- incomplete, only for strings
detokenize' :: Show token => WhitespaceMap -> Path -> Bool -> Presentation doc node clip token -> 
               ([Layout doc node clip], FocusPres)
detokenize' wm p t (StructuralP idp pres)      = let (pres', f) = detokenize wm (p++[0]) pres
                                              in  ([StructuralP idp pres'], f)
detokenize' wm p t (EmptyP idp)                = ([EmptyP idp], noFocus)

             
detokenize' wm p t (StringP idp str)           = ([StringP idp str], noFocus)
detokenize' wm p t (TokenP idp token)          = addWhitespaceToken wm p idp token
detokenize' wm p t (ImageP idp str st)         = ([ImageP idp str st], noFocus)
detokenize' wm p t (PolyP idp pts w st)        = ([PolyP idp pts w st], noFocus)
detokenize' wm p t (RectangleP idp w h lw st)  = ([RectangleP idp w h lw st], noFocus)
detokenize' wm p t (EllipseP idp w h lw st)    = ([EllipseP idp w h lw st], noFocus)

detokenize' wm p t (RowP idp rf press)         = detokenizeRow' wm p t 0 press -- ref gets lost
--detokenize' wm p t (ColP idp rf fm press)      = let (press',f) = detokenizeList' wm p t 0 press
--                                               in  ([ColP idp rf fm press'], f)
--detokenize' wm p t (OverlayP idp (pres:press)) = let (press',f) = detokenize' wm (p++[0]) t pres -- cast is safe, no tokens in press
--                                              in  ([ OverlayP idp (pres' : map castPresToLay press) | pres' <- press' ], f)
detokenize' wm p t (WithP ar pres)            = let (press',f) = detokenize' wm (p++[0]) t pres 
                                              in  (map (WithP ar) press', f)
detokenize' wm p t (ParsingP idp l pres)       = let (press', f) = detokenize' wm (p++[0]) t pres 
                                              in  (map (ParsingP idp l) press', f)
detokenize' wm p t (LocatorP l pres)          = let (press', f) = detokenize' wm (p++[0]) t pres 
                                              in  (map (LocatorP l) press', f)
--detokenize' wm p t (FormatterP idp press)      = let (press', f) = detokenizeList' wm p t 0 press
--                                              in  ([FormatterP idp press'], f)
-- graph and vertex are not assumed to be in parsing presentations
detokenize' wm p t pr                         = debug Err ("Layout.detokenize': can't handle "++ show pr) ([castPresToLay pr], noFocus)

detokenizeList' wm p t i []           = ([], noFocus)
detokenizeList' wm p t i (pres:press) = let (press',  f1) = detokenize' wm (p++[i]) t pres 
                                            (presss', f2) = detokenizeList' wm p t (i+length press') press
                                        in  debug Lay ("detokList "++show i)  (press' ++ presss', combineFocus f1 f2)


-- recursive rows cause problems. we cannot add to the path for every row (only the topmost), but if deeper rows
-- cause the creation of more rows in the result, then this should be taken into account at the top-level.
-- in order to do so, we would need some threaded attribute, which will complicate the code even more.

detokenizeRow' :: Show token => WhitespaceMap -> Path -> Bool -> Int -> [Presentation doc node clip token] -> 
                  ([Layout doc node clip], FocusPres)
detokenizeRow' wm p t i [] = ([], noFocus)
detokenizeRow' wm p t i (pres:press) =
  let i' = i+length press' - if null press' then 0 else 1
      (press',  f1) = if t then detokenize' wm (p) True pres
                           else detokenize' wm (p++[i]) True pres
      (press'', f2) = detokenizeRow' wm p t i' press
  in (combine p i' press' press'' f1 f2)
  -- the last and first lines are merged, so if press' has more than 0 lines, we decrement i with 1
  

singleton []       = debug Err ("Layout.detokenize': graph child without singleton token (add row to presentation)") $ EmptyP NoIDP
singleton [pres]   = pres
singleton (pres:_) = debug Err ("Layout.detokenize': graph child without singleton token (add row to presentation)") $ pres

combine :: Path -> Int -> [Presentation doc node clip token] -> [Presentation doc node clip token] ->
           FocusPres -> FocusPres -> ([Presentation doc node clip token], FocusPres)
combine pth i [] l2 f1 f2 = (l2, f2) -- in this case f1 will always be noFocus, so we take f2
combine pth i l1 [] f1 f2 = (l1, f1) -- in this case f2 will always be noFocus, so we take f1
combine pth i l1 l2 f1 f2 = let f1' = modifyLeftFocus pth i f1 
                                f2' = modifyRightFocus pth i f2
                            in  ( init l1 ++ [RowP NoIDP 0 $ [last l1,head l2] ] ++ tail l2
                                , combineFocus f1' f2'
                                )

-- focus : [ .. path .. ] ++ n : rest  -> [ .. path .. ] ++ n : 0 : rest
modifyLeftFocus path mergedRow focus = mapFocusPath f focus
 where f focusPath = 
         case drop (length path) focusPath of -- the focus starting at location pth
                 (i:rest) -> if i == mergedRow then let res = path ++ i:0:rest 
                                             in debug Lay ("Focus l modified "++show focusPath ++ "->"++ show res ) res
                             else path ++ i:rest
                 []       -> debug Err ("Layout.detokenize': modifyLeftFocus path is empty") $ focusPath
                            
-- focus : [ .. path .. ] ++ n : rest  -> [ .. path .. ] ++ n : 1 : rest
modifyRightFocus path mergedRow focus = mapFocusPath f focus
 where f focusPath = 
         case drop (length path) focusPath of -- the focus starting at location pth
                 (i:rest) -> if i == mergedRow then let res = path ++ i:1:rest 
                                             in debug Lay ("Focus r modified "++show focusPath ++ "->"++ show res ) res
                             else path ++ i:rest
                 []       -> debug Err ("Layout.detokenize': modifyLeftFocus path is empty") $ focusPath

mapFocusPath :: (Path -> Path) -> FocusPres -> FocusPres
mapFocusPath f NoFocusP = NoFocusP
mapFocusPath f (FocusP p1 p2) = FocusP (mapPath p1) (mapPath p2)
 where mapPath NoPathP = NoPathP
       mapPath (PathP p i) = PathP (f p) i


addWhitespaceToken :: Show token => WhitespaceMap -> Path -> IDP -> Token doc node clip token -> 
                      ([Layout doc node clip], FocusPres)
addWhitespaceToken wm p idp (UserTk _ str _ _) = 
  case Map.lookup idp wm of 
    Just tokenLayout  -> (if hasFocus (tokenFocus tokenLayout) then debug Lay ("Focus on"++str++show p) else id) 
                         (addWhitespace wm idp str, if hasFocus (tokenFocus tokenLayout) then FocusP (PathP p 0) (PathP p 0) else noFocus)
    Nothing           -> error "no info"
  where hasFocus (Nothing, Nothing) = False
        hasFocus _ = True
addWhitespaceToken wm p idp (StructuralTk _ pres _ _) = 
  let (pres', f) = detokenize wm p pres
  in (addWhitespaceStruct wm idp pres', f)

addWhitespace :: WhitespaceMap -> IDP -> String -> [Layout doc node clip]
addWhitespace wm NoIDP str = [StringP NoIDP str]
addWhitespace wm idp str = 
  case fmap whitespaceFocus $ Map.lookup idp wm  of
    Nothing -> [StringP idp str]
    Just ((breaks, spaces),focus) ->    replicate breaks (StringP NoIDP "") 
                                     ++ [row [StringP NoIDP (replicate spaces ' '), StringP idp str]]

addWhitespaceStruct :: WhitespaceMap -> IDP -> Layout doc node clip -> [Layout doc node clip]
addWhitespaceStruct wm NoIDP struct = [struct]
addWhitespaceStruct wm idp struct = 
  case fmap whitespaceFocus $ Map.lookup idp wm of
    Nothing -> [struct]
    Just ((breaks, spaces),focus) ->    replicate breaks (StringP NoIDP "") 
                                     ++ [RowP NoIDP 0 [ StringP NoIDP (replicate spaces ' ')
                                                      , struct
                                                      ]]
                                




{-
onlyWhitespace :: WhitespaceMap -> IDP -> [ Presentation ]
onlyWhitespace wm NoIDP = []
onlyWhitespace wm idp = 
  case Map.lookup wm idp of
    Nothing -> []
    Just (breaks, spaces) ->    replicate breaks (StringP NoIDP "") 
                             ++ [StringP NoIDP (replicate spaces ' ')]
-}

--instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a,b,c,d,e,f) where -- why isn't this a standard instance?
--  show (a,b,c,d,e,f) = "("++show a++","++show b++","++show c++","++show d++","++show e++","++show f++")"

-- row and column mappings get lost in a parsing structure. What are the consequences? And can we do something
-- about it?

-- !!(Look further at this:)last bit of layout in an empty token? for now we put it in an empty string token. 


-- white space in front of images not correct now

-- new anonymous character in front of token, leads to anonymous token. Maybe take idp from the first non anonymous
-- character.

-- structurals should have whitespace
