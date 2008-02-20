module Layout.Layout where

import Common.CommonTypes
import Layout.LayLayerTypes
import Layout.LayLayerUtils

import qualified Data.Map as Map
import Data.Map (Map)


-- add layout from local state to all descendents of ParsingP node, until StructureP node is encountered

-- mutually recursive functions: detokenize for walking the tree until ParsingP node
--                               detokenize' for adding whitespace until StructureP node

-- Local State is layout

-- detokenize ignores Lexer information, since alle tokens can be treated the same when layouting.
detokenize :: Show token => WhitespaceMap -> Presentation doc node clip token -> Layout doc node clip
detokenize wm (ParsingP id l pres)       = let press = detokenize' wm pres
                                           in  if null press 
                                               then debug Err ("TreeEditPres.detokenize empty token list") (StringP NoIDP "") 
                                               else if length press == 1  -- this will usually be the case because breaks
                                               then ParsingP id l $ head press -- are produced by col's so there will be one there
                                               else ParsingP id l $ ColP NoIDP 0 NF press 
detokenize wm (EmptyP id)                = EmptyP id
detokenize wm (StringP id str)           = StringP id str
detokenize wm (ImageP id str st)         = ImageP id str st
detokenize wm (PolyP id pts w st)        = PolyP id pts w st
detokenize wm (RectangleP id w h lw st)  = RectangleP id w h lw st
detokenize wm (EllipseP id w h lw st)    = EllipseP id w h lw st
detokenize wm (RowP id rf press)         = RowP id rf $ map (detokenize wm) press
detokenize wm (ColP id rf f press)       = ColP id rf f $ map (detokenize wm) press
detokenize wm (OverlayP id (pres:press)) = OverlayP id (detokenize wm pres : (map castPresToLay press)) -- cast is safe, no tokens in press
detokenize wm (WithP ar pres)            = WithP ar $ detokenize wm pres
detokenize wm (StructuralP id pres)      = StructuralP id $ detokenize wm pres
detokenize wm (LocatorP l pres)          = LocatorP l $ detokenize wm pres
detokenize wm (GraphP id d w h es press) = GraphP id d w h es $ map (detokenize wm) press
detokenize wm (VertexP id v x y o pres)  = VertexP id v x y o $ detokenize wm pres
detokenize wm (FormatterP id press)      = FormatterP id $ map (detokenize wm) press
detokenize wm pr                         = debug Err ("Layout.detokenize: can't handle "++ show pr) $ castPresToLay pr


-- find out semantics of this one        What about Refs?

-- incomplete, only for strings
detokenize' :: Show token => WhitespaceMap -> Presentation doc node clip token -> [Layout doc node clip]
detokenize' wm (StructuralP id pres)      = [StructuralP id $ detokenize wm pres]
detokenize' wm (EmptyP id)                = [EmptyP id]
detokenize' wm (StringP id str)           = [StringP id str] -- addWhitespace wm id str
detokenize' wm (TokenP id token)          = addWhitespaceToken wm id token
detokenize' wm (ImageP id str st)         = [ImageP id str st]
detokenize' wm (PolyP id pts w st)        = [PolyP id pts w st]
detokenize' wm (RectangleP id w h lw st)  = [RectangleP id w h lw st]
detokenize' wm (EllipseP id w h lw st)    = [EllipseP id w h lw st]
detokenize' wm (RowP id rf press)         = detokenizeRow' wm press -- ref gets lost
detokenize' wm (ColP id rf f press)       = [ColP id rf f $ concat (map (detokenize' wm) press)]
detokenize' wm (OverlayP id (pres:press)) = let press' = detokenize' wm pres -- cast is safe, no tokens in press
                                            in  [ OverlayP id (pres' : map castPresToLay press) | pres' <- press' ]
detokenize' wm (WithP ar pres)            = let press = detokenize' wm pres 
                                            in  map (WithP ar) press
detokenize' wm (ParsingP id l pres)       = let press = detokenize' wm pres 
                                            in  map (ParsingP id l) press
detokenize' wm (LocatorP l pres)          = let press = detokenize' wm pres 
                                            in  map (LocatorP l) press
detokenize' wm (GraphP id d w h es press) = let press' = map (singleton . detokenize' wm) press
                                            in  [GraphP id d w h es press']
detokenize' wm (VertexP id v x y ol pres) = [VertexP id v x y ol (singleton $ detokenize' wm pres)]
detokenize' wm (FormatterP id press)      = [FormatterP id $ concat (map (detokenize' wm) press) ]
detokenize' wm pr                         = debug Err ("Layout.detokenize': can't handle "++ show pr) [castPresToLay pr]

singleton []       = debug Err ("TreeEditPres.detokenize': graph child without singleton token (add row to presentation)") $ EmptyP NoIDP
singleton [pres]   = pres
singleton (pres:_) = debug Err ("TreeEditPres.detokenize': graph child without singleton token (add row to presentation)") $ pres

detokenizeRow' :: Show token => WhitespaceMap -> [Presentation doc node clip token] -> [Layout doc node clip]
detokenizeRow' wm [] = []
detokenizeRow' wm (pres:press) =
  let press' = detokenize' wm pres
      press'' = detokenizeRow' wm press                         
  in combine press' press''

combine :: [Presentation doc node clip token] -> [Presentation doc node clip token] -> [Presentation doc node clip token]
combine [] l2 = l2
combine l1 [] = l1 
combine l1 l2 = init l1 ++ [RowP NoIDP 0 $ [last l1,head l2] ] ++ tail l2

addWhitespace :: WhitespaceMap -> IDP -> String -> [Layout doc node clip]
addWhitespace wm NoIDP str = [StringP NoIDP str]
addWhitespace wm id str = 
  case Map.lookup id wm of
    Nothing -> [StringP id str]
    Just ((breaks, spaces),focus) ->    replicate breaks (StringP NoIDP "") 
                                     ++ [StringP id (replicate spaces ' ' ++ str)]

addWhitespaceStruct :: WhitespaceMap -> IDP -> Layout doc node clip -> [Layout doc node clip]
addWhitespaceStruct wm NoIDP struct = [struct]
addWhitespaceStruct wm id struct = 
  case Map.lookup id wm of
    Nothing -> [struct]
    Just ((breaks, spaces),focus) ->    replicate breaks (StringP NoIDP "") 
                                     ++ [RowP NoIDP 0 [ StringP NoIDP (replicate spaces ' ')
                                                      , struct
                                                      ]]
                                
addWhitespaceToken :: Show token => WhitespaceMap -> IDP -> Token doc node clip token -> [Layout doc node clip]
addWhitespaceToken wm id (UserTk _ str _ _) = addWhitespace wm id str
addWhitespaceToken wm id (StructuralTk _ pres _ _) = addWhitespaceStruct wm id (detokenize wm pres)




{-
onlyWhitespace :: WhitespaceMap -> IDP -> [ Presentation ]
onlyWhitespace wm NoIDP = []
onlyWhitespace wm id = 
  case Map.lookup wm id of
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

-- new anonymous character in front of token, leads to anonymous token. Maybe take id from the first non anonymous
-- character.

-- structurals should have whitespace
