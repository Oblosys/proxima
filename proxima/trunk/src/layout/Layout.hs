module Layout where

import CommonTypes
import LayLayerTypes
import LayLayerUtils

import qualified Data.Map as Map
import Data.Map (Map)


-- add layout from local state to all descendents of ParsingP node, until StructureP node is encountered

-- mutually recursive functions: detokenize for walking the tree until ParsingP node
--                               detokenize' for adding whitespace until StructureP node

-- Local State is layout, inserted and deleted state

-- detokenize ignores Lexer information, since alle tokens can be treated the same when layouting.
-- Note that freetext tokens have no layout in the extra state, but since their id's will not be
-- in the WhitespaceMap, nothing will be inserted.
detokenize :: Show token => WhitespaceMap -> Presentation doc node clip token -> Layout doc node clip
detokenize lm (ParsingP id l pres)       = let press = detokenize' lm pres
                                           in  if null press 
                                               then debug Err ("TreeEditPres.detokenize empty token list") (StringP NoIDP "") 
                                               else if length press == 1  -- this will usually be the case because breaks
                                               then ParsingP id l $ head press -- are produced by col's so there will be one there
                                               else ParsingP id l $ ColP NoIDP 0 NF press 
detokenize lm (EmptyP id)            = EmptyP id
detokenize lm (StringP id str)       = StringP id str
detokenize lm (ImageP id str st)        = ImageP id str st
detokenize lm (PolyP id pts w st)       = PolyP id pts w st
detokenize lm (RectangleP id w h lw st)  = RectangleP id w h lw st
detokenize lm (EllipseP id w h lw st)    = EllipseP id w h lw st
detokenize lm (RowP id rf press)         = RowP id rf $ map (detokenize lm) press
detokenize lm (ColP id rf f press)         = ColP id rf f $ map (detokenize lm) press
detokenize lm (OverlayP id (pres:press)) = OverlayP id (detokenize lm pres : (map castPresToLay press)) -- cast is safe, no tokens in press
detokenize lm (WithP ar pres)            = WithP ar $ detokenize lm pres
detokenize lm (StructuralP id pres)      = StructuralP id $ detokenize lm pres
detokenize lm (LocatorP l pres)          = LocatorP l $ detokenize lm pres
detokenize lm (GraphP id d w h es press) = GraphP id d w h es $ map (detokenize lm) press
detokenize lm (VertexP id v x y o pres)  = VertexP id v x y o $ detokenize lm pres
detokenize lm (FormatterP id press)      = FormatterP id $ map (detokenize lm) press
detokenize lm pr                         = debug Err ("Layout.detokenize: can't handle "++ show pr) $ castPresToLay pr


-- find out semantics of this one        What about Refs?

-- incomplete, only for strings
detokenize' :: Show token => WhitespaceMap -> Presentation doc node clip token -> [Layout doc node clip]
detokenize' lm (StructuralP id pres)      = addWhitespaceStruct lm id (StructuralP id $ detokenize lm pres)
detokenize' lm (EmptyP id)                = [EmptyP id]
detokenize' lm (StringP id str)           = addWhitespace lm id str
detokenize' lm (ImageP id str st)         = [ImageP id str st]
detokenize' lm (PolyP id pts w st)        = [PolyP id pts w st]
detokenize' lm (RectangleP id w h lw st)  = [RectangleP id w h lw st]
detokenize' lm (EllipseP id w h lw st)    = [EllipseP id w h lw st]
detokenize' lm (RowP id rf press)         = detokenizeRow' lm press -- ref gets lost
detokenize' lm (ColP id rf f press)       = [ColP id rf f $ concat (map (detokenize' lm) press) ]
detokenize' lm (OverlayP id (pres:press)) = let press' = detokenize' lm pres -- cast is safe, no tokens in press
                                            in  [ OverlayP id (pres' : map castPresToLay press) | pres' <- press' ]
detokenize' lm (WithP ar pres)            = let press = detokenize' lm pres 
                                            in  map (WithP ar) press
detokenize' lm (ParsingP id l pres)       = let press = detokenize' lm pres 
                                            in  map (ParsingP id l) press
detokenize' lm (LocatorP l pres)          = let press = detokenize' lm pres 
                                            in  map (LocatorP l) press
detokenize' lm (GraphP id d w h es press) = let press' = map (singleton . detokenize' lm) press
                                            in  [GraphP id d w h es press']
detokenize' lm (VertexP id v x y ol pres) = [VertexP id v x y ol (singleton $ detokenize' lm pres)]
detokenize' lm (FormatterP id press)      = [FormatterP id $ concat (map (detokenize' lm) press) ]
detokenize' lm pr                         = debug Err ("Layout.detokenize': can't handle "++ show pr) [castPresToLay pr]

singleton []       = debug Err ("TreeEditPres.detokenize': graph child without singleton token (add row to presentation)") $ EmptyP NoIDP
singleton [pres]   = pres
singleton (pres:_) = debug Err ("TreeEditPres.detokenize': graph child without singleton token (add row to presentation)") $ pres

detokenizeRow' :: Show token => WhitespaceMap -> [Presentation doc node clip token] -> [Layout doc node clip]
detokenizeRow' lm [] = []
detokenizeRow' lm (pres:press) =
  let press' = detokenize' lm pres
      press'' = detokenizeRow' lm press                         
  in combine press' press''

combine :: [Presentation doc node clip token] -> [Presentation doc node clip token] -> [Presentation doc node clip token]
combine [] l2 = l2
combine l1 [] = l1 
combine l1 l2 = init l1 ++ [RowP NoIDP 0 $ [last l1,head l2] ] ++ tail l2

addWhitespace :: WhitespaceMap -> IDP -> String -> [Layout doc node clip]
addWhitespace lm NoIDP str = [StringP NoIDP str]
addWhitespace lm id str = 
  case Map.lookup id lm of
    Nothing -> [StringP id str]
    Just (breaks, spaces) ->    replicate breaks (StringP NoIDP "") 
                             ++ [StringP id (replicate spaces ' ' ++ str)]

addWhitespaceStruct :: WhitespaceMap -> IDP -> Layout doc node clip -> [Layout doc node clip]
addWhitespaceStruct lm NoIDP struct = [struct]
addWhitespaceStruct lm id struct = 
  case Map.lookup id lm of
    Nothing -> [struct]
    Just (breaks, spaces) ->    replicate breaks (StringP NoIDP "") 
                             ++ [RowP NoIDP 0 [ StringP NoIDP (replicate spaces ' ')
                                             , struct
                                             ]]
                                


{-
onlyWhitespace :: WhitespaceMap -> IDP -> [ Presentation ]
onlyWhitespace lm NoIDP = []
onlyWhitespace lm id = 
  case Map.lookup lm id of
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
