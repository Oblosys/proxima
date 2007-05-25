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
detokenize :: (LayoutMap, InsertedTokenList, DeletedTokenMap doc node clip) -> Presentation doc node clip -> Presentation doc node clip
detokenize lm (ParsingP id pres)         = let press = detokenize' lm pres
                                           in  if null press 
                                               then debug Err ("TreeEditPres.detokenize empty token list") (StringP NoIDP "") 
                                               else if length press == 1  -- this will usually be the case because breaks
                                               then ParsingP id $ head press -- are produced by col's so there will be one there
                                               else ParsingP id $ ColP NoIDP 0 press 
detokenize lm pres@(EmptyP _)            = pres
detokenize lm pres@(StringP _ str)       = pres
detokenize lm pres@(ImageP _ _)          = pres
detokenize lm pres@(PolyP _ _ _)         = pres
detokenize lm pres@(RectangleP _ _ _ _)  = pres
detokenize lm pres@(EllipseP _ _ _ _)    = pres
detokenize lm (RowP id rf press)         = RowP id rf $ map (detokenize lm) press
detokenize lm (ColP id rf press)         = ColP id rf $ map (detokenize lm) press
detokenize lm (OverlayP id (pres:press)) = OverlayP id (detokenize lm pres : press)
detokenize lm (WithP ar pres)            = WithP ar $ detokenize lm pres
detokenize lm (StructuralP id pres)      = StructuralP id $ detokenize lm pres
detokenize lm (LocatorP l pres)          = LocatorP l $ detokenize lm pres
detokenize lm (GraphP id w h es press)   = GraphP id w h es $ map (detokenize lm) press
detokenize lm (VertexP id o pres)        = VertexP id o $ detokenize lm pres
detokenize lm pr                         = debug Err ("TreeEditPres.detokenize: can't handle "++ show pr) pr


-- find out semantics of this one        What about Refs?

-- incomplete, only for strings
detokenize' lm (StructuralP id pres)      = addWhitespaceStruct lm id (StructuralP id $ detokenize lm pres)
detokenize' lm pres@(EmptyP _)            = [pres]
detokenize' lm pres@(StringP id str)      = addWhitespace lm id str
detokenize' lm pres@(ImageP id _)         = [pres]
detokenize' lm pres@(PolyP id _ _)        = [pres]
detokenize' lm pres@(RectangleP _ _ _ _)  = [pres]
detokenize' lm pres@(EllipseP _ _ _ _)    = [pres]
detokenize' lm (RowP id rf press)         = detokenizeRow' lm press -- ref gets lost
detokenize' lm (ColP id rf press)         = [ColP id rf $ concat (map (detokenize' lm) press) ]
detokenize' lm (OverlayP id (pres:press)) = let press' = detokenize' lm pres
                                            in  [ OverlayP id (pres' : press) | pres' <- press' ]
detokenize' lm (WithP ar pres)            = let press = detokenize' lm pres 
                                            in  map (WithP ar) press
detokenize' lm (ParsingP id pres)         = let press = detokenize' lm pres 
                                            in  map (ParsingP id) press
detokenize' lm (LocatorP l pres)          = let press = detokenize' lm pres 
                                            in  map (LocatorP l) press
detokenize' lm (GraphP id w h es press)   = let press' = map (singleton . detokenize' lm) press
                                            in  [GraphP id w h es press']
detokenize' lm (VertexP id ol pres)       = [VertexP id ol (singleton $ detokenize' lm pres)]
detokenize' lm pr                         = debug Err ("TreeEditPres.detokenize': can't handle "++ show pr) [pr]

singleton []       = debug Err ("TreeEditPres.detokenize': graph child without singleton token (add row to presentation)") $ EmptyP NoIDP
singleton [pres]   = pres
singleton (pres:_) = debug Err ("TreeEditPres.detokenize': graph child without singleton token (add row to presentation)") $ pres

detokenizeRow' :: (LayoutMap, InsertedTokenList, DeletedTokenMap doc node clip) -> [Presentation doc node clip] -> [Presentation doc node clip]
detokenizeRow' lm [] = []
detokenizeRow' lm (pres:press) =
  let press' = detokenize' lm pres
      press'' = detokenizeRow' lm press                         
  in combine press' press''

combine :: [Presentation doc node clip] -> [Presentation doc node clip] -> [Presentation doc node clip]
combine [] l2 = l2
combine l1 [] = l1 
combine l1 l2 = init l1 ++ [RowP NoIDP 0 $ [last l1,head l2] ] ++ tail l2

addWhitespace :: (LayoutMap, InsertedTokenList, DeletedTokenMap doc node clip) -> IDP -> String -> [Presentation doc node clip]
addWhitespace (lm,inss, dels) NoIDP str = [StringP NoIDP str]
addWhitespace (lm,inss, dels) id str = 
  case Map.lookup id lm of
    Nothing -> markInssDels (lm,inss,dels) id $ StringP id str
    Just (breaks, spaces) ->    replicate breaks (StringP NoIDP "") 
                             ++ (markInssDels (lm,inss,dels) id $ StringP id (replicate spaces ' ' ++ str))

addWhitespaceStruct :: (LayoutMap, InsertedTokenList, DeletedTokenMap doc node clip) -> IDP -> Presentation doc node clip -> [Presentation doc node clip]
addWhitespaceStruct (lm,inss, dels) NoIDP struct = [struct]
addWhitespaceStruct (lm,inss, dels) id struct = 
  case Map.lookup id lm of
    Nothing -> markInssDels (lm,inss,dels) id $ struct
    Just (breaks, spaces) ->    replicate breaks (StringP NoIDP "") 
                             ++ (markInssDels (lm,inss,dels) id $ RowP NoIDP 0 [ StringP NoIDP (replicate spaces ' ')
                                                                               , struct
                                                                               ])
                                

markInssDels :: (LayoutMap, InsertedTokenList, DeletedTokenMap doc node clip) -> IDP -> Presentation doc node clip -> [Presentation doc node clip]
markInssDels (lm,inss,dels) idp pres = 
  combine 
    (case Map.lookup idp dels of
      Just deletedTk -> detokenize' (lm,inss,dels) (squiggly red deletedTk)
      Nothing        -> [])
    (if {-idp `elem` inss-} idp ==IDP (-1)
     then [squiggly green pres `withbgColor` whiteSmoke]
     else [pres])

{-
onlyWhitespace :: LayoutMap -> IDP -> [ Presentation ]
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
