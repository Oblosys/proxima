module DocumentEdit_Generated where

import Common.CommonTypes hiding (Dirty (..))
import Evaluation.DocTypes
import DocTypes_Generated
import DocUtils_Generated
import Evaluation.DocumentEdit
import Evaluation.DocUtils
import Presentation.PresTypes

----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- Clip instance                                                        --
--------------------------------------------------------------------------

instance Clip ClipDoc where
  arityClip Clip_Nothing = -1
  arityClip (Clip_EnrichedDoc x) = arity x
  arityClip (Clip_Document x) = arity x
  arityClip (Clip_Tree x) = arity x
  arityClip (Clip_List_Tree x) = arity x
  arityClip (Clip_Bool x) = arity x
  arityClip (Clip_Int x) = arity x
  arityClip (Clip_String x) = arity x
  arityClip (Clip_Float x) = arity x

  alternativesClip Clip_Nothing = []
  alternativesClip (Clip_EnrichedDoc x) = alternatives x
  alternativesClip (Clip_Document x) = alternatives x
  alternativesClip (Clip_Tree x) = alternatives x
  alternativesClip (Clip_List_Tree x) = alternatives x
  alternativesClip (Clip_Bool x) = alternatives x
  alternativesClip (Clip_Int x) = alternatives x
  alternativesClip (Clip_String x) = alternatives x
  alternativesClip (Clip_Float x) = alternatives x

  holeClip Clip_Nothing = Clip_Nothing
  holeClip (Clip_EnrichedDoc x) = Clip_EnrichedDoc hole
  holeClip (Clip_Document x) = Clip_Document hole
  holeClip (Clip_Tree x) = Clip_Tree hole
  holeClip (Clip_List_Tree x) = Clip_List_Tree hole
  holeClip (Clip_Bool x) = Clip_Bool hole
  holeClip (Clip_Int x) = Clip_Int hole
  holeClip (Clip_String x) = Clip_String hole
  holeClip (Clip_Float x) = Clip_Float hole

  isListClip Clip_Nothing = False
  isListClip (Clip_EnrichedDoc x) = isList x
  isListClip (Clip_Document x) = isList x
  isListClip (Clip_Tree x) = isList x
  isListClip (Clip_List_Tree x) = isList x
  isListClip (Clip_Bool x) = isList x
  isListClip (Clip_Int x) = isList x
  isListClip (Clip_String x) = isList x
  isListClip (Clip_Float x) = isList x

  insertListClip i c Clip_Nothing = Clip_Nothing
  insertListClip i c (Clip_EnrichedDoc x) = insertList i c x
  insertListClip i c (Clip_Document x) = insertList i c x
  insertListClip i c (Clip_Tree x) = insertList i c x
  insertListClip i c (Clip_List_Tree x) = insertList i c x
  insertListClip i c (Clip_Bool x) = insertList i c x
  insertListClip i c (Clip_Int x) = insertList i c x
  insertListClip i c (Clip_String x) = insertList i c x
  insertListClip i c (Clip_Float x) = insertList i c x

  removeListClip i Clip_Nothing = Clip_Nothing
  removeListClip i (Clip_EnrichedDoc x) = removeList i x
  removeListClip i (Clip_Document x) = removeList i x
  removeListClip i (Clip_Tree x) = removeList i x
  removeListClip i (Clip_List_Tree x) = removeList i x
  removeListClip i (Clip_Bool x) = removeList i x
  removeListClip i (Clip_Int x) = removeList i x
  removeListClip i (Clip_String x) = removeList i x
  removeListClip i (Clip_Float x) = removeList i x




--------------------------------------------------------------------------
-- Editable instances                                                   --
--------------------------------------------------------------------------

instance Editable EnrichedDoc Document Node ClipDoc UserToken where
  select [] x = Clip_EnrichedDoc x
  select (0:p) (RootEnr x0 x1) = select p x0
  select (1:p) (RootEnr x0 x1) = select p x1
  select _ _ = Clip_Nothing

  paste [] (Clip_EnrichedDoc c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on EnrichedDoc") x
  paste (0:p) c (RootEnr x0 x1) = RootEnr (paste p c x0) x1
  paste (1:p) c (RootEnr x0 x1) = RootEnr x0 (paste p c x1)
  paste _ _ x = x

  alternatives _ = [ ("RootEnr {List_Tree} {List_Tree} "  , Clip_EnrichedDoc $ RootEnr hole hole)
                   ,("{EnrichedDoc}", Clip_EnrichedDoc hole)
                   ]

  arity (RootEnr x0 x1) = 2
  arity _                        = 0

  toClip t = Clip_EnrichedDoc t

  fromClip (Clip_EnrichedDoc t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrEnrichedDoc

  hole = HoleEnrichedDoc

  holeNodeConstr = Node_HoleEnrichedDoc

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Document Document Node ClipDoc UserToken where
  select [] x = Clip_Document x
  select (0:p) (RootDoc x0 x1) = select p x0
  select (1:p) (RootDoc x0 x1) = select p x1
  select _ _ = Clip_Nothing

  paste [] (Clip_Document c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Document") x
  paste (0:p) c (RootDoc x0 x1) = RootDoc (paste p c x0) x1
  paste (1:p) c (RootDoc x0 x1) = RootDoc x0 (paste p c x1)
  paste _ _ x = x

  alternatives _ = [ ("RootDoc {List_Tree} {List_Tree} "  , Clip_Document $ RootDoc hole hole)
                   ,("{Document}", Clip_Document hole)
                   ]

  arity (RootDoc x0 x1) = 2
  arity _                        = 0

  toClip t = Clip_Document t

  fromClip (Clip_Document t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrDocument

  hole = HoleDocument

  holeNodeConstr = Node_HoleDocument

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Tree Document Node ClipDoc UserToken where
  select [] x = Clip_Tree x
  select (0:p) (Bin _ _ _ _ _ x0 x1) = select p x0
  select (1:p) (Bin _ _ _ _ _ x0 x1) = select p x1
  select (0:p) (Leaf _ _ x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Tree c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Tree") x
  paste (0:p) c (Bin i0 i1 i2 i3 i4 x0 x1) = Bin i0 i1 i2 i3 i4 (paste p c x0) x1
  paste (1:p) c (Bin i0 i1 i2 i3 i4 x0 x1) = Bin i0 i1 i2 i3 i4 x0 (paste p c x1)
  paste (0:p) c (Leaf i0 i1 x0) = Leaf i0 i1 (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("Bin {Tree} {Tree} "  , Clip_Tree $ Bin NoIDP NoIDP NoIDP NoIDP NoIDP hole hole)
                   , ("Leaf {Int} "  , Clip_Tree $ Leaf NoIDP NoIDP hole)
                   ,("{Tree}", Clip_Tree hole)
                   ]

  arity (Bin _ _ _ _ _ x0 x1) = 2
  arity (Leaf _ _ x0) = 1
  arity _                        = 0

  toClip t = Clip_Tree t

  fromClip (Clip_Tree t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrTree

  hole = HoleTree

  holeNodeConstr = Node_HoleTree

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable List_Tree Document Node ClipDoc UserToken where
  select [] x = Clip_List_Tree x
  select (n:p) (List_Tree cxs) =
    let xs = fromConsList_Tree cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Tree c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Tree")   x
  paste (n:p) c (List_Tree cxs) =
    let xs = fromConsList_Tree cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Tree (replaceList_Tree n x' cxs)
        else List_Tree cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Tree}", Clip_List_Tree hole)
                   ]

  arity (List_Tree x1) = length (fromConsList_Tree x1)
  arity _ = 0

  toClip t = Clip_List_Tree t

  fromClip (Clip_List_Tree t) = Just t
  fromClip _ = Nothing

  parseErr = ParseErrList_Tree

  hole = List_Tree Nil_Tree

  holeNodeConstr = Node_HoleList_Tree

  isList _ = True

  insertList n (Clip_Tree c) (List_Tree cxs) = Clip_List_Tree $ List_Tree (insertList_Tree n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Tree xs
  insertList _ c xs = Clip_List_Tree xs

  removeList n (List_Tree cxs) = Clip_List_Tree $ List_Tree (removeList_Tree n cxs)
  removeList _ xs = Clip_List_Tree $ xs




--------------------------------------------------------------------------
-- Editable instances for Document, EnrichedDoc and primitive types     --
--------------------------------------------------------------------------

instance Editable Int Document Node ClipDoc UserToken where
  select [] x = Clip_Int x
  select _  _ = Clip_Nothing
  paste [] (Clip_Int c) x = c
  paste [] c            x =  debug Err ("Type error: pasting "++show c++" on Int") x
  paste _  _             x = x
  
  alternatives _ = [ ("0", Clip_Int 0) ]
  
  arity _ = 0

  toClip t = Clip_Int t

  fromClip (Clip_Int t) = Just t
  fromClip _            = Nothing

  parseErr _ = 0

  hole = 0

  holeNodeConstr = error "Type Int is primitive and has no hole node constructorstructor"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Float Document Node ClipDoc UserToken where
  select [] x = Clip_Float x
  select _  _ = Clip_Nothing
  paste [] (Clip_Float c) x = c
  paste [] c              x =  debug Err ("Type error: pasting "++show c++" on Float") x
  paste _  _              x = x
  
  alternatives _ = [ ("0.0", Clip_Float 0.0) ]
  
  arity _ = 0

  toClip t = Clip_Float t

  fromClip (Clip_Float t) = Just t
  fromClip _              = Nothing

  parseErr _ = 0

  hole = 0

  holeNodeConstr = error "Type Float is primitive and has no hole node constructorstructor"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Bool Document Node ClipDoc UserToken where
  select [] x = Clip_Bool x                            
  select _  _ = Clip_Nothing                           
  paste [] (Clip_Bool c) x = c                         
  paste [] c             x =  debug Err ("Type error: pasting "++show c++" on Bool") x
  paste _  _             x = x
  alternatives _ = [ ("True", Clip_Bool True)        
                   , ("False", Clip_Bool False)      
                   ]    
  arity _ = 0                                          

  toClip t = Clip_Bool t

  fromClip (Clip_Bool t) = Just t
  fromClip _             = Nothing

  parseErr _ = False

  hole = False

  holeNodeConstr = error "Type Bool is primitive and has no hole node constructorstructor"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable String Document Node ClipDoc UserToken where
  select [] x = Clip_String x
  select _  _ = Clip_Nothing
  paste [] (Clip_String c) x = c
  paste [] c             x =  debug Err ("Type error: pasting "++show c++" on String") x
  paste _  _             x = x

  alternatives _ = [ ("string", Clip_String "string")
                   ] 
 
  arity _ = 0

  toClip t = Clip_String t

  fromClip (Clip_String t) = Just t
  fromClip _               = Nothing

  parseErr _ = "{ParseErr}"

  hole = "{String}"

  holeNodeConstr = error "Type String is primitive and has no hole node constructorstructor"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


