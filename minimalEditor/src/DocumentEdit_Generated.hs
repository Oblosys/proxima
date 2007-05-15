module DocumentEdit_Generated where

import CommonTypes
import DocTypes
import DocTypes_Generated
import DocumentEdit
import DocUtils
import PresTypes

import Debug.Trace


-- not entirely generated: hole is special for chessboard and pppresentation, because hole is initialized
-- This is not good. hole must be just a hole for cut operations.
-- an initialized value must be specified separately class member initialValue?


-- Constructor for HoleClip can be put in class as toClip or inject


-- paths start below RootDoc, so on traversing the RootDoc constructor p is not modified
instance Editable Document Document Node ClipDoc where
  select p (RootDoc id x) = select p x
  paste p c (RootDoc id  x) = RootDoc id $ paste p c x
  hole = HoleDocument
  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing
  
instance Editable Int Document Node ClipDoc where
  select [] x = Clip_Int x
  select _  _ = Clip_Nothing
  paste [] (Clip_Int c) x = c
  paste [] c            x =  trace ("Type error: pasting "++show c++" on Int") x
  paste _  _             x = x
  
  alternatives _ = [ ("0", Clip_Int 0)
                   , ("1", Clip_Int 1)
                   , ("{Int}", Clip_Int hole) ]
  
  arity _ = 0
  parseErr _ = 0

  hole = 0

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Bool Document Node ClipDoc where                         
  select [] x = Clip_Bool x                            
  select _  _ = Clip_Nothing                           
  paste [] (Clip_Bool c) x = c                         
  paste [] c             x =  trace ("Type error: pasting "++show c++" on Bool") x
  paste _  _             x = x
  alternatives _ = [ ("True", Clip_Bool True)        
                   , ("False", Clip_Bool False)      
                   , ("{Bool}", Clip_Bool hole) ]    
  arity _ = 0                                          
  parseErr _ = False

  hole = False

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable String Document Node ClipDoc where
  select [] x = Clip_String x
  select _  _ = Clip_Nothing
  paste [] (Clip_String c) x = c
  paste [] c             x =  trace ("Type error: pasting "++show c++" on String") x
  paste _  _             x = x

  alternatives _ = [ ("a", Clip_String "a")
                   , ("ab", Clip_String "ab")
                   , ("{String}", Clip_String hole) ] 
 
  arity _ = 0
  parseErr _= "{ParseErr}"

  hole = "{String}"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing



----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

{- ------------------------------------

 generated part

-------------------------------------- -}


-- Generated clipfunctions  --

instance Clip ClipDoc where
  arityClip (Clip_Root x) = arity x
  arityClip (Clip_Document x) = arity x
  arityClip (Clip_String x) = arity x
  arityClip (Clip_Bool x) = arity x
  arityClip (Clip_Int x) = arity x
  arityClip (Clip_List_Dummy x) = arity x
  arityClip (Clip_String_ x) = arity x
  arityClip (Clip_Bool_ x) = arity x
  arityClip (Clip_Int_ x) = arity x
  arityClip (Clip_Tree x) = arity x
  arityClip (Clip_Graph x) = arity x
  arityClip (Clip_List_Vertex x) = arity x
  arityClip (Clip_List_Edge x) = arity x
  arityClip (Clip_Dummy x) = arity x
  arityClip (Clip_Vertex x) = arity x
  arityClip (Clip_Edge x) = arity x
  arityClip (Clip_Nothing)   = -1
  alternativesClip (Clip_Root x) = alternatives x
  alternativesClip (Clip_Document x) = alternatives x
  alternativesClip (Clip_String x) = alternatives x
  alternativesClip (Clip_Bool x) = alternatives x
  alternativesClip (Clip_Int x) = alternatives x
  alternativesClip (Clip_List_Dummy x) = alternatives x
  alternativesClip (Clip_String_ x) = alternatives x
  alternativesClip (Clip_Bool_ x) = alternatives x
  alternativesClip (Clip_Int_ x) = alternatives x
  alternativesClip (Clip_Tree x) = alternatives x
  alternativesClip (Clip_Graph x) = alternatives x
  alternativesClip (Clip_List_Vertex x) = alternatives x
  alternativesClip (Clip_List_Edge x) = alternatives x
  alternativesClip (Clip_Dummy x) = alternatives x
  alternativesClip (Clip_Vertex x) = alternatives x
  alternativesClip (Clip_Edge x) = alternatives x
  alternativesClip (Clip_Nothing)   = []

  holeClip (Clip_Root x) = Clip_Root hole
  holeClip (Clip_Document x) = Clip_Document hole
  holeClip (Clip_String x) = Clip_String hole
  holeClip (Clip_Bool x) = Clip_Bool hole
  holeClip (Clip_Int x) = Clip_Int hole
  holeClip (Clip_List_Dummy x) = Clip_List_Dummy hole
  holeClip (Clip_String_ x) = Clip_String_ hole
  holeClip (Clip_Bool_ x) = Clip_Bool_ hole
  holeClip (Clip_Int_ x) = Clip_Int_ hole
  holeClip (Clip_Tree x) = Clip_Tree hole
  holeClip (Clip_Graph x) = Clip_Graph hole
  holeClip (Clip_List_Vertex x) = Clip_List_Vertex hole
  holeClip (Clip_List_Edge x) = Clip_List_Edge hole
  holeClip (Clip_Dummy x) = Clip_Dummy hole
  holeClip (Clip_Vertex x) = Clip_Vertex hole
  holeClip (Clip_Edge x) = Clip_Edge hole
  holeClip Clip_Nothing   = Clip_Nothing

  isListClip (Clip_Root x) = isList x
  isListClip (Clip_Document x) = isList x
  isListClip (Clip_String x) = isList x
  isListClip (Clip_Bool x) = isList x
  isListClip (Clip_Int x) = isList x
  isListClip (Clip_List_Dummy x) = isList x
  isListClip (Clip_String_ x) = isList x
  isListClip (Clip_Bool_ x) = isList x
  isListClip (Clip_Int_ x) = isList x
  isListClip (Clip_Tree x) = isList x
  isListClip (Clip_Graph x) = isList x
  isListClip (Clip_List_Vertex x) = isList x
  isListClip (Clip_List_Edge x) = isList x
  isListClip (Clip_Dummy x) = isList x
  isListClip (Clip_Vertex x) = isList x
  isListClip (Clip_Edge x) = isList x
  isListClip (Clip_Nothing)   = False

  insertListClip i c (Clip_Root x) = insertList i c x
  insertListClip i c (Clip_Document x) = insertList i c x
  insertListClip i c (Clip_String x) = insertList i c x
  insertListClip i c (Clip_Bool x) = insertList i c x
  insertListClip i c (Clip_Int x) = insertList i c x
  insertListClip i c (Clip_List_Dummy x) = insertList i c x
  insertListClip i c (Clip_String_ x) = insertList i c x
  insertListClip i c (Clip_Bool_ x) = insertList i c x
  insertListClip i c (Clip_Int_ x) = insertList i c x
  insertListClip i c (Clip_Tree x) = insertList i c x
  insertListClip i c (Clip_Graph x) = insertList i c x
  insertListClip i c (Clip_List_Vertex x) = insertList i c x
  insertListClip i c (Clip_List_Edge x) = insertList i c x
  insertListClip i c (Clip_Dummy x) = insertList i c x
  insertListClip i c (Clip_Vertex x) = insertList i c x
  insertListClip i c (Clip_Edge x) = insertList i c x
  insertListClip i c (Clip_Nothing)   = Clip_Nothing

  removeListClip i (Clip_Root x) = removeList i x
  removeListClip i (Clip_Document x) = removeList i x
  removeListClip i (Clip_String x) = removeList i x
  removeListClip i (Clip_Bool x) = removeList i x
  removeListClip i (Clip_Int x) = removeList i x
  removeListClip i (Clip_List_Dummy x) = removeList i x
  removeListClip i (Clip_String_ x) = removeList i x
  removeListClip i (Clip_Bool_ x) = removeList i x
  removeListClip i (Clip_Int_ x) = removeList i x
  removeListClip i (Clip_Tree x) = removeList i x
  removeListClip i (Clip_Graph x) = removeList i x
  removeListClip i (Clip_List_Vertex x) = removeList i x
  removeListClip i (Clip_List_Edge x) = removeList i x
  removeListClip i (Clip_Dummy x) = removeList i x
  removeListClip i (Clip_Vertex x) = removeList i x
  removeListClip i (Clip_Edge x) = removeList i x
  removeListClip i (Clip_Nothing)   = Clip_Nothing


-- Editable Instances --



instance Editable String_ Document Node ClipDoc where
  select []    x                  = Clip_String_ x
  select (0:p) (String_ _ x1) = select p x1
  select _     _                  = Clip_Nothing

  paste [] (Clip_String_ c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on String_")   x
  paste (0:p) c (String_ i1 x1) = String_ i1 (paste p c x1)
  paste _  _  x                    = x

  alternatives _ = [("String_ "  , Clip_String_ $ String_ NoIDD hole)
                   ,("{String_}", Clip_String_ hole)
                   ]

  arity (String_ _ x1) = 1
  arity _                        = 0

  parseErr = ParseErrString_

  hole = HoleString_


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Bool_ Document Node ClipDoc where
  select []    x                  = Clip_Bool_ x
  select (0:p) (Bool_ _ x1) = select p x1
  select _     _                  = Clip_Nothing

  paste [] (Clip_Bool_ c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Bool_")   x
  paste (0:p) c (Bool_ i1 x1) = Bool_ i1 (paste p c x1)
  paste _  _  x                    = x

  alternatives _ = [("Bool_ "  , Clip_Bool_ $ Bool_ NoIDD hole)
                   ,("{Bool_}", Clip_Bool_ hole)
                   ]

  arity (Bool_ _ x1) = 1
  arity _                        = 0

  parseErr = ParseErrBool_

  hole = HoleBool_


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Int_ Document Node ClipDoc where
  select []    x                  = Clip_Int_ x
  select (0:p) (Int_ _ x1) = select p x1
  select _     _                  = Clip_Nothing

  paste [] (Clip_Int_ c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Int_")   x
  paste (0:p) c (Int_ i1 x1) = Int_ i1 (paste p c x1)
  paste _  _  x                    = x

  alternatives _ = [("Int_ "  , Clip_Int_ $ Int_ NoIDD hole)
                   ,("{Int_}", Clip_Int_ hole)
                   ]

  arity (Int_ _ x1) = 1
  arity _                        = 0

  parseErr = ParseErrInt_

  hole = HoleInt_


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Dummy Document Node ClipDoc where
  select []    x                  = Clip_Dummy x
  select (0:p) (Dummy _ x1 x2 x3 x4) = select p x1
  select (1:p) (Dummy _ x1 x2 x3 x4) = select p x2
  select (2:p) (Dummy _ x1 x2 x3 x4) = select p x3
  select (3:p) (Dummy _ x1 x2 x3 x4) = select p x4
  select _     _                  = Clip_Nothing

  paste [] (Clip_Dummy c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Dummy")   x
  paste (0:p) c (Dummy i1 x1 x2 x3 x4) = Dummy i1 (paste p c x1) x2 x3 x4
  paste (1:p) c (Dummy i1 x1 x2 x3 x4) = Dummy i1 x1 (paste p c x2) x3 x4
  paste (2:p) c (Dummy i1 x1 x2 x3 x4) = Dummy i1 x1 x2 (paste p c x3) x4
  paste (3:p) c (Dummy i1 x1 x2 x3 x4) = Dummy i1 x1 x2 x3 (paste p c x4)
  paste _  _  x                    = x

  alternatives _ = [("Dummy {Dummys} {String_} {Bool_} {Int_} "  , Clip_Dummy $ Dummy NoIDD hole hole hole hole)
                   ,("{Dummy}", Clip_Dummy hole)
                   ]

  arity (Dummy _ x1 x2 x3 x4) = 4
  arity _                        = 0

  parseErr = ParseErrDummy

  hole = HoleDummy


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Root Document Node ClipDoc where
  select []    x                  = Clip_Root x
  select (0:p) (Root _ x1 x2) = select p x1
  select (1:p) (Root _ x1 x2) = select p x2
  select _     _                  = Clip_Nothing

  paste [] (Clip_Root c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Root")   x
  paste (0:p) c (Root i1 x1 x2) = Root i1 (paste p c x1) x2
  paste (1:p) c (Root i1 x1 x2) = Root i1 x1 (paste p c x2)
  paste _  _  x                    = x

  alternatives _ = [("Root {Tree} {Graph} "  , Clip_Root $ Root NoIDD hole hole)
                   ,("{Root}", Clip_Root hole)
                   ]

  arity (Root _ x1 x2) = 2
  arity _                        = 0

  parseErr = ParseErrRoot

  hole = HoleRoot


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Tree Document Node ClipDoc where
  select []    x                  = Clip_Tree x
  select (0:p) (Bin _ x1 x2) = select p x1
  select (1:p) (Bin _ x1 x2) = select p x2
  select _     _                  = Clip_Nothing

  paste [] (Clip_Tree c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Tree")   x
  paste (0:p) c (Bin i1 x1 x2) = Bin i1 (paste p c x1) x2
  paste (1:p) c (Bin i1 x1 x2) = Bin i1 x1 (paste p c x2)
  paste _  _  x                    = x

  alternatives _ = [("Bin {Tree} {Tree} "  , Clip_Tree $ Bin NoIDD hole hole)
                   ,("Leaf "  , Clip_Tree $ Leaf NoIDD)
                   ,("{Tree}", Clip_Tree hole)
                   ]

  arity (Bin _ x1 x2) = 2
  arity (Leaf _) = 0
  arity _                        = 0

  parseErr = ParseErrTree

  hole = HoleTree


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Graph Document Node ClipDoc where
  select []    x                  = Clip_Graph x
  select (0:p) (Graph _ x1 x2) = select p x1
  select (1:p) (Graph _ x1 x2) = select p x2
  select _     _                  = Clip_Nothing

  paste [] (Clip_Graph c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Graph")   x
  paste (0:p) c (Graph i1 x1 x2) = Graph i1 (paste p c x1) x2
  paste (1:p) c (Graph i1 x1 x2) = Graph i1 x1 (paste p c x2)
  paste _  _  x                    = x

  alternatives _ = [("Graph {Vertexs} {Edges} "  , Clip_Graph $ Graph NoIDD hole hole)
                   ,("{Graph}", Clip_Graph hole)
                   ]

  arity (Graph _ x1 x2) = 2
  arity _                        = 0

  parseErr = ParseErrGraph

  hole = HoleGraph


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Vertex Document Node ClipDoc where
  select []    x                  = Clip_Vertex x
  select (0:p) (Vertex _ x1 x2 x3 x4) = select p x1
  select (1:p) (Vertex _ x1 x2 x3 x4) = select p x2
  select (2:p) (Vertex _ x1 x2 x3 x4) = select p x3
  select (3:p) (Vertex _ x1 x2 x3 x4) = select p x4
  select _     _                  = Clip_Nothing

  paste [] (Clip_Vertex c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Vertex")   x
  paste (0:p) c (Vertex i1 x1 x2 x3 x4) = Vertex i1 (paste p c x1) x2 x3 x4
  paste (1:p) c (Vertex i1 x1 x2 x3 x4) = Vertex i1 x1 (paste p c x2) x3 x4
  paste (2:p) c (Vertex i1 x1 x2 x3 x4) = Vertex i1 x1 x2 (paste p c x3) x4
  paste (3:p) c (Vertex i1 x1 x2 x3 x4) = Vertex i1 x1 x2 x3 (paste p c x4)
  paste _  _  x                    = x

  alternatives _ = [("Vertex {String_} {Int_} {Int_} {Int_} "  , Clip_Vertex $ Vertex NoIDD hole hole hole hole)
                   ,("{Vertex}", Clip_Vertex hole)
                   ]

  arity (Vertex _ x1 x2 x3 x4) = 4
  arity _                        = 0

  parseErr = ParseErrVertex

  hole = HoleVertex


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


instance Editable Edge Document Node ClipDoc where
  select []    x                  = Clip_Edge x
  select (0:p) (Edge _ x1 x2) = select p x1
  select (1:p) (Edge _ x1 x2) = select p x2
  select _     _                  = Clip_Nothing

  paste [] (Clip_Edge c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Edge")   x
  paste (0:p) c (Edge i1 x1 x2) = Edge i1 (paste p c x1) x2
  paste (1:p) c (Edge i1 x1 x2) = Edge i1 x1 (paste p c x2)
  paste _  _  x                    = x

  alternatives _ = [("Edge {Int_} {Int_} "  , Clip_Edge $ Edge NoIDD hole hole)
                   ,("{Edge}", Clip_Edge hole)
                   ]

  arity (Edge _ x1 x2) = 2
  arity _                        = 0

  parseErr = ParseErrEdge

  hole = HoleEdge


  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing
toConsList_Dummy [] = Nil_Dummy
toConsList_Dummy (x:xs) = Cons_Dummy x (toConsList_Dummy xs)

fromConsList_Dummy Nil_Dummy = []
fromConsList_Dummy (Cons_Dummy x xs) = x: fromConsList_Dummy xs

replaceList_Dummy _ x Nil_Dummy = Nil_Dummy -- replace beyond end of list
replaceList_Dummy 0 x (Cons_Dummy cx cxs) = Cons_Dummy x cxs
replaceList_Dummy n x (Cons_Dummy cx cxs) = Cons_Dummy cx (replaceList_Dummy (n-1) x cxs)

insertList_Dummy 0 x cxs = Cons_Dummy x cxs
insertList_Dummy _ x Nil_Dummy  = Nil_Dummy   -- insert beyond end of list
insertList_Dummy n x (Cons_Dummy cx cxs) = Cons_Dummy cx (insertList_Dummy (n-1) x cxs)

removeList_Dummy _ Nil_Dummy  = Nil_Dummy -- remove beyond end of list
removeList_Dummy 0 (Cons_Dummy cx cxs) = cxs
removeList_Dummy n (Cons_Dummy cx cxs) = Cons_Dummy cx (removeList_Dummy (n-1) cxs)

instance Editable List_Dummy Document Node ClipDoc where
  select []    x                  = Clip_List_Dummy x
  select (n:p) (List_Dummy _ cxs) = let xs = fromConsList_Dummy cxs
                                  in  if n < length xs 
                                      then select p (xs !! n)
                                      else Clip_Nothing
  select _     _                  = Clip_Nothing

  paste [] (Clip_List_Dummy c) _   = c
  paste [] c  x                  = trace ("Type error: pasting "++show c++" on List_Dummy")   x
  paste (n:p) c (List_Dummy i1 cxs) = let xs = fromConsList_Dummy cxs
                                    in  if n < length xs
                                        then let x  = xs!!n
                                                 x' = paste p c x
                                             in  List_Dummy i1 (replaceList_Dummy n x' cxs)
                                        else List_Dummy i1 cxs -- paste beyond end of list
  paste _  _  x                  = x

  alternatives _ = [("{List_Dummy}", Clip_List_Dummy hole)
                   ]

  arity (List_Dummy _ x1) = length (fromConsList_Dummy x1)
  arity _                        = 0

  parseErr = ParseErrList_Dummy

  hole = List_Dummy NoIDD Nil_Dummy

  isList _ = True

  insertList n (Clip_Dummy c) (List_Dummy idd cxs) = Clip_List_Dummy $ List_Dummy idd (insertList_Dummy n c cxs)
  insertList _ _             xs = trace "Type error, no paste" $ Clip_List_Dummy xs
  insertList _ c xs                 = Clip_List_Dummy xs

  removeList n (List_Dummy idd cxs) = Clip_List_Dummy $ List_Dummy idd (removeList_Dummy n cxs)
  removeList _ xs                        = Clip_List_Dummy $ xs

toConsList_Vertex [] = Nil_Vertex
toConsList_Vertex (x:xs) = Cons_Vertex x (toConsList_Vertex xs)

fromConsList_Vertex Nil_Vertex = []
fromConsList_Vertex (Cons_Vertex x xs) = x: fromConsList_Vertex xs

replaceList_Vertex _ x Nil_Vertex = Nil_Vertex -- replace beyond end of list
replaceList_Vertex 0 x (Cons_Vertex cx cxs) = Cons_Vertex x cxs
replaceList_Vertex n x (Cons_Vertex cx cxs) = Cons_Vertex cx (replaceList_Vertex (n-1) x cxs)

insertList_Vertex 0 x cxs = Cons_Vertex x cxs
insertList_Vertex _ x Nil_Vertex  = Nil_Vertex   -- insert beyond end of list
insertList_Vertex n x (Cons_Vertex cx cxs) = Cons_Vertex cx (insertList_Vertex (n-1) x cxs)

removeList_Vertex _ Nil_Vertex  = Nil_Vertex -- remove beyond end of list
removeList_Vertex 0 (Cons_Vertex cx cxs) = cxs
removeList_Vertex n (Cons_Vertex cx cxs) = Cons_Vertex cx (removeList_Vertex (n-1) cxs)

instance Editable List_Vertex Document Node ClipDoc where
  select []    x                  = Clip_List_Vertex x
  select (n:p) (List_Vertex _ cxs) = let xs = fromConsList_Vertex cxs
                                  in  if n < length xs 
                                      then select p (xs !! n)
                                      else Clip_Nothing
  select _     _                  = Clip_Nothing

  paste [] (Clip_List_Vertex c) _   = c
  paste [] c  x                  = trace ("Type error: pasting "++show c++" on List_Vertex")   x
  paste (n:p) c (List_Vertex i1 cxs) = let xs = fromConsList_Vertex cxs
                                    in  if n < length xs
                                        then let x  = xs!!n
                                                 x' = paste p c x
                                             in  List_Vertex i1 (replaceList_Vertex n x' cxs)
                                        else List_Vertex i1 cxs -- paste beyond end of list
  paste _  _  x                  = x

  alternatives _ = [("{List_Vertex}", Clip_List_Vertex hole)
                   ]

  arity (List_Vertex _ x1) = length (fromConsList_Vertex x1)
  arity _                        = 0

  parseErr = ParseErrList_Vertex

  hole = List_Vertex NoIDD Nil_Vertex

  isList _ = True

  insertList n (Clip_Vertex c) (List_Vertex idd cxs) = Clip_List_Vertex $ List_Vertex idd (insertList_Vertex n c cxs)
  insertList _ _             xs = trace "Type error, no paste" $ Clip_List_Vertex xs
  insertList _ c xs                 = Clip_List_Vertex xs

  removeList n (List_Vertex idd cxs) = Clip_List_Vertex $ List_Vertex idd (removeList_Vertex n cxs)
  removeList _ xs                        = Clip_List_Vertex $ xs

toConsList_Edge [] = Nil_Edge
toConsList_Edge (x:xs) = Cons_Edge x (toConsList_Edge xs)

fromConsList_Edge Nil_Edge = []
fromConsList_Edge (Cons_Edge x xs) = x: fromConsList_Edge xs

replaceList_Edge _ x Nil_Edge = Nil_Edge -- replace beyond end of list
replaceList_Edge 0 x (Cons_Edge cx cxs) = Cons_Edge x cxs
replaceList_Edge n x (Cons_Edge cx cxs) = Cons_Edge cx (replaceList_Edge (n-1) x cxs)

insertList_Edge 0 x cxs = Cons_Edge x cxs
insertList_Edge _ x Nil_Edge  = Nil_Edge   -- insert beyond end of list
insertList_Edge n x (Cons_Edge cx cxs) = Cons_Edge cx (insertList_Edge (n-1) x cxs)

removeList_Edge _ Nil_Edge  = Nil_Edge -- remove beyond end of list
removeList_Edge 0 (Cons_Edge cx cxs) = cxs
removeList_Edge n (Cons_Edge cx cxs) = Cons_Edge cx (removeList_Edge (n-1) cxs)

instance Editable List_Edge Document Node ClipDoc where
  select []    x                  = Clip_List_Edge x
  select (n:p) (List_Edge _ cxs) = let xs = fromConsList_Edge cxs
                                  in  if n < length xs 
                                      then select p (xs !! n)
                                      else Clip_Nothing
  select _     _                  = Clip_Nothing

  paste [] (Clip_List_Edge c) _   = c
  paste [] c  x                  = trace ("Type error: pasting "++show c++" on List_Edge")   x
  paste (n:p) c (List_Edge i1 cxs) = let xs = fromConsList_Edge cxs
                                    in  if n < length xs
                                        then let x  = xs!!n
                                                 x' = paste p c x
                                             in  List_Edge i1 (replaceList_Edge n x' cxs)
                                        else List_Edge i1 cxs -- paste beyond end of list
  paste _  _  x                  = x

  alternatives _ = [("{List_Edge}", Clip_List_Edge hole)
                   ]

  arity (List_Edge _ x1) = length (fromConsList_Edge x1)
  arity _                        = 0

  parseErr = ParseErrList_Edge

  hole = List_Edge NoIDD Nil_Edge

  isList _ = True

  insertList n (Clip_Edge c) (List_Edge idd cxs) = Clip_List_Edge $ List_Edge idd (insertList_Edge n c cxs)
  insertList _ _             xs = trace "Type error, no paste" $ Clip_List_Edge xs
  insertList _ c xs                 = Clip_List_Edge xs

  removeList n (List_Edge idd cxs) = Clip_List_Edge $ List_Edge idd (removeList_Edge n cxs)
  removeList _ xs                        = Clip_List_Edge $ xs

