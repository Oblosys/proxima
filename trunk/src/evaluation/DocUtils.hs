module DocUtils ( module DocUtils_Generated
                , module DocUtils           ) where

import DocUtils_Generated

import DocTypes
import PresTypes

-- mapping info is directly in Document, so doc depends on presentation. Only for now.
-- First ID is document-level ID for node. The rest are presentation ID's


redirect (SkipDoc i)     = (SkipDoc' i)
redirect (SetDoc doc {- inssdels -})    = (SetDoc' doc {- inssdels -})
--redirect InitDoc         = (SetDoc' initDoc) -- is done in translate
redirect (UpdateDoc upd) = UpdateDoc' upd
redirect NavUpDoc        = NavUpDoc'
redirect NavDownDoc      = NavDownDoc'
redirect NavLeftDoc      = NavLeftDoc'
redirect NavRightDoc     = NavRightDoc'
redirect CutDoc          = CutDoc'
redirect CopyDoc         = CopyDoc'
redirect PasteDoc        = PasteDoc'
redirect DeleteDoc       = DeleteDoc'
redirect EvaluateDoc     = EvaluateDoc'
redirect _               = (SkipDoc' 0)



-- obsolete, use bool_
boolVal :: Bool_ -> Bool -- unbox, for non-AG functions (AG functions use @<child>.boolVal)
boolVal (Bool_ _ bool) = bool
boolVal _              = False

---- deconstructors for boxed primitive types

string_ :: String_ -> String
string_ (String_ _ str) = str
string_ _ = ""

bool_ :: Bool_ -> Bool
bool_ (Bool_ _ b) = b
bool_ _ = False

int_ :: Int_ -> Int
int_ (Int_ _ i) = i
int_ _ = 0

---- constructors for boxed primitive types
-- fix name clash in ProxParser 
--mkString_ :: String -> String_
--mkString_ str = String_ NoIDD str

--mkBool_ :: Bool -> Bool_
--mkBool_ b = Bool_ NoIDD b

--mkInt_ :: Int -> Int_
--mkInt_ i = Int_ NoIDD i

----

-- separate module for the Editable class?


initBoard = demoBoard -- Board NoIDD (backRow (Bool_ NoIDD True)) (pawnRow (Bool_ NoIDD True)) emptyRow emptyRow emptyRow emptyRow (pawnRow (Bool_ NoIDD False)) (backRow (Bool_ NoIDD False))

demoBoard = Board NoIDD r8 r7 r6 r5 r4 r3 r2 r1
 where r1 = BoardRow NoIDD e  e  bb e  e  e  br bk
       r2 = BoardRow NoIDD e  e  e  e  bn bp e  bp
       r3 = BoardRow NoIDD bp e  e  e  e  e  e  wp
       r4 = BoardRow NoIDD e  e  br e  bq bp wq wr
       r5 = BoardRow NoIDD e  bp wp e  e  e  e  e
       r6 = BoardRow NoIDD e  e  e  e  wn e  e  e
       r7 = BoardRow NoIDD wp wp wb e  e  wp wp e
       r8 = BoardRow NoIDD e  wk e  wr e  e  e  e
       [e,wp,wr,wn,wb,wq,wk,bp,br,bn,bb,bq,bk] = Empty : pieces (Bool_ NoIDD True) ++ pieces (Bool_ NoIDD False)
       pieces c = [Pawn NoIDD c, Rook NoIDD c, Knight NoIDD c, Bishop NoIDD c, Queen NoIDD c, King NoIDD c]
      
  
-- Kasparov,G - Lautier,J Moscow, 1995 1.Ng4!! Qe6 [1...Rxg5 2.Nxe5 Rxh5 3.Rd8+ Ng8 4.Nxf7#] 2.Rd8 Qg6 3.Qxe7 1-0

emptyBoard = Board NoIDD emptyRow emptyRow emptyRow emptyRow emptyRow emptyRow emptyRow emptyRow

emptyRow = BoardRow NoIDD Empty Empty Empty Empty Empty Empty Empty Empty

pawnRow c = BoardRow NoIDD (Pawn NoIDD c) (Pawn NoIDD c) (Pawn NoIDD c) (Pawn NoIDD c) 
                          (Pawn NoIDD c) (Pawn NoIDD c) (Pawn NoIDD c) (Pawn NoIDD c)
backRow c = BoardRow NoIDD (Rook NoIDD c) (Knight NoIDD c) (Bishop NoIDD c) (Queen NoIDD c) 
                          (King NoIDD c) (Bishop NoIDD c) (Knight NoIDD c) (Rook NoIDD c)




initPPPresentation = 
  PPPresentation NoIDD (Bool_ NoIDD True) $ List_Slide NoIDD $
    mkSlides
      [ Slide NoIDD (mkString_ "slide_1") $
          ItemList NoIDD (Bullet NoIDD) $ List_Item NoIDD $
                         mkItems [ StringItem NoIDD (mkString_ "item_1")
                                 , HeliumItem NoIDD -- simple trick to use parser: Needs an additional parse (F1) though!
                                     --(ident "\\ x -> increaze x")
                                     (ident "\\b -> \\x -> if b then ink x else x")
                                 , StringItem NoIDD (mkString_ "item_2")
                                 , ListItem NoIDD listItem
                                 ]
       , Slide NoIDD (mkString_ "slide_2") $
          ItemList NoIDD (Alpha NoIDD) $  List_Item NoIDD $
                         mkItems [ StringItem NoIDD (mkString_ "item_a")
                                 , StringItem NoIDD (mkString_ "item_b")
                                 , StringItem NoIDD (mkString_ "item_c")
                                 ]
      ]
 where listItem = ItemList NoIDD (Number NoIDD) $  List_Item NoIDD $
                    mkItems [ StringItem NoIDD (mkString_ "nested_item_1")
                            , ListItem NoIDD listItem'
                            , StringItem NoIDD (mkString_ "nested_item_2")
                            , StringItem NoIDD (mkString_ "nested_item_3")
                            ]
       listItem' = ItemList NoIDD (Bullet NoIDD) $  List_Item NoIDD $
                    mkItems [ StringItem NoIDD (mkString_ "nested_nested_item")
                            , StringItem NoIDD (mkString_ "nested_nested_item")
                            , StringItem NoIDD (mkString_ "nested_nested_item")
                            ]
       dv e1 e2 = DivExp NoIDD NoIDP e1 e2 
       lam str body = LamExp NoIDD NoIDP NoIDP (Ident NoIDD NoIDP NoIDP (mkString_ str)) body
       ifxp c t e = IfExp NoIDD NoIDP NoIDP NoIDP c t e 
       int i = IntExp NoIDD NoIDP (Int_ NoIDD i)
       bool b = BoolExp NoIDD NoIDP (Bool_ NoIDD b)
       ident str = IdentExp NoIDD (Ident NoIDD NoIDP NoIDP (mkString_ str))
       

       mkString_ str = String_ NoIDD str

mkSlides []     = Nil_Slide
mkSlides (s:ss) = Cons_Slide s (mkSlides ss)

mkItems []     = Nil_Item 
mkItems (s:ss) = Cons_Item s (mkItems ss)





{-

class Editable a where
  select :: PathD -> a -> ClipDoc
  paste :: PathD -> ClipDoc -> a -> a
  alternatives :: a -> [ (String, ClipDoc) ] -- a is dummy arg
  arity :: a -> Int
  hole :: a
  isList :: a -> Bool
  isList _ = False
  
  insertList :: Int -> ClipDoc -> a -> ClipDoc
  insertList _ _ _ = Clip_Nothing
  removeList :: Int -> a -> ClipDoc
  removeList _ _ = Clip_Nothing


-}

