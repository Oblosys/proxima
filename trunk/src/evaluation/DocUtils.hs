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




boolVal :: Bool_ -> Bool -- unbox, for non-AG functions (AG functions use @<child>.boolVal)
boolVal (Bool_ _ bool) = bool
boolVal _              = False


-- separate module for the Editable class?


initBoard = Board NoIDD (backRow (Bool_ NoIDD True)) (pawnRow (Bool_ NoIDD True)) emptyRow emptyRow emptyRow emptyRow (pawnRow (Bool_ NoIDD False)) (backRow (Bool_ NoIDD False))

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
                                 , HeliumItem NoIDD (DivExp NoIDD NoIDP (IntExp NoIDD NoIDP (Int_ NoIDD 1))
                                                                        (IntExp NoIDD NoIDP (Int_ NoIDD 2)))
                                 , ListItem NoIDD listItem
                                 ]
       , Slide NoIDD (mkString_ "slide_2") $
          ItemList NoIDD (Alpha NoIDD) $  List_Item NoIDD $
                         mkItems [ StringItem NoIDD (mkString_ "item_1")
                                 ]
      ]
 where listItem = ItemList NoIDD (Number NoIDD) $  List_Item NoIDD $
                    mkItems [ StringItem NoIDD (mkString_ "nested_item_1")
                            , StringItem NoIDD (mkString_ "nested_item_2")
                            ]

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

