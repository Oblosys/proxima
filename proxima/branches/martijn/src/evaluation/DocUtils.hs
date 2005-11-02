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


-- separate module for the Editable class?


initBoard = Board NoIDD (backRow True) (pawnRow True) emptyRow emptyRow emptyRow emptyRow (pawnRow False) (backRow False)

emptyBoard = Board NoIDD emptyRow emptyRow emptyRow emptyRow emptyRow emptyRow emptyRow emptyRow

emptyRow = BoardRow NoIDD Empty Empty Empty Empty Empty Empty Empty Empty

pawnRow c = BoardRow NoIDD (Pawn NoIDD c) (Pawn NoIDD c) (Pawn NoIDD c) (Pawn NoIDD c) 
                          (Pawn NoIDD c) (Pawn NoIDD c) (Pawn NoIDD c) (Pawn NoIDD c)
backRow c = BoardRow NoIDD (Rook NoIDD c) (Knight NoIDD c) (Bishop NoIDD c) (Queen NoIDD c) 
                          (King NoIDD c) (Bishop NoIDD c) (Knight NoIDD c) (Rook NoIDD c)




initPPPresentation = 
  PPPresentation NoIDD True $
    mkSlides
      [ Slide NoIDD (String_ NoIDD "slide_1") $
          ItemList NoIDD (Bullet NoIDD) $ 
                         mkItems [ StringItem NoIDD (String_ NoIDD "item_1")
                                 , HeliumItem NoIDD (DivExp NoIDD NoIDP (IntExp NoIDD NoIDP 1)(IntExp NoIDD NoIDP 2))
                                 , ListItem NoIDD listItem
                                 ]
       , Slide NoIDD (String_ NoIDD "slide_2") $
          ItemList NoIDD (Alpha NoIDD) $ 
                         mkItems [ StringItem NoIDD (String_ NoIDD "item_1")
                                 ]
      ]
 where listItem = ItemList NoIDD (Number NoIDD) $ 
                    mkItems [ StringItem NoIDD (String_ NoIDD "nested_item_1")
                            , StringItem NoIDD (String_ NoIDD "nested_item_2")
                            ]
                                  
mkSlides []     = NilSlides NoIDD
mkSlides (s:ss) = ConsSlides NoIDD s (mkSlides ss)

mkItems []     = NilItems NoIDD
mkItems (s:ss) = ConsItems NoIDD s (mkItems ss)







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




