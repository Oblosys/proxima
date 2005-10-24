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

