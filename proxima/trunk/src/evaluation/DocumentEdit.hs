module DocumentEdit where

{-

Defines document edit (structure edit) functions


Observations:

Document root is different from the rest of the elements. It cannot be selected or deleted, so no hole is needed either.

Basic values should probably be boxed in some way, so there are holes for them.

-}

import CommonTypes
import DocTypes
import DocUtils
import PresTypes

import List
import Debug.Trace

class Editable a doc node clip | a -> doc node clip where
  select :: PathD -> a -> clip
  paste :: PathD -> clip -> a -> a
  alternatives :: a -> [ (String, clip) ]
  arity :: a -> Int
  parseErr :: node -> Presentation doc node clip -> a
  hole :: a
  isList :: a -> Bool
  insertList :: Int -> clip -> a -> clip
  removeList :: Int -> a -> clip
-- defaults for non-list types don't work because of ClipNothing constructor

class Clip clip where
  arityClip :: clip -> Int
  alternativesClip :: clip -> [ (String, clip) ]
  holeClip :: clip -> clip
  isListClip :: clip -> Bool
  insertListClip :: Int -> clip -> clip -> clip
  removeListClip :: Int -> clip -> clip


-- Generic part

  
navigateUpD :: FocusDoc -> document -> FocusDoc
navigateUpD NoPathD         _  = NoPathD
navigateUpD (PathD [])      _  = NoPathD
navigateUpD (PathD p@(_:_)) _  = PathD $ init p
navigateUpD pd              _ = pd

navigateDownD :: (Editable doc doc node clip, Clip clip) => FocusDoc -> doc -> FocusDoc
navigateDownD NoPathD   d = PathD []
navigateDownD (PathD p) d = PathD $ if arityD p d > 0 then p++[0] else p
 
navigateLeftD :: Editable doc doc node clip => FocusDoc -> doc -> FocusDoc
navigateLeftD NoPathD         _ = NoPathD
navigateLeftD (PathD p@(_:_)) d = PathD $ let i = last p
                                          in  if i > 0 then init p ++ [i-1] else p
navigateLeftD pd              _ = pd

navigateRightD :: (Editable doc doc node clip, Clip clip) => FocusDoc -> doc -> FocusDoc
navigateRightD NoPathD         _ = NoPathD
navigateRightD (PathD p@(_:_)) d = PathD $ let i = last p
                                               a = arityD (init p) d
                                           in  if i < a-1 then init p ++ [i+1] else p
navigateRightD pd              _ = pd




editCopyD :: Editable doc doc node clip => DocumentLevel doc clip -> DocumentLevel doc clip
editCopyD (DocumentLevel doc NoPathD clip)        = DocumentLevel doc NoPathD clip
editCopyD (DocumentLevel doc pd@(PathD pth) clip) = DocumentLevel doc pd (selectD pth doc)

editCutD :: (Editable doc doc node clip, Clip clip) => DocumentLevel doc clip -> DocumentLevel doc clip
editCutD  (DocumentLevel doc NoPathD clip)           = DocumentLevel doc NoPathD clip
editCutD  (DocumentLevel doc pd@(PathD pth) clip)    = let (doc', pd') = deleteD pth doc
                                                       in  DocumentLevel doc' pd' (selectD pth doc)

editDeleteD :: (Editable doc doc node clip, Clip clip) => DocumentLevel doc clip -> DocumentLevel doc clip
editDeleteD (DocumentLevel doc NoPathD clip)        = DocumentLevel doc NoPathD clip
editDeleteD (DocumentLevel doc pd@(PathD pth) clip) =  let (doc', pd') = deleteD pth doc
                                                       in  DocumentLevel doc' pd' clip

editPasteD :: Editable doc doc node clip => DocumentLevel doc clip -> DocumentLevel doc clip
editPasteD (DocumentLevel doc NoPathD clip)         = DocumentLevel doc NoPathD clip
editPasteD (DocumentLevel doc pd@(PathD pth) clip)  = DocumentLevel (pasteD pth clip doc) pd clip

-- menuD is probably not a good name
menuD :: (Editable doc doc node clip, Clip clip) => PathDoc -> doc -> [ (String, DocumentLevel doc clip -> DocumentLevel doc clip) ]
menuD NoPathD _              = []
menuD path@(PathD p) d =
  let alts = alternativesD p d
      mkItem (s,c) = (s, \(DocumentLevel _ pth clip) -> DocumentLevel (pasteD p c d) pth clip)
  in  [ ("<cut>", \(DocumentLevel d _ clip) -> let (d',p') = deleteD p d 
                                               in  DocumentLevel d' p' (selectD p d) ) 
      , ("<copy>", \(DocumentLevel d _ clip) -> DocumentLevel d path (selectD p d) ) 
      , ("<paste>", \(DocumentLevel d _ clip) -> DocumentLevel (pasteD p clip d) path clip )
      , ("<select>", \(DocumentLevel d _ clip) -> DocumentLevel d path clip ) ]
      ++ map mkItem alts   -- use this one or the one in the argument? They should be the same
      ++ if null p then [] else
           let parent = (selectD (init p) d)
           in if not (isListClip parent) then [] else
                let alts2 = [ ("add "++s, insertListClip (last p+1) c parent) | (s,c) <- alts]
                    mkItem2 (s,c) = (s, \(DocumentLevel _ pth clip) -> DocumentLevel (pasteD (init p) c d) pth clip)
                    pasteBefore = ("<paste before>", \(DocumentLevel _ pth clip) -> 
                                                     DocumentLevel (pasteD (init p) (insertListClip (last p) clip parent) d) pth clip )
                    pasteAfter = ("<paste after>", \(DocumentLevel _ pth clip) -> 
                                                     DocumentLevel (pasteD (init p) (insertListClip (last p+1) clip parent) d) pth clip )
                in  map mkItem2 alts2 ++ [pasteBefore,pasteAfter]


selectD :: Editable doc doc node clip => PathD -> doc -> clip
selectD p doc = select p doc

pasteD :: Editable doc doc node clip => PathD -> clip -> doc -> doc
pasteD p c doc = paste p c doc

-- ugly mix of levels, find out how to do it nicely
deleteD :: (Editable doc doc node clip, Clip clip) => PathD -> doc -> (doc, PathDoc)
deleteD p d = if not (null p) && isListClip (selectD (init p) d) -- if parent is list, then delete is remove from list
              then (pasteD (init p) (removeListClip (last p) (selectD (init p) d)) d, NoPathD)
              else let newhole = holeClip (selectD p d)
                   in  (pasteD p newhole d, PathD p) 

arityD :: (Editable doc doc node clip, Clip clip) => PathD -> doc -> Int
arityD p d = arityClip (select p d)

alternativesD :: (Editable doc doc node clip, Clip clip) => PathD -> doc -> [ (String, clip) ]
alternativesD p d = alternativesClip (select p d)

 
