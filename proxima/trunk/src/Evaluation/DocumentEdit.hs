module Evaluation.DocumentEdit where

{-

Defines document edit (structure edit) functions

-}

import Common.CommonTypes
import Evaluation.DocTypes
import Evaluation.DocUtils
import Presentation.PresTypes

import List
import Debug.Trace

class Editable a doc node clip token | a -> doc node clip token where
  select :: Path -> a -> clip
  paste :: Path -> clip -> a -> a
  alternatives :: a -> [ (String, clip) ]
  arity :: a -> Int
  parseErr :: Presentation doc node clip token -> a
  hole :: a
  holeNodeConstr :: (a -> Path -> node) -- for automatic hole parsing in PresentationParsing.pStr
  isList :: a -> Bool
  insertList :: Int -> clip -> a -> clip
  removeList :: Int -> a -> clip

class Clip clip where
  arityClip :: clip -> Int
  alternativesClip :: clip -> [ (String, clip) ]
  holeClip :: clip -> clip
  isListClip :: clip -> Bool
  insertListClip :: Int -> clip -> clip -> clip
  removeListClip :: Int -> clip -> clip
  
navigateUpD :: FocusDoc -> document -> FocusDoc
navigateUpD NoPathD         _  = NoPathD
navigateUpD (PathD [])      _  = NoPathD
navigateUpD (PathD p@(_:_)) _  = PathD $ init p
navigateUpD pd              _ = pd

navigateDownD :: (Editable doc doc node clip token, Clip clip) => FocusDoc -> doc -> FocusDoc
navigateDownD NoPathD   d = PathD []
navigateDownD (PathD p) d = PathD $ if arityD p d > 0 then p++[0] else p
 
navigateLeftD :: Editable doc doc node clip token => FocusDoc -> doc -> FocusDoc
navigateLeftD NoPathD         _ = NoPathD
navigateLeftD (PathD p@(_:_)) d = PathD $ let i = last p
                                          in  if i > 0 then init p ++ [i-1] else p
navigateLeftD pd              _ = pd

navigateRightD :: (Editable doc doc node clip token, Clip clip) => FocusDoc -> doc -> FocusDoc
navigateRightD NoPathD         _ = NoPathD
navigateRightD (PathD p@(_:_)) d = PathD $ let i = last p
                                               a = arityD (init p) d
                                           in  if i < a-1 then init p ++ [i+1] else p
navigateRightD pd              _ = pd




editCopyD :: Editable doc doc node clip token => DocumentLevel doc clip -> DocumentLevel doc clip
editCopyD (DocumentLevel doc NoPathD clip)        = DocumentLevel doc NoPathD clip
editCopyD (DocumentLevel doc pd@(PathD pth) clip) = DocumentLevel doc pd (selectD pth doc)

editCutD :: (Editable doc doc node clip token, Clip clip) => DocumentLevel doc clip -> DocumentLevel doc clip
editCutD  (DocumentLevel doc NoPathD clip)           = DocumentLevel doc NoPathD clip
editCutD  (DocumentLevel doc pd@(PathD pth) clip)    = let (doc', pd') = deleteD pth doc
                                                       in  DocumentLevel doc' pd' (selectD pth doc)

editDeleteD :: (Editable doc doc node clip token, Clip clip) => DocumentLevel doc clip -> DocumentLevel doc clip
editDeleteD (DocumentLevel doc NoPathD clip)        = DocumentLevel doc NoPathD clip
editDeleteD (DocumentLevel doc pd@(PathD pth) clip) =  let (doc', pd') = deleteD pth doc
                                                       in  DocumentLevel doc' pd' clip

editPasteD :: Editable doc doc node clip token => DocumentLevel doc clip -> DocumentLevel doc clip
editPasteD (DocumentLevel doc NoPathD clip)         = DocumentLevel doc NoPathD clip
editPasteD (DocumentLevel doc pd@(PathD pth) clip)  = DocumentLevel (pasteD pth clip doc) pd clip


-- menuD is probably not a good name
menuD :: (Editable doc doc node clip token, Clip clip) => PathDoc -> doc -> [ (String, DocumentLevel doc clip -> DocumentLevel doc clip) ]
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


selectD :: Editable doc doc node clip token => Path -> doc -> clip
selectD p doc = select p doc

pasteD :: Editable doc doc node clip token => Path -> clip -> doc -> doc
pasteD p c doc = paste p c doc

insertListD :: (Show clip,Clip clip, Editable doc doc node clip token) => Path -> Int -> clip -> doc -> doc
insertListD path index clip doc = 
  let list = selectD path doc
  in  if isListClip list
      then if index <= arityClip list 
           then pasteD path (insertListClip index clip list) doc
           else debug Err ("DocumentEdit.insertBeforeD beyond end of list "++show path) $ doc
      else debug Err ("DocumentEdit.insertBeforeD on non-list at "++show path++show clip) $ doc

-- ugly mix of levels, find out how to do it nicely
deleteD :: (Editable doc doc node clip token, Clip clip) => Path -> doc -> (doc, PathDoc)
deleteD p d = if not (null p) && isListClip (selectD (init p) d) -- if parent is list, then delete is remove from list
              then (pasteD (init p) (removeListClip (last p) (selectD (init p) d)) d, NoPathD)
              else let newhole = holeClip (selectD p d)
                   in  (pasteD p newhole d, PathD p) 

arityD :: (Editable doc doc node clip token, Clip clip) => Path -> doc -> Int
arityD p d = arityClip (select p d)

alternativesD :: (Editable doc doc node clip token, Clip clip) => Path -> doc -> [ (String, clip) ]
alternativesD p d = alternativesClip (select p d)

 
