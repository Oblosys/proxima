module LayTranslate where

import CommonTypes
import LayLayerTypes
import LayLayerUtils

import Scanner

import TreeEditPres



--translateIO :: state -> low -> high -> editLow -> IO (editHigh, state, low)
translateIO :: LayerStateLay -> LayoutLevel -> PresentationLevel -> EditLayout
            -> IO (EditPresentation, LayerStateLay, LayoutLevel)
translateIO state low high editLow =
  do { (editHigh, state', low') <- parseIO state low high editLow
     ; debugLnIO Prs $ "Edit Layout: "++show editLow
     ; return (editHigh, state', low')
     }


-- one extra indirection because separate cases make it hard to do debugging on result



--------------- Parser


-- split in monadic and non-monadic part
parseIO :: LayerStateLay -> LayoutLevel -> PresentationLevel -> EditLayout -> IO (EditPresentation, LayerStateLay, LayoutLevel)
--parseIO state layLvl prs (OpenFileLay str) = openFile str state layLvl prs
--parseIO state layLvl prs (SaveFileLay str) = setUpd NothingUpdated $ saveFile state layLvl prs str 
--parseIO state layLvl prs (DocumentLoadedLay str) =  return $ editLay (editInsert 'X') state layLvl prs
parseIO state layLvl prs (OpenFileLay str) = return (OpenFilePres str, state, layLvl)
parseIO state layLvl prs (SaveFileLay str) = return (SaveFilePres str, state, layLvl)
parseIO state layLvl prs event = return $ parse state layLvl prs event

parse :: LayerStateLay -> LayoutLevel -> PresentationLevel -> EditLayout -> (EditPresentation, LayerStateLay, LayoutLevel)
parse state layLvl@(LayoutLevel pres _ dt) prs (SetFocusLay focus) = 
  setUpd NothingUpdated $ (SkipPres 0, state, LayoutLevel pres focus dt)
parse state layLvl prs (SkipLay i)   = (SkipPres (i+1), state, layLvl)
parse state layLvl prs InitLay       = (InitPres, state, layLvl)
parse state layLvl prs (InsertLay c) = editLay (editInsert c) state layLvl prs
parse state layLvl prs CutLay   = editLay editCut state layLvl prs
parse state layLvl prs CopyLay   = setUpd NothingUpdated $ editCopy state layLvl prs
parse state layLvl prs PasteLay  = editLay editPaste state layLvl prs
parse state layLvl prs DeleteLay = editLay editDelete state layLvl prs 
parse state layLvl prs SplitLay  = editLay editSplit state layLvl prs
parse state layLvl prs LeftDeleteLay = editLay editLeftDelete state layLvl prs
parse state layLvl prs RightDeleteLay = editLay editRightDelete state layLvl prs
parse state layLvl prs LeftLay   = setUpd NothingUpdated $ navigateLeft state layLvl prs 
parse state layLvl prs RightLay  = setUpd NothingUpdated $ navigateRight state layLvl prs

parse state layLvl prs EnlargeLeftLay   = setUpd NothingUpdated $ enlargeLeft state layLvl prs
parse state layLvl prs EnlargeRightLay  = setUpd NothingUpdated $ enlargeRight state layLvl prs

parse state layLvl prs NormalizeLay       = setUpd NothingUpdated $ editLay editNormalize state layLvl prs  

parse state layLvl prs TestLay            = tokenizeLay state layLvl prs
parse state layLvl prs Test2Lay           = (Test2Pres, state, layLvl)


parse state layLvl prs (UpdateDocLay upd) = (UpdateDocPres upd, state, layLvl)
parse state layLvl prs NavUpDocLay        = (NavUpDocPres, state, layLvl)
parse state layLvl prs NavDownDocLay      = (NavDownDocPres, state, layLvl)
parse state layLvl prs NavLeftDocLay      = (NavLeftDocPres, state, layLvl)
parse state layLvl prs NavRightDocLay     = (NavRightDocPres, state, layLvl)
parse state layLvl prs CutDocLay          = (CutDocPres, state, layLvl)
parse state layLvl prs CopyDocLay         = (CopyDocPres, state, layLvl)
parse state layLvl prs PasteDocLay        = (PasteDocPres, state, layLvl)
parse state layLvl prs DeleteDocLay       = (DeleteDocPres, state, layLvl)
parse state layLvl prs Test2Lay           = (Test2Pres, state, layLvl)
-- We want to be able to set the presentation here and probably do a Layout to Presentation mapping.
-- Not possible with just single edit commands. The problem is that the parser must not always be called. This
-- does not readily fit in the current model.
-- We can fix it by not setting the higher pres level if we don't want a parse.


{-parse state layLvl prs Test2Lay   = setUpd AllUpdated $editReadFile state layLvl prs focus 
--parse state layLvl prs (MouseDownLay path ms i) = setUpd AllUpdated $ editMouseDown state layLvl prs path -- Helium
-- to allow presenter mouse handle: change GestureInterpreter, so the event is handled there
-}
parse state layLvl prs _            = (SkipPres 0, state, layLvl)


-- edit ops need to be consistent, when navigating with non-empty focus, collapse focus
-- when inserting with non-empty focus, first delete

-- edit ops that actually change the presentation tree should be a separate type because now we have multiple 
-- functions or lose type safety  (?)


-- doc and/or presentation need some way to say whether document parts are parsed. Now With nodes pile up on the 
-- unparsed presentation.


tokenizeLay state layLvl@(LayoutLevel pres focus dt) (PresentationLevel _ (layout, idCounter, inserted, deleted)) = 
 let (pres', layout', idCounter') = tokenize idCounter Nothing pres
     pres''                       = deleteInsertedTokens inserted pres'
     presLvl'                     = debug Err ("layTranslate: inss="++show inserted) $ PresentationLevel pres'' (layout',idCounter', inserted, deleted)
 in  setUpd AllUpdated $ (SetPres presLvl', state, layLvl) --LayoutLevel (markUnparsed pres') (markUnparsedF pres' focus'))




-- if focus is valid, apply editF to the presentation, and try to reparse the presentation 
--editLay :: 
--            Presentation -> Presentation -> LayoutLevel -> FocusPres -> (EditPresentation, Presentation, Presentation)

editLay editF state layLvl@(LayoutLevel pres NoFocusP dt) presLvl = (SkipPres 0, state, layLvl)
editLay editF state (LayoutLevel pres focus dt) (PresentationLevel _ (layout, idCounter, inserted, deleted)) = 
 let (ll@(LayoutLevel pres' focus' dt), state') = editF state (LayoutLevel pres focus dt) -- this will be layLvl's own focus
     (pres'', focus'')             = (markUnparsed pres', markUnparsedF pres' focus')
   --  (pres''', layout', idCounter') = tokenize idCounter Nothing pres''
   -- ******** don't forget to delete inserted tokens! when uncommenting this tokenize
   --  presLvl'                      = PresentationLevel pres''' (layout', idCounter')
-- in  setUpd AllUpdated $ (SetPres presLvl', state', ll)
     diffTree = diffPres pres'' pres
 in --        _                                    -> return ()  -- where did this line come from?
     setUpd AllUpdated $ (SkipPres 0, state', LayoutLevel pres'' focus'' diffTree)


-- should we make a similar function for edit ops that do not alter the presentation? This function would not do
-- much, except setting the update region, getting rid of the document argument and returning a SkipPres 0,
-- Also some edit ops change the focus, whereas others only change the clip, or do IO.  Different functions?
-- They differ also in handling an empty focus. copy and navigate can't handle empty focus, but save would work fine
-- What is a nice abstraction here?

-- replace the current layout by pres, focus is reset


-- replace the current layout by pres, focus is reset
editSet :: Presentation -> Presentation -> LayoutLevel -> (LayoutLevel, Presentation)
editSet pres' clip (LayoutLevel pres focus@(FocusP f t) dt) = (LayoutLevel pres' NoFocusP dt, clip)

openFile :: String -> Presentation -> LayoutLevel -> PresentationLevel -> IO (EditPresentation, Presentation, LayoutLevel)
openFile filePath clip layLvl prs =
 do { debugLnIO Lay $ "Opening file: "++filePath
    ; str <- readFile filePath
    ; let pres' = StringP NoIDP filePath
    ; return $ editLay (editSet pres') clip layLvl prs
    }
    
editInsert :: Char -> Presentation -> LayoutLevel -> (LayoutLevel, Presentation)
editInsert c clip (LayoutLevel pres focus@(FocusP f t) dt) = 
  let (pres', focus')  = if f==t then (pres,focus) else deleteTree focus pres
      (pres'',focus'') = pasteTree (fromP focus') (text [c]) pres'
  in  (LayoutLevel pres'' focus'' dt, clip)

editCut :: Presentation -> LayoutLevel -> (LayoutLevel, Presentation)
editCut clip (LayoutLevel pres focus dt) = 
  let clip' = copyTree focus clip pres                                                                                              
      (pres', focus') = deleteTree focus pres
  in  (LayoutLevel pres' focus' dt, clip')

editCopy :: Presentation -> LayoutLevel -> PresentationLevel -> (EditPresentation, Presentation, LayoutLevel)
editCopy clip layLvl@(LayoutLevel pres NoFocusP dt) doc = (SkipPres 0, clip, layLvl)
editCopy clip layLvl@(LayoutLevel pres focus dt)    doc = 
  let clip' = copyTree focus clip pres                                                                     
  in  (SkipPres 0, clip', (LayoutLevel pres focus dt))   -- set the pres focus to the one coming from the arranger, see focus discussion

editPaste clip (LayoutLevel pres focus@(FocusP f t) dt) = 
  let (pres', focus') = if f==t then (pres,focus) else deleteTree focus pres
      (pres'', focus'') = pasteTree (fromP focus') clip pres'                                                                                          
  in  (LayoutLevel pres'' focus'' dt, clip)

editDelete clip (LayoutLevel pres focus dt) = 
  let (pres', focus') = deleteTree focus pres
  in  (LayoutLevel pres' focus' dt, clip)

editSplit clip (LayoutLevel pres focus dt) = 
  let (pres', focus')   = deleteTree focus pres
      (pres'', focus'') = splitRowTree (fromP focus') pres'
  in (LayoutLevel pres'' focus'' dt, clip)



-- only for column of rows:
editNormalize :: Presentation -> LayoutLevel -> (LayoutLevel, Presentation)
editNormalize clip (LayoutLevel pres focus dt) = 
 let (pres', focus') = normalizePresentation pres focus
 in  (LayoutLevel pres' focus' dt, clip)



-- unlike paste split and insert, left and right delete do not perform their edit command when the focus was non-empty
-- ie. in that case, they are interpreted as a regular delete
editLeftDelete clip layLvl@(LayoutLevel pres focus@(FocusP f t) dt) =
  if f /= t then editDelete clip layLvl else
    let focus'          = navigateLeftTreePres (toP focus) pres
        focus''         = FocusP (toP focus) (toP focus')
        (pres', focus''') = deleteTree focus'' pres
    in (LayoutLevel pres' focus''' dt, clip)

editRightDelete clip layLvl@(LayoutLevel pres focus@(FocusP f t) dt) = 
  if f /= t then editDelete clip layLvl else
    let focus'          = navigateRightTreePres (toP focus) pres
        focus''         = FocusP (toP focus) (toP focus')
        (pres', focus''') = deleteTree focus'' pres
    in  (LayoutLevel pres' focus''' dt, clip)



navigateLeft :: Presentation -> LayoutLevel -> PresentationLevel -> (EditPresentation, Presentation, LayoutLevel)
navigateLeft clip layLvl@(LayoutLevel pres NoFocusP dt) doc = (SkipPres 0, clip, layLvl)
navigateLeft clip (LayoutLevel pres focus dt) doc =
  let  focus' = navigateLeftTreePres (toP focus) pres
  in  (SkipPres 0, clip, LayoutLevel pres focus' dt)

navigateRight :: Presentation -> LayoutLevel -> PresentationLevel -> (EditPresentation, Presentation, LayoutLevel)
navigateRight clip layLvl@(LayoutLevel pres NoFocusP dt) doc = (SkipPres 0, clip, layLvl)
navigateRight clip (LayoutLevel pres focus dt) doc = 
  let  focus' = navigateRightTreePres (toP focus) pres
  in  (SkipPres 0, clip, LayoutLevel pres focus' dt)

enlargeLeft :: Presentation -> LayoutLevel -> PresentationLevel -> (EditPresentation, Presentation, LayoutLevel)
enlargeLeft clip layLvl@(LayoutLevel pres NoFocusP dt) doc = (SkipPres 0, clip, layLvl)
enlargeLeft clip (LayoutLevel pres focus dt) doc =
  let  focus' = navigateLeftTreePres (toP focus) pres
       focus'' = FocusP (fromP focus) (fromP focus')
  in  (SkipPres 0, clip, LayoutLevel pres focus'' dt)

enlargeRight :: Presentation -> LayoutLevel -> PresentationLevel -> (EditPresentation, Presentation, LayoutLevel)
enlargeRight clip layLvl@(LayoutLevel pres NoFocusP dt) doc = (SkipPres 0, clip, layLvl)
enlargeRight clip (LayoutLevel pres focus dt) doc = 
  let  focus' = navigateRightTreePres (toP focus) pres
       focus'' = FocusP (fromP focus) (fromP focus')
  in  (SkipPres 0, clip, LayoutLevel pres focus'' dt)



{-
openFile :: Presentation -> LayoutLevel -> PresentationLevel -> FilePath -> IO (EditPresentation, Presentation, LayoutLevel) 
openFile clip layLvl doc filePath =
 do { debugLnIO Prs "Opening file"
    ; str <- readFile filePath
    ; let (doc', layLvl', clip') = read str
    ; return (SetPres doc', clip', layLvl')
    }

saveFile :: Presentation -> LayoutLevel -> PresentationLevel -> FilePath -> IO (EditPresentation, Presentation, LayoutLevel)
saveFile clip layLvl doc filePath =
 do { debugLnIO Prs "Saving file"
   -- ; let (src, errsStr) =  span (\l -> not (isPrefixOf "#######" l)) . lines . stringFromPres $ pres
   -- ; let str = unlines src
    ; writeFile filePath $ show (doc, layLvl, clip)  -- can't show pres, because of with nodes
    ; return (SkipPres 0, clip, layLvl)
    }

-}
{-

Focus:

Focus is not completely right yet. Instead of passing the focus from gest int. to presenter for every edit, all levels
should keep a focus. This will allow the nice up/down navigation.

Also, a skip doc, operation will reinstall the old pres focus, which is not right if an up down has been performed on
arrangement focus. There is a choice here. Either every level has correct focus, or a skip does lead to a focus update
on the lower level. In the last case, the focus moves up and down only when required. This might lead to some difficult
administration though.

Probably everything will be ok, once we have Level types on all levels, then the passed datastructure will always contain
the current focus, and it is passed only when the lower level cannot handel it.

When we stay at lower levels, the focus will not be correct at the higher levels, this is similar edit ops that are 
short cut at lower levels. Maybe we need a distinction though. An edit on the pres tree without a reparse should signal
that the document is not consistent with the presentation yet, but if only the focus has changed, we don't want that signal.

The difference we have here might be that the focus updates are LS updates.

For now, we fix it by having edit ops that do a skip doc, also set the presentation focus.


edit, compute focus after edit, compute focus in terms of position in string repr.

present: recomputes focus in updated tree from string position except for skip, because then focus is ok.

arrange: translate focusP to focusA

structural problem: if presentation before focus changes in size, the focus is incorrect.
-- this will be solved by having a document focus.


focus on presentation requires rearrange after each focus move. This does not seem to be what we want
will we allow the presentation to be influenced by the focus? This will be even more expensive

mouse handling stuff seems to call for a backtrack in edit levels, try highest level, if fail try lower.
This is not part of the model yet



BUG copy depends on direction!!
-}






