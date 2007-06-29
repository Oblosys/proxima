module LayTranslate where

import CommonTypes
import LayLayerTypes
import LayLayerUtils


import TreeEditPres


--translateIO :: state -> low -> high -> editLow -> IO (editHigh, state, low)
translateIO :: ScannerSheet doc node clip -> LayerStateLay doc node clip -> LayoutLevel doc node clip -> PresentationLevel doc node clip -> EditLayout documentLevel doc node clip
            -> IO (EditPresentation documentLevel doc node clip, LayerStateLay doc node clip, LayoutLevel doc node clip)
translateIO scannerSheet state low high editLow =
  do { (editHigh, state', low') <- parseIO scannerSheet state low high editLow
     ; debugLnIO Err $ "Edit Layout: "++show editLow
     ; return (editHigh, state', low')
     }


-- one extra indirection because separate cases make it hard to do debugging on result



--------------- Parser


-- split in monadic and non-monadic part
parseIO :: ScannerSheet doc node clip -> LayerStateLay doc node clip -> LayoutLevel doc node clip -> PresentationLevel doc node clip -> EditLayout documentLevel doc node clip -> IO (EditPresentation documentLevel doc node clip, LayerStateLay doc node clip, LayoutLevel doc node clip)
--parseIO _ state layLvl prs (OpenFileLay str) = openFile str state layLvl prs
--parseIO _ state layLvl prs (SaveFileLay str) = setUpd NothingUpdated $ saveFile state layLvl prs str 
--parseIO _ state layLvl prs (DocumentLoadedLay str) =  return $ editLay (editInsert 'X') state layLvl prs
parseIO _ state layLvl prs (OpenFileLay str) = return (OpenFilePres str, state, layLvl)
parseIO _ state layLvl prs (SaveFileLay str) = return (SaveFilePres str, state, layLvl)
parseIO scannerSheet state layLvl prs event = return $ parse scannerSheet state layLvl prs event

parse :: ScannerSheet doc node clip -> LayerStateLay doc node clip -> LayoutLevel doc node clip -> PresentationLevel doc node clip -> EditLayout documentLevel doc node clip -> (EditPresentation documentLevel doc node clip, LayerStateLay doc node clip, LayoutLevel doc node clip)
parse _ state layLvl@(LayoutLevel pres _ dt) prs (SetFocusLay focus) = 
  (SkipPres 0, state, LayoutLevel pres focus dt)
parse _ state layLvl prs (SkipLay i)   = (SkipPres (i+1), state, layLvl)
parse _ state layLvl prs InitLay       = (InitPres, state, layLvl)
parse _ state layLvl prs (InsertLay c) = editLay (editInsert c) state layLvl prs
parse _ state layLvl prs CutLay   = editLay editCut state layLvl prs
parse _ state layLvl prs CopyLay   = editCopy state layLvl prs
parse _ state layLvl prs PasteLay  = editLay editPaste state layLvl prs
parse _ state layLvl prs DeleteLay = editLay editDelete state layLvl prs 
parse _ state layLvl prs SplitLay  = editLay editSplit state layLvl prs
parse _ state layLvl prs LeftDeleteLay = editLay editLeftDelete state layLvl prs
parse _ state layLvl prs RightDeleteLay = editLay editRightDelete state layLvl prs
parse _ state layLvl prs LeftLay   = navigateLeft state layLvl prs 
parse _ state layLvl prs RightLay  = navigateRight state layLvl prs

parse _ state layLvl prs EnlargeLeftLay   = enlargeLeft state layLvl prs
parse _ state layLvl prs EnlargeRightLay  = enlargeRight state layLvl prs

parse _ state layLvl@(LayoutLevel pres _ _) prs (AddVertexLay pth pos)  = addVertex pth pos state layLvl
parse _ state layLvl@(LayoutLevel pres _ _) prs (AddEdgeLay pth)        = addEdge pth state layLvl
parse _ state layLvl@(LayoutLevel pres _ _) prs (MoveVertexLay pth pos) = moveVertex pth pos state layLvl
parse _ state layLvl prs NormalizeLay       = editLay editNormalize state layLvl prs  

parse scannerSheet state layLvl prs ParseLay = tokenizeLay scannerSheet state layLvl prs
parse _ state layLvl prs Test2Lay           = (Test2Pres, state, layLvl)


parse _ state layLvl prs (UpdateDocLay upd) = (UpdateDocPres upd, state, layLvl)
parse _ state layLvl prs NavUpDocLay        = (NavUpDocPres, state, layLvl)
parse _ state layLvl prs NavDownDocLay      = (NavDownDocPres, state, layLvl)
parse _ state layLvl prs NavLeftDocLay      = (NavLeftDocPres, state, layLvl)
parse _ state layLvl prs NavRightDocLay     = (NavRightDocPres, state, layLvl)
parse _ state layLvl prs CutDocLay          = (CutDocPres, state, layLvl)
parse _ state layLvl prs CopyDocLay         = (CopyDocPres, state, layLvl)
parse _ state layLvl prs PasteDocLay        = (PasteDocPres, state, layLvl)
parse _ state layLvl prs DeleteDocLay       = (DeleteDocPres, state, layLvl)
parse _ state layLvl prs Test2Lay           = (Test2Pres, state, layLvl)
-- We want to be able to set the presentation here and probably do a Layout to Presentation mapping.
-- Not possible with just single edit commands. The problem is that the parser must not always be called. This
-- does not readily fit in the current model.
-- We can fix it by not setting the higher pres level if we don't want a parse.


{-parse _ state layLvl prs Test2Lay   = setUpd AllUpdated $editReadFile state layLvl prs focus 
--parse _ state layLvl prs (MouseDownLay path ms i) = setUpd AllUpdated $ editMouseDown state layLvl prs path -- Helium
-- to allow presenter mouse handle: change GestureInterpreter, so the event is handled there
-}
parse _ state layLvl prs _            = (SkipPres 0, state, layLvl)


-- edit ops need to be consistent, when navigating with non-empty focus, collapse focus
-- when inserting with non-empty focus, first delete

-- edit ops that actually change the presentation tree should be a separate type because now we have multiple 
-- functions or lose type safety  (?)


-- doc and/or presentation need some way to say whether document parts are parsed. Now With nodes pile up on the 
-- unparsed presentation.


tokenizeLay scannerSheet state layLvl@(LayoutLevel pres focus dt) (PresentationLevel _ (layout, idCounter, inserted, deleted)) = 
 let (pres', layout', idCounter') = scannerSheet idCounter Nothing pres
     pres''                       = deleteInsertedTokens inserted pres'
     presLvl'                     = debug Err ("layTranslate: inss="++show inserted) $ PresentationLevel pres'' (layout',idCounter', inserted, deleted)
 in  (SetPres presLvl', state, layLvl) --LayoutLevel (markUnparsed pres') (markUnparsedF pres' focus'))




-- if focus is valid, apply editF to the presentation, and try to reparse the presentation 
--editLay :: 
--            Presentation doc node clip -> Presentation doc node clip -> LayoutLevel doc node clip -> FocusPres -> (EditPresentation documentLevel doc node clip, Presentation doc node clip, Presentation doc node clip)

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
    (SkipPres 0, state', LayoutLevel pres'' focus'' diffTree)


-- should we make a similar function for edit ops that do not alter the presentation? This function would not do
-- much, except setting the update region, getting rid of the document argument and returning a SkipPres 0,
-- Also some edit ops change the focus, whereas others only change the clip, or do IO.  Different functions?
-- They differ also in handling an empty focus. copy and navigate can't handle empty focus, but save would work fine
-- What is a nice abstraction here?

-- replace the current layout by pres, focus is reset


-- replace the current layout by pres, focus is reset
editSet :: Presentation doc node clip -> Presentation doc node clip -> LayoutLevel doc node clip -> (LayoutLevel doc node clip, Presentation doc node clip)
editSet pres' clip (LayoutLevel pres focus@(FocusP f t) dt) = (LayoutLevel pres' NoFocusP dt, clip)

openFile :: String -> Presentation doc node clip -> LayoutLevel doc node clip -> PresentationLevel doc node clip -> IO (EditPresentation documentLevel doc node clip, Presentation doc node clip, LayoutLevel doc node clip)
openFile filePath clip layLvl prs =
 do { debugLnIO Lay $ "Opening file: "++filePath
    ; str <- readFile filePath
    ; let pres' = StringP NoIDP filePath
    ; return $ editLay (editSet pres') clip layLvl prs
    }
    
editInsert :: Char -> Presentation doc node clip -> LayoutLevel doc node clip -> (LayoutLevel doc node clip, Presentation doc node clip)
editInsert c clip (LayoutLevel pres focus@(FocusP f t) dt) = 
  let (pres', focus')  = if f==t then (pres,focus) else deleteTree focus pres
      (pres'',focus'') = pasteTree (fromP focus') (text [c]) pres'
  in  (LayoutLevel pres'' focus'' dt, clip)

editCut :: Presentation doc node clip -> LayoutLevel doc node clip -> (LayoutLevel doc node clip, Presentation doc node clip)
editCut clip (LayoutLevel pres focus dt) = 
  let clip' = copyTree focus clip pres                                                                                              
      (pres', focus') = deleteTree focus pres
  in  (LayoutLevel pres' focus' dt, clip')

editCopy :: Presentation doc node clip -> LayoutLevel doc node clip -> PresentationLevel doc node clip -> (EditPresentation documentLevel doc node clip, Presentation doc node clip, LayoutLevel doc node clip)
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
editNormalize :: Presentation doc node clip -> LayoutLevel doc node clip -> (LayoutLevel doc node clip, Presentation doc node clip)
editNormalize clip (LayoutLevel pres focus dt) = 
 let (pres', focus') = normalizePresentation pres focus
 in  (LayoutLevel pres' focus' dt, clip)



-- unlike paste split and insert, left and right delete do not perform their edit command when the focus was non-empty
-- ie. in that case, they are interpreted as a regular delete
editLeftDelete clip layLvl@(LayoutLevel pres focus@(FocusP f t) dt) =
  if focusIsOnGraph f pres then -- if the from path is in a graph, this is a graph edit
    (LayoutLevel (deleteGraphPres f pres) NoFocusP dt, clip)
  else   
    if f /= t then editDelete clip layLvl else
      let focus'          = navigateLeftTreePres (toP focus) pres
          focus''         = FocusP (toP focus) (toP focus')
          (pres', focus''') = deleteTree focus'' pres
      in (LayoutLevel pres' focus''' dt, clip)

-- if the from path is in a graph, this is a graph edit
editRightDelete clip layLvl@(LayoutLevel pres focus@(FocusP f t) dt) =
  if focusIsOnGraph f pres then -- if the from path is in a graph, this is a graph edit
    (LayoutLevel (deleteGraphPres f pres) NoFocusP dt, clip)
  else   
    if f /= t then editDelete clip layLvl else
      let focus'          = navigateRightTreePres (toP focus) pres
          focus''         = FocusP (toP focus) (toP focus')
          (pres', focus''') = deleteTree focus'' pres
      in  (LayoutLevel pres' focus''' dt, clip)



navigateLeft :: Presentation doc node clip -> LayoutLevel doc node clip -> PresentationLevel doc node clip -> (EditPresentation documentLevel doc node clip, Presentation doc node clip, LayoutLevel doc node clip)
navigateLeft clip layLvl@(LayoutLevel pres NoFocusP dt) doc = (SkipPres 0, clip, layLvl)
navigateLeft clip (LayoutLevel pres focus dt) doc =
  let  focus' = navigateLeftTreePres (toP focus) pres
  in  (SkipPres 0, clip, LayoutLevel pres focus' dt)

navigateRight :: Presentation doc node clip -> LayoutLevel doc node clip -> PresentationLevel doc node clip -> (EditPresentation documentLevel doc node clip, Presentation doc node clip, LayoutLevel doc node clip)
navigateRight clip layLvl@(LayoutLevel pres NoFocusP dt) doc = (SkipPres 0, clip, layLvl)
navigateRight clip (LayoutLevel pres focus dt) doc = 
  let  focus' = navigateRightTreePres (toP focus) pres
  in  (SkipPres 0, clip, LayoutLevel pres focus' dt)

enlargeLeft :: Presentation doc node clip -> LayoutLevel doc node clip -> PresentationLevel doc node clip -> (EditPresentation documentLevel doc node clip, Presentation doc node clip, LayoutLevel doc node clip)
enlargeLeft clip layLvl@(LayoutLevel pres NoFocusP dt) doc = (SkipPres 0, clip, layLvl)
enlargeLeft clip (LayoutLevel pres focus dt) doc =
  let  focus' = navigateLeftTreePres (toP focus) pres
       focus'' = FocusP (fromP focus) (fromP focus')
  in  (SkipPres 0, clip, LayoutLevel pres focus'' dt)

enlargeRight :: Presentation doc node clip -> LayoutLevel doc node clip -> PresentationLevel doc node clip -> (EditPresentation documentLevel doc node clip, Presentation doc node clip, LayoutLevel doc node clip)
enlargeRight clip layLvl@(LayoutLevel pres NoFocusP dt) doc = (SkipPres 0, clip, layLvl)
enlargeRight clip (LayoutLevel pres focus dt) doc = 
  let  focus' = navigateRightTreePres (toP focus) pres
       focus'' = FocusP (fromP focus) (fromP focus')
  in  (SkipPres 0, clip, LayoutLevel pres focus'' dt)

addVertex pth (x,y) state layLvl@(LayoutLevel pres focus dt) =
  let pres' = addVertexPres (PathP pth 0) (VertexP NoIDP x y outline vanillaVertex) pres
  in  (SkipPres 0, state, LayoutLevel pres' focus dt)                  -- 0 in path is ignored
 where vanillaVertex = col [ rowR 1 [glue, ellipse 36 36 `withRef` (18,18) `withfColor` (200, 255, 255) , glue]
                         , vSpace 4 `withHStretch` True
                         , rowR 1 [glue, boxed 
                                            (row [ hSpace 3, text "<new>" `withFont'` ("Arial", 10), hSpace 3])
                                              `withbgColor` (236, 236, 169)
                                  , glue]
                         ]
       outline = \a -> (round $ 17*cos a -1, round $ -17*sin a -1)

addEdge toPth state layLvl@(LayoutLevel pres focus@(FocusP (PathP fromPth _) _) dt) =
  let pres' = case selectTree fromPth pres of
                (VertexP _ _ _ _ _) -> addEdgePres (PathP fromPth 0) (PathP toPth 0) pres -- 0 in path is ignored
                _                   -> pres
  in  (SkipPres 0, state, LayoutLevel pres' focus dt)
addEdge _ state layLvl = (SkipPres 0, state, layLvl)
 
moveVertex pth pt state layLvl@(LayoutLevel pres focus dt) =
  let pres' = moveVertex' pth pt pres
  in  (SkipPres 0, state, LayoutLevel pres' focus dt)

moveVertex' :: [Int] -> (Int,Int) -> Presentation doc node clip -> Presentation doc node clip                                      
moveVertex' (p:ps) pt (RowP id rf press)         = RowP id rf $ replace p press (moveVertex' ps pt (press!!!p))
moveVertex' (p:ps) pt (ColP id rf press)         = ColP id rf $ replace p press (moveVertex' ps pt (press!!!p))
moveVertex' (p:ps) pt (OverlayP id (pres:press)) = OverlayP id $ replace p press (moveVertex' ps pt (press!!!p))
moveVertex' (0:ps) pt (WithP ar pres)            = WithP ar (moveVertex' ps pt pres)
moveVertex' (0:ps) pt (StructuralP id pres)      = StructuralP id (moveVertex' ps pt pres)
moveVertex' (0:ps) pt (LocatorP l pres)          = LocatorP l (moveVertex' ps pt pres)
moveVertex' (p:ps) pt (GraphP id w h es press)   = 
  if p < length press 
  then GraphP id w h es $ replace p press (moveVertex' ps pt (press!!!p))
  else debug Err ("TreeEditPres.moveVertex'': can't handle "++ show pr) $ GraphP id w h es press
moveVertex' []     (dx,dy) (VertexP id x y ol pres)  = VertexP id (x+dx) (y+dy) ol pres
moveVertex' _      pt pr                      = debug Err ("TreeEditPres.moveVertex': can't handle "++ show pr) pr

{-
openFile :: Presentation doc node clip -> LayoutLevel doc node clip -> PresentationLevel doc node clip -> FilePath -> IO (EditPresentation documentLevel doc node clip, Presentation doc node clip, LayoutLevel doc node clip) 
openFile clip layLvl doc filePath =
 do { debugLnIO Prs "Opening file"
    ; str <- readFile filePath
    ; let (doc', layLvl', clip') = read str
    ; return (SetPres doc', clip', layLvl')
    }

saveFile :: Presentation doc node clip -> LayoutLevel doc node clip -> PresentationLevel doc node clip -> FilePath -> IO (EditPresentation documentLevel doc node clip, Presentation doc node clip, LayoutLevel doc node clip)
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






