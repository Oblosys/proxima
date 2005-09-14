module PresTranslate where

import CommonTypes
import PresLayerTypes

import PresLayerUtils


--translateIO :: LayerStatePres -> low -> high -> editLow -> IO (editHigh, state, low)
translateIO :: ParseSheet node -> LayerStatePres -> PresentationLevel node -> EnrichedDocLevel -> EditPresentation node -> IO (EditEnrichedDoc, LayerStatePres, PresentationLevel node)
translateIO parseSheet state low high editLow =
  do { (editHigh, state', low') <- parseIO parseSheet state low high editLow
--     ; debugLnIO Prs $ "Edit Pres:"++show editLow
     ; return (editHigh, state', low')
     }


-- split in monadic and non-monadic part
parseIO :: ParseSheet node -> LayerStatePres -> PresentationLevel node -> EnrichedDocLevel -> EditPresentation node -> IO (EditEnrichedDoc, LayerStatePres, PresentationLevel node)

-- these need not be monadic anymore:
parseIO _ state presLvl enrLvl                  (OpenFilePres fpth) = return (OpenFileEnr fpth, state, presLvl)
parseIO _ state presLvl enrLvl                  (SaveFilePres fpth) = return (SaveFileEnr fpth, state, presLvl)
parseIO _ state presLvl enrLvl                  InitPres            = return (InitEnr,          state, presLvl)
parseIO parseSheet state presLvl enrLvl                  event               = return $ parse parseSheet state presLvl enrLvl event


parse :: ParseSheet node -> LayerStatePres -> PresentationLevel node -> EnrichedDocLevel -> EditPresentation node ->
         (EditEnrichedDoc, LayerStatePres, PresentationLevel node)

parse parseSheet state _       enrLvl (SetPres presLvl@(PresentationLevel pres layout))  = 
  setUpd AllUpdated $ editParse parseSheet state presLvl enrLvl
--  ( SkipDoc 0, state
--  , PresentationLevel (markUnparsed pres) layout)  -- this goes wrong, presentation focus is not used

parse _ state presLvl enrLvl (SkipPres i) = (SkipEnr (i+1), state, presLvl)
--can't normalize here because there is no focus. Maybe normalize without focus. 
--parse state presLvl enrLvl NormalizePres = setUpd AllUpdated $ editNormalize state presLvl enr 
parse parseSheet state presLvl enrLvl TestPres      = setUpd AllUpdated $ editParse parseSheet state presLvl enrLvl
{-parse state presLvl enrLvl Test2Pres   = setUpd AllUpdated $editReadFile state presLvl enr focus 
--parse state presLvl enrLvl (MouseDownPres path ms i) = setUpd AllUpdated $ editMouseDown state presLvl enr path -- Helium
parse state presLvl enr (DocumentLoadedPres str) =  setUpd AllUpdated $ setDocument state presLvl enr str
-- to allow presenter mouse handle: change GestureInterpreter, so the event is handled there
-}
parse _ state presLvl enrLvl NavUpDocPres = (NavUpDocEnr, state, presLvl)
parse _ state presLvl enrLvl NavDownDocPres = (NavDownDocEnr, state, presLvl)
parse _ state presLvl enrLvl NavLeftDocPres = (NavLeftDocEnr, state, presLvl)
parse _ state presLvl enrLvl NavRightDocPres = (NavRightDocEnr, state, presLvl)
parse _ state presLvl enrLvl CutDocPres    = (CutDocEnr, state, presLvl)
parse _ state presLvl enrLvl CopyDocPres   = (CopyDocEnr, state, presLvl)
parse _ state presLvl enrLvl PasteDocPres  = (PasteDocEnr, state, presLvl)
parse _ state presLvl enrLvl DeleteDocPres = (DeleteDocEnr, state, presLvl)
parse _ state presLvl enrLvl (UpdateDocPres upd) = (UpdateDocEnr upd, state, presLvl)
parse _ state presLvl enrLvl Test2Pres     = (EvaluateDocEnr, state, presLvl)
parse _ state presLvl enrLvl _            = (SkipEnr 0, state, presLvl)


{-
-- edit ops need to be consistent, when navigating with non-empty focus, collapse focus
-- when inserting with non-empty focus, first delete

-- edit ops that actually change the presentation tree should be a separate type because now we have multiple 
-- functions or lose type safety  (?)


-- enr and/or presentation need some way to say whether document parts are parsed. Now With nodes pile up on the 
-- unparsed presentation.




-- if focus is valid, apply editF to the presentation, and try to reparse the presentation 
--editPres :: 
--            Presentation -> Presentation -> Document -> FocusPres -> (EditDocument, Presentation, Presentation)


-}
    -- parse and type check  (reuse old enrdocument focus)
editParse :: ParseSheet node -> LayerStatePres-> PresentationLevel node -> EnrichedDocLevel -> (EditEnrichedDoc, LayerStatePres, PresentationLevel node)
editParse parseSheet state presLvl@(PresentationLevel pres layout) (EnrichedDocLevel _ oldFocus) = 
  case parseSheet pres of
     Nothing   -> (SkipEnr 0, state, PresentationLevel (markUnparsed pres) layout)
     Just enr' -> (SetEnr (EnrichedDocLevel enr' oldFocus), state, presLvl)


