module PresTranslate where

import CommonTypes
import PresLayerTypes

import PresLayerUtils

import ProxParser


--translateIO :: LayerStatePres -> low -> high -> editLow -> IO (editHigh, state, low)
translateIO :: LayerStatePres -> PresentationLevel -> EnrichedDocLevel -> EditPresentation -> IO (EditEnrichedDoc, LayerStatePres, PresentationLevel)
translateIO state low high editLow =
  do { (editHigh, state', low') <- parseIO state low high editLow
--     ; debugLnIO Prs $ "Edit Pres:"++show editLow
     ; return (editHigh, state', low')
     }


-- split in monadic and non-monadic part
parseIO :: LayerStatePres -> PresentationLevel -> EnrichedDocLevel -> EditPresentation -> IO (EditEnrichedDoc, LayerStatePres, PresentationLevel)

-- these need not be monadic anymore:
parseIO state presLvl enrLvl                  (OpenFilePres fpth) = return (OpenFileEnr fpth, state, presLvl)
parseIO state presLvl enrLvl                  (SaveFilePres fpth) = return (SaveFileEnr fpth, state, presLvl)
parseIO state presLvl enrLvl                  InitPres            = return (InitEnr,          state, presLvl)
parseIO state presLvl enrLvl                  event               = return $ parse state presLvl enrLvl event


parse :: LayerStatePres -> PresentationLevel -> EnrichedDocLevel -> EditPresentation ->
         (EditEnrichedDoc, LayerStatePres, PresentationLevel)

parse state _       enrLvl (SetPres presLvl@(PresentationLevel pres layout))  = 
  setUpd AllUpdated $ editParse state presLvl enrLvl
--  ( SkipDoc 0, state
--  , PresentationLevel (markUnparsed pres) layout)  -- this goes wrong, presentation focus is not used

parse state presLvl enrLvl (SkipPres i) = (SkipEnr (i+1), state, presLvl)
--can't normalize here because there is no focus. Maybe normalize without focus. 
--parse state presLvl enrLvl NormalizePres = setUpd AllUpdated $ editNormalize state presLvl enr 
parse state presLvl enrLvl TestPres      = setUpd AllUpdated $ editParse state presLvl enrLvl
{-parse state presLvl enrLvl Test2Pres   = setUpd AllUpdated $editReadFile state presLvl enr focus 
--parse state presLvl enrLvl (MouseDownPres path ms i) = setUpd AllUpdated $ editMouseDown state presLvl enr path -- Helium
parse state presLvl enr (DocumentLoadedPres str) =  setUpd AllUpdated $ setDocument state presLvl enr str
-- to allow presenter mouse handle: change GestureInterpreter, so the event is handled there
-}
parse state presLvl enrLvl NavUpDocPres = (NavUpDocEnr, state, presLvl)
parse state presLvl enrLvl NavDownDocPres = (NavDownDocEnr, state, presLvl)
parse state presLvl enrLvl NavLeftDocPres = (NavLeftDocEnr, state, presLvl)
parse state presLvl enrLvl NavRightDocPres = (NavRightDocEnr, state, presLvl)
parse state presLvl enrLvl CutDocPres    = (CutDocEnr, state, presLvl)
parse state presLvl enrLvl CopyDocPres   = (CopyDocEnr, state, presLvl)
parse state presLvl enrLvl PasteDocPres  = (PasteDocEnr, state, presLvl)
parse state presLvl enrLvl DeleteDocPres = (DeleteDocEnr, state, presLvl)
parse state presLvl enrLvl (UpdateDocPres upd) = (UpdateDocEnr upd, state, presLvl)
parse state presLvl enrLvl Test2Pres     = (EvaluateDocEnr, state, presLvl)
parse state presLvl enrLvl _            = (SkipEnr 0, state, presLvl)


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
editParse :: LayerStatePres-> PresentationLevel -> EnrichedDocLevel -> (EditEnrichedDoc, LayerStatePres, PresentationLevel)
editParse state presLvl@(PresentationLevel pres layout) (EnrichedDocLevel _ oldFocus) = 
  case parsePres pres of
     Nothing   -> (SkipEnr 0, state, PresentationLevel (markUnparsed pres) layout)
     Just (enr', inss, dels) -> (SetEnr (EnrichedDocLevel enr' oldFocus), state, presLvl) -- ignore inss and dels for now



