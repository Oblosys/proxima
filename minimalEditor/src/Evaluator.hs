module Evaluator (evaluationSheet) where

import CommonTypes
import EvalLayerTypes
import EvalLayerUtils

import DocumentEdit
import DocumentEdit_Generated
import DocTypes_Generated


evaluationSheet :: LayerStateEval -> DocumentLevel Document ClipDoc -> EnrichedDocLevel EnrichedDoc ->
             EditDocument' (DocumentLevel Document ClipDoc) Document ->
             IO (EditEnrichedDoc' EnrichedDoc, LayerStateEval, DocumentLevel Document ClipDoc)
evaluationSheet  state high low@(EnrichedDocLevel enr focus) editHigh =
  let (editLow, state', high') = eval state high low editHigh
  in do { -- debugLnIO Prs ("editDoc':"++show editHigh)
        --; debugLnIO Prs ("editEnr':"++show editLow)
        ; return $ (editLow, state', high')
        }


eval :: LayerStateEval -> DocumentLevel Document ClipDoc -> EnrichedDocLevel EnrichedDoc ->
             EditDocument' (DocumentLevel Document ClipDoc) Document ->
             (EditEnrichedDoc' EnrichedDoc, LayerStateEval, DocumentLevel Document ClipDoc)
eval state docLvl@(DocumentLevel doc focusD clipD) (EnrichedDocLevel enr oldFocus) docEdit =
  case docEdit of 
    SkipDoc' 0 -> (SetEnr' (EnrichedDocLevel enr oldFocus), state, docLvl)  -- we should re-evaluate here because of local state
    SkipDoc' i -> (SkipEnr' (i-1), state, docLvl)
    SetDoc' d  -> let (enr') = evalDoc state (DocumentLevel d NoPathD clipD) enr 
			      in  (SetEnr' (EnrichedDocLevel enr' focusD), state, DocumentLevel d NoPathD clipD)
    EvaluateDoc' -> let (enr')                  = evalDoc state docLvl enr
					in  (SetEnr' (EnrichedDocLevel enr' focusD), state, docLvl)
    _ -> debug Eva ("DocNavigate"++show focusD) $
           let (doclvl'@(DocumentLevel doc' focusD' clipD'),state') = editDoc state docLvl docEdit
               (enr')                  = evalDoc state' doclvl' enr
           in  (SetEnr' (EnrichedDocLevel enr' focusD'), state', DocumentLevel doc' focusD' clipD')



-- TODO: make sure that document is parsed before doing these:
editDoc :: LayerStateEval -> DocumentLevel Document ClipDoc -> EditDocument' (DocumentLevel Document ClipDoc) Document ->
           (DocumentLevel Document ClipDoc, LayerStateEval)
editDoc state doclvl                        (UpdateDoc' upd) = (upd doclvl, state)
editDoc state (DocumentLevel doc pth clipD) NavUpDoc'        = ((DocumentLevel doc (navigateUpD pth doc) clipD), state)
editDoc state (DocumentLevel doc pth clipD) NavDownDoc'      = ((DocumentLevel doc (navigateDownD pth doc) clipD), state)
editDoc state (DocumentLevel doc pth clipD) NavLeftDoc'      = ((DocumentLevel doc (navigateLeftD pth doc) clipD), state)
editDoc state (DocumentLevel doc pth clipD) NavRightDoc'     = ((DocumentLevel doc (navigateRightD pth doc) clipD), state)
editDoc state doclvl                        CutDoc'          = (editCutD doclvl, state)
editDoc state doclvl                        CopyDoc'         = (editCopyD doclvl, state)
editDoc state doclvl                        PasteDoc'        = (editPasteD doclvl, state)
editDoc state doclvl                        DeleteDoc'       = (editDeleteD doclvl, state)
editDoc state doclvl                        op               = debug Err ("EvalPresent:unhandled doc edit: "++show op) (doclvl, state)


evalDoc :: LayerStateEval -> DocumentLevel Document clip -> EnrichedDoc -> EnrichedDoc
evalDoc state (DocumentLevel doc@(RootDoc idd root) _ _) enr = RootEnr idd root doc
evalDoc state (DocumentLevel doc@(RootDoc idd root) _ _) enr = RootEnr idd root doc
evalDoc state (DocumentLevel (HoleDocument) _ _) _ = HoleEnrichedDoc
evalDoc state (DocumentLevel (ParseErrDocument pr) _ _) _ = ParseErrEnrichedDoc pr -- not the right node type
