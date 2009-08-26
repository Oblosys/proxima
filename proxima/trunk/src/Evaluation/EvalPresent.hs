module Evaluation.EvalPresent where

import Common.CommonTypes
import Evaluation.EvalLayerTypes
import Evaluation.EvalLayerUtils

import Evaluation.DocumentEdit

import Proxima.Wrap

presentIO :: ( Doc doc, Clip clip, Editable doc doc enr node clip token, EvaluationSheet doc enr clip
             , Show doc, Show enr, Show token, Show node ) =>
             LayerStateEval doc clip -> DocumentLevel doc clip -> EnrichedDocLevel enr doc ->
             [EditDocument' doc enr node clip token] ->
             IO ([EditEnrichedDoc' doc enr node clip token], LayerStateEval doc clip, DocumentLevel doc clip)
presentIO state high low = castRemainingEditOps $ \editHigh ->
  do { (editLow, state', high') <- eval state high low editHigh
     
     ; debugLnIO Prs ("editDoc':"++show editHigh)
     --; debugLnIO Prs ("editEnr':"++show editLow)
     ; return $ ([editLow], state', high')
     }

{-
eval :: (Doc doc, Clip clip, Editable doc doc enr node clip token, EvaluationSheet doc enr clip) =>
             LayerStateEval -> DocumentLevel doc clip -> EnrichedDocLevel enr ->
             EditDocument' doc clip doc ->
             IO (EditEnrichedDoc' enr, LayerStateEval, DocumentLevel doc clip)
-}
eval state docLvl@(DocumentLevel doc focusD clipD) enrLvl docEdit =
  case docEdit of 
    SkipDoc' 0 -> return (SetEnr' enrLvl, state, docLvl)  -- we should re-evaluate here because of local state
    SkipDoc' i -> return (SkipEnr' (i-1), state, docLvl)
    SetDoc' docLvl  -> evaluationSheet (recordEditAction docLvl state) docLvl enrLvl docEdit docLvl
    EvaluateDoc' -> evaluationSheet state docLvl enrLvl docEdit docLvl
    WrapDoc' wrapped -> return (unwrap wrapped, state, docLvl)
    _ -> do { let (doclvl', state') = editDoc state docLvl docEdit
            ; evaluationSheet state' docLvl enrLvl docEdit doclvl'
            }


-- TODO: make sure that document is parsed before doing these:
editDoc :: ( Doc doc, Clip clip, Editable doc doc enr node clip token,
             Show doc, Show enr, Show token, Show node ) =>
           LayerStateEval doc clip -> DocumentLevel doc clip -> EditDocument' doc enr node clip token ->
           (DocumentLevel doc clip, LayerStateEval doc clip)
editDoc state (DocumentLevel doc pth clipD) (NavPathDoc' path) = ((DocumentLevel doc path clipD), state)
editDoc state (DocumentLevel doc pth clipD) NavUpDoc'        = ((DocumentLevel doc (navigateUpD pth doc) clipD), state)
editDoc state (DocumentLevel doc pth clipD) NavDownDoc'      = ((DocumentLevel doc (navigateDownD pth doc) clipD), state)
editDoc state (DocumentLevel doc pth clipD) NavLeftDoc'      = ((DocumentLevel doc (navigateLeftD pth doc) clipD), state)
editDoc state (DocumentLevel doc pth clipD) NavRightDoc'     = ((DocumentLevel doc (navigateRightD pth doc) clipD), state)
editDoc state doclvl                        UndoDoc'         = undo doclvl state
editDoc state doclvl                        RedoDoc'         = redo doclvl state 
editDoc state doclvl                        (UpdateDoc' upd) = recordEditAction' (upd doclvl) state
editDoc state doclvl                        CutDoc'          = recordEditAction' (editCutD doclvl) state
editDoc state doclvl                        CopyDoc'         = recordEditAction' (editCopyD doclvl) state
editDoc state doclvl                        PasteDoc'        = recordEditAction' (editPasteD doclvl) state
editDoc state doclvl                        DeleteDoc'       = recordEditAction' (editDeleteD doclvl) state
editDoc state doclvl                        op               = debug Err ("EvalPresent:unhandled doc edit: "++show op) (doclvl, state)

recordEditAction :: DocumentLevel doc clip -> LayerStateEval doc clip -> LayerStateEval doc clip
recordEditAction docLevel (LayerStateEval history future) = 
  LayerStateEval (docLevel:history) future
-- return doc in tuple, for easy use in editDoc

recordEditAction' :: DocumentLevel doc clip -> LayerStateEval doc clip -> 
                     (DocumentLevel doc clip, LayerStateEval doc clip)
recordEditAction' docLevel (LayerStateEval history _) = 
  (docLevel, LayerStateEval (docLevel:history) [])

undo :: DocumentLevel doc clip -> LayerStateEval doc clip -> 
        (DocumentLevel doc clip, LayerStateEval doc clip)
undo docLevel state@(LayerStateEval [] future) = 
  (docLevel, state)
undo docLevel (LayerStateEval (docLevel':history) future) = 
  (docLevel', LayerStateEval history (docLevel:future))

redo :: DocumentLevel doc clip -> LayerStateEval doc clip -> 
        (DocumentLevel doc clip, LayerStateEval doc clip)
redo docLevel state@(LayerStateEval history []) = 
  (docLevel, state)
redo docLevel (LayerStateEval history (docLevel':future)) = 
  (docLevel', LayerStateEval (docLevel:history) future)
