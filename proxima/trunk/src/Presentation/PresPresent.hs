module Presentation.PresPresent where

import Common.CommonTypes
import Presentation.PresLayerTypes
import Presentation.PresLayerUtils

import qualified Data.Map as Map
import Data.Map (Map)

presentIO :: PresentationSheet doc enr node clip token -> LayerStatePres -> EnrichedDocLevel enr ->
             PresentationLevel doc node clip token -> EditEnrichedDoc' enr ->
             IO (EditPresentation' doc node clip token, LayerStatePres, EnrichedDocLevel enr)
presentIO presentationSheet state high low@(PresentationLevel pres layout) editHigh =
  let (editLow, state', high') = present presentationSheet state high low editHigh
  in do { -- debugLnIO Prs ("editEnr':"++show editHigh)
        --; debugLnIO Prs ("editPres':"++show editLow)
        ; return $ (editLow, state', high')
        }


-- inss and dels are passed in a bizarre way now because lower local state may not be updated
-- at the translation side
-- inserted and deleted are taken from setDoc' and put in PresentationLevelState
-- on document edit, old inserted and deleted from level are reused

present :: PresentationSheet doc enr node clip token -> LayerStatePres -> EnrichedDocLevel enr ->
           PresentationLevel doc node clip token -> EditEnrichedDoc' enr ->
           (EditPresentation' doc node clip token, LayerStatePres, EnrichedDocLevel enr)
present _ state enrLvl (PresentationLevel pres layout) (SkipEnr' 0) = {-debug Prs ("Present:"++show pres++"\n focus "++show focus)-} 
  (SetPres' (PresentationLevel pres layout), state, enrLvl)  -- we should re present here because of local state
present _ state enrlvl pres                            (SkipEnr' i) = (SkipPres' (i-1), state, enrlvl)
present presentationSheet state (EnrichedDocLevel enr _) (PresentationLevel pres (layoutMap,idC)) (SetEnr' enrlvl)  =
    
  let --focusXY             = saveFocus focus pres
      (pres', layoutMap', idC')      = presentEnr presentationSheet state enrlvl layoutMap idC
      --focus'              = restoreFocus focusXY pres'

      --  (pres'', focus'') = (pres',focus')--normalizePresentation pres focus
  in  (SetPres' (PresentationLevel pres' (layoutMap', idC')), state, enrlvl)

presentEnr :: PresentationSheet doc enr node clip token -> LayerStatePres -> EnrichedDocLevel enr ->
              WhitespaceMap -> IDPCounter ->
              (Presentation doc node clip token, WhitespaceMap, IDPCounter)
presentEnr presentationSheet state (EnrichedDocLevel d focusD ) layM idC = 
      let (layM', idC', pres', self) = (presentationSheet d focusD layM idC)
      in  (pres', layM', idC')                      
