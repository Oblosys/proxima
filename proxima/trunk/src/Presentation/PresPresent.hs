module Presentation.PresPresent where

import Common.CommonTypes
import Common.CommonUtils
import Presentation.PresLayerTypes
import Presentation.PresLayerUtils
import Proxima.Wrap

import qualified Data.Map as Map
import Data.Map (Map)

presentIO :: PresentationSheet doc enr node clip token -> LayerStatePres -> EnrichedDocLevel enr doc ->
             PresentationLevel doc enr node clip token -> [EditEnrichedDoc' doc enr node clip token] ->
             IO ([EditPresentation' doc enr node clip token], LayerStatePres, EnrichedDocLevel enr doc)
presentIO presentationSheet state high low@(PresentationLevel pres layout) = castRemainingEditOps $ \editHigh ->
  let (editLow, state', high') = present presentationSheet state high low editHigh
  in do { -- debugLnIO Prs ("editEnr':"++show editHigh)
        --; debugLnIO Prs ("editPres':"++show editLow)
        ; return $ ([editLow], state', high')
        }


-- inss and dels are passed in a bizarre way now because lower local state may not be updated
-- at the translation side
-- inserted and deleted are taken from setDoc' and put in PresentationLevelState
-- on document edit, old inserted and deleted from level are reused

present :: PresentationSheet doc enr node clip token -> LayerStatePres -> EnrichedDocLevel enr doc ->
           PresentationLevel doc enr node clip token -> EditEnrichedDoc' doc enr node clip token ->
           (EditPresentation' doc enr node clip token, LayerStatePres, EnrichedDocLevel enr doc)
present _ state enrLvl (PresentationLevel pres layout) (SkipEnr' 0) = {-debug Prs ("Present:"++show pres++"\n focus "++show focus)-} 
  (SetPres' (PresentationLevel pres layout), state, enrLvl)  -- we should re present here because of local state
present _ state enrLvl pres                            (SkipEnr' i) = (SkipPres' (i-1), state, enrLvl)
present presentationSheet state (EnrichedDocLevel _ _ _) (PresentationLevel pres (layoutMap,idC)) (SetEnr' enrLvl)  =
    
  let --focusXY             = saveFocus focus pres
      (pres', layoutMap', idC')      = presentEnr presentationSheet state enrLvl layoutMap idC
      --focus'              = restoreFocus focusXY pres'

      --  (pres'', focus'') = (pres',focus')--normalizePresentation pres focus
  in  (SetPres' (PresentationLevel pres' (layoutMap', idC')), state, enrLvl)
present _ state enrLvl pres (WrapEnr' wrapped) = (unwrap wrapped, state, enrLvl)

presentEnr :: PresentationSheet doc enr node clip token -> LayerStatePres -> EnrichedDocLevel enr doc ->
              WhitespaceMap -> IDPCounter ->
              (Presentation doc enr node clip token, WhitespaceMap, IDPCounter)
presentEnr presentationSheet state (EnrichedDocLevel enr focusD doc) layM idC = 
      let (layM', idC', pres') = presentationSheet enr doc focusD layM idC
                   -- Bit of a hack, doc is passed to presentationSheet for automatic popups.
                   -- Does not allow structural 
                   -- differences between doc and enriched doc (paths are enr paths)
      in  (pres', layM', idC')                      
