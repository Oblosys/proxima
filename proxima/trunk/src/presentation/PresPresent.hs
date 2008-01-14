module PresPresent where

import CommonTypes
import PresLayerTypes
import PresLayerUtils

import qualified Data.Map as Map
import Data.Map (Map)

presentIO :: PresentationSheet doc enr node clip -> LayerStatePres -> EnrichedDocLevel enr ->
             PresentationLevel doc node clip -> EditEnrichedDoc' enr ->
             IO (EditPresentation' doc node clip, LayerStatePres, EnrichedDocLevel enr)
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

present :: PresentationSheet doc enr node clip -> LayerStatePres -> EnrichedDocLevel enr ->
           PresentationLevel doc node clip -> EditEnrichedDoc' enr ->
           (EditPresentation' doc node clip, LayerStatePres, EnrichedDocLevel enr)
present _ state enrLvl (PresentationLevel pres layout) (SkipEnr' 0) = {-debug Prs ("Present:"++show pres++"\n focus "++show focus)-} 
  (SetPres' (PresentationLevel pres layout), state, enrLvl)  -- we should re present here because of local state
present _ state enrlvl pres                            (SkipEnr' i) = (SkipPres' (i-1), state, enrlvl)
present presentationSheet state (EnrichedDocLevel enr _) (PresentationLevel pres (layoutMap,idC  , _,_  )) (SetEnr' enrlvl)  =
    
  let --focusXY             = saveFocus focus pres
      (pres', layoutMap', idC')      = presentEnr presentationSheet state enrlvl layoutMap idC
      --focus'              = restoreFocus focusXY pres'

      --  (pres'', focus'') = (pres',focus')--normalizePresentation pres focus
  in  (SetPres' (PresentationLevel pres' (layoutMap', idC' , [] , Map.empty {-, inss, dels -} {- [(IDP 2, StringP (IDP (-1)) "deleted")
                                                                                     ,(IDP (-1), StringP (IDP (-2)) "both")
                                                                                     ] -} )), state, enrlvl)

presentEnr :: PresentationSheet doc enr node clip -> LayerStatePres -> EnrichedDocLevel enr ->
              WhitespaceMap -> IDPCounter ->
              (Presentation doc node clip, WhitespaceMap, IDPCounter)
presentEnr presentationSheet state (EnrichedDocLevel d focusD ) layM idC = 
      let (layM', idC', pres', self) = (presentationSheet d focusD layM idC)
      in  (pres', layM', idC')                      