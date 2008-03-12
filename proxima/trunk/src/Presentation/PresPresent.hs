module Presentation.PresPresent where

import Common.CommonTypes
import Presentation.PresLayerTypes
import Presentation.PresLayerUtils

import qualified Data.Map as Map
import Data.Map (Map)

presentIO :: PopupMenuHack node doc =>
             PresentationSheet doc enr node clip token -> LayerStatePres -> EnrichedDocLevel enr doc ->
             PresentationLevel doc node clip token -> EditEnrichedDoc' enr doc ->
             IO (EditPresentation' doc node clip token, LayerStatePres, EnrichedDocLevel enr doc)
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

present :: PopupMenuHack node doc =>
           PresentationSheet doc enr node clip token -> LayerStatePres -> EnrichedDocLevel enr doc ->
           PresentationLevel doc node clip token -> EditEnrichedDoc' enr doc ->
           (EditPresentation' doc node clip token, LayerStatePres, EnrichedDocLevel enr doc)
present _ state enrLvl (PresentationLevel pres layout) (SkipEnr' 0) = {-debug Prs ("Present:"++show pres++"\n focus "++show focus)-} 
  (SetPres' (PresentationLevel pres layout), state, enrLvl)  -- we should re present here because of local state
present _ state enrlvl pres                            (SkipEnr' i) = (SkipPres' (i-1), state, enrlvl)
present presentationSheet state (EnrichedDocLevel enr _ _) (PresentationLevel pres (layoutMap,idC)) (SetEnr' enrlvl)  =
    
  let --focusXY             = saveFocus focus pres
      (pres', layoutMap', idC')      = presentEnr presentationSheet state enrlvl layoutMap idC
      --focus'              = restoreFocus focusXY pres'

      --  (pres'', focus'') = (pres',focus')--normalizePresentation pres focus
  in  (SetPres' (PresentationLevel pres' (layoutMap', idC')), state, enrlvl)

presentEnr :: PopupMenuHack node doc =>
              PresentationSheet doc enr node clip token -> LayerStatePres -> EnrichedDocLevel enr doc ->
              WhitespaceMap -> IDPCounter ->
              (Presentation doc node clip token, WhitespaceMap, IDPCounter)
presentEnr presentationSheet state (EnrichedDocLevel d focusD doc) layM idC = 
      let (layM', idC', pres', self) = presentationSheet d focusD layM idC []
          pres'' = loc (mkDocNode doc) $ pres' 
                   -- HACK!! top level loc needs to be a ref to the document
                   -- it is used by mkPopupMenuXY in Renderer.
                   -- A better implementation of popups will create the menu in
                   -- the higher layers. Although it is still a bit unclear where
                   
                   -- The implementation should take care that popups are correct after parsing without evaluation
                   -- Furthermore, the current implementation does not allow structural 
                   -- differences between doc and enriched doc (paths are enr paths)
      in  (pres'', layM', idC')                      
