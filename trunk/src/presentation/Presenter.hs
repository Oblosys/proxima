module Presenter where


import CommonTypes
import PresLayerTypes

import PresLayerUtils



import XprezLib
import TreeEditPres

import IOExts

import List
import Char


-- Testing:
{-
presentDoc :: Document -> Presentation node
presentDoc doc = prez 

parsePres :: Presentation node -> Maybe Document
parsePres pr = Nothing

parsePres' l pr = Just initDoc
-}

-- Presentation parser

presentEnr :: PresentationSheet doc enr node clip -> LayerStatePres -> EnrichedDocLevel enr ->
              LayoutMap -> IDPCounter ->
              (Presentation doc node clip, LayoutMap, IDPCounter)
presentEnr presentationSheet state enrlvl@(EnrichedDocLevel _ focusD) = -- debug Prs ("Doc Focus is "++show focusD) 
  presentEnr' presentationSheet state enrlvl



presentEnr' :: PresentationSheet doc enr node clip -> LayerStatePres -> EnrichedDocLevel enr ->
               LayoutMap -> IDPCounter ->
               (Presentation doc node clip, LayoutMap, IDPCounter)
presentEnr' presentationSheet state (EnrichedDocLevel d focusD ) layM idC = 
      let (layM', idC', pres', self) = (presentationSheet d focusD layM idC)
      in  (pres', layM', idC')                                 
      {-
  let (pres, idc) = presentDecls' 0 [0] decls
  in  loc (DocNode d []) $ structural $ pres --  ++ result (evaluateExp e))
 where result (f1,f2,valid) = if valid then [text "=", text (show f1)]
                                       else  [text "=", text (show f1) `withColor` red
                                             ,text ("  {"++show f2++"}") `withColor` grey]
-}