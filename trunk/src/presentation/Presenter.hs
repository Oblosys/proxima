module Presenter where


import CommonTypes
import PresLayerTypes

import PresLayerUtils

import DocumentEdit


import PresentationAG

import XprezLib
import TreeEditPres

import IOExts

import List
import Char


-- Testing:
{-
presentDoc :: Document -> Presentation
presentDoc doc = prez 

parsePres :: Presentation -> Maybe Document
parsePres pr = Nothing

parsePres' l pr = Just initDoc
-}

-- Presentation parser

presentEnr :: LayerStatePres -> EnrichedDocLevel -> LayoutMap -> IDPCounter -> (Presentation, LayoutMap, IDPCounter)
presentEnr state enrlvl@(EnrichedDocLevel _ focusD) = -- debug Prs ("Doc Focus is "++show focusD) 
                                                          presentEnr' state enrlvl



presentEnr' :: LayerStatePres -> EnrichedDocLevel -> LayoutMap -> IDPCounter -> (Presentation, LayoutMap, IDPCounter)
presentEnr' state (EnrichedDocLevel d focusD ) layM idC =
      let (layM', idC', pres', self) = (sem_EnrichedDoc d focusD layM idC)
      in  (pres', layM', idC')                                 
      {-
  let (pres, idc) = presentDecls' 0 [0] decls
  in  loc (DocNode d []) $ structural $ pres --  ++ result (evaluateExp e))
 where result (f1,f2,valid) = if valid then [text "=", text (show f1)]
                                       else  [text "=", text (show f1) `withColor` red
                                             ,text ("  {"++show f2++"}") `withColor` grey]
-}                                             
presentEnr' state (EnrichedDocLevel d@(HoleEnrichedDoc) _) lay idc =
      (structural $ overlay [poly [(0,0),(1,0),(1,1),(0,1),(0,0)], text "<HoleEnr>: something is very wrong"] `withColor` black `withbgColor` yellow `withFont'` ("Courier New", 10)
      , lay, idc)
presentEnr' state (EnrichedDocLevel d@(ParseErrEnrichedDoc node pres) _) lay idc = 
     (loc node $ parsing $ overlay [pres, poly [(0,0),(1,0),(1,1),(0,1),(0,0)] `withColor` red ] `withbgColor` lightGrey
     , lay, idc)

