module Evaluator (evaluationSheet) where

import Common.CommonTypes
import Evaluation.EvalLayerTypes
import Evaluation.EvalLayerUtils

import DocTypes_Generated
import DocUtils_Generated

instance EvaluationSheet Document EnrichedDoc ClipDoc where
  evaluationSheetSimplest (RootDoc root)        = RootEnr $ evaluateRoot root
  evaluationSheetSimplest HoleDocument          = HoleEnrichedDoc
  evaluationSheetSimplest (ParseErrDocument pr) = ParseErrEnrichedDoc pr -- not the right node type

-- The extra doc parameter is necessary for popups in the current version of Proxima

evaluateRoot (Root graph probtables title sections) =
  Root graph probtables title (setProbtablesSectionList probtableMap sections)
 where probtableMap = map probtableMapEntry (fromList_Probtable probtables)
       probtableMapEntry probtable@(Probtable id _ _) = (id,probtable)

type ProbtableMap = [(Int, Probtable)]

setProbtablesSectionList :: ProbtableMap -> List_Section -> List_Section
setProbtablesSectionList probtableMap sections = 
  toList_Section $ setProbtablesSections probtableMap $ fromList_Section sections

setProbtablesSections probtableMap []                                           = []
setProbtablesSections probtableMap (Section title paras subsections : sections) = 
  Section title 
          (toList_Paragraph $ setProbtablesParas probtableMap (fromList_Paragraph paras))
          subsections :
  setProbtablesSections probtableMap sections     


setProbtablesParas probtableMap [] = []
setProbtablesParas probtableMap (ProbtablePara (Probtable id _ _):paras) = 
  let probtable' = case lookup id probtableMap of
                     Just probtable -> probtable
                     Nothing        -> Probtable id (toList_Value []) $
                                         Table [] (toList_Axis []) (toList_Probability [])
  in  ProbtablePara probtable' : setProbtablesParas probtableMap paras
setProbtablesParas probtableMap (para:paras)                    = 
  para : setProbtablesParas probtableMap paras
