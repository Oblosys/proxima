module Evaluator (evaluationSheet) where

import Common.CommonTypes
import Evaluation.EvalLayerTypes
import Evaluation.EvalLayerUtils

import DocTypes_Generated
import DocUtils_Generated

import Data.Generics

instance EvaluationSheet Document EnrichedDoc ClipDoc where
  evaluationSheetSimplest (RootDoc root)        = RootEnr $ evaluateRoot root
  evaluationSheetSimplest HoleDocument          = HoleEnrichedDoc
  evaluationSheetSimplest (ParseErrDocument pr) = ParseErrEnrichedDoc pr -- not the right node type

-- The extra doc parameter is necessary for popups in the current version of Proxima


evaluateRoot (Root graph probtables title sections) =
  Root graph probtables title (everywhere (mkT (setProbtablePara probtableMap)) sections)
 where probtableMap = map probtableMapEntry (fromList_Probtable probtables)
       probtableMapEntry probtable@(Probtable id _ _) = (id,probtable)

type ProbtableMap = [(Int, Probtable)]

setProbtablePara probtableMap (ProbtablePara (Probtable id _ _)) = 
  let probtable' = case lookup id probtableMap of
                     Just probtable -> probtable
                     Nothing        -> Probtable id (toList_Value []) $
                                         Table [] (toList_Axis []) (toList_Probability [])
  in  ProbtablePara probtable'
setProbtablePara probtableMap para = para
