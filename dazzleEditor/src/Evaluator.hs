module Evaluator (evaluationSheet) where

import CommonTypes
import EvalLayerTypes
import EvalLayerUtils

import DocTypes_Generated

instance EvaluationSheet Document EnrichedDoc ClipDoc where
  evaluationSheetSimplest doc@(RootDoc  root)   = RootEnr root doc
  evaluationSheetSimplest HoleDocument          = HoleEnrichedDoc
  evaluationSheetSimplest (ParseErrDocument pr) = ParseErrEnrichedDoc pr -- not the right node type

-- The extra doc parameter is necessary for popups in the current version of Proxima