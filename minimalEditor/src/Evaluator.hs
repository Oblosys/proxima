module Evaluator (evaluationSheet) where

import Common.CommonTypes
import Evaluation.EvalLayerTypes
import Evaluation.EvalLayerUtils

import DocTypes_Generated

instance EvaluationSheet Document EnrichedDoc ClipDoc where
  evaluationSheetSimplest (RootDoc root tree)        = RootEnr root tree
  evaluationSheetSimplest HoleDocument          = HoleEnrichedDoc
  evaluationSheetSimplest (ParseErrDocument pr) = ParseErrEnrichedDoc pr 