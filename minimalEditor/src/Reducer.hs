module Reducer (reductionSheet) where

import Common.CommonTypes hiding (Dirty (..))
import Evaluation.EvalLayerTypes

import Evaluation.EvalLayerUtils

import DocTypes_Generated
import DocUtils_Generated
import DocumentEdit_Generated


instance ReductionSheet Document EnrichedDoc ClipDoc where
  reductionSheetSimplest (RootEnr root tree)            = RootDoc root tree
  reductionSheetSimplest HoleEnrichedDoc           = HoleDocument
  reductionSheetSimplest (ParseErrEnrichedDoc prs) = ParseErrDocument prs
