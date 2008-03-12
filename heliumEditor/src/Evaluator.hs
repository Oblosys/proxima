module Evaluator where

import Common.CommonTypes
import Evaluation.EvalLayerTypes
import Evaluation.EvalLayerUtils

import Evaluation.DocumentEdit
import DocumentEdit_Generated
import DocTypes_Generated

--import EvaluateTypes
import EvaluateTypesStubs

instance EvaluationSheet Document EnrichedDoc ClipDoc where
  evaluationSheet state oldDocLvl (EnrichedDocLevel oldEnr oldEnrFocus) 
                        docEdit docLvl@(DocumentLevel doc docFocus clip) = 
    let enr = evalDoc state oldEnr docLvl
        enr' = case docEdit of
                 EvaluateDoc' -> evalTypes enr
                 _            -> enr             
    in  return (SetEnr' (EnrichedDocLevel enr' docFocus), state, docLvl)


evalTypes :: EnrichedDoc -> EnrichedDoc
evalTypes (RootEnr (RootE idp dcls idlDcls) oldTypes doc) = 
  let (errs, env, tps) = evaluate doc
  in  debug Prs ("ERRS AND TYPES: "++show errs++show tps) $
      RootEnr (RootE idp dcls idlDcls) (errs, tps, env) doc


getOldTypeInfo (RootEnr _ oldTypes _) = oldTypes 
getOldTypeInfo (HoleEnrichedDoc)                    = ([],[],[])
getOldTypeInfo (ParseErrEnrichedDoc _)            = ([],[],[])

-- in case of a parse err, don't duplicate, because parser of idList will fail. What to do with parse errs?
evalDoc :: LayerStateEval -> EnrichedDoc -> DocumentLevel Document clip -> EnrichedDoc
evalDoc state enr (DocumentLevel doc@(RootDoc (ParseErrRoot prs)) _ _) = RootEnr (ParseErrRootE prs) (getOldTypeInfo enr) doc
evalDoc state enr (DocumentLevel doc@(RootDoc (Root idp dcls)) _ _) = RootEnr (RootE idp dcls dcls) (getOldTypeInfo enr) doc
evalDoc state enr (DocumentLevel (HoleDocument) _ _) = HoleEnrichedDoc
evalDoc state enr (DocumentLevel (ParseErrDocument pr) _ _) = ParseErrEnrichedDoc pr
