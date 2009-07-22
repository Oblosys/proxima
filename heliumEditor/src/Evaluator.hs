module Evaluator where

import Common.CommonTypes
import Evaluation.EvalLayerTypes
import Evaluation.EvalLayerUtils

import Evaluation.DocumentEdit
import DocumentEdit_Generated
import DocTypes_Generated

import EvaluateTypes
--import EvaluateTypesStubs

instance EvaluationSheet Document EnrichedDoc ClipDoc where
  evaluationSheet state oldDocLvl (EnrichedDocLevel oldEnr oldEnrFocus _) 
                        docEdit docLvl@(DocumentLevel doc docFocus clip) = 
    let enr = evalDoc state oldEnr docLvl
        enr' = case docEdit of
                 EvaluateDoc' -> evalTypes doc enr
                 _            -> enr             
    in  return (SetEnr' (EnrichedDocLevel enr' docFocus doc), state, docLvl)


evalTypes :: Document -> EnrichedDoc -> EnrichedDoc
evalTypes doc (RootEnr rootE _) = 
  let (errs, env, tps) = evaluate doc
  in  debug Eva ("ERRS AND TYPES: "++show errs++show tps++"\n"++unlines (map show env)) $
      RootEnr rootE (errs, tps, env)


getOldTypeInfo (RootEnr _ oldTypes)    = oldTypes 
getOldTypeInfo (HoleEnrichedDoc)       = ([],[],[])
getOldTypeInfo (ParseErrEnrichedDoc _) = ([],[],[])

-- in case of a parse err, don't duplicate, because parser of idList will fail. What to do with parse errs?
evalDoc :: LayerStateEval doc clip -> EnrichedDoc -> DocumentLevel Document clip -> EnrichedDoc
evalDoc state enr (DocumentLevel (RootDoc (Root idp dcls)) _ _) = RootEnr (RootE idp dcls dcls) (getOldTypeInfo enr)
evalDoc state enr (DocumentLevel (HoleDocument) _ _) = HoleEnrichedDoc
evalDoc state enr (DocumentLevel (ParseErrDocument e) _ _) = ParseErrEnrichedDoc e
