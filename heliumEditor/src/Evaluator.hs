module Evaluator where

import CommonTypes
import EvalLayerTypes
import EvalLayerUtils

import DocumentEdit
import DocumentEdit_Generated
import DocTypes_Generated

--import EvaluateTypes
import EvaluateTypesStubs


{-


make Module element, so Root can get rid of ParseErr and Hole

fix up translate and present modules
 caps in ..lvl: eg docLvl instead of doclvl
 nice case
 clear handling of set and skip

BUG Nil in decls kills the entire presentation

What node types should exist and where to define them?

top level Enr should be called EnrichedDoc (in node, makestructural, etc)


-}
instance EvaluationSheet Document EnrichedDoc ClipDoc where
  evaluationSheet state oldDocLvl (EnrichedDocLevel oldEnr oldEnrFocus) 
                        docEdit docLvl@(DocumentLevel doc docFocus clip) = 
    let enr = evalDoc state oldEnr docLvl
        enr' = case docEdit of
                 EvaluateDoc' -> evalTypes enr
                 _            -> enr             
    in  return (SetEnr' (EnrichedDocLevel enr' docFocus), state, docLvl)


evalTypes :: EnrichedDoc -> EnrichedDoc
evalTypes (RootEnr idd1 (RootE idd2 idp dcls idlDcls) oldTypes doc) = 
  let (errs, env, tps) = evaluate doc
  in  debug Prs ("ERRS AND TYPES: "++show errs++show tps) $
      RootEnr idd1 (RootE idd2 idp dcls idlDcls) (errs, tps, env) doc


getOldTypeInfo (RootEnr _ _ oldTypes _) = oldTypes 
getOldTypeInfo (HoleEnrichedDoc)                    = ([],[],[])
getOldTypeInfo (ParseErrEnrichedDoc _)            = ([],[],[])

-- in case of a parse err, don't duplicate, because parser of idList will fail. What to do with parse errs?
evalDoc :: LayerStateEval -> EnrichedDoc -> DocumentLevel Document clip -> EnrichedDoc
evalDoc state enr (DocumentLevel doc@(RootDoc idd1 (ParseErrRoot prs)) _ _) = RootEnr idd1 (ParseErrRootE prs) (getOldTypeInfo enr) doc
evalDoc state enr (DocumentLevel doc@(RootDoc idd1 (Root idd2 idp dcls)) _ _) = RootEnr idd1 (RootE idd2 idp dcls dcls) (getOldTypeInfo enr) doc
evalDoc state enr (DocumentLevel (HoleDocument) _ _) = HoleEnrichedDoc
evalDoc state enr (DocumentLevel (ParseErrDocument pr) _ _) = ParseErrEnrichedDoc pr


{-

Focus:

Focus is not completely right yet. Instead of passing the focus from gest int. to presenter for every edit, all levels
should keep a focus. This will allow the nice up/down navigation.

Also, a skip doc, operation will reinstall the old pres focus, which is not right if an up down has been performed on
arrangement focus. There is a choice here. Either every level has correct focus, or a skip does lead to a focus update
on the lower level. In the last case, the focus moves up and down only when required. This might lead to some difficult
administration though.

Probably everything will be ok, once we have Level types on all levels, then the passed datastructure will always contain
the current focus, and it is passed only when the lower level cannot handel it.

When we stay at lower levels, the focus will not be correct at the higher levels, this is similar edit ops that are 
short cut at lower levels. Maybe we need a distinction though. An edit on the pres tree without a reparse should signal
that the document is not consistent with the presentation yet, but if only the focus has changed, we don't want that signal.

The difference we have here might be that the focus updates are LS updates.

For now, we fix it by having edit ops that do a skip doc, also set the presentation focus.


edit, compute focus after edit, compute focus in terms of position in string repr.

present: recomputes focus in updated tree from string position except for skip, because then focus is ok.

arrange: translate focusP to focusA

structural problem: if presentation before focus changes in size, the focus is incorrect.
-- this will be solved by having a document focus.


focus on presentation requires rearrange after each focus move. This does not seem to be what we want
will we allow the presentation to be influenced by the focus? This will be even more expensive

mouse handling stuff seems to call for a backtrack in edit levels, try highest level, if fail try lower.
This is not part of the model yet



BUG copy depends on direction!!
-}
