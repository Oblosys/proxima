module EvalPresent where

import CommonTypes
import EvalLayerTypes
import EvalLayerUtils

import DocumentEdit

--import EvaluateTypes
import EvaluateTypesStubs


{-

Inserted deleted lists are not passed around correctly yet

make Module element, so Root can get rid of ParseErr and Hole

fix up translate and present modules
 caps in ..lvl: eg docLvl instead of doclvl
 nice case
 clear handling of set and skip

BUG Nil in decls kills the entire presentation

What node types should exist and where to define them?

top level Enr should be called EnrichedDoc (in node, makestructural, etc)


-}

presentIO :: LayerStateEval -> DocumentLevel -> EnrichedDocLevel -> EditDocument' -> IO (EditEnrichedDoc', LayerStateEval, DocumentLevel)
presentIO  state high low@(EnrichedDocLevel enr focus) editHigh =
  let (editLow, state', high') = eval state high low editHigh
  in do { -- debugLnIO Prs ("editDoc':"++show editHigh)
        --; debugLnIO Prs ("editEnr':"++show editLow)
        ; return $ (editLow, state', high')
        }

-- type evaluation is currently an edit op on doc.

eval state docLvl (EnrichedDocLevel enr oldFocus) (SkipDoc' 0) =
  (SetEnr' (EnrichedDocLevel enr oldFocus), state, docLvl)  -- we should re-evaluate here because of local state
eval state doclvl enr                            (SkipDoc' i) = (SkipEnr' (i-1), state, doclvl)
eval state (DocumentLevel doc focusD clipD) (EnrichedDocLevel enr _) (SetDoc' d {- (inss, dels) -})  = 
  let (enr')      = evalDoc state (DocumentLevel d NoPathD clipD) enr -- should not reuse focus from old Doc      
  in  (SetEnr' (EnrichedDocLevel enr' focusD), state, DocumentLevel d NoPathD clipD)
eval state doclvl@(DocumentLevel doc focusD clipD) (EnrichedDocLevel enr _) (EvaluateDoc') =
  let (enr')                  = evalDoc state doclvl enr
      (enr'')                 = evalTypes enr'
  in  (SetEnr' (EnrichedDocLevel enr'' focusD), state, doclvl)
eval state doclvl@(DocumentLevel doc focusD clipD) (EnrichedDocLevel enr _) docEdit = debug Eva ("DocNavigate"++show focusD) $
  let (DocumentLevel doc' focusD' clipD',state') = setUpd AllUpdated $ editDoc state doclvl docEdit
      (enr')                  = evalDoc state' (DocumentLevel doc' focusD' clipD') enr
  in  (SetEnr' (EnrichedDocLevel enr' focusD'), state', DocumentLevel doc' focusD' clipD')




-- TODO: make sure that document is parsed before doing these:
editDoc :: LayerStateEval -> DocumentLevel -> EditDocument' -> (DocumentLevel, LayerStateEval)
editDoc state doclvl                        (UpdateDoc' upd) = (upd doclvl, state)
editDoc state (DocumentLevel doc pth clipD) NavUpDoc'        = ((DocumentLevel doc (navigateUpD pth doc) clipD), state)
editDoc state (DocumentLevel doc pth clipD) NavDownDoc'      = ((DocumentLevel doc (navigateDownD pth doc) clipD), state)
editDoc state (DocumentLevel doc pth clipD) NavLeftDoc'      = ((DocumentLevel doc (navigateLeftD pth doc) clipD), state)
editDoc state (DocumentLevel doc pth clipD) NavRightDoc'     = ((DocumentLevel doc (navigateRightD pth doc) clipD), state)
editDoc state doclvl                        CutDoc'          = (editCutD doclvl, state)
editDoc state doclvl                        CopyDoc'         = (editCopyD doclvl, state)
editDoc state doclvl                        PasteDoc'        = (editPasteD doclvl, state)
editDoc state doclvl                        DeleteDoc'       = (editDeleteD doclvl, state)
editDoc state doclvl                        op               = debug Err ("EvalPresent:unhandled doc edit: "++show op) (doclvl, state)
{-
editDoc state doclvl@(DocumentLevel doc pth clipD) EvaluateDoc'     =
  let (errs, env, tps) = evaluate doc
  in  --debug Prs (show errs++show tps) $
      (doclvl, (errs, tps, env))
-}
-- type evaluation is only done at explicit edit command (F2) because it is expensive

-- add the computed types to the enriched document root
evalTypes :: EnrichedDoc -> EnrichedDoc
evalTypes (RootEnr idd idp dcls dcls' oldTypes doc) = 
  let (errs, env, tps) = evaluate doc
  in  debug Prs ("ERRS AND TYPES: "++show errs++show tps) $
      RootEnr idd idp dcls dcls' (errs, tps, env) doc


getOldTypeInfo (RootEnr _ _ _ _ oldTypes _) = oldTypes 
getOldTypeInfo (HoleEnrichedDoc)                    = ([],[],[])
getOldTypeInfo (ParseErrEnrichedDoc _ _)            = ([],[],[])

-- in case of a parse err, don't duplicate, because parser of idList will fail. What to do with parse errs?
evalDoc :: LayerStateEval -> DocumentLevel -> EnrichedDoc -> EnrichedDoc
evalDoc state (DocumentLevel doc@(RootDoc idd idp dcls@(ParseErrList_Decl _ _)) _ _) enr = RootEnr idd idp (List_Decl NoIDD Nil_Decl) dcls (getOldTypeInfo enr) doc
evalDoc state (DocumentLevel doc@(RootDoc idd idp dcls) _ _) enr = RootEnr idd idp dcls dcls (getOldTypeInfo enr) doc
evalDoc state (DocumentLevel (HoleDoc) _ _) _ = HoleEnrichedDoc
evalDoc state (DocumentLevel (ParseErrDoc nd pr) _ _) _ = ParseErrEnrichedDoc nd pr -- not the right node type



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
