

-- UUAGC 0.9.6 (src/PresentationAG.ag)
module PresentationAG where
{-# LINE 2 "src/PresentationAG.ag" #-}

import Common.CommonTypes hiding (Dirty (..))
import Common.CommonUtils
import GHC.Float (formatRealFloat, FFFormat(FFFixed))

import Presentation.PresLayerTypes
import Presentation.PresLayerUtils

import Presentation.XprezLib
import Presentation.XLatex hiding (bold)

import Evaluation.DocumentEdit

import List
import Maybe
import qualified Data.Map as Map
import Data.Map (Map)


import Evaluation.DocTypes (DocumentLevel (..))
import DocTypes_Generated
import DocUtils_Generated
import DocumentEdit_Generated

import ProxParser   
{-# LINE 32 "src/PresentationAG.hs" #-}
{-# LINE 117 "src/PresentationAG.ag" #-}

addCurrencyItems currencies path pres = 
  pres `addPopupItems` [ (name, setCurrencyIx path ix) | (ix, (name, _)) <- zip [0..] currencies ]

addNewCurrency :: Path -> UpdateDoc Document ClipDoc
addNewCurrency pth =
  insertElementAtEnd newCurrency pth
  
newCurrency = Clip_Currency $ Currency "new" 0.0

setCurrencyIx :: Path -> Int -> UpdateDoc Document ClipDoc
setCurrencyIx pth newCurrencyIx= 
  \(DocumentLevel doc focus clip) ->
    let doc' = case selectD pth doc of
                 Clip_Expense (Expense desc amount _) -> 
                   pasteD pth (Clip_Expense (Expense desc amount newCurrencyIx)) doc
                 _ -> doc
    in  DocumentLevel doc' focus clip

-- Document utils

-- PRECONDITION: path points to a list element of the same type as clip
addElementAfter :: ClipDoc -> Path -> UpdateDoc Document ClipDoc
addElementAfter clip path = 
  \(DocumentLevel d pth cl) ->
    (DocumentLevel (insertListD (init path) (last path+1) clip d) pth cl)

-- PRECONDITION: path points to a list of elements of the same type as clip
-- NB path points to the list, rather than to an element.
insertElementAtHead :: ClipDoc -> Path -> UpdateDoc Document ClipDoc
insertElementAtHead  clip listPath = 
  \(DocumentLevel d pth cl) ->
    (DocumentLevel (insertListD listPath 0 clip d) pth cl)

-- PRECONDITION: path points to a list of elements of the same type as clip
-- NB path points to the list, rather than to an element.
insertElementAtEnd :: ClipDoc -> Path -> UpdateDoc Document ClipDoc
insertElementAtEnd  clip listPath = 
  \(DocumentLevel d pth cl) ->
    let list = selectD listPath d
    in  (DocumentLevel (insertListD listPath (arityClip list) clip d) pth cl)

deleteAtPath :: Path -> UpdateDoc Document ClipDoc
deleteAtPath pth =
   \(DocumentLevel d _ cl) ->
     editCutD (DocumentLevel d (PathD pth) cl)

-- Presentation utils

leftTopMargins left top pres = 
  row [ hSpace left, col [ vSpace top, pres, empty `withStretch` True ], empty `withStretch` True] 
  `withStretch` True
  
textField w str = boxed $ row [text str, empty `withHStretch` True]  `withbgColor` white `withWidth` w  

-- Utils


showCurrency :: Float -> String
showCurrency f =
    let s = formatRealFloat FFFixed (Just 2) f
        s' = reverse s -- s -- dropWhile (== '0') (reverse s)
        s'' = if head s' == '.' then '0':s' else s'
    in reverse s''
{-# LINE 98 "src/PresentationAG.hs" #-}

{-# LINE 235 "src/PresentationAG.ag" #-}

flipCompleted pth (BasicTask descr completed) = pasteAt (Clip_Task (BasicTask descr (not completed))) pth

flipExpanded pth (CompositeTask expanded descr subtasks) = pasteAt (Clip_Task (CompositeTask (not expanded) descr subtasks)) pth

setCompletedRec completed pth task = pasteAt (Clip_Task (setCompletedRec' task)) pth
 where setCompletedRec' (BasicTask descr _) = BasicTask descr completed
       setCompletedRec' (CompositeTask exp descr subtasks) = CompositeTask exp descr $ toList_Task $ map setCompletedRec' (fromList_Task subtasks)

presentCompleted completed =
  boxed (text ch) `withFontFam` "Courier" 
 where ch = if completed then "v" else " "


pasteAt :: ClipDoc -> Path -> UpdateDoc Document ClipDoc
pasteAt clip pth =
  \(DocumentLevel d path cl) ->
    let (DocumentLevel d' _ _) = editPasteD (DocumentLevel d (PathD pth) clip)
    in  (DocumentLevel d' path cl)
{-# LINE 120 "src/PresentationAG.hs" #-}

{-# LINE 9 "src/PresentationAG_Generated.ag" #-}

-- type PresentationSheet doc enr node clip token = 
--        enr -> doc -> FocusDoc -> WhitespaceMap -> IDPCounter -> 
--        (WhitespaceMap, IDPCounter, Presentation doc node clip token)

presentationSheet :: PresentationSheet Document EnrichedDoc Node ClipDoc UserToken
presentationSheet enrichedDoc document focusD whitespaceMap pIdC = 
  let (Syn_EnrichedDoc pIdC' pres self whitespaceMap') = 
        wrap_EnrichedDoc (sem_EnrichedDoc enrichedDoc) (Inh_EnrichedDoc document focusD pIdC [] whitespaceMap)
  in  (whitespaceMap', pIdC', pres)

{- 
A type error here means that extra attributes were declared on EnrichedDoc
The attribute signature for EnrichedDoc should be:

EnrichedDoc  [ focusD : FocusDoc path : Path
             | pIdC : Int layoutMap : WhitespaceMap
             | pres : Presentation_Doc_Node_Clip_Token EnrichedDoc 
             ]
-}
{-# LINE 143 "src/PresentationAG.hs" #-}
-- ChoiceDoc ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         doc                  : Document
         focusD               : FocusDoc
      chained attributes:
         ix                   : Int
         pIdC                 : Int
         path                 : Path
         whitespaceMap        : WhitespaceMap
      synthesized attributes:
         parseErrors          : [ParseErrorMessage]
         pres                 : Presentation_Doc_Node_Clip_Token
         presTree             : Presentation_Doc_Node_Clip_Token
         presXML              : Presentation_Doc_Node_Clip_Token
         self                 : SELF 
   alternatives:
      alternative FormDoc:
         child form           : Form 
         visit 0:
            local pres        : _
            local self        : _
      alternative HoleChoiceDoc:
         visit 0:
            local self        : _
      alternative ParseErrChoiceDoc:
         child error          : {(ParseError Document Node ClipDoc UserToken)}
         visit 0:
            local self        : _
      alternative TaskDoc:
         child tasks          : Tasks 
         visit 0:
            local pres        : _
            local self        : _
-}
-- cata
sem_ChoiceDoc :: ChoiceDoc  ->
                 T_ChoiceDoc 
sem_ChoiceDoc (FormDoc _form )  =
    (sem_ChoiceDoc_FormDoc (sem_Form _form ) )
sem_ChoiceDoc (HoleChoiceDoc )  =
    (sem_ChoiceDoc_HoleChoiceDoc )
sem_ChoiceDoc (ParseErrChoiceDoc _error )  =
    (sem_ChoiceDoc_ParseErrChoiceDoc _error )
sem_ChoiceDoc (TaskDoc _tasks )  =
    (sem_ChoiceDoc_TaskDoc (sem_Tasks _tasks ) )
-- semantic domain
type T_ChoiceDoc  = Document ->
                    FocusDoc ->
                    Int ->
                    Int ->
                    Path ->
                    WhitespaceMap ->
                    ( Int,Int,([ParseErrorMessage]),Path,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,ChoiceDoc,WhitespaceMap)
data Inh_ChoiceDoc  = Inh_ChoiceDoc {doc_Inh_ChoiceDoc :: Document,focusD_Inh_ChoiceDoc :: FocusDoc,ix_Inh_ChoiceDoc :: Int,pIdC_Inh_ChoiceDoc :: Int,path_Inh_ChoiceDoc :: Path,whitespaceMap_Inh_ChoiceDoc :: WhitespaceMap}
data Syn_ChoiceDoc  = Syn_ChoiceDoc {ix_Syn_ChoiceDoc :: Int,pIdC_Syn_ChoiceDoc :: Int,parseErrors_Syn_ChoiceDoc :: [ParseErrorMessage],path_Syn_ChoiceDoc :: Path,pres_Syn_ChoiceDoc :: Presentation_Doc_Node_Clip_Token,presTree_Syn_ChoiceDoc :: Presentation_Doc_Node_Clip_Token,presXML_Syn_ChoiceDoc :: Presentation_Doc_Node_Clip_Token,self_Syn_ChoiceDoc :: ChoiceDoc,whitespaceMap_Syn_ChoiceDoc :: WhitespaceMap}
wrap_ChoiceDoc :: T_ChoiceDoc  ->
                  Inh_ChoiceDoc  ->
                  Syn_ChoiceDoc 
wrap_ChoiceDoc sem (Inh_ChoiceDoc _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )  =
    (let ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap) =
             (sem _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )
     in  (Syn_ChoiceDoc _lhsOix _lhsOpIdC _lhsOparseErrors _lhsOpath _lhsOpres _lhsOpresTree _lhsOpresXML _lhsOself _lhsOwhitespaceMap ))
sem_ChoiceDoc_FormDoc :: T_Form  ->
                         T_ChoiceDoc 
sem_ChoiceDoc_FormDoc form_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _formOpIdC :: Int
              _lhsOpIdC :: Int
              _formOpath :: Path
              _formOix :: Int
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpath :: Path
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: ChoiceDoc
              _lhsOix :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              _formOdoc :: Document
              _formOfocusD :: FocusDoc
              _formOwhitespaceMap :: WhitespaceMap
              _formIix :: Int
              _formIpIdC :: Int
              _formIparseErrors :: ([ParseErrorMessage])
              _formIpath :: Path
              _formIpres :: Presentation_Doc_Node_Clip_Token
              _formIpresTree :: Presentation_Doc_Node_Clip_Token
              _formIpresXML :: Presentation_Doc_Node_Clip_Token
              _formIself :: Form
              _formIwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG.ag"(line 38, column 7)
              _pres =
                  {-# LINE 38 "src/PresentationAG.ag" #-}
                  structural $ _formIpres `withbgColor` lightBlue
                  {-# LINE 245 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 152, column 7)
              _formOpIdC =
                  {-# LINE 152 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 0
                  {-# LINE 250 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 153, column 7)
              _lhsOpIdC =
                  {-# LINE 153 "src/PresentationAG_Generated.ag" #-}
                  _formIpIdC
                  {-# LINE 255 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 154, column 7)
              _formOpath =
                  {-# LINE 154 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[0]
                  {-# LINE 260 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 160, column 5)
              _formOix =
                  {-# LINE 160 "src/PresentationAG_Generated.ag" #-}
                  0
                  {-# LINE 265 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 164, column 7)
              _lhsOpres =
                  {-# LINE 164 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_FormDoc _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 271 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 380, column 13)
              _lhsOpath =
                  {-# LINE 380 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 276 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 417, column 7)
              _lhsOpresXML =
                  {-# LINE 417 "src/PresentationAG_Generated.ag" #-}
                  presentElementXML _lhsIfocusD (Node_FormDoc _self _lhsIpath) _lhsIpath "FormDoc" [ _formIpresXML ]
                  {-# LINE 281 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 509, column 7)
              _lhsOpresTree =
                  {-# LINE 509 "src/PresentationAG_Generated.ag" #-}
                  presentElementTree _lhsIfocusD (Node_FormDoc _self _lhsIpath) _lhsIpath "FormDoc" [ _formIpresTree ]
                  {-# LINE 286 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  _formIparseErrors
                  {-# LINE 291 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  FormDoc _formIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _formIix
                  {-# LINE 302 "src/PresentationAG.hs" #-}
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _formIwhitespaceMap
                  {-# LINE 307 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _formOdoc =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 312 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _formOfocusD =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 317 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _formOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 322 "src/PresentationAG.hs" #-}
              ( _formIix,_formIpIdC,_formIparseErrors,_formIpath,_formIpres,_formIpresTree,_formIpresXML,_formIself,_formIwhitespaceMap) =
                  (form_ _formOdoc _formOfocusD _formOix _formOpIdC _formOpath _formOwhitespaceMap )
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
sem_ChoiceDoc_HoleChoiceDoc :: T_ChoiceDoc 
sem_ChoiceDoc_HoleChoiceDoc  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: ChoiceDoc
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 170, column 7)
              _lhsOpres =
                  {-# LINE 170 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "ChoiceDoc" (Node_HoleChoiceDoc _self _lhsIpath) _lhsIpath
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 348 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 420, column 23)
              _lhsOpresXML =
                  {-# LINE 420 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "ChoiceDoc" (Node_HoleChoiceDoc _self _lhsIpath) _lhsIpath
                  {-# LINE 353 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 512, column 23)
              _lhsOpresTree =
                  {-# LINE 512 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "ChoiceDoc" (Node_HoleChoiceDoc _self _lhsIpath) _lhsIpath
                  {-# LINE 358 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 363 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleChoiceDoc
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 374 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 379 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 384 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 389 "src/PresentationAG.hs" #-}
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
sem_ChoiceDoc_ParseErrChoiceDoc :: ((ParseError Document Node ClipDoc UserToken)) ->
                                   T_ChoiceDoc 
sem_ChoiceDoc_ParseErrChoiceDoc error_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOself :: ChoiceDoc
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 173, column 7)
              _lhsOpres =
                  {-# LINE 173 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrChoiceDoc _self _lhsIpath) error_
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 414 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 175, column 7)
              _lhsOparseErrors =
                  {-# LINE 175 "src/PresentationAG_Generated.ag" #-}
                  getErrorMessages error_
                  {-# LINE 419 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 421, column 23)
              _lhsOpresXML =
                  {-# LINE 421 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrChoiceDoc _self _lhsIpath) error_
                  {-# LINE 424 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 513, column 23)
              _lhsOpresTree =
                  {-# LINE 513 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrChoiceDoc _self _lhsIpath) error_
                  {-# LINE 429 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrChoiceDoc error_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 440 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 445 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 450 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 455 "src/PresentationAG.hs" #-}
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
sem_ChoiceDoc_TaskDoc :: T_Tasks  ->
                         T_ChoiceDoc 
sem_ChoiceDoc_TaskDoc tasks_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _tasksOpIdC :: Int
              _lhsOpIdC :: Int
              _tasksOpath :: Path
              _tasksOix :: Int
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpath :: Path
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: ChoiceDoc
              _lhsOix :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              _tasksOdoc :: Document
              _tasksOfocusD :: FocusDoc
              _tasksOwhitespaceMap :: WhitespaceMap
              _tasksIix :: Int
              _tasksIpIdC :: Int
              _tasksIparseErrors :: ([ParseErrorMessage])
              _tasksIpath :: Path
              _tasksIpres :: Presentation_Doc_Node_Clip_Token
              _tasksIpresTree :: Presentation_Doc_Node_Clip_Token
              _tasksIpresXML :: Presentation_Doc_Node_Clip_Token
              _tasksIself :: Tasks
              _tasksIwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG.ag"(line 40, column 7)
              _pres =
                  {-# LINE 40 "src/PresentationAG.ag" #-}
                  structural $ _tasksIpres
                  {-# LINE 494 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 156, column 7)
              _tasksOpIdC =
                  {-# LINE 156 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 0
                  {-# LINE 499 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 157, column 7)
              _lhsOpIdC =
                  {-# LINE 157 "src/PresentationAG_Generated.ag" #-}
                  _tasksIpIdC
                  {-# LINE 504 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 158, column 7)
              _tasksOpath =
                  {-# LINE 158 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[0]
                  {-# LINE 509 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 162, column 5)
              _tasksOix =
                  {-# LINE 162 "src/PresentationAG_Generated.ag" #-}
                  0
                  {-# LINE 514 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 167, column 7)
              _lhsOpres =
                  {-# LINE 167 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_TaskDoc _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 520 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 381, column 13)
              _lhsOpath =
                  {-# LINE 381 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 525 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 419, column 7)
              _lhsOpresXML =
                  {-# LINE 419 "src/PresentationAG_Generated.ag" #-}
                  presentElementXML _lhsIfocusD (Node_TaskDoc _self _lhsIpath) _lhsIpath "TaskDoc" [ _tasksIpresXML ]
                  {-# LINE 530 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 511, column 7)
              _lhsOpresTree =
                  {-# LINE 511 "src/PresentationAG_Generated.ag" #-}
                  presentElementTree _lhsIfocusD (Node_TaskDoc _self _lhsIpath) _lhsIpath "TaskDoc" [ _tasksIpresTree ]
                  {-# LINE 535 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  _tasksIparseErrors
                  {-# LINE 540 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  TaskDoc _tasksIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _tasksIix
                  {-# LINE 551 "src/PresentationAG.hs" #-}
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _tasksIwhitespaceMap
                  {-# LINE 556 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tasksOdoc =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 561 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tasksOfocusD =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 566 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tasksOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 571 "src/PresentationAG.hs" #-}
              ( _tasksIix,_tasksIpIdC,_tasksIparseErrors,_tasksIpath,_tasksIpres,_tasksIpresTree,_tasksIpresXML,_tasksIself,_tasksIwhitespaceMap) =
                  (tasks_ _tasksOdoc _tasksOfocusD _tasksOix _tasksOpIdC _tasksOpath _tasksOwhitespaceMap )
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
-- ConsList_Currency -------------------------------------------
{-
   visit 0:
      inherited attributes:
         doc                  : Document
         focusD               : FocusDoc
         ix                   : Int
         path                 : Path
      chained attributes:
         pIdC                 : Int
         whitespaceMap        : WhitespaceMap
      synthesized attributes:
         allCurrencies        : [(String,Float)]
         parseErrors          : [ParseErrorMessage]
         press                : [Presentation_Doc_Node_Clip_Token]
         pressTree            : [Presentation_Doc_Node_Clip_Token]
         pressXML             : [Presentation_Doc_Node_Clip_Token]
         self                 : SELF 
   alternatives:
      alternative Cons_Currency:
         child head           : Currency 
         child tail           : ConsList_Currency 
         visit 0:
            local self        : _
      alternative Nil_Currency:
         visit 0:
            local self        : _
-}
-- cata
sem_ConsList_Currency :: ConsList_Currency  ->
                         T_ConsList_Currency 
sem_ConsList_Currency (Cons_Currency _head _tail )  =
    (sem_ConsList_Currency_Cons_Currency (sem_Currency _head ) (sem_ConsList_Currency _tail ) )
sem_ConsList_Currency (Nil_Currency )  =
    (sem_ConsList_Currency_Nil_Currency )
-- semantic domain
type T_ConsList_Currency  = Document ->
                            FocusDoc ->
                            Int ->
                            Int ->
                            Path ->
                            WhitespaceMap ->
                            ( ([(String,Float)]),Int,([ParseErrorMessage]),([Presentation_Doc_Node_Clip_Token]),([Presentation_Doc_Node_Clip_Token]),([Presentation_Doc_Node_Clip_Token]),ConsList_Currency,WhitespaceMap)
data Inh_ConsList_Currency  = Inh_ConsList_Currency {doc_Inh_ConsList_Currency :: Document,focusD_Inh_ConsList_Currency :: FocusDoc,ix_Inh_ConsList_Currency :: Int,pIdC_Inh_ConsList_Currency :: Int,path_Inh_ConsList_Currency :: Path,whitespaceMap_Inh_ConsList_Currency :: WhitespaceMap}
data Syn_ConsList_Currency  = Syn_ConsList_Currency {allCurrencies_Syn_ConsList_Currency :: [(String,Float)],pIdC_Syn_ConsList_Currency :: Int,parseErrors_Syn_ConsList_Currency :: [ParseErrorMessage],press_Syn_ConsList_Currency :: [Presentation_Doc_Node_Clip_Token],pressTree_Syn_ConsList_Currency :: [Presentation_Doc_Node_Clip_Token],pressXML_Syn_ConsList_Currency :: [Presentation_Doc_Node_Clip_Token],self_Syn_ConsList_Currency :: ConsList_Currency,whitespaceMap_Syn_ConsList_Currency :: WhitespaceMap}
wrap_ConsList_Currency :: T_ConsList_Currency  ->
                          Inh_ConsList_Currency  ->
                          Syn_ConsList_Currency 
wrap_ConsList_Currency sem (Inh_ConsList_Currency _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )  =
    (let ( _lhsOallCurrencies,_lhsOpIdC,_lhsOparseErrors,_lhsOpress,_lhsOpressTree,_lhsOpressXML,_lhsOself,_lhsOwhitespaceMap) =
             (sem _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )
     in  (Syn_ConsList_Currency _lhsOallCurrencies _lhsOpIdC _lhsOparseErrors _lhsOpress _lhsOpressTree _lhsOpressXML _lhsOself _lhsOwhitespaceMap ))
sem_ConsList_Currency_Cons_Currency :: T_Currency  ->
                                       T_ConsList_Currency  ->
                                       T_ConsList_Currency 
sem_ConsList_Currency_Cons_Currency head_ tail_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _headOpath :: Path
              _tailOpath :: Path
              _lhsOpress :: ([Presentation_Doc_Node_Clip_Token])
              _headOpIdC :: Int
              _tailOpIdC :: Int
              _lhsOpIdC :: Int
              _tailOix :: Int
              _lhsOpressXML :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOpressTree :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOallCurrencies :: ([(String,Float)])
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: ConsList_Currency
              _lhsOwhitespaceMap :: WhitespaceMap
              _headOdoc :: Document
              _headOfocusD :: FocusDoc
              _headOix :: Int
              _headOwhitespaceMap :: WhitespaceMap
              _tailOdoc :: Document
              _tailOfocusD :: FocusDoc
              _tailOwhitespaceMap :: WhitespaceMap
              _headIallCurrencies :: ([(String,Float)])
              _headIix :: Int
              _headIpIdC :: Int
              _headIparseErrors :: ([ParseErrorMessage])
              _headIpath :: Path
              _headIpres :: Presentation_Doc_Node_Clip_Token
              _headIpresTree :: Presentation_Doc_Node_Clip_Token
              _headIpresXML :: Presentation_Doc_Node_Clip_Token
              _headIself :: Currency
              _headIwhitespaceMap :: WhitespaceMap
              _tailIallCurrencies :: ([(String,Float)])
              _tailIpIdC :: Int
              _tailIparseErrors :: ([ParseErrorMessage])
              _tailIpress :: ([Presentation_Doc_Node_Clip_Token])
              _tailIpressTree :: ([Presentation_Doc_Node_Clip_Token])
              _tailIpressXML :: ([Presentation_Doc_Node_Clip_Token])
              _tailIself :: ConsList_Currency
              _tailIwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 352, column 7)
              _headOpath =
                  {-# LINE 352 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[_lhsIix]
                  {-# LINE 679 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 353, column 7)
              _tailOpath =
                  {-# LINE 353 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 684 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 354, column 7)
              _lhsOpress =
                  {-# LINE 354 "src/PresentationAG_Generated.ag" #-}
                  _headIpres : _tailIpress
                  {-# LINE 689 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 355, column 7)
              _headOpIdC =
                  {-# LINE 355 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 30
                  {-# LINE 694 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 356, column 7)
              _tailOpIdC =
                  {-# LINE 356 "src/PresentationAG_Generated.ag" #-}
                  _headIpIdC
                  {-# LINE 699 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 357, column 7)
              _lhsOpIdC =
                  {-# LINE 357 "src/PresentationAG_Generated.ag" #-}
                  _tailIpIdC
                  {-# LINE 704 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 358, column 7)
              _tailOix =
                  {-# LINE 358 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix + 1
                  {-# LINE 709 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 493, column 19)
              _lhsOpressXML =
                  {-# LINE 493 "src/PresentationAG_Generated.ag" #-}
                  _headIpresXML : _tailIpressXML
                  {-# LINE 714 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 585, column 19)
              _lhsOpressTree =
                  {-# LINE 585 "src/PresentationAG_Generated.ag" #-}
                  _headIpresTree : _tailIpressTree
                  {-# LINE 719 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 96, column 26)
              _lhsOallCurrencies =
                  {-# LINE 96 "src/PresentationAG.ag" #-}
                  _headIallCurrencies ++ _tailIallCurrencies
                  {-# LINE 724 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  _headIparseErrors ++ _tailIparseErrors
                  {-# LINE 729 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Cons_Currency _headIself _tailIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _tailIwhitespaceMap
                  {-# LINE 740 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOdoc =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 745 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOfocusD =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 750 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOix =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 755 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 760 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tailOdoc =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 765 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tailOfocusD =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 770 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _tailOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _headIwhitespaceMap
                  {-# LINE 775 "src/PresentationAG.hs" #-}
              ( _headIallCurrencies,_headIix,_headIpIdC,_headIparseErrors,_headIpath,_headIpres,_headIpresTree,_headIpresXML,_headIself,_headIwhitespaceMap) =
                  (head_ _headOdoc _headOfocusD _headOix _headOpIdC _headOpath _headOwhitespaceMap )
              ( _tailIallCurrencies,_tailIpIdC,_tailIparseErrors,_tailIpress,_tailIpressTree,_tailIpressXML,_tailIself,_tailIwhitespaceMap) =
                  (tail_ _tailOdoc _tailOfocusD _tailOix _tailOpIdC _tailOpath _tailOwhitespaceMap )
          in  ( _lhsOallCurrencies,_lhsOpIdC,_lhsOparseErrors,_lhsOpress,_lhsOpressTree,_lhsOpressXML,_lhsOself,_lhsOwhitespaceMap)))
sem_ConsList_Currency_Nil_Currency :: T_ConsList_Currency 
sem_ConsList_Currency_Nil_Currency  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpress :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOpressXML :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOpressTree :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOallCurrencies :: ([(String,Float)])
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: ConsList_Currency
              _lhsOpIdC :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 359, column 23)
              _lhsOpress =
                  {-# LINE 359 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 801 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 494, column 19)
              _lhsOpressXML =
                  {-# LINE 494 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 806 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 586, column 19)
              _lhsOpressTree =
                  {-# LINE 586 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 811 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 96, column 26)
              _lhsOallCurrencies =
                  {-# LINE 96 "src/PresentationAG.ag" #-}
                  []
                  {-# LINE 816 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 821 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Nil_Currency
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 832 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 837 "src/PresentationAG.hs" #-}
          in  ( _lhsOallCurrencies,_lhsOpIdC,_lhsOparseErrors,_lhsOpress,_lhsOpressTree,_lhsOpressXML,_lhsOself,_lhsOwhitespaceMap)))
-- ConsList_Expense --------------------------------------------
{-
   visit 0:
      inherited attributes:
         allCurrencies        : [(String,Float)]
         doc                  : Document
         focusD               : FocusDoc
         ix                   : Int
         path                 : Path
      chained attributes:
         pIdC                 : Int
         whitespaceMap        : WhitespaceMap
      synthesized attributes:
         parseErrors          : [ParseErrorMessage]
         press                : [Presentation_Doc_Node_Clip_Token]
         pressTree            : [Presentation_Doc_Node_Clip_Token]
         pressXML             : [Presentation_Doc_Node_Clip_Token]
         self                 : SELF 
         total                : Float
   alternatives:
      alternative Cons_Expense:
         child head           : Expense 
         child tail           : ConsList_Expense 
         visit 0:
            local self        : _
      alternative Nil_Expense:
         visit 0:
            local self        : _
-}
-- cata
sem_ConsList_Expense :: ConsList_Expense  ->
                        T_ConsList_Expense 
sem_ConsList_Expense (Cons_Expense _head _tail )  =
    (sem_ConsList_Expense_Cons_Expense (sem_Expense _head ) (sem_ConsList_Expense _tail ) )
sem_ConsList_Expense (Nil_Expense )  =
    (sem_ConsList_Expense_Nil_Expense )
-- semantic domain
type T_ConsList_Expense  = ([(String,Float)]) ->
                           Document ->
                           FocusDoc ->
                           Int ->
                           Int ->
                           Path ->
                           WhitespaceMap ->
                           ( Int,([ParseErrorMessage]),([Presentation_Doc_Node_Clip_Token]),([Presentation_Doc_Node_Clip_Token]),([Presentation_Doc_Node_Clip_Token]),ConsList_Expense,Float,WhitespaceMap)
data Inh_ConsList_Expense  = Inh_ConsList_Expense {allCurrencies_Inh_ConsList_Expense :: [(String,Float)],doc_Inh_ConsList_Expense :: Document,focusD_Inh_ConsList_Expense :: FocusDoc,ix_Inh_ConsList_Expense :: Int,pIdC_Inh_ConsList_Expense :: Int,path_Inh_ConsList_Expense :: Path,whitespaceMap_Inh_ConsList_Expense :: WhitespaceMap}
data Syn_ConsList_Expense  = Syn_ConsList_Expense {pIdC_Syn_ConsList_Expense :: Int,parseErrors_Syn_ConsList_Expense :: [ParseErrorMessage],press_Syn_ConsList_Expense :: [Presentation_Doc_Node_Clip_Token],pressTree_Syn_ConsList_Expense :: [Presentation_Doc_Node_Clip_Token],pressXML_Syn_ConsList_Expense :: [Presentation_Doc_Node_Clip_Token],self_Syn_ConsList_Expense :: ConsList_Expense,total_Syn_ConsList_Expense :: Float,whitespaceMap_Syn_ConsList_Expense :: WhitespaceMap}
wrap_ConsList_Expense :: T_ConsList_Expense  ->
                         Inh_ConsList_Expense  ->
                         Syn_ConsList_Expense 
wrap_ConsList_Expense sem (Inh_ConsList_Expense _lhsIallCurrencies _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )  =
    (let ( _lhsOpIdC,_lhsOparseErrors,_lhsOpress,_lhsOpressTree,_lhsOpressXML,_lhsOself,_lhsOtotal,_lhsOwhitespaceMap) =
             (sem _lhsIallCurrencies _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )
     in  (Syn_ConsList_Expense _lhsOpIdC _lhsOparseErrors _lhsOpress _lhsOpressTree _lhsOpressXML _lhsOself _lhsOtotal _lhsOwhitespaceMap ))
sem_ConsList_Expense_Cons_Expense :: T_Expense  ->
                                     T_ConsList_Expense  ->
                                     T_ConsList_Expense 
sem_ConsList_Expense_Cons_Expense head_ tail_  =
    (\ _lhsIallCurrencies
       _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _headOpath :: Path
              _tailOpath :: Path
              _lhsOpress :: ([Presentation_Doc_Node_Clip_Token])
              _headOpIdC :: Int
              _tailOpIdC :: Int
              _lhsOpIdC :: Int
              _tailOix :: Int
              _lhsOpressXML :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOpressTree :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOtotal :: Float
              _lhsOself :: ConsList_Expense
              _lhsOwhitespaceMap :: WhitespaceMap
              _headOallCurrencies :: ([(String,Float)])
              _headOdoc :: Document
              _headOfocusD :: FocusDoc
              _headOix :: Int
              _headOwhitespaceMap :: WhitespaceMap
              _tailOallCurrencies :: ([(String,Float)])
              _tailOdoc :: Document
              _tailOfocusD :: FocusDoc
              _tailOwhitespaceMap :: WhitespaceMap
              _headIix :: Int
              _headIpIdC :: Int
              _headIparseErrors :: ([ParseErrorMessage])
              _headIpath :: Path
              _headIpres :: Presentation_Doc_Node_Clip_Token
              _headIpresTree :: Presentation_Doc_Node_Clip_Token
              _headIpresXML :: Presentation_Doc_Node_Clip_Token
              _headIself :: Expense
              _headItotal :: Float
              _headIwhitespaceMap :: WhitespaceMap
              _tailIpIdC :: Int
              _tailIparseErrors :: ([ParseErrorMessage])
              _tailIpress :: ([Presentation_Doc_Node_Clip_Token])
              _tailIpressTree :: ([Presentation_Doc_Node_Clip_Token])
              _tailIpressXML :: ([Presentation_Doc_Node_Clip_Token])
              _tailIself :: ConsList_Expense
              _tailItotal :: Float
              _tailIwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 341, column 7)
              _headOpath =
                  {-# LINE 341 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[_lhsIix]
                  {-# LINE 948 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 342, column 7)
              _tailOpath =
                  {-# LINE 342 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 953 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 343, column 7)
              _lhsOpress =
                  {-# LINE 343 "src/PresentationAG_Generated.ag" #-}
                  _headIpres : _tailIpress
                  {-# LINE 958 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 344, column 7)
              _headOpIdC =
                  {-# LINE 344 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 30
                  {-# LINE 963 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 345, column 7)
              _tailOpIdC =
                  {-# LINE 345 "src/PresentationAG_Generated.ag" #-}
                  _headIpIdC
                  {-# LINE 968 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 346, column 7)
              _lhsOpIdC =
                  {-# LINE 346 "src/PresentationAG_Generated.ag" #-}
                  _tailIpIdC
                  {-# LINE 973 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 347, column 7)
              _tailOix =
                  {-# LINE 347 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix + 1
                  {-# LINE 978 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 489, column 18)
              _lhsOpressXML =
                  {-# LINE 489 "src/PresentationAG_Generated.ag" #-}
                  _headIpresXML : _tailIpressXML
                  {-# LINE 983 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 581, column 18)
              _lhsOpressTree =
                  {-# LINE 581 "src/PresentationAG_Generated.ag" #-}
                  _headIpresTree : _tailIpressTree
                  {-# LINE 988 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  _headIparseErrors ++ _tailIparseErrors
                  {-# LINE 993 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 111, column 18)
              _lhsOtotal =
                  {-# LINE 111 "src/PresentationAG.ag" #-}
                  _headItotal + _tailItotal
                  {-# LINE 998 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Cons_Expense _headIself _tailIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _tailIwhitespaceMap
                  {-# LINE 1009 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOallCurrencies =
                  {-# LINE 103 "src/PresentationAG.ag" #-}
                  _lhsIallCurrencies
                  {-# LINE 1014 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOdoc =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 1019 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOfocusD =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 1024 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOix =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 1029 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1034 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tailOallCurrencies =
                  {-# LINE 103 "src/PresentationAG.ag" #-}
                  _lhsIallCurrencies
                  {-# LINE 1039 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tailOdoc =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 1044 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tailOfocusD =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 1049 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _tailOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _headIwhitespaceMap
                  {-# LINE 1054 "src/PresentationAG.hs" #-}
              ( _headIix,_headIpIdC,_headIparseErrors,_headIpath,_headIpres,_headIpresTree,_headIpresXML,_headIself,_headItotal,_headIwhitespaceMap) =
                  (head_ _headOallCurrencies _headOdoc _headOfocusD _headOix _headOpIdC _headOpath _headOwhitespaceMap )
              ( _tailIpIdC,_tailIparseErrors,_tailIpress,_tailIpressTree,_tailIpressXML,_tailIself,_tailItotal,_tailIwhitespaceMap) =
                  (tail_ _tailOallCurrencies _tailOdoc _tailOfocusD _tailOix _tailOpIdC _tailOpath _tailOwhitespaceMap )
          in  ( _lhsOpIdC,_lhsOparseErrors,_lhsOpress,_lhsOpressTree,_lhsOpressXML,_lhsOself,_lhsOtotal,_lhsOwhitespaceMap)))
sem_ConsList_Expense_Nil_Expense :: T_ConsList_Expense 
sem_ConsList_Expense_Nil_Expense  =
    (\ _lhsIallCurrencies
       _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpress :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOpressXML :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOpressTree :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOtotal :: Float
              _lhsOself :: ConsList_Expense
              _lhsOpIdC :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 348, column 22)
              _lhsOpress =
                  {-# LINE 348 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 1081 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 490, column 18)
              _lhsOpressXML =
                  {-# LINE 490 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 1086 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 582, column 18)
              _lhsOpressTree =
                  {-# LINE 582 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 1091 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 1096 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 111, column 18)
              _lhsOtotal =
                  {-# LINE 111 "src/PresentationAG.ag" #-}
                  0
                  {-# LINE 1101 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Nil_Expense
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 1112 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1117 "src/PresentationAG.hs" #-}
          in  ( _lhsOpIdC,_lhsOparseErrors,_lhsOpress,_lhsOpressTree,_lhsOpressXML,_lhsOself,_lhsOtotal,_lhsOwhitespaceMap)))
-- ConsList_Task -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         doc                  : Document
         focusD               : FocusDoc
         ix                   : Int
         path                 : Path
      chained attributes:
         pIdC                 : Int
         whitespaceMap        : WhitespaceMap
      synthesized attributes:
         completed            : Bool
         parseErrors          : [ParseErrorMessage]
         press                : [Presentation_Doc_Node_Clip_Token]
         pressTree            : [Presentation_Doc_Node_Clip_Token]
         pressXML             : [Presentation_Doc_Node_Clip_Token]
         self                 : SELF 
   alternatives:
      alternative Cons_Task:
         child head           : Task 
         child tail           : ConsList_Task 
         visit 0:
            local self        : _
      alternative Nil_Task:
         visit 0:
            local self        : _
-}
-- cata
sem_ConsList_Task :: ConsList_Task  ->
                     T_ConsList_Task 
sem_ConsList_Task (Cons_Task _head _tail )  =
    (sem_ConsList_Task_Cons_Task (sem_Task _head ) (sem_ConsList_Task _tail ) )
sem_ConsList_Task (Nil_Task )  =
    (sem_ConsList_Task_Nil_Task )
-- semantic domain
type T_ConsList_Task  = Document ->
                        FocusDoc ->
                        Int ->
                        Int ->
                        Path ->
                        WhitespaceMap ->
                        ( Bool,Int,([ParseErrorMessage]),([Presentation_Doc_Node_Clip_Token]),([Presentation_Doc_Node_Clip_Token]),([Presentation_Doc_Node_Clip_Token]),ConsList_Task,WhitespaceMap)
data Inh_ConsList_Task  = Inh_ConsList_Task {doc_Inh_ConsList_Task :: Document,focusD_Inh_ConsList_Task :: FocusDoc,ix_Inh_ConsList_Task :: Int,pIdC_Inh_ConsList_Task :: Int,path_Inh_ConsList_Task :: Path,whitespaceMap_Inh_ConsList_Task :: WhitespaceMap}
data Syn_ConsList_Task  = Syn_ConsList_Task {completed_Syn_ConsList_Task :: Bool,pIdC_Syn_ConsList_Task :: Int,parseErrors_Syn_ConsList_Task :: [ParseErrorMessage],press_Syn_ConsList_Task :: [Presentation_Doc_Node_Clip_Token],pressTree_Syn_ConsList_Task :: [Presentation_Doc_Node_Clip_Token],pressXML_Syn_ConsList_Task :: [Presentation_Doc_Node_Clip_Token],self_Syn_ConsList_Task :: ConsList_Task,whitespaceMap_Syn_ConsList_Task :: WhitespaceMap}
wrap_ConsList_Task :: T_ConsList_Task  ->
                      Inh_ConsList_Task  ->
                      Syn_ConsList_Task 
wrap_ConsList_Task sem (Inh_ConsList_Task _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )  =
    (let ( _lhsOcompleted,_lhsOpIdC,_lhsOparseErrors,_lhsOpress,_lhsOpressTree,_lhsOpressXML,_lhsOself,_lhsOwhitespaceMap) =
             (sem _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )
     in  (Syn_ConsList_Task _lhsOcompleted _lhsOpIdC _lhsOparseErrors _lhsOpress _lhsOpressTree _lhsOpressXML _lhsOself _lhsOwhitespaceMap ))
sem_ConsList_Task_Cons_Task :: T_Task  ->
                               T_ConsList_Task  ->
                               T_ConsList_Task 
sem_ConsList_Task_Cons_Task head_ tail_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _headOpath :: Path
              _tailOpath :: Path
              _lhsOpress :: ([Presentation_Doc_Node_Clip_Token])
              _headOpIdC :: Int
              _tailOpIdC :: Int
              _lhsOpIdC :: Int
              _tailOix :: Int
              _lhsOpressXML :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOpressTree :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOcompleted :: Bool
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: ConsList_Task
              _lhsOwhitespaceMap :: WhitespaceMap
              _headOdoc :: Document
              _headOfocusD :: FocusDoc
              _headOix :: Int
              _headOwhitespaceMap :: WhitespaceMap
              _tailOdoc :: Document
              _tailOfocusD :: FocusDoc
              _tailOwhitespaceMap :: WhitespaceMap
              _headIcompleted :: Bool
              _headIix :: Int
              _headIpIdC :: Int
              _headIparseErrors :: ([ParseErrorMessage])
              _headIpath :: Path
              _headIpres :: Presentation_Doc_Node_Clip_Token
              _headIpresTree :: Presentation_Doc_Node_Clip_Token
              _headIpresXML :: Presentation_Doc_Node_Clip_Token
              _headIself :: Task
              _headIwhitespaceMap :: WhitespaceMap
              _tailIcompleted :: Bool
              _tailIpIdC :: Int
              _tailIparseErrors :: ([ParseErrorMessage])
              _tailIpress :: ([Presentation_Doc_Node_Clip_Token])
              _tailIpressTree :: ([Presentation_Doc_Node_Clip_Token])
              _tailIpressXML :: ([Presentation_Doc_Node_Clip_Token])
              _tailIself :: ConsList_Task
              _tailIwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 363, column 7)
              _headOpath =
                  {-# LINE 363 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[_lhsIix]
                  {-# LINE 1223 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 364, column 7)
              _tailOpath =
                  {-# LINE 364 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 1228 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 365, column 7)
              _lhsOpress =
                  {-# LINE 365 "src/PresentationAG_Generated.ag" #-}
                  _headIpres : _tailIpress
                  {-# LINE 1233 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 366, column 7)
              _headOpIdC =
                  {-# LINE 366 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 30
                  {-# LINE 1238 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 367, column 7)
              _tailOpIdC =
                  {-# LINE 367 "src/PresentationAG_Generated.ag" #-}
                  _headIpIdC
                  {-# LINE 1243 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 368, column 7)
              _lhsOpIdC =
                  {-# LINE 368 "src/PresentationAG_Generated.ag" #-}
                  _tailIpIdC
                  {-# LINE 1248 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 369, column 7)
              _tailOix =
                  {-# LINE 369 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix + 1
                  {-# LINE 1253 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 497, column 15)
              _lhsOpressXML =
                  {-# LINE 497 "src/PresentationAG_Generated.ag" #-}
                  _headIpresXML : _tailIpressXML
                  {-# LINE 1258 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 589, column 15)
              _lhsOpressTree =
                  {-# LINE 589 "src/PresentationAG_Generated.ag" #-}
                  _headIpresTree : _tailIpressTree
                  {-# LINE 1263 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 229, column 22)
              _lhsOcompleted =
                  {-# LINE 229 "src/PresentationAG.ag" #-}
                  _headIcompleted && _tailIcompleted
                  {-# LINE 1268 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  _headIparseErrors ++ _tailIparseErrors
                  {-# LINE 1273 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Cons_Task _headIself _tailIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _tailIwhitespaceMap
                  {-# LINE 1284 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOdoc =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 1289 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOfocusD =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 1294 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOix =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 1299 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1304 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tailOdoc =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 1309 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tailOfocusD =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 1314 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _tailOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _headIwhitespaceMap
                  {-# LINE 1319 "src/PresentationAG.hs" #-}
              ( _headIcompleted,_headIix,_headIpIdC,_headIparseErrors,_headIpath,_headIpres,_headIpresTree,_headIpresXML,_headIself,_headIwhitespaceMap) =
                  (head_ _headOdoc _headOfocusD _headOix _headOpIdC _headOpath _headOwhitespaceMap )
              ( _tailIcompleted,_tailIpIdC,_tailIparseErrors,_tailIpress,_tailIpressTree,_tailIpressXML,_tailIself,_tailIwhitespaceMap) =
                  (tail_ _tailOdoc _tailOfocusD _tailOix _tailOpIdC _tailOpath _tailOwhitespaceMap )
          in  ( _lhsOcompleted,_lhsOpIdC,_lhsOparseErrors,_lhsOpress,_lhsOpressTree,_lhsOpressXML,_lhsOself,_lhsOwhitespaceMap)))
sem_ConsList_Task_Nil_Task :: T_ConsList_Task 
sem_ConsList_Task_Nil_Task  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpress :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOpressXML :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOpressTree :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOcompleted :: Bool
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: ConsList_Task
              _lhsOpIdC :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 370, column 19)
              _lhsOpress =
                  {-# LINE 370 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 1345 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 498, column 15)
              _lhsOpressXML =
                  {-# LINE 498 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 1350 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 590, column 15)
              _lhsOpressTree =
                  {-# LINE 590 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 1355 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 229, column 22)
              _lhsOcompleted =
                  {-# LINE 229 "src/PresentationAG.ag" #-}
                  True
                  {-# LINE 1360 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 1365 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Nil_Task
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 1376 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1381 "src/PresentationAG.hs" #-}
          in  ( _lhsOcompleted,_lhsOpIdC,_lhsOparseErrors,_lhsOpress,_lhsOpressTree,_lhsOpressXML,_lhsOself,_lhsOwhitespaceMap)))
-- Currency ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         doc                  : Document
         focusD               : FocusDoc
      chained attributes:
         ix                   : Int
         pIdC                 : Int
         path                 : Path
         whitespaceMap        : WhitespaceMap
      synthesized attributes:
         allCurrencies        : [(String,Float)]
         parseErrors          : [ParseErrorMessage]
         pres                 : Presentation_Doc_Node_Clip_Token
         presTree             : Presentation_Doc_Node_Clip_Token
         presXML              : Presentation_Doc_Node_Clip_Token
         self                 : SELF 
   alternatives:
      alternative Currency:
         child name           : {String}
         child euroRate       : {Float}
         visit 0:
            local pres        : _
            local self        : _
      alternative HoleCurrency:
         visit 0:
            local self        : _
      alternative ParseErrCurrency:
         child error          : {(ParseError Document Node ClipDoc UserToken)}
         visit 0:
            local self        : _
-}
-- cata
sem_Currency :: Currency  ->
                T_Currency 
sem_Currency (Currency _name _euroRate )  =
    (sem_Currency_Currency _name _euroRate )
sem_Currency (HoleCurrency )  =
    (sem_Currency_HoleCurrency )
sem_Currency (ParseErrCurrency _error )  =
    (sem_Currency_ParseErrCurrency _error )
-- semantic domain
type T_Currency  = Document ->
                   FocusDoc ->
                   Int ->
                   Int ->
                   Path ->
                   WhitespaceMap ->
                   ( ([(String,Float)]),Int,Int,([ParseErrorMessage]),Path,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,Currency,WhitespaceMap)
data Inh_Currency  = Inh_Currency {doc_Inh_Currency :: Document,focusD_Inh_Currency :: FocusDoc,ix_Inh_Currency :: Int,pIdC_Inh_Currency :: Int,path_Inh_Currency :: Path,whitespaceMap_Inh_Currency :: WhitespaceMap}
data Syn_Currency  = Syn_Currency {allCurrencies_Syn_Currency :: [(String,Float)],ix_Syn_Currency :: Int,pIdC_Syn_Currency :: Int,parseErrors_Syn_Currency :: [ParseErrorMessage],path_Syn_Currency :: Path,pres_Syn_Currency :: Presentation_Doc_Node_Clip_Token,presTree_Syn_Currency :: Presentation_Doc_Node_Clip_Token,presXML_Syn_Currency :: Presentation_Doc_Node_Clip_Token,self_Syn_Currency :: Currency,whitespaceMap_Syn_Currency :: WhitespaceMap}
wrap_Currency :: T_Currency  ->
                 Inh_Currency  ->
                 Syn_Currency 
wrap_Currency sem (Inh_Currency _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )  =
    (let ( _lhsOallCurrencies,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap) =
             (sem _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )
     in  (Syn_Currency _lhsOallCurrencies _lhsOix _lhsOpIdC _lhsOparseErrors _lhsOpath _lhsOpres _lhsOpresTree _lhsOpresXML _lhsOself _lhsOwhitespaceMap ))
sem_Currency_Currency :: String ->
                         Float ->
                         T_Currency 
sem_Currency_Currency name_ euroRate_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOallCurrencies :: ([(String,Float)])
              _lhsOpIdC :: Int
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpath :: Path
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: Currency
              _lhsOix :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG.ag"(line 88, column 7)
              _pres =
                  {-# LINE 88 "src/PresentationAG.ag" #-}
                  structural $ col [ textField 80 name_
                                   , textField 120 $ show euroRate_
                                   ]
                  {-# LINE 1468 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG.ag"(line 100, column 7)
              _lhsOallCurrencies =
                  {-# LINE 100 "src/PresentationAG.ag" #-}
                  [(name_, euroRate_)]
                  {-# LINE 1473 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 215, column 7)
              _lhsOpIdC =
                  {-# LINE 215 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 0
                  {-# LINE 1478 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 218, column 7)
              _lhsOpres =
                  {-# LINE 218 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_Currency _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1484 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 390, column 14)
              _lhsOpath =
                  {-# LINE 390 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 1489 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 437, column 7)
              _lhsOpresXML =
                  {-# LINE 437 "src/PresentationAG_Generated.ag" #-}
                  presentElementXML _lhsIfocusD (Node_Currency _self _lhsIpath) _lhsIpath "Currency" [ presentPrimXMLString name_, presentPrimXMLFloat euroRate_ ]
                  {-# LINE 1494 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 529, column 7)
              _lhsOpresTree =
                  {-# LINE 529 "src/PresentationAG_Generated.ag" #-}
                  presentElementTree _lhsIfocusD (Node_Currency _self _lhsIpath) _lhsIpath "Currency" [ presentPrimXMLString name_, presentPrimXMLFloat euroRate_ ]
                  {-# LINE 1499 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 1504 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Currency name_ euroRate_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 1515 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1520 "src/PresentationAG.hs" #-}
          in  ( _lhsOallCurrencies,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
sem_Currency_HoleCurrency :: T_Currency 
sem_Currency_HoleCurrency  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOallCurrencies :: ([(String,Float)])
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: Currency
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 221, column 7)
              _lhsOpres =
                  {-# LINE 221 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Currency" (Node_HoleCurrency _self _lhsIpath) _lhsIpath
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1545 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 438, column 22)
              _lhsOpresXML =
                  {-# LINE 438 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Currency" (Node_HoleCurrency _self _lhsIpath) _lhsIpath
                  {-# LINE 1550 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 530, column 22)
              _lhsOpresTree =
                  {-# LINE 530 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Currency" (Node_HoleCurrency _self _lhsIpath) _lhsIpath
                  {-# LINE 1555 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 96, column 26)
              _lhsOallCurrencies =
                  {-# LINE 96 "src/PresentationAG.ag" #-}
                  []
                  {-# LINE 1560 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 1565 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleCurrency
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 1576 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 1581 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 1586 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1591 "src/PresentationAG.hs" #-}
          in  ( _lhsOallCurrencies,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
sem_Currency_ParseErrCurrency :: ((ParseError Document Node ClipDoc UserToken)) ->
                                 T_Currency 
sem_Currency_ParseErrCurrency error_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOallCurrencies :: ([(String,Float)])
              _lhsOself :: Currency
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 224, column 7)
              _lhsOpres =
                  {-# LINE 224 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrCurrency _self _lhsIpath) error_
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1617 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 226, column 7)
              _lhsOparseErrors =
                  {-# LINE 226 "src/PresentationAG_Generated.ag" #-}
                  getErrorMessages error_
                  {-# LINE 1622 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 439, column 22)
              _lhsOpresXML =
                  {-# LINE 439 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrCurrency _self _lhsIpath) error_
                  {-# LINE 1627 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 531, column 22)
              _lhsOpresTree =
                  {-# LINE 531 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrCurrency _self _lhsIpath) error_
                  {-# LINE 1632 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 96, column 26)
              _lhsOallCurrencies =
                  {-# LINE 96 "src/PresentationAG.ag" #-}
                  []
                  {-# LINE 1637 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrCurrency error_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 1648 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 1653 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 1658 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1663 "src/PresentationAG.hs" #-}
          in  ( _lhsOallCurrencies,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
-- EnrichedDoc -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         doc                  : Document
         focusD               : FocusDoc
         path                 : Path
      chained attributes:
         pIdC                 : Int
         whitespaceMap        : WhitespaceMap
      synthesized attributes:
         pres                 : Presentation_Doc_Node_Clip_Token
         self                 : SELF 
   alternatives:
      alternative HoleEnrichedDoc:
         visit 0:
            local self        : _
      alternative ParseErrEnrichedDoc:
         child error          : {(ParseError Document Node ClipDoc UserToken)}
         visit 0:
            local self        : _
      alternative RootEnr:
         child choiceDoc      : ChoiceDoc 
         visit 0:
            local pres        : _
            local self        : _
-}
-- cata
sem_EnrichedDoc :: EnrichedDoc  ->
                   T_EnrichedDoc 
sem_EnrichedDoc (HoleEnrichedDoc )  =
    (sem_EnrichedDoc_HoleEnrichedDoc )
sem_EnrichedDoc (ParseErrEnrichedDoc _error )  =
    (sem_EnrichedDoc_ParseErrEnrichedDoc _error )
sem_EnrichedDoc (RootEnr _choiceDoc )  =
    (sem_EnrichedDoc_RootEnr (sem_ChoiceDoc _choiceDoc ) )
-- semantic domain
type T_EnrichedDoc  = Document ->
                      FocusDoc ->
                      Int ->
                      Path ->
                      WhitespaceMap ->
                      ( Int,Presentation_Doc_Node_Clip_Token,EnrichedDoc,WhitespaceMap)
data Inh_EnrichedDoc  = Inh_EnrichedDoc {doc_Inh_EnrichedDoc :: Document,focusD_Inh_EnrichedDoc :: FocusDoc,pIdC_Inh_EnrichedDoc :: Int,path_Inh_EnrichedDoc :: Path,whitespaceMap_Inh_EnrichedDoc :: WhitespaceMap}
data Syn_EnrichedDoc  = Syn_EnrichedDoc {pIdC_Syn_EnrichedDoc :: Int,pres_Syn_EnrichedDoc :: Presentation_Doc_Node_Clip_Token,self_Syn_EnrichedDoc :: EnrichedDoc,whitespaceMap_Syn_EnrichedDoc :: WhitespaceMap}
wrap_EnrichedDoc :: T_EnrichedDoc  ->
                    Inh_EnrichedDoc  ->
                    Syn_EnrichedDoc 
wrap_EnrichedDoc sem (Inh_EnrichedDoc _lhsIdoc _lhsIfocusD _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )  =
    (let ( _lhsOpIdC,_lhsOpres,_lhsOself,_lhsOwhitespaceMap) =
             (sem _lhsIdoc _lhsIfocusD _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )
     in  (Syn_EnrichedDoc _lhsOpIdC _lhsOpres _lhsOself _lhsOwhitespaceMap ))
sem_EnrichedDoc_HoleEnrichedDoc :: T_EnrichedDoc 
sem_EnrichedDoc_HoleEnrichedDoc  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOself :: EnrichedDoc
              _lhsOpIdC :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 144, column 7)
              _lhsOpres =
                  {-# LINE 144 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "EnrichedDoc" (Node_HoleEnrichedDoc _self _lhsIpath) _lhsIpath
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1733 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleEnrichedDoc
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 1744 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1749 "src/PresentationAG.hs" #-}
          in  ( _lhsOpIdC,_lhsOpres,_lhsOself,_lhsOwhitespaceMap)))
sem_EnrichedDoc_ParseErrEnrichedDoc :: ((ParseError Document Node ClipDoc UserToken)) ->
                                       T_EnrichedDoc 
sem_EnrichedDoc_ParseErrEnrichedDoc error_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOself :: EnrichedDoc
              _lhsOpIdC :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 147, column 7)
              _lhsOpres =
                  {-# LINE 147 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrEnrichedDoc _self _lhsIpath) error_
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1768 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrEnrichedDoc error_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 1779 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1784 "src/PresentationAG.hs" #-}
          in  ( _lhsOpIdC,_lhsOpres,_lhsOself,_lhsOwhitespaceMap)))
sem_EnrichedDoc_RootEnr :: T_ChoiceDoc  ->
                           T_EnrichedDoc 
sem_EnrichedDoc_RootEnr choiceDoc_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _choiceDocOpIdC :: Int
              _lhsOpIdC :: Int
              _choiceDocOpath :: Path
              _choiceDocOix :: Int
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOself :: EnrichedDoc
              _lhsOwhitespaceMap :: WhitespaceMap
              _choiceDocOdoc :: Document
              _choiceDocOfocusD :: FocusDoc
              _choiceDocOwhitespaceMap :: WhitespaceMap
              _choiceDocIix :: Int
              _choiceDocIpIdC :: Int
              _choiceDocIparseErrors :: ([ParseErrorMessage])
              _choiceDocIpath :: Path
              _choiceDocIpres :: Presentation_Doc_Node_Clip_Token
              _choiceDocIpresTree :: Presentation_Doc_Node_Clip_Token
              _choiceDocIpresXML :: Presentation_Doc_Node_Clip_Token
              _choiceDocIself :: ChoiceDoc
              _choiceDocIwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG.ag"(line 34, column 7)
              _pres =
                  {-# LINE 34 "src/PresentationAG.ag" #-}
                  structural $ _choiceDocIpres
                  {-# LINE 1817 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 135, column 7)
              _choiceDocOpIdC =
                  {-# LINE 135 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 0
                  {-# LINE 1822 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 136, column 7)
              _lhsOpIdC =
                  {-# LINE 136 "src/PresentationAG_Generated.ag" #-}
                  _choiceDocIpIdC
                  {-# LINE 1827 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 137, column 7)
              _choiceDocOpath =
                  {-# LINE 137 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[0]
                  {-# LINE 1832 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 139, column 5)
              _choiceDocOix =
                  {-# LINE 139 "src/PresentationAG_Generated.ag" #-}
                  0
                  {-# LINE 1837 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 141, column 7)
              _lhsOpres =
                  {-# LINE 141 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_RootEnr _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1843 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  RootEnr _choiceDocIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _choiceDocIwhitespaceMap
                  {-# LINE 1854 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _choiceDocOdoc =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 1859 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _choiceDocOfocusD =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 1864 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _choiceDocOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1869 "src/PresentationAG.hs" #-}
              ( _choiceDocIix,_choiceDocIpIdC,_choiceDocIparseErrors,_choiceDocIpath,_choiceDocIpres,_choiceDocIpresTree,_choiceDocIpresXML,_choiceDocIself,_choiceDocIwhitespaceMap) =
                  (choiceDoc_ _choiceDocOdoc _choiceDocOfocusD _choiceDocOix _choiceDocOpIdC _choiceDocOpath _choiceDocOwhitespaceMap )
          in  ( _lhsOpIdC,_lhsOpres,_lhsOself,_lhsOwhitespaceMap)))
-- Expense -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allCurrencies        : [(String,Float)]
         doc                  : Document
         focusD               : FocusDoc
      chained attributes:
         ix                   : Int
         pIdC                 : Int
         path                 : Path
         whitespaceMap        : WhitespaceMap
      synthesized attributes:
         parseErrors          : [ParseErrorMessage]
         pres                 : Presentation_Doc_Node_Clip_Token
         presTree             : Presentation_Doc_Node_Clip_Token
         presXML              : Presentation_Doc_Node_Clip_Token
         self                 : SELF 
         total                : Float
   alternatives:
      alternative Expense:
         child description    : {String}
         child amount         : {Float}
         child currencyIx     : {Int}
         visit 0:
            local pres        : _
            local total       : _
            local self        : _
      alternative HoleExpense:
         visit 0:
            local self        : _
      alternative ParseErrExpense:
         child error          : {(ParseError Document Node ClipDoc UserToken)}
         visit 0:
            local self        : _
-}
-- cata
sem_Expense :: Expense  ->
               T_Expense 
sem_Expense (Expense _description _amount _currencyIx )  =
    (sem_Expense_Expense _description _amount _currencyIx )
sem_Expense (HoleExpense )  =
    (sem_Expense_HoleExpense )
sem_Expense (ParseErrExpense _error )  =
    (sem_Expense_ParseErrExpense _error )
-- semantic domain
type T_Expense  = ([(String,Float)]) ->
                  Document ->
                  FocusDoc ->
                  Int ->
                  Int ->
                  Path ->
                  WhitespaceMap ->
                  ( Int,Int,([ParseErrorMessage]),Path,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,Expense,Float,WhitespaceMap)
data Inh_Expense  = Inh_Expense {allCurrencies_Inh_Expense :: [(String,Float)],doc_Inh_Expense :: Document,focusD_Inh_Expense :: FocusDoc,ix_Inh_Expense :: Int,pIdC_Inh_Expense :: Int,path_Inh_Expense :: Path,whitespaceMap_Inh_Expense :: WhitespaceMap}
data Syn_Expense  = Syn_Expense {ix_Syn_Expense :: Int,pIdC_Syn_Expense :: Int,parseErrors_Syn_Expense :: [ParseErrorMessage],path_Syn_Expense :: Path,pres_Syn_Expense :: Presentation_Doc_Node_Clip_Token,presTree_Syn_Expense :: Presentation_Doc_Node_Clip_Token,presXML_Syn_Expense :: Presentation_Doc_Node_Clip_Token,self_Syn_Expense :: Expense,total_Syn_Expense :: Float,whitespaceMap_Syn_Expense :: WhitespaceMap}
wrap_Expense :: T_Expense  ->
                Inh_Expense  ->
                Syn_Expense 
wrap_Expense sem (Inh_Expense _lhsIallCurrencies _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )  =
    (let ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOtotal,_lhsOwhitespaceMap) =
             (sem _lhsIallCurrencies _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )
     in  (Syn_Expense _lhsOix _lhsOpIdC _lhsOparseErrors _lhsOpath _lhsOpres _lhsOpresTree _lhsOpresXML _lhsOself _lhsOtotal _lhsOwhitespaceMap ))
sem_Expense_Expense :: String ->
                       Float ->
                       Int ->
                       T_Expense 
sem_Expense_Expense description_ amount_ currencyIx_  =
    (\ _lhsIallCurrencies
       _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpIdC :: Int
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpath :: Path
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOtotal :: Float
              _lhsOself :: Expense
              _lhsOix :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG.ag"(line 66, column 7)
              _pres =
                  {-# LINE 66 "src/PresentationAG.ag" #-}
                  structural $ addCurrencyItems _lhsIallCurrencies _lhsIpath $
                    row [ textField 400 description_
                        , hSpace 5
                        , textField 30 (show amount_)
                        , hSpace 5
                        , textField 60 $ (fst $ index "Expense.pres" _lhsIallCurrencies currencyIx_)
                        , hSpace 10
                        , textField 120 (showCurrency _total ++ " Euro")
                        ]
                  {-# LINE 1970 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG.ag"(line 115, column 7)
              _total =
                  {-# LINE 115 "src/PresentationAG.ag" #-}
                  amount_ * (snd $ index "Expense.total" _lhsIallCurrencies currencyIx_)
                  {-# LINE 1975 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 200, column 7)
              _lhsOpIdC =
                  {-# LINE 200 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 0
                  {-# LINE 1980 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 203, column 7)
              _lhsOpres =
                  {-# LINE 203 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_Expense _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1986 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 387, column 13)
              _lhsOpath =
                  {-# LINE 387 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 1991 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 431, column 7)
              _lhsOpresXML =
                  {-# LINE 431 "src/PresentationAG_Generated.ag" #-}
                  presentElementXML _lhsIfocusD (Node_Expense _self _lhsIpath) _lhsIpath "Expense" [ presentPrimXMLString description_, presentPrimXMLFloat amount_, presentPrimXMLInt currencyIx_ ]
                  {-# LINE 1996 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 523, column 7)
              _lhsOpresTree =
                  {-# LINE 523 "src/PresentationAG_Generated.ag" #-}
                  presentElementTree _lhsIfocusD (Node_Expense _self _lhsIpath) _lhsIpath "Expense" [ presentPrimXMLString description_, presentPrimXMLFloat amount_, presentPrimXMLInt currencyIx_ ]
                  {-# LINE 2001 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 2006 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 111, column 18)
              _lhsOtotal =
                  {-# LINE 111 "src/PresentationAG.ag" #-}
                  _total
                  {-# LINE 2011 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Expense description_ amount_ currencyIx_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 2022 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 2027 "src/PresentationAG.hs" #-}
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOtotal,_lhsOwhitespaceMap)))
sem_Expense_HoleExpense :: T_Expense 
sem_Expense_HoleExpense  =
    (\ _lhsIallCurrencies
       _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOtotal :: Float
              _lhsOself :: Expense
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 206, column 7)
              _lhsOpres =
                  {-# LINE 206 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Expense" (Node_HoleExpense _self _lhsIpath) _lhsIpath
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 2053 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 432, column 21)
              _lhsOpresXML =
                  {-# LINE 432 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Expense" (Node_HoleExpense _self _lhsIpath) _lhsIpath
                  {-# LINE 2058 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 524, column 21)
              _lhsOpresTree =
                  {-# LINE 524 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Expense" (Node_HoleExpense _self _lhsIpath) _lhsIpath
                  {-# LINE 2063 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 2068 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 111, column 18)
              _lhsOtotal =
                  {-# LINE 111 "src/PresentationAG.ag" #-}
                  0
                  {-# LINE 2073 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleExpense
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 2084 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 2089 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 2094 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 2099 "src/PresentationAG.hs" #-}
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOtotal,_lhsOwhitespaceMap)))
sem_Expense_ParseErrExpense :: ((ParseError Document Node ClipDoc UserToken)) ->
                               T_Expense 
sem_Expense_ParseErrExpense error_  =
    (\ _lhsIallCurrencies
       _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOtotal :: Float
              _lhsOself :: Expense
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 209, column 7)
              _lhsOpres =
                  {-# LINE 209 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrExpense _self _lhsIpath) error_
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 2126 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 211, column 7)
              _lhsOparseErrors =
                  {-# LINE 211 "src/PresentationAG_Generated.ag" #-}
                  getErrorMessages error_
                  {-# LINE 2131 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 433, column 21)
              _lhsOpresXML =
                  {-# LINE 433 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrExpense _self _lhsIpath) error_
                  {-# LINE 2136 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 525, column 21)
              _lhsOpresTree =
                  {-# LINE 525 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrExpense _self _lhsIpath) error_
                  {-# LINE 2141 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 111, column 18)
              _lhsOtotal =
                  {-# LINE 111 "src/PresentationAG.ag" #-}
                  0
                  {-# LINE 2146 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrExpense error_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 2157 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 2162 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 2167 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 2172 "src/PresentationAG.hs" #-}
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOtotal,_lhsOwhitespaceMap)))
-- Form --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         doc                  : Document
         focusD               : FocusDoc
      chained attributes:
         ix                   : Int
         pIdC                 : Int
         path                 : Path
         whitespaceMap        : WhitespaceMap
      synthesized attributes:
         parseErrors          : [ParseErrorMessage]
         pres                 : Presentation_Doc_Node_Clip_Token
         presTree             : Presentation_Doc_Node_Clip_Token
         presXML              : Presentation_Doc_Node_Clip_Token
         self                 : SELF 
   alternatives:
      alternative Form:
         child name           : {String}
         child faculty        : {String}
         child expenses       : List_Expense 
         child currencies     : List_Currency 
         visit 0:
            local pres        : _
            local self        : _
      alternative HoleForm:
         visit 0:
            local self        : _
      alternative ParseErrForm:
         child error          : {(ParseError Document Node ClipDoc UserToken)}
         visit 0:
            local self        : _
-}
-- cata
sem_Form :: Form  ->
            T_Form 
sem_Form (Form _name _faculty _expenses _currencies )  =
    (sem_Form_Form _name _faculty (sem_List_Expense _expenses ) (sem_List_Currency _currencies ) )
sem_Form (HoleForm )  =
    (sem_Form_HoleForm )
sem_Form (ParseErrForm _error )  =
    (sem_Form_ParseErrForm _error )
-- semantic domain
type T_Form  = Document ->
               FocusDoc ->
               Int ->
               Int ->
               Path ->
               WhitespaceMap ->
               ( Int,Int,([ParseErrorMessage]),Path,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,Form,WhitespaceMap)
data Inh_Form  = Inh_Form {doc_Inh_Form :: Document,focusD_Inh_Form :: FocusDoc,ix_Inh_Form :: Int,pIdC_Inh_Form :: Int,path_Inh_Form :: Path,whitespaceMap_Inh_Form :: WhitespaceMap}
data Syn_Form  = Syn_Form {ix_Syn_Form :: Int,pIdC_Syn_Form :: Int,parseErrors_Syn_Form :: [ParseErrorMessage],path_Syn_Form :: Path,pres_Syn_Form :: Presentation_Doc_Node_Clip_Token,presTree_Syn_Form :: Presentation_Doc_Node_Clip_Token,presXML_Syn_Form :: Presentation_Doc_Node_Clip_Token,self_Syn_Form :: Form,whitespaceMap_Syn_Form :: WhitespaceMap}
wrap_Form :: T_Form  ->
             Inh_Form  ->
             Syn_Form 
wrap_Form sem (Inh_Form _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )  =
    (let ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap) =
             (sem _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )
     in  (Syn_Form _lhsOix _lhsOpIdC _lhsOparseErrors _lhsOpath _lhsOpres _lhsOpresTree _lhsOpresXML _lhsOself _lhsOwhitespaceMap ))
sem_Form_Form :: String ->
                 String ->
                 T_List_Expense  ->
                 T_List_Currency  ->
                 T_Form 
sem_Form_Form name_ faculty_ expenses_ currencies_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _expensesOallCurrencies :: ([(String,Float)])
              _expensesOpIdC :: Int
              _currenciesOpIdC :: Int
              _lhsOpIdC :: Int
              _expensesOpath :: Path
              _currenciesOpath :: Path
              _expensesOix :: Int
              _currenciesOix :: Int
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpath :: Path
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: Form
              _lhsOix :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              _expensesOdoc :: Document
              _expensesOfocusD :: FocusDoc
              _expensesOwhitespaceMap :: WhitespaceMap
              _currenciesOdoc :: Document
              _currenciesOfocusD :: FocusDoc
              _currenciesOwhitespaceMap :: WhitespaceMap
              _expensesIix :: Int
              _expensesIpIdC :: Int
              _expensesIparseErrors :: ([ParseErrorMessage])
              _expensesIpath :: Path
              _expensesIpres :: Presentation_Doc_Node_Clip_Token
              _expensesIpresTree :: Presentation_Doc_Node_Clip_Token
              _expensesIpresXML :: Presentation_Doc_Node_Clip_Token
              _expensesIpress :: ([Presentation_Doc_Node_Clip_Token])
              _expensesIself :: List_Expense
              _expensesItotal :: Float
              _expensesIwhitespaceMap :: WhitespaceMap
              _currenciesIallCurrencies :: ([(String,Float)])
              _currenciesIix :: Int
              _currenciesIpIdC :: Int
              _currenciesIparseErrors :: ([ParseErrorMessage])
              _currenciesIpath :: Path
              _currenciesIpres :: Presentation_Doc_Node_Clip_Token
              _currenciesIpresTree :: Presentation_Doc_Node_Clip_Token
              _currenciesIpresXML :: Presentation_Doc_Node_Clip_Token
              _currenciesIpress :: ([Presentation_Doc_Node_Clip_Token])
              _currenciesIself :: List_Currency
              _currenciesIwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG.ag"(line 44, column 7)
              _pres =
                  {-# LINE 44 "src/PresentationAG.ag" #-}
                  structural $ leftTopMargins 30 40 $
                    col [ text "Travel declaration form"
                        , vSpace 5
                        , textField 400 name_
                        , vSpace 5
                        , textField 400 faculty_
                        , vSpace 10
                        , text "Expenses"
                        , _expensesIpres
                        , row [ hSpace (400 + 5 + 30 + 5 + 60 + 10 + 6)
                              , textField 120 (showCurrency _expensesItotal ++ " Euro")
                              ]
                        , vSpace 30
                        , _currenciesIpres `withFontSize_` (\fs -> (fs * 80) `div` 100)
                        ]
                  {-# LINE 2308 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG.ag"(line 107, column 7)
              _expensesOallCurrencies =
                  {-# LINE 107 "src/PresentationAG.ag" #-}
                  _currenciesIallCurrencies
                  {-# LINE 2313 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 179, column 7)
              _expensesOpIdC =
                  {-# LINE 179 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 0
                  {-# LINE 2318 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 180, column 7)
              _currenciesOpIdC =
                  {-# LINE 180 "src/PresentationAG_Generated.ag" #-}
                  _expensesIpIdC
                  {-# LINE 2323 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 181, column 7)
              _lhsOpIdC =
                  {-# LINE 181 "src/PresentationAG_Generated.ag" #-}
                  _currenciesIpIdC
                  {-# LINE 2328 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 182, column 7)
              _expensesOpath =
                  {-# LINE 182 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[2]
                  {-# LINE 2333 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 183, column 7)
              _currenciesOpath =
                  {-# LINE 183 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[3]
                  {-# LINE 2338 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 185, column 5)
              _expensesOix =
                  {-# LINE 185 "src/PresentationAG_Generated.ag" #-}
                  2
                  {-# LINE 2343 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 186, column 5)
              _currenciesOix =
                  {-# LINE 186 "src/PresentationAG_Generated.ag" #-}
                  3
                  {-# LINE 2348 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 188, column 7)
              _lhsOpres =
                  {-# LINE 188 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_Form _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 2354 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 384, column 10)
              _lhsOpath =
                  {-# LINE 384 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 2359 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 425, column 7)
              _lhsOpresXML =
                  {-# LINE 425 "src/PresentationAG_Generated.ag" #-}
                  presentElementXML _lhsIfocusD (Node_Form _self _lhsIpath) _lhsIpath "Form" [ presentPrimXMLString name_, presentPrimXMLString faculty_, _expensesIpresXML, _currenciesIpresXML ]
                  {-# LINE 2364 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 517, column 7)
              _lhsOpresTree =
                  {-# LINE 517 "src/PresentationAG_Generated.ag" #-}
                  presentElementTree _lhsIfocusD (Node_Form _self _lhsIpath) _lhsIpath "Form" [ presentPrimXMLString name_, presentPrimXMLString faculty_, _expensesIpresTree, _currenciesIpresTree ]
                  {-# LINE 2369 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  _expensesIparseErrors ++ _currenciesIparseErrors
                  {-# LINE 2374 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Form name_ faculty_ _expensesIself _currenciesIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _currenciesIix
                  {-# LINE 2385 "src/PresentationAG.hs" #-}
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _currenciesIwhitespaceMap
                  {-# LINE 2390 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _expensesOdoc =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 2395 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _expensesOfocusD =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 2400 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _expensesOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 2405 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _currenciesOdoc =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 2410 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _currenciesOfocusD =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 2415 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _currenciesOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _expensesIwhitespaceMap
                  {-# LINE 2420 "src/PresentationAG.hs" #-}
              ( _expensesIix,_expensesIpIdC,_expensesIparseErrors,_expensesIpath,_expensesIpres,_expensesIpresTree,_expensesIpresXML,_expensesIpress,_expensesIself,_expensesItotal,_expensesIwhitespaceMap) =
                  (expenses_ _expensesOallCurrencies _expensesOdoc _expensesOfocusD _expensesOix _expensesOpIdC _expensesOpath _expensesOwhitespaceMap )
              ( _currenciesIallCurrencies,_currenciesIix,_currenciesIpIdC,_currenciesIparseErrors,_currenciesIpath,_currenciesIpres,_currenciesIpresTree,_currenciesIpresXML,_currenciesIpress,_currenciesIself,_currenciesIwhitespaceMap) =
                  (currencies_ _currenciesOdoc _currenciesOfocusD _currenciesOix _currenciesOpIdC _currenciesOpath _currenciesOwhitespaceMap )
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
sem_Form_HoleForm :: T_Form 
sem_Form_HoleForm  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: Form
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 191, column 7)
              _lhsOpres =
                  {-# LINE 191 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Form" (Node_HoleForm _self _lhsIpath) _lhsIpath
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 2448 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 426, column 18)
              _lhsOpresXML =
                  {-# LINE 426 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Form" (Node_HoleForm _self _lhsIpath) _lhsIpath
                  {-# LINE 2453 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 518, column 18)
              _lhsOpresTree =
                  {-# LINE 518 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Form" (Node_HoleForm _self _lhsIpath) _lhsIpath
                  {-# LINE 2458 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 2463 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleForm
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 2474 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 2479 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 2484 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 2489 "src/PresentationAG.hs" #-}
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
sem_Form_ParseErrForm :: ((ParseError Document Node ClipDoc UserToken)) ->
                         T_Form 
sem_Form_ParseErrForm error_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOself :: Form
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 194, column 7)
              _lhsOpres =
                  {-# LINE 194 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrForm _self _lhsIpath) error_
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 2514 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 196, column 7)
              _lhsOparseErrors =
                  {-# LINE 196 "src/PresentationAG_Generated.ag" #-}
                  getErrorMessages error_
                  {-# LINE 2519 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 427, column 18)
              _lhsOpresXML =
                  {-# LINE 427 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrForm _self _lhsIpath) error_
                  {-# LINE 2524 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 519, column 18)
              _lhsOpresTree =
                  {-# LINE 519 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrForm _self _lhsIpath) error_
                  {-# LINE 2529 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrForm error_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 2540 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 2545 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 2550 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 2555 "src/PresentationAG.hs" #-}
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
-- List_Currency -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         doc                  : Document
         focusD               : FocusDoc
      chained attributes:
         ix                   : Int
         pIdC                 : Int
         path                 : Path
         whitespaceMap        : WhitespaceMap
      synthesized attributes:
         allCurrencies        : [(String,Float)]
         parseErrors          : [ParseErrorMessage]
         pres                 : Presentation_Doc_Node_Clip_Token
         presTree             : Presentation_Doc_Node_Clip_Token
         presXML              : Presentation_Doc_Node_Clip_Token
         press                : [Presentation_Doc_Node_Clip_Token]
         self                 : SELF 
   alternatives:
      alternative HoleList_Currency:
         visit 0:
            local self        : _
      alternative List_Currency:
         child elts           : ConsList_Currency 
         visit 0:
            local pres        : _
            local self        : _
      alternative ParseErrList_Currency:
         child error          : {(ParseError Document Node ClipDoc UserToken)}
         visit 0:
            local self        : _
-}
-- cata
sem_List_Currency :: List_Currency  ->
                     T_List_Currency 
sem_List_Currency (HoleList_Currency )  =
    (sem_List_Currency_HoleList_Currency )
sem_List_Currency (List_Currency _elts )  =
    (sem_List_Currency_List_Currency (sem_ConsList_Currency _elts ) )
sem_List_Currency (ParseErrList_Currency _error )  =
    (sem_List_Currency_ParseErrList_Currency _error )
-- semantic domain
type T_List_Currency  = Document ->
                        FocusDoc ->
                        Int ->
                        Int ->
                        Path ->
                        WhitespaceMap ->
                        ( ([(String,Float)]),Int,Int,([ParseErrorMessage]),Path,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,([Presentation_Doc_Node_Clip_Token]),List_Currency,WhitespaceMap)
data Inh_List_Currency  = Inh_List_Currency {doc_Inh_List_Currency :: Document,focusD_Inh_List_Currency :: FocusDoc,ix_Inh_List_Currency :: Int,pIdC_Inh_List_Currency :: Int,path_Inh_List_Currency :: Path,whitespaceMap_Inh_List_Currency :: WhitespaceMap}
data Syn_List_Currency  = Syn_List_Currency {allCurrencies_Syn_List_Currency :: [(String,Float)],ix_Syn_List_Currency :: Int,pIdC_Syn_List_Currency :: Int,parseErrors_Syn_List_Currency :: [ParseErrorMessage],path_Syn_List_Currency :: Path,pres_Syn_List_Currency :: Presentation_Doc_Node_Clip_Token,presTree_Syn_List_Currency :: Presentation_Doc_Node_Clip_Token,presXML_Syn_List_Currency :: Presentation_Doc_Node_Clip_Token,press_Syn_List_Currency :: [Presentation_Doc_Node_Clip_Token],self_Syn_List_Currency :: List_Currency,whitespaceMap_Syn_List_Currency :: WhitespaceMap}
wrap_List_Currency :: T_List_Currency  ->
                      Inh_List_Currency  ->
                      Syn_List_Currency 
wrap_List_Currency sem (Inh_List_Currency _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )  =
    (let ( _lhsOallCurrencies,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself,_lhsOwhitespaceMap) =
             (sem _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )
     in  (Syn_List_Currency _lhsOallCurrencies _lhsOix _lhsOpIdC _lhsOparseErrors _lhsOpath _lhsOpres _lhsOpresTree _lhsOpresXML _lhsOpress _lhsOself _lhsOwhitespaceMap ))
sem_List_Currency_HoleList_Currency :: T_List_Currency 
sem_List_Currency_HoleList_Currency  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpress :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOallCurrencies :: ([(String,Float)])
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: List_Currency
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 303, column 27)
              _lhsOpress =
                  {-# LINE 303 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 2639 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 313, column 7)
              _lhsOpres =
                  {-# LINE 313 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Currency" (Node_HoleList_Currency _self _lhsIpath) _lhsIpath
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 2645 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 474, column 7)
              _lhsOpresXML =
                  {-# LINE 474 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Currency _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  presHole _lhsIfocusD "List_Currency" (Node_HoleList_Currency _self _lhsIpath) _lhsIpath
                  {-# LINE 2651 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 566, column 7)
              _lhsOpresTree =
                  {-# LINE 566 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Currency _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    presHole _lhsIfocusD "List_Currency" (Node_HoleList_Currency _self _lhsIpath) _lhsIpath
                  {-# LINE 2657 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 96, column 26)
              _lhsOallCurrencies =
                  {-# LINE 96 "src/PresentationAG.ag" #-}
                  []
                  {-# LINE 2662 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 2667 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleList_Currency
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 2678 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 2683 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 2688 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 2693 "src/PresentationAG.hs" #-}
          in  ( _lhsOallCurrencies,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself,_lhsOwhitespaceMap)))
sem_List_Currency_List_Currency :: T_ConsList_Currency  ->
                                   T_List_Currency 
sem_List_Currency_List_Currency elts_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpress :: ([Presentation_Doc_Node_Clip_Token])
              _eltsOpIdC :: Int
              _lhsOpIdC :: Int
              _eltsOpath :: Path
              _eltsOix :: Int
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpath :: Path
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOallCurrencies :: ([(String,Float)])
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: List_Currency
              _lhsOix :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              _eltsOdoc :: Document
              _eltsOfocusD :: FocusDoc
              _eltsOwhitespaceMap :: WhitespaceMap
              _eltsIallCurrencies :: ([(String,Float)])
              _eltsIpIdC :: Int
              _eltsIparseErrors :: ([ParseErrorMessage])
              _eltsIpress :: ([Presentation_Doc_Node_Clip_Token])
              _eltsIpressTree :: ([Presentation_Doc_Node_Clip_Token])
              _eltsIpressXML :: ([Presentation_Doc_Node_Clip_Token])
              _eltsIself :: ConsList_Currency
              _eltsIwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG.ag"(line 81, column 7)
              _pres =
                  {-# LINE 81 "src/PresentationAG.ag" #-}
                  structural $
                    row $ intersperse (hSpace 5) _eltsIpress ++
                          [ hSpace 5, text "add currency" `withMouseDown` addNewCurrency _lhsIpath ]
                  {-# LINE 2735 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 295, column 7)
              _lhsOpress =
                  {-# LINE 295 "src/PresentationAG_Generated.ag" #-}
                  map ( loc (Node_List_Currency _self _lhsIpath)
                      . presentFocus _lhsIfocusD _lhsIpath )
                      _eltsIpress
                  {-# LINE 2742 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 299, column 7)
              _eltsOpIdC =
                  {-# LINE 299 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 100
                  {-# LINE 2747 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 300, column 7)
              _lhsOpIdC =
                  {-# LINE 300 "src/PresentationAG_Generated.ag" #-}
                  _eltsIpIdC
                  {-# LINE 2752 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 301, column 7)
              _eltsOpath =
                  {-# LINE 301 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 2757 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 302, column 7)
              _eltsOix =
                  {-# LINE 302 "src/PresentationAG_Generated.ag" #-}
                  0
                  {-# LINE 2762 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 306, column 7)
              _lhsOpres =
                  {-# LINE 306 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Currency _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath $ _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 2768 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 403, column 19)
              _lhsOpath =
                  {-# LINE 403 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 2773 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 468, column 7)
              _lhsOpresXML =
                  {-# LINE 468 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Currency _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  col _eltsIpressXML
                  {-# LINE 2779 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 560, column 7)
              _lhsOpresTree =
                  {-# LINE 560 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Currency _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    col _eltsIpressTree
                  {-# LINE 2785 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 96, column 26)
              _lhsOallCurrencies =
                  {-# LINE 96 "src/PresentationAG.ag" #-}
                  _eltsIallCurrencies
                  {-# LINE 2790 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  _eltsIparseErrors
                  {-# LINE 2795 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  List_Currency _eltsIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 2806 "src/PresentationAG.hs" #-}
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _eltsIwhitespaceMap
                  {-# LINE 2811 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOdoc =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 2816 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOfocusD =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 2821 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 2826 "src/PresentationAG.hs" #-}
              ( _eltsIallCurrencies,_eltsIpIdC,_eltsIparseErrors,_eltsIpress,_eltsIpressTree,_eltsIpressXML,_eltsIself,_eltsIwhitespaceMap) =
                  (elts_ _eltsOdoc _eltsOfocusD _eltsOix _eltsOpIdC _eltsOpath _eltsOwhitespaceMap )
          in  ( _lhsOallCurrencies,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself,_lhsOwhitespaceMap)))
sem_List_Currency_ParseErrList_Currency :: ((ParseError Document Node ClipDoc UserToken)) ->
                                           T_List_Currency 
sem_List_Currency_ParseErrList_Currency error_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpress :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOallCurrencies :: ([(String,Float)])
              _lhsOself :: List_Currency
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 304, column 27)
              _lhsOpress =
                  {-# LINE 304 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 2854 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 309, column 7)
              _lhsOpres =
                  {-# LINE 309 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrList_Currency _self _lhsIpath) error_
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 2860 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 311, column 7)
              _lhsOparseErrors =
                  {-# LINE 311 "src/PresentationAG_Generated.ag" #-}
                  getErrorMessages error_
                  {-# LINE 2865 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 471, column 7)
              _lhsOpresXML =
                  {-# LINE 471 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Currency _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  presParseErr (Node_ParseErrList_Currency _self _lhsIpath) error_
                  {-# LINE 2871 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 563, column 7)
              _lhsOpresTree =
                  {-# LINE 563 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Currency _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    presParseErr (Node_ParseErrList_Currency _self _lhsIpath) error_
                  {-# LINE 2877 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 96, column 26)
              _lhsOallCurrencies =
                  {-# LINE 96 "src/PresentationAG.ag" #-}
                  []
                  {-# LINE 2882 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrList_Currency error_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 2893 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 2898 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 2903 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 2908 "src/PresentationAG.hs" #-}
          in  ( _lhsOallCurrencies,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself,_lhsOwhitespaceMap)))
-- List_Expense ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allCurrencies        : [(String,Float)]
         doc                  : Document
         focusD               : FocusDoc
      chained attributes:
         ix                   : Int
         pIdC                 : Int
         path                 : Path
         whitespaceMap        : WhitespaceMap
      synthesized attributes:
         parseErrors          : [ParseErrorMessage]
         pres                 : Presentation_Doc_Node_Clip_Token
         presTree             : Presentation_Doc_Node_Clip_Token
         presXML              : Presentation_Doc_Node_Clip_Token
         press                : [Presentation_Doc_Node_Clip_Token]
         self                 : SELF 
         total                : Float
   alternatives:
      alternative HoleList_Expense:
         visit 0:
            local self        : _
      alternative List_Expense:
         child elts           : ConsList_Expense 
         visit 0:
            local pres        : _
            local self        : _
      alternative ParseErrList_Expense:
         child error          : {(ParseError Document Node ClipDoc UserToken)}
         visit 0:
            local self        : _
-}
-- cata
sem_List_Expense :: List_Expense  ->
                    T_List_Expense 
sem_List_Expense (HoleList_Expense )  =
    (sem_List_Expense_HoleList_Expense )
sem_List_Expense (List_Expense _elts )  =
    (sem_List_Expense_List_Expense (sem_ConsList_Expense _elts ) )
sem_List_Expense (ParseErrList_Expense _error )  =
    (sem_List_Expense_ParseErrList_Expense _error )
-- semantic domain
type T_List_Expense  = ([(String,Float)]) ->
                       Document ->
                       FocusDoc ->
                       Int ->
                       Int ->
                       Path ->
                       WhitespaceMap ->
                       ( Int,Int,([ParseErrorMessage]),Path,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,([Presentation_Doc_Node_Clip_Token]),List_Expense,Float,WhitespaceMap)
data Inh_List_Expense  = Inh_List_Expense {allCurrencies_Inh_List_Expense :: [(String,Float)],doc_Inh_List_Expense :: Document,focusD_Inh_List_Expense :: FocusDoc,ix_Inh_List_Expense :: Int,pIdC_Inh_List_Expense :: Int,path_Inh_List_Expense :: Path,whitespaceMap_Inh_List_Expense :: WhitespaceMap}
data Syn_List_Expense  = Syn_List_Expense {ix_Syn_List_Expense :: Int,pIdC_Syn_List_Expense :: Int,parseErrors_Syn_List_Expense :: [ParseErrorMessage],path_Syn_List_Expense :: Path,pres_Syn_List_Expense :: Presentation_Doc_Node_Clip_Token,presTree_Syn_List_Expense :: Presentation_Doc_Node_Clip_Token,presXML_Syn_List_Expense :: Presentation_Doc_Node_Clip_Token,press_Syn_List_Expense :: [Presentation_Doc_Node_Clip_Token],self_Syn_List_Expense :: List_Expense,total_Syn_List_Expense :: Float,whitespaceMap_Syn_List_Expense :: WhitespaceMap}
wrap_List_Expense :: T_List_Expense  ->
                     Inh_List_Expense  ->
                     Syn_List_Expense 
wrap_List_Expense sem (Inh_List_Expense _lhsIallCurrencies _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )  =
    (let ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself,_lhsOtotal,_lhsOwhitespaceMap) =
             (sem _lhsIallCurrencies _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )
     in  (Syn_List_Expense _lhsOix _lhsOpIdC _lhsOparseErrors _lhsOpath _lhsOpres _lhsOpresTree _lhsOpresXML _lhsOpress _lhsOself _lhsOtotal _lhsOwhitespaceMap ))
sem_List_Expense_HoleList_Expense :: T_List_Expense 
sem_List_Expense_HoleList_Expense  =
    (\ _lhsIallCurrencies
       _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpress :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOtotal :: Float
              _lhsOself :: List_Expense
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 280, column 26)
              _lhsOpress =
                  {-# LINE 280 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 2995 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 290, column 7)
              _lhsOpres =
                  {-# LINE 290 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Expense" (Node_HoleList_Expense _self _lhsIpath) _lhsIpath
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 3001 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 463, column 7)
              _lhsOpresXML =
                  {-# LINE 463 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Expense _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  presHole _lhsIfocusD "List_Expense" (Node_HoleList_Expense _self _lhsIpath) _lhsIpath
                  {-# LINE 3007 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 555, column 7)
              _lhsOpresTree =
                  {-# LINE 555 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Expense _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    presHole _lhsIfocusD "List_Expense" (Node_HoleList_Expense _self _lhsIpath) _lhsIpath
                  {-# LINE 3013 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 3018 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 111, column 18)
              _lhsOtotal =
                  {-# LINE 111 "src/PresentationAG.ag" #-}
                  0
                  {-# LINE 3023 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleList_Expense
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 3034 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 3039 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 3044 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 3049 "src/PresentationAG.hs" #-}
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself,_lhsOtotal,_lhsOwhitespaceMap)))
sem_List_Expense_List_Expense :: T_ConsList_Expense  ->
                                 T_List_Expense 
sem_List_Expense_List_Expense elts_  =
    (\ _lhsIallCurrencies
       _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpress :: ([Presentation_Doc_Node_Clip_Token])
              _eltsOpIdC :: Int
              _lhsOpIdC :: Int
              _eltsOpath :: Path
              _eltsOix :: Int
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpath :: Path
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOtotal :: Float
              _lhsOself :: List_Expense
              _lhsOix :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              _eltsOallCurrencies :: ([(String,Float)])
              _eltsOdoc :: Document
              _eltsOfocusD :: FocusDoc
              _eltsOwhitespaceMap :: WhitespaceMap
              _eltsIpIdC :: Int
              _eltsIparseErrors :: ([ParseErrorMessage])
              _eltsIpress :: ([Presentation_Doc_Node_Clip_Token])
              _eltsIpressTree :: ([Presentation_Doc_Node_Clip_Token])
              _eltsIpressXML :: ([Presentation_Doc_Node_Clip_Token])
              _eltsIself :: ConsList_Expense
              _eltsItotal :: Float
              _eltsIwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG.ag"(line 61, column 7)
              _pres =
                  {-# LINE 61 "src/PresentationAG.ag" #-}
                  structural $
                    col $ intersperse (vSpace 5) _eltsIpress
                  {-# LINE 3092 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 272, column 7)
              _lhsOpress =
                  {-# LINE 272 "src/PresentationAG_Generated.ag" #-}
                  map ( loc (Node_List_Expense _self _lhsIpath)
                      . presentFocus _lhsIfocusD _lhsIpath )
                      _eltsIpress
                  {-# LINE 3099 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 276, column 7)
              _eltsOpIdC =
                  {-# LINE 276 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 100
                  {-# LINE 3104 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 277, column 7)
              _lhsOpIdC =
                  {-# LINE 277 "src/PresentationAG_Generated.ag" #-}
                  _eltsIpIdC
                  {-# LINE 3109 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 278, column 7)
              _eltsOpath =
                  {-# LINE 278 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 3114 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 279, column 7)
              _eltsOix =
                  {-# LINE 279 "src/PresentationAG_Generated.ag" #-}
                  0
                  {-# LINE 3119 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 283, column 7)
              _lhsOpres =
                  {-# LINE 283 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Expense _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath $ _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 3125 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 400, column 18)
              _lhsOpath =
                  {-# LINE 400 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 3130 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 457, column 7)
              _lhsOpresXML =
                  {-# LINE 457 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Expense _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  col _eltsIpressXML
                  {-# LINE 3136 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 549, column 7)
              _lhsOpresTree =
                  {-# LINE 549 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Expense _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    col _eltsIpressTree
                  {-# LINE 3142 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  _eltsIparseErrors
                  {-# LINE 3147 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 111, column 18)
              _lhsOtotal =
                  {-# LINE 111 "src/PresentationAG.ag" #-}
                  _eltsItotal
                  {-# LINE 3152 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  List_Expense _eltsIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 3163 "src/PresentationAG.hs" #-}
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _eltsIwhitespaceMap
                  {-# LINE 3168 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOallCurrencies =
                  {-# LINE 103 "src/PresentationAG.ag" #-}
                  _lhsIallCurrencies
                  {-# LINE 3173 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOdoc =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 3178 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOfocusD =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 3183 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 3188 "src/PresentationAG.hs" #-}
              ( _eltsIpIdC,_eltsIparseErrors,_eltsIpress,_eltsIpressTree,_eltsIpressXML,_eltsIself,_eltsItotal,_eltsIwhitespaceMap) =
                  (elts_ _eltsOallCurrencies _eltsOdoc _eltsOfocusD _eltsOix _eltsOpIdC _eltsOpath _eltsOwhitespaceMap )
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself,_lhsOtotal,_lhsOwhitespaceMap)))
sem_List_Expense_ParseErrList_Expense :: ((ParseError Document Node ClipDoc UserToken)) ->
                                         T_List_Expense 
sem_List_Expense_ParseErrList_Expense error_  =
    (\ _lhsIallCurrencies
       _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpress :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOtotal :: Float
              _lhsOself :: List_Expense
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 281, column 26)
              _lhsOpress =
                  {-# LINE 281 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 3217 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 286, column 7)
              _lhsOpres =
                  {-# LINE 286 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrList_Expense _self _lhsIpath) error_
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 3223 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 288, column 7)
              _lhsOparseErrors =
                  {-# LINE 288 "src/PresentationAG_Generated.ag" #-}
                  getErrorMessages error_
                  {-# LINE 3228 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 460, column 7)
              _lhsOpresXML =
                  {-# LINE 460 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Expense _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  presParseErr (Node_ParseErrList_Expense _self _lhsIpath) error_
                  {-# LINE 3234 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 552, column 7)
              _lhsOpresTree =
                  {-# LINE 552 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Expense _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    presParseErr (Node_ParseErrList_Expense _self _lhsIpath) error_
                  {-# LINE 3240 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 111, column 18)
              _lhsOtotal =
                  {-# LINE 111 "src/PresentationAG.ag" #-}
                  0
                  {-# LINE 3245 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrList_Expense error_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 3256 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 3261 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 3266 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 3271 "src/PresentationAG.hs" #-}
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself,_lhsOtotal,_lhsOwhitespaceMap)))
-- List_Task ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         doc                  : Document
         focusD               : FocusDoc
      chained attributes:
         ix                   : Int
         pIdC                 : Int
         path                 : Path
         whitespaceMap        : WhitespaceMap
      synthesized attributes:
         completed            : Bool
         parseErrors          : [ParseErrorMessage]
         pres                 : Presentation_Doc_Node_Clip_Token
         presTree             : Presentation_Doc_Node_Clip_Token
         presXML              : Presentation_Doc_Node_Clip_Token
         press                : [Presentation_Doc_Node_Clip_Token]
         self                 : SELF 
   alternatives:
      alternative HoleList_Task:
         visit 0:
            local self        : _
      alternative List_Task:
         child elts           : ConsList_Task 
         visit 0:
            local pres        : _
            local self        : _
      alternative ParseErrList_Task:
         child error          : {(ParseError Document Node ClipDoc UserToken)}
         visit 0:
            local self        : _
-}
-- cata
sem_List_Task :: List_Task  ->
                 T_List_Task 
sem_List_Task (HoleList_Task )  =
    (sem_List_Task_HoleList_Task )
sem_List_Task (List_Task _elts )  =
    (sem_List_Task_List_Task (sem_ConsList_Task _elts ) )
sem_List_Task (ParseErrList_Task _error )  =
    (sem_List_Task_ParseErrList_Task _error )
-- semantic domain
type T_List_Task  = Document ->
                    FocusDoc ->
                    Int ->
                    Int ->
                    Path ->
                    WhitespaceMap ->
                    ( Bool,Int,Int,([ParseErrorMessage]),Path,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,([Presentation_Doc_Node_Clip_Token]),List_Task,WhitespaceMap)
data Inh_List_Task  = Inh_List_Task {doc_Inh_List_Task :: Document,focusD_Inh_List_Task :: FocusDoc,ix_Inh_List_Task :: Int,pIdC_Inh_List_Task :: Int,path_Inh_List_Task :: Path,whitespaceMap_Inh_List_Task :: WhitespaceMap}
data Syn_List_Task  = Syn_List_Task {completed_Syn_List_Task :: Bool,ix_Syn_List_Task :: Int,pIdC_Syn_List_Task :: Int,parseErrors_Syn_List_Task :: [ParseErrorMessage],path_Syn_List_Task :: Path,pres_Syn_List_Task :: Presentation_Doc_Node_Clip_Token,presTree_Syn_List_Task :: Presentation_Doc_Node_Clip_Token,presXML_Syn_List_Task :: Presentation_Doc_Node_Clip_Token,press_Syn_List_Task :: [Presentation_Doc_Node_Clip_Token],self_Syn_List_Task :: List_Task,whitespaceMap_Syn_List_Task :: WhitespaceMap}
wrap_List_Task :: T_List_Task  ->
                  Inh_List_Task  ->
                  Syn_List_Task 
wrap_List_Task sem (Inh_List_Task _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )  =
    (let ( _lhsOcompleted,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself,_lhsOwhitespaceMap) =
             (sem _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )
     in  (Syn_List_Task _lhsOcompleted _lhsOix _lhsOpIdC _lhsOparseErrors _lhsOpath _lhsOpres _lhsOpresTree _lhsOpresXML _lhsOpress _lhsOself _lhsOwhitespaceMap ))
sem_List_Task_HoleList_Task :: T_List_Task 
sem_List_Task_HoleList_Task  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpress :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOcompleted :: Bool
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: List_Task
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 326, column 23)
              _lhsOpress =
                  {-# LINE 326 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 3355 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 336, column 7)
              _lhsOpres =
                  {-# LINE 336 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Task" (Node_HoleList_Task _self _lhsIpath) _lhsIpath
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 3361 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 485, column 7)
              _lhsOpresXML =
                  {-# LINE 485 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Task _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  presHole _lhsIfocusD "List_Task" (Node_HoleList_Task _self _lhsIpath) _lhsIpath
                  {-# LINE 3367 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 577, column 7)
              _lhsOpresTree =
                  {-# LINE 577 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Task _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    presHole _lhsIfocusD "List_Task" (Node_HoleList_Task _self _lhsIpath) _lhsIpath
                  {-# LINE 3373 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 229, column 22)
              _lhsOcompleted =
                  {-# LINE 229 "src/PresentationAG.ag" #-}
                  True
                  {-# LINE 3378 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 3383 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleList_Task
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 3394 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 3399 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 3404 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 3409 "src/PresentationAG.hs" #-}
          in  ( _lhsOcompleted,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself,_lhsOwhitespaceMap)))
sem_List_Task_List_Task :: T_ConsList_Task  ->
                           T_List_Task 
sem_List_Task_List_Task elts_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpress :: ([Presentation_Doc_Node_Clip_Token])
              _eltsOpIdC :: Int
              _lhsOpIdC :: Int
              _eltsOpath :: Path
              _eltsOix :: Int
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpath :: Path
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOcompleted :: Bool
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: List_Task
              _lhsOix :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              _eltsOdoc :: Document
              _eltsOfocusD :: FocusDoc
              _eltsOwhitespaceMap :: WhitespaceMap
              _eltsIcompleted :: Bool
              _eltsIpIdC :: Int
              _eltsIparseErrors :: ([ParseErrorMessage])
              _eltsIpress :: ([Presentation_Doc_Node_Clip_Token])
              _eltsIpressTree :: ([Presentation_Doc_Node_Clip_Token])
              _eltsIpressXML :: ([Presentation_Doc_Node_Clip_Token])
              _eltsIself :: ConsList_Task
              _eltsIwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG.ag"(line 202, column 7)
              _pres =
                  {-# LINE 202 "src/PresentationAG.ag" #-}
                  structural $
                    col _eltsIpress
                  {-# LINE 3450 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 318, column 7)
              _lhsOpress =
                  {-# LINE 318 "src/PresentationAG_Generated.ag" #-}
                  map ( loc (Node_List_Task _self _lhsIpath)
                      . presentFocus _lhsIfocusD _lhsIpath )
                      _eltsIpress
                  {-# LINE 3457 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 322, column 7)
              _eltsOpIdC =
                  {-# LINE 322 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 100
                  {-# LINE 3462 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 323, column 7)
              _lhsOpIdC =
                  {-# LINE 323 "src/PresentationAG_Generated.ag" #-}
                  _eltsIpIdC
                  {-# LINE 3467 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 324, column 7)
              _eltsOpath =
                  {-# LINE 324 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 3472 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 325, column 7)
              _eltsOix =
                  {-# LINE 325 "src/PresentationAG_Generated.ag" #-}
                  0
                  {-# LINE 3477 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 329, column 7)
              _lhsOpres =
                  {-# LINE 329 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Task _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath $ _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 3483 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 406, column 15)
              _lhsOpath =
                  {-# LINE 406 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 3488 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 479, column 7)
              _lhsOpresXML =
                  {-# LINE 479 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Task _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  col _eltsIpressXML
                  {-# LINE 3494 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 571, column 7)
              _lhsOpresTree =
                  {-# LINE 571 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Task _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    col _eltsIpressTree
                  {-# LINE 3500 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 229, column 22)
              _lhsOcompleted =
                  {-# LINE 229 "src/PresentationAG.ag" #-}
                  _eltsIcompleted
                  {-# LINE 3505 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  _eltsIparseErrors
                  {-# LINE 3510 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  List_Task _eltsIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 3521 "src/PresentationAG.hs" #-}
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _eltsIwhitespaceMap
                  {-# LINE 3526 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOdoc =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 3531 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOfocusD =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 3536 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 3541 "src/PresentationAG.hs" #-}
              ( _eltsIcompleted,_eltsIpIdC,_eltsIparseErrors,_eltsIpress,_eltsIpressTree,_eltsIpressXML,_eltsIself,_eltsIwhitespaceMap) =
                  (elts_ _eltsOdoc _eltsOfocusD _eltsOix _eltsOpIdC _eltsOpath _eltsOwhitespaceMap )
          in  ( _lhsOcompleted,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself,_lhsOwhitespaceMap)))
sem_List_Task_ParseErrList_Task :: ((ParseError Document Node ClipDoc UserToken)) ->
                                   T_List_Task 
sem_List_Task_ParseErrList_Task error_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpress :: ([Presentation_Doc_Node_Clip_Token])
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOcompleted :: Bool
              _lhsOself :: List_Task
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 327, column 23)
              _lhsOpress =
                  {-# LINE 327 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 3569 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 332, column 7)
              _lhsOpres =
                  {-# LINE 332 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrList_Task _self _lhsIpath) error_
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 3575 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 334, column 7)
              _lhsOparseErrors =
                  {-# LINE 334 "src/PresentationAG_Generated.ag" #-}
                  getErrorMessages error_
                  {-# LINE 3580 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 482, column 7)
              _lhsOpresXML =
                  {-# LINE 482 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Task _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  presParseErr (Node_ParseErrList_Task _self _lhsIpath) error_
                  {-# LINE 3586 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 574, column 7)
              _lhsOpresTree =
                  {-# LINE 574 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Task _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    presParseErr (Node_ParseErrList_Task _self _lhsIpath) error_
                  {-# LINE 3592 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 229, column 22)
              _lhsOcompleted =
                  {-# LINE 229 "src/PresentationAG.ag" #-}
                  True
                  {-# LINE 3597 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrList_Task error_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 3608 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 3613 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 3618 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 3623 "src/PresentationAG.hs" #-}
          in  ( _lhsOcompleted,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself,_lhsOwhitespaceMap)))
-- Task --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         doc                  : Document
         focusD               : FocusDoc
      chained attributes:
         ix                   : Int
         pIdC                 : Int
         path                 : Path
         whitespaceMap        : WhitespaceMap
      synthesized attributes:
         completed            : Bool
         parseErrors          : [ParseErrorMessage]
         pres                 : Presentation_Doc_Node_Clip_Token
         presTree             : Presentation_Doc_Node_Clip_Token
         presXML              : Presentation_Doc_Node_Clip_Token
         self                 : SELF 
   alternatives:
      alternative BasicTask:
         child description    : {String}
         child completed      : {Bool}
         visit 0:
            local pres        : _
            local completed   : _
            local self        : _
      alternative CompositeTask:
         child expanded       : {Bool}
         child description    : {String}
         child subtasks       : List_Task 
         visit 0:
            local pres        : _
            local self        : _
      alternative HoleTask:
         visit 0:
            local self        : _
      alternative ParseErrTask:
         child error          : {(ParseError Document Node ClipDoc UserToken)}
         visit 0:
            local self        : _
-}
-- cata
sem_Task :: Task  ->
            T_Task 
sem_Task (BasicTask _description _completed )  =
    (sem_Task_BasicTask _description _completed )
sem_Task (CompositeTask _expanded _description _subtasks )  =
    (sem_Task_CompositeTask _expanded _description (sem_List_Task _subtasks ) )
sem_Task (HoleTask )  =
    (sem_Task_HoleTask )
sem_Task (ParseErrTask _error )  =
    (sem_Task_ParseErrTask _error )
-- semantic domain
type T_Task  = Document ->
               FocusDoc ->
               Int ->
               Int ->
               Path ->
               WhitespaceMap ->
               ( Bool,Int,Int,([ParseErrorMessage]),Path,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,Task,WhitespaceMap)
data Inh_Task  = Inh_Task {doc_Inh_Task :: Document,focusD_Inh_Task :: FocusDoc,ix_Inh_Task :: Int,pIdC_Inh_Task :: Int,path_Inh_Task :: Path,whitespaceMap_Inh_Task :: WhitespaceMap}
data Syn_Task  = Syn_Task {completed_Syn_Task :: Bool,ix_Syn_Task :: Int,pIdC_Syn_Task :: Int,parseErrors_Syn_Task :: [ParseErrorMessage],path_Syn_Task :: Path,pres_Syn_Task :: Presentation_Doc_Node_Clip_Token,presTree_Syn_Task :: Presentation_Doc_Node_Clip_Token,presXML_Syn_Task :: Presentation_Doc_Node_Clip_Token,self_Syn_Task :: Task,whitespaceMap_Syn_Task :: WhitespaceMap}
wrap_Task :: T_Task  ->
             Inh_Task  ->
             Syn_Task 
wrap_Task sem (Inh_Task _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )  =
    (let ( _lhsOcompleted,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap) =
             (sem _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )
     in  (Syn_Task _lhsOcompleted _lhsOix _lhsOpIdC _lhsOparseErrors _lhsOpath _lhsOpres _lhsOpresTree _lhsOpresXML _lhsOself _lhsOwhitespaceMap ))
sem_Task_BasicTask :: String ->
                      Bool ->
                      T_Task 
sem_Task_BasicTask description_ completed_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpIdC :: Int
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpath :: Path
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOcompleted :: Bool
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: Task
              _lhsOix :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG.ag"(line 207, column 7)
              _pres =
                  {-# LINE 207 "src/PresentationAG.ag" #-}
                  structural $ row [ text " " `withFontFam` "Courier"
                                   , hSpace 5
                                   , presentCompleted completed_
                                       `withMouseDown` flipCompleted _lhsIpath _self
                                   , hSpace 2, text description_ `withColor` if completed_ then green else blue]
                  {-# LINE 3722 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG.ag"(line 233, column 7)
              _completed =
                  {-# LINE 233 "src/PresentationAG.ag" #-}
                  completed_
                  {-# LINE 3727 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 248, column 7)
              _lhsOpIdC =
                  {-# LINE 248 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 0
                  {-# LINE 3732 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 257, column 7)
              _lhsOpres =
                  {-# LINE 257 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_BasicTask _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 3738 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 396, column 15)
              _lhsOpath =
                  {-# LINE 396 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 3743 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 449, column 7)
              _lhsOpresXML =
                  {-# LINE 449 "src/PresentationAG_Generated.ag" #-}
                  presentElementXML _lhsIfocusD (Node_BasicTask _self _lhsIpath) _lhsIpath "BasicTask" [ presentPrimXMLString description_, presentPrimXMLBool completed_ ]
                  {-# LINE 3748 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 541, column 7)
              _lhsOpresTree =
                  {-# LINE 541 "src/PresentationAG_Generated.ag" #-}
                  presentElementTree _lhsIfocusD (Node_BasicTask _self _lhsIpath) _lhsIpath "BasicTask" [ presentPrimXMLString description_, presentPrimXMLBool completed_ ]
                  {-# LINE 3753 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 229, column 22)
              _lhsOcompleted =
                  {-# LINE 229 "src/PresentationAG.ag" #-}
                  completed_
                  {-# LINE 3758 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 3763 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  BasicTask description_ _completed
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 3774 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 3779 "src/PresentationAG.hs" #-}
          in  ( _lhsOcompleted,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
sem_Task_CompositeTask :: Bool ->
                          String ->
                          T_List_Task  ->
                          T_Task 
sem_Task_CompositeTask expanded_ description_ subtasks_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _subtasksOpIdC :: Int
              _lhsOpIdC :: Int
              _subtasksOpath :: Path
              _subtasksOix :: Int
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpath :: Path
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOcompleted :: Bool
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: Task
              _lhsOix :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              _subtasksOdoc :: Document
              _subtasksOfocusD :: FocusDoc
              _subtasksOwhitespaceMap :: WhitespaceMap
              _subtasksIcompleted :: Bool
              _subtasksIix :: Int
              _subtasksIpIdC :: Int
              _subtasksIparseErrors :: ([ParseErrorMessage])
              _subtasksIpath :: Path
              _subtasksIpres :: Presentation_Doc_Node_Clip_Token
              _subtasksIpresTree :: Presentation_Doc_Node_Clip_Token
              _subtasksIpresXML :: Presentation_Doc_Node_Clip_Token
              _subtasksIpress :: ([Presentation_Doc_Node_Clip_Token])
              _subtasksIself :: List_Task
              _subtasksIwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG.ag"(line 214, column 7)
              _pres =
                  {-# LINE 214 "src/PresentationAG.ag" #-}
                  structural $ col $ row [ (text $ if expanded_ then "-" else "+") `withFontFam` "Courier"
                                           `withMouseDown` flipExpanded _lhsIpath _self
                                         , hSpace 5
                                         , presentCompleted _subtasksIcompleted
                                             `withMouseDown` setCompletedRec (not _subtasksIcompleted) _lhsIpath _self
                                         , hSpace 2
                                         ,  text description_
                                              `withColor` if _subtasksIcompleted then green else blue
                                         ]
                                   : if expanded_
                                     then [row [text " " `withFontFam` "Courier", hSpace 5, _subtasksIpres]]
                                     else []
                  {-# LINE 3834 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 250, column 7)
              _subtasksOpIdC =
                  {-# LINE 250 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 0
                  {-# LINE 3839 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 251, column 7)
              _lhsOpIdC =
                  {-# LINE 251 "src/PresentationAG_Generated.ag" #-}
                  _subtasksIpIdC
                  {-# LINE 3844 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 252, column 7)
              _subtasksOpath =
                  {-# LINE 252 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[2]
                  {-# LINE 3849 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 255, column 5)
              _subtasksOix =
                  {-# LINE 255 "src/PresentationAG_Generated.ag" #-}
                  2
                  {-# LINE 3854 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 260, column 7)
              _lhsOpres =
                  {-# LINE 260 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_CompositeTask _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 3860 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 397, column 19)
              _lhsOpath =
                  {-# LINE 397 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 3865 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 451, column 7)
              _lhsOpresXML =
                  {-# LINE 451 "src/PresentationAG_Generated.ag" #-}
                  presentElementXML _lhsIfocusD (Node_CompositeTask _self _lhsIpath) _lhsIpath "CompositeTask" [ presentPrimXMLBool expanded_, presentPrimXMLString description_, _subtasksIpresXML ]
                  {-# LINE 3870 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 543, column 7)
              _lhsOpresTree =
                  {-# LINE 543 "src/PresentationAG_Generated.ag" #-}
                  presentElementTree _lhsIfocusD (Node_CompositeTask _self _lhsIpath) _lhsIpath "CompositeTask" [ presentPrimXMLBool expanded_, presentPrimXMLString description_, _subtasksIpresTree ]
                  {-# LINE 3875 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 229, column 22)
              _lhsOcompleted =
                  {-# LINE 229 "src/PresentationAG.ag" #-}
                  _subtasksIcompleted
                  {-# LINE 3880 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  _subtasksIparseErrors
                  {-# LINE 3885 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  CompositeTask expanded_ description_ _subtasksIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _subtasksIix
                  {-# LINE 3896 "src/PresentationAG.hs" #-}
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _subtasksIwhitespaceMap
                  {-# LINE 3901 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _subtasksOdoc =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 3906 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _subtasksOfocusD =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 3911 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _subtasksOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 3916 "src/PresentationAG.hs" #-}
              ( _subtasksIcompleted,_subtasksIix,_subtasksIpIdC,_subtasksIparseErrors,_subtasksIpath,_subtasksIpres,_subtasksIpresTree,_subtasksIpresXML,_subtasksIpress,_subtasksIself,_subtasksIwhitespaceMap) =
                  (subtasks_ _subtasksOdoc _subtasksOfocusD _subtasksOix _subtasksOpIdC _subtasksOpath _subtasksOwhitespaceMap )
          in  ( _lhsOcompleted,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
sem_Task_HoleTask :: T_Task 
sem_Task_HoleTask  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOcompleted :: Bool
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: Task
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 263, column 7)
              _lhsOpres =
                  {-# LINE 263 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Task" (Node_HoleTask _self _lhsIpath) _lhsIpath
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 3943 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 452, column 18)
              _lhsOpresXML =
                  {-# LINE 452 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Task" (Node_HoleTask _self _lhsIpath) _lhsIpath
                  {-# LINE 3948 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 544, column 18)
              _lhsOpresTree =
                  {-# LINE 544 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Task" (Node_HoleTask _self _lhsIpath) _lhsIpath
                  {-# LINE 3953 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 229, column 22)
              _lhsOcompleted =
                  {-# LINE 229 "src/PresentationAG.ag" #-}
                  True
                  {-# LINE 3958 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 3963 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleTask
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 3974 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 3979 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 3984 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 3989 "src/PresentationAG.hs" #-}
          in  ( _lhsOcompleted,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
sem_Task_ParseErrTask :: ((ParseError Document Node ClipDoc UserToken)) ->
                         T_Task 
sem_Task_ParseErrTask error_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOcompleted :: Bool
              _lhsOself :: Task
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 266, column 7)
              _lhsOpres =
                  {-# LINE 266 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrTask _self _lhsIpath) error_
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 4015 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 268, column 7)
              _lhsOparseErrors =
                  {-# LINE 268 "src/PresentationAG_Generated.ag" #-}
                  getErrorMessages error_
                  {-# LINE 4020 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 453, column 18)
              _lhsOpresXML =
                  {-# LINE 453 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrTask _self _lhsIpath) error_
                  {-# LINE 4025 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 545, column 18)
              _lhsOpresTree =
                  {-# LINE 545 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrTask _self _lhsIpath) error_
                  {-# LINE 4030 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 229, column 22)
              _lhsOcompleted =
                  {-# LINE 229 "src/PresentationAG.ag" #-}
                  True
                  {-# LINE 4035 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrTask error_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 4046 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 4051 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 4056 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 4061 "src/PresentationAG.hs" #-}
          in  ( _lhsOcompleted,_lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
-- Tasks -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         doc                  : Document
         focusD               : FocusDoc
      chained attributes:
         ix                   : Int
         pIdC                 : Int
         path                 : Path
         whitespaceMap        : WhitespaceMap
      synthesized attributes:
         parseErrors          : [ParseErrorMessage]
         pres                 : Presentation_Doc_Node_Clip_Token
         presTree             : Presentation_Doc_Node_Clip_Token
         presXML              : Presentation_Doc_Node_Clip_Token
         self                 : SELF 
   alternatives:
      alternative HoleTasks:
         visit 0:
            local self        : _
      alternative ParseErrTasks:
         child error          : {(ParseError Document Node ClipDoc UserToken)}
         visit 0:
            local self        : _
      alternative Tasks:
         child tasks          : List_Task 
         visit 0:
            local pres        : _
            local self        : _
-}
-- cata
sem_Tasks :: Tasks  ->
             T_Tasks 
sem_Tasks (HoleTasks )  =
    (sem_Tasks_HoleTasks )
sem_Tasks (ParseErrTasks _error )  =
    (sem_Tasks_ParseErrTasks _error )
sem_Tasks (Tasks _tasks )  =
    (sem_Tasks_Tasks (sem_List_Task _tasks ) )
-- semantic domain
type T_Tasks  = Document ->
                FocusDoc ->
                Int ->
                Int ->
                Path ->
                WhitespaceMap ->
                ( Int,Int,([ParseErrorMessage]),Path,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,Tasks,WhitespaceMap)
data Inh_Tasks  = Inh_Tasks {doc_Inh_Tasks :: Document,focusD_Inh_Tasks :: FocusDoc,ix_Inh_Tasks :: Int,pIdC_Inh_Tasks :: Int,path_Inh_Tasks :: Path,whitespaceMap_Inh_Tasks :: WhitespaceMap}
data Syn_Tasks  = Syn_Tasks {ix_Syn_Tasks :: Int,pIdC_Syn_Tasks :: Int,parseErrors_Syn_Tasks :: [ParseErrorMessage],path_Syn_Tasks :: Path,pres_Syn_Tasks :: Presentation_Doc_Node_Clip_Token,presTree_Syn_Tasks :: Presentation_Doc_Node_Clip_Token,presXML_Syn_Tasks :: Presentation_Doc_Node_Clip_Token,self_Syn_Tasks :: Tasks,whitespaceMap_Syn_Tasks :: WhitespaceMap}
wrap_Tasks :: T_Tasks  ->
              Inh_Tasks  ->
              Syn_Tasks 
wrap_Tasks sem (Inh_Tasks _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )  =
    (let ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap) =
             (sem _lhsIdoc _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )
     in  (Syn_Tasks _lhsOix _lhsOpIdC _lhsOparseErrors _lhsOpath _lhsOpres _lhsOpresTree _lhsOpresXML _lhsOself _lhsOwhitespaceMap ))
sem_Tasks_HoleTasks :: T_Tasks 
sem_Tasks_HoleTasks  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: Tasks
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 239, column 7)
              _lhsOpres =
                  {-# LINE 239 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Tasks" (Node_HoleTasks _self _lhsIpath) _lhsIpath
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 4142 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 444, column 19)
              _lhsOpresXML =
                  {-# LINE 444 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Tasks" (Node_HoleTasks _self _lhsIpath) _lhsIpath
                  {-# LINE 4147 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 536, column 19)
              _lhsOpresTree =
                  {-# LINE 536 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Tasks" (Node_HoleTasks _self _lhsIpath) _lhsIpath
                  {-# LINE 4152 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 4157 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleTasks
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 4168 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 4173 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 4178 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 4183 "src/PresentationAG.hs" #-}
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
sem_Tasks_ParseErrTasks :: ((ParseError Document Node ClipDoc UserToken)) ->
                           T_Tasks 
sem_Tasks_ParseErrTasks error_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOself :: Tasks
              _lhsOix :: Int
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 242, column 7)
              _lhsOpres =
                  {-# LINE 242 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrTasks _self _lhsIpath) error_
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 4208 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 244, column 7)
              _lhsOparseErrors =
                  {-# LINE 244 "src/PresentationAG_Generated.ag" #-}
                  getErrorMessages error_
                  {-# LINE 4213 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 445, column 19)
              _lhsOpresXML =
                  {-# LINE 445 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrTasks _self _lhsIpath) error_
                  {-# LINE 4218 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 537, column 19)
              _lhsOpresTree =
                  {-# LINE 537 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrTasks _self _lhsIpath) error_
                  {-# LINE 4223 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrTasks error_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 4234 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 4239 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 4244 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 4249 "src/PresentationAG.hs" #-}
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
sem_Tasks_Tasks :: T_List_Task  ->
                   T_Tasks 
sem_Tasks_Tasks tasks_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _tasksOpIdC :: Int
              _lhsOpIdC :: Int
              _tasksOpath :: Path
              _tasksOix :: Int
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpath :: Path
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOparseErrors :: ([ParseErrorMessage])
              _lhsOself :: Tasks
              _lhsOix :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              _tasksOdoc :: Document
              _tasksOfocusD :: FocusDoc
              _tasksOwhitespaceMap :: WhitespaceMap
              _tasksIcompleted :: Bool
              _tasksIix :: Int
              _tasksIpIdC :: Int
              _tasksIparseErrors :: ([ParseErrorMessage])
              _tasksIpath :: Path
              _tasksIpres :: Presentation_Doc_Node_Clip_Token
              _tasksIpresTree :: Presentation_Doc_Node_Clip_Token
              _tasksIpresXML :: Presentation_Doc_Node_Clip_Token
              _tasksIpress :: ([Presentation_Doc_Node_Clip_Token])
              _tasksIself :: List_Task
              _tasksIwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG.ag"(line 198, column 7)
              _pres =
                  {-# LINE 198 "src/PresentationAG.ag" #-}
                  structural $ _tasksIpres
                  {-# LINE 4290 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 230, column 7)
              _tasksOpIdC =
                  {-# LINE 230 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 0
                  {-# LINE 4295 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 231, column 7)
              _lhsOpIdC =
                  {-# LINE 231 "src/PresentationAG_Generated.ag" #-}
                  _tasksIpIdC
                  {-# LINE 4300 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 232, column 7)
              _tasksOpath =
                  {-# LINE 232 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[0]
                  {-# LINE 4305 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 234, column 5)
              _tasksOix =
                  {-# LINE 234 "src/PresentationAG_Generated.ag" #-}
                  0
                  {-# LINE 4310 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 236, column 7)
              _lhsOpres =
                  {-# LINE 236 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_Tasks _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 4316 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 393, column 11)
              _lhsOpath =
                  {-# LINE 393 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 4321 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 443, column 7)
              _lhsOpresXML =
                  {-# LINE 443 "src/PresentationAG_Generated.ag" #-}
                  presentElementXML _lhsIfocusD (Node_Tasks _self _lhsIpath) _lhsIpath "Tasks" [ _tasksIpresXML ]
                  {-# LINE 4326 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 535, column 7)
              _lhsOpresTree =
                  {-# LINE 535 "src/PresentationAG_Generated.ag" #-}
                  presentElementTree _lhsIfocusD (Node_Tasks _self _lhsIpath) _lhsIpath "Tasks" [ _tasksIpresTree ]
                  {-# LINE 4331 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 115, column 33)
              _lhsOparseErrors =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  _tasksIparseErrors
                  {-# LINE 4336 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Tasks _tasksIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOix =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  _tasksIix
                  {-# LINE 4347 "src/PresentationAG.hs" #-}
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _tasksIwhitespaceMap
                  {-# LINE 4352 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tasksOdoc =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 4357 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tasksOfocusD =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 4362 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tasksOwhitespaceMap =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 4367 "src/PresentationAG.hs" #-}
              ( _tasksIcompleted,_tasksIix,_tasksIpIdC,_tasksIparseErrors,_tasksIpath,_tasksIpres,_tasksIpresTree,_tasksIpresXML,_tasksIpress,_tasksIself,_tasksIwhitespaceMap) =
                  (tasks_ _tasksOdoc _tasksOfocusD _tasksOix _tasksOpIdC _tasksOpath _tasksOwhitespaceMap )
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))