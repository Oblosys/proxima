

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
{-# LINE 111 "src/PresentationAG.ag" #-}

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
{-# LINE 121 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG_Generated.ag"(line 234, column 7)
              _headOpath =
                  {-# LINE 234 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[_lhsIix]
                  {-# LINE 226 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 235, column 7)
              _tailOpath =
                  {-# LINE 235 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 231 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 236, column 7)
              _lhsOpress =
                  {-# LINE 236 "src/PresentationAG_Generated.ag" #-}
                  _headIpres : _tailIpress
                  {-# LINE 236 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 237, column 7)
              _headOpIdC =
                  {-# LINE 237 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 30
                  {-# LINE 241 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 238, column 7)
              _tailOpIdC =
                  {-# LINE 238 "src/PresentationAG_Generated.ag" #-}
                  _headIpIdC
                  {-# LINE 246 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 239, column 7)
              _lhsOpIdC =
                  {-# LINE 239 "src/PresentationAG_Generated.ag" #-}
                  _tailIpIdC
                  {-# LINE 251 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 240, column 7)
              _tailOix =
                  {-# LINE 240 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix + 1
                  {-# LINE 256 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 317, column 19)
              _lhsOpressXML =
                  {-# LINE 317 "src/PresentationAG_Generated.ag" #-}
                  _headIpresXML : _tailIpressXML
                  {-# LINE 261 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 372, column 19)
              _lhsOpressTree =
                  {-# LINE 372 "src/PresentationAG_Generated.ag" #-}
                  _headIpresTree : _tailIpressTree
                  {-# LINE 266 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 90, column 26)
              _lhsOallCurrencies =
                  {-# LINE 90 "src/PresentationAG.ag" #-}
                  _headIallCurrencies ++ _tailIallCurrencies
                  {-# LINE 271 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 89, column 33)
              _lhsOparseErrors =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  _headIparseErrors ++ _tailIparseErrors
                  {-# LINE 276 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Cons_Currency _headIself _tailIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _tailIwhitespaceMap
                  {-# LINE 287 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOdoc =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 292 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOfocusD =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 297 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOix =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 302 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 307 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tailOdoc =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 312 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tailOfocusD =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 317 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _tailOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _headIwhitespaceMap
                  {-# LINE 322 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG_Generated.ag"(line 241, column 23)
              _lhsOpress =
                  {-# LINE 241 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 348 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 318, column 19)
              _lhsOpressXML =
                  {-# LINE 318 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 353 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 373, column 19)
              _lhsOpressTree =
                  {-# LINE 373 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 358 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 90, column 26)
              _lhsOallCurrencies =
                  {-# LINE 90 "src/PresentationAG.ag" #-}
                  []
                  {-# LINE 363 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 89, column 33)
              _lhsOparseErrors =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 368 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Nil_Currency
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 379 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 384 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG_Generated.ag"(line 223, column 7)
              _headOpath =
                  {-# LINE 223 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[_lhsIix]
                  {-# LINE 495 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 224, column 7)
              _tailOpath =
                  {-# LINE 224 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 500 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 225, column 7)
              _lhsOpress =
                  {-# LINE 225 "src/PresentationAG_Generated.ag" #-}
                  _headIpres : _tailIpress
                  {-# LINE 505 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 226, column 7)
              _headOpIdC =
                  {-# LINE 226 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 30
                  {-# LINE 510 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 227, column 7)
              _tailOpIdC =
                  {-# LINE 227 "src/PresentationAG_Generated.ag" #-}
                  _headIpIdC
                  {-# LINE 515 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 228, column 7)
              _lhsOpIdC =
                  {-# LINE 228 "src/PresentationAG_Generated.ag" #-}
                  _tailIpIdC
                  {-# LINE 520 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 229, column 7)
              _tailOix =
                  {-# LINE 229 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix + 1
                  {-# LINE 525 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 313, column 18)
              _lhsOpressXML =
                  {-# LINE 313 "src/PresentationAG_Generated.ag" #-}
                  _headIpresXML : _tailIpressXML
                  {-# LINE 530 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 368, column 18)
              _lhsOpressTree =
                  {-# LINE 368 "src/PresentationAG_Generated.ag" #-}
                  _headIpresTree : _tailIpressTree
                  {-# LINE 535 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 89, column 33)
              _lhsOparseErrors =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  _headIparseErrors ++ _tailIparseErrors
                  {-# LINE 540 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 105, column 18)
              _lhsOtotal =
                  {-# LINE 105 "src/PresentationAG.ag" #-}
                  _headItotal + _tailItotal
                  {-# LINE 545 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Cons_Expense _headIself _tailIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _tailIwhitespaceMap
                  {-# LINE 556 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOallCurrencies =
                  {-# LINE 97 "src/PresentationAG.ag" #-}
                  _lhsIallCurrencies
                  {-# LINE 561 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOdoc =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 566 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOfocusD =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 571 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOix =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 576 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _headOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 581 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tailOallCurrencies =
                  {-# LINE 97 "src/PresentationAG.ag" #-}
                  _lhsIallCurrencies
                  {-# LINE 586 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tailOdoc =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 591 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _tailOfocusD =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 596 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _tailOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _headIwhitespaceMap
                  {-# LINE 601 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG_Generated.ag"(line 230, column 22)
              _lhsOpress =
                  {-# LINE 230 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 628 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 314, column 18)
              _lhsOpressXML =
                  {-# LINE 314 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 633 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 369, column 18)
              _lhsOpressTree =
                  {-# LINE 369 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 638 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 89, column 33)
              _lhsOparseErrors =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 643 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 105, column 18)
              _lhsOtotal =
                  {-# LINE 105 "src/PresentationAG.ag" #-}
                  0
                  {-# LINE 648 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Nil_Expense
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 659 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 664 "src/PresentationAG.hs" #-}
          in  ( _lhsOpIdC,_lhsOparseErrors,_lhsOpress,_lhsOpressTree,_lhsOpressXML,_lhsOself,_lhsOtotal,_lhsOwhitespaceMap)))
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
              -- "src/PresentationAG.ag"(line 82, column 7)
              _pres =
                  {-# LINE 82 "src/PresentationAG.ag" #-}
                  structural $ col [ textField 80 name_
                                   , textField 120 $ show euroRate_
                                   ]
                  {-# LINE 751 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG.ag"(line 94, column 7)
              _lhsOallCurrencies =
                  {-# LINE 94 "src/PresentationAG.ag" #-}
                  [(name_, euroRate_)]
                  {-# LINE 756 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 162, column 7)
              _lhsOpIdC =
                  {-# LINE 162 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 0
                  {-# LINE 761 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 165, column 7)
              _lhsOpres =
                  {-# LINE 165 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_Currency _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 767 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 257, column 14)
              _lhsOpath =
                  {-# LINE 257 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 772 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 286, column 7)
              _lhsOpresXML =
                  {-# LINE 286 "src/PresentationAG_Generated.ag" #-}
                  presentElementXML _lhsIfocusD (Node_Currency _self _lhsIpath) _lhsIpath "Currency" [ presentPrimXMLString name_, presentPrimXMLFloat euroRate_ ]
                  {-# LINE 777 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 341, column 7)
              _lhsOpresTree =
                  {-# LINE 341 "src/PresentationAG_Generated.ag" #-}
                  presentElementTree _lhsIfocusD (Node_Currency _self _lhsIpath) _lhsIpath "Currency" [ presentPrimXMLString name_, presentPrimXMLFloat euroRate_ ]
                  {-# LINE 782 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 89, column 33)
              _lhsOparseErrors =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 787 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Currency name_ euroRate_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 798 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 803 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG_Generated.ag"(line 168, column 7)
              _lhsOpres =
                  {-# LINE 168 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Currency" (Node_HoleCurrency _self _lhsIpath) _lhsIpath
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 828 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 287, column 22)
              _lhsOpresXML =
                  {-# LINE 287 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Currency" (Node_HoleCurrency _self _lhsIpath) _lhsIpath
                  {-# LINE 833 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 342, column 22)
              _lhsOpresTree =
                  {-# LINE 342 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Currency" (Node_HoleCurrency _self _lhsIpath) _lhsIpath
                  {-# LINE 838 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 90, column 26)
              _lhsOallCurrencies =
                  {-# LINE 90 "src/PresentationAG.ag" #-}
                  []
                  {-# LINE 843 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 89, column 33)
              _lhsOparseErrors =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 848 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleCurrency
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 859 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 864 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 869 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 874 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG_Generated.ag"(line 171, column 7)
              _lhsOpres =
                  {-# LINE 171 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrCurrency _self _lhsIpath) error_
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 900 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 173, column 7)
              _lhsOparseErrors =
                  {-# LINE 173 "src/PresentationAG_Generated.ag" #-}
                  getErrorMessages error_
                  {-# LINE 905 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 288, column 22)
              _lhsOpresXML =
                  {-# LINE 288 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrCurrency _self _lhsIpath) error_
                  {-# LINE 910 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 343, column 22)
              _lhsOpresTree =
                  {-# LINE 343 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrCurrency _self _lhsIpath) error_
                  {-# LINE 915 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 90, column 26)
              _lhsOallCurrencies =
                  {-# LINE 90 "src/PresentationAG.ag" #-}
                  []
                  {-# LINE 920 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrCurrency error_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 931 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 936 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 941 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 946 "src/PresentationAG.hs" #-}
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
         child form           : Form 
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
sem_EnrichedDoc (RootEnr _form )  =
    (sem_EnrichedDoc_RootEnr (sem_Form _form ) )
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
              -- "src/PresentationAG_Generated.ag"(line 118, column 7)
              _lhsOpres =
                  {-# LINE 118 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "EnrichedDoc" (Node_HoleEnrichedDoc _self _lhsIpath) _lhsIpath
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1016 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleEnrichedDoc
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 1027 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1032 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG_Generated.ag"(line 121, column 7)
              _lhsOpres =
                  {-# LINE 121 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrEnrichedDoc _self _lhsIpath) error_
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1051 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrEnrichedDoc error_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 1062 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1067 "src/PresentationAG.hs" #-}
          in  ( _lhsOpIdC,_lhsOpres,_lhsOself,_lhsOwhitespaceMap)))
sem_EnrichedDoc_RootEnr :: T_Form  ->
                           T_EnrichedDoc 
sem_EnrichedDoc_RootEnr form_  =
    (\ _lhsIdoc
       _lhsIfocusD
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _formOpIdC :: Int
              _lhsOpIdC :: Int
              _formOpath :: Path
              _formOix :: Int
              _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOself :: EnrichedDoc
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
              -- "src/PresentationAG.ag"(line 34, column 7)
              _pres =
                  {-# LINE 34 "src/PresentationAG.ag" #-}
                  structural $ _formIpres `withbgColor` lightBlue
                  {-# LINE 1100 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 109, column 7)
              _formOpIdC =
                  {-# LINE 109 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 0
                  {-# LINE 1105 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 110, column 7)
              _lhsOpIdC =
                  {-# LINE 110 "src/PresentationAG_Generated.ag" #-}
                  _formIpIdC
                  {-# LINE 1110 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 111, column 7)
              _formOpath =
                  {-# LINE 111 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[0]
                  {-# LINE 1115 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 113, column 5)
              _formOix =
                  {-# LINE 113 "src/PresentationAG_Generated.ag" #-}
                  0
                  {-# LINE 1120 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 115, column 7)
              _lhsOpres =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_RootEnr _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1126 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  RootEnr _formIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _formIwhitespaceMap
                  {-# LINE 1137 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _formOdoc =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 1142 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _formOfocusD =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 1147 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _formOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1152 "src/PresentationAG.hs" #-}
              ( _formIix,_formIpIdC,_formIparseErrors,_formIpath,_formIpres,_formIpresTree,_formIpresXML,_formIself,_formIwhitespaceMap) =
                  (form_ _formOdoc _formOfocusD _formOix _formOpIdC _formOpath _formOwhitespaceMap )
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
              -- "src/PresentationAG.ag"(line 60, column 7)
              _pres =
                  {-# LINE 60 "src/PresentationAG.ag" #-}
                  structural $ addCurrencyItems _lhsIallCurrencies _lhsIpath $
                    row [ textField 400 description_
                        , hSpace 5
                        , textField 30 (show amount_)
                        , hSpace 5
                        , textField 60 $ (fst $ index "Expense.pres" _lhsIallCurrencies currencyIx_)
                        , hSpace 10
                        , textField 120 (showCurrency _total ++ " Euro")
                        ]
                  {-# LINE 1253 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG.ag"(line 109, column 7)
              _total =
                  {-# LINE 109 "src/PresentationAG.ag" #-}
                  amount_ * (snd $ index "Expense.total" _lhsIallCurrencies currencyIx_)
                  {-# LINE 1258 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 147, column 7)
              _lhsOpIdC =
                  {-# LINE 147 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 0
                  {-# LINE 1263 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 150, column 7)
              _lhsOpres =
                  {-# LINE 150 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_Expense _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1269 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 254, column 13)
              _lhsOpath =
                  {-# LINE 254 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 1274 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 280, column 7)
              _lhsOpresXML =
                  {-# LINE 280 "src/PresentationAG_Generated.ag" #-}
                  presentElementXML _lhsIfocusD (Node_Expense _self _lhsIpath) _lhsIpath "Expense" [ presentPrimXMLString description_, presentPrimXMLFloat amount_, presentPrimXMLInt currencyIx_ ]
                  {-# LINE 1279 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 335, column 7)
              _lhsOpresTree =
                  {-# LINE 335 "src/PresentationAG_Generated.ag" #-}
                  presentElementTree _lhsIfocusD (Node_Expense _self _lhsIpath) _lhsIpath "Expense" [ presentPrimXMLString description_, presentPrimXMLFloat amount_, presentPrimXMLInt currencyIx_ ]
                  {-# LINE 1284 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 89, column 33)
              _lhsOparseErrors =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 1289 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 105, column 18)
              _lhsOtotal =
                  {-# LINE 105 "src/PresentationAG.ag" #-}
                  _total
                  {-# LINE 1294 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Expense description_ amount_ currencyIx_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 1305 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1310 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG_Generated.ag"(line 153, column 7)
              _lhsOpres =
                  {-# LINE 153 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Expense" (Node_HoleExpense _self _lhsIpath) _lhsIpath
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1336 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 281, column 21)
              _lhsOpresXML =
                  {-# LINE 281 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Expense" (Node_HoleExpense _self _lhsIpath) _lhsIpath
                  {-# LINE 1341 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 336, column 21)
              _lhsOpresTree =
                  {-# LINE 336 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Expense" (Node_HoleExpense _self _lhsIpath) _lhsIpath
                  {-# LINE 1346 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 89, column 33)
              _lhsOparseErrors =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 1351 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 105, column 18)
              _lhsOtotal =
                  {-# LINE 105 "src/PresentationAG.ag" #-}
                  0
                  {-# LINE 1356 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleExpense
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 1367 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 1372 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 1377 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1382 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG_Generated.ag"(line 156, column 7)
              _lhsOpres =
                  {-# LINE 156 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrExpense _self _lhsIpath) error_
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1409 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 158, column 7)
              _lhsOparseErrors =
                  {-# LINE 158 "src/PresentationAG_Generated.ag" #-}
                  getErrorMessages error_
                  {-# LINE 1414 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 282, column 21)
              _lhsOpresXML =
                  {-# LINE 282 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrExpense _self _lhsIpath) error_
                  {-# LINE 1419 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 337, column 21)
              _lhsOpresTree =
                  {-# LINE 337 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrExpense _self _lhsIpath) error_
                  {-# LINE 1424 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 105, column 18)
              _lhsOtotal =
                  {-# LINE 105 "src/PresentationAG.ag" #-}
                  0
                  {-# LINE 1429 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrExpense error_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 1440 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 1445 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 1450 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1455 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG.ag"(line 38, column 7)
              _pres =
                  {-# LINE 38 "src/PresentationAG.ag" #-}
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
                  {-# LINE 1591 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG.ag"(line 101, column 7)
              _expensesOallCurrencies =
                  {-# LINE 101 "src/PresentationAG.ag" #-}
                  _currenciesIallCurrencies
                  {-# LINE 1596 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 126, column 7)
              _expensesOpIdC =
                  {-# LINE 126 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 0
                  {-# LINE 1601 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 127, column 7)
              _currenciesOpIdC =
                  {-# LINE 127 "src/PresentationAG_Generated.ag" #-}
                  _expensesIpIdC
                  {-# LINE 1606 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 128, column 7)
              _lhsOpIdC =
                  {-# LINE 128 "src/PresentationAG_Generated.ag" #-}
                  _currenciesIpIdC
                  {-# LINE 1611 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 129, column 7)
              _expensesOpath =
                  {-# LINE 129 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[2]
                  {-# LINE 1616 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 130, column 7)
              _currenciesOpath =
                  {-# LINE 130 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[3]
                  {-# LINE 1621 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 132, column 5)
              _expensesOix =
                  {-# LINE 132 "src/PresentationAG_Generated.ag" #-}
                  2
                  {-# LINE 1626 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 133, column 5)
              _currenciesOix =
                  {-# LINE 133 "src/PresentationAG_Generated.ag" #-}
                  3
                  {-# LINE 1631 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 135, column 7)
              _lhsOpres =
                  {-# LINE 135 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_Form _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1637 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 251, column 10)
              _lhsOpath =
                  {-# LINE 251 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 1642 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 274, column 7)
              _lhsOpresXML =
                  {-# LINE 274 "src/PresentationAG_Generated.ag" #-}
                  presentElementXML _lhsIfocusD (Node_Form _self _lhsIpath) _lhsIpath "Form" [ presentPrimXMLString name_, presentPrimXMLString faculty_, _expensesIpresXML, _currenciesIpresXML ]
                  {-# LINE 1647 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 329, column 7)
              _lhsOpresTree =
                  {-# LINE 329 "src/PresentationAG_Generated.ag" #-}
                  presentElementTree _lhsIfocusD (Node_Form _self _lhsIpath) _lhsIpath "Form" [ presentPrimXMLString name_, presentPrimXMLString faculty_, _expensesIpresTree, _currenciesIpresTree ]
                  {-# LINE 1652 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 89, column 33)
              _lhsOparseErrors =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  _expensesIparseErrors ++ _currenciesIparseErrors
                  {-# LINE 1657 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Form name_ faculty_ _expensesIself _currenciesIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOix =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _currenciesIix
                  {-# LINE 1668 "src/PresentationAG.hs" #-}
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _currenciesIwhitespaceMap
                  {-# LINE 1673 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _expensesOdoc =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 1678 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _expensesOfocusD =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 1683 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _expensesOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1688 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _currenciesOdoc =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 1693 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _currenciesOfocusD =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 1698 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _currenciesOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _expensesIwhitespaceMap
                  {-# LINE 1703 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG_Generated.ag"(line 138, column 7)
              _lhsOpres =
                  {-# LINE 138 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Form" (Node_HoleForm _self _lhsIpath) _lhsIpath
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1731 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 275, column 18)
              _lhsOpresXML =
                  {-# LINE 275 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Form" (Node_HoleForm _self _lhsIpath) _lhsIpath
                  {-# LINE 1736 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 330, column 18)
              _lhsOpresTree =
                  {-# LINE 330 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Form" (Node_HoleForm _self _lhsIpath) _lhsIpath
                  {-# LINE 1741 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 89, column 33)
              _lhsOparseErrors =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 1746 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleForm
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 1757 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 1762 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 1767 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1772 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG_Generated.ag"(line 141, column 7)
              _lhsOpres =
                  {-# LINE 141 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrForm _self _lhsIpath) error_
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1797 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 143, column 7)
              _lhsOparseErrors =
                  {-# LINE 143 "src/PresentationAG_Generated.ag" #-}
                  getErrorMessages error_
                  {-# LINE 1802 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 276, column 18)
              _lhsOpresXML =
                  {-# LINE 276 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrForm _self _lhsIpath) error_
                  {-# LINE 1807 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 331, column 18)
              _lhsOpresTree =
                  {-# LINE 331 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrForm _self _lhsIpath) error_
                  {-# LINE 1812 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrForm error_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 1823 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 1828 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 1833 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1838 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG_Generated.ag"(line 208, column 27)
              _lhsOpress =
                  {-# LINE 208 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 1922 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 218, column 7)
              _lhsOpres =
                  {-# LINE 218 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Currency" (Node_HoleList_Currency _self _lhsIpath) _lhsIpath
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 1928 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 309, column 7)
              _lhsOpresXML =
                  {-# LINE 309 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Currency _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  presHole _lhsIfocusD "List_Currency" (Node_HoleList_Currency _self _lhsIpath) _lhsIpath
                  {-# LINE 1934 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 364, column 7)
              _lhsOpresTree =
                  {-# LINE 364 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Currency _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    presHole _lhsIfocusD "List_Currency" (Node_HoleList_Currency _self _lhsIpath) _lhsIpath
                  {-# LINE 1940 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 90, column 26)
              _lhsOallCurrencies =
                  {-# LINE 90 "src/PresentationAG.ag" #-}
                  []
                  {-# LINE 1945 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 89, column 33)
              _lhsOparseErrors =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 1950 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleList_Currency
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 1961 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 1966 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 1971 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 1976 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG.ag"(line 75, column 7)
              _pres =
                  {-# LINE 75 "src/PresentationAG.ag" #-}
                  structural $
                    row $ intersperse (hSpace 5) _eltsIpress ++
                          [ hSpace 5, text "add currency" `withMouseDown` addNewCurrency _lhsIpath ]
                  {-# LINE 2018 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 200, column 7)
              _lhsOpress =
                  {-# LINE 200 "src/PresentationAG_Generated.ag" #-}
                  map ( loc (Node_List_Currency _self _lhsIpath)
                      . presentFocus _lhsIfocusD _lhsIpath )
                      _eltsIpress
                  {-# LINE 2025 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 204, column 7)
              _eltsOpIdC =
                  {-# LINE 204 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 100
                  {-# LINE 2030 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 205, column 7)
              _lhsOpIdC =
                  {-# LINE 205 "src/PresentationAG_Generated.ag" #-}
                  _eltsIpIdC
                  {-# LINE 2035 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 206, column 7)
              _eltsOpath =
                  {-# LINE 206 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 2040 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 207, column 7)
              _eltsOix =
                  {-# LINE 207 "src/PresentationAG_Generated.ag" #-}
                  0
                  {-# LINE 2045 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 211, column 7)
              _lhsOpres =
                  {-# LINE 211 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Currency _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath $ _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 2051 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 263, column 19)
              _lhsOpath =
                  {-# LINE 263 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 2056 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 303, column 7)
              _lhsOpresXML =
                  {-# LINE 303 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Currency _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  col _eltsIpressXML
                  {-# LINE 2062 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 358, column 7)
              _lhsOpresTree =
                  {-# LINE 358 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Currency _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    col _eltsIpressTree
                  {-# LINE 2068 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 90, column 26)
              _lhsOallCurrencies =
                  {-# LINE 90 "src/PresentationAG.ag" #-}
                  _eltsIallCurrencies
                  {-# LINE 2073 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 89, column 33)
              _lhsOparseErrors =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  _eltsIparseErrors
                  {-# LINE 2078 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  List_Currency _eltsIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 2089 "src/PresentationAG.hs" #-}
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _eltsIwhitespaceMap
                  {-# LINE 2094 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOdoc =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 2099 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOfocusD =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 2104 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 2109 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG_Generated.ag"(line 209, column 27)
              _lhsOpress =
                  {-# LINE 209 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 2137 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 214, column 7)
              _lhsOpres =
                  {-# LINE 214 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrList_Currency _self _lhsIpath) error_
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 2143 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 216, column 7)
              _lhsOparseErrors =
                  {-# LINE 216 "src/PresentationAG_Generated.ag" #-}
                  getErrorMessages error_
                  {-# LINE 2148 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 306, column 7)
              _lhsOpresXML =
                  {-# LINE 306 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Currency _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  presParseErr (Node_ParseErrList_Currency _self _lhsIpath) error_
                  {-# LINE 2154 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 361, column 7)
              _lhsOpresTree =
                  {-# LINE 361 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Currency _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    presParseErr (Node_ParseErrList_Currency _self _lhsIpath) error_
                  {-# LINE 2160 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 90, column 26)
              _lhsOallCurrencies =
                  {-# LINE 90 "src/PresentationAG.ag" #-}
                  []
                  {-# LINE 2165 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrList_Currency error_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 2176 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 2181 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 2186 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 2191 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG_Generated.ag"(line 185, column 26)
              _lhsOpress =
                  {-# LINE 185 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 2278 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 195, column 7)
              _lhsOpres =
                  {-# LINE 195 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Expense" (Node_HoleList_Expense _self _lhsIpath) _lhsIpath
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 2284 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 298, column 7)
              _lhsOpresXML =
                  {-# LINE 298 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Expense _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  presHole _lhsIfocusD "List_Expense" (Node_HoleList_Expense _self _lhsIpath) _lhsIpath
                  {-# LINE 2290 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 353, column 7)
              _lhsOpresTree =
                  {-# LINE 353 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Expense _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    presHole _lhsIfocusD "List_Expense" (Node_HoleList_Expense _self _lhsIpath) _lhsIpath
                  {-# LINE 2296 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 89, column 33)
              _lhsOparseErrors =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 2301 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 105, column 18)
              _lhsOtotal =
                  {-# LINE 105 "src/PresentationAG.ag" #-}
                  0
                  {-# LINE 2306 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleList_Expense
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 2317 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 2322 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 2327 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 2332 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG.ag"(line 55, column 7)
              _pres =
                  {-# LINE 55 "src/PresentationAG.ag" #-}
                  structural $
                    col $ intersperse (vSpace 5) _eltsIpress
                  {-# LINE 2375 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 177, column 7)
              _lhsOpress =
                  {-# LINE 177 "src/PresentationAG_Generated.ag" #-}
                  map ( loc (Node_List_Expense _self _lhsIpath)
                      . presentFocus _lhsIfocusD _lhsIpath )
                      _eltsIpress
                  {-# LINE 2382 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 181, column 7)
              _eltsOpIdC =
                  {-# LINE 181 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 100
                  {-# LINE 2387 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 182, column 7)
              _lhsOpIdC =
                  {-# LINE 182 "src/PresentationAG_Generated.ag" #-}
                  _eltsIpIdC
                  {-# LINE 2392 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 183, column 7)
              _eltsOpath =
                  {-# LINE 183 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 2397 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 184, column 7)
              _eltsOix =
                  {-# LINE 184 "src/PresentationAG_Generated.ag" #-}
                  0
                  {-# LINE 2402 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 188, column 7)
              _lhsOpres =
                  {-# LINE 188 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Expense _self _lhsIpath) $ presentFocus _lhsIfocusD _lhsIpath $ _pres
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 2408 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 260, column 18)
              _lhsOpath =
                  {-# LINE 260 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 2413 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 292, column 7)
              _lhsOpresXML =
                  {-# LINE 292 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Expense _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  col _eltsIpressXML
                  {-# LINE 2419 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 347, column 7)
              _lhsOpresTree =
                  {-# LINE 347 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Expense _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    col _eltsIpressTree
                  {-# LINE 2425 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG_Generated.ag"(line 89, column 33)
              _lhsOparseErrors =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  _eltsIparseErrors
                  {-# LINE 2430 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 105, column 18)
              _lhsOtotal =
                  {-# LINE 105 "src/PresentationAG.ag" #-}
                  _eltsItotal
                  {-# LINE 2435 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  List_Expense _eltsIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 2446 "src/PresentationAG.hs" #-}
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _eltsIwhitespaceMap
                  {-# LINE 2451 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOallCurrencies =
                  {-# LINE 97 "src/PresentationAG.ag" #-}
                  _lhsIallCurrencies
                  {-# LINE 2456 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOdoc =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIdoc
                  {-# LINE 2461 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOfocusD =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 2466 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _eltsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 2471 "src/PresentationAG.hs" #-}
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
              -- "src/PresentationAG_Generated.ag"(line 186, column 26)
              _lhsOpress =
                  {-# LINE 186 "src/PresentationAG_Generated.ag" #-}
                  []
                  {-# LINE 2500 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 191, column 7)
              _lhsOpres =
                  {-# LINE 191 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrList_Expense _self _lhsIpath) error_
                  `withLocalPopupMenuItems` menuD (PathD _lhsIpath) _lhsIdoc
                  {-# LINE 2506 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 193, column 7)
              _lhsOparseErrors =
                  {-# LINE 193 "src/PresentationAG_Generated.ag" #-}
                  getErrorMessages error_
                  {-# LINE 2511 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 295, column 7)
              _lhsOpresXML =
                  {-# LINE 295 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Expense _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  presParseErr (Node_ParseErrList_Expense _self _lhsIpath) error_
                  {-# LINE 2517 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 350, column 7)
              _lhsOpresTree =
                  {-# LINE 350 "src/PresentationAG_Generated.ag" #-}
                  loc (Node_List_Expense _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    presParseErr (Node_ParseErrList_Expense _self _lhsIpath) error_
                  {-# LINE 2523 "src/PresentationAG.hs" #-}
              -- use rule "src/PresentationAG.ag"(line 105, column 18)
              _lhsOtotal =
                  {-# LINE 105 "src/PresentationAG.ag" #-}
                  0
                  {-# LINE 2528 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrList_Expense error_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOix =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIix
                  {-# LINE 2539 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 2544 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 92 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 2549 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 2554 "src/PresentationAG.hs" #-}
          in  ( _lhsOix,_lhsOpIdC,_lhsOparseErrors,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself,_lhsOtotal,_lhsOwhitespaceMap)))