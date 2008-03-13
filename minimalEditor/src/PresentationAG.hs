
-- UUAGC 0.9.5 (src/PresentationAG.ag)
module PresentationAG where
{-# LINE 2 "src/PresentationAG.ag" #-}

import Common.CommonTypes hiding (Dirty (..))
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


{-# LINE 28 "src/PresentationAG.hs" #-}
{-# LINE 9 "src/PresentationAG_Generated.ag" #-}

-- type PresentationSheet doc enr node clip token = 
--        enr -> FocusDoc -> WhitespaceMap -> IDPCounter -> 
--        (WhitespaceMap, IDPCounter, Presentation doc node clip token)

presentationSheet :: PresentationSheet Document EnrichedDoc Node ClipDoc UserToken
presentationSheet enrichedDoc focusD whiteSpaceMap pIdC = 
  let (Syn_EnrichedDoc pIdC' pres self whitespaceMap) = 
        wrap_EnrichedDoc (sem_EnrichedDoc enrichedDoc) (Inh_EnrichedDoc focusD pIdC [] whitespaceMap)
  in  (whitespaceMap, pIdC', pres)

{- 
A type error here means that extra attributes were declared on EnrichedDoc
The attribute signature for EnrichedDoc should be:

EnrichedDoc  [ focusD : FocusDoc path : Path
             | pIdC : Int layoutMap : WhitespaceMap
             | pres : Presentation_Doc_Node_Clip_Token EnrichedDoc 
             ]
-}
{-# LINE 50 "src/PresentationAG.hs" #-}
-- EnrichedDoc -------------------------------------------------
{-
   visit 0:
      inherited attributes:
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
         child presentation   : {(Presentation Document Node ClipDoc UserToken)}
         visit 0:
            local self        : _
      alternative RootEnr:
         child tree           : Tree
         visit 0:
            local self        : _
-}
-- cata
sem_EnrichedDoc :: EnrichedDoc ->
                   T_EnrichedDoc
sem_EnrichedDoc (HoleEnrichedDoc )  =
    (sem_EnrichedDoc_HoleEnrichedDoc )
sem_EnrichedDoc (ParseErrEnrichedDoc _presentation )  =
    (sem_EnrichedDoc_ParseErrEnrichedDoc _presentation )
sem_EnrichedDoc (RootEnr _tree )  =
    (sem_EnrichedDoc_RootEnr (sem_Tree _tree ) )
-- semantic domain
type T_EnrichedDoc = FocusDoc ->
                     Int ->
                     Path ->
                     WhitespaceMap ->
                     ( Int,Presentation_Doc_Node_Clip_Token,EnrichedDoc,WhitespaceMap)
data Inh_EnrichedDoc = Inh_EnrichedDoc {focusD_Inh_EnrichedDoc :: FocusDoc,pIdC_Inh_EnrichedDoc :: Int,path_Inh_EnrichedDoc :: Path,whitespaceMap_Inh_EnrichedDoc :: WhitespaceMap}
data Syn_EnrichedDoc = Syn_EnrichedDoc {pIdC_Syn_EnrichedDoc :: Int,pres_Syn_EnrichedDoc :: Presentation_Doc_Node_Clip_Token,self_Syn_EnrichedDoc :: EnrichedDoc,whitespaceMap_Syn_EnrichedDoc :: WhitespaceMap}
wrap_EnrichedDoc sem (Inh_EnrichedDoc _lhsIfocusD _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )  =
    (let ( _lhsOpIdC,_lhsOpres,_lhsOself,_lhsOwhitespaceMap) =
             (sem _lhsIfocusD _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )
     in  (Syn_EnrichedDoc _lhsOpIdC _lhsOpres _lhsOself _lhsOwhitespaceMap ))
sem_EnrichedDoc_HoleEnrichedDoc :: T_EnrichedDoc
sem_EnrichedDoc_HoleEnrichedDoc  =
    (\ _lhsIfocusD
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOself :: EnrichedDoc
              _lhsOpIdC :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 76, column 25)
              _lhsOpres =
                  {-# LINE 76 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "EnrichedDoc" (Node_HoleEnrichedDoc _self _lhsIpath) _lhsIpath
                  {-# LINE 111 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleEnrichedDoc
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 122 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 127 "src/PresentationAG.hs" #-}
          in  ( _lhsOpIdC,_lhsOpres,_lhsOself,_lhsOwhitespaceMap)))
sem_EnrichedDoc_ParseErrEnrichedDoc :: ((Presentation Document Node ClipDoc UserToken)) ->
                                       T_EnrichedDoc
sem_EnrichedDoc_ParseErrEnrichedDoc presentation_  =
    (\ _lhsIfocusD
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOself :: EnrichedDoc
              _lhsOpIdC :: Int
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 77, column 25)
              _lhsOpres =
                  {-# LINE 77 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrEnrichedDoc _self _lhsIpath) presentation_
                  {-# LINE 144 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrEnrichedDoc presentation_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 155 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 160 "src/PresentationAG.hs" #-}
          in  ( _lhsOpIdC,_lhsOpres,_lhsOself,_lhsOwhitespaceMap)))
sem_EnrichedDoc_RootEnr :: T_Tree ->
                           T_EnrichedDoc
sem_EnrichedDoc_RootEnr tree_  =
    (\ _lhsIfocusD
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _treeOpIdC :: Int
              _lhsOpIdC :: Int
              _treeOpath :: Path
              _lhsOself :: EnrichedDoc
              _lhsOwhitespaceMap :: WhitespaceMap
              _treeOfocusD :: FocusDoc
              _treeOwhitespaceMap :: WhitespaceMap
              _treeIpIdC :: Int
              _treeIpath :: Path
              _treeIpres :: Presentation_Doc_Node_Clip_Token
              _treeIpresTree :: Presentation_Doc_Node_Clip_Token
              _treeIpresXML :: Presentation_Doc_Node_Clip_Token
              _treeIself :: Tree
              _treeIwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG.ag"(line 31, column 7)
              _lhsOpres =
                  {-# LINE 31 "src/PresentationAG.ag" #-}
                  loc (Node_RootEnr _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    row [ text "Tree: ", _treeIpres ]
                  {-# LINE 189 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 73, column 7)
              _treeOpIdC =
                  {-# LINE 73 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 0
                  {-# LINE 194 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 74, column 7)
              _lhsOpIdC =
                  {-# LINE 74 "src/PresentationAG_Generated.ag" #-}
                  _treeIpIdC
                  {-# LINE 199 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 75, column 7)
              _treeOpath =
                  {-# LINE 75 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[0]
                  {-# LINE 204 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  RootEnr _treeIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _treeIwhitespaceMap
                  {-# LINE 215 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _treeOfocusD =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 220 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _treeOwhitespaceMap =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 225 "src/PresentationAG.hs" #-}
              ( _treeIpIdC,_treeIpath,_treeIpres,_treeIpresTree,_treeIpresXML,_treeIself,_treeIwhitespaceMap) =
                  (tree_ _treeOfocusD _treeOpIdC _treeOpath _treeOwhitespaceMap )
          in  ( _lhsOpIdC,_lhsOpres,_lhsOself,_lhsOwhitespaceMap)))
-- Tree --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         focusD               : FocusDoc
      chained attributes:
         pIdC                 : Int
         path                 : Path
         whitespaceMap        : WhitespaceMap
      synthesized attributes:
         pres                 : Presentation_Doc_Node_Clip_Token
         presTree             : Presentation_Doc_Node_Clip_Token
         presXML              : Presentation_Doc_Node_Clip_Token
         self                 : SELF
   alternatives:
      alternative Bin:
         child left           : Tree
         child right          : Tree
         visit 0:
            local self        : _
      alternative HoleTree:
         visit 0:
            local self        : _
      alternative Leaf:
         visit 0:
            local self        : _
      alternative ParseErrTree:
         child presentation   : {(Presentation Document Node ClipDoc UserToken)}
         visit 0:
            local self        : _
-}
-- cata
sem_Tree :: Tree ->
            T_Tree
sem_Tree (Bin _left _right )  =
    (sem_Tree_Bin (sem_Tree _left ) (sem_Tree _right ) )
sem_Tree (HoleTree )  =
    (sem_Tree_HoleTree )
sem_Tree (Leaf )  =
    (sem_Tree_Leaf )
sem_Tree (ParseErrTree _presentation )  =
    (sem_Tree_ParseErrTree _presentation )
-- semantic domain
type T_Tree = FocusDoc ->
              Int ->
              Path ->
              WhitespaceMap ->
              ( Int,Path,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,Presentation_Doc_Node_Clip_Token,Tree,WhitespaceMap)
data Inh_Tree = Inh_Tree {focusD_Inh_Tree :: FocusDoc,pIdC_Inh_Tree :: Int,path_Inh_Tree :: Path,whitespaceMap_Inh_Tree :: WhitespaceMap}
data Syn_Tree = Syn_Tree {pIdC_Syn_Tree :: Int,path_Syn_Tree :: Path,pres_Syn_Tree :: Presentation_Doc_Node_Clip_Token,presTree_Syn_Tree :: Presentation_Doc_Node_Clip_Token,presXML_Syn_Tree :: Presentation_Doc_Node_Clip_Token,self_Syn_Tree :: Tree,whitespaceMap_Syn_Tree :: WhitespaceMap}
wrap_Tree sem (Inh_Tree _lhsIfocusD _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )  =
    (let ( _lhsOpIdC,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap) =
             (sem _lhsIfocusD _lhsIpIdC _lhsIpath _lhsIwhitespaceMap )
     in  (Syn_Tree _lhsOpIdC _lhsOpath _lhsOpres _lhsOpresTree _lhsOpresXML _lhsOself _lhsOwhitespaceMap ))
sem_Tree_Bin :: T_Tree ->
                T_Tree ->
                T_Tree
sem_Tree_Bin left_ right_  =
    (\ _lhsIfocusD
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _leftOpIdC :: Int
              _rightOpIdC :: Int
              _lhsOpIdC :: Int
              _leftOpath :: Path
              _rightOpath :: Path
              _lhsOpath :: Path
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOself :: Tree
              _lhsOwhitespaceMap :: WhitespaceMap
              _leftOfocusD :: FocusDoc
              _leftOwhitespaceMap :: WhitespaceMap
              _rightOfocusD :: FocusDoc
              _rightOwhitespaceMap :: WhitespaceMap
              _leftIpIdC :: Int
              _leftIpath :: Path
              _leftIpres :: Presentation_Doc_Node_Clip_Token
              _leftIpresTree :: Presentation_Doc_Node_Clip_Token
              _leftIpresXML :: Presentation_Doc_Node_Clip_Token
              _leftIself :: Tree
              _leftIwhitespaceMap :: WhitespaceMap
              _rightIpIdC :: Int
              _rightIpath :: Path
              _rightIpres :: Presentation_Doc_Node_Clip_Token
              _rightIpresTree :: Presentation_Doc_Node_Clip_Token
              _rightIpresXML :: Presentation_Doc_Node_Clip_Token
              _rightIself :: Tree
              _rightIwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG.ag"(line 44, column 7)
              _lhsOpres =
                  {-# LINE 44 "src/PresentationAG.ag" #-}
                  loc (Node_Bin _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    row [ col [ vSpace 10, _leftIpres]
                        , text "Bin"
                        , col [ vSpace 10, _rightIpres ]
                        ]
                  {-# LINE 328 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 81, column 7)
              _leftOpIdC =
                  {-# LINE 81 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 0
                  {-# LINE 333 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 82, column 7)
              _rightOpIdC =
                  {-# LINE 82 "src/PresentationAG_Generated.ag" #-}
                  _leftIpIdC
                  {-# LINE 338 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 83, column 7)
              _lhsOpIdC =
                  {-# LINE 83 "src/PresentationAG_Generated.ag" #-}
                  _rightIpIdC
                  {-# LINE 343 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 84, column 7)
              _leftOpath =
                  {-# LINE 84 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[0]
                  {-# LINE 348 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 85, column 7)
              _rightOpath =
                  {-# LINE 85 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath++[1]
                  {-# LINE 353 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 99, column 9)
              _lhsOpath =
                  {-# LINE 99 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 358 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 111, column 7)
              _lhsOpresXML =
                  {-# LINE 111 "src/PresentationAG_Generated.ag" #-}
                  presentElementXML _lhsIfocusD (Node_Bin _self _lhsIpath) _lhsIpath "Bin" [ _leftIpresXML, _rightIpresXML ]
                  {-# LINE 363 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 126, column 7)
              _lhsOpresTree =
                  {-# LINE 126 "src/PresentationAG_Generated.ag" #-}
                  presentElementTree _lhsIfocusD (Node_Bin _self _lhsIpath) _lhsIpath "Bin" [ _leftIpresTree, _rightIpresTree ]
                  {-# LINE 368 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Bin _leftIself _rightIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOwhitespaceMap =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _rightIwhitespaceMap
                  {-# LINE 379 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _leftOfocusD =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 384 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _leftOwhitespaceMap =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 389 "src/PresentationAG.hs" #-}
              -- copy rule (down)
              _rightOfocusD =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _lhsIfocusD
                  {-# LINE 394 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _rightOwhitespaceMap =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _leftIwhitespaceMap
                  {-# LINE 399 "src/PresentationAG.hs" #-}
              ( _leftIpIdC,_leftIpath,_leftIpres,_leftIpresTree,_leftIpresXML,_leftIself,_leftIwhitespaceMap) =
                  (left_ _leftOfocusD _leftOpIdC _leftOpath _leftOwhitespaceMap )
              ( _rightIpIdC,_rightIpath,_rightIpres,_rightIpresTree,_rightIpresXML,_rightIself,_rightIwhitespaceMap) =
                  (right_ _rightOfocusD _rightOpIdC _rightOpath _rightOwhitespaceMap )
          in  ( _lhsOpIdC,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
sem_Tree_HoleTree :: T_Tree
sem_Tree_HoleTree  =
    (\ _lhsIfocusD
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOself :: Tree
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 88, column 18)
              _lhsOpres =
                  {-# LINE 88 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Tree" (Node_HoleTree _self _lhsIpath) _lhsIpath
                  {-# LINE 422 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 114, column 18)
              _lhsOpresXML =
                  {-# LINE 114 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Tree" (Node_HoleTree _self _lhsIpath) _lhsIpath
                  {-# LINE 427 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 129, column 18)
              _lhsOpresTree =
                  {-# LINE 129 "src/PresentationAG_Generated.ag" #-}
                  presHole _lhsIfocusD "Tree" (Node_HoleTree _self _lhsIpath) _lhsIpath
                  {-# LINE 432 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  HoleTree
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 443 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 62 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 448 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 453 "src/PresentationAG.hs" #-}
          in  ( _lhsOpIdC,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
sem_Tree_Leaf :: T_Tree
sem_Tree_Leaf  =
    (\ _lhsIfocusD
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOself :: Tree
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG.ag"(line 50, column 7)
              _lhsOpres =
                  {-# LINE 50 "src/PresentationAG.ag" #-}
                  loc (Node_Leaf _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    row [ text "Leaf" ]
                  {-# LINE 473 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 87, column 7)
              _lhsOpIdC =
                  {-# LINE 87 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC + 0
                  {-# LINE 478 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 100, column 10)
              _lhsOpath =
                  {-# LINE 100 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 483 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 113, column 7)
              _lhsOpresXML =
                  {-# LINE 113 "src/PresentationAG_Generated.ag" #-}
                  presentElementXML _lhsIfocusD (Node_Leaf _self _lhsIpath) _lhsIpath "Leaf" [  ]
                  {-# LINE 488 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 128, column 7)
              _lhsOpresTree =
                  {-# LINE 128 "src/PresentationAG_Generated.ag" #-}
                  presentElementTree _lhsIfocusD (Node_Leaf _self _lhsIpath) _lhsIpath "Leaf" [  ]
                  {-# LINE 493 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  Leaf
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 504 "src/PresentationAG.hs" #-}
          in  ( _lhsOpIdC,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))
sem_Tree_ParseErrTree :: ((Presentation Document Node ClipDoc UserToken)) ->
                         T_Tree
sem_Tree_ParseErrTree presentation_  =
    (\ _lhsIfocusD
       _lhsIpIdC
       _lhsIpath
       _lhsIwhitespaceMap ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip_Token
              _lhsOpresXML :: Presentation_Doc_Node_Clip_Token
              _lhsOpresTree :: Presentation_Doc_Node_Clip_Token
              _lhsOself :: Tree
              _lhsOpIdC :: Int
              _lhsOpath :: Path
              _lhsOwhitespaceMap :: WhitespaceMap
              -- "src/PresentationAG_Generated.ag"(line 89, column 18)
              _lhsOpres =
                  {-# LINE 89 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrTree _self _lhsIpath) presentation_
                  {-# LINE 524 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 115, column 18)
              _lhsOpresXML =
                  {-# LINE 115 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrTree _self _lhsIpath) presentation_
                  {-# LINE 529 "src/PresentationAG.hs" #-}
              -- "src/PresentationAG_Generated.ag"(line 130, column 18)
              _lhsOpresTree =
                  {-# LINE 130 "src/PresentationAG_Generated.ag" #-}
                  presParseErr (Node_ParseErrTree _self _lhsIpath) presentation_
                  {-# LINE 534 "src/PresentationAG.hs" #-}
              -- self rule
              _self =
                  ParseErrTree presentation_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpIdC
                  {-# LINE 545 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOpath =
                  {-# LINE 62 "src/PresentationAG_Generated.ag" #-}
                  _lhsIpath
                  {-# LINE 550 "src/PresentationAG.hs" #-}
              -- copy rule (chain)
              _lhsOwhitespaceMap =
                  {-# LINE 56 "src/PresentationAG_Generated.ag" #-}
                  _lhsIwhitespaceMap
                  {-# LINE 555 "src/PresentationAG.hs" #-}
          in  ( _lhsOpIdC,_lhsOpath,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOwhitespaceMap)))