-- UUAGC 0.9.3 (src/PresentationAG.ag)
module PresentationAG where

import CommonTypes
import PresLayerTypes
import PresLayerUtils

import XprezLib
import XLatex hiding (bold)

import DocumentEdit

import List
import qualified Data.Map as Map
import Data.Map (Map)


import DocTypes (DocumentLevel (..))
import DocTypes_Generated
import DocUtils_Generated
import DocumentEdit_Generated

presHole focus typeStr nd pth = loc nd $
  structural $ row [text $ "{"++typeStr++"}"] `withColor` black `withbgColor` yellow `withFontFam` ("Courier New")

presParseErr node pres =
  loc NoNode $ parsing $ pres `withbgColor` whiteSmoke

presentFocus NoPathD     path pres = pres
presentFocus (PathD pth) path pres = if pth==path then pres `withbgColor` focusCol else pres

focusCol = lightBlue


type Presentation_Doc_Node_Clip = Presentation Document Node ClipDoc

presentElementXML :: FocusDoc -> Node -> [Int] -> String -> [Presentation_Doc_Node_Clip] -> Presentation_Doc_Node_Clip
presentElementXML focusD node path tag children =
  loc node $ parsing $ presentFocus focusD path $                  
    if null children
    then col [ text $ "<"++tag++"/>"]
    else col [ text  $ "<"++tag++">"
             , row [ text "  ", col children ]
             , text $ "</"++tag++">" ]      
    
presentPrimXMLBool :: Bool -> Presentation_Doc_Node_Clip
presentPrimXMLBool x = text $ "<Bool>"++show x++"<Bool/>"

presentPrimXMLInt :: Int -> Presentation_Doc_Node_Clip
presentPrimXMLInt x = text $ "<Int>"++show x++"<Int/>"

presentPrimXMLString :: String -> Presentation_Doc_Node_Clip
presentPrimXMLString x = text $ "<String>"++x++"<String>"


presentElementTree :: FocusDoc -> Node -> [Int] -> String -> [Presentation_Doc_Node_Clip] -> Presentation_Doc_Node_Clip
presentElementTree focusD node path tag children =
  loc node $ parsing $ presentFocus focusD path $                  
    if null children
    then mkTreeLeaf False $ text $ tag
    else mkTreeNode False True (text tag) children
    
presentPrimTreeBool :: Bool -> Presentation_Doc_Node_Clip
presentPrimTreeBool x =  mkTreeLeaf False $ text $ "Bool: "++show x

presentPrimTreeInt :: Int -> Presentation_Doc_Node_Clip
presentPrimTreeInt x =  mkTreeLeaf False $ text $ "Int: "++show x

presentPrimTreeString :: String -> Presentation_Doc_Node_Clip
presentPrimTreeString x =  mkTreeLeaf False $ text $ "String: "++x



-- Bool_ -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         focusD               : FocusDoc
         ix                   : Int
         path                 : [Int]
      chained attribute:
         pIdC                 : Int
      synthesized attributes:
         bool                 : Bool
         pres                 : Presentation_Doc_Node_Clip
         presTree             : Presentation_Doc_Node_Clip
         presXML              : Presentation_Doc_Node_Clip
         self                 : SELF
   alternatives:
      alternative Bool_:
         child idd            : {IDD}
         child bool           : {Bool}
         local self           : _
      alternative HoleBool_:
         local self           : _
      alternative ParseErrBool_:
         child node           : {Node}
         child presentation   : {Presentation_Doc_Node_Clip}
         local self           : _
-}
-- cata
sem_Bool_ :: Bool_ ->
             T_Bool_
sem_Bool_ (Bool_ _idd _bool) =
    (sem_Bool__Bool_ _idd _bool)
sem_Bool_ (HoleBool_ ) =
    (sem_Bool__HoleBool_ )
sem_Bool_ (ParseErrBool_ _node _presentation) =
    (sem_Bool__ParseErrBool_ _node _presentation)
-- semantic domain
type T_Bool_ = FocusDoc ->
               Int ->
               Int ->
               ([Int]) ->
               ( Bool,Int,Presentation_Doc_Node_Clip,Presentation_Doc_Node_Clip,Presentation_Doc_Node_Clip,Bool_)
data Inh_Bool_ = Inh_Bool_ {focusD_Inh_Bool_ :: FocusDoc,ix_Inh_Bool_ :: Int,pIdC_Inh_Bool_ :: Int,path_Inh_Bool_ :: [Int]}
data Syn_Bool_ = Syn_Bool_ {bool_Syn_Bool_ :: Bool,pIdC_Syn_Bool_ :: Int,pres_Syn_Bool_ :: Presentation_Doc_Node_Clip,presTree_Syn_Bool_ :: Presentation_Doc_Node_Clip,presXML_Syn_Bool_ :: Presentation_Doc_Node_Clip,self_Syn_Bool_ :: Bool_}
wrap_Bool_ sem (Inh_Bool_ _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath) =
    (let ( _lhsObool,_lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself) =
             (sem _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath)
     in  (Syn_Bool_ _lhsObool _lhsOpIdC _lhsOpres _lhsOpresTree _lhsOpresXML _lhsOself))
sem_Bool__Bool_ :: IDD ->
                   Bool ->
                   T_Bool_
sem_Bool__Bool_ idd_ bool_ =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsObool :: Bool
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: Bool_
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 78, column 7)
              _lhsOpres =
                  loc (Bool_Node _self _lhsIpath) $ parsing $ presentFocus _lhsIfocusD _lhsIpath $
                    row [text $ show bool_, text ""]
              -- "src/PresentationAG_Generated.ag"(line 82, column 14)
              _lhsObool =
                  bool_
              -- "src/PresentationAG_Generated.ag"(line 256, column 7)
              _lhsOpresXML =
                  presentElementXML _lhsIfocusD (Bool_Node _self _lhsIpath) _lhsIpath "Bool_" [ presentPrimXMLBool bool_ ]
              -- "src/PresentationAG_Generated.ag"(line 314, column 7)
              _lhsOpresTree =
                  presentElementTree _lhsIfocusD (Bool_Node _self _lhsIpath) _lhsIpath "Bool_" [ presentPrimTreeBool bool_ ]
              -- self rule
              _self =
                  Bool_ idd_ bool_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsObool,_lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself)))
sem_Bool__HoleBool_ :: T_Bool_
sem_Bool__HoleBool_  =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsObool :: Bool
              _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: Bool_
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 84, column 13)
              _lhsObool =
                  False
              -- "src/PresentationAG_Generated.ag"(line 148, column 19)
              _lhsOpres =
                  presHole _lhsIfocusD "Bool_" (HoleBool_Node _self _lhsIpath) _lhsIpath
              -- "src/PresentationAG_Generated.ag"(line 257, column 19)
              _lhsOpresXML =
                  presHole _lhsIfocusD "Bool_" (HoleBool_Node _self _lhsIpath) _lhsIpath
              -- "src/PresentationAG_Generated.ag"(line 315, column 19)
              _lhsOpresTree =
                  presHole _lhsIfocusD "Bool_" (HoleBool_Node _self _lhsIpath) _lhsIpath
              -- self rule
              _self =
                  HoleBool_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsObool,_lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself)))
sem_Bool__ParseErrBool_ :: Node ->
                           Presentation_Doc_Node_Clip ->
                           T_Bool_
sem_Bool__ParseErrBool_ node_ presentation_ =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsObool :: Bool
              _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: Bool_
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 84, column 13)
              _lhsObool =
                  False
              -- "src/PresentationAG_Generated.ag"(line 149, column 19)
              _lhsOpres =
                  presParseErr node_ presentation_
              -- "src/PresentationAG_Generated.ag"(line 258, column 19)
              _lhsOpresXML =
                  presParseErr node_ presentation_
              -- "src/PresentationAG_Generated.ag"(line 316, column 19)
              _lhsOpresTree =
                  presParseErr node_ presentation_
              -- self rule
              _self =
                  ParseErrBool_ node_ presentation_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsObool,_lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself)))
-- ConsList_Dummy ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         focusD               : FocusDoc
         ix                   : Int
         path                 : [Int]
      chained attribute:
         pIdC                 : Int
      synthesized attributes:
         press                : [Presentation_Doc_Node_Clip]
         pressTree            : [Presentation_Doc_Node_Clip]
         pressXML             : [Presentation_Doc_Node_Clip]
         self                 : SELF
   alternatives:
      alternative Cons_Dummy:
         child head           : Dummy
         child tail           : ConsList_Dummy
         local self           : _
      alternative Nil_Dummy:
         local self           : _
-}
-- cata
sem_ConsList_Dummy :: ConsList_Dummy ->
                      T_ConsList_Dummy
sem_ConsList_Dummy (Cons_Dummy _head _tail) =
    (sem_ConsList_Dummy_Cons_Dummy (sem_Dummy _head) (sem_ConsList_Dummy _tail))
sem_ConsList_Dummy (Nil_Dummy ) =
    (sem_ConsList_Dummy_Nil_Dummy )
-- semantic domain
type T_ConsList_Dummy = FocusDoc ->
                        Int ->
                        Int ->
                        ([Int]) ->
                        ( Int,([Presentation_Doc_Node_Clip]),([Presentation_Doc_Node_Clip]),([Presentation_Doc_Node_Clip]),ConsList_Dummy)
data Inh_ConsList_Dummy = Inh_ConsList_Dummy {focusD_Inh_ConsList_Dummy :: FocusDoc,ix_Inh_ConsList_Dummy :: Int,pIdC_Inh_ConsList_Dummy :: Int,path_Inh_ConsList_Dummy :: [Int]}
data Syn_ConsList_Dummy = Syn_ConsList_Dummy {pIdC_Syn_ConsList_Dummy :: Int,press_Syn_ConsList_Dummy :: [Presentation_Doc_Node_Clip],pressTree_Syn_ConsList_Dummy :: [Presentation_Doc_Node_Clip],pressXML_Syn_ConsList_Dummy :: [Presentation_Doc_Node_Clip],self_Syn_ConsList_Dummy :: ConsList_Dummy}
wrap_ConsList_Dummy sem (Inh_ConsList_Dummy _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath) =
    (let ( _lhsOpIdC,_lhsOpress,_lhsOpressTree,_lhsOpressXML,_lhsOself) =
             (sem _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath)
     in  (Syn_ConsList_Dummy _lhsOpIdC _lhsOpress _lhsOpressTree _lhsOpressXML _lhsOself))
sem_ConsList_Dummy_Cons_Dummy :: T_Dummy ->
                                 T_ConsList_Dummy ->
                                 T_ConsList_Dummy
sem_ConsList_Dummy_Cons_Dummy head_ tail_ =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _headOpath :: ([Int])
              _tailOpath :: ([Int])
              _lhsOpress :: ([Presentation_Doc_Node_Clip])
              _headOpIdC :: Int
              _tailOpIdC :: Int
              _lhsOpIdC :: Int
              _tailOix :: Int
              _lhsOpressXML :: ([Presentation_Doc_Node_Clip])
              _lhsOpressTree :: ([Presentation_Doc_Node_Clip])
              _lhsOself :: ConsList_Dummy
              _headOfocusD :: FocusDoc
              _headOix :: Int
              _tailOfocusD :: FocusDoc
              _headIpIdC :: Int
              _headIpres :: Presentation_Doc_Node_Clip
              _headIpresTree :: Presentation_Doc_Node_Clip
              _headIpresXML :: Presentation_Doc_Node_Clip
              _headIself :: Dummy
              _tailIpIdC :: Int
              _tailIpress :: ([Presentation_Doc_Node_Clip])
              _tailIpressTree :: ([Presentation_Doc_Node_Clip])
              _tailIpressXML :: ([Presentation_Doc_Node_Clip])
              _tailIself :: ConsList_Dummy
              -- "src/PresentationAG_Generated.ag"(line 211, column 16)
              _headOpath =
                  _lhsIpath++[_lhsIix]
              -- "src/PresentationAG_Generated.ag"(line 212, column 15)
              _tailOpath =
                  _lhsIpath
              -- "src/PresentationAG_Generated.ag"(line 213, column 18)
              _lhsOpress =
                  _headIpres : _tailIpress
              -- "src/PresentationAG_Generated.ag"(line 214, column 18)
              _headOpIdC =
                  _lhsIpIdC + 30
              -- "src/PresentationAG_Generated.ag"(line 215, column 18)
              _tailOpIdC =
                  _headIpIdC
              -- "src/PresentationAG_Generated.ag"(line 216, column 18)
              _lhsOpIdC =
                  _tailIpIdC
              -- "src/PresentationAG_Generated.ag"(line 221, column 20)
              _tailOix =
                  _lhsIix + 1
              -- "src/PresentationAG_Generated.ag"(line 298, column 20)
              _lhsOpressXML =
                  _headIpresXML : _tailIpressXML
              -- "src/PresentationAG_Generated.ag"(line 356, column 20)
              _lhsOpressTree =
                  _headIpresTree : _tailIpressTree
              -- self rule
              _self =
                  Cons_Dummy _headIself _tailIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _headOfocusD =
                  _lhsIfocusD
              -- copy rule (down)
              _headOix =
                  _lhsIix
              -- copy rule (down)
              _tailOfocusD =
                  _lhsIfocusD
              ( _headIpIdC,_headIpres,_headIpresTree,_headIpresXML,_headIself) =
                  (head_ _headOfocusD _headOix _headOpIdC _headOpath)
              ( _tailIpIdC,_tailIpress,_tailIpressTree,_tailIpressXML,_tailIself) =
                  (tail_ _tailOfocusD _tailOix _tailOpIdC _tailOpath)
          in  ( _lhsOpIdC,_lhsOpress,_lhsOpressTree,_lhsOpressXML,_lhsOself)))
sem_ConsList_Dummy_Nil_Dummy :: T_ConsList_Dummy
sem_ConsList_Dummy_Nil_Dummy  =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOpress :: ([Presentation_Doc_Node_Clip])
              _lhsOpressXML :: ([Presentation_Doc_Node_Clip])
              _lhsOpressTree :: ([Presentation_Doc_Node_Clip])
              _lhsOself :: ConsList_Dummy
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 217, column 20)
              _lhsOpress =
                  []
              -- "src/PresentationAG_Generated.ag"(line 299, column 20)
              _lhsOpressXML =
                  []
              -- "src/PresentationAG_Generated.ag"(line 357, column 20)
              _lhsOpressTree =
                  []
              -- self rule
              _self =
                  Nil_Dummy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOpIdC,_lhsOpress,_lhsOpressTree,_lhsOpressXML,_lhsOself)))
-- Dummy -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         focusD               : FocusDoc
         ix                   : Int
         path                 : [Int]
      chained attribute:
         pIdC                 : Int
      synthesized attributes:
         pres                 : Presentation_Doc_Node_Clip
         presTree             : Presentation_Doc_Node_Clip
         presXML              : Presentation_Doc_Node_Clip
         self                 : SELF
   alternatives:
      alternative Dummy:
         child idd            : {IDD}
         child dummys         : List_Dummy
         child string_        : String_
         child bool_          : Bool_
         child int_           : Int_
         local self           : _
      alternative HoleDummy:
         local self           : _
      alternative ParseErrDummy:
         child node           : {Node}
         child presentation   : {Presentation_Doc_Node_Clip}
         local self           : _
-}
-- cata
sem_Dummy :: Dummy ->
             T_Dummy
sem_Dummy (Dummy _idd _dummys _string_ _bool_ _int_) =
    (sem_Dummy_Dummy _idd (sem_List_Dummy _dummys) (sem_String_ _string_) (sem_Bool_ _bool_) (sem_Int_ _int_))
sem_Dummy (HoleDummy ) =
    (sem_Dummy_HoleDummy )
sem_Dummy (ParseErrDummy _node _presentation) =
    (sem_Dummy_ParseErrDummy _node _presentation)
-- semantic domain
type T_Dummy = FocusDoc ->
               Int ->
               Int ->
               ([Int]) ->
               ( Int,Presentation_Doc_Node_Clip,Presentation_Doc_Node_Clip,Presentation_Doc_Node_Clip,Dummy)
data Inh_Dummy = Inh_Dummy {focusD_Inh_Dummy :: FocusDoc,ix_Inh_Dummy :: Int,pIdC_Inh_Dummy :: Int,path_Inh_Dummy :: [Int]}
data Syn_Dummy = Syn_Dummy {pIdC_Syn_Dummy :: Int,pres_Syn_Dummy :: Presentation_Doc_Node_Clip,presTree_Syn_Dummy :: Presentation_Doc_Node_Clip,presXML_Syn_Dummy :: Presentation_Doc_Node_Clip,self_Syn_Dummy :: Dummy}
wrap_Dummy sem (Inh_Dummy _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath) =
    (let ( _lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself) =
             (sem _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath)
     in  (Syn_Dummy _lhsOpIdC _lhsOpres _lhsOpresTree _lhsOpresXML _lhsOself))
sem_Dummy_Dummy :: IDD ->
                   T_List_Dummy ->
                   T_String_ ->
                   T_Bool_ ->
                   T_Int_ ->
                   T_Dummy
sem_Dummy_Dummy idd_ dummys_ string__ bool__ int__ =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _dummysOpIdC :: Int
              _int_OpIdC :: Int
              _bool_OpIdC :: Int
              _string_OpIdC :: Int
              _lhsOpIdC :: Int
              _dummysOpath :: ([Int])
              _string_Opath :: ([Int])
              _bool_Opath :: ([Int])
              _int_Opath :: ([Int])
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: Dummy
              _lhsOpres :: Presentation_Doc_Node_Clip
              _dummysOfocusD :: FocusDoc
              _string_OfocusD :: FocusDoc
              _string_Oix :: Int
              _bool_OfocusD :: FocusDoc
              _bool_Oix :: Int
              _int_OfocusD :: FocusDoc
              _int_Oix :: Int
              _dummysIpIdC :: Int
              _dummysIpresTree :: Presentation_Doc_Node_Clip
              _dummysIpresXML :: Presentation_Doc_Node_Clip
              _dummysIpress :: ([Presentation_Doc_Node_Clip])
              _dummysIself :: List_Dummy
              _string_Ilength :: Int
              _string_IpIdC :: Int
              _string_Ipres :: Presentation_Doc_Node_Clip
              _string_IpresTree :: Presentation_Doc_Node_Clip
              _string_IpresXML :: Presentation_Doc_Node_Clip
              _string_Iself :: String_
              _string_Istr :: String
              _bool_Ibool :: Bool
              _bool_IpIdC :: Int
              _bool_Ipres :: Presentation_Doc_Node_Clip
              _bool_IpresTree :: Presentation_Doc_Node_Clip
              _bool_IpresXML :: Presentation_Doc_Node_Clip
              _bool_Iself :: Bool_
              _int_Iint :: Int
              _int_IpIdC :: Int
              _int_Ipres :: Presentation_Doc_Node_Clip
              _int_IpresTree :: Presentation_Doc_Node_Clip
              _int_IpresXML :: Presentation_Doc_Node_Clip
              _int_Iself :: Int_
              -- "src/PresentationAG_Generated.ag"(line 158, column 11)
              _dummysOpIdC =
                  _lhsIpIdC + 0
              -- "src/PresentationAG_Generated.ag"(line 159, column 11)
              _int_OpIdC =
                  _bool_IpIdC
              -- "src/PresentationAG_Generated.ag"(line 160, column 11)
              _bool_OpIdC =
                  _string_IpIdC
              -- "src/PresentationAG_Generated.ag"(line 161, column 11)
              _string_OpIdC =
                  _dummysIpIdC
              -- "src/PresentationAG_Generated.ag"(line 162, column 11)
              _lhsOpIdC =
                  _int_IpIdC
              -- "src/PresentationAG_Generated.ag"(line 168, column 11)
              _dummysOpath =
                  _lhsIpath++[0]
              -- "src/PresentationAG_Generated.ag"(line 169, column 11)
              _string_Opath =
                  _lhsIpath++[1]
              -- "src/PresentationAG_Generated.ag"(line 170, column 11)
              _bool_Opath =
                  _lhsIpath++[2]
              -- "src/PresentationAG_Generated.ag"(line 171, column 11)
              _int_Opath =
                  _lhsIpath++[3]
              -- "src/PresentationAG_Generated.ag"(line 268, column 7)
              _lhsOpresXML =
                  presentElementXML _lhsIfocusD (DummyNode _self _lhsIpath) _lhsIpath "Dummy" [ _dummysIpresXML, _string_IpresXML, _bool_IpresXML, _int_IpresXML ]
              -- "src/PresentationAG_Generated.ag"(line 326, column 7)
              _lhsOpresTree =
                  presentElementTree _lhsIfocusD (DummyNode _self _lhsIpath) _lhsIpath "Dummy" [ _dummysIpresTree, _string_IpresTree, _bool_IpresTree, _int_IpresTree ]
              -- self rule
              _self =
                  Dummy idd_ _dummysIself _string_Iself _bool_Iself _int_Iself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOpres =
                  _int_Ipres
              -- copy rule (down)
              _dummysOfocusD =
                  _lhsIfocusD
              -- copy rule (down)
              _string_OfocusD =
                  _lhsIfocusD
              -- copy rule (down)
              _string_Oix =
                  _lhsIix
              -- copy rule (down)
              _bool_OfocusD =
                  _lhsIfocusD
              -- copy rule (down)
              _bool_Oix =
                  _lhsIix
              -- copy rule (down)
              _int_OfocusD =
                  _lhsIfocusD
              -- copy rule (down)
              _int_Oix =
                  _lhsIix
              ( _dummysIpIdC,_dummysIpresTree,_dummysIpresXML,_dummysIpress,_dummysIself) =
                  (dummys_ _dummysOfocusD _dummysOpIdC _dummysOpath)
              ( _string_Ilength,_string_IpIdC,_string_Ipres,_string_IpresTree,_string_IpresXML,_string_Iself,_string_Istr) =
                  (string__ _string_OfocusD _string_Oix _string_OpIdC _string_Opath)
              ( _bool_Ibool,_bool_IpIdC,_bool_Ipres,_bool_IpresTree,_bool_IpresXML,_bool_Iself) =
                  (bool__ _bool_OfocusD _bool_Oix _bool_OpIdC _bool_Opath)
              ( _int_Iint,_int_IpIdC,_int_Ipres,_int_IpresTree,_int_IpresXML,_int_Iself) =
                  (int__ _int_OfocusD _int_Oix _int_OpIdC _int_Opath)
          in  ( _lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself)))
sem_Dummy_HoleDummy :: T_Dummy
sem_Dummy_HoleDummy  =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: Dummy
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 163, column 19)
              _lhsOpres =
                  presHole _lhsIfocusD "Dummy" (HoleDummyNode _self _lhsIpath) _lhsIpath
              -- "src/PresentationAG_Generated.ag"(line 269, column 19)
              _lhsOpresXML =
                  presHole _lhsIfocusD "Dummy" (HoleDummyNode _self _lhsIpath) _lhsIpath
              -- "src/PresentationAG_Generated.ag"(line 327, column 19)
              _lhsOpresTree =
                  presHole _lhsIfocusD "Dummy" (HoleDummyNode _self _lhsIpath) _lhsIpath
              -- self rule
              _self =
                  HoleDummy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself)))
sem_Dummy_ParseErrDummy :: Node ->
                           Presentation_Doc_Node_Clip ->
                           T_Dummy
sem_Dummy_ParseErrDummy node_ presentation_ =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: Dummy
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 164, column 19)
              _lhsOpres =
                  presParseErr node_ presentation_
              -- "src/PresentationAG_Generated.ag"(line 270, column 19)
              _lhsOpresXML =
                  presParseErr node_ presentation_
              -- "src/PresentationAG_Generated.ag"(line 328, column 19)
              _lhsOpresTree =
                  presParseErr node_ presentation_
              -- self rule
              _self =
                  ParseErrDummy node_ presentation_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself)))
-- EnrichedDoc -------------------------------------------------
{-
   visit 0:
      inherited attribute:
         focusD               : FocusDoc
      chained attributes:
         layoutMap            : LayoutMap
         pIdC                 : Int
      synthesized attributes:
         pres                 : Presentation_Doc_Node_Clip
         self                 : SELF
   alternatives:
      alternative HoleEnrichedDoc:
         local self           : _
      alternative ParseErrEnrichedDoc:
         child node           : {Node}
         child presentation   : {Presentation_Doc_Node_Clip}
         local self           : _
      alternative RootEnr:
         child id             : {IDD}
         child root           : Root
         child document       : {Document}
         local self           : _
-}
-- cata
sem_EnrichedDoc :: EnrichedDoc ->
                   T_EnrichedDoc
sem_EnrichedDoc (HoleEnrichedDoc ) =
    (sem_EnrichedDoc_HoleEnrichedDoc )
sem_EnrichedDoc (ParseErrEnrichedDoc _node _presentation) =
    (sem_EnrichedDoc_ParseErrEnrichedDoc _node _presentation)
sem_EnrichedDoc (RootEnr _id _root _document) =
    (sem_EnrichedDoc_RootEnr _id (sem_Root _root) _document)
-- semantic domain
type T_EnrichedDoc = FocusDoc ->
                     LayoutMap ->
                     Int ->
                     ( LayoutMap,Int,Presentation_Doc_Node_Clip,EnrichedDoc)
data Inh_EnrichedDoc = Inh_EnrichedDoc {focusD_Inh_EnrichedDoc :: FocusDoc,layoutMap_Inh_EnrichedDoc :: LayoutMap,pIdC_Inh_EnrichedDoc :: Int}
data Syn_EnrichedDoc = Syn_EnrichedDoc {layoutMap_Syn_EnrichedDoc :: LayoutMap,pIdC_Syn_EnrichedDoc :: Int,pres_Syn_EnrichedDoc :: Presentation_Doc_Node_Clip,self_Syn_EnrichedDoc :: EnrichedDoc}
wrap_EnrichedDoc sem (Inh_EnrichedDoc _lhsIfocusD _lhsIlayoutMap _lhsIpIdC) =
    (let ( _lhsOlayoutMap,_lhsOpIdC,_lhsOpres,_lhsOself) =
             (sem _lhsIfocusD _lhsIlayoutMap _lhsIpIdC)
     in  (Syn_EnrichedDoc _lhsOlayoutMap _lhsOpIdC _lhsOpres _lhsOself))
sem_EnrichedDoc_HoleEnrichedDoc :: T_EnrichedDoc
sem_EnrichedDoc_HoleEnrichedDoc  =
    (\ _lhsIfocusD
       _lhsIlayoutMap
       _lhsIpIdC ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOself :: EnrichedDoc
              _lhsOlayoutMap :: LayoutMap
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 49, column 25)
              _lhsOpres =
                  presHole _lhsIfocusD "EnrichedDoc" (HoleEnrichedDocNode _self []) []
              -- self rule
              _self =
                  HoleEnrichedDoc
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOlayoutMap =
                  _lhsIlayoutMap
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOlayoutMap,_lhsOpIdC,_lhsOpres,_lhsOself)))
sem_EnrichedDoc_ParseErrEnrichedDoc :: Node ->
                                       Presentation_Doc_Node_Clip ->
                                       T_EnrichedDoc
sem_EnrichedDoc_ParseErrEnrichedDoc node_ presentation_ =
    (\ _lhsIfocusD
       _lhsIlayoutMap
       _lhsIpIdC ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOself :: EnrichedDoc
              _lhsOlayoutMap :: LayoutMap
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 50, column 25)
              _lhsOpres =
                  presParseErr node_ presentation_
              -- self rule
              _self =
                  ParseErrEnrichedDoc node_ presentation_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOlayoutMap =
                  _lhsIlayoutMap
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOlayoutMap,_lhsOpIdC,_lhsOpres,_lhsOself)))
sem_EnrichedDoc_RootEnr :: IDD ->
                           T_Root ->
                           Document ->
                           T_EnrichedDoc
sem_EnrichedDoc_RootEnr id_ root_ document_ =
    (\ _lhsIfocusD
       _lhsIlayoutMap
       _lhsIpIdC ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip
              _rootOpath :: ([Int])
              _rootOix :: Int
              _lhsOself :: EnrichedDoc
              _lhsOlayoutMap :: LayoutMap
              _lhsOpIdC :: Int
              _rootOfocusD :: FocusDoc
              _rootOpIdC :: Int
              _rootIpIdC :: Int
              _rootIpres :: Presentation_Doc_Node_Clip
              _rootIpresTree :: Presentation_Doc_Node_Clip
              _rootIpresXML :: Presentation_Doc_Node_Clip
              _rootIself :: Root
              -- "src/PresentationAG.ag"(line 43, column 7)
              _lhsOpres =
                  loc (RootDocNode document_ []) $
                  loc (RootEnrNode _self []) $ structural $
                    _rootIpres
              -- "src/PresentationAG_Generated.ag"(line 3, column 13)
              _rootOpath =
                  []
              -- "src/PresentationAG_Generated.ag"(line 4, column 13)
              _rootOix =
                  0
              -- self rule
              _self =
                  RootEnr id_ _rootIself document_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOlayoutMap =
                  _lhsIlayoutMap
              -- copy rule (up)
              _lhsOpIdC =
                  _rootIpIdC
              -- copy rule (down)
              _rootOfocusD =
                  _lhsIfocusD
              -- copy rule (down)
              _rootOpIdC =
                  _lhsIpIdC
              ( _rootIpIdC,_rootIpres,_rootIpresTree,_rootIpresXML,_rootIself) =
                  (root_ _rootOfocusD _rootOix _rootOpIdC _rootOpath)
          in  ( _lhsOlayoutMap,_lhsOpIdC,_lhsOpres,_lhsOself)))
-- Int_ --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         focusD               : FocusDoc
         ix                   : Int
         path                 : [Int]
      chained attribute:
         pIdC                 : Int
      synthesized attributes:
         int                  : Int
         pres                 : Presentation_Doc_Node_Clip
         presTree             : Presentation_Doc_Node_Clip
         presXML              : Presentation_Doc_Node_Clip
         self                 : SELF
   alternatives:
      alternative HoleInt_:
         local self           : _
      alternative Int_:
         child idd            : {IDD}
         child int            : {Int}
         local self           : _
      alternative ParseErrInt_:
         child node           : {Node}
         child presentation   : {Presentation_Doc_Node_Clip}
         local self           : _
-}
-- cata
sem_Int_ :: Int_ ->
            T_Int_
sem_Int_ (HoleInt_ ) =
    (sem_Int__HoleInt_ )
sem_Int_ (Int_ _idd _int) =
    (sem_Int__Int_ _idd _int)
sem_Int_ (ParseErrInt_ _node _presentation) =
    (sem_Int__ParseErrInt_ _node _presentation)
-- semantic domain
type T_Int_ = FocusDoc ->
              Int ->
              Int ->
              ([Int]) ->
              ( Int,Int,Presentation_Doc_Node_Clip,Presentation_Doc_Node_Clip,Presentation_Doc_Node_Clip,Int_)
data Inh_Int_ = Inh_Int_ {focusD_Inh_Int_ :: FocusDoc,ix_Inh_Int_ :: Int,pIdC_Inh_Int_ :: Int,path_Inh_Int_ :: [Int]}
data Syn_Int_ = Syn_Int_ {int_Syn_Int_ :: Int,pIdC_Syn_Int_ :: Int,pres_Syn_Int_ :: Presentation_Doc_Node_Clip,presTree_Syn_Int_ :: Presentation_Doc_Node_Clip,presXML_Syn_Int_ :: Presentation_Doc_Node_Clip,self_Syn_Int_ :: Int_}
wrap_Int_ sem (Inh_Int_ _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath) =
    (let ( _lhsOint,_lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself) =
             (sem _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath)
     in  (Syn_Int_ _lhsOint _lhsOpIdC _lhsOpres _lhsOpresTree _lhsOpresXML _lhsOself))
sem_Int__HoleInt_ :: T_Int_
sem_Int__HoleInt_  =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOint :: Int
              _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: Int_
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 74, column 13)
              _lhsOint =
                  0
              -- "src/PresentationAG_Generated.ag"(line 153, column 18)
              _lhsOpres =
                  presHole _lhsIfocusD "Int_" (HoleInt_Node _self _lhsIpath) _lhsIpath
              -- "src/PresentationAG_Generated.ag"(line 263, column 18)
              _lhsOpresXML =
                  presHole _lhsIfocusD "Int_" (HoleInt_Node _self _lhsIpath) _lhsIpath
              -- "src/PresentationAG_Generated.ag"(line 321, column 18)
              _lhsOpresTree =
                  presHole _lhsIfocusD "Int_" (HoleInt_Node _self _lhsIpath) _lhsIpath
              -- self rule
              _self =
                  HoleInt_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOint,_lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself)))
sem_Int__Int_ :: IDD ->
                 Int ->
                 T_Int_
sem_Int__Int_ idd_ int_ =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOint :: Int
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: Int_
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 68, column 7)
              _lhsOpres =
                  loc (Int_Node _self _lhsIpath) $ parsing $ presentFocus _lhsIfocusD _lhsIpath $
                    row [text $ show int_, text ""]
              -- "src/PresentationAG_Generated.ag"(line 72, column 13)
              _lhsOint =
                  int_
              -- "src/PresentationAG_Generated.ag"(line 262, column 7)
              _lhsOpresXML =
                  presentElementXML _lhsIfocusD (Int_Node _self _lhsIpath) _lhsIpath "Int_" [ presentPrimXMLInt int_ ]
              -- "src/PresentationAG_Generated.ag"(line 320, column 7)
              _lhsOpresTree =
                  presentElementTree _lhsIfocusD (Int_Node _self _lhsIpath) _lhsIpath "Int_" [ presentPrimTreeInt int_ ]
              -- self rule
              _self =
                  Int_ idd_ int_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOint,_lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself)))
sem_Int__ParseErrInt_ :: Node ->
                         Presentation_Doc_Node_Clip ->
                         T_Int_
sem_Int__ParseErrInt_ node_ presentation_ =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOint :: Int
              _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: Int_
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 74, column 13)
              _lhsOint =
                  0
              -- "src/PresentationAG_Generated.ag"(line 154, column 18)
              _lhsOpres =
                  presParseErr node_ presentation_
              -- "src/PresentationAG_Generated.ag"(line 264, column 18)
              _lhsOpresXML =
                  presParseErr node_ presentation_
              -- "src/PresentationAG_Generated.ag"(line 322, column 18)
              _lhsOpresTree =
                  presParseErr node_ presentation_
              -- self rule
              _self =
                  ParseErrInt_ node_ presentation_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOint,_lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself)))
-- List_Dummy --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         focusD               : FocusDoc
         path                 : [Int]
      chained attribute:
         pIdC                 : Int
      synthesized attributes:
         presTree             : Presentation_Doc_Node_Clip
         presXML              : Presentation_Doc_Node_Clip
         press                : [Presentation_Doc_Node_Clip]
         self                 : SELF
   alternatives:
      alternative HoleList_Dummy:
         local self           : _
      alternative List_Dummy:
         child idd            : {IDD}
         child elts           : ConsList_Dummy
         local self           : _
      alternative ParseErrList_Dummy:
         child node           : {Node}
         child presentation   : {Presentation_Doc_Node_Clip}
         local self           : _
-}
-- cata
sem_List_Dummy :: List_Dummy ->
                  T_List_Dummy
sem_List_Dummy (HoleList_Dummy ) =
    (sem_List_Dummy_HoleList_Dummy )
sem_List_Dummy (List_Dummy _idd _elts) =
    (sem_List_Dummy_List_Dummy _idd (sem_ConsList_Dummy _elts))
sem_List_Dummy (ParseErrList_Dummy _node _presentation) =
    (sem_List_Dummy_ParseErrList_Dummy _node _presentation)
-- semantic domain
type T_List_Dummy = FocusDoc ->
                    Int ->
                    ([Int]) ->
                    ( Int,Presentation_Doc_Node_Clip,Presentation_Doc_Node_Clip,([Presentation_Doc_Node_Clip]),List_Dummy)
data Inh_List_Dummy = Inh_List_Dummy {focusD_Inh_List_Dummy :: FocusDoc,pIdC_Inh_List_Dummy :: Int,path_Inh_List_Dummy :: [Int]}
data Syn_List_Dummy = Syn_List_Dummy {pIdC_Syn_List_Dummy :: Int,presTree_Syn_List_Dummy :: Presentation_Doc_Node_Clip,presXML_Syn_List_Dummy :: Presentation_Doc_Node_Clip,press_Syn_List_Dummy :: [Presentation_Doc_Node_Clip],self_Syn_List_Dummy :: List_Dummy}
wrap_List_Dummy sem (Inh_List_Dummy _lhsIfocusD _lhsIpIdC _lhsIpath) =
    (let ( _lhsOpIdC,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself) =
             (sem _lhsIfocusD _lhsIpIdC _lhsIpath)
     in  (Syn_List_Dummy _lhsOpIdC _lhsOpresTree _lhsOpresXML _lhsOpress _lhsOself))
sem_List_Dummy_HoleList_Dummy :: T_List_Dummy
sem_List_Dummy_HoleList_Dummy  =
    (\ _lhsIfocusD
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOpress :: ([Presentation_Doc_Node_Clip])
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: List_Dummy
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 207, column 24)
              _lhsOpress =
                  []
              -- "src/PresentationAG_Generated.ag"(line 294, column 7)
              _lhsOpresXML =
                  loc (List_DummyNode _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  presHole _lhsIfocusD "List_Dummy" (HoleList_DummyNode _self _lhsIpath) _lhsIpath
              -- "src/PresentationAG_Generated.ag"(line 352, column 7)
              _lhsOpresTree =
                  loc (List_DummyNode _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    presHole _lhsIfocusD "List_Dummy" (HoleList_DummyNode _self _lhsIpath) _lhsIpath
              -- self rule
              _self =
                  HoleList_Dummy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOpIdC,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself)))
sem_List_Dummy_List_Dummy :: IDD ->
                             T_ConsList_Dummy ->
                             T_List_Dummy
sem_List_Dummy_List_Dummy idd_ elts_ =
    (\ _lhsIfocusD
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOpress :: ([Presentation_Doc_Node_Clip])
              _eltsOpIdC :: Int
              _lhsOpIdC :: Int
              _eltsOpath :: ([Int])
              _eltsOix :: Int
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: List_Dummy
              _eltsOfocusD :: FocusDoc
              _eltsIpIdC :: Int
              _eltsIpress :: ([Presentation_Doc_Node_Clip])
              _eltsIpressTree :: ([Presentation_Doc_Node_Clip])
              _eltsIpressXML :: ([Presentation_Doc_Node_Clip])
              _eltsIself :: ConsList_Dummy
              -- "src/PresentationAG_Generated.ag"(line 199, column 7)
              _lhsOpress =
                  map ( loc (List_DummyNode _self _lhsIpath)
                      . presentFocus _lhsIfocusD _lhsIpath )
                      _eltsIpress
              -- "src/PresentationAG_Generated.ag"(line 203, column 7)
              _eltsOpIdC =
                  _lhsIpIdC + 100
              -- "src/PresentationAG_Generated.ag"(line 204, column 7)
              _lhsOpIdC =
                  _eltsIpIdC
              -- "src/PresentationAG_Generated.ag"(line 205, column 7)
              _eltsOpath =
                  _lhsIpath
              -- "src/PresentationAG_Generated.ag"(line 206, column 7)
              _eltsOix =
                  0
              -- "src/PresentationAG_Generated.ag"(line 288, column 7)
              _lhsOpresXML =
                  loc (List_DummyNode _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  col _eltsIpressXML
              -- "src/PresentationAG_Generated.ag"(line 346, column 7)
              _lhsOpresTree =
                  loc (List_DummyNode _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    col _eltsIpressTree
              -- self rule
              _self =
                  List_Dummy idd_ _eltsIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _eltsOfocusD =
                  _lhsIfocusD
              ( _eltsIpIdC,_eltsIpress,_eltsIpressTree,_eltsIpressXML,_eltsIself) =
                  (elts_ _eltsOfocusD _eltsOix _eltsOpIdC _eltsOpath)
          in  ( _lhsOpIdC,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself)))
sem_List_Dummy_ParseErrList_Dummy :: Node ->
                                     Presentation_Doc_Node_Clip ->
                                     T_List_Dummy
sem_List_Dummy_ParseErrList_Dummy node_ presentation_ =
    (\ _lhsIfocusD
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOpress :: ([Presentation_Doc_Node_Clip])
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: List_Dummy
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 208, column 24)
              _lhsOpress =
                  [ presParseErr node_ presentation_ ]
              -- "src/PresentationAG_Generated.ag"(line 291, column 7)
              _lhsOpresXML =
                  loc (List_DummyNode _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                  presParseErr node_ presentation_
              -- "src/PresentationAG_Generated.ag"(line 349, column 7)
              _lhsOpresTree =
                  loc (List_DummyNode _self _lhsIpath) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                    presParseErr node_ presentation_
              -- self rule
              _self =
                  ParseErrList_Dummy node_ presentation_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOpIdC,_lhsOpresTree,_lhsOpresXML,_lhsOpress,_lhsOself)))
-- Root --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         focusD               : FocusDoc
         ix                   : Int
         path                 : [Int]
      chained attribute:
         pIdC                 : Int
      synthesized attributes:
         pres                 : Presentation_Doc_Node_Clip
         presTree             : Presentation_Doc_Node_Clip
         presXML              : Presentation_Doc_Node_Clip
         self                 : SELF
   alternatives:
      alternative HoleRoot:
         local self           : _
      alternative ParseErrRoot:
         child node           : {Node}
         child presentation   : {Presentation_Doc_Node_Clip}
         local self           : _
      alternative Root:
         child idd            : {IDD}
         child tree           : Tree
         local self           : _
-}
-- cata
sem_Root :: Root ->
            T_Root
sem_Root (HoleRoot ) =
    (sem_Root_HoleRoot )
sem_Root (ParseErrRoot _node _presentation) =
    (sem_Root_ParseErrRoot _node _presentation)
sem_Root (Root _idd _tree) =
    (sem_Root_Root _idd (sem_Tree _tree))
-- semantic domain
type T_Root = FocusDoc ->
              Int ->
              Int ->
              ([Int]) ->
              ( Int,Presentation_Doc_Node_Clip,Presentation_Doc_Node_Clip,Presentation_Doc_Node_Clip,Root)
data Inh_Root = Inh_Root {focusD_Inh_Root :: FocusDoc,ix_Inh_Root :: Int,pIdC_Inh_Root :: Int,path_Inh_Root :: [Int]}
data Syn_Root = Syn_Root {pIdC_Syn_Root :: Int,pres_Syn_Root :: Presentation_Doc_Node_Clip,presTree_Syn_Root :: Presentation_Doc_Node_Clip,presXML_Syn_Root :: Presentation_Doc_Node_Clip,self_Syn_Root :: Root}
wrap_Root sem (Inh_Root _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath) =
    (let ( _lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself) =
             (sem _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath)
     in  (Syn_Root _lhsOpIdC _lhsOpres _lhsOpresTree _lhsOpresXML _lhsOself))
sem_Root_HoleRoot :: T_Root
sem_Root_HoleRoot  =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: Root
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 177, column 18)
              _lhsOpres =
                  presHole _lhsIfocusD "Root" (HoleRootNode _self _lhsIpath) _lhsIpath
              -- "src/PresentationAG_Generated.ag"(line 275, column 18)
              _lhsOpresXML =
                  presHole _lhsIfocusD "Root" (HoleRootNode _self _lhsIpath) _lhsIpath
              -- "src/PresentationAG_Generated.ag"(line 333, column 18)
              _lhsOpresTree =
                  presHole _lhsIfocusD "Root" (HoleRootNode _self _lhsIpath) _lhsIpath
              -- self rule
              _self =
                  HoleRoot
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself)))
sem_Root_ParseErrRoot :: Node ->
                         Presentation_Doc_Node_Clip ->
                         T_Root
sem_Root_ParseErrRoot node_ presentation_ =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: Root
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 178, column 18)
              _lhsOpres =
                  presParseErr node_ presentation_
              -- "src/PresentationAG_Generated.ag"(line 276, column 18)
              _lhsOpresXML =
                  presParseErr node_ presentation_
              -- "src/PresentationAG_Generated.ag"(line 334, column 18)
              _lhsOpresTree =
                  presParseErr node_ presentation_
              -- self rule
              _self =
                  ParseErrRoot node_ presentation_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself)))
sem_Root_Root :: IDD ->
                 T_Tree ->
                 T_Root
sem_Root_Root idd_ tree_ =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip
              _treeOpIdC :: Int
              _lhsOpIdC :: Int
              _treeOpath :: ([Int])
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: Root
              _treeOfocusD :: FocusDoc
              _treeOix :: Int
              _treeIpIdC :: Int
              _treeIpres :: Presentation_Doc_Node_Clip
              _treeIpresTree :: Presentation_Doc_Node_Clip
              _treeIpresXML :: Presentation_Doc_Node_Clip
              _treeIself :: Tree
              -- "src/PresentationAG.ag"(line 52, column 7)
              _lhsOpres =
                  loc (RootNode _self []) $ structural $ presentFocus _lhsIfocusD _lhsIpath $
                      col [ text "Root", _treeIpres ]
              -- "src/PresentationAG_Generated.ag"(line 175, column 10)
              _treeOpIdC =
                  _lhsIpIdC + 0
              -- "src/PresentationAG_Generated.ag"(line 176, column 10)
              _lhsOpIdC =
                  _treeIpIdC
              -- "src/PresentationAG_Generated.ag"(line 182, column 10)
              _treeOpath =
                  _lhsIpath++[0]
              -- "src/PresentationAG_Generated.ag"(line 274, column 7)
              _lhsOpresXML =
                  presentElementXML _lhsIfocusD (RootNode _self _lhsIpath) _lhsIpath "Root" [ _treeIpresXML ]
              -- "src/PresentationAG_Generated.ag"(line 332, column 7)
              _lhsOpresTree =
                  presentElementTree _lhsIfocusD (RootNode _self _lhsIpath) _lhsIpath "Root" [ _treeIpresTree ]
              -- self rule
              _self =
                  Root idd_ _treeIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _treeOfocusD =
                  _lhsIfocusD
              -- copy rule (down)
              _treeOix =
                  _lhsIix
              ( _treeIpIdC,_treeIpres,_treeIpresTree,_treeIpresXML,_treeIself) =
                  (tree_ _treeOfocusD _treeOix _treeOpIdC _treeOpath)
          in  ( _lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself)))
-- String_ -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         focusD               : FocusDoc
         ix                   : Int
         path                 : [Int]
      chained attribute:
         pIdC                 : Int
      synthesized attributes:
         length               : Int
         pres                 : Presentation_Doc_Node_Clip
         presTree             : Presentation_Doc_Node_Clip
         presXML              : Presentation_Doc_Node_Clip
         self                 : SELF
         str                  : String
   alternatives:
      alternative HoleString_:
         local self           : _
      alternative ParseErrString_:
         child node           : {Node}
         child presentation   : {Presentation_Doc_Node_Clip}
         local self           : _
      alternative String_:
         child idd            : {IDD}
         child string         : {String}
         local self           : _
-}
-- cata
sem_String_ :: String_ ->
               T_String_
sem_String_ (HoleString_ ) =
    (sem_String__HoleString_ )
sem_String_ (ParseErrString_ _node _presentation) =
    (sem_String__ParseErrString_ _node _presentation)
sem_String_ (String_ _idd _string) =
    (sem_String__String_ _idd _string)
-- semantic domain
type T_String_ = FocusDoc ->
                 Int ->
                 Int ->
                 ([Int]) ->
                 ( Int,Int,Presentation_Doc_Node_Clip,Presentation_Doc_Node_Clip,Presentation_Doc_Node_Clip,String_,String)
data Inh_String_ = Inh_String_ {focusD_Inh_String_ :: FocusDoc,ix_Inh_String_ :: Int,pIdC_Inh_String_ :: Int,path_Inh_String_ :: [Int]}
data Syn_String_ = Syn_String_ {length_Syn_String_ :: Int,pIdC_Syn_String_ :: Int,pres_Syn_String_ :: Presentation_Doc_Node_Clip,presTree_Syn_String_ :: Presentation_Doc_Node_Clip,presXML_Syn_String_ :: Presentation_Doc_Node_Clip,self_Syn_String_ :: String_,str_Syn_String_ :: String}
wrap_String_ sem (Inh_String_ _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath) =
    (let ( _lhsOlength,_lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOstr) =
             (sem _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath)
     in  (Syn_String_ _lhsOlength _lhsOpIdC _lhsOpres _lhsOpresTree _lhsOpresXML _lhsOself _lhsOstr))
sem_String__HoleString_ :: T_String_
sem_String__HoleString_  =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOlength :: Int
              _lhsOstr :: String
              _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: String_
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 63, column 13)
              _lhsOlength =
                  0
              -- "src/PresentationAG_Generated.ag"(line 64, column 13)
              _lhsOstr =
                  ""
              -- "src/PresentationAG_Generated.ag"(line 143, column 21)
              _lhsOpres =
                  presHole _lhsIfocusD "String_" (HoleString_Node _self _lhsIpath) _lhsIpath
              -- "src/PresentationAG_Generated.ag"(line 251, column 21)
              _lhsOpresXML =
                  presHole _lhsIfocusD "String_" (HoleString_Node _self _lhsIpath) _lhsIpath
              -- "src/PresentationAG_Generated.ag"(line 309, column 21)
              _lhsOpresTree =
                  presHole _lhsIfocusD "String_" (HoleString_Node _self _lhsIpath) _lhsIpath
              -- self rule
              _self =
                  HoleString_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOlength,_lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOstr)))
sem_String__ParseErrString_ :: Node ->
                               Presentation_Doc_Node_Clip ->
                               T_String_
sem_String__ParseErrString_ node_ presentation_ =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOlength :: Int
              _lhsOstr :: String
              _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: String_
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 63, column 13)
              _lhsOlength =
                  0
              -- "src/PresentationAG_Generated.ag"(line 64, column 13)
              _lhsOstr =
                  ""
              -- "src/PresentationAG_Generated.ag"(line 144, column 21)
              _lhsOpres =
                  presParseErr node_ presentation_
              -- "src/PresentationAG_Generated.ag"(line 252, column 21)
              _lhsOpresXML =
                  presParseErr node_ presentation_
              -- "src/PresentationAG_Generated.ag"(line 310, column 21)
              _lhsOpresTree =
                  presParseErr node_ presentation_
              -- self rule
              _self =
                  ParseErrString_ node_ presentation_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOlength,_lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOstr)))
sem_String__String_ :: IDD ->
                       String ->
                       T_String_
sem_String__String_ idd_ string_ =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOlength :: Int
              _lhsOstr :: String
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: String_
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 56, column 7)
              _lhsOpres =
                  loc (String_Node _self _lhsIpath) $ parsing $ presentFocus _lhsIfocusD _lhsIpath $
                    row [text string_, text ""]
              -- "src/PresentationAG_Generated.ag"(line 60, column 13)
              _lhsOlength =
                  length string_
              -- "src/PresentationAG_Generated.ag"(line 61, column 13)
              _lhsOstr =
                  string_
              -- "src/PresentationAG_Generated.ag"(line 250, column 7)
              _lhsOpresXML =
                  presentElementXML _lhsIfocusD (String_Node _self _lhsIpath) _lhsIpath "String_" [ presentPrimXMLString string_ ]
              -- "src/PresentationAG_Generated.ag"(line 308, column 7)
              _lhsOpresTree =
                  presentElementTree _lhsIfocusD (String_Node _self _lhsIpath) _lhsIpath "String_" [ presentPrimTreeString string_ ]
              -- self rule
              _self =
                  String_ idd_ string_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOlength,_lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself,_lhsOstr)))
-- Tree --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         focusD               : FocusDoc
         ix                   : Int
         path                 : [Int]
      chained attribute:
         pIdC                 : Int
      synthesized attributes:
         pres                 : Presentation_Doc_Node_Clip
         presTree             : Presentation_Doc_Node_Clip
         presXML              : Presentation_Doc_Node_Clip
         self                 : SELF
   alternatives:
      alternative Bin:
         child idd            : {IDD}
         child left           : Tree
         child right          : Tree
         local self           : _
      alternative HoleTree:
         local self           : _
      alternative Leaf:
         child idd            : {IDD}
         local self           : _
      alternative ParseErrTree:
         child node           : {Node}
         child presentation   : {Presentation_Doc_Node_Clip}
         local self           : _
-}
-- cata
sem_Tree :: Tree ->
            T_Tree
sem_Tree (Bin _idd _left _right) =
    (sem_Tree_Bin _idd (sem_Tree _left) (sem_Tree _right))
sem_Tree (HoleTree ) =
    (sem_Tree_HoleTree )
sem_Tree (Leaf _idd) =
    (sem_Tree_Leaf _idd)
sem_Tree (ParseErrTree _node _presentation) =
    (sem_Tree_ParseErrTree _node _presentation)
-- semantic domain
type T_Tree = FocusDoc ->
              Int ->
              Int ->
              ([Int]) ->
              ( Int,Presentation_Doc_Node_Clip,Presentation_Doc_Node_Clip,Presentation_Doc_Node_Clip,Tree)
data Inh_Tree = Inh_Tree {focusD_Inh_Tree :: FocusDoc,ix_Inh_Tree :: Int,pIdC_Inh_Tree :: Int,path_Inh_Tree :: [Int]}
data Syn_Tree = Syn_Tree {pIdC_Syn_Tree :: Int,pres_Syn_Tree :: Presentation_Doc_Node_Clip,presTree_Syn_Tree :: Presentation_Doc_Node_Clip,presXML_Syn_Tree :: Presentation_Doc_Node_Clip,self_Syn_Tree :: Tree}
wrap_Tree sem (Inh_Tree _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath) =
    (let ( _lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself) =
             (sem _lhsIfocusD _lhsIix _lhsIpIdC _lhsIpath)
     in  (Syn_Tree _lhsOpIdC _lhsOpres _lhsOpresTree _lhsOpresXML _lhsOself))
sem_Tree_Bin :: IDD ->
                T_Tree ->
                T_Tree ->
                T_Tree
sem_Tree_Bin idd_ left_ right_ =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip
              _leftOpIdC :: Int
              _rightOpIdC :: Int
              _lhsOpIdC :: Int
              _leftOpath :: ([Int])
              _rightOpath :: ([Int])
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: Tree
              _leftOfocusD :: FocusDoc
              _leftOix :: Int
              _rightOfocusD :: FocusDoc
              _rightOix :: Int
              _leftIpIdC :: Int
              _leftIpres :: Presentation_Doc_Node_Clip
              _leftIpresTree :: Presentation_Doc_Node_Clip
              _leftIpresXML :: Presentation_Doc_Node_Clip
              _leftIself :: Tree
              _rightIpIdC :: Int
              _rightIpres :: Presentation_Doc_Node_Clip
              _rightIpresTree :: Presentation_Doc_Node_Clip
              _rightIpresXML :: Presentation_Doc_Node_Clip
              _rightIself :: Tree
              -- "src/PresentationAG.ag"(line 57, column 7)
              _lhsOpres =
                  loc (BinNode _self _lhsIpath) $ parsing $ presentFocus _lhsIfocusD _lhsIpath $
                      row [ text $ "(", _leftIpres, text " Bin ", _rightIpres, text ")" ]
              -- "src/PresentationAG_Generated.ag"(line 186, column 9)
              _leftOpIdC =
                  _lhsIpIdC + 0
              -- "src/PresentationAG_Generated.ag"(line 187, column 9)
              _rightOpIdC =
                  _leftIpIdC
              -- "src/PresentationAG_Generated.ag"(line 188, column 9)
              _lhsOpIdC =
                  _rightIpIdC
              -- "src/PresentationAG_Generated.ag"(line 194, column 9)
              _leftOpath =
                  _lhsIpath++[0]
              -- "src/PresentationAG_Generated.ag"(line 195, column 9)
              _rightOpath =
                  _lhsIpath++[1]
              -- "src/PresentationAG_Generated.ag"(line 280, column 7)
              _lhsOpresXML =
                  presentElementXML _lhsIfocusD (BinNode _self _lhsIpath) _lhsIpath "Bin" [ _leftIpresXML, _rightIpresXML ]
              -- "src/PresentationAG_Generated.ag"(line 338, column 7)
              _lhsOpresTree =
                  presentElementTree _lhsIfocusD (BinNode _self _lhsIpath) _lhsIpath "Bin" [ _leftIpresTree, _rightIpresTree ]
              -- self rule
              _self =
                  Bin idd_ _leftIself _rightIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _leftOfocusD =
                  _lhsIfocusD
              -- copy rule (down)
              _leftOix =
                  _lhsIix
              -- copy rule (down)
              _rightOfocusD =
                  _lhsIfocusD
              -- copy rule (down)
              _rightOix =
                  _lhsIix
              ( _leftIpIdC,_leftIpres,_leftIpresTree,_leftIpresXML,_leftIself) =
                  (left_ _leftOfocusD _leftOix _leftOpIdC _leftOpath)
              ( _rightIpIdC,_rightIpres,_rightIpresTree,_rightIpresXML,_rightIself) =
                  (right_ _rightOfocusD _rightOix _rightOpIdC _rightOpath)
          in  ( _lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself)))
sem_Tree_HoleTree :: T_Tree
sem_Tree_HoleTree  =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: Tree
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 189, column 18)
              _lhsOpres =
                  presHole _lhsIfocusD "Tree" (HoleTreeNode _self _lhsIpath) _lhsIpath
              -- "src/PresentationAG_Generated.ag"(line 283, column 18)
              _lhsOpresXML =
                  presHole _lhsIfocusD "Tree" (HoleTreeNode _self _lhsIpath) _lhsIpath
              -- "src/PresentationAG_Generated.ag"(line 341, column 18)
              _lhsOpresTree =
                  presHole _lhsIfocusD "Tree" (HoleTreeNode _self _lhsIpath) _lhsIpath
              -- self rule
              _self =
                  HoleTree
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself)))
sem_Tree_Leaf :: IDD ->
                 T_Tree
sem_Tree_Leaf idd_ =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: Tree
              _lhsOpIdC :: Int
              -- "src/PresentationAG.ag"(line 60, column 7)
              _lhsOpres =
                  loc (LeafNode _self _lhsIpath) $ parsing $ presentFocus _lhsIfocusD _lhsIpath $
                      row [text $ "Leaf"]
              -- "src/PresentationAG_Generated.ag"(line 282, column 7)
              _lhsOpresXML =
                  presentElementXML _lhsIfocusD (LeafNode _self _lhsIpath) _lhsIpath "Leaf" [  ]
              -- "src/PresentationAG_Generated.ag"(line 340, column 7)
              _lhsOpresTree =
                  presentElementTree _lhsIfocusD (LeafNode _self _lhsIpath) _lhsIpath "Leaf" [  ]
              -- self rule
              _self =
                  Leaf idd_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself)))
sem_Tree_ParseErrTree :: Node ->
                         Presentation_Doc_Node_Clip ->
                         T_Tree
sem_Tree_ParseErrTree node_ presentation_ =
    (\ _lhsIfocusD
       _lhsIix
       _lhsIpIdC
       _lhsIpath ->
         (let _lhsOpres :: Presentation_Doc_Node_Clip
              _lhsOpresXML :: Presentation_Doc_Node_Clip
              _lhsOpresTree :: Presentation_Doc_Node_Clip
              _lhsOself :: Tree
              _lhsOpIdC :: Int
              -- "src/PresentationAG_Generated.ag"(line 190, column 18)
              _lhsOpres =
                  presParseErr node_ presentation_
              -- "src/PresentationAG_Generated.ag"(line 284, column 18)
              _lhsOpresXML =
                  presParseErr node_ presentation_
              -- "src/PresentationAG_Generated.ag"(line 342, column 18)
              _lhsOpresTree =
                  presParseErr node_ presentation_
              -- self rule
              _self =
                  ParseErrTree node_ presentation_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOpIdC =
                  _lhsIpIdC
          in  ( _lhsOpIdC,_lhsOpres,_lhsOpresTree,_lhsOpresXML,_lhsOself)))
