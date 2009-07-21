{-# OPTIONS_GHC -fbang-patterns #-}

-- UUAGC 0.9.10 (InterfacesRules.lag)
module InterfacesRules where

import Interfaces
import SequentialTypes
import CodeSyntax
import GrammarInfo

import qualified Data.Sequence as Seq
import Data.Sequence(Seq)
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Tree(Tree(Node))
import Data.Graph(Graph, dfs, edges, buildG, transposeG)
import Data.Maybe (fromJust)
import Data.List (partition,transpose,(\\),nub,intersect, findIndex)
import Data.Array ((!),inRange,bounds,assocs)
import Data.Foldable(toList)

import Debug.Trace(trace)


import CommonTypes
import SequentialTypes

type VisitSS = [Vertex]


gather :: Info -> [Vertex] -> [[Vertex]]
gather info =  eqClasses comp
               where comp a b = isEqualField (ruleTable info ! a) (ruleTable info ! b)


-- Only non-empty syn will ever be forced, because visits with empty syn are never performed
-- Right hand side synthesized attributes always have a field
cv :: (Vertex -> CRule) -> Int -> Vertex -> ([Vertex],[Vertex]) -> (Vertex,ChildVisit)
cv look n v (inh,syn) =  let  fld = getField (look (head syn))
                              rnt = fromJust (getRhsNt (look (head syn)))
                              d = ChildVisit fld rnt n inh syn
                         in (v,d)


ed v (inh,syn) = map (\i -> (i,v)) inh ++ map (\s -> (v,s)) syn


postorder (Node a ts) = postorderF ts ++ [a]
postorderF = concatMap postorder
postOrd g = postorderF . dfs g
topSort' g = postOrd g


type IntraVisit = [Vertex]


swap (a,b) = (b,a)


ccv :: Identifier -> NontermIdent -> Int -> CInterfaceMap -> CRule
ccv name nt n table
  =  CChildVisit name nt n inh syn last
     where  CInterface segs = Map.findWithDefault (error ("InterfacesRules::ccv::interfaces not in table for nt: " ++ show nt)) nt table
            (seg:remain) = drop n segs
            CSegment inh syn = seg           
            last = null remain
-- IRoot -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dpr                  : [Edge]
         info                 : Info
         tdp                  : Graph
      synthesized attributes:
         edp                  : [Edge]
         inters               : CInterfaceMap
         visits               : CVisitsMap
   alternatives:
      alternative IRoot:
         child inters         : Interfaces 
         visit 0:
            local newedges    : _
            local visitssGraph : _
            local descr       : _
-}
-- cata
sem_IRoot :: IRoot  ->
             T_IRoot 
sem_IRoot !(IRoot _inters )  =
    (sem_IRoot_IRoot (sem_Interfaces _inters ) )
-- semantic domain
newtype T_IRoot  = T_IRoot (([Edge]) ->
                            Info ->
                            Graph ->
                            ( ([Edge]),CInterfaceMap,CVisitsMap))
data Inh_IRoot  = Inh_IRoot {dpr_Inh_IRoot :: !([Edge]),info_Inh_IRoot :: !(Info),tdp_Inh_IRoot :: !(Graph)}
data Syn_IRoot  = Syn_IRoot {edp_Syn_IRoot :: !([Edge]),inters_Syn_IRoot :: !(CInterfaceMap),visits_Syn_IRoot :: !(CVisitsMap)}
wrap_IRoot :: T_IRoot  ->
              Inh_IRoot  ->
              Syn_IRoot 
wrap_IRoot !(T_IRoot sem ) !(Inh_IRoot _lhsIdpr _lhsIinfo _lhsItdp )  =
    (let ( !_lhsOedp,!_lhsOinters,!_lhsOvisits) =
             (sem _lhsIdpr _lhsIinfo _lhsItdp )
     in  (Syn_IRoot _lhsOedp _lhsOinters _lhsOvisits ))
sem_IRoot_IRoot :: T_Interfaces  ->
                   T_IRoot 
sem_IRoot_IRoot !(T_Interfaces inters_ )  =
    (T_IRoot (\ (!_lhsIdpr)
                (!_lhsIinfo)
                (!_lhsItdp) ->
                  (case (_lhsIinfo) of
                   { !_intersOinfo ->
                   (case (snd (bounds _lhsItdp) + 1) of
                    { !_intersOv ->
                    (case ((inters_ _intersOinfo _intersOv )) of
                     { ( !_intersIdescr,!_intersIfirstvisitvertices,!_intersInewedges,!_intersIv,!T_Interfaces_1 inters_1) ->
                     (case (let terminals = [ v | (v,cr) <- assocs (ruleTable _lhsIinfo), not (getHasCode cr), isLocal cr ]
                            in _intersIfirstvisitvertices ++ terminals) of
                      { !_intersOprev ->
                      (case (toList _intersInewedges) of
                       { !_newedges ->
                       (case (let graph = buildG (0,_intersIv-1) es
                                  es = _newedges ++ edges _lhsItdp
                              in transposeG graph) of
                        { !_visitssGraph ->
                        (case (_visitssGraph) of
                         { !_intersOvssGraph ->
                         (case (toList _intersIdescr) of
                          { !_descr ->
                          (case (Map.fromList _descr) of
                           { !_intersOvisitDescr ->
                           (case ((inters_1 _intersOprev _intersOvisitDescr _intersOvssGraph )) of
                            { ( !_intersIinters,!T_Interfaces_2 inters_2) ->
                            (case (_intersIinters) of
                             { !_intersOallInters ->
                             (case (buildG (0,_intersIv-1) (map swap (_lhsIdpr ++ _newedges))) of
                              { !_intersOddp ->
                              (case ((inters_2 _intersOallInters _intersOddp )) of
                               { ( !_intersIedp,!_intersIvisits) ->
                               (case (toList _intersIedp) of
                                { !_lhsOedp ->
                                (case (_intersIinters) of
                                 { !_lhsOinters ->
                                 (case (_intersIvisits) of
                                  { !_lhsOvisits ->
                                  ( _lhsOedp,_lhsOinters,_lhsOvisits) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
-- Interface ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         info                 : Info
      chained attribute:
         v                    : Vertex
      synthesized attributes:
         descr                : Seq (Vertex,ChildVisit)
         firstvisitvertices   : [Vertex]
         newedges             : Seq Edge 
   visit 1:
      inherited attributes:
         prev                 : [Vertex]
         visitDescr           : Map Vertex ChildVisit
         vssGraph             : Graph
      synthesized attributes:
         inter                : CInterface
         nt                   : NontermIdent
   visit 2:
      inherited attributes:
         allInters            : CInterfaceMap
         ddp                  : Graph
      synthesized attributes:
         edp                  : Seq Edge
         visits               : Map ConstructorIdent CVisits
   alternatives:
      alternative Interface:
         child nt             : {NontermIdent}
         child cons           : {[ConstructorIdent]}
         child seg            : Segments 
         visit 0:
            local look        : {Vertex -> CRule}
            local v           : _
            local firstvisitvertices : _
            local descr       : _
            local newedges    : _
-}
-- cata
sem_Interface :: Interface  ->
                 T_Interface 
sem_Interface !(Interface _nt _cons _seg )  =
    (sem_Interface_Interface _nt _cons (sem_Segments _seg ) )
-- semantic domain
newtype T_Interface  = T_Interface (Info ->
                                    Vertex ->
                                    ( (Seq (Vertex,ChildVisit)),([Vertex]),(Seq Edge ),Vertex,T_Interface_1 ))
newtype T_Interface_1  = T_Interface_1 (([Vertex]) ->
                                        (Map Vertex ChildVisit) ->
                                        Graph ->
                                        ( CInterface,NontermIdent,T_Interface_2 ))
newtype T_Interface_2  = T_Interface_2 (CInterfaceMap ->
                                        Graph ->
                                        ( (Seq Edge),(Map ConstructorIdent CVisits)))
data Inh_Interface  = Inh_Interface {allInters_Inh_Interface :: !(CInterfaceMap),ddp_Inh_Interface :: !(Graph),info_Inh_Interface :: !(Info),prev_Inh_Interface :: !([Vertex]),v_Inh_Interface :: !(Vertex),visitDescr_Inh_Interface :: !(Map Vertex ChildVisit),vssGraph_Inh_Interface :: !(Graph)}
data Syn_Interface  = Syn_Interface {descr_Syn_Interface :: !(Seq (Vertex,ChildVisit)),edp_Syn_Interface :: !(Seq Edge),firstvisitvertices_Syn_Interface :: !([Vertex]),inter_Syn_Interface :: !(CInterface),newedges_Syn_Interface :: !(Seq Edge ),nt_Syn_Interface :: !(NontermIdent),v_Syn_Interface :: !(Vertex),visits_Syn_Interface :: !(Map ConstructorIdent CVisits)}
wrap_Interface :: T_Interface  ->
                  Inh_Interface  ->
                  Syn_Interface 
wrap_Interface !(T_Interface sem ) !(Inh_Interface _lhsIallInters _lhsIddp _lhsIinfo _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph )  =
    (let ( !_lhsOdescr,!_lhsOfirstvisitvertices,!_lhsOnewedges,!_lhsOv,!T_Interface_1 sem_1) =
             (sem _lhsIinfo _lhsIv )
         ( !_lhsOinter,!_lhsOnt,!T_Interface_2 sem_2) =
             (sem_1 _lhsIprev _lhsIvisitDescr _lhsIvssGraph )
         ( !_lhsOedp,!_lhsOvisits) =
             (sem_2 _lhsIallInters _lhsIddp )
     in  (Syn_Interface _lhsOdescr _lhsOedp _lhsOfirstvisitvertices _lhsOinter _lhsOnewedges _lhsOnt _lhsOv _lhsOvisits ))
sem_Interface_Interface :: NontermIdent ->
                           ([ConstructorIdent]) ->
                           T_Segments  ->
                           T_Interface 
sem_Interface_Interface !nt_ !cons_ !(T_Segments seg_ )  =
    (T_Interface (\ (!_lhsIinfo)
                    (!_lhsIv) ->
                      (case (_lhsIinfo) of
                       { !_segOinfo ->
                       (case (0) of
                        { !_segOn ->
                        (case (\a -> ruleTable _lhsIinfo ! a) of
                         { !_look ->
                         (case (_lhsIv) of
                          { !_segOv ->
                          (case ((seg_ _segOinfo _segOn _segOv )) of
                           { ( !_segIdescr,!_segIgroups,!_segInewedges,!_segInewvertices,!_segIv,!T_Segments_1 seg_1) ->
                           (case (_segIv + length _segInewvertices) of
                            { !_v ->
                            (case ([_segIv .. _v-1]) of
                             { !_firstvisitvertices ->
                             (case (zipWith (cv _look (-1)) _firstvisitvertices _segIgroups) of
                              { !_descr ->
                              (case (_segIdescr Seq.>< Seq.fromList _descr) of
                               { !_lhsOdescr ->
                               (case (_firstvisitvertices) of
                                { !_lhsOfirstvisitvertices ->
                                (case (zip _firstvisitvertices _segInewvertices) of
                                 { !_newedges ->
                                 (case (_segInewedges Seq.>< Seq.fromList _newedges) of
                                  { !_lhsOnewedges ->
                                  (case (_v) of
                                   { !_lhsOv ->
                                   (case ((sem_Interface_Interface_1 cons_ (T_Segments_1 seg_1 ) nt_ )) of
                                    { ( !sem_Interface_1) ->
                                    ( _lhsOdescr,_lhsOfirstvisitvertices,_lhsOnewedges,_lhsOv,sem_Interface_1) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Interface_Interface_1 :: ([ConstructorIdent]) ->
                             T_Segments_1  ->
                             NontermIdent ->
                             T_Interface_1 
sem_Interface_Interface_1 !cons_ !(T_Segments_1 seg_1 ) !nt_  =
    (T_Interface_1 (\ (!_lhsIprev)
                      (!_lhsIvisitDescr)
                      (!_lhsIvssGraph) ->
                        (case (_lhsIvssGraph) of
                         { !_segOvssGraph ->
                         (case (_lhsIvisitDescr) of
                          { !_segOvisitDescr ->
                          (case (_lhsIprev) of
                           { !_segOprev ->
                           (case (cons_) of
                            { !_segOcons ->
                            (case ((seg_1 _segOcons _segOprev _segOvisitDescr _segOvssGraph )) of
                             { ( !_segIsegs,!T_Segments_2 seg_2) ->
                             (case (CInterface _segIsegs) of
                              { !_lhsOinter ->
                              (case (nt_) of
                               { !_lhsOnt ->
                               (case ((sem_Interface_Interface_2 _lhsIprev (T_Segments_2 seg_2 ) cons_ )) of
                                { ( !sem_Interface_2) ->
                                ( _lhsOinter,_lhsOnt,sem_Interface_2) }) }) }) }) }) }) }) })) )
sem_Interface_Interface_2 :: ([Vertex]) ->
                             T_Segments_2  ->
                             ([ConstructorIdent]) ->
                             T_Interface_2 
sem_Interface_Interface_2 !_lhsIprev !(T_Segments_2 seg_2 ) !cons_  =
    (T_Interface_2 (\ (!_lhsIallInters)
                      (!_lhsIddp) ->
                        (case (_lhsIddp) of
                         { !_segOddp ->
                         (case (_lhsIallInters) of
                          { !_segOallInters ->
                          (case (_lhsIprev) of
                           { !_segOfromLhs ->
                           (case (True) of
                            { !_segOisFirst ->
                            (case ((seg_2 _segOallInters _segOddp _segOfromLhs _segOisFirst )) of
                             { ( !_segIcvisits,!_segIedp,!_segIfirstInh,!_segIhdIntravisits,!_segIprev) ->
                             (case (_segIedp) of
                              { !_lhsOedp ->
                              (case (Map.fromList (zip cons_ (transpose _segIcvisits))) of
                               { !_lhsOvisits ->
                               ( _lhsOedp,_lhsOvisits) }) }) }) }) }) }) })) )
-- Interfaces --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         info                 : Info
      chained attribute:
         v                    : Vertex
      synthesized attributes:
         descr                : Seq (Vertex,ChildVisit)
         firstvisitvertices   : [Vertex]
         newedges             : Seq Edge 
   visit 1:
      inherited attributes:
         prev                 : [Vertex]
         visitDescr           : Map Vertex ChildVisit
         vssGraph             : Graph
      synthesized attribute:
         inters               : CInterfaceMap
   visit 2:
      inherited attributes:
         allInters            : CInterfaceMap
         ddp                  : Graph
      synthesized attributes:
         edp                  : Seq Edge
         visits               : CVisitsMap
   alternatives:
      alternative Cons:
         child hd             : Interface 
         child tl             : Interfaces 
      alternative Nil:
-}
-- cata
sem_Interfaces :: Interfaces  ->
                  T_Interfaces 
sem_Interfaces !list  =
    (Prelude.foldr sem_Interfaces_Cons sem_Interfaces_Nil (Prelude.map sem_Interface list) )
-- semantic domain
newtype T_Interfaces  = T_Interfaces (Info ->
                                      Vertex ->
                                      ( (Seq (Vertex,ChildVisit)),([Vertex]),(Seq Edge ),Vertex,T_Interfaces_1 ))
newtype T_Interfaces_1  = T_Interfaces_1 (([Vertex]) ->
                                          (Map Vertex ChildVisit) ->
                                          Graph ->
                                          ( CInterfaceMap,T_Interfaces_2 ))
newtype T_Interfaces_2  = T_Interfaces_2 (CInterfaceMap ->
                                          Graph ->
                                          ( (Seq Edge),CVisitsMap))
data Inh_Interfaces  = Inh_Interfaces {allInters_Inh_Interfaces :: !(CInterfaceMap),ddp_Inh_Interfaces :: !(Graph),info_Inh_Interfaces :: !(Info),prev_Inh_Interfaces :: !([Vertex]),v_Inh_Interfaces :: !(Vertex),visitDescr_Inh_Interfaces :: !(Map Vertex ChildVisit),vssGraph_Inh_Interfaces :: !(Graph)}
data Syn_Interfaces  = Syn_Interfaces {descr_Syn_Interfaces :: !(Seq (Vertex,ChildVisit)),edp_Syn_Interfaces :: !(Seq Edge),firstvisitvertices_Syn_Interfaces :: !([Vertex]),inters_Syn_Interfaces :: !(CInterfaceMap),newedges_Syn_Interfaces :: !(Seq Edge ),v_Syn_Interfaces :: !(Vertex),visits_Syn_Interfaces :: !(CVisitsMap)}
wrap_Interfaces :: T_Interfaces  ->
                   Inh_Interfaces  ->
                   Syn_Interfaces 
wrap_Interfaces !(T_Interfaces sem ) !(Inh_Interfaces _lhsIallInters _lhsIddp _lhsIinfo _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph )  =
    (let ( !_lhsOdescr,!_lhsOfirstvisitvertices,!_lhsOnewedges,!_lhsOv,!T_Interfaces_1 sem_1) =
             (sem _lhsIinfo _lhsIv )
         ( !_lhsOinters,!T_Interfaces_2 sem_2) =
             (sem_1 _lhsIprev _lhsIvisitDescr _lhsIvssGraph )
         ( !_lhsOedp,!_lhsOvisits) =
             (sem_2 _lhsIallInters _lhsIddp )
     in  (Syn_Interfaces _lhsOdescr _lhsOedp _lhsOfirstvisitvertices _lhsOinters _lhsOnewedges _lhsOv _lhsOvisits ))
sem_Interfaces_Cons :: T_Interface  ->
                       T_Interfaces  ->
                       T_Interfaces 
sem_Interfaces_Cons !(T_Interface hd_ ) !(T_Interfaces tl_ )  =
    (T_Interfaces (\ (!_lhsIinfo)
                     (!_lhsIv) ->
                       (case (_lhsIv) of
                        { !_hdOv ->
                        (case (_lhsIinfo) of
                         { !_hdOinfo ->
                         (case ((hd_ _hdOinfo _hdOv )) of
                          { ( !_hdIdescr,!_hdIfirstvisitvertices,!_hdInewedges,!_hdIv,!T_Interface_1 hd_1) ->
                          (case (_hdIv) of
                           { !_tlOv ->
                           (case (_lhsIinfo) of
                            { !_tlOinfo ->
                            (case ((tl_ _tlOinfo _tlOv )) of
                             { ( !_tlIdescr,!_tlIfirstvisitvertices,!_tlInewedges,!_tlIv,!T_Interfaces_1 tl_1) ->
                             (case (_hdIdescr Seq.>< _tlIdescr) of
                              { !_lhsOdescr ->
                              (case (_hdIfirstvisitvertices ++ _tlIfirstvisitvertices) of
                               { !_lhsOfirstvisitvertices ->
                               (case (_hdInewedges Seq.>< _tlInewedges) of
                                { !_lhsOnewedges ->
                                (case (_tlIv) of
                                 { !_lhsOv ->
                                 (case ((sem_Interfaces_Cons_1 (T_Interfaces_1 tl_1 ) (T_Interface_1 hd_1 ) )) of
                                  { ( !sem_Interfaces_1) ->
                                  ( _lhsOdescr,_lhsOfirstvisitvertices,_lhsOnewedges,_lhsOv,sem_Interfaces_1) }) }) }) }) }) }) }) }) }) }) })) )
sem_Interfaces_Cons_1 :: T_Interfaces_1  ->
                         T_Interface_1  ->
                         T_Interfaces_1 
sem_Interfaces_Cons_1 !(T_Interfaces_1 tl_1 ) !(T_Interface_1 hd_1 )  =
    (T_Interfaces_1 (\ (!_lhsIprev)
                       (!_lhsIvisitDescr)
                       (!_lhsIvssGraph) ->
                         (case (_lhsIvssGraph) of
                          { !_tlOvssGraph ->
                          (case (_lhsIvisitDescr) of
                           { !_tlOvisitDescr ->
                           (case (_lhsIprev) of
                            { !_tlOprev ->
                            (case (_lhsIvssGraph) of
                             { !_hdOvssGraph ->
                             (case (_lhsIvisitDescr) of
                              { !_hdOvisitDescr ->
                              (case (_lhsIprev) of
                               { !_hdOprev ->
                               (case ((tl_1 _tlOprev _tlOvisitDescr _tlOvssGraph )) of
                                { ( !_tlIinters,!T_Interfaces_2 tl_2) ->
                                (case ((hd_1 _hdOprev _hdOvisitDescr _hdOvssGraph )) of
                                 { ( !_hdIinter,!_hdInt,!T_Interface_2 hd_2) ->
                                 (case (Map.insert _hdInt _hdIinter _tlIinters) of
                                  { !_lhsOinters ->
                                  (case ((sem_Interfaces_Cons_2 (T_Interfaces_2 tl_2 ) (T_Interface_2 hd_2 ) _hdInt )) of
                                   { ( !sem_Interfaces_2) ->
                                   ( _lhsOinters,sem_Interfaces_2) }) }) }) }) }) }) }) }) }) })) )
sem_Interfaces_Cons_2 :: T_Interfaces_2  ->
                         T_Interface_2  ->
                         NontermIdent ->
                         T_Interfaces_2 
sem_Interfaces_Cons_2 !(T_Interfaces_2 tl_2 ) !(T_Interface_2 hd_2 ) !_hdInt  =
    (T_Interfaces_2 (\ (!_lhsIallInters)
                       (!_lhsIddp) ->
                         (case (_lhsIddp) of
                          { !_tlOddp ->
                          (case (_lhsIallInters) of
                           { !_tlOallInters ->
                           (case ((tl_2 _tlOallInters _tlOddp )) of
                            { ( !_tlIedp,!_tlIvisits) ->
                            (case (_lhsIddp) of
                             { !_hdOddp ->
                             (case (_lhsIallInters) of
                              { !_hdOallInters ->
                              (case ((hd_2 _hdOallInters _hdOddp )) of
                               { ( !_hdIedp,!_hdIvisits) ->
                               (case (_hdIedp Seq.>< _tlIedp) of
                                { !_lhsOedp ->
                                (case (Map.insert _hdInt _hdIvisits _tlIvisits) of
                                 { !_lhsOvisits ->
                                 ( _lhsOedp,_lhsOvisits) }) }) }) }) }) }) }) })) )
sem_Interfaces_Nil :: T_Interfaces 
sem_Interfaces_Nil  =
    (T_Interfaces (\ (!_lhsIinfo)
                     (!_lhsIv) ->
                       (case (Seq.empty) of
                        { !_lhsOdescr ->
                        (case ([]) of
                         { !_lhsOfirstvisitvertices ->
                         (case (Seq.empty) of
                          { !_lhsOnewedges ->
                          (case (_lhsIv) of
                           { !_lhsOv ->
                           (case ((sem_Interfaces_Nil_1 )) of
                            { ( !sem_Interfaces_1) ->
                            ( _lhsOdescr,_lhsOfirstvisitvertices,_lhsOnewedges,_lhsOv,sem_Interfaces_1) }) }) }) }) })) )
sem_Interfaces_Nil_1 :: T_Interfaces_1 
sem_Interfaces_Nil_1  =
    (T_Interfaces_1 (\ (!_lhsIprev)
                       (!_lhsIvisitDescr)
                       (!_lhsIvssGraph) ->
                         (case (Map.empty) of
                          { !_lhsOinters ->
                          (case ((sem_Interfaces_Nil_2 )) of
                           { ( !sem_Interfaces_2) ->
                           ( _lhsOinters,sem_Interfaces_2) }) })) )
sem_Interfaces_Nil_2 :: T_Interfaces_2 
sem_Interfaces_Nil_2  =
    (T_Interfaces_2 (\ (!_lhsIallInters)
                       (!_lhsIddp) ->
                         (case (Seq.empty) of
                          { !_lhsOedp ->
                          (case (Map.empty) of
                           { !_lhsOvisits ->
                           ( _lhsOedp,_lhsOvisits) }) })) )
-- Segment -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         info                 : Info
      chained attribute:
         v                    : Vertex
   visit 1:
      inherited attributes:
         n                    : Int
         nextNewvertices      : [Vertex]
      synthesized attributes:
         descr                : Seq (Vertex,ChildVisit)
         groups               : [([Vertex],[Vertex])]
         newedges             : Seq Edge 
         newvertices          : [Vertex]
   visit 2:
      inherited attributes:
         cons                 : [ConstructorIdent]
         visitDescr           : Map Vertex ChildVisit
         vssGraph             : Graph
      chained attribute:
         prev                 : [Vertex]
      synthesized attribute:
         seg                  : CSegment
   visit 3:
      inherited attributes:
         allInters            : CInterfaceMap
         ddp                  : Graph
         fromLhs              : [Vertex]
         isFirst              : Bool
         nextInh              : [Vertex]
         nextIntravisits      : [IntraVisit]
      synthesized attributes:
         cvisits              : [CVisit]
         edp                  : Seq Edge
         inh                  : [Vertex]
         intravisits          : [IntraVisit]
         visitss              : [VisitSS]
   alternatives:
      alternative Segment:
         child inh            : {[Vertex]}
         child syn            : {[Vertex]}
         visit 0:
            local look        : {Vertex -> CRule}
            local occurAs     : {(CRule -> Bool) -> [Vertex] -> [Vertex]}
            local groups      : {[([Vertex],[Vertex])]}
            local v           : {Int}
         visit 1:
            local newvertices : _
            local visitedges  : _
            local attredges   : _
            intra v           : {Int}
            intra groups      : {[([Vertex],[Vertex])]}
            intra look        : {Vertex -> CRule}
            intra occurAs     : {(CRule -> Bool) -> [Vertex] -> [Vertex]}
         visit 2:
            local synOccur    : _
            local vss         : _
            local visitss'    : _
            local visitss     : {[[Vertex]]}
            local defined     : _
            local _tup1       : {(Map Identifier Type,Map Identifier Type)}
            local synmap      : {Map Identifier Type}
            local inhmap      : {Map Identifier Type}
            intra occurAs     : {(CRule -> Bool) -> [Vertex] -> [Vertex]}
         visit 3:
            local computed    : _
            local fromLhs     : _
            local iv          : _
            local intravisits : _
            intra visitss     : {[[Vertex]]}
            intra occurAs     : {(CRule -> Bool) -> [Vertex] -> [Vertex]}
            intra synmap      : {Map Identifier Type}
            intra inhmap      : {Map Identifier Type}
-}
-- cata
sem_Segment :: Segment  ->
               T_Segment 
sem_Segment !(Segment _inh _syn )  =
    (sem_Segment_Segment _inh _syn )
-- semantic domain
newtype T_Segment  = T_Segment (Info ->
                                Vertex ->
                                ( Vertex,T_Segment_1 ))
newtype T_Segment_1  = T_Segment_1 (Int ->
                                    ([Vertex]) ->
                                    ( (Seq (Vertex,ChildVisit)),([([Vertex],[Vertex])]),(Seq Edge ),([Vertex]),T_Segment_2 ))
newtype T_Segment_2  = T_Segment_2 (([ConstructorIdent]) ->
                                    ([Vertex]) ->
                                    (Map Vertex ChildVisit) ->
                                    Graph ->
                                    ( ([Vertex]),CSegment,T_Segment_3 ))
newtype T_Segment_3  = T_Segment_3 (CInterfaceMap ->
                                    Graph ->
                                    ([Vertex]) ->
                                    Bool ->
                                    ([Vertex]) ->
                                    ([IntraVisit]) ->
                                    ( ([CVisit]),(Seq Edge),([Vertex]),([IntraVisit]),([VisitSS])))
data Inh_Segment  = Inh_Segment {allInters_Inh_Segment :: !(CInterfaceMap),cons_Inh_Segment :: !([ConstructorIdent]),ddp_Inh_Segment :: !(Graph),fromLhs_Inh_Segment :: !([Vertex]),info_Inh_Segment :: !(Info),isFirst_Inh_Segment :: !(Bool),n_Inh_Segment :: !(Int),nextInh_Inh_Segment :: !([Vertex]),nextIntravisits_Inh_Segment :: !([IntraVisit]),nextNewvertices_Inh_Segment :: !([Vertex]),prev_Inh_Segment :: !([Vertex]),v_Inh_Segment :: !(Vertex),visitDescr_Inh_Segment :: !(Map Vertex ChildVisit),vssGraph_Inh_Segment :: !(Graph)}
data Syn_Segment  = Syn_Segment {cvisits_Syn_Segment :: !([CVisit]),descr_Syn_Segment :: !(Seq (Vertex,ChildVisit)),edp_Syn_Segment :: !(Seq Edge),groups_Syn_Segment :: !([([Vertex],[Vertex])]),inh_Syn_Segment :: !([Vertex]),intravisits_Syn_Segment :: !([IntraVisit]),newedges_Syn_Segment :: !(Seq Edge ),newvertices_Syn_Segment :: !([Vertex]),prev_Syn_Segment :: !([Vertex]),seg_Syn_Segment :: !(CSegment),v_Syn_Segment :: !(Vertex),visitss_Syn_Segment :: !([VisitSS])}
wrap_Segment :: T_Segment  ->
                Inh_Segment  ->
                Syn_Segment 
wrap_Segment !(T_Segment sem ) !(Inh_Segment _lhsIallInters _lhsIcons _lhsIddp _lhsIfromLhs _lhsIinfo _lhsIisFirst _lhsIn _lhsInextInh _lhsInextIntravisits _lhsInextNewvertices _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph )  =
    (let ( !_lhsOv,!T_Segment_1 sem_1) =
             (sem _lhsIinfo _lhsIv )
         ( !_lhsOdescr,!_lhsOgroups,!_lhsOnewedges,!_lhsOnewvertices,!T_Segment_2 sem_2) =
             (sem_1 _lhsIn _lhsInextNewvertices )
         ( !_lhsOprev,!_lhsOseg,!T_Segment_3 sem_3) =
             (sem_2 _lhsIcons _lhsIprev _lhsIvisitDescr _lhsIvssGraph )
         ( !_lhsOcvisits,!_lhsOedp,!_lhsOinh,!_lhsOintravisits,!_lhsOvisitss) =
             (sem_3 _lhsIallInters _lhsIddp _lhsIfromLhs _lhsIisFirst _lhsInextInh _lhsInextIntravisits )
     in  (Syn_Segment _lhsOcvisits _lhsOdescr _lhsOedp _lhsOgroups _lhsOinh _lhsOintravisits _lhsOnewedges _lhsOnewvertices _lhsOprev _lhsOseg _lhsOv _lhsOvisitss ))
sem_Segment_Segment :: ([Vertex]) ->
                       ([Vertex]) ->
                       T_Segment 
sem_Segment_Segment !inh_ !syn_  =
    (T_Segment (\ (!_lhsIinfo)
                  (!_lhsIv) ->
                    (case (\a -> ruleTable _lhsIinfo ! a) of
                     { !_look ->
                     (case (\p us -> [ a  |  u <- us
                                          ,  a <- tdsToTdp _lhsIinfo ! u
                                          ,  p (_look a)]) of
                      { !_occurAs ->
                      (case (let group as = gather _lhsIinfo (_occurAs isRhs as)
                             in map (partition (isInh . _look)) (group (inh_ ++ syn_))) of
                       { !_groups ->
                       (case (_lhsIv + length _groups) of
                        { !_v ->
                        (case (_v) of
                         { !_lhsOv ->
                         (case ((sem_Segment_Segment_1 _v _lhsIv _groups _look syn_ _occurAs _lhsIinfo inh_ )) of
                          { ( !sem_Segment_1) ->
                          ( _lhsOv,sem_Segment_1) }) }) }) }) }) })) )
sem_Segment_Segment_1 :: Int ->
                         Vertex ->
                         ([([Vertex],[Vertex])]) ->
                         (Vertex -> CRule) ->
                         ([Vertex]) ->
                         ((CRule -> Bool) -> [Vertex] -> [Vertex]) ->
                         Info ->
                         ([Vertex]) ->
                         T_Segment_1 
sem_Segment_Segment_1 !_v !_lhsIv !_groups !_look !syn_ !_occurAs !_lhsIinfo !inh_  =
    (T_Segment_1 (\ (!_lhsIn)
                    (!_lhsInextNewvertices) ->
                      (case ([_lhsIv .. _v    -1]) of
                       { !_newvertices ->
                       (case (Seq.fromList $ zipWith (cv _look _lhsIn) _newvertices _groups) of
                        { !_lhsOdescr ->
                        (case (_groups) of
                         { !_lhsOgroups ->
                         (case (zip _newvertices _lhsInextNewvertices) of
                          { !_visitedges ->
                          (case (concat (zipWith ed _newvertices _groups)) of
                           { !_attredges ->
                           (case (Seq.fromList _attredges Seq.>< Seq.fromList _visitedges) of
                            { !_lhsOnewedges ->
                            (case (_newvertices) of
                             { !_lhsOnewvertices ->
                             (case ((sem_Segment_Segment_2 syn_ _occurAs _lhsIinfo inh_ )) of
                              { ( !sem_Segment_2) ->
                              ( _lhsOdescr,_lhsOgroups,_lhsOnewedges,_lhsOnewvertices,sem_Segment_2) }) }) }) }) }) }) }) })) )
sem_Segment_Segment_2 :: ([Vertex]) ->
                         ((CRule -> Bool) -> [Vertex] -> [Vertex]) ->
                         Info ->
                         ([Vertex]) ->
                         T_Segment_2 
sem_Segment_Segment_2 !syn_ !_occurAs !_lhsIinfo !inh_  =
    (T_Segment_2 (\ (!_lhsIcons)
                    (!_lhsIprev)
                    (!_lhsIvisitDescr)
                    (!_lhsIvssGraph) ->
                      (case (gather _lhsIinfo (_occurAs isLhs syn_)) of
                       { !_synOccur ->
                       (case (let hasCode v | inRange (bounds (ruleTable _lhsIinfo)) v =  getHasCode (ruleTable _lhsIinfo ! v)
                                            | otherwise = True
                              in if  null syn_
                                     then replicate (length _lhsIcons) []
                                     else map (filter hasCode . topSort' _lhsIvssGraph) _synOccur) of
                        { !_vss ->
                        (case (map (\\ _lhsIprev) _vss) of
                         { !_visitss' ->
                         (case (let  rem :: [(Identifier,Identifier,Maybe Type)] -> [Vertex] -> [Vertex]
                                     rem prev [] = []
                                     rem prev (v:vs)
                                       | inRange (bounds table) v
                                           = let  cr = table ! v
                                                  addV = case findIndex cmp prev of
                                                           Just _ -> id
                                                           _      -> (v:)
                                                  cmp (fld,attr,tp) = getField cr == fld && getAttr cr == attr && sameNT (getType cr) tp
                                                  sameNT (Just (NT ntA _)) (Just (NT ntB _)) = ntA == ntB
                                                  sameNT _          _                        = False
                                                  def = Map.elems (getDefines cr)
                                             in addV (rem (def ++ prev) vs)
                                       | otherwise = v:rem prev vs
                                     table = ruleTable _lhsIinfo
                                in map (rem []) _visitss') of
                          { !_visitss ->
                          (case (let defines v = case Map.lookup v _lhsIvisitDescr of
                                                   Nothing -> [v]
                                                   Just (ChildVisit _ _ _ inh _) -> v:inh
                                 in concatMap (concatMap defines) _visitss) of
                           { !_defined ->
                           (case (_lhsIprev ++ _defined) of
                            { !_lhsOprev ->
                            (case (let makemap = Map.fromList . map findType
                                       findType v = getNtaNameType (attrTable _lhsIinfo ! v)
                                   in (makemap inh_,makemap syn_)) of
                             { !__tup1 ->
                             (case (__tup1) of
                              { !(_,!_synmap) ->
                              (case (__tup1) of
                               { !(!_inhmap,_) ->
                               (case (if False then undefined _lhsIvssGraph _lhsIvisitDescr _lhsIprev else CSegment _inhmap _synmap) of
                                { !_lhsOseg ->
                                (case ((sem_Segment_Segment_3 _visitss _lhsIinfo _lhsIvisitDescr inh_ _occurAs _synmap _inhmap syn_ )) of
                                 { ( !sem_Segment_3) ->
                                 ( _lhsOprev,_lhsOseg,sem_Segment_3) }) }) }) }) }) }) }) }) }) }) })) )
sem_Segment_Segment_3 :: ([[Vertex]]) ->
                         Info ->
                         (Map Vertex ChildVisit) ->
                         ([Vertex]) ->
                         ((CRule -> Bool) -> [Vertex] -> [Vertex]) ->
                         (Map Identifier Type) ->
                         (Map Identifier Type) ->
                         ([Vertex]) ->
                         T_Segment_3 
sem_Segment_Segment_3 !_visitss !_lhsIinfo !_lhsIvisitDescr !inh_ !_occurAs !_synmap !_inhmap !syn_  =
    (T_Segment_3 (\ (!_lhsIallInters)
                    (!_lhsIddp)
                    (!_lhsIfromLhs)
                    (!_lhsIisFirst)
                    (!_lhsInextInh)
                    (!_lhsInextIntravisits) ->
                      (case (let computes v = case Map.lookup v _lhsIvisitDescr of
                                                Nothing -> Map.keys (getDefines (ruleTable _lhsIinfo ! v))
                                                Just (ChildVisit _ _ _ _ syn) -> v:syn
                             in concatMap (concatMap computes) _visitss) of
                       { !_computed ->
                       (case (_occurAs isLhs inh_ ++ _lhsIfromLhs) of
                        { !_fromLhs ->
                        (case (\vs next ->
                                 let needed = concatMap (_lhsIddp !) vs
                                 in nub (needed ++ next) \\ (_fromLhs ++ _computed)) of
                         { !_iv ->
                         (case (zipWith _iv _visitss _lhsInextIntravisits) of
                          { !_intravisits ->
                          (case (let  mkVisit vss intra = CVisit _inhmap _synmap (mkSequence vss) (mkSequence intra) True
                                      mkSequence = map mkRule
                                      mkRule v = case Map.lookup v _lhsIvisitDescr of
                                                   Nothing -> ruleTable _lhsIinfo ! v
                                                   Just (ChildVisit name nt n _ _) -> ccv name nt n _lhsIallInters
                                 in zipWith mkVisit _visitss _intravisits) of
                           { !_lhsOcvisits ->
                           (case (Seq.fromList [(i,s) | i <- inh_, s <- syn_]
                                  Seq.>< Seq.fromList [(s,i) | s <- syn_, i <- _lhsInextInh ]) of
                            { !_lhsOedp ->
                            (case (inh_) of
                             { !_lhsOinh ->
                             (case (_intravisits) of
                              { !_lhsOintravisits ->
                              (case (_visitss) of
                               { !_lhsOvisitss ->
                               ( _lhsOcvisits,_lhsOedp,_lhsOinh,_lhsOintravisits,_lhsOvisitss) }) }) }) }) }) }) }) }) })) )
-- Segments ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         info                 : Info
         n                    : Int
      chained attribute:
         v                    : Vertex
      synthesized attributes:
         descr                : Seq (Vertex,ChildVisit)
         groups               : [([Vertex],[Vertex])]
         newedges             : Seq Edge 
         newvertices          : [Vertex]
   visit 1:
      inherited attributes:
         cons                 : [ConstructorIdent]
         prev                 : [Vertex]
         visitDescr           : Map Vertex ChildVisit
         vssGraph             : Graph
      synthesized attribute:
         segs                 : CSegments
   visit 2:
      inherited attributes:
         allInters            : CInterfaceMap
         ddp                  : Graph
         fromLhs              : [Vertex]
         isFirst              : Bool
      synthesized attributes:
         cvisits              : [[CVisit]]
         edp                  : Seq Edge
         firstInh             : [Vertex]
         hdIntravisits        : [IntraVisit]
         prev                 : [Vertex]
   alternatives:
      alternative Cons:
         child hd             : Segment 
         child tl             : Segments 
      alternative Nil:
-}
-- cata
sem_Segments :: Segments  ->
                T_Segments 
sem_Segments !list  =
    (Prelude.foldr sem_Segments_Cons sem_Segments_Nil (Prelude.map sem_Segment list) )
-- semantic domain
newtype T_Segments  = T_Segments (Info ->
                                  Int ->
                                  Vertex ->
                                  ( (Seq (Vertex,ChildVisit)),([([Vertex],[Vertex])]),(Seq Edge ),([Vertex]),Vertex,T_Segments_1 ))
newtype T_Segments_1  = T_Segments_1 (([ConstructorIdent]) ->
                                      ([Vertex]) ->
                                      (Map Vertex ChildVisit) ->
                                      Graph ->
                                      ( CSegments,T_Segments_2 ))
newtype T_Segments_2  = T_Segments_2 (CInterfaceMap ->
                                      Graph ->
                                      ([Vertex]) ->
                                      Bool ->
                                      ( ([[CVisit]]),(Seq Edge),([Vertex]),([IntraVisit]),([Vertex])))
data Inh_Segments  = Inh_Segments {allInters_Inh_Segments :: !(CInterfaceMap),cons_Inh_Segments :: !([ConstructorIdent]),ddp_Inh_Segments :: !(Graph),fromLhs_Inh_Segments :: !([Vertex]),info_Inh_Segments :: !(Info),isFirst_Inh_Segments :: !(Bool),n_Inh_Segments :: !(Int),prev_Inh_Segments :: !([Vertex]),v_Inh_Segments :: !(Vertex),visitDescr_Inh_Segments :: !(Map Vertex ChildVisit),vssGraph_Inh_Segments :: !(Graph)}
data Syn_Segments  = Syn_Segments {cvisits_Syn_Segments :: !([[CVisit]]),descr_Syn_Segments :: !(Seq (Vertex,ChildVisit)),edp_Syn_Segments :: !(Seq Edge),firstInh_Syn_Segments :: !([Vertex]),groups_Syn_Segments :: !([([Vertex],[Vertex])]),hdIntravisits_Syn_Segments :: !([IntraVisit]),newedges_Syn_Segments :: !(Seq Edge ),newvertices_Syn_Segments :: !([Vertex]),prev_Syn_Segments :: !([Vertex]),segs_Syn_Segments :: !(CSegments),v_Syn_Segments :: !(Vertex)}
wrap_Segments :: T_Segments  ->
                 Inh_Segments  ->
                 Syn_Segments 
wrap_Segments !(T_Segments sem ) !(Inh_Segments _lhsIallInters _lhsIcons _lhsIddp _lhsIfromLhs _lhsIinfo _lhsIisFirst _lhsIn _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph )  =
    (let ( !_lhsOdescr,!_lhsOgroups,!_lhsOnewedges,!_lhsOnewvertices,!_lhsOv,!T_Segments_1 sem_1) =
             (sem _lhsIinfo _lhsIn _lhsIv )
         ( !_lhsOsegs,!T_Segments_2 sem_2) =
             (sem_1 _lhsIcons _lhsIprev _lhsIvisitDescr _lhsIvssGraph )
         ( !_lhsOcvisits,!_lhsOedp,!_lhsOfirstInh,!_lhsOhdIntravisits,!_lhsOprev) =
             (sem_2 _lhsIallInters _lhsIddp _lhsIfromLhs _lhsIisFirst )
     in  (Syn_Segments _lhsOcvisits _lhsOdescr _lhsOedp _lhsOfirstInh _lhsOgroups _lhsOhdIntravisits _lhsOnewedges _lhsOnewvertices _lhsOprev _lhsOsegs _lhsOv ))
sem_Segments_Cons :: T_Segment  ->
                     T_Segments  ->
                     T_Segments 
sem_Segments_Cons !(T_Segment hd_ ) !(T_Segments tl_ )  =
    (T_Segments (\ (!_lhsIinfo)
                   (!_lhsIn)
                   (!_lhsIv) ->
                     (case (_lhsIv) of
                      { !_hdOv ->
                      (case (_lhsIinfo) of
                       { !_hdOinfo ->
                       (case ((hd_ _hdOinfo _hdOv )) of
                        { ( !_hdIv,!T_Segment_1 hd_1) ->
                        (case (_hdIv) of
                         { !_tlOv ->
                         (case (_lhsIinfo) of
                          { !_tlOinfo ->
                          (case (_lhsIn) of
                           { !_hdOn ->
                           (case (_lhsIn + 1) of
                            { !_tlOn ->
                            (case ((tl_ _tlOinfo _tlOn _tlOv )) of
                             { ( !_tlIdescr,!_tlIgroups,!_tlInewedges,!_tlInewvertices,!_tlIv,!T_Segments_1 tl_1) ->
                             (case (_tlInewvertices) of
                              { !_hdOnextNewvertices ->
                              (case ((hd_1 _hdOn _hdOnextNewvertices )) of
                               { ( !_hdIdescr,!_hdIgroups,!_hdInewedges,!_hdInewvertices,!T_Segment_2 hd_2) ->
                               (case (_hdIdescr Seq.>< _tlIdescr) of
                                { !_lhsOdescr ->
                                (case (_hdIgroups) of
                                 { !_lhsOgroups ->
                                 (case (_hdInewedges Seq.>< _tlInewedges) of
                                  { !_lhsOnewedges ->
                                  (case (_hdInewvertices) of
                                   { !_lhsOnewvertices ->
                                   (case (_tlIv) of
                                    { !_lhsOv ->
                                    (case ((sem_Segments_Cons_1 (T_Segment_2 hd_2 ) (T_Segments_1 tl_1 ) )) of
                                     { ( !sem_Segments_1) ->
                                     ( _lhsOdescr,_lhsOgroups,_lhsOnewedges,_lhsOnewvertices,_lhsOv,sem_Segments_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Segments_Cons_1 :: T_Segment_2  ->
                       T_Segments_1  ->
                       T_Segments_1 
sem_Segments_Cons_1 !(T_Segment_2 hd_2 ) !(T_Segments_1 tl_1 )  =
    (T_Segments_1 (\ (!_lhsIcons)
                     (!_lhsIprev)
                     (!_lhsIvisitDescr)
                     (!_lhsIvssGraph) ->
                       (case (_lhsIvssGraph) of
                        { !_tlOvssGraph ->
                        (case (_lhsIvisitDescr) of
                         { !_tlOvisitDescr ->
                         (case (_lhsIvssGraph) of
                          { !_hdOvssGraph ->
                          (case (_lhsIvisitDescr) of
                           { !_hdOvisitDescr ->
                           (case (_lhsIprev) of
                            { !_hdOprev ->
                            (case (_lhsIcons) of
                             { !_hdOcons ->
                             (case ((hd_2 _hdOcons _hdOprev _hdOvisitDescr _hdOvssGraph )) of
                              { ( !_hdIprev,!_hdIseg,!T_Segment_3 hd_3) ->
                              (case (_hdIprev) of
                               { !_tlOprev ->
                               (case (_lhsIcons) of
                                { !_tlOcons ->
                                (case ((tl_1 _tlOcons _tlOprev _tlOvisitDescr _tlOvssGraph )) of
                                 { ( !_tlIsegs,!T_Segments_2 tl_2) ->
                                 (case (_hdIseg : _tlIsegs) of
                                  { !_lhsOsegs ->
                                  (case ((sem_Segments_Cons_2 (T_Segments_2 tl_2 ) (T_Segment_3 hd_3 ) )) of
                                   { ( !sem_Segments_2) ->
                                   ( _lhsOsegs,sem_Segments_2) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Segments_Cons_2 :: T_Segments_2  ->
                       T_Segment_3  ->
                       T_Segments_2 
sem_Segments_Cons_2 !(T_Segments_2 tl_2 ) !(T_Segment_3 hd_3 )  =
    (T_Segments_2 (\ (!_lhsIallInters)
                     (!_lhsIddp)
                     (!_lhsIfromLhs)
                     (!_lhsIisFirst) ->
                       (case (_lhsIddp) of
                        { !_tlOddp ->
                        (case (_lhsIallInters) of
                         { !_tlOallInters ->
                         (case (_lhsIddp) of
                          { !_hdOddp ->
                          (case (_lhsIallInters) of
                           { !_hdOallInters ->
                           (case ([]) of
                            { !_tlOfromLhs ->
                            (case (_lhsIfromLhs) of
                             { !_hdOfromLhs ->
                             (case (False) of
                              { !_tlOisFirst ->
                              (case ((tl_2 _tlOallInters _tlOddp _tlOfromLhs _tlOisFirst )) of
                               { ( !_tlIcvisits,!_tlIedp,!_tlIfirstInh,!_tlIhdIntravisits,!_tlIprev) ->
                               (case (_tlIhdIntravisits) of
                                { !_hdOnextIntravisits ->
                                (case (_lhsIisFirst) of
                                 { !_hdOisFirst ->
                                 (case (_tlIfirstInh) of
                                  { !_hdOnextInh ->
                                  (case ((hd_3 _hdOallInters _hdOddp _hdOfromLhs _hdOisFirst _hdOnextInh _hdOnextIntravisits )) of
                                   { ( !_hdIcvisits,!_hdIedp,!_hdIinh,!_hdIintravisits,!_hdIvisitss) ->
                                   (case (_hdIcvisits : _tlIcvisits) of
                                    { !_lhsOcvisits ->
                                    (case (_hdIedp Seq.>< _tlIedp) of
                                     { !_lhsOedp ->
                                     (case (_hdIinh) of
                                      { !_lhsOfirstInh ->
                                      (case (_hdIintravisits) of
                                       { !_lhsOhdIntravisits ->
                                       (case (_tlIprev) of
                                        { !_lhsOprev ->
                                        ( _lhsOcvisits,_lhsOedp,_lhsOfirstInh,_lhsOhdIntravisits,_lhsOprev) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Segments_Nil :: T_Segments 
sem_Segments_Nil  =
    (T_Segments (\ (!_lhsIinfo)
                   (!_lhsIn)
                   (!_lhsIv) ->
                     (case (Seq.empty) of
                      { !_lhsOdescr ->
                      (case ([]) of
                       { !_lhsOgroups ->
                       (case (Seq.empty) of
                        { !_lhsOnewedges ->
                        (case ([]) of
                         { !_lhsOnewvertices ->
                         (case (_lhsIv) of
                          { !_lhsOv ->
                          (case ((sem_Segments_Nil_1 )) of
                           { ( !sem_Segments_1) ->
                           ( _lhsOdescr,_lhsOgroups,_lhsOnewedges,_lhsOnewvertices,_lhsOv,sem_Segments_1) }) }) }) }) }) })) )
sem_Segments_Nil_1 :: T_Segments_1 
sem_Segments_Nil_1  =
    (T_Segments_1 (\ (!_lhsIcons)
                     (!_lhsIprev)
                     (!_lhsIvisitDescr)
                     (!_lhsIvssGraph) ->
                       (case ([]) of
                        { !_lhsOsegs ->
                        (case ((sem_Segments_Nil_2 _lhsIprev )) of
                         { ( !sem_Segments_2) ->
                         ( _lhsOsegs,sem_Segments_2) }) })) )
sem_Segments_Nil_2 :: ([Vertex]) ->
                      T_Segments_2 
sem_Segments_Nil_2 !_lhsIprev  =
    (T_Segments_2 (\ (!_lhsIallInters)
                     (!_lhsIddp)
                     (!_lhsIfromLhs)
                     (!_lhsIisFirst) ->
                       (case ([]) of
                        { !_lhsOcvisits ->
                        (case (Seq.empty) of
                         { !_lhsOedp ->
                         (case ([]) of
                          { !_lhsOfirstInh ->
                          (case (repeat []) of
                           { !_lhsOhdIntravisits ->
                           (case (_lhsIprev) of
                            { !_lhsOprev ->
                            ( _lhsOcvisits,_lhsOedp,_lhsOfirstInh,_lhsOhdIntravisits,_lhsOprev) }) }) }) }) })) )