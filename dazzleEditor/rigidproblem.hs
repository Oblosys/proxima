{-# OPTIONS_GHC -fglasgow-exts #-}

-- UUAGC 0.9.10 (../../proxima/src/Layout/ScannerAG.ag)
module Layout.ScannerAG where
{-# LINE 11 "../../proxima/src/Layout/ScannerAG.ag" #-}

import Common.CommonTypes
import Common.CommonUtils
import Presentation.PresTypes
import Layout.LayLayerTypes
import Layout.LayLayerUtils
import List
import Char
import qualified Data.Map as Map
{-# LINE 16 "../../proxima/src/Layout/ScannerAG.hs" #-}

stylesFromAttrs = undefined
sem_Layout :: (DocNode node ,Show token ) => (Layout) (doc) (enr) (node) (clip) (token)  ->
                                             (T_Layout) (doc) (enr) (node) (clip) (token) 
sem_Layout (WithP _attrRule _child )  | (WithP _attrRule _child ) `seq` (True) =
    (sem_Layout_WithP _attrRule (undefined ) )
-- semantic domain
type T_Layout doc enr node clip token  = ([Style]) ->
                                         (((Path,Int),(Path,Int))) ->
                                         (IDPCounter) ->
                                         (Lexer) ->
                                         (Maybe node) ->
                                         (Path) ->
                                         (Int) ->
                                         ([Style]) ->
                                         (((Path,Int),(Path,Int)) ->
                                                                Lexer -> Maybe node  -> Path -> IDPCounter -> WhitespaceMap -> Layout doc  enr  node  clip  token  ->
                                                                ([Token doc  enr  node  clip  token ], IDPCounter, WhitespaceMap, Presentation doc  enr  node  clip  token )) ->
                                         (Maybe Int) ->
                                         (Maybe Int) ->
                                         (Inherited doc  enr  node  clip  token) ->
                                         (WhitespaceMap) ->
                                         (#  (IDPCounter),(Int),([Style]),([ScanChar doc  enr  node  clip  token ]),(Maybe Int),(Maybe Int),(Layout doc enr node clip token),(WhitespaceMap) #)
sem_Layout_WithP :: (DocNode node ,Show token ) => (AttrRule doc enr node clip token) ->
                                                   (T_Layout) (doc) (enr) (node) (clip) (token)  ->
                                                   (T_Layout) (doc) (enr) (node) (clip) (token) 
sem_Layout_WithP (attrRule_ :: (AttrRule doc enr node clip token)) (child_ :: ([Style]) ->
                                                                              (((Path,Int),(Path,Int))) ->
                                                                              (IDPCounter) ->
                                                                              (Lexer) ->
                                                                              (Maybe node) ->
                                                                              (Path) ->
                                                                              (Int) ->
                                                                              ([Style]) ->
                                                                              (((Path,Int),(Path,Int)) ->
                                                                                                     Lexer -> Maybe node  -> Path -> IDPCounter -> WhitespaceMap -> Layout doc  enr  node  clip  token  ->
                                                                                                     ([Token doc  enr  node  clip  token ], IDPCounter, WhitespaceMap, Presentation doc  enr  node  clip  token )) ->
                                                                              (Maybe Int) ->
                                                                              (Maybe Int) ->
                                                                              (Inherited doc  enr  node  clip  token) ->
                                                                              (WhitespaceMap) ->
                                                                              (#  (IDPCounter),(Int),([Style]),([ScanChar doc  enr  node  clip  token ]),(Maybe Int),(Maybe Int),(Layout doc enr node clip token),(WhitespaceMap) #))  | (attrRule_ :: (AttrRule doc enr node clip token)) `seq` ((child_ :: ([Style]) ->
                                                                                                                                                                                                                                                                                                             (((Path,Int),(Path,Int))) ->
                                                                                                                                                                                                                                                                                                             (IDPCounter) ->
                                                                                                                                                                                                                                                                                                             (Lexer) ->
                                                                                                                                                                                                                                                                                                             (Maybe node) ->
                                                                                                                                                                                                                                                                                                             (Path) ->
                                                                                                                                                                                                                                                                                                             (Int) ->
                                                                                                                                                                                                                                                                                                             ([Style]) ->
                                                                                                                                                                                                                                                                                                             (((Path,Int),(Path,Int)) ->
                                                                                                                                                                                                                                                                                                                                    Lexer -> Maybe node  -> Path -> IDPCounter -> WhitespaceMap -> Layout doc  enr  node  clip  token  ->
                                                                                                                                                                                                                                                                                                                                    ([Token doc  enr  node  clip  token ], IDPCounter, WhitespaceMap, Presentation doc  enr  node  clip  token )) ->
                                                                                                                                                                                                                                                                                                             (Maybe Int) ->
                                                                                                                                                                                                                                                                                                             (Maybe Int) ->
                                                                                                                                                                                                                                                                                                             (Inherited doc  enr  node  clip  token) ->
                                                                                                                                                                                                                                                                                                             (WhitespaceMap) ->
                                                                                                                                                                                                                                                                                                             (#  (IDPCounter),(Int),([Style]),([ScanChar doc  enr  node  clip  token ]),(Maybe Int),(Maybe Int),(Layout doc enr node clip token),(WhitespaceMap) #)) `seq` (True)) =
    (\ (_lhsIcurrentStyles :: ([Style]))
       (_lhsIfocus :: (((Path,Int),(Path,Int))))
       (_lhsIidPCounter :: (IDPCounter))
       (_lhsIlexer :: (Lexer))
       (_lhsIlloc :: (Maybe node))
       (_lhsIpath :: (Path))
       (_lhsIpos :: (Int))
       (_lhsIpreviousCharStyles :: ([Style]))
       (_lhsIscanStructural :: (((Path,Int),(Path,Int)) ->
                                                      Lexer -> Maybe node  -> Path -> IDPCounter -> WhitespaceMap -> Layout doc  enr  node  clip  token  ->
                                                      ([Token doc  enr  node  clip  token ], IDPCounter, WhitespaceMap, Presentation doc  enr  node  clip  token )))
       (_lhsIscannedFocusEnd :: (Maybe Int))
       (_lhsIscannedFocusStart :: (Maybe Int))
       (_lhsIstyleAttrs :: (Inherited doc  enr  node  clip  token))
       (_lhsIwhitespaceMap :: (WhitespaceMap)) ->
         (_lhsIcurrentStyles :: ([Style])) `seq`
         ((_lhsIfocus :: (((Path,Int),(Path,Int)))) `seq`
          ((_lhsIidPCounter :: (IDPCounter)) `seq`
           ((_lhsIlexer :: (Lexer)) `seq`
            ((_lhsIlloc :: (Maybe node)) `seq`
             ((_lhsIpath :: (Path)) `seq`
              ((_lhsIpos :: (Int)) `seq`
               ((_lhsIpreviousCharStyles :: ([Style])) `seq`
                ((_lhsIscanStructural :: (((Path,Int),(Path,Int)) ->
                                                                Lexer -> Maybe node  -> Path -> IDPCounter -> WhitespaceMap -> Layout doc  enr  node  clip  token  ->
                                                                ([Token doc  enr  node  clip  token ], IDPCounter, WhitespaceMap, Presentation doc  enr  node  clip  token ))) `seq`
                 ((_lhsIscannedFocusEnd :: (Maybe Int)) `seq`
                  ((_lhsIscannedFocusStart :: (Maybe Int)) `seq`
                   ((_lhsIstyleAttrs :: (Inherited doc  enr  node  clip  token)) `seq`
                    ((_lhsIwhitespaceMap :: (WhitespaceMap)) `seq`
                     ((case ({-# LINE 137 "../../proxima/src/Layout/ScannerAG.ag" #-}
                             _lhsIwhitespaceMap
                             {-# LINE 2260 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                       { _childOwhitespaceMap | _childOwhitespaceMap `seq` (True) ->
                       (case ({-# LINE 47 "../../proxima/src/Layout/ScannerAG.ag" #-}
                              _lhsIscanStructural
                              {-# LINE 2264 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                        { _childOscanStructural | _childOscanStructural `seq` (True) ->
                        (case ({-# LINE 63 "../../proxima/src/Layout/ScannerAG.ag" #-}
                               _lhsIlloc
                               {-# LINE 2268 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                         { _childOlloc | _childOlloc `seq` (True) ->
                         (case ({-# LINE 57 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                _lhsIlexer
                                {-# LINE 2272 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                          { _childOlexer | _childOlexer `seq` (True) ->
                          (case ({-# LINE 137 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                 _lhsIidPCounter
                                 {-# LINE 2276 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                           { _childOidPCounter | _childOidPCounter `seq` (True) ->
                           (case ({-# LINE 69 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                  _lhsIfocus
                                  {-# LINE 2280 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                            { _childOfocus | _childOfocus `seq` (True) ->
                            (case ({-# LINE 78 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                   _lhsIpath ++ [0]
                                   {-# LINE 2284 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                             { _childOpath | _childOpath `seq` (True) ->
                             (case ({-# LINE 120 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                    _lhsIscannedFocusStart
                                    {-# LINE 2288 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                              { _childOscannedFocusStart | _childOscannedFocusStart `seq` (True) ->
                              (case ({-# LINE 120 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                     _lhsIscannedFocusEnd
                                     {-# LINE 2292 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                               { _childOscannedFocusEnd | _childOscannedFocusEnd `seq` (True) ->
                               (case ({-# LINE 189 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                      _lhsIpreviousCharStyles
                                      {-# LINE 2296 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                                { _childOpreviousCharStyles | _childOpreviousCharStyles `seq` (True) ->
                                (case ({-# LINE 88 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                       _lhsIpos
                                       {-# LINE 2300 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                                 { _childOpos | _childOpos `seq` (True) ->
                                 (case ({-# LINE 193 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                        let (inh,syn) = attrRule_ (_lhsIstyleAttrs, error "Error in presentation: style depends on synthesized attributes")
                                        in  inh
                                        {-# LINE 2305 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                                  { _styleAttrs | _styleAttrs `seq` (True) ->
                                  (case ({-# LINE 196 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                         stylesFromAttrs _styleAttrs
                                         {-# LINE 2309 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                                   { _childOcurrentStyles | _childOcurrentStyles `seq` (True) ->
                                   (case ({-# LINE 195 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                          _styleAttrs
                                          {-# LINE 2313 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                                    { _childOstyleAttrs | _childOstyleAttrs `seq` (True) ->
                                    (case ((child_ _childOcurrentStyles _childOfocus _childOidPCounter _childOlexer _childOlloc _childOpath _childOpos _childOpreviousCharStyles _childOscanStructural _childOscannedFocusEnd _childOscannedFocusStart _childOstyleAttrs _childOwhitespaceMap )) of
                                     { (#  _childIidPCounter,_childIpos,_childIpreviousCharStyles,_childIscanChars,_childIscannedFocusEnd,_childIscannedFocusStart,_childIself,_childIwhitespaceMap #) | _childIidPCounter `seq` (_childIpos `seq` (_childIpreviousCharStyles `seq` (_childIscanChars `seq` (_childIscannedFocusEnd `seq` (_childIscannedFocusStart `seq` (_childIself `seq` (_childIwhitespaceMap `seq` (True)))))))) ->
                                     (case ({-# LINE 137 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                            _childIidPCounter
                                            {-# LINE 2319 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                                      { _lhsOidPCounter | _lhsOidPCounter `seq` (True) ->
                                      (case ({-# LINE 88 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                             _childIpos
                                             {-# LINE 2323 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                                       { _lhsOpos | _lhsOpos `seq` (True) ->
                                       (case ({-# LINE 189 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                              _childIpreviousCharStyles
                                              {-# LINE 2327 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                                        { _lhsOpreviousCharStyles | _lhsOpreviousCharStyles `seq` (True) ->
                                        (case ({-# LINE 155 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                               _childIscanChars
                                               {-# LINE 2331 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                                         { _lhsOscanChars | _lhsOscanChars `seq` (True) ->
                                         (case ({-# LINE 120 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                                _childIscannedFocusEnd
                                                {-# LINE 2335 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                                          { _lhsOscannedFocusEnd | _lhsOscannedFocusEnd `seq` (True) ->
                                          (case ({-# LINE 120 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                                 _childIscannedFocusStart
                                                 {-# LINE 2339 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                                           { _lhsOscannedFocusStart | _lhsOscannedFocusStart `seq` (True) ->
                                           (case (WithP attrRule_ _childIself) of
                                            { _self | _self `seq` (True) ->
                                            (case (_self) of
                                             { _lhsOself | _lhsOself `seq` (True) ->
                                             (case ({-# LINE 137 "../../proxima/src/Layout/ScannerAG.ag" #-}
                                                    _childIwhitespaceMap
                                                    {-# LINE 2347 "../../proxima/src/Layout/ScannerAG.hs" #-}) of
                                              { _lhsOwhitespaceMap | _lhsOwhitespaceMap `seq` (True) ->
                                              (#  _lhsOidPCounter,_lhsOpos,_lhsOpreviousCharStyles,_lhsOscanChars,_lhsOscannedFocusEnd,_lhsOscannedFocusStart,_lhsOself,_lhsOwhitespaceMap #) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))))))))

-- cata
sem_LayoutList :: (DocNode node ,Show token ) => (LayoutList) (doc) (enr) (node) (clip) (token)  ->
                                                 (T_LayoutList) (doc) (enr) (node) (clip) (token) 
sem_LayoutList = undefined 
                 
                 
data T_LayoutList a v c k l 