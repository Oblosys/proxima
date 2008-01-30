
-- UUAGC 0.9.5 (../../proxima/src/layout/ScannerAG.ag)
module ScannerAG where
{-# LINE 11 "../../proxima/src/layout/ScannerAG.ag" #-}

import PresTypes
{-# LINE 8 "../../proxima/src/layout/ScannerAG.hs" #-}
-- Layout ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lloc                 : Maybe node
      chained attribute:
         i                    : Int
      synthesized attribute:
         self                 : SELF
   alternatives:
      alternative ArrangedP:
         visit 0:
            local self        : _
      alternative ColP:
         child id             : {IDP}
         child vRefNr         : {Int}
         child formatted      : {Formatted}
         child presentationList : LayoutList
         visit 0:
            local self        : _
      alternative EllipseP:
         child id             : {IDP}
         child w              : {Int}
         child h              : {Int}
         child lineWidth      : {Int}
         child style          : {Style}
         visit 0:
            local self        : _
      alternative EmptyP:
         child id             : {IDP}
         visit 0:
            local self        : _
      alternative FormatterP:
         child id             : {IDP}
         child presentationList : LayoutList
         visit 0:
            local self        : _
      alternative GraphP:
         child id             : {IDP}
         child d              : {Dirty}
         child w              : {Int}
         child h              : {Int}
         child edges          : {[(Int,Int)]}
         child presentationList : LayoutList
         visit 0:
            local self        : _
      alternative ImageP:
         child id             : {IDP}
         child src            : {String}
         child style          : {ImgStyle}
         visit 0:
            local self        : _
      alternative LocatorP:
         child location       : {node}
         child child          : Layout
         visit 0:
            local self        : _
      alternative OverlayP:
         child id             : {IDP}
         child presentationList : LayoutList
         visit 0:
            local self        : _
      alternative ParsingP:
         child id             : {IDP}
         child lexer          : {Lexer}
         child child          : Layout
         visit 0:
            local self        : _
      alternative PolyP:
         child id             : {IDP}
         child pointList      : {[ (Float, Float) ]}
         child lineWidth      : {Int}
         child style          : {Style}
         visit 0:
            local self        : _
      alternative RectangleP:
         child id             : {IDP}
         child w              : {Int}
         child h              : {Int}
         child lineWidth      : {Int}
         child style          : {Style}
         visit 0:
            local self        : _
      alternative RowP:
         child id             : {IDP}
         child hRefNr         : {Int}
         child presentationList : LayoutList
         visit 0:
            local self        : _
      alternative StringP:
         child id             : {IDP}
         child text           : {String}
         visit 0:
            local self        : _
      alternative StructuralP:
         child id             : {IDP}
         child child          : Layout
         visit 0:
            local self        : _
      alternative VertexP:
         child id             : {IDP}
         child i              : {Int}
         child x              : {Int}
         child y              : {Int}
         child outline        : {Outline}
         child child          : Layout
         visit 0:
            local self        : _
      alternative WithP:
         child attrRule       : {AttrRule}
         child child          : Layout
         visit 0:
            local self        : _
-}
-- cata
sem_Layout (ArrangedP )  =
    (sem_Layout_ArrangedP )
sem_Layout (ColP _id _vRefNr _formatted _presentationList )  =
    (sem_Layout_ColP _id _vRefNr _formatted (sem_LayoutList _presentationList ) )
sem_Layout (EllipseP _id _w _h _lineWidth _style )  =
    (sem_Layout_EllipseP _id _w _h _lineWidth _style )
sem_Layout (EmptyP _id )  =
    (sem_Layout_EmptyP _id )
sem_Layout (FormatterP _id _presentationList )  =
    (sem_Layout_FormatterP _id (sem_LayoutList _presentationList ) )
sem_Layout (GraphP _id _d _w _h _edges _presentationList )  =
    (sem_Layout_GraphP _id _d _w _h _edges (sem_LayoutList _presentationList ) )
sem_Layout (ImageP _id _src _style )  =
    (sem_Layout_ImageP _id _src _style )
sem_Layout (LocatorP _location _child )  =
    (sem_Layout_LocatorP _location (sem_Layout _child ) )
sem_Layout (OverlayP _id _presentationList )  =
    (sem_Layout_OverlayP _id (sem_LayoutList _presentationList ) )
sem_Layout (ParsingP _id _lexer _child )  =
    (sem_Layout_ParsingP _id _lexer (sem_Layout _child ) )
sem_Layout (PolyP _id _pointList _lineWidth _style )  =
    (sem_Layout_PolyP _id _pointList _lineWidth _style )
sem_Layout (RectangleP _id _w _h _lineWidth _style )  =
    (sem_Layout_RectangleP _id _w _h _lineWidth _style )
sem_Layout (RowP _id _hRefNr _presentationList )  =
    (sem_Layout_RowP _id _hRefNr (sem_LayoutList _presentationList ) )
sem_Layout (StringP _id _text )  =
    (sem_Layout_StringP _id _text )
sem_Layout (StructuralP _id _child )  =
    (sem_Layout_StructuralP _id (sem_Layout _child ) )
sem_Layout (VertexP _id _i _x _y _outline _child )  =
    (sem_Layout_VertexP _id _i _x _y _outline (sem_Layout _child ) )
sem_Layout (WithP _attrRule _child )  =
    (sem_Layout_WithP _attrRule (sem_Layout _child ) )
data Inh_Layout = Inh_Layout {i_Inh_Layout :: Int,lloc_Inh_Layout :: Maybe node}
data Syn_Layout = Syn_Layout {i_Syn_Layout :: Int,self_Syn_Layout :: Layout}
wrap_Layout sem (Inh_Layout _lhsIi _lhsIlloc )  =
    (let ( _lhsOi,_lhsOself) =
             (sem _lhsIi _lhsIlloc )
     in  (Syn_Layout _lhsOi _lhsOself ))
sem_Layout_ArrangedP  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  ArrangedP
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 177 "../../proxima/src/layout/ScannerAG.hs" #-}
          in  ( _lhsOi,_lhsOself)))
sem_Layout_ColP id_ vRefNr_ formatted_ presentationList_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  ColP id_ vRefNr_ formatted_ _presentationListIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _presentationListIi
                  {-# LINE 192 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _presentationListOi =
                  {-# LINE 40 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 197 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _presentationListOlloc =
                  {-# LINE 40 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIlloc
                  {-# LINE 202 "../../proxima/src/layout/ScannerAG.hs" #-}
              ( _presentationListIi,_presentationListIself) =
                  (presentationList_ _presentationListOi _presentationListOlloc )
          in  ( _lhsOi,_lhsOself)))
sem_Layout_EllipseP id_ w_ h_ lineWidth_ style_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  EllipseP id_ w_ h_ lineWidth_ style_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 219 "../../proxima/src/layout/ScannerAG.hs" #-}
          in  ( _lhsOi,_lhsOself)))
sem_Layout_EmptyP id_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  EmptyP id_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 234 "../../proxima/src/layout/ScannerAG.hs" #-}
          in  ( _lhsOi,_lhsOself)))
sem_Layout_FormatterP id_ presentationList_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  FormatterP id_ _presentationListIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _presentationListIi
                  {-# LINE 249 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _presentationListOi =
                  {-# LINE 40 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 254 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _presentationListOlloc =
                  {-# LINE 40 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIlloc
                  {-# LINE 259 "../../proxima/src/layout/ScannerAG.hs" #-}
              ( _presentationListIi,_presentationListIself) =
                  (presentationList_ _presentationListOi _presentationListOlloc )
          in  ( _lhsOi,_lhsOself)))
sem_Layout_GraphP id_ d_ w_ h_ edges_ presentationList_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  GraphP id_ d_ w_ h_ edges_ _presentationListIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _presentationListIi
                  {-# LINE 276 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _presentationListOi =
                  {-# LINE 40 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 281 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _presentationListOlloc =
                  {-# LINE 40 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIlloc
                  {-# LINE 286 "../../proxima/src/layout/ScannerAG.hs" #-}
              ( _presentationListIi,_presentationListIself) =
                  (presentationList_ _presentationListOi _presentationListOlloc )
          in  ( _lhsOi,_lhsOself)))
sem_Layout_ImageP id_ src_ style_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  ImageP id_ src_ style_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 303 "../../proxima/src/layout/ScannerAG.hs" #-}
          in  ( _lhsOi,_lhsOself)))
sem_Layout_LocatorP location_ child_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- "../../proxima/src/layout/ScannerAG.ag"(line 38, column 7)
              _childOlloc =
                  {-# LINE 38 "../../proxima/src/layout/ScannerAG.ag" #-}
                  Just location_
                  {-# LINE 312 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- self rule
              _self =
                  LocatorP location_ _childIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _childIi
                  {-# LINE 323 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _childOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 328 "../../proxima/src/layout/ScannerAG.hs" #-}
              ( _childIi,_childIself) =
                  (child_ _childOi _childOlloc )
          in  ( _lhsOi,_lhsOself)))
sem_Layout_OverlayP id_ presentationList_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  OverlayP id_ _presentationListIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _presentationListIi
                  {-# LINE 345 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _presentationListOi =
                  {-# LINE 40 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 350 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _presentationListOlloc =
                  {-# LINE 40 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIlloc
                  {-# LINE 355 "../../proxima/src/layout/ScannerAG.hs" #-}
              ( _presentationListIi,_presentationListIself) =
                  (presentationList_ _presentationListOi _presentationListOlloc )
          in  ( _lhsOi,_lhsOself)))
sem_Layout_ParsingP id_ lexer_ child_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  ParsingP id_ lexer_ _childIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _childIi
                  {-# LINE 372 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _childOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 377 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _childOlloc =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIlloc
                  {-# LINE 382 "../../proxima/src/layout/ScannerAG.hs" #-}
              ( _childIi,_childIself) =
                  (child_ _childOi _childOlloc )
          in  ( _lhsOi,_lhsOself)))
sem_Layout_PolyP id_ pointList_ lineWidth_ style_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  PolyP id_ pointList_ lineWidth_ style_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 399 "../../proxima/src/layout/ScannerAG.hs" #-}
          in  ( _lhsOi,_lhsOself)))
sem_Layout_RectangleP id_ w_ h_ lineWidth_ style_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  RectangleP id_ w_ h_ lineWidth_ style_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 414 "../../proxima/src/layout/ScannerAG.hs" #-}
          in  ( _lhsOi,_lhsOself)))
sem_Layout_RowP id_ hRefNr_ presentationList_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  RowP id_ hRefNr_ _presentationListIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _presentationListIi
                  {-# LINE 429 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _presentationListOi =
                  {-# LINE 40 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 434 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _presentationListOlloc =
                  {-# LINE 40 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIlloc
                  {-# LINE 439 "../../proxima/src/layout/ScannerAG.hs" #-}
              ( _presentationListIi,_presentationListIself) =
                  (presentationList_ _presentationListOi _presentationListOlloc )
          in  ( _lhsOi,_lhsOself)))
sem_Layout_StringP id_ text_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  StringP id_ text_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 456 "../../proxima/src/layout/ScannerAG.hs" #-}
          in  ( _lhsOi,_lhsOself)))
sem_Layout_StructuralP id_ child_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  StructuralP id_ _childIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _childIi
                  {-# LINE 471 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _childOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 476 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _childOlloc =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIlloc
                  {-# LINE 481 "../../proxima/src/layout/ScannerAG.hs" #-}
              ( _childIi,_childIself) =
                  (child_ _childOi _childOlloc )
          in  ( _lhsOi,_lhsOself)))
sem_Layout_VertexP id_ i_ x_ y_ outline_ child_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  VertexP id_ i_ x_ y_ outline_ _childIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _childIi
                  {-# LINE 498 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _childOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 503 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _childOlloc =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIlloc
                  {-# LINE 508 "../../proxima/src/layout/ScannerAG.hs" #-}
              ( _childIi,_childIself) =
                  (child_ _childOi _childOlloc )
          in  ( _lhsOi,_lhsOself)))
sem_Layout_WithP attrRule_ child_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  WithP attrRule_ _childIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _childIi
                  {-# LINE 525 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _childOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 530 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _childOlloc =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIlloc
                  {-# LINE 535 "../../proxima/src/layout/ScannerAG.hs" #-}
              ( _childIi,_childIself) =
                  (child_ _childOi _childOlloc )
          in  ( _lhsOi,_lhsOself)))
-- LayoutList --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lloc                 : Maybe node
      chained attribute:
         i                    : Int
      synthesized attribute:
         self                 : SELF
   alternatives:
      alternative Cons:
         child hd             : Layout
         child tl             : LayoutList
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
-- cata
sem_LayoutList list  =
    (Prelude.foldr sem_LayoutList_Cons sem_LayoutList_Nil (Prelude.map sem_Layout list) )
data Inh_LayoutList = Inh_LayoutList {i_Inh_LayoutList :: Int,lloc_Inh_LayoutList :: Maybe node}
data Syn_LayoutList = Syn_LayoutList {i_Syn_LayoutList :: Int,self_Syn_LayoutList :: LayoutList}
wrap_LayoutList sem (Inh_LayoutList _lhsIi _lhsIlloc )  =
    (let ( _lhsOi,_lhsOself) =
             (sem _lhsIi _lhsIlloc )
     in  (Syn_LayoutList _lhsOi _lhsOself ))
sem_LayoutList_Cons hd_ tl_  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  (:) _hdIself _tlIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOi =
                  {-# LINE 40 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _tlIi
                  {-# LINE 580 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _hdOi =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 585 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _hdOlloc =
                  {-# LINE 36 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIlloc
                  {-# LINE 590 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (chain)
              _tlOi =
                  {-# LINE 40 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _hdIi
                  {-# LINE 595 "../../proxima/src/layout/ScannerAG.hs" #-}
              -- copy rule (down)
              _tlOlloc =
                  {-# LINE 40 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIlloc
                  {-# LINE 600 "../../proxima/src/layout/ScannerAG.hs" #-}
              ( _hdIi,_hdIself) =
                  (hd_ _hdOi _hdOlloc )
              ( _tlIi,_tlIself) =
                  (tl_ _tlOi _tlOlloc )
          in  ( _lhsOi,_lhsOself)))
sem_LayoutList_Nil  =
    (\ _lhsIi
       _lhsIlloc ->
         (let -- self rule
              _self =
                  []
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOi =
                  {-# LINE 40 "../../proxima/src/layout/ScannerAG.ag" #-}
                  _lhsIi
                  {-# LINE 619 "../../proxima/src/layout/ScannerAG.hs" #-}
          in  ( _lhsOi,_lhsOself)))