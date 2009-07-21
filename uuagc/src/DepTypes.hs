module DepTypes where

import CommonTypes

-------------------------------------------------------------------------------
--  Class definitions
-------------------------------------------------------------------------------

class HasAttrAndField a where
 getField :: a -> Identifier
 getAttr :: a -> Identifier

class ContainsTrace a where
 addTraceElem :: TraceElem -> a -> a
 addTrace :: Trace -> a -> a
 
-------------------------------------------------------------------------------
--  Vertices
-------------------------------------------------------------------------------

-- kan helaas niet met named fields, want dat levert een conflict op in de uniekheid vd naamgeving.
-- ERROR "CommonTypes.hs" (line 44): Repeated definition for selector "nt"

data Vertex = Local Identifier Identifier Identifier -- lhs nt, constructor, attribute
            | LHSInh Identifier Identifier Identifier -- lhs nt, constructor, attribute
            | LHSSyn Identifier Identifier Identifier -- lhs nt, constructor, attribute
            | ShRHSInh Identifier Identifier Identifier Identifier -- lhs nt, constructor, field, attribute !!rhs nt not known, equal to RHSInh!!
            | ShRHSSyn Identifier Identifier Identifier Identifier -- lhs nt, constructor, field, attribute !!rhs nt not known, equal to RHSSyn!!
            | RHSInh Identifier Identifier Identifier Identifier Identifier -- rhs nt, lhs nt, constructor, field, attribute
            | RHSSyn Identifier Identifier Identifier Identifier Identifier -- rhs nt, lhs nt, constructor, field, attribute
            | NTInh Identifier Identifier -- nt, attribute
            | NTSyn Identifier Identifier -- nt, attribute

instance Eq Vertex where
 Local       lhs1 con1       attr1 == Local       lhs2 con2       attr2 = lhs1==lhs2 && con1==con2 && attr1==attr2
 LHSInh      lhs1 con1       attr1 == LHSInh      lhs2 con2       attr2 = lhs1==lhs2 && con1==con2 && attr1==attr2
 LHSSyn      lhs1 con1       attr1 == LHSSyn      lhs2 con2       attr2 = lhs1==lhs2 && con1==con2 && attr1==attr2
 ShRHSInh    lhs1 con1 name1 attr1 == ShRHSInh    lhs2 con2 name2 attr2 = lhs1==lhs2 && con1==con2 && name1==name2 && attr1==attr2
 ShRHSSyn    lhs1 con1 name1 attr1 == ShRHSSyn    lhs2 con2 name2 attr2 = lhs1==lhs2 && con1==con2 && name1==name2 && attr1==attr2
 RHSInh rhs1 lhs1 con1 name1 attr1 == RHSInh rhs2 lhs2 con2 name2 attr2 = lhs1==lhs2 && con1==con2 && name1==name2 && attr1==attr2
 RHSSyn rhs1 lhs1 con1 name1 attr1 == RHSSyn rhs2 lhs2 con2 name2 attr2 = lhs1==lhs2 && con1==con2 && name1==name2 && attr1==attr2
 ShRHSInh    lhs1 con1 name1 attr1 == RHSInh _    lhs2 con2 name2 attr2 = lhs1==lhs2 && con1==con2 && name1==name2 && attr1==attr2
 ShRHSSyn    lhs1 con1 name1 attr1 == RHSSyn _    lhs2 con2 name2 attr2 = lhs1==lhs2 && con1==con2 && name1==name2 && attr1==attr2
 RHSInh _    lhs1 con1 name1 attr1 == ShRHSInh    lhs2 con2 name2 attr2 = lhs1==lhs2 && con1==con2 && name1==name2 && attr1==attr2
 RHSSyn _    lhs1 con1 name1 attr1 == ShRHSSyn    lhs2 con2 name2 attr2 = lhs1==lhs2 && con1==con2 && name1==name2 && attr1==attr2
 NTInh       nt1             attr1 == NTInh       nt2             attr2 = nt1==nt2 && attr1==attr2
 NTSyn       nt1             attr1 == NTSyn       nt2             attr2 = nt1==nt2 && attr1==attr2
 _                                 == _                                 = False

instance HasAttrAndField Vertex where
 getAttr (Local _ _ attr) = attr
 getAttr (LHSInh _ _ attr) = attr
 getAttr (LHSSyn _ _ attr) = attr
 getAttr (ShRHSInh _ _ _ attr) = attr
 getAttr (ShRHSSyn _ _ _ attr) = attr
 getAttr (RHSInh _ _ _ _ attr) = attr
 getAttr (RHSSyn _ _ _ _ attr) = attr
 getAttr (NTInh _ attr) = attr
 getAttr (NTSyn _ attr) = attr
 getField (Local _ _ _) = nullIdent
 getField (LHSInh _ _ _) = _LHS
 getField (LHSSyn _ _ _) = _LHS
 getField (ShRHSInh _ _ fld _) = fld
 getField (ShRHSSyn _ _ fld _) = fld
 getField (RHSInh _ _ _ fld _) = fld
 getField (RHSSyn _ _ _ fld _) = fld
 getField (NTInh _ _) = nullIdent
 getField (NTSyn _ _) = nullIdent

instance Show Vertex where
 show (Local      lhs con      attr) = show lhs ++ "." ++ show attr ++ "(L," ++ show lhs ++ "." ++ show con ++ ")"
 show (LHSInh     lhs con      attr) = show lhs ++ "." ++ show attr ++ "(I," ++ show lhs ++ "." ++ show con ++ ".lhs)"
 show (LHSSyn     lhs con      attr) = show lhs ++ "." ++ show attr ++ "(S," ++ show lhs ++ "." ++ show con ++ ".lhs)"
 show (ShRHSInh   lhs con name attr) = ""
 show (ShRHSSyn   lhs con name attr) = ""
 show (RHSInh rhs lhs con name attr) = show rhs ++ "." ++ show attr ++ "(I," ++ show lhs ++ "." ++ show con ++ "." ++ show name ++ ")"
 show (RHSSyn rhs lhs con name attr) = show rhs ++ "." ++ show attr ++ "(S," ++ show lhs ++ "." ++ show con ++ "." ++ show name ++ ")"
 show (NTInh      nt           attr) = ""
 show (NTSyn      nt           attr) = ""


-------------------------------------------------------------------------------
--  Streams, results
-------------------------------------------------------------------------------

type UseStream = (Vertex,Stream)
type Stream = [Result]
type Result = ([UsedAttr],[UsedAttr])
data UsedAttr = Loc Identifier Identifier Trace -- field name and attribute name
              | Glo Identifier Trace -- attribute name

-- lhs en rhs slaan op de lhs en rhs van een attribute equation en niet van de productie!!
data TraceElem = TE { lineNr  :: Int
                    , nt      :: NontermIdent
                    , prod    :: Identifier
                    , lhsNt   :: NontermIdent
                    , lhsFld  :: Identifier
                    , lhsAttr :: Identifier
                    , rhsFld  :: Identifier
                    , rhsAttr :: Identifier } deriving Show

type Trace = [TraceElem]

getTrace :: UsedAttr -> Trace
getTrace (Loc _ _ trace) = trace
getTrace (Glo   _ trace) = trace

toPair :: UsedAttr -> (Identifier,Trace)
toPair (Loc _ attr trace) = (attr,trace)
toPair (Glo   attr trace) = (attr,trace)

-- de Eq op Res negeert de trace, want dit is geen onderdeel van het resultaat, maar een oorzaak ervan.
-- hierdoor kan gewoon de elem functie op sets van resultaten gebruikt worden.
instance Eq UsedAttr where
 Loc fld1 attr1 _ == Loc fld2 attr2 _ = fld1==fld2 && attr1==attr2
 Glo      attr1 _ == Glo      attr2 _ = attr1==attr2
 _                == _                = False

instance Show UsedAttr where
 show (Loc fld attr trace) = "(" ++ show fld ++ "," ++ show attr ++ ")"
 show (Glo attr trace) = show attr

instance HasAttrAndField UsedAttr where
 getAttr (Loc _ attr _) = attr
 getAttr (Glo attr _) = attr
 getField (Loc fld attr _) = fld
 getField _ = nullIdent

instance ContainsTrace UsedAttr where
 addTraceElem newtre (Loc fld attr oldtr) = Loc fld attr (newtre:oldtr)
 addTraceElem newtre (Glo     attr oldtr) = Glo     attr (newtre:oldtr)
 addTrace newtr (Loc fld attr oldtr) = Loc fld attr (newtr++oldtr)
 addTrace newtr (Glo     attr oldtr) = Glo     attr (newtr++oldtr)

instance ContainsTrace Result where
 addTraceElem newcs (set,upd) = (map (addTraceElem newcs) set,map (addTraceElem newcs) upd)
 addTrace newcss (set,upd) = (map (addTrace newcss) set,map (addTrace newcss) upd)

instance ContainsTrace Stream where
 addTraceElem newcs stream = map (addTraceElem newcs) stream
 addTrace newcss stream = map (addTrace newcss) stream
