module ArrLayerUtils ( module LayUtils
                     , module ArrUtils
                     , module ArrLayerUtils     ) where


import CommonTypes
import ArrLayerTypes

import CommonUtils
import LayUtils
import ArrUtils

idAFromP :: IDP -> IDA
idAFromP NoIDP    = NoIDA
idAFromP (IDP id) = IDA id

focusAFromFocusP (FocusP f t) pres = focusA (pathAFromPathP f pres) (pathAFromPathP t pres)
focusAFromFocusP NoFocusP     pres = NoFocusA

focusPFromFocusA (FocusA f t) pres = focusP (pathPFromPathA f pres) (pathPFromPathA t pres)
focusPFromFocusA NoFocusA     pres = NoFocusP

pathAFromPathP (PathP pth ix) pres = PathA (pathAFromPathP' pth pres) ix
pathAFromPathP NoPathP        pres = NoPathA

pathPFromPathA (PathA pth ix) pres = PathP (pathPFromPathA' pth pres) ix
pathPFromPathA NoPathA        pres = NoPathP


pathAFromPathP' []       _                      = []
pathAFromPathP' (p:path) (RowP id rf press)     = p:pathAFromPathP' path (press!!!p)
pathAFromPathP' (p:path) (ColP id rf press)     = p:pathAFromPathP' path (press!!!p)                                            
pathAFromPathP' (p:path) (OverlayP id press)    = p:pathAFromPathP' path (press!!!p)                                            
pathAFromPathP' (p:path) (GraphP _ _ _ _ press) = p:pathAFromPathP' path (press!!!p) -- edges are put after vertices, so selection is ok
pathAFromPathP' (p:path) (VertexP _ _ _ _ pres) = p:pathAFromPathP' path pres                                            
pathAFromPathP' (p:path) (WithP ar pres)        = pathAFromPathP' path pres -- ignore with path step 
pathAFromPathP' (p:path) (StructuralP id pres)  = p:pathAFromPathP' path pres
pathAFromPathP' (p:path) (ParsingP id pres)     = p:pathAFromPathP' path pres
pathAFromPathP' (p:path) (LocatorP l pres)      = p:pathAFromPathP' path pres
pathAFromPathP' pth      pr                     = debug Err ("*** Arranger.pathAFromPathP: can't handle "++show pth++" "++ show pr++"***") []
-- should return NoPath

pathPFromPathA' []       (WithP ar pres)            = 0:pathPFromPathA' [] pres -- add step for with node 
pathPFromPathA' []       _                          = []
pathPFromPathA' (p:path) (RowP id rf press)         = p:pathPFromPathA' path (press!!!p)
pathPFromPathA' (p:path) (ColP id rf press)         = p:pathPFromPathA' path (press!!!p)                                            
pathPFromPathA' (p:path) (GraphP _ _ _ _ press)     = if p >= length press then [p] -- selection is on edge
                                                      else p:pathPFromPathA' path (press!!!p)                                            
pathPFromPathA' (p:path) (VertexP _ _ _ _ pres)     = p:pathPFromPathA' path pres
pathPFromPathA' (0:path) (OverlayP id (pres:press)) = 0:pathPFromPathA' path pres
pathPFromPathA' (path)   (WithP id pres)            = 0:pathPFromPathA' path pres -- add step for with node 
pathPFromPathA' (p:path) (StructuralP l pres)       = p:pathPFromPathA' path pres
pathPFromPathA' (p:path) (ParsingP id pres)         = p:pathPFromPathA' path pres
pathPFromPathA' (p:path) (LocatorP id pres)         = p:pathPFromPathA' path pres
pathPFromPathA' pth      pr                         = debug Err ("*** Unarranger.pathPFromPathA': can't handle "++show pth++" "++ show pr++"***") []

-- focus remains the same, except for With nodes, what to do about these?
-- Translation is not that difficult, just follow the path and add steps for with nodes.
-- afterwards. Formatters will make this all harder
