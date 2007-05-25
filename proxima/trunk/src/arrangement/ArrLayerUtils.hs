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

pathAFromPathP (PathP pth ix) pres = PathA (removeWithSteps pth pres) ix
pathAFromPathP NoPathP        pres = NoPathA

pathPFromPathA (PathA pth ix) pres = PathP (addWithSteps pth pres) ix
pathPFromPathA NoPathA        pres = NoPathP


removeWithSteps []       _                     = []
removeWithSteps (p:path) (RowP id rf press)    = p:removeWithSteps path (press!!!p)
removeWithSteps (p:path) (ColP id rf press)    = p:removeWithSteps path (press!!!p)                                            
removeWithSteps (p:path) (OverlayP id press)   = p:removeWithSteps path (press!!!p)                                            
removeWithSteps (p:path) (GraphP _ _ _ _ press) = p:removeWithSteps path (press!!!p)                                            
removeWithSteps (p:path) (VertexP _ _ pres)    = p:removeWithSteps path pres                                            
removeWithSteps (p:path) (WithP ar pres)       = removeWithSteps path pres -- ignore with path step 
removeWithSteps (p:path) (StructuralP id pres) = p:removeWithSteps path pres
removeWithSteps (p:path) (ParsingP id pres)    = p:removeWithSteps path pres
removeWithSteps (p:path) (LocatorP l pres)     = p:removeWithSteps path pres
removeWithSteps pth      pr                    = debug Err ("*** Arranger.removeWithSteps: can't handle "++show pth++" "++ show pr++"***") []
-- should return NoPath

addWithSteps []       (WithP ar pres)            = 0:addWithSteps [] pres -- add step for with node 
addWithSteps []       _                          = []
addWithSteps (p:path) (RowP id rf press)         = p:addWithSteps path (press!!!p)
addWithSteps (p:path) (ColP id rf press)         = p:addWithSteps path (press!!!p)                                            
addWithSteps (p:path) (GraphP _ _ _ _ press)     = p:addWithSteps path (press!!!p)                                            
addWithSteps (p:path) (VertexP _ _ pres)         = p:addWithSteps path pres
addWithSteps (0:path) (OverlayP id (pres:press)) = 0:addWithSteps path pres
addWithSteps (path)   (WithP id pres)            = 0:addWithSteps path pres -- add step for with node 
addWithSteps (p:path) (StructuralP l pres)       = p:addWithSteps path pres
addWithSteps (p:path) (ParsingP id pres)         = p:addWithSteps path pres
addWithSteps (p:path) (LocatorP id pres)         = p:addWithSteps path pres
addWithSteps pth      pr                         = debug Err ("*** Unarranger.addWithSteps: can't handle "++show pth++" "++ show pr++"***") []

-- focus remains the same, except for With nodes, what to do about these?
-- Translation is not that difficult, just follow the path and add steps for with nodes.
-- afterwards. Formatters will make this all harder
