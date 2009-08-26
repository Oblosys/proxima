module Arrangement.ArrLayerUtils ( module Layout.LayUtils
                     , module Arrangement.ArrUtils
                     , module Arrangement.ArrLayerUtils     ) where


import Common.CommonTypes
import Arrangement.ArrLayerTypes

import Common.CommonUtils
import Layout.LayUtils
import Arrangement.ArrUtils

idAFromP :: IDP -> IDA
idAFromP NoIDP    = NoIDA
idAFromP (IDP id) = IDA id

-- For both conversions, both the arrangement and the presentation is needed:
--     the arrangement for information on the distribution of elements over rows
-- and the prersentation for taking into account With nodes.

focusAFromFocusP (FocusP f t) arr pres = focusA (pathAFromPathP f arr pres) (pathAFromPathP t arr pres)
focusAFromFocusP NoFocusP     arr pres = NoFocusA

focusPFromFocusA (FocusA f t) arr pres = focusP (pathPFromPathA f arr pres) (pathPFromPathA t arr pres)
focusPFromFocusA NoFocusA     arr pres = NoFocusP

pathAFromPathP (PathP pth ix) arr pres = PathA (pathAFromPathP' arr pres pth) ix
pathAFromPathP NoPathP        arr pres = NoPathA

pathPFromPathA (PathA pth ix) arr pres = PathP (pathPFromPathA' arr pres pth) ix
pathPFromPathA NoPathA        arr pres = NoPathP


pathAFromPathP' _                               _                        []       = []
pathAFromPathP' (RowA _ _ _ _ _ _ _ _ arrs)     (RowP _ _ press)         (p:path) = p:pathAFromPathP' (index "ArrLayerUtils.pathAFromPathP'" arrs p) (index "ArrLayerUtils.pathAFromPathP'" press p) path
pathAFromPathP' (ColA _ _ _ _ _ _ _ _ _ arrs)   (ColP _ _ _ press)       (p:path) = p:pathAFromPathP' (index "ArrLayerUtils.pathAFromPathP'" arrs p) (index "ArrLayerUtils.pathAFromPathP'" press p) path                                            
pathAFromPathP' (OverlayA _ _ _ _ _ _ _ _ _ arrs) (OverlayP _ _ press)     (p:path) = p:pathAFromPathP' (index "ArrLayerUtils.pathAFromPathP'" arrs p) (index "ArrLayerUtils.pathAFromPathP'" press p) path
pathAFromPathP' (GraphA _ _ _ _ _ _ _ _ _ arrs) (GraphP _ _ _ _ _ press) (p:path) = p:pathAFromPathP' (index "ArrLayerUtils.pathAFromPathP'" arrs p) (index "ArrLayerUtils.pathAFromPathP'" press p) path -- edges are put after vertices, so selection is ok
pathAFromPathP' (VertexA _ _ _ _ _ _ _ _ _ arr) (VertexP _ _ _ _ _ pres) (0:path) = 0:pathAFromPathP' arr pres path
pathAFromPathP' arr                             (WithP ar pres)          (0:path) = pathAFromPathP' arr pres path -- ignore with path step 
pathAFromPathP' (StructuralA _ arr)             (StructuralP _ pres)     (0:path) = 0:pathAFromPathP' arr pres path
pathAFromPathP' (ParsingA _ arr)                (ParsingP _ _ _ pres)      (0:path) = 0:pathAFromPathP' arr pres path
pathAFromPathP' (LocatorA _ arr)                (LocatorP _ pres)        (0:path) = 0:pathAFromPathP' arr pres path
pathAFromPathP' (TagA _ arr)                    (TagP _ pres)        (0:path) = 0:pathAFromPathP' arr pres path
pathAFromPathP' arr                             (FormatterP _ press)     path     = pathAFromPathPFormatter arr press path
pathAFromPathP' ar                              pr                       pth      = debug Err ("*** ArrLayerUtils.pathAFromPathP: can't handle "++show pth++" "++ shallowShowPres pr++"***") []

-- p is the index in the formatter and is mapped onto an index in the column of rows
pathAFromPathPFormatter (ColA _ _ _ _ _ _ _ _ (F nrOfRowEltss) rowArrs) press (p:path) =
  let subtractedIndices = filter (>=0) $ scanl (-) p nrOfRowEltss
      (cIx, rIx) = (length subtractedIndices -1 , last subtractedIndices) -- safe, since subtractedIndices always starts with p
      arr = case index "ArrLayerUtils.pathAFromPathPFormatter" rowArrs cIx of
              (RowA _ _ _ _ _ _ _ _ arrs) -> index "ArrLayerUtils.pathAFromPathPFormatter, arr index" arrs rIx
              _                           -> debug Err ("ArrLayerUtils.pathAFromPathPFormatter: unfolded formatter has wrong stucture, no row") (EmptyA NoIDA 0 0 0 0 0 0 transparent)
  in  cIx : rIx : pathAFromPathP' arr (index "ArrLayerUtils.pathAFromPathPFormatter, pres index" press p) path
pathAFromPathPFormatter _                           press path = debug Err ("ArrLayerUtils.pathAFromPathPFormatter: unfolded formatter has wrong stucture, no column") []

pathPFromPathA' :: (Show node, Show token) => Arrangement node -> Layout doc enr node clip token -> Path -> Path
pathPFromPathA' arr                             (WithP _ pres)           path     = 0:pathPFromPathA' arr pres path -- add step for with node 
pathPFromPathA' _                               _                        []       = []
pathPFromPathA' (RowA _ _ _ _ _ _ _ _ arrs)     (RowP _ _ press)         (p:path) = p:pathPFromPathA' (index "ArrLayerUtils.pathPFromPathA'" arrs p) (index "ArrLayerUtils.pathPFromPathA'" press p) path
pathPFromPathA' (ColA _ _ _ _ _ _ _ _ _ arrs)   (ColP _ _ _ press)       (p:path) = p:pathPFromPathA' (index "ArrLayerUtils.pathPFromPathA'" arrs p) (index "ArrLayerUtils.pathPFromPathA'" press p) path                                       
pathPFromPathA' (OverlayA _ _ _ _ _ _ _ _ _ arrs) (OverlayP _ _ press)     (p:path) = p:pathPFromPathA' (index "ArrLayerUtils.pathPFromPathA'" arrs p) (index "ArrLayerUtils.pathPFromPathA'" press p) path
pathPFromPathA' (GraphA _ _ _ _ _ _ _ _ _ arrs) (GraphP _ _ _ _ _ press) (p:path) = if p >= length press then [p] -- selection is on edge
                                                                                    else p:pathPFromPathA' (index "ArrLayerUtils.pathPFromPathA'" arrs p) (index "ArrLayerUtils.pathPFromPathA'" press p) path                                            
pathPFromPathA' (VertexA _ _ _ _ _ _ _ _ _ arr) (VertexP _ _ _ _ _ pres) (0:path) = 0:pathPFromPathA' arr pres path
pathPFromPathA' (StructuralA _ arr)             (StructuralP _ pres)     (0:path) = 0:pathPFromPathA' arr pres path
pathPFromPathA' (ParsingA _ arr)                (ParsingP _ _ _ pres)      (0:path) = 0:pathPFromPathA' arr pres path
pathPFromPathA' (LocatorA _ arr)                (LocatorP _ pres)        (0:path) = 0:pathPFromPathA' arr pres path
pathPFromPathA' (TagA _ arr)                    (TagP _ pres)        (0:path) = 0:pathPFromPathA' arr pres path
pathPFromPathA' arr                             (FormatterP _ press)     path     = pathPFromPathAFormatter arr press path
pathPFromPathA' ar                              pr                       pth      = debug Err ("*** ArrLayerUtils.pathPFromPathA': can't handle "++show pth++" "++ shallowShowPres pr++"***") []

-- (cIx,rIx) are the indices in the column of rows and are mapped onto an index in the formatter
pathPFromPathAFormatter (ColA _ _ _ _ _ _ _ _ (F nrOfRowEltss) rowArrs) press (cIx:rIx:path) = -- debug Arr ("\n\n\ncol row index is:"++show (cIx,rIx)) $
  let fIx = sum (take cIx nrOfRowEltss) + rIx
      arr = case index "ArrLayerUtils.pathPFromPathAFormatter" rowArrs cIx of
              (RowA _ _ _ _ _ _ _ _ arrs) -> index "ArrLayerUtils.pathPFromPathAFormatter, arr index" arrs rIx
              _                           -> debug Err ("ArrLayerUtils.pathPFromPathAFormatter: unfolded formatter has wrong stucture, no row") (EmptyA NoIDA 0 0 0 0 0 0 transparent)
  in  fIx : pathPFromPathA' arr (index "ArrLayerUtils.pathPFromPathAFormatter, pres index" press fIx) path
pathPFromPathAFormatter _                           press path = debug Err ("ArrLayerUtils.pathPFromPathAFormatter: unfolded formatter has wrong stucture, no column") []


-- refs are not passed, since they are not used in rendering.
-- What happens if an empty is used as oldArrangement? Can this happen? And do we need the refs then?
arrangeWhenFocusedOrViewed rootPath focus absx absy x y w h hrf vrf viewedArea idA arrangement =
  let (fromPath,toPath) = case focus of
                    FocusP (PathP f _) (PathP t _) -> if f <= t then (f,t) else (t,f)
                    _                              -> ([-1],[-1])
  in  if rootPath >= take (length rootPath) fromPath && rootPath <= toPath ||
         overlap ((absx,absy),(w,h)) viewedArea then 
--          (if rootPath >= take (length rootPath) fromPath && rootPath <= toPath then debug Err ("arrangeinfocus\n"++ show rootPath ++"\n"++ show focus++shallowShowArr arrangement) else id) 
          arrangement else
          --arrangement 
          unarrangedA x y w h hrf vrf
