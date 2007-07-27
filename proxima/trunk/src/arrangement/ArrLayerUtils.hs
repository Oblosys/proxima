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

focusAFromFocusP (FocusP f t) arr pres = focusA (pathAFromPathP f arr pres) (pathAFromPathP t arr pres)
focusAFromFocusP NoFocusP     arr pres = NoFocusA

focusPFromFocusA (FocusA f t) arr pres = focusP (pathPFromPathA f arr pres) (pathPFromPathA t arr pres)
focusPFromFocusA NoFocusA     arr pres = NoFocusP

pathAFromPathP (PathP pth ix) arr pres = PathA (pathAFromPathP' arr pres pth) ix
pathAFromPathP NoPathP        arr pres = NoPathA

pathPFromPathA (PathA pth ix) arr pres = PathP (pathPFromPathA' arr pres pth) ix
pathPFromPathA NoPathA        arr pres = NoPathP



-- Both levels are necessary because of formatters.
pathAFromPathP' _                               _                        []       = []
pathAFromPathP' (RowA _ _ _ _ _ _ _ _ arrs)     (RowP _ _ press)         (p:path) = p:pathAFromPathP' (index "ArrLayerUtils.pathAFromPathP'" arrs p) (index "ArrLayerUtils.pathAFromPathP'" press p) path
pathAFromPathP' (ColA _ _ _ _ _ _ _ _ arrs)     (ColP _ _ press)         (p:path) = p:pathAFromPathP' (index "ArrLayerUtils.pathAFromPathP'" arrs p) (index "ArrLayerUtils.pathAFromPathP'" press p) path                                            
pathAFromPathP' (OverlayA _ _ _ _ _ _ _ _ arrs) (OverlayP _ press)       (p:path) = p:pathAFromPathP' (index "ArrLayerUtils.pathAFromPathP'" arrs p) (index "ArrLayerUtils.pathAFromPathP'" press p) path
pathAFromPathP' (GraphA _ _ _ _ _ _ _ _ _ arrs) (GraphP _ _ _ _ _ press) (p:path) = p:pathAFromPathP' (index "ArrLayerUtils.pathAFromPathP'" arrs p) (index "ArrLayerUtils.pathAFromPathP'" press p) path -- edges are put after vertices, so selection is ok
pathAFromPathP' (VertexA _ _ _ _ _ _ _ _ _ arr) (VertexP _ _ _ _ _ pres) (0:path) = 0:pathAFromPathP' arr pres path
pathAFromPathP' arr                             (WithP ar pres)          (0:path) = pathAFromPathP' arr pres path -- ignore with path step 
pathAFromPathP' (StructuralA _ arr)             (StructuralP _ pres)     (0:path) = 0:pathAFromPathP' arr pres path
pathAFromPathP' (ParsingA _ arr)                (ParsingP _ pres)        (0:path) = 0:pathAFromPathP' arr pres path
pathAFromPathP' (LocatorA _ arr)                (LocatorP _ pres)        (0:path) = 0:pathAFromPathP' arr pres path
pathAFromPathP' arr                             (FormatterP _ press)     path     = pathAFromPathPFormatter arr press path
pathAFromPathP' ar                              pr                       pth      = debug Err ("*** ArrLayerUtils.pathAFromPathP: can't handle "++show pth++" "++ show pr++"***") []
-- should return NoPath

-- p is the index in the formatter and is mapped on an index in the column of rows
pathAFromPathPFormatter (ColA _ _ _ _ _ _ _ _ rowArrs) press (p:path) = debug Err ("\n\n\nformatter index is:"++show p) $
  let (arr, (cIx, rIx)) = colRowIx rowArrs 0 p
  in  cIx : rIx : pathAFromPathP' arr (index "ArrLayerUtils.pathAFromPathP'" press p) path
pathAFromPathPFormatter _                           press path = debug Err ("ArrLayerUtils.pathAFromPathPFormatter: unfolded formatter has wrong stucture") []


colRowIx (RowA _ _ _ _ _ _ _ _ arrs : rowArrs) cIx ix =
  if ix < length arrs 
  then showDebug' Err "\n\n\n\nWe are onnn: " $ (index "ArrLayerUtils.colRowIx" arrs ix, (cIx, ix))
  else colRowIx rowArrs (cIx+1) (ix-length arrs)
colRowIx _ _ _ =  debug Err ("ArrLayerUtils.colRowIx: unfolded formatter has wrong stucture") (EmptyA NoIDA 0 0 0 0 0 0, (0,0))


-- Both levels are necessary because of formatters.
pathPFromPathA' _                               _                        []       = []
pathPFromPathA' (RowA _ _ _ _ _ _ _ _ arrs)     (RowP _ _ press)         (p:path) = p:pathPFromPathA' (index "ArrLayerUtils.pathPFromPathA'" arrs p) (index "ArrLayerUtils.pathPFromPathA'" press p) path
pathPFromPathA' (ColA _ _ _ _ _ _ _ _ arrs)     (ColP _ _ press)         (p:path) = p:pathPFromPathA' (index "ArrLayerUtils.pathPFromPathA'" arrs p) (index "ArrLayerUtils.pathPFromPathA'" press p) path                                       
pathPFromPathA' (OverlayA _ _ _ _ _ _ _ _ arrs) (OverlayP _ press)       (p:path) = p:pathPFromPathA' (index "ArrLayerUtils.pathPFromPathA'" arrs p) (index "ArrLayerUtils.pathPFromPathA'" press p) path
pathPFromPathA' (GraphA _ _ _ _ _ _ _ _ _ arrs) (GraphP _ _ _ _ _ press) (p:path) = if p >= length press then [p] -- selection is on edge
                                                                                    else p:pathPFromPathA' (index "ArrLayerUtils.pathPFromPathA'" arrs p) (index "ArrLayerUtils.pathPFromPathA'" press p) path                                            
pathPFromPathA' (VertexA _ _ _ _ _ _ _ _ _ arr) (VertexP _ _ _ _ _ pres) (0:path) = 0:pathPFromPathA' arr pres path
pathPFromPathA' arr                             (WithP _ pres)           (path)   = 0:pathPFromPathA' arr pres path -- add step for with node 
pathPFromPathA' (StructuralA _ arr)             (StructuralP _ pres)     (0:path) = 0:pathPFromPathA' arr pres path
pathPFromPathA' (ParsingA _ arr)                (ParsingP _ pres)        (0:path) = 0:pathPFromPathA' arr pres path
pathPFromPathA' (LocatorA _ arr)                (LocatorP _ pres)        (0:path) = 0:pathPFromPathA' arr pres path
pathPFromPathA' arr                             (FormatterP _ press)     path     = pathPFromPathAFormatter arr press path
pathPFromPathA' ar                              pr                       pth      = debug Err ("*** ArrLayerUtils.pathPFromPathA': can't handle "++show pth++" "++ show pr++"***") []

-- p is the index in the formatter and is mapped on an index in the column of rows
pathPFromPathAFormatter (ColA _ _ _ _ _ _ _ _ rowArrs) press (cIx:rIx:path) = debug Err ("\n\n\ncol row index is:"++show (cIx,rIx)) $
  let (arr, fIx) = formatterIx rowArrs 0 (cIx, rIx)
  in  fIx : pathAFromPathP' arr (index "ArrLayerUtils.pathPFromPathA'" press fIx) path
pathPFromPathAFormatter _                           press path = debug Err ("ArrLayerUtils.pathPFromPathAFormatter: unfolded formatter has wrong stucture") []


formatterIx :: Show node => [Arrangement node] -> Int -> (Int, Int) -> (Arrangement node, Int)
formatterIx (RowA _ _ _ _ _ _ _ _ arrs : rowArrs) ix (cIx, rIx) =
  if cIx == 0 
  then showDebug' Err "\n\n\n\nWe are on: " $ (index "ArrLayerUtils.colRowIx" arrs rIx, ix+rIx)
  else formatterIx rowArrs (ix+length arrs) (cIx-1,rIx) 
formatterIx _ _ _ =  debug Err ("ArrLayerUtils.formatterIx: unfolded formatter has wrong stucture") (EmptyA NoIDA 0 0 0 0 0 0, 0)

