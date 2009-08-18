module Layout.TreeEditPres where

-- import IOExts

import Common.CommonTypes
import Common.CommonUtils
import Evaluation.DocTypes
import Presentation.PresTypes
import Layout.LayTypes

import Presentation.PresUtils
import Presentation.XprezLib

{-

When editing the presentation tree, take care that references do not become invalid, because then arrangerAG will
throw: *** Exception: Prelude.(!!): index too large
-}



-- NoFocus can be handled more at the top, but doing it neatly is a lot of work for a possibly temporal focus solution...
-- ...PresList functions don't handle NoFocus, but this will be caught by calling functions


-- reorder focus?

-- cut = delete
-- copy should not do anything when nofocus. 
copyTree :: (DocNode node, Show token) => FocusPres -> Layout doc node clip token -> Layout doc node clip token -> Layout doc node clip token
copyTree NoFocusP clip pres         = (clip{-, NoFocusP -})
copyTree focus clip pres            = (copyTreePres [] (orderFocusP focus) pres)

pasteTree NoPathP clip pres    = (pres, NoFocusP)
pasteTree path clip pres       = (pasteTreePres True [] path clip pres, pasteTreePresF path clip pres)
-- resulting paste focus is right/down of clips. Also selection of clip, or even left of clip are possibilities

deleteTree NoFocusP pres                 = (pres, NoFocusP)
deleteTree (FocusP NoPathP _) pres       = (pres, NoFocusP)
deleteTree (FocusP _ NoPathP) pres       = (pres, NoFocusP)
deleteTree focus pres =
  let orderedFocus@(FocusP (PathP _ sti) _) = orderFocusP focus
      pth'   = deleteTreePresF True [] [] orderedFocus pres
      focus' = FocusP (PathP pth' sti) (PathP pth' sti)
  in  (deleteTreePres True [] orderedFocus pres, focus')




normalizePresentation :: (DocNode node, Show token) => Layout doc node clip token -> FocusPres -> (Layout doc node clip token, FocusPres)
normalizePresentation pres NoFocusP = (normalizeTreePres pres, NoFocusP)
normalizePresentation pres focus = 
  let fxy   = xyFromPath (fromP focus) pres
      txy   = xyFromPath (toP focus) pres
      pres' = normalizeTreePres pres
      pf    = pathFromXY fxy pres'
      pt    = pathFromXY txy pres'
      focus' = FocusP pf pt
  in  (pres', focus')



-- focus in string is not always 0!!!!
splitRowTree NoPathP pres      = (pres, NoFocusP)
splitRowTree path@(PathP pth ix) pres = 
  let path'    = if isEditableTreePres pth pres 
                then case splitRowTreePresPth True pth pres of Left p -> PathP ([1]++p) 0
                                                               Right p -> PathP p 0
                else path
      focus' = FocusP path' path'
  in  case splitRowTreePres True [] path pres of
        Left (ps1, ps2) -> (ColP NoIDP 0 NF [ps1,ps2], focus')
        Right ps        -> (ps                   , focus')

-- Bool is for disambiguating end of one string and start of the next. True means at start of string


normalizeTreePres :: (DocNode node, Show token) => Layout doc node clip token -> Layout doc node clip token
normalizeTreePres pres = normalizePres pres

navigateLeftTreePres :: (DocNode node, Show token) => PathPres -> Layout doc node clip token -> FocusPres
navigateLeftTreePres NoPathP pres = NoFocusP
navigateLeftTreePres path pres =
  let path' = leftNavigatePath path pres
  in  FocusP path' path'
  
navigateRightTreePres :: (DocNode node, Show token) => PathPres -> Layout doc node clip token -> FocusPres
navigateRightTreePres NoPathP pres = NoFocusP
navigateRightTreePres path pres =
  let path' = rightNavigatePath path pres
  in  FocusP path' path'

{- perform a delete in a graph:
    - if the path is on or inside an edge, it is removed
    - if the path is exactly on a vertex, it is removed
    - if a path is inside a vertex, delete recurses into it (happens when a vertex contains a graph)
-}
deleteGraphPres NoPathP pres      = pres
deleteGraphPres (PathP ps _) pres = case deleteGraphPres' ps pres of
                                      Just pres' -> pres'
                                      Nothing    -> debug Err ("TreeEditPres.deleteGraphPres: delete went wrong, incorrect presentation structure") pres
                                      
deleteGraphPres' (p:ps) (RowP id rf press)         = case deleteGraphPres' ps (index "TreeEditPres.deleteGraphPres'" press p) of
                                                       Just pres -> Just $ RowP id rf $ replace "TreeEditPres.deleteGraphPres'" p press pres
                                                       Nothing    -> Nothing
deleteGraphPres' (p:ps) (ColP id rf f press)       = case deleteGraphPres' ps (index "TreeEditPres.deleteGraphPres'" press p) of
                                                       Just pres -> Just $ ColP id rf f $ replace "TreeEditPres.deleteGraphPres'" p press pres
                                                       Nothing    -> Nothing
deleteGraphPres' (p:ps) (OverlayP id d (pres:press)) = case deleteGraphPres' ps (index "TreeEditPres.deleteGraphPres'" press p) of
                                                       Just pres -> Just $ OverlayP id d $ replace "TreeEditPres.deleteGraphPres'" p press pres
                                                       Nothing    -> Nothing
deleteGraphPres' (0:ps) (WithP ar pres)            = case deleteGraphPres' ps pres of
                                                       Just pres' -> Just $ WithP ar pres'
                                                       Nothing    -> Nothing
deleteGraphPres' (0:ps) (StructuralP id pres)      = case deleteGraphPres' ps pres of
                                                       Just pres' -> Just $ StructuralP id pres'
                                                       Nothing    -> Nothing
deleteGraphPres' (0:ps) (ParsingP id pr l pres)      = case deleteGraphPres' ps pres of
                                                       Just pres' -> Just $ ParsingP id pr l pres'
                                                       Nothing    -> Nothing
deleteGraphPres' (0:ps) (LocatorP l pres)          = case deleteGraphPres' ps pres of
                                                       Just pres' -> Just $ LocatorP l pres'
                                                       Nothing    -> Nothing
deleteGraphPres' (0:ps) (TagP t pres)          = case deleteGraphPres' ps pres of
                                                       Just pres' -> Just $ TagP t pres'
                                                       Nothing    -> Nothing
deleteGraphPres' (p:ps) (GraphP id d w h es press)   = 
  if p < length press 
  then  case deleteGraphPres' ps (index "TreeEditPres.deleteGraphPres'" press p) of
          Just pres -> Just $ GraphP id Dirty w h es $ replace "TreeEditPres.deleteGraphPres'" p press pres
          Nothing   -> Just $ GraphP id Dirty w h es $ (take p press ++ drop (p+1) press)
  else let edgeNr = p - length press
       in  Just $ GraphP id Dirty w h (take edgeNr es ++ drop (edgeNr+1) es) press
deleteGraphPres' []     (VertexP id _ x y ol pres)   = Nothing
deleteGraphPres' (0:ps) (VertexP id v x y ol pres)   = case deleteGraphPres' ps pres of
                                                         Just pres' -> Just $ VertexP id v x y ol pres'
                                                         Nothing    -> Nothing
deleteGraphPres' _      pr                         = debug Err ("TreeEditPres.deleteGraphPres': can't handle "++ show pr) Nothing



addVertexPres NoPathP vertex pres      = pres
addVertexPres (PathP ps _) vertex pres = addVertexPres' ps vertex pres
                                      
addVertexPres' (p:ps) vertex (RowP id rf press)         = RowP id rf $ replace "TreeEditPres.addVertexPres'" p press (addVertexPres' ps vertex (index "TreeEditPres.addVertexPres'" press p))
addVertexPres' (p:ps) vertex (ColP id rf f press)       = ColP id rf f $ replace "TreeEditPres.addVertexPres'" p press (addVertexPres' ps vertex (index "TreeEditPres.addVertexPres'" press p))
addVertexPres' (p:ps) vertex (OverlayP id d press)      = OverlayP id d $ replace "TreeEditPres.addVertexPres'" p press (addVertexPres' ps vertex (index "TreeEditPres.addVertexPres'" press p))
addVertexPres' (0:ps) vertex (WithP ar pres)            = WithP ar (addVertexPres' ps vertex pres)
addVertexPres' (0:ps) vertex (StructuralP id pres)      = StructuralP id (addVertexPres' ps vertex pres)
addVertexPres' (0:ps) vertex (ParsingP id p l pres)         = ParsingP id p l (addVertexPres' ps vertex pres)
addVertexPres' (0:ps) vertex (LocatorP l pres)          = LocatorP l (addVertexPres' ps vertex pres)
addVertexPres' (0:ps) vertex (TagP t pres)          = TagP t (addVertexPres' ps vertex pres)
addVertexPres' []     vertex (GraphP id d w h es press) = 
  GraphP id Dirty w h es $ press ++ [vertex] -- by adding the vertex at the end, the edges are left intact
addVertexPres' (0:ps) vertex (VertexP id v x y ol pres) = VertexP id v x y ol (addVertexPres' ps vertex pres)
addVertexPres' _      vertex pr                         = debug Err ("TreeEditPres.addVertexPres': can't handle "++ show pr) pr

-- PRECONDITION: fromPs and toPs are correct paths to Vertices
addEdgePres (PathP fromPs _) (PathP toPs _) pres =
 case ( selectTree fromPs pres, selectTree toPs pres, 
        getVertexGraphPath fromPs pres, getVertexGraphPath toPs pres) of
   ( VertexP _ fromId _ _ _ _, VertexP _ toId _ _ _ _, Just graphFrom, Just graphTo) ->
     if graphFrom == graphTo then addEdgePres' graphFrom (fromId, toId) pres else pres
   _ -> pres
addEdgePres _                _              pres = pres

addEdgePres' (p:ps) edge (RowP id rf press)         = RowP id rf $ replace "TreeEditPres.addEdgePres'" p press (addEdgePres' ps edge (index "TreeEditPres.addEdgePres'" press p))
addEdgePres' (p:ps) edge (ColP id rf f press)       = ColP id rf f $ replace "TreeEditPres.addEdgePres'" p press (addEdgePres' ps edge (index "TreeEditPres.addEdgePres'" press p))
addEdgePres' (p:ps) edge (OverlayP id d (pres:press)) = OverlayP id d $ replace "TreeEditPres.addEdgePres'" p press (addEdgePres' ps edge (index "TreeEditPres.addEdgePres'" press p))
addEdgePres' (0:ps) edge (WithP ar pres)            = WithP ar (addEdgePres' ps edge pres)
addEdgePres' (0:ps) edge (StructuralP id pres)      = StructuralP id (addEdgePres' ps edge pres)
addEdgePres' (0:ps) edge (ParsingP id p l pres)         = ParsingP id p l (addEdgePres' ps edge pres)
addEdgePres' (0:ps) edge (LocatorP l pres)          = LocatorP l (addEdgePres' ps edge pres)
addEdgePres' (0:ps) edge (TagP t pres)          = TagP t (addEdgePres' ps edge pres)
addEdgePres' []     edge (GraphP id d w h es press) = GraphP id Dirty w h (edge:es) press
addEdgePres' (0:ps) edge (VertexP id v x y ol pres) = VertexP id v x y ol (addEdgePres' ps edge pres)
addEdgePres' _      edge pr                         = debug Err ("TreeEditPres.addEdgePres': can't handle "++ show pr) pr

-- PRECONDITION: path points to a vertex
-- return the path to the graph that the vertex is part of, together with its index in the graph
getVertexGraphPath path pres = getVertexGraphPath' Nothing [] path pres

getVertexGraphPath' graphPath pth [] pres                           = graphPath
getVertexGraphPath' graphPath pth (p:ps) (RowP id rf press)         = getVertexGraphPath' graphPath (pth++[p]) ps (index "TreeEditPres.getVertexGraphPath'" press p)
getVertexGraphPath' graphPath pth (p:ps) (ColP id rf _ press)       = getVertexGraphPath' graphPath (pth++[p]) ps (index "TreeEditPres.getVertexGraphPath'" press p)
getVertexGraphPath' graphPath pth (p:ps) (OverlayP id d press)      = getVertexGraphPath' graphPath (pth++[p]) ps (index "TreeEditPres.getVertexGraphPath'" press p)
getVertexGraphPath' graphPath pth (0:ps) (WithP ar pres)            = getVertexGraphPath' graphPath (pth++[0]) ps pres
getVertexGraphPath' graphPath pth (0:ps) (StructuralP id pres)      = getVertexGraphPath' graphPath (pth++[0]) ps pres
getVertexGraphPath' graphPath pth (0:ps) (ParsingP id pr l pres)         = getVertexGraphPath' graphPath (pth++[0]) ps pres
getVertexGraphPath' graphPath pth (0:ps) (LocatorP l pres)          = getVertexGraphPath' graphPath (pth++[0]) ps pres
getVertexGraphPath' graphPath pth (0:ps) (TagP _ pres)          = getVertexGraphPath' graphPath (pth++[0]) ps pres
getVertexGraphPath' graphPath pth (p:ps) (GraphP id d w h es press) = getVertexGraphPath' (Just pth) (pth++[p]) ps (index "TreeEditPres.getVertexGraphPath'" press p)
getVertexGraphPath' graphPath pth (0:ps) (VertexP id v x y ol pres) = getVertexGraphPath' graphPath (pth++[0]) ps pres
getVertexGraphPath' graphPath pth _      pr                         = debug Err ("TreeEditPres.getVertexGraphPath': can't handle "++ show pr) Nothing

getVertexIDs pth pres = 
  case selectTree pth pres of
    (GraphP _ _ _ _ _ press) -> map getVertexID press
    _                        -> debug Err "TreeEditPres.getVertexIDs called on non-graph presentation" []

getVertexID (LocatorP _ pres)     = getVertexID pres
getVertexID (TagP _ pres)     = getVertexID pres
getVertexID (StructuralP _ pres)  = getVertexID pres
getVertexID (ParsingP _ _ _ pres)     = getVertexID pres
getVertexID (WithP _ pres)        = getVertexID pres
getVertexID (VertexP _ i _ _ _ _) = i
getVertexID _                     = debug Err "TreeEditPres.getVertexID: graph presentation has incorrect structure" (-1)


moveVertexPres :: (DocNode node, Show token) => [Int] -> (Int,Int) -> Layout doc node clip token -> Layout doc node clip token                                     
moveVertexPres (p:ps) pt (RowP id rf press)         = RowP id rf $ replace "TreeEditPres.moveVertexPres" p press (moveVertexPres ps pt (index "TreeEditPres.moveVertexPres" press p))
moveVertexPres (p:ps) pt (ColP id rf f press)       = ColP id rf f $ replace "TreeEditPres.moveVertexPres" p press (moveVertexPres ps pt (index "TreeEditPres.moveVertexPres" press p))
moveVertexPres (p:ps) pt (OverlayP id d press)      = OverlayP id d $ replace "TreeEditPres.moveVertexPres" p press (moveVertexPres ps pt (index "TreeEditPres.moveVertexPres" press p))
moveVertexPres (0:ps) pt (WithP ar pres)            = WithP ar (moveVertexPres ps pt pres)
moveVertexPres (0:ps) pt (StructuralP id pres)      = StructuralP id (moveVertexPres ps pt pres)
moveVertexPres (0:ps) pt (ParsingP id pr l pres)    = ParsingP id pr l (moveVertexPres ps pt pres)
moveVertexPres (0:ps) pt (LocatorP l pres)          = LocatorP l (moveVertexPres ps pt pres)
moveVertexPres (0:ps) pt (TagP t pres)          = TagP t (moveVertexPres ps pt pres)
moveVertexPres (p:ps) pt pr@(GraphP id d w h es press)   = 
  if p < length press 
  then GraphP id Dirty w h es $ replace "TreeEditPres.moveVertexPres" p press (moveVertexPres ps pt (index "TreeEditPres.moveVertexPres" press p))
  else debug Err ("TreeEditPres.moveVertexPres: index out of bounds "++ show p ++" in "++ show pr) $ GraphP id d w h es press
moveVertexPres []     (dx,dy) (VertexP id i x y ol pres)  = VertexP id i (x+dx) (y+dy) ol pres
moveVertexPres _      pt pr                      = debug Err ("TreeEditPres.moveVertexPres: can't handle "++ show pr) pr




-- delete:
-- what to do with reference index? Ideally, it points to its original element, if possible. For
-- now, it stays the same, unless the number of elements is less, in which case it points to the last 

-- delete is complex if nested rows and columns exist, what are the exact semantics?

-- cut+paste is not always identity
-- no normalization yet! Be careful when and where because paths will be invalidated


-- some edit ops may invalidate focus, so an updated focus is computed by <edit>TreePresF
-- this function is closely related to the edit function, and it could be tupled for efficiency,
-- however, this will make the edit functions even harder to understand, so for now, they are separated.

-- precondition for all: paths are correct.
-- precondition: current node is at least partially in focus


-- [] case of list functions will never occur in paste or split due to 2nd precondition
-- however, for the others, the list might be called when it is not in focus (not entirely clear).
-- maybe split in more if branches, so precondition is met

-- what about non composite presentations (ImageP, PolyP, EmptyP etc.)?
-- they should be deleted from their parent list if selected entirely
-- current focus model probably doesn't handle this well

deleteTreePres editable p focus pr@(EmptyP id)          = pr -- ?
deleteTreePres editable p focus pr@(ImageP _ _ _)         = pr --
deleteTreePres editable p focus pr@(PolyP _ _ _ _)        = pr -- 
deleteTreePres editable p focus pr@(RectangleP _ _ _ _ _) = pr --
deleteTreePres editable p focus pr@(EllipseP _ _ _ _ _)   = pr --

deleteTreePres editable p (FocusP (PathP stp sti) (PathP enp eni)) (StringP id str) = 
  if editable then let st = if  stp < p then 0 else sti
                       en = if  enp > p then length str else eni               
                   in  StringP id (take st str ++ drop en str)
              else StringP id str
deleteTreePres editable p (FocusP st en) (RowP id rf press) = let press' = deleteTreeRow editable p 0 (FocusP st en) press
                                                     in  RowP id (if rf < length press' then rf else length press' -1) press'
deleteTreePres editable p (FocusP st en) (ColP id rf f press) = let press' = deleteTreeCol editable p 0 (FocusP st en) press
                                                     in  ColP id (if rf < length press' then rf else length press' -1) f press'
deleteTreePres editable p focus  (OverlayP id d (pres:press)) = OverlayP id d (deleteTreePres editable (p++[0]) focus pres:press)
deleteTreePres editable p focus          (WithP ar pres)    = WithP ar (deleteTreePres editable (p++[0]) focus pres)
deleteTreePres editable p focus          (StructuralP id pres)  = StructuralP id (deleteTreePres False (p++[0]) focus pres)
deleteTreePres editable p focus          (ParsingP id pr l pres)  = ParsingP id pr l (deleteTreePres True (p++[0]) focus pres)
deleteTreePres editable p focus          (LocatorP l pres)  = LocatorP l (deleteTreePres editable (p++[0]) focus pres)
deleteTreePres editable p focus          (TagP t pres)  = TagP t (deleteTreePres editable (p++[0]) focus pres)
deleteTreePres editable p focus@(FocusP (PathP focusPath _) en) (GraphP id d w h es press) =
  let editedVertexNr = head' "TreeEditPres.deleteTreePres" $ drop (length p) focusPath 
                     -- we only take into account the from path
      (left, editedVertex: right) = splitAt editedVertexNr press
      editedVertex' = deleteTreePres editable (p++[editedVertexNr]) focus editedVertex
  in  GraphP id d w h es (left ++ editedVertex' : right)
deleteTreePres editable p focus          (VertexP id vid x y ol pres) = VertexP id vid x y ol (deleteTreePres editable (p++[0]) focus pres)
deleteTreePres editable p (FocusP st en) (FormatterP id press) = let press' = deleteTreeRow editable p 0 (FocusP st en) press
                                                     in  FormatterP id press'
deleteTreePres editable p f pr = debug Err ("TreeEditPres.deleteTreePres: can't handle "++show f++" "++ show pr) $ pr

--pasteTreePres editable p path clip (GraphP id d w h es press) = GraphP id d w h es (pasteTreePresList editable p 0 path clip press)
--pasteTreePres editable p path clip (VertexP id vid x y ol pres) = VertexP id vid x y ol (pasteTreePres editable (p++[0]) path clip pres)





{-
algorithms are tricky.

    pres : press
]..                     enp < (p++[i])
[..        .. ]         stp < (p++[i]) && enp >= (p++[i+1])
  [..[  ]               stp < (p++[i+1])  -- this means pres contains start, end or both
or  [   ]..]

            .. [             

    pres : press
]..                     enp < (p++[i])
[..        .. ]         stp < (p++[i]) && enp >= p++[i+1]
[..   ]                 stp < (p++[i])  -- so enp < p++[i+1], otherwise above case applies
    [   ]..]            stp < (p++[i+1]) -- so stp >= p++[i]  

            .. [             
TODO: in the stp < p++[i] case, we have  end < p++[i+1] so no recursive call is needed

all cases of the list algorithm
outermost is end
    pres:press
[ ]               : if enp < p++[i]        then
[     ]           : else if enp < p++[i+1] then if stp < p++[i] then
    [ ]           :                             else 
[           ]     : else                        if stp < p++[i] then
    [       ]     :                             else if stp < p__[i+1] 
          [ ]     :                             else 

outermost is start
[ ]               : 
[     ]           :  
[           ]     :
    [ ]           :
    [       ]     :
          [ ]     :

TODO: find the conditions for these, see which alternative is nicest to apply to the algorithms here, and
      update the algorithms

      or maybe split the algorithm up in simpler algorithms: first go down the shared path of from and to and
      then split.

-}

-- pres is only deleted if it's completely inside the focus, and editable is true.
-- If editable is False, deletePres is called on pres to delete all editable parts 
-- (so we can't focus on them anymore :-(  )    
-- Furthermore, row insertion only takes place when editable is False (even though in 
deleteTreeRow editable p i _ [] = []
deleteTreeRow editable p i focus@(FocusP (PathP stp sti) (PathP enp eni)) (pres:press) = 
                           if   enp < (p++[i]) 
                           then (pres:press)
                           else if stp < (p++[i]) && enp >= (p++[i+1])
                           then if editable 
                                then deleteTreeRow editable p (i+1) focus press 
                                else deleteTreePres editable (p++[i]) focus pres : deleteTreeRow editable p (i+1) focus press 
                           else if stp < (p++[i]) 
                           then deleteTreePres editable (p++[i]) focus pres : deleteTreeRow editable p (i+1) focus press -- last rec. call unnecessary
                           else if stp < (p++[i+1])
                           then deleteTreePres editable (p++[i]) focus pres : deleteTreeRow editable p (i+1) focus press 
                           else pres : deleteTreeRow editable p (i+1) focus press
deleteTreeCol editable p i _ [] = []
deleteTreeCol editable p i focus@(FocusP (PathP stp sti) (PathP enp eni)) (pres:press) = 
                           if   enp < (p++[i])
                           then (pres:press)
                           else if stp < (p++[i]) && enp >= (p++[i+1])
                           then if editable 
                                then deleteTreeCol editable p (i+1) focus press
                                else deleteTreePres editable (p++[i]) focus pres : deleteTreeCol editable p (i+1) focus press
                           else if stp < (p++[i]) 
                           then deleteTreePres editable (p++[i]) focus pres : deleteTreeCol editable p (i+1) focus press  -- last rec. call unnecessary
                           else if stp < (p++[i+1])
                           then if enp < (p++[i+1]) 
                                 then deleteTreePres editable (p++[i]) focus pres : deleteTreeCol editable p (i+1) focus press -- can be just press? 
                                 else if editable
                                      then let rest = deleteTreeCol editable p (i+1) focus press
                                           in  case rest of 
                                                [] -> [deleteTreePres editable (p++[i]) focus pres]
                                                (pr:prs) -> [RowP NoIDP 0 [deleteTreePres editable (p++[i]) focus pres, pr]]++prs
                                      else deleteTreePres editable (p++[i]) focus pres : deleteTreeCol editable p (i+1) focus press
                           else pres : deleteTreeCol editable p (i+1) focus press
-- probably makes some unnecessary rows (when in a row) but this delete merges rows when deleting in a
-- column

-- big ugly hack copied from deleteTreePres definition to compute new path
-- the focus start is followed, so presentations that lie inside or after focus should not occur

-- do not try to understand this code
deleteTreePresF editable updp p focus          (EmptyP id) = debug Err "problem" updp
deleteTreePresF editable updp p (FocusP (PathP stp sti) (PathP enp eni)) (StringP id str) = updp 
deleteTreePresF editable updp p (FocusP st en) (RowP id rf press) = deleteTreeRowF editable updp p 0 (FocusP st en) press
deleteTreePresF editable updp p (FocusP st en) (ColP id rf _ press) = deleteTreeColF editable updp p 0 (FocusP st en) press
deleteTreePresF editable updp p focus  (OverlayP id d (pres:press)) = deleteTreePresF editable  (updp++[0]) (p++[0]) focus pres
deleteTreePresF editable updp p focus          (WithP ar pres)    = deleteTreePresF editable (updp++[0]) (p++[0]) focus pres
deleteTreePresF editable updp p focus          (StructuralP id pres)  = deleteTreePresF False (updp++[0]) (p++[0]) focus pres
deleteTreePresF editable updp p focus          (ParsingP id pr l pres)  = deleteTreePresF True (updp++[0]) (p++[0]) focus pres
deleteTreePresF editable updp p focus          (LocatorP l pres)  = deleteTreePresF editable (updp++[0]) (p++[0]) focus pres
deleteTreePresF editable updp p focus          (TagP _ pres)  = deleteTreePresF editable (updp++[0]) (p++[0]) focus pres
deleteTreePresF editable updp p focus@(FocusP (PathP focusPath _) en) (GraphP id d w h es press) =
  let editedVertexNr = head' "TreeEditPres.deleteTreePres" $ drop (length p) focusPath 
  in  deleteTreePresF editable  (updp++[editedVertexNr]) (p++[editedVertexNr]) focus (press!!editedVertexNr)
deleteTreePresF editable updp p focus          (VertexP _ _ _ _ _ pres) = deleteTreePresF editable (updp++[0]) (p++[0]) focus pres
deleteTreePresF editable updp p (FocusP st en) (FormatterP id press) = deleteTreeRowF editable updp p 0 (FocusP st en) press
deleteTreePresF editable updp p f pr = debug Err ("TreeEditPres.deleteTreePresF: can't handle "++show f++" "++ show pr) []

deleteTreeRowF editable updp p i _ [] = debug Err "problem" updp
deleteTreeRowF editable updp p i focus@(FocusP (PathP stp sti) (PathP enp eni)) (pres:press) = 
                           if   enp < (p++[i])
                           then debug Err "problem" updp
                           else if stp < (p++[i]) && enp >= (p++[i+1])
                           then -- editable is ignored, case does not occur
                                debug Err "problem" deleteTreeRowF editable updp p (i+1) focus press
                           else if stp < (p++[i]) 
                           then deleteTreePresF editable (updp++[i]) (p++[i]) focus pres
                           else if stp < (p++[i+1])
                           then deleteTreePresF editable (updp++[i]) (p++[i]) focus pres
                           else deleteTreeRowF editable updp p (i+1) focus press
deleteTreeColF editable updp p i _ [] = debug Err "problem" updp
{-
    pres : press
]..                     enp < (p++[i])
[..        .. ]         stp < (p++[i]) && enp >= p++[i+1]
[..   ]                 stp < (p++[i])  -- so enp < p++[i+1], otherwise above case applies
    [   ]..]            stp < (p++[i+1]) -- so stp >= p++[i]  
-}


deleteTreeColF editable updp p i focus@(FocusP (PathP stp sti) (PathP enp eni)) (pres:press) = 
                           if   enp < (p++[i])
                           then debug Err "problem" updp
                           else if stp < (p++[i]) && enp >= (p++[i+1])
                           then -- editable is ignored, case does not occur
                                debug Err "problem" deleteTreeColF editable updp p (i+1) focus press
                           else if stp < (p++[i]) 
                           then deleteTreePresF editable (updp++[i]) (p++[i]) focus pres
                           else if stp < (p++[i+1])
                           then if enp < (p++[i+1])               -- focus is inside pres
                                 then deleteTreePresF editable (updp++[i]) (p++[i]) focus pres  
                                 else  if editable
                                       then let rest = deleteTreeCol editable p (i+1) focus press -- use original deleteTreeCol to see if a row is inserted
                                            in  case rest of         -- focus starts in pres, and maybe there is an extra row 
                                                 [] -> deleteTreePresF editable (updp++[i]) (p++[i]) focus pres
                                                 (pr:prs) -> deleteTreePresF editable (updp++[i,0]) (p++[i]) focus pres
                                            else deleteTreePresF editable (updp++[i]) (p++[i]) focus pres
                           else deleteTreeColF editable updp p (i+1) focus press

 
-- for graphs, editing is only implemented for children, not the graph itself
pasteTreePres editable p (PathP stp sti) clip (StringP id str) = 
  if editable 
  then let st = if stp < p then 0 else sti
       in  case clip of
             StringP _ clipStr -> StringP id (take st str ++ clipStr ++ drop st str)
             _                 -> RowP NoIDP 0 [StringP id (take st str), clip,StringP NoIDP (drop st str)]
  else StringP id str
pasteTreePres editable p path clip (RowP id rf press) = RowP id rf (pasteTreePresList editable p 0 path clip press)
pasteTreePres editable p path clip (ColP id rf f press) = ColP id rf f (pasteTreePresList editable p 0 path clip press)
pasteTreePres editable p path clip (OverlayP id d (pres:press)) = OverlayP id d (pasteTreePres editable (p++[0]) path clip pres : press)
pasteTreePres editable p path clip (WithP ar pres) = WithP ar (pasteTreePres editable (p++[0]) path clip pres)
pasteTreePres editable p path clip (StructuralP id pres) = StructuralP  id (pasteTreePres False (p++[0]) path clip pres)
pasteTreePres editable p path clip (ParsingP id pr l pres) = ParsingP  id pr l (pasteTreePres True (p++[0]) path clip pres)
pasteTreePres editable p path clip (LocatorP l pres) = LocatorP  l (pasteTreePres editable (p++[0]) path clip pres)
pasteTreePres editable p path clip (TagP t pres) = TagP  t (pasteTreePres editable (p++[0]) path clip pres)
pasteTreePres editable p path clip (GraphP id d w h es press) = GraphP id d w h es (pasteTreePresList editable p 0 path clip press)
pasteTreePres editable p path clip (VertexP id vid x y ol pres) = VertexP id vid x y ol (pasteTreePres editable (p++[0]) path clip pres)
pasteTreePres editable p path clip (FormatterP id press) = FormatterP id (pasteTreePresList editable p 0 path clip press)
pasteTreePres editable p _ clip pr = debug Err ("TreeEditPres.pasteTreePres: can't handle "++ show pr) $ text "<ERROR>"


pasteTreePresList editable p i _ _ [] = []
pasteTreePresList editable p i path@(PathP stp sti) clip (pres:press) = 
                           if stp < p++[i]
                           then pres : press
                           else if stp < p++[i+1] 
                           then pasteTreePres editable (p++[i]) path clip pres : press
                           else pres : pasteTreePresList editable p (i+1) path clip press

-- paste keeps tree the same, except for the final leaf (always a StringP     (for now)) 
-- if the clip has type string, the target string is inserted. otherwise it is split, and the clip is inserted in between
-- the updated focus can therefore be computed nonrecursively

pasteTreePresF path@(PathP p i) clip pres = if isEditableTreePres p pres 
                                            then case clip of
                                                   StringP _ _ -> FocusP (PathP p (i+1)) (PathP p (i+1))
                                                   _ -> FocusP (PathP (p++[2]) 0) (PathP (p++[2]) 0) 
                                            else FocusP path path 
pasteTreePresF NoPathP          _    pres = NoFocusP





-- nicer to give focus of clip and let calling function decide on after paste focus left or right of clip

copyTreePres :: (DocNode node, Show token) => Path -> FocusPres -> Layout doc node clip token -> Layout doc node clip token
copyTreePres p (FocusP (PathP stp sti) (PathP enp eni)) (StringP id str) = 
  let st = if  stp < p then 0 else sti
      en = if  enp > p then length str else eni               
  in StringP id (take (en-st) (drop st str))
copyTreePres p (FocusP st en) (RowP id rf press) = let press' = copyTreePresList p 0 (FocusP st en) press
                                                     in  RowP id (if rf < length press' then rf else length press' -1) press'
copyTreePres p (FocusP st en) (ColP id rf f press) = let press' = copyTreePresList p 0 (FocusP st en) press
                                                     in  ColP id (if rf < length press' then rf else length press' -1) f press'
copyTreePres p (FocusP st en) (FormatterP id press) = let press' = copyTreePresList p 0 (FocusP st en) press
                                                      in RowP id 0 press'
copyTreePres p focus (OverlayP id d (pres:press)) = OverlayP id d (copyTreePres (p++[0]) focus pres : press) 
copyTreePres p focus          (WithP ar pres) = {- WithP ar -} (copyTreePres (p++[0]) focus pres) 
copyTreePres p focus          (StructuralP id pres) = (copyTreePres (p++[0]) focus pres)
copyTreePres p focus          (ParsingP id _ l pres) = (copyTreePres (p++[0]) focus pres)
copyTreePres p focus          (LocatorP l pres) = LocatorP l (copyTreePres (p++[0]) focus pres)
copyTreePres p focus          (TagP t pres) = TagP t (copyTreePres (p++[0]) focus pres)
copyTreePres p f pr = debug Err("TreeEditPres.copyTreePres: can't handle "++show f++" "++ show pr) $ text "<ERROR>"

copyTreePresList :: (DocNode node, Show token) => Path -> Int -> FocusPres -> [Layout doc node clip token] -> [Layout doc node clip token] 
copyTreePresList p i _ [] = []
copyTreePresList p i focus@(FocusP (PathP stp sti) (PathP enp eni)) (pres:press) = 
                           if   enp < (p++[i])-- take (length p+1) enp < p++[i]
                           then []
                           else if stp < (p++[i])--(length stp < length p+1 || take (length p+1) stp < p++[i]) && -- if start falls before a 
                                   && enp >= (p++[i+1])--(length enp >= length p+1 || take (length p+1) stp > p++[i])   -- and end after
                           then pres : copyTreePresList p (i+1) focus press
                           else if stp < (p++[i+1]) --length stp < length p+1 || take (length p+1) stp <= p++[i]  -- if start falls in or before a 
                           then copyTreePres (p++[i]) focus pres : copyTreePresList p (i+1) focus press
                           else copyTreePresList p (i+1) focus press

-- split the string and surrounding rows until innermost surrounding column
-- what about refs? Find out what desired behaviour is of splitting ref objects
-- this seems ok, rows of texts behave similar to one text and row as ref object also works correctly
-- locator can be duplicated here, is this ok?

-- editable behaviour is not trivial, current implementation sucks, too many editable checks
-- problem is that many splits are allowed, only the ones that are still split inside the non-edit are forbidden
splitRowTreePres editable p (PathP stp sti) (StringP id str) = 
  if editable
  then let st = if stp < p then 0 else sti
       in Left (StringP id (take st str), StringP NoIDP (drop st str))
  else Right (StringP id str)
splitRowTreePres editable p path            (RowP id rf press) =
  case splitRowTreePresList editable p 0 path press of
    Left (ps1, ps2) -> if editable
                       then let lenps1 = length ps1
                                (rf1, rf2) = if rf < lenps1 then (rf, 0)               -- split after or in ref
                                                            else (0, rf-lenps1+1) -- split before ref
                            in  Left (RowP id rf1 ps1, RowP NoIDP rf2 ps2)
                       else Right $ RowP id rf press
    Right ps        -> Right $ RowP id rf ps
splitRowTreePres editable p path  (ColP id rf f press) =
  case splitRowTreePresList editable p 0 path press of
    Left (ps1, ps2) -> if editable
                       then Right $ ColP id rf f (ps1++ps2)
                       else Right $ ColP id rf f press
    Right ps        -> Right $ ColP id rf f ps    
splitRowTreePres editable p path  (OverlayP id d (pres:press)) =
  case(splitRowTreePres editable (p++[0]) path pres) of
     Left (p1,p2) -> if editable
                     then Left (OverlayP id d (p1:press), OverlayP id d (p2:press))
                     else Right $ OverlayP id d (pres:press)
     Right p      -> Right $ OverlayP id d (p:press)
splitRowTreePres editable p path           (WithP ar pres) =
  case(splitRowTreePres editable (p++[0]) path pres) of
     Left (p1,p2) -> if editable
                     then Left (WithP ar p1, WithP ar p2)
                     else Right $ WithP ar pres
     Right p      -> Right $ WithP ar p
splitRowTreePres editable p path           (StructuralP id pres) =
  case(splitRowTreePres False (p++[0]) path pres) of
     Left (p1,p2) -> if editable
                     then Left (StructuralP id p1, StructuralP id p2)
                     else Right $ StructuralP id pres
     Right p      -> Right $ StructuralP id p
splitRowTreePres editable p path           (ParsingP id pr l pres) =
  case(splitRowTreePres True (p++[0]) path pres) of
     Left (p1,p2) -> if editable
                     then Left (ParsingP id pr l p1, ParsingP id pr l p2)
                     else Right $ ParsingP id pr l pres
     Right p      -> Right $ ParsingP id pr l p
splitRowTreePres editable p path           (LocatorP l pres) =
  case(splitRowTreePres editable (p++[0]) path pres) of
     Left (p1,p2) -> if editable
                     then Left (LocatorP l p1, LocatorP l p2)
                     else Right $ LocatorP l pres
     Right p      -> Right $ LocatorP l p
splitRowTreePres editable p path           (TagP t pres) =
  case(splitRowTreePres editable (p++[0]) path pres) of
     Left (p1,p2) -> if editable
                     then Left (TagP t p1, TagP t p2)
                     else Right $ TagP t pres
     Right p      -> Right $ TagP t p
splitRowTreePres editable p path            (FormatterP id press) =
  case splitRowTreePresList editable p 0 path press of
    Left (ps1, ps2) -> if editable
                       then let lenps1 = length ps1
                            in  Left (FormatterP id ps1, FormatterP NoIDP ps2)
                       else Right $ FormatterP id press
    Right ps        -> Right $ FormatterP id ps
splitRowTreePres editable p _ pr = Right $ debug Err ("TreeEditPres.splitRowTreePres: can't handle "++ show pr) $ text "<ERROR>"

splitRowTreePresList editable p i _ [] = Right []  -- This will not occur.
splitRowTreePresList editable p i path@(PathP stp sti) (pres:press) = 
                           if stp < p++[i]
                           then Right $ pres : press
                           else if stp < p++[i+1] 
                           then case splitRowTreePres editable (p++[i]) path pres of
                                  Left (p1, p2) -> if editable
                                                   then Left ([p1], p2:press)
                                                   else Right $ pres:press
                                  Right p       -> Right $ p : press
                           else case splitRowTreePresList editable p (i+1) path press of
                                  Left (ps1, ps2) -> if editable
                                                     then Left $ (pres:ps1, ps2)
                                                     else Right $ pres:press
                                  Right ps        -> Right $ pres:ps

-- for splitRow, the updated focus path is not that easily computed. the path returned is either the path 
-- in (Right presentation) or in sndPres of (Left (fstPres, sndPres))


splitRowTreePresPth editable _        (StringP id str)   = 
  if editable then Left [] else Right []
splitRowTreePresPth editable (p:path) (RowP id rf press) = 
  case splitRowTreePresPth editable path (index "TreeEditPres.splitRowTreePresPth" press p) of
    Left pth -> if editable then Left $ 0:pth else Right $ p:pth
    Right pth -> Right $ p:pth
splitRowTreePresPth editable (p:path) (ColP id rf _ press) = 
  case splitRowTreePresPth editable path (index "TreeEditPres.splitRowTreePresPth" press p) of
    Left pth -> if editable then  Right $ p+1:pth else Right $ p:pth
    Right pth -> Right $ p:pth
splitRowTreePresPth editable (0:path) (OverlayP id d (pres:press)) =  
  case splitRowTreePresPth editable path pres of
    Left pth  -> if editable then  Left $ 0:pth else Right $ 0:pth
    Right pth -> Right $ 0:pth
splitRowTreePresPth editable (p:path) (WithP ar pres) =  
  case splitRowTreePresPth editable path pres of
    Left pth  -> if editable then  Left $ 0:pth else Right $ p:pth
    Right pth -> Right $ 0:pth
splitRowTreePresPth editable (p:path) (StructuralP id pres) =
  case splitRowTreePresPth False path pres of
    Left pth  -> if editable then  Left $ 0:pth else Right $ p:pth
    Right pth -> Right $ 0:pth
splitRowTreePresPth editable (p:path) (ParsingP id pr l pres) =
  case splitRowTreePresPth True path pres of
    Left pth  -> if editable then  Left $ 0:pth else Right $ p:pth
    Right pth -> Right $ 0:pth
splitRowTreePresPth editable (p:path) (LocatorP l pres) =
  case splitRowTreePresPth editable path pres of
    Left pth  -> if editable then  Left $ 0:pth else Right $ p:pth
    Right pth -> Right $ 0:pth
splitRowTreePresPth editable (p:path) (TagP t pres) =
  case splitRowTreePresPth editable path pres of
    Left pth  -> if editable then  Left $ 0:pth else Right $ p:pth
    Right pth -> Right $ 0:pth
splitRowTreePresPth editable (p:path) (FormatterP id press) = 
  case splitRowTreePresPth editable path (index "TreeEditPres.splitRowTreePresPth" press p) of
    Left pth -> if editable then Left $ 0:pth else Right $ p:pth
    Right pth -> Right $ p:pth
splitRowTreePresPth editable pth pr = debug Err ("*** TreeEditPres.splitRowTreePresPth: can't handle "++show pth++" "++ show pr++"***") Right []
-- should return a NoFocusP here

setStylePres (FocusP from@(PathP _ _) to@(PathP _ _)) prs = let PathP fromPath fromIndex = from `min` to
                                                                PathP toPath toIndex = from `max` to
                                                            in  debug Lay ("setStylePres on "++ show fromPath ++ show toPath) $
                                                                setStylePres' fromPath fromIndex toPath toIndex [] prs
setStylePres _ prs = prs

setStylePresF (FocusP (PathP fp fi) (PathP tp ti)) = if fp == tp 
                                                     then FocusP (PathP (fp++[1,0]) 0) (PathP (tp++[1,0]) (ti-fi))
                                                     else FocusP (PathP (fp++[1,0]) 0) (PathP (tp++[0,0]) ti)
setStylePresF focus = focus  

-- todo: should only fix stuff in the Parsing subtree that the focus is in
setStylePres' fromPath fromIndex toPath toIndex rootPath prs =
  if rootPath < take (length rootPath) fromPath || rootPath > toPath
  then prs
  else 
    case prs of 
      stringP@(StringP id str)   -> if fromPath == rootPath 
                                    then if toPath == rootPath
                                         then let (left, rest) = splitAt fromIndex str 
                                                  (middle, right) = splitAt (toIndex - fromIndex) rest
                                              in  RowP NoIDP 0 [ StringP id left, WithP setStyleRed $ StringP NoIDP middle, StringP NoIDP right ]
                                         else RowP NoIDP 0 [ StringP id $ take fromIndex str, WithP setStyleRed $ StringP NoIDP $ drop fromIndex str ]
                                    else if toPath == rootPath
                                         then RowP NoIDP 0 [ WithP setStyleRed $ StringP id $ take toIndex str, StringP NoIDP $ drop toIndex str ]
                                         else WithP setStyleRed $ StringP id str
      (RowP id rf press)         -> RowP id rf [ setStylePres' fromPath fromIndex toPath toIndex (rootPath ++ [p]) pres | (p, pres) <- zip [0..] press ]
      (ColP id rf f press)       -> ColP id rf f [ setStylePres' fromPath fromIndex toPath toIndex (rootPath ++ [p]) pres | (p, pres) <- zip [0..] press ]
      (OverlayP d id press)      -> OverlayP d id [ setStylePres' fromPath fromIndex toPath toIndex (rootPath ++ [p]) pres | (p, pres) <- zip [0..] press ]
      (FormatterP  id press)     -> FormatterP id [ setStylePres' fromPath fromIndex toPath toIndex (rootPath ++ [p]) pres | (p, pres) <- zip [0..] press ]
      (GraphP id d w h es press) -> GraphP id d w h es [ setStylePres' fromPath fromIndex toPath toIndex (rootPath ++ [p]) pres | (p, pres) <- zip [0..] press ]
      (WithP ar pres)            -> WithP ar $ setStylePres' fromPath fromIndex toPath toIndex (rootPath ++ [0]) pres
      (StructuralP id pres)      -> StructuralP id $ setStylePres' fromPath fromIndex toPath toIndex (rootPath ++ [0]) pres
      (ParsingP id p l pres)     -> ParsingP id p l $ setStylePres' fromPath fromIndex toPath toIndex (rootPath ++ [0]) pres
      (LocatorP loc pres)        -> LocatorP loc $ setStylePres' fromPath fromIndex toPath toIndex (rootPath ++ [0]) pres
      (TagP ts pres)             -> TagP ts $ setStylePres' fromPath fromIndex toPath toIndex (rootPath ++ [0]) pres
      pres                       -> WithP setStyleRed pres
        {-
findLay (ColP id rf f press)      = 
findLay (OverlayP d id press)     = 
findLay (FormatterP  id press)    = 
findLay (GraphP id _ _ _ _ press) =
findLay (VertexP _ _ x y _  pres) =
findLay (WithP ar pres)           =
findLay (StructuralP id pres)     =
findLay (ParsingP id p l pres)    =
findLay (LocatorP loc pres)       =
findLay (TagP loc pres)           =

findLay (EmptyP id)               = 
findLay (ImageP id str _)         = 
findLay (PolyP id _ _ _)          = 
findLay (RectangleP id _ _ _ _)   = 
findLay (EllipseP id _ _ _ _)     = 

    (RowP id rf press)        = 
    (VertexP _ _ x y _  pres) -> findLay' str fromPath fromIndex toPath toIndex (rootPath ++ [0]) pres
-}
setStyleRed (inh,syn) = (inh {textColor = red}, syn)








-- BUGS: during debug, delete sometimes removes too much 
{-

3
--  +
4 

from -|- to |+      sometimes removes the + as well

**

backspace
1
--       
2|3

lets focus disappear
**

delete

  1 
 ---
 |3
  -
  4|

also gets rid of the - (or ---?) (but not of the Structural nodes)
**
delete does not delete structurals in parsing nodes of structurals >(1/2/3)< leaves (//)
Can we just put in a Str ""?
This is a problem in the algorithm of delete. because a child cannot be deleted, it must be
deleted from a parent, but that's only for row or col. Delete must be set up from the beginning
in better way.
Also Parsing/structure: div children of div should be parsing somewhere. Who is responsible 
for telling what's parsing and what not, parent or child? Or both?



-}
