module TreeEditPres where

-- import IOExts

import CommonTypes
import DocTypes
import PresTypes

import PresUtils
import XprezLib

{-

When editing the presentation tree, take care that references do not become invalid, because then arrangerAG will
throw: *** Exception: Prelude.(!!): index too large
-}



-- NoFocus can be handled more at the top, but doing it neatly is a lot of work for a possibly temporal focus solution...
-- ...PresList functions don't handle NoFocus, but this will be caught by calling functions


-- reorder focus?

-- cut = delete
-- copy should not do anything when nofocus. 
copyTree :: FocusPres -> Presentation doc node clip -> Presentation doc node clip -> Presentation doc node clip
copyTree NoFocusP clip pres         = (clip{-, NoFocusP -})
copyTree focus clip pres            = (copyTreePres [] (orderFocusP focus) pres)

pasteTree NoPathP clip pres    = (pres, NoFocusP)
pasteTree path clip pres       = (pasteTreePres True [] path clip pres, pasteTreePresF path pres)
-- resulting paste focus is right/down of clips. Also selection of clip, or even left of clip are possibilities

deleteTree NoFocusP pres                 = (pres, NoFocusP)
deleteTree (FocusP NoPathP _) pres       = (pres, NoFocusP)
deleteTree (FocusP _ NoPathP) pres       = (pres, NoFocusP)
deleteTree focus pres =
  let orderedFocus@(FocusP (PathP _ sti) _) = orderFocusP focus
      pth'   = deleteTreePresF True [] [] orderedFocus pres
      focus' = FocusP (PathP pth' sti) (PathP pth' sti)
  in  debug Prs ("delfocus"++show focus') (deleteTreePres True [] orderedFocus pres, focus')





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
        Left (ps1, ps2) -> (ColP NoIDP 0 [ps1,ps2], focus')
        Right ps        -> (ps                   , focus')

-- Bool is for disambiguating end of one string and start of the next. True means at start of string


normalizeTreePres :: Presentation doc node clip -> Presentation doc node clip
normalizeTreePres pres = normalizePres pres

navigateLeftTreePres :: PathPres -> Presentation doc node clip -> FocusPres
navigateLeftTreePres NoPathP pres = NoFocusP
navigateLeftTreePres path pres =
  let path' = leftNavigatePath path pres
  in  FocusP path' path'
  
navigateRightTreePres :: PathPres -> Presentation doc node clip -> FocusPres
navigateRightTreePres NoPathP pres = NoFocusP
navigateRightTreePres path pres =
  let path' = rightNavigatePath path pres
  in  FocusP path' path'


-- what to do with reference index? Ideally, it points to its original element, if possible. For
-- now, it stays the same, unless the number of elements is less, in which case it points to the last 

-- delete is complex if nested rows and columns exist, what is the exact semantics?

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
deleteTreePres editable p focus pr@(ImageP _ _)         = pr --
deleteTreePres editable p focus pr@(PolyP _ _ _)        = pr -- 
deleteTreePres editable p focus pr@(RectangleP _ _ _ _) = pr --
deleteTreePres editable p focus pr@(EllipseP _ _ _ _)   = pr --

deleteTreePres editable p (FocusP (PathP stp sti) (PathP enp eni)) (StringP id str) = 
  if editable then let st = if  stp < p then 0 else sti
                       en = if  enp > p then length str else eni               
                   in  StringP id (take st str ++ drop en str)
              else StringP id str
deleteTreePres editable p (FocusP st en) (RowP id rf press) = let press' = deleteTreeRow editable p 0 (FocusP st en) press
                                                     in  RowP id (if rf < length press' then rf else length press' -1) press'
deleteTreePres editable p (FocusP st en) (ColP id rf press) = let press' = deleteTreeCol editable p 0 (FocusP st en) press
                                                     in  ColP id (if rf < length press' then rf else length press' -1) press'
deleteTreePres editable p focus  (OverlayP id (pres:press)) = OverlayP id (deleteTreePres editable (p++[0]) focus pres:press)
deleteTreePres editable p focus          (WithP ar pres)    = WithP ar (deleteTreePres editable (p++[0]) focus pres)
deleteTreePres editable p focus          (StructuralP id pres)  = StructuralP id (deleteTreePres False (p++[0]) focus pres)
deleteTreePres editable p focus          (ParsingP id pres)  = ParsingP id (deleteTreePres True (p++[0]) focus pres)
deleteTreePres editable p focus          (LocatorP l pres)  = LocatorP l (deleteTreePres editable (p++[0]) focus pres)
deleteTreePres editable p f pr = debug Err ("TreeEditPres.deleteTreePres: can't handle "++show f++" "++ show pr) $ pr


deleteGraphPres NoPathP pres      = pres
deleteGraphPres (PathP ps _) pres = case deleteGraphPres' ps pres of
                                      Just pres' -> pres'
                                      Nothing    -> debug Err ("TreeEditPres.deleteGraphPres: delete went wrong, incorrect presentation structure") pres
                                      
deleteGraphPres' (p:ps) (RowP id rf press)         = case deleteGraphPres' ps (press!!!p) of
                                                       Just pres -> Just $ RowP id rf $ replace p press pres
                                                       Nothing    -> Nothing
deleteGraphPres' (p:ps) (ColP id rf press)         = case deleteGraphPres' ps (press!!!p) of
                                                       Just pres -> Just $ ColP id rf $ replace p press pres
                                                       Nothing    -> Nothing
deleteGraphPres' (p:ps) (OverlayP id (pres:press)) = case deleteGraphPres' ps (press!!!p) of
                                                       Just pres -> Just $ OverlayP id $ replace p press pres
                                                       Nothing    -> Nothing
deleteGraphPres' (0:ps) (WithP ar pres)            = case deleteGraphPres' ps pres of
                                                       Just pres' -> Just $ WithP ar pres'
                                                       Nothing    -> Nothing
deleteGraphPres' (0:ps) (StructuralP id pres)      = case deleteGraphPres' ps pres of
                                                       Just pres' -> Just $ StructuralP id pres'
                                                       Nothing    -> Nothing
deleteGraphPres' (0:ps) (LocatorP l pres)          = case deleteGraphPres' ps pres of
                                                       Just pres' -> Just $ LocatorP l pres'
                                                       Nothing    -> Nothing
deleteGraphPres' (p:ps) (GraphP id w h es press)   = 
  if p < length press 
  then  case deleteGraphPres' ps (press!!!p) of
          Just pres -> Just $ GraphP id w h es $ replace p press pres
          Nothing   -> Just $ GraphP id w h (removeVertexFromEdges p es) $ (take p press ++ drop (p+1) press)
  else let edgeNr = p - length press
       in  Just $ GraphP id w h (take edgeNr es ++ drop (edgeNr+1) es) press
deleteGraphPres' []     (VertexP id x y ol pres)   = Nothing
deleteGraphPres' _      pr                         = debug Err ("TreeEditPres.deleteGraphPres': can't handle "++ show pr) Nothing

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
deleteTreePresF editable updp p (FocusP st en) (ColP id rf press) = deleteTreeColF editable updp p 0 (FocusP st en) press
deleteTreePresF editable updp p focus  (OverlayP id (pres:press)) = deleteTreePresF editable  (updp++[0]) (p++[0]) focus pres
deleteTreePresF editable updp p focus          (WithP ar pres)    = deleteTreePresF editable (updp++[0]) (p++[0]) focus pres
deleteTreePresF editable updp p focus          (StructuralP id pres)  = deleteTreePresF False (updp++[0]) (p++[0]) focus pres
deleteTreePresF editable updp p focus          (ParsingP id pres)  = deleteTreePresF True (updp++[0]) (p++[0]) focus pres
deleteTreePresF editable updp p focus          (LocatorP l pres)  = deleteTreePresF editable (updp++[0]) (p++[0]) focus pres
deleteTreePresF editable updp p f pr = debug Err ("TreeEditPres.deleteTreePresF: can't handle "++show f++" "++ show pr) []

deleteTreeRowF editable updp p i _ [] = debug Err "problem" updp
deleteTreeRowF editable updp p i focus@(FocusP (PathP stp sti) (PathP enp eni)) (pres:press) = 
                           if   enp < (p++[i])
                           then debug Err "problem" updp
                           else if stp < (p++[i]) && enp >= (p++[i+1])
                           then -- editable is ignored, case does not occur
                                debug Err "problem" deleteTreeRowF editable updp p (i+1) focus press
                           else if stp < (p++[i]) 
                           then debug Err "a" $ deleteTreePresF editable (updp++[i]) (p++[i]) focus pres
                           else if stp < (p++[i+1])
                           then debug Err "b" $ deleteTreePresF editable (updp++[i]) (p++[i]) focus pres
                           else debug Err "c" $ deleteTreeRowF editable updp p (i+1) focus press
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

 

pasteTreePres editable p (PathP stp sti) clip (StringP id str) = 
  if editable 
  then let st = if stp < p then 0 else sti
       in  RowP NoIDP 0 [StringP id (take st str), clip,StringP NoIDP (drop st str)]
  else StringP id str
pasteTreePres editable p path clip (RowP id rf press) = RowP id rf (pasteTreePresList editable p 0 path clip press)
pasteTreePres editable p path clip (ColP id rf press) = ColP id rf (pasteTreePresList editable p 0 path clip press)
pasteTreePres editable p path clip (OverlayP id (pres:press)) = OverlayP id (pasteTreePres editable (p++[0]) path clip pres : press)
pasteTreePres editable p path clip (WithP ar pres) = WithP ar (pasteTreePres editable (p++[0]) path clip pres)
pasteTreePres editable p path clip (StructuralP id pres) = StructuralP  id (pasteTreePres False (p++[0]) path clip pres)
pasteTreePres editable p path clip (ParsingP id pres) = ParsingP  id (pasteTreePres True (p++[0]) path clip pres)
pasteTreePres editable p path clip (LocatorP l pres) = LocatorP  l (pasteTreePres editable (p++[0]) path clip pres)
pasteTreePres editable p _ clip pr = text $ "TreeEditPres.pasteTreePres: can't handle "++ show pr


pasteTreePresList editable p i _ _ [] = []
pasteTreePresList editable p i path@(PathP stp sti) clip (pres:press) = 
                           if stp < p++[i]
                           then pres : press
                           else if stp < p++[i+1] 
                           then pasteTreePres editable (p++[i]) path clip pres : press
                           else pres : pasteTreePresList editable p (i+1) path clip press

-- paste keeps tree the same, except for the final leaf (always a StringP     (for now)) 
-- which, if it is editable, is always split in 3
-- the updated focus can therefore be computed nonrecursively

pasteTreePresF path@(PathP p i) pres = if isEditableTreePres p pres 
                                       then FocusP (PathP (p++[2]) 0) (PathP (p++[2]) 0) 
                                       else FocusP path path 
pasteTreePresF NoPathP          pres = NoFocusP





-- nicer to give focus of clip and let calling function decide on after paste focus left or right of clip


-- sample focus (data decls are in PresTypes.hs)
fc = (FocusP (PathP [] 2)(PathP [] 4))

-- sample presentation (text& row, etc. are in XprezLib.hs, decls also in PresTypes.hs)
pr = col [text "abcdefg"]

copyTreePres p (FocusP (PathP stp sti) (PathP enp eni)) (StringP id str) = 
  let st = if  stp < p then 0 else sti
      en = if  enp > p then length str else eni               
  in StringP id (take (en-st) (drop st str))
copyTreePres p (FocusP st en) (RowP id rf press) = let press' = copyTreePresList p 0 (FocusP st en) press
                                                     in  RowP id (if rf < length press' then rf else length press' -1) press'
copyTreePres p (FocusP st en) (ColP id rf press) = let press' = copyTreePresList p 0 (FocusP st en) press
                                                     in  ColP id (if rf < length press' then rf else length press' -1) press'
copyTreePres p focus (OverlayP id (pres:press)) = OverlayP id (copyTreePres (p++[0]) focus pres : press) 
copyTreePres p focus          (WithP ar pres) = {- WithP ar -} (copyTreePres (p++[0]) focus pres) 
copyTreePres p focus          (StructuralP id pres) = (copyTreePres (p++[0]) focus pres)
copyTreePres p focus          (ParsingP id pres) = (copyTreePres (p++[0]) focus pres)
copyTreePres p focus          (LocatorP l pres) = LocatorP l (copyTreePres (p++[0]) focus pres)
copyTreePres p f pr = text $ "TreeEditPres.copyTreePres: can't handle "++show f++" "++ show pr


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
splitRowTreePres editable p path  (ColP id rf press) =
  case splitRowTreePresList editable p 0 path press of
    Left (ps1, ps2) -> if editable
                       then Right $ ColP id rf (ps1++ps2)
                       else Right $ ColP id rf press
    Right ps        -> Right $ ColP id rf ps    
splitRowTreePres editable p path  (OverlayP id (pres:press)) =
  case(splitRowTreePres editable (p++[0]) path pres) of
     Left (p1,p2) -> if editable
                     then Left (OverlayP id (p1:press), OverlayP id (p2:press))
                     else Right $ OverlayP id (pres:press)
     Right p      -> Right $ OverlayP id (p:press)
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
splitRowTreePres editable p path           (ParsingP id pres) =
  case(splitRowTreePres True (p++[0]) path pres) of
     Left (p1,p2) -> if editable
                     then Left (ParsingP id p1, ParsingP id p2)
                     else Right $ ParsingP id pres
     Right p      -> Right $ ParsingP id p
splitRowTreePres editable p path           (LocatorP l pres) =
  case(splitRowTreePres editable (p++[0]) path pres) of
     Left (p1,p2) -> if editable
                     then Left (LocatorP l p1, LocatorP l p2)
                     else Right $ LocatorP l pres
     Right p      -> Right $ LocatorP l p
splitRowTreePres editable p _ pr = Right $ text $ "TreeEditPres.splitRowTreePres: can't handle "++ show pr

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
  case splitRowTreePresPth editable path (press!!!p) of
    Left pth -> if editable then Left $ 0:pth else Right $ p:pth
    Right pth -> Right $ p:pth
splitRowTreePresPth editable (p:path) (ColP id rf press) = 
  case splitRowTreePresPth editable path (press!!!p) of
    Left pth -> if editable then  Right $ p+1:pth else Right $ p:pth
    Right pth -> Right $ p:pth
splitRowTreePresPth editable (0:path) (OverlayP id (pres:press)) =  
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
splitRowTreePresPth editable (p:path) (ParsingP id pres) =
  case splitRowTreePresPth True path pres of
    Left pth  -> if editable then  Left $ 0:pth else Right $ p:pth
    Right pth -> Right $ 0:pth
splitRowTreePresPth editable (p:path) (LocatorP l pres) =
  case splitRowTreePresPth editable path pres of
    Left pth  -> if editable then  Left $ 0:pth else Right $ p:pth
    Right pth -> Right $ 0:pth
splitRowTreePresPth editable pth pr = debug Err ("*** TreeEditPres.splitRowTreePresPth: can't handle "++show pth++" "++ show pr++"***") Right []
-- should return a NoFocusP here













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