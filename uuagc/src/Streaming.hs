module Streaming where

import CommonTypes(Name)
import DepTypes
import List(partition)
import Maybe(fromMaybe,fromJust)

mapp :: (a -> b) -> (a,a) -> (b,b)
mapp f (l,r) = (f l,f r)

-- vervangt nub, bepaald wel de nub maar accumuleert tevens de causes
reduce :: [UsedAttr] -> [UsedAttr]
reduce set = red set [] where
 red (res:rest) set' = red rest (r res set')
 red []         set' = set'
 r res [] = [res]
 r (r1@(Loc f1 a1 c1)) ((r2@(Loc f2 a2 c2)):rest) = if f1==f2 && a1==a2
                                                    then if length c1 < length c2
                                                         then (Loc f2 a2 c1):rest
                                                         else r2:rest
                                                    else r2:(r r1 rest)
 r (r1@(Glo a1 c1)) ((r2@(Glo a2 c2)):rest) = if a1==a2
                                              then if length c1 < length c2
                                                   then (Glo a2 c1):rest
                                                   else r2:rest
                                              else r2:(r r1 rest)
 r (r1@(Loc f1 a1 c1)) ((r2@(Glo a2 c2)):rest) = r2:(r r1 rest)
 r (r1@(Glo a1 c1)) ((r2@(Loc f2 a2 c2)):rest) = r2:(r r1 rest)

-- NB: de rechter (update-) zijde van een streamelement is nooit bevat in de linker (set-) zijde!!
-- nieuwe update set (nu) is de concat van linker en rechter (lu & ru), maar met de al gevonden resultaten (ls' & rs') eruit gefilterd (lu' & ru').
-- deze al gevonden resultaten kunnen onbekende causes bevatten, zijn worden daarom meegenomen in de reductie van de al gevonden resltaten (ls & rs).
-- deze resultaten zijn dus 100% zeker dubbel, maar worden meegenomen voor bepaling van de causes in het nieuwe resultaat
stUnion :: Stream -> Stream -> Stream
stUnion []           _            = []
stUnion _            []           = []
stUnion ((ls,lu):lr) ((rs,ru):rr) = (ns,nu):nr where
 (lu',ls') = partition (flip notElem rs) lu
 (ru',rs') = partition (flip notElem ls) ru
 nu = lu' ++ ru'
 ns = reduce (ls++ls'++rs++rs')
 nr = stUnion lr rr

{-
-- oude versie, met nub ipv reduce
stUnion :: Stream -> Stream -> Stream
stUnion [] _ = []
stUnion _ [] = []
stUnion ((xset,xupd):xrest) ((yset,yupd):yrest) = (nub (xset++yset),new):(stUnion xrest yrest) where
 new = (filter (flip notElem yset) xupd) ++ (filter (flip notElem xset) yupd)
-}

{-
-- de x wordt gebruikt om te kijken of de poort open staat, de y wordt doorgegeven als de poort openstaat
-- ouder versie, zonder kortste cause
stPort :: Name -> Stream -> Stream -> Stream
stPort attr ((xset,xupd):xrest) ((yset,yupd):yrest) = case lookup attr (map toPair xupd) of
                                                       Just causes -> addTrace causes (([],yset++yupd):yrest)
                                                       Nothing    -> ([],[]):(stPort attr xrest yrest)
-}
-- de x wordt gebruikt om te kijken of de poort open staat, de y wordt doorgegeven als de poort openstaat
-- de recursieve aanroep van stPortRest wordt gedaan om in het uiteindelijke resultaat de kortste cause te krijgen
-- waarom de cause niet initiëel de kortste is, weet ik niet. Maar er worden wel kortere causes gevonden later in het proces
{-
stPort :: Name -> Stream -> Stream -> Stream
stPort attr ((xset,xupd):xrest) ((yset,yupd):yrest) = case lookup attr (map toPair xupd) of
                                                       Just causes -> (addTrace causes ([],yset++yupd)):(stPortRest attr xrest yrest)
                                                       Nothing    -> ([],[]):(stPort attr xrest yrest)
 where                                                     
  stPortRest attr ((xset,xupd):xrest) (y:yrest) = case lookup attr (map toPair xset) of
                                                   Just causes -> (addTrace causes y):(stPortRest attr xrest yrest)
                                                   Nothing -> (error "Dit mag nooit gebeuren!" ([],[])):(stPortRest attr xrest yrest)
-}


-- 20 januari: herschrijving van bovenstaande versie in 1 functie (zonder where)...
stPort :: Name -> Stream -> Stream -> Stream
stPort attr ((xset,xupd):xrest) ((yset,yupd):yrest) = case lookup attr (map toPair xupd) of
                                                       Just causes -> (addTrace causes ([],yset++yupd)):(zipWith (\(xset,xupd) y -> addTrace (fromJust . lookup attr . map toPair $ xset) y) xrest yrest)
                                                       Nothing    -> ([],[]):(stPort attr xrest yrest)

-- stStart biedt geen functionaliteit voor een initiële cause, want er zijn geen initiële causes:
-- het begin van een stream is namelijk altijd een attribuut dat zichzelf bepaalt. Dat heeft geen oorzaak nodig.
stStart :: Name -> Name -> Stream
stStart field attr = first:(repeat other) where
 first = ([],[Loc field attr []])
 other = ([Loc field attr []],[])

stEmpty :: Stream
stEmpty = repeat ([],[])

getStream :: Vertex -> [UseStream] -> Stream
getStream vertex streams = fromMaybe stEmpty (lookup vertex streams)

stLocal2global :: Stream -> Stream
stLocal2global = map (mapp (map (\res -> Glo (getAttr res) (getTrace res))))
-- de nub hoeft niet, want het filter op fieldname van een lokaal resultaat filtert doublures er al uit
--stLocal2global = map (mapp (nub . (map (\res -> Glo (getAttr res)))))

stFilterInclSide :: UseStream -> Stream
stFilterInclSide (vertex,stream) = stFilterOnField ((getField vertex)==) stream

stFilterExclSide :: UseStream -> Stream
stFilterExclSide (vertex,stream) = stFilterOnField ((getField vertex)/=) stream

stFilterOnField :: (Name -> Bool) -> Stream -> Stream
stFilterOnField f = map (mapp (filter (f . getField)))