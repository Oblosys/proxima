-----------------------------------------------------------------------------------------
{-| Module      : Gen_ProxParser
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Gen_ProxParser where

import TypesUtils

generate :: DocumentType -> [String]
generate docType = genReuse   docTypeWithLists
                ++ genExtract docTypeWithLists
                ++ genDefault docTypeWithLists
                ++ genExtractFromTokens 
                ++ genGenericReuse docTypeWithLists
  where docTypeWithLists = docType ++ genListDecls docType
  
genReuse decls = addBanner "reuse functions" $ concat
  [ [ "reuse%1 :: [Token doc Node clip token]%2 -> %3"
    , "reuse%1 nodes%4"
    , "  = case extractFromTokens extract%1 default%1 nodes of"
    , "           %5 -> reuse%6 %1%7%4"
    , "           _ -> error \"Internal error:ProxParser_Generated.reuse%1\""
    , ""
    ] `subst` [ cnstrName                                                         -- %1
              , prefixBy " -> Maybe " $ map (genIDPType . fieldType) idpFields ++   
                                        map (genType . fieldType) fields          -- %2
              , typeName                                                          -- %3
              , prefixBy " m" $ cnstrArgs                                         -- %4
              , "("++cnstrName++ concatMap (" "++) cnstrArgs ++")"                -- %5
              , show $ length idpFields + length fields                           -- %6
              , prefixBy " " $ cnstrArgs                                          -- %7
              ] -- we don't use genPattern and fieldNames, since the (" m"++) could cause problems 
                -- e.g. for T a:A ma : M, we would get ma mma and a ma
  | Decl _ typeName prods <- decls, prod@(Prod _ cnstrName idpFields fields) <- prods
  , let cnstrArgs = zipWith (++) (replicate (length idpFields + length fields) "a") (map show [0..]) 
  ]


genExtract decls = addBanner "extract functions" $ concat
  [ [ "extract%1 :: Maybe Node -> Maybe %2"
    , "extract%1 (Just (%1Node x@%3 _)) = Just x"
    , "extract%1 _ = Nothing"
    , ""
    ] `subst` [ cnstrName, typeName, "(" ++ cnstrName ++ concat (replicate (getArity prod) " _") ++ ")" ]
  | Decl _ typeName prods <- decls, prod@(Prod _ cnstrName _ _) <- prods 
  ]             
   

genDefault decls = addBanner "default functions" $ concat
  [ case declKind of
      Basic -> [ "default%1 :: %2"
               , "default%1 = %1" ++ prefixBy " " (map genNoIDP idpFields) ++
                                     concat (replicate (length fields) " hole")
               , ""
               ] `subst` [ cnstrName, typeName ]
      List  -> [ "defaultList_%1 :: List_%1"
               , "defaultList_%1 = List_%1 Nil_%1"
               , ""
               ] `subst` [ drop 5 typeName ]
             
  | Decl declKind typeName prods <- decls, Prod _ cnstrName idpFields fields <- prods 
  ]


{-
this one is here because we don't want DocUtils to import another module (it could also be put in the
non-generated part). 
-}
genExtractFromTokens = addBanner "extractFromTokens" $
 [ "-- return result of the first extraction application in the list that is not Nothing"
 , "extractFromTokens :: (Maybe Node -> Maybe a) -> a -> [Token doc Node clip token] -> a"
 , "extractFromTokens extr def []     = def"
 , "extractFromTokens extr def (t:ts) = maybe (extractFromTokens extr def ts) id (extr (tokenNode t))"
 ]

genGenericReuse decls = addBanner "genericReuse functions" $
  concatMap genGenericReuseN $ [0.. maximum (map getArity (getAllProductions decls))]
 where genGenericReuseN n = 
         let aArgs = zipWith (++) (replicate n "a") (map show [0..])
             maArgs = map ("m"++) aArgs
         in [ "reuse%1 :: (%2r) ->"
            , "          %2"
            , "          %3r"
            , "reuse%1 f%4 ="
            , "  f%5"
            , ""
            ] `subst` [ show n                                                                          -- %1
                      , suffixBy " -> " $ aArgs                                                         -- %2
                      , surroundBy "Maybe " " -> " $ aArgs                                              -- %3
                      , prefixBy " " $ aArgs ++ maArgs                                                  -- %4
                      , concat $ zipWith (\a ma -> " (maybe " ++ a ++ " id " ++ ma ++ ")") aArgs maArgs -- %5
                      ]
       
{- 
reuse2 :: (a0 -> a1 -> r) -> 
          a0 -> a1 -> 
          Maybe a0 -> Maybe a1 -> r
reuse2 f  a0 a1 ma0 ma1 =
  f (maybe a0 id ma0) (maybe a1 id ma1) 

reuse3 :: (a0 -> a1 -> a2 -> r) -> 
          a0 -> a1 -> a2 -> 
          Maybe a0 -> Maybe a1 -> Maybe a2 -> r
reuse3 f  a0 a1 a2 ma0 ma1 ma2 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) 

-}