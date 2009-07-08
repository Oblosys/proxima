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
generate docType = genProxParserTypeSynonym
                ++ genConstruct (addHolesParseErrs docTypeWithLists)
                ++ genReuse   docTypeWithLists
                ++ genExtract docTypeWithLists
                ++ genDefault docTypeWithLists
                ++ genExtractFromTokens 
                ++ genGenericReuse docTypeWithLists
  where docTypeWithLists = addListDecls (addEnrichedDocDecl docType)
  

genProxParserTypeSynonym = genBanner "ProxParser type synonym" $
  [ "type ProxParser a = ListParser Document Node ClipDoc UserToken a"
  ]
  
genConstruct decls = genBanner "Construct instance" $
  [ "instance Construct Document Node ClipDoc UserToken where"
  ,   "  construct NoNode = error $ \"ProxParser_Generated.construct not defined on NoNode\""
  ] ++
  [ "  construct (Node_%1 _ _) = construct_%1" <~ [cnstrName] 
  | cnstrName <-  getAllConstructorNames decls
  ] ++
  concatMap genConstructDecl decls
                                   
  
genConstructDecl (Decl lhsType prods) = 
  [ case prodKind of
      HoleProd     -> "construct_Hole%1 tk ~[] = Clip_%1 $ hole" <~ [ genTypeName lhsType ]
      ParseErrProd -> "construct_ParseErr%1 (StructuralTk _ _ pres _ _) ~[] = Clip_%1 $ parseErr (StructuralParseErr pres)" <~ [ genTypeName lhsType ]
      _            -> 
        case lhsType of 
          (LHSListType lhsTypeName) ->
            "construct_List_%1 tk mClips = genericConstruct_List \"%1\" toList_%1 mClips" <~ [ lhsTypeName ]
          (LHSBasicType lhsTypeName) ->
            "construct_%1 tk ~[%2] = Clip_%3 $ reuse%1 [tk] %4%5"
            <~ [ cnstrName
               , separateBy "," ["mClip"++show i | i <- [0..length fields - 1]]
               , lhsTypeName 
               , concat $ replicate (length idpFields) " Nothing"
               , concat [ " (retrieveArg \"%1\" \"%2\" mClip%3)" <~ [ cnstrName
                                                                   , fieldName ++ "::" ++ genType fieldType
                                                                   , show i ] 
                        | (i, Field fieldName fieldType) <- zip [0..] fields 
                        ]
               ]
  | (Prod prodKind cnstrName idpFields fields) <- prods
  ]

genReuse decls = genBanner "reuse functions" $ concat
  [ [ "reuse%1 :: [Token doc Node clip token]%2 -> %3"
    , "reuse%1 nodes%4"
    , "  = case extractFromTokens extract%1 default%1 nodes of"
    , "           %5 -> genericReuse%6 %1%7%4"
    , "           _ -> error \"Internal error:ProxParser_Generated.reuse%1\""
    , ""
    ] <~ [ cnstrName                                                         -- %1
         , prefixBy " -> Maybe " $ map (genIDPType . fieldType) idpFields ++   
                                   map (genType . fieldType) fields          -- %2
         , genTypeName lhsType                                               -- %3
         , prefixBy " m" $ cnstrArgs                                         -- %4
         , "("++cnstrName++ concatMap (" "++) cnstrArgs ++")"                -- %5
         , show $ length idpFields + length fields                           -- %6
         , prefixBy " " $ cnstrArgs                                          -- %7
         ] -- we don't use genPattern and fieldNames, since the (" m"++) could cause problems 
           -- e.g. for T a:A ma : M, we would get ma mma and a ma
  | Decl lhsType prods <- decls, prod@(Prod _ cnstrName idpFields fields) <- prods
  , let cnstrArgs = zipWith (++) (replicate (length idpFields + length fields) "a") (map show [0..]) 
  ]


genExtract decls = genBanner "extract functions" $ concat
  [ [ "extract%1 :: Maybe Node -> Maybe %2"
    , "extract%1 (Just (Node_%1 x@%3 _)) = Just x"
    , "extract%1 _ = Nothing"
    , ""
    ] <~ [ cnstrName, genTypeName lhsType, "(" ++ cnstrName ++ concat (replicate (getArity prod) " _") ++ ")" ]
  | Decl lhsType prods <- decls, prod@(Prod _ cnstrName _ _) <- prods 
  ]             
   

genDefault decls = genBanner "default functions" $ concat
  [ case lhsType of
      LHSBasicType typeName -> 
               [ "default%1 :: %2"
               , "default%1 = %1" ++ prefixBy " " (map genNoIDP idpFields) ++
                                     concat (replicate (length fields) " hole")
               , ""
               ] <~ [ cnstrName, genTypeName lhsType ]
      LHSListType typeName -> 
               [ "defaultList_%1 :: List_%1"
               , "defaultList_%1 = List_%1 Nil_%1"
               , ""
               ] <~ [ typeName ]
             
  | Decl lhsType prods <- decls, Prod _ cnstrName idpFields fields <- prods 
  ]


{-
this one is here because we don't want DocUtils to import another module (it could also be put in the
non-generated part). 
-}
genExtractFromTokens = genBanner "extractFromTokens" $
 [ "-- return result of the first extraction application in the list that is not Nothing"
 , "extractFromTokens :: (Maybe Node -> Maybe a) -> a -> [Token doc Node clip token] -> a"
 , "extractFromTokens extr def []     = def"
 , "extractFromTokens extr def (t:ts) = maybe (extractFromTokens extr def ts) id (extr (tokenNode t))"
 ]

genGenericReuse decls = genBanner "genericReuse functions" $
  concatMap genGenericReuseN $ [0.. maximum (map getArity (getAllProductions decls))]
 where genGenericReuseN n = 
         let aArgs = zipWith (++) (replicate n "a") (map show [0..])
             maArgs = map ("m"++) aArgs
         in [ "genericReuse%1 :: (%2r) ->"
            , "                 %2"
            , "                 %3r"
            , "genericReuse%1 f%4 ="
            , "  f%5"
            , ""
            ] <~ [ show n                                                                          -- %1
                      , suffixBy " -> " $ aArgs                                                         -- %2
                      , surroundBy "Maybe " " -> " $ aArgs                                              -- %3
                      , prefixBy " " $ aArgs ++ maArgs                                                  -- %4
                      , concat $ zipWith (\a ma -> " (maybe " ++ a ++ " id " ++ ma ++ ")") aArgs maArgs -- %5
                      ]
       