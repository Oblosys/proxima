-----------------------------------------------------------------------------------------
{-| Module      : Gen_PresentationAG
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Gen_PresentationAG where

import TypesUtils

generate :: DocumentType -> [String]
generate docType = genPresentationSheet
                ++ genDataType (addHolesParseErrs docTypeWithListsAndConsLists)
                ++ genAttr     (removeDocumentDecl (addEnrichedDocDecl docType))
                ++ genSem                docTypeWithListsAndConsLists
                ++ genSemSynthesizedPath docTypeWithoutEnrWithLists
                ++ genSemXML             docTypeWithoutEnrWithListsAndConsLists
                ++ genSemTree            docTypeWithoutEnrWithListsAndConsLists
  where docTypeWithoutEnrWithListsAndConsLists = removeEnrichedDocDecl docTypeWithListsAndConsLists
        docTypeWithoutEnrWithLists             = removeEnrichedDocDecl docTypeWithLists
        docTypeWithListsAndConsLists           = addConsListDecls docTypeWithLists
        docTypeWithLists = addListDecls (removeDocumentDecl (addEnrichedDocDecl docType))
-- the behavior for holes and parse errors is too different, therefore we do not add them to the type
-- but just generate code for them in the gen functions.

-- instead of removing documentDecl, we should actually recursively determine all used AG types from EnrichedDoc

genPresentationSheet = genBanner "presentationSheet" $
  [ "WRAPPER Root"
  , ""
  , "{"
  , "-- type PresentationSheet doc enr node clip token = "
  , "--        enr -> doc -> FocusDoc -> WhitespaceMap -> IDPCounter -> "
  , "--        (WhitespaceMap, IDPCounter, Presentation doc node clip token)"
  , ""
  , "presentationSheet :: PresentationSheet Document EnrichedDoc Node ClipDoc UserToken"
  , "presentationSheet enrichedDoc document focusD whitespaceMap pIdC = "
  , "  let (Syn_EnrichedDoc _ pIdC' pres _ self _ whitespaceMap' _) = "
  , "        wrap_EnrichedDoc (sem_EnrichedDoc enrichedDoc) (Inh_EnrichedDoc undefined document Map.empty focusD pIdC [] emptyTokenStreamT whitespaceMap initLayout)"
  , "  in  (whitespaceMap', pIdC', pres, self)"
  , ""
  , "{- "
  , "A type error here means that extra attributes were declared on EnrichedDoc"
  , "The attribute signature for EnrichedDoc should be:"
  , ""
  , "EnrichedDoc  [ focusD : FocusDoc path : Path"
  , "             | pIdC : Int layoutMap : WhitespaceMap"
  , "             | pres : Presentation_Doc_Node_Clip_Token EnrichedDoc "
  , "             ]"
  , "-}"
  , ""
  , "-- Phi"
  , "modifiedTree :: EnrichedDoc -> (EnrichedDoc, WhitespaceMap)"
  , "modifiedTree enrichedDoc = "
  , "  let (Syn_EnrichedDoc _ _pIdC' _pres _ self _tokStr' _whitespaceMap' whitespaceMap2') ="
  , "        wrap_EnrichedDoc (sem_EnrichedDoc enrichedDoc) (Inh_EnrichedDoc undefined HoleDocument Map.empty (PathD []) 0 [] emptyTokenStreamT initLayout initLayout)"
  , "   in (self, whitespaceMap2')"
  , "}" 
  ]

genDataType decls = genBanner "AG data type" $
  concatMap genDataDecl decls
 where genDataDecl (Decl lhsType prods) = 
         "DATA %1" <~ [genTypeName lhsType] :
         [ "  | " ++ genProd prod | prod <- prods] ++
         [ "" ]
        where genProd (Prod _ cnstrName idpFields fields) =
                cnstrName ++ (prefixBy " " $ map genIDPField idpFields ++
                                             map genField fields)
                      
       genIDPField (Field fieldName fieldType) = fieldName ++ ":" ++ genIDPTypeAG  fieldType
       genField    (Field fieldName fieldType) = fieldName ++ ":" ++ if isDeclaredType decls fieldType
                                                                     then genTypeAG fieldType
                                                                     else genNotDeclaredTypeAG fieldType

-- TODO enriched can be treated more uniformly
genAttr decls = genBanner "Attr declarations" $
 ([ "ATTR %1" -- all types including EnrichedDoc, lists and conslists
  , "     [ doc : Document focusD : FocusDoc path : Path errLocs : ErrLocs checkedModule : CheckedModule |  pIdC : Int whitespaceMap : WhitespaceMap whitespaceMapCreated : WhitespaceMap tokStr : TokenStreamT | ]" -- Phi
  -- , "     [ doc : Document focusD : FocusDoc path : Path |  pIdC : Int whitespaceMap : WhitespaceMap | ]"
  , ""  -- Document is for popups, will be removed in the future
  , "ATTR %2" -- all types including EnrichedDoc except lists and conslists
  , "     [ | | pres : Presentation_Doc_Node_Clip_Token noIdps : Int pres' : {(Presentation_Doc_Node_Clip_Token, [IDP], WhitespaceMap, TokenStreamT)} ]" -- Phi
  -- , "     [ | | pres : Presentation_Doc_Node_Clip_Token ]"
  , ""
  ] ++ if null (removeEnrichedDocDecl (addListDecls decls)) then [] else
  [ "ATTR %3"  -- all types except EnrichedDoc, including lists and conslists
  , "     [ ix : Int | | parseErrors USE {++} {[]} : {[ParseErrorMessage]} ]"
  , ""
  , "ATTR %4" -- all types except EnrichedDoc, including lists
  , "     [ | | ix : Int path : Path presXML : Presentation_Doc_Node_Clip_Token presTree : Presentation_Doc_Node_Clip_Token ]"
  , ""
  ] ++ if null listTypeNames then [] else
  [ "ATTR %5" -- all lists and conslists
  , "     [ | | press : {[Presentation_Doc_Node_Clip_Token]} ]"
  , ""
  , "ATTR %6"  -- all conslists
  , "     [ | | pressXML : {[Presentation_Doc_Node_Clip_Token]} pressTree : {[Presentation_Doc_Node_Clip_Token]} ]"
  , ""
  ]) <~ [ separateBy " " $ getAllDeclaredTypeNames (addConsListDecls (addListDecls decls))
        , separateBy " " $ getAllDeclaredTypeNames (addListDecls decls)
        , separateBy " " $ getAllDeclaredTypeNames (removeEnrichedDocDecl (addConsListDecls (addListDecls decls)))
        , separateBy " " $ getAllDeclaredTypeNames (removeEnrichedDocDecl (addListDecls decls))
        , separateBy " " $ listNames ++ consListNames
        , separateBy " " $ consListNames
        ]
 where listTypeNames = map typeName $ getAllUsedListTypes decls
       listNames = map ("List_"++) listTypeNames
       consListNames = map ("ConsList_"++) listTypeNames
 
genSem decls = genBanner "General sem functions" $
  concatMap genSemDecl decls
 where genSemDecl decl@(Decl (LHSBasicType _) _)    = genSemBasicDecl decls decl
       genSemDecl decl@(Decl (LHSListType _) _)     = genSemListDecl decl
       genSemDecl decl@(Decl (LHSConsListType _) _) = genSemConsListDecl decl

genSemBasicDecl decls (Decl (LHSBasicType typeName) prods) = 
  "SEM %1" <~ [typeName] :
  concatMap genSemPIDCProd prods ++
  concatMap genSemIxProd prods ++ 
  concatMap genSemPresProd prods ++
  [ "  | Hole%1"
  , "      loc.noIdps = -1"
  , "      loc.pres' = error \"loc.pres' in Hole%1\""
  , "      lhs.pres = presHole @lhs.focusD \"%1\" (Node_Hole%1 @self @lhs.path) @lhs.path"
  , "                 `withLocalPopupMenuItems` menuD (PathD @lhs.path) @lhs.doc"                  
  , "  | ParseErr%1"
  , "      loc.noIdps = -1"
  , "      loc.pres' = error \"loc.pres' in ParseErr%1\""
  , "      lhs.pres = presParseErr (Node_ParseErr%1 @self @lhs.path) @error"
  , "                 `withLocalPopupMenuItems` menuD (PathD @lhs.path) @lhs.doc"                  
  ] <~ [typeName] ++
  (if typeName /= "EnrichedDoc"
   then ["      lhs.parseErrors = getErrorMessages @error"]
   else []) ++ 
  [ ""
  ]
 where genSemPIDCProd (Prod _ cnstrName idpFields fields) =
         let agFields = filter (isDeclaredType decls . fieldType) fields
             -- only take into account fields that have AG types (instead of prim or other composite)
             pIdCs = 
--               map ("      "++)  
                 (addPlus $ zipWith (\l r -> "%1.pIdC = @%2.pIdC" <~ [l,r])
                                     ( map fieldName agFields ++ ["lhs"])
                                     ( "lhs" : map fieldName agFields))++
                  [ ("%1.path  = @lhs.path++["++show i++"]") <~ [fieldName field] 
                  | (i,field) <- zip [0..] fields, isDeclaredType decls $ fieldType field 
                  ] -- only generate for AG field types, but do include the others in the index computation
               where addPlus (l:ls) = -- if not $ null idpFields
                                      {-then-} (l++ " + @loc.noIdps") : ls -- Phi: [IDP]
                                      -- then (l++ " + " ++ show (length idpFields)) : ls
--                                      else ls
             pres' = [ "__ADMINISTRATE" --(loc.pres, loc.idps, lhs.whitespaceMapCreated, lhs.tokStr, loc.noIdps)"
                     --, "  = let (pres, idps, wsMap, tokStr) = @loc.pres'"
                     -- , "     in (addErr @lhs.errLocs @lhs.tokStr tokStr pres, idps, wsMap, tokStr, length idps)"
                     ]
         in {- if not $ null pIdCs 
            then "  | %1 " <~ [cnstrName] : pIdCs
            else [] -}
            "  | %1 " <~ [cnstrName] : map ("      "++) (pIdCs ++ pres')
              -- this computation goes wrong when there are lists of idps (but it will be obsolete in a future version)
       genSemIxProd (Prod _ cnstrName idpFields fields) =
         let ixs = 
               [ "    %1.ix = %2" <~ [fieldName, show i]
               | (i,Field fieldName fieldType) <- zip [0..] fields, isDeclaredType decls $ fieldType
               ]
          in if not $ null ixs
             then [ "  | %1" <~ [cnstrName] ] ++ ixs
             else []
       genSemPresProd (Prod _ cnstrName idpFields fields) =
         [ "  | %1"
         , "      lhs.pres = loc (Node_%1 @self @lhs.path) $ presentFocus @lhs.focusD @lhs.path @pres"
         , "                 `withLocalPopupMenuItems` menuD (PathD @lhs.path) @lhs.doc" 
         ] <~ [cnstrName]
              
genSemListDecl (Decl (LHSListType typeName) _) = 
  [ "SEM List_%1"
  , "  | List_%1"
  , "      lhs.press = map ( loc (Node_List_%1 @self @lhs.path)" -- remove spaces
  , "                      . presentFocus @lhs.focusD @lhs.path )"
  , "                      @elts.press"
  , "                      -- parent is reponsible for setting parsing/structural"
-- gerbo TODO: fix that 100, make it @loc.noIdps
  , "      elts.pIdC = @lhs.pIdC + 100 -- NOT RIGHT, should be taken from document type def."
  , "      lhs.pIdC = @elts.pIdC"
  , "      elts.path = @lhs.path"
  , "      elts.ix = 0"
  , "  | HoleList_%1"
  , "      lhs.press = []"
  , "      loc.noIdps = 0"
  , "      loc.pres' = error \"loc.pres' in List%1\""
  , "  | ParseErrList_%1"
  , "      lhs.press = []"
  , "      loc.noIdps = 0"
  , "      loc.pres' = error \"loc.pres' in List%1\""
  , "  | List_%1"
  , "      lhs.pres = loc (Node_List_%1 @self @lhs.path) $ presentFocus @lhs.focusD @lhs.path $ @pres"
  , "                 `withLocalPopupMenuItems` menuD (PathD @lhs.path) @lhs.doc"                  
  , "  | ParseErrList_%1"
  , "      lhs.pres = presParseErr (Node_ParseErrList_%1 @self @lhs.path) @error"
  , "                 `withLocalPopupMenuItems` menuD (PathD @lhs.path) @lhs.doc"                  
  , "      lhs.parseErrors = getErrorMessages @error"
  , "  | HoleList_%1"
  , "      lhs.pres = presHole @lhs.focusD \"%1\" (Node_HoleList_%1 @self @lhs.path) @lhs.path"
  , "                 `withLocalPopupMenuItems` menuD (PathD @lhs.path) @lhs.doc"                  
  , ""
  ] <~ [typeName]

genSemConsListDecl (Decl (LHSConsListType typeName) _) = 
  [ "SEM ConsList_%1"
  , "  | Cons_%1" 
  , "      head.path  = @lhs.path++[@lhs.ix]"
  , "      tail.path = @lhs.path"
  , "      lhs.press = @head.pres : @tail.press"
  , "      head.pIdC = @lhs.pIdC + 30 -- NOT RIGHT, should be taken from document type def."
  , "      tail.pIdC = @head.pIdC"
  , "      lhs.pIdC = @tail.pIdC"
  , "      tail.ix  = @lhs.ix + 1"
  , "  | Nil_%1      lhs.press = []"
  , ""
  ] <~ [typeName]

-- somehow this is also produced by the copy rules, but that seems odd, since path would be a threaded attribute
genSemSynthesizedPath decls = genBanner "Synthesized path rules" $ concat
  [ "SEM %1" <~ [genTypeName lhsType] :
    [ "  | %1 lhs.path = @lhs.path" <~ [cnstrName]
    | Prod _ cnstrName  _ _ <- prods 
    ] ++
    [ "" ] 
  | Decl lhsType prods <- decls
  ] 
  
genSemXML decls = genBanner "Sem functions for XML presentation" $
  concatMap genSemXMLDecl decls
 where genSemXMLDecl decl@(Decl (LHSBasicType _) _)    = genSemXMLBasicDecl decls decl
       genSemXMLDecl decl@(Decl (LHSListType _) _)     = genSemXMLListDecl decl
       genSemXMLDecl decl@(Decl (LHSConsListType _) _) = genSemXMLConsListDecl decl

genSemXMLBasicDecl decls (Decl (LHSBasicType lhsTypeName) prods) =  
  "SEM %1" <~ [lhsTypeName] : concat
  [ [ "  | %1"
    , "      lhs.presXML = presentElementXML @lhs.focusD (Node_%1 @self @lhs.path) @lhs.path \"%1\" [ %2 ] "
    ] <~ [ cnstrName, separateBy ", " $ map genField fields ] 
  | Prod _ cnstrName idpFields fields <- prods
  ] ++
  ([ "  | Hole%1     lhs.presXML = presHole @lhs.focusD \"%1\" (Node_Hole%1 @self @lhs.path) @lhs.path"
   , "  | ParseErr%1 lhs.presXML = presParseErr (Node_ParseErr%1 @self @lhs.path) @error"
   , ""
   ] <~ [lhsTypeName])
 where genField (Field fieldName fieldType) = 
         (if isDeclaredType decls fieldType
          then "@%1.presXML"
          else if typeName fieldType `elem` primTypeNames
          then "presentPrimXML%2 @%1"
          else "presentNonDeclaredXML \"%2\" @%1" --"presentXML%2 @%1" --gerbo
         ) <~ [ fieldName, genTypeAG fieldType]  
       
genSemXMLListDecl (Decl (LHSListType typeName) _) = 
  [ "SEM List_%1"
  , "  | List_%1"
  , "      lhs.presXML = loc (Node_List_%1 @self @lhs.path) $ structural $ presentFocus @lhs.focusD @lhs.path $"
  , "                    col @elts.pressXML"
  , "  | ParseErrList_%1"
  , "      lhs.presXML = loc (Node_List_%1 @self @lhs.path) $ structural $ presentFocus @lhs.focusD @lhs.path $"
  , "                    presParseErr (Node_ParseErrList_%1 @self @lhs.path) @error"
  , "  | HoleList_%1"
  , "      lhs.presXML = loc (Node_List_%1 @self @lhs.path) $ structural $ presentFocus @lhs.focusD @lhs.path $"
  , "                    presHole @lhs.focusD \"List_%1\" (Node_HoleList_%1 @self @lhs.path) @lhs.path"
  , ""
  ] <~ [typeName]

genSemXMLConsListDecl (Decl (LHSConsListType typeName) _) = 
  [ "SEM ConsList_%1"
  , "  | Cons_%1 lhs.pressXML  = @head.presXML : @tail.pressXML"
  , "  | Nil_%1  lhs.pressXML  = []"
  , ""
  ] <~ [typeName]


genSemTree decls = genBanner "Sem functions for tree presentation" $
  concatMap genSemTreeDecl decls
 where genSemTreeDecl decl@(Decl (LHSBasicType _) _)    = genSemTreeBasicDecl decls decl
       genSemTreeDecl decl@(Decl (LHSListType _) _)     = genSemTreeListDecl decl
       genSemTreeDecl decl@(Decl (LHSConsListType _) _) = genSemTreeConsListDecl decl

genSemTreeBasicDecl decls (Decl (LHSBasicType lhsTypeName) prods) =  
  "SEM %1" <~ [lhsTypeName] : concat
  [ [ "  | %1"
    , "      lhs.presTree = presentElementTree @lhs.focusD (Node_%1 @self @lhs.path) @lhs.path \"%1\" [ %2 ] "
    ] <~ [ cnstrName, separateBy ", " $ map genField fields ] 
  | Prod _ cnstrName idpFields fields <- prods
  ] ++
  ([ "  | Hole%1     lhs.presTree = presHole @lhs.focusD \"%1\" (Node_Hole%1 @self @lhs.path) @lhs.path"
   , "  | ParseErr%1 lhs.presTree = presParseErr (Node_ParseErr%1 @self @lhs.path) @error"
   , ""
   ] <~ [lhsTypeName])
 where genField (Field fieldName fieldType) = 
         (if isDeclaredType decls fieldType
          then "@%1.presTree"
          else if typeName fieldType `elem` primTypeNames
          then "presentPrimTree%2 @%1"
          else "presentNonDeclaredTree \"%2\" @%1"-- "presentXML%2 @%1" -- gerbo
         ) <~ [ fieldName, genTypeAG fieldType]  

genSemTreeListDecl (Decl (LHSListType typeName) _) = 
  [ "SEM List_%1"
  , "  | List_%1"
  , "      lhs.presTree = loc (Node_List_%1 @self @lhs.path) $ structural $ presentFocus @lhs.focusD @lhs.path $"
  , "                       col @elts.pressTree"
  , "  | ParseErrList_%1"
  , "      lhs.presTree = loc (Node_List_%1 @self @lhs.path) $ structural $ presentFocus @lhs.focusD @lhs.path $"
  , "                       presParseErr (Node_ParseErrList_%1 @self @lhs.path) @error"
  , "  | HoleList_%1"
  , "      lhs.presTree = loc (Node_List_%1 @self @lhs.path) $ structural $ presentFocus @lhs.focusD @lhs.path $"
  , "                       presHole @lhs.focusD \"List_%1\" (Node_HoleList_%1 @self @lhs.path) @lhs.path"
  , ""
  ] <~ [typeName]

genSemTreeConsListDecl (Decl (LHSConsListType typeName) _) = 
  [ "SEM ConsList_%1"
  , "  | Cons_%1 lhs.pressTree  = @head.presTree : @tail.pressTree"
  , "  | Nil_%1  lhs.pressTree  = []"
  , ""
  ] <~ [typeName]

