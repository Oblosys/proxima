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
  , "  let (Syn_EnrichedDoc pIdC' pres self whitespaceMap') = "
  , "        wrap_EnrichedDoc (sem_EnrichedDoc enrichedDoc) (Inh_EnrichedDoc document focusD pIdC [] whitespaceMap)"
  , "  in  (whitespaceMap', pIdC', pres)"
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
  , "     [ doc : Document focusD : FocusDoc path : Path |  pIdC : Int whitespaceMap : WhitespaceMap | ]"
  , ""  -- Document is for popups, will be removed in the future
  , "ATTR %2" -- all types including EnrichedDoc except lists and conslists
  , "     [ | | pres : Presentation_Doc_Node_Clip_Token ]"
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
  , "      lhs.pres = presHole @lhs.focusD \"%1\" (Node_Hole%1 @self @lhs.path) @lhs.path"
  , "                 `withLocalPopupMenuItems` (mkPopupItemsFromDocUpdates $ menuD (PathD @lhs.path) @lhs.doc)"                  
  , "  | ParseErr%1"
  , "      lhs.pres = presParseErr (Node_ParseErr%1 @self @lhs.path) @error"
  , "                 `withLocalPopupMenuItems` (mkPopupItemsFromDocUpdates $ menuD (PathD @lhs.path) @lhs.doc)"                  
  ] <~ [typeName] ++
  (if typeName /= "EnrichedDoc"
   then ["      lhs.parseErrors = getErrorMessages @error"]
   else []) ++ 
  [ ""
  ]
 where genSemPIDCProd (Prod _ cnstrName idpFields fields) =
         let agFields = filter (isDeclaredType decls . fieldType) fields
             -- only take into account fields that have AG types (instead of prim or other composite)
         in  "  | %1 " <~ [cnstrName] : 
             map ("      "++)  
                 ((addPlus $ zipWith (\l r -> "%1.pIdC = @%2.pIdC" <~ [l,r])
                                     ( map fieldName agFields ++ ["lhs"])
                                     ( "lhs" : map fieldName agFields))++
                  [ ("%1.path  = @lhs.path++["++show i++"]") <~ [fieldName field] 
                  | (i,field) <- zip [0..] fields, isDeclaredType decls $ fieldType field 
                  ] -- only generate for AG field types, but do include the others in the index computation
                 )
        where addPlus (l:ls) = (l++ " + " ++ show (length idpFields)) : ls
              -- this computation goes wrong when there are lists of idps (but it will be obsolete in a future version)
       genSemIxProd (Prod _ cnstrName idpFields fields) =
         [ "  | %1" <~ [cnstrName] ] ++
         [ "    %1.ix = %2" <~ [fieldName, show i]
         | (i,Field fieldName fieldType) <- zip [0..] fields, isDeclaredType decls $ fieldType
         ]
       genSemPresProd (Prod _ cnstrName idpFields fields) =
         [ "  | %1"
         , "      lhs.pres = loc (Node_%1 @self @lhs.path) $ presentFocus @lhs.focusD @lhs.path @pres"
         , "                 `withLocalPopupMenuItems` (mkPopupItemsFromDocUpdates $ menuD (PathD @lhs.path) @lhs.doc)"                  

--         , "      loc.pres = empty" --gerbo HACK! for testing
         ] <~ [cnstrName]
              
genSemListDecl (Decl (LHSListType typeName) _) = 
  [ "SEM List_%1"
  , "  | List_%1"
  , "      lhs.press = map ( loc (Node_List_%1 @self @lhs.path)" -- remove spaces
  , "                      . presentFocus @lhs.focusD @lhs.path )"
  , "                      @elts.press"
  , "                      -- parent is reponsible for setting parsing/structural"
  , "      elts.pIdC = @lhs.pIdC + 100 -- NOT RIGHT, should be taken from document type def."
  , "      lhs.pIdC = @elts.pIdC"
  , "      elts.path = @lhs.path"
  , "      elts.ix = 0"
--  , "      loc.pres = empty" --gerbo HACK! for testing
  , "  | HoleList_%1     lhs.press = []"
  , "  | ParseErrList_%1 lhs.press = []"
  , "  | List_%1"
  , "      lhs.pres = loc (Node_List_%1 @self @lhs.path) $ presentFocus @lhs.focusD @lhs.path $ @pres"
  , "                 `withLocalPopupMenuItems` (mkPopupItemsFromDocUpdates $ menuD (PathD @lhs.path) @lhs.doc)"                  
  , "  | ParseErrList_%1"
  , "      lhs.pres = presParseErr (Node_ParseErrList_%1 @self @lhs.path) @error"
  , "                 `withLocalPopupMenuItems` (mkPopupItemsFromDocUpdates $ menuD (PathD @lhs.path) @lhs.doc)"                  
  , "      lhs.parseErrors = getErrorMessages @error"
  , "  | HoleList_%1"
  , "      lhs.pres = presHole @lhs.focusD \"%1\" (Node_HoleList_%1 @self @lhs.path) @lhs.path"
  , "                 `withLocalPopupMenuItems` (mkPopupItemsFromDocUpdates $ menuD (PathD @lhs.path) @lhs.doc)"                  
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
          else "presentXML%2 @%1"
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
          then "presentPrimXML%2 @%1"
          else "presentXML%2 @%1"
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

