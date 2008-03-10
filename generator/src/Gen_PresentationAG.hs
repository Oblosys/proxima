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

{-
RUI's ipd computation was wrong:
node without children
should have
 lhs.pIdC = @lhs.pIdC + n
 
 Anyway, this is too buggy, so we should use FreshID and fill it in after presenting. (maybe during Layout)



TODO change order and layout after diff
-}

import TypesUtils

import List

generate :: DocumentType -> [String]
generate docType = genDataType (addHolesParseErrs (addConsListDecls docTypeWithLists))
                ++ genSem (addConsListDecls' docTypeWithoutEnrWithLists)
                ++ genAttr docType -- some attributes are declared on EnrichedDoc
                ++ genSemXML (addConsListDecls' docTypeWithoutEnrWithLists)
                ++ genSemTree (addConsListDecls' docTypeWithoutEnrWithLists)
  where docTypeWithoutEnrWithLists = addListDecls (removeEnrichedDocDecl docType)
        docTypeWithLists = addListDecls docType
-- the behavior for holes and parse errors is too different, therefore we do not add them to the type
-- but just generate code for them in the gen functions.

-- TODO remove space
genAttr decls = genBanner "Attr declarations" $
  [ "ATTR  %1"
  , "       [ |  pIdC : Int  | ]"
  , ""
  , ""
  , "ATTR  %1"
  , "       [ focusD : FocusDoc | | ]"
  , ""
  , ""
  , "ATTR  %2"
  , "       [ | path : {[Int]} | ]"
  , ""
  , "" -- TODO newline
  , "ATTR  %3 [ | | press : {[Presentation_Doc_Node_Clip_Token]} ]"
  , ""
  , "" -- TODO newline
  , "ATTR  %4 [ | | pres : Presentation_Doc_Node_Clip_Token ]"
  , ""
  , "" -- TODO newline
  , "ATTR  %5 [ ix : Int || ]"
  ] <~ [ separateBy " " $ getAllDeclaredTypeNames (addConsListDecls (addListDecls decls))
       , separateBy " " $ getAllDeclaredTypeNames (removeEnrichedDocDecl (addConsListDecls (addListDecls decls)))
       , separateBy " " $ [ "List_" ++ typeName tpe ++ " ConsList_" ++ typeName tpe 
                          | tpe <- getAllUsedListTypes decls ]
       , separateBy " " $ getAllDeclaredTypeNames decls
       , separateBy " " $ map typeName $ getAllUsedListTypes decls 
       ]
 
genDataType decls = genBanner "AG data type" $
  concatMap genDataDecl decls
 where genDataDecl (Decl lhsType prods) = 
         "DATA %1" <~ [genTypeName lhsType] :
         [ "  | " ++ genProd prod | prod <- prods] ++
         [ "", "" ]
        where genProd (Prod _ cnstrName idpFields fields) =
                cnstrName ++ (prefixBy " " $ map genIDPField idpFields ++
                                             map genField fields)
                      
       genIDPField (Field fieldName fieldType) = fieldName ++ ":" ++ genIDPTypeAG  fieldType
       genField    (Field fieldName fieldType) = fieldName ++ ":" ++ genTypeAG fieldType

genSem decls = genBanner "General sem functions" $
  concatMap genSemDecl decls
 where genSemDecl decl@(Decl (LHSBasicType _) _)    = genSemBasicDecl decls decl
       genSemDecl decl@(Decl (LHSListType _) _)     = genSemListDecl decl
       genSemDecl decl@(Decl (LHSConsListType _) _) = genSemConsListDecl decl


{-

SEM Exp
  | PlusExp exp1.pIdC = @lhs.pIdC + 1
            exp2.pIdC = @exp1.pIdC
            lhs.pIdC = @exp2.pIdC
  | CaseExp exp.pIdC = @lhs.pIdC + 2
            alts.pIdC = @exp.pIdC
            lhs.pIdC = @alts.pIdC
  | LetExp decls.pIdC = @lhs.pIdC + 2
           exp.pIdC = @decls.pIdC
           lhs.pIdC = @exp.pIdC
  | IdentExp ident.pIdC = @lhs.pIdC + 0
             lhs.pIdC = @ident.pIdC
  | IfExp exp1.pIdC = @lhs.pIdC + 3
          exp3.pIdC = @exp2.pIdC
          exp2.pIdC = @exp1.pIdC
          lhs.pIdC = @exp3.pIdC
  | IntExp lhs.pIdC = @lhs.pIdC +1
  
  | ParenExp exp.pIdC = @lhs.pIdC + 2
             lhs.pIdC = @exp.pIdC
  | ListExp exps.pIdC = @lhs.pIdC + 3
            lhs.pIdC = @exps.pIdC
  | ProductExp exps.pIdC = @lhs.pIdC + 3
               lhs.pIdC = @exps.pIdC
  | HoleExp     lhs.pres = presHole @lhs.focusD "Exp" (HoleExpNode @self @lhs.path) @lhs.path
  | ParseErrExp lhs.pres = presParseErr @presentation

-}
-- IDP computation has some problems, but will probably be obsolete soon

--TODO: remove zipWith, remove reverse init crap
genSemBasicDecl decls (Decl (LHSBasicType typeName) prods) = 
  "SEM %1" <~ [typeName] :
  concatMap genSemPIDCProd prods ++
  [ "  | Hole%1     lhs.pres = presHole @lhs.focusD \"%1\" (Hole%1Node @self @lhs.path) @lhs.path"
  , "  | ParseErr%1 lhs.pres = presParseErr @presentation"
  ] <~ [typeName] ++
  [ "", "" ] ++ --remove
  let pathSemLines = concatMap genSemPathProd prods
  in  if null pathSemLines then []
      else "SEM %1" <~ [typeName] : pathSemLines ++ ["",""]
 where genSemPIDCProd (Prod _ cnstrName idpFields fields) =
         let agFields = filter (isDeclaredType decls . fieldType) fields
             -- only take into account fields that have AG types (instead of prim or other composite)
         in  zipWith (++) ("  | %1 " <~ [cnstrName] : repeat (replicate (5+length cnstrName) ' '))
                          (addPlus $ zipWith (\l r -> "%1.pIdC = @%2.pIdC" <~ [l,r])
                                             ( map fieldName agFields ++ ["lhs"])
                                             ( "lhs" : map fieldName agFields))
        where addPlus (l:ls) = (l++ " + " ++ show (length idpFields)) : if null ls then [] else reverse (init ls) ++ [last ls]
              -- TODO: this computationgoes wrong when there are lists of idps

       genSemPathProd (Prod _ cnstrName idpFields fields) =
         zipWith (++) ("  | %1 " <~ [cnstrName] : repeat (replicate (5+length cnstrName) ' '))
                      [ ("%1.path  = @lhs.path++["++show i++"]") <~ [fieldName field] 
                      | (i,field) <- zip [0..] fields, isDeclaredType decls $ fieldType field 
                      ] -- only generate for AG field types, but do include the others in the index computation
{-
SEM Exp
  | PlusExp exp1.path  = @lhs.path++[0]
            exp2.path  = @lhs.path++[1]
  | LetExp decls.path  = @lhs.path++[0]
           exp.path  = @lhs.path++[1]
  | IdentExp ident.path  = @lhs.path++[0]
  | IfExp exp1.path  = @lhs.path++[0]
          exp2.path  = @lhs.path++[1]
          exp3.path  = @lhs.path++[2]
  | ParenExp exp.path  = @lhs.path++[0]
  | ListExp exps.path  = @lhs.path++[0]
  | ProductExp exps.path  = @lhs.path++[0]
-}

genSemListDecl (Decl (LHSListType typeName) _) = 
  [ "SEM List_%1"
  , "  | List_%1"
  , "      lhs.press = map ( loc (List_%1Node @self @lhs.path) " -- remove spaces
  , "                      . presentFocus @lhs.focusD @lhs.path ) "
  , "                      @elts.press"
  , "                      -- parent is reponsible for setting parsing/structural"
  , "      elts.pIdC = @lhs.pIdC + 100 -- NOT RIGHT, should be taken from document type def."
  , "      lhs.pIdC = @elts.pIdC"
  , "      elts.path = @lhs.path"
  , "      elts.ix = 0"
  , "  | HoleList_%1     lhs.press = []"
  , "  | ParseErrList_%1 lhs.press = [ presParseErr @presentation ]"
  , ""
  ] <~ [typeName]

genSemConsListDecl (Decl (LHSConsListType typeName) _) = 
  [ "SEM ConsList_%1"
  , "  | Cons_%1 head.path  = @lhs.path++[@lhs.ix]"
  , "             tail.path = @lhs.path"
  , "                 lhs.press = @head.pres : @tail.press"
  , "                 head.pIdC = @lhs.pIdC + 30 -- NOT RIGHT, should be taken from document type def."
  , "                 tail.pIdC = @head.pIdC"
  , "                 lhs.pIdC = @tail.pIdC"
  , "  | Nil_%1      lhs.press = []"
  , "" -- put together after diff
  , "" -- put together after diff
  , "  SEM ConsList_%1 [ ix : Int | | ]"
  , "  | Cons_%1     tail.ix  = @lhs.ix + 1"
  , ""
  ] <~ [typeName]


  
genSemXML decls = genBanner "Sem functions for XML presentation" $
  concatMap genSemXMLDecl decls
 where genSemXMLDecl decl@(Decl (LHSBasicType _) _)    = genSemXMLBasicDecl decls decl
       genSemXMLDecl decl@(Decl (LHSListType _) _)     = genSemXMLListDecl decl
       genSemXMLDecl decl@(Decl (LHSConsListType _) _) = genSemXMLConsListDecl decl

-- TODO : use {(Presentation Document Node ClipDoc UseToken)}
genSemXMLBasicDecl decls (Decl (LHSBasicType typeName) prods) =  
  "SEM %1 [ || presXML : Presentation_Doc_Node_Clip_Token ]" <~ [typeName] : concat
  [ [ "  | %1"
    , "      lhs.presXML = presentElementXML @lhs.focusD (%1Node @self @lhs.path) @lhs.path \"%1\" [ %2 ] "
    ] <~ [ cnstrName, separateBy ", " $ map genField fields ] 
  | Prod _ cnstrName idpFields fields <- prods
  ] ++
  ([ "  | Hole%1     lhs.presXML = presHole @lhs.focusD \"%1\" (Hole%1Node @self @lhs.path) @lhs.path"
   , "  | ParseErr%1 lhs.presXML = presParseErr @presentation"
   , ""
   ] <~ [typeName])
 where genField (Field fieldName fieldType) = 
         (if isDeclaredType decls fieldType
          then "@%1.presXML"
          else "presentPrimXML%2 @%1") <~ [ fieldName, genTypeAG fieldType]  

genSemXMLListDecl (Decl (LHSListType typeName) _) = 
  [ "SEM List_%1 [ || presXML : Presentation_Doc_Node_Clip_Token ]"
  , "  | List_%1"
  , "      lhs.presXML = loc (List_%1Node @self @lhs.path) $ structural $ presentFocus @lhs.focusD @lhs.path $"
  , "                    col @elts.pressXML"
  , "  | ParseErrList_%1"
  , "      lhs.presXML = loc (List_%1Node @self @lhs.path) $ structural $ presentFocus @lhs.focusD @lhs.path $"
  , "                    presParseErr @presentation"
  , "  | HoleList_%1"
  , "      lhs.presXML = loc (List_%1Node @self @lhs.path) $ structural $ presentFocus @lhs.focusD @lhs.path $"
  , "                    presHole @lhs.focusD \"List_%1\" (HoleList_%1Node @self @lhs.path) @lhs.path"
  , ""
  ] <~ [typeName]

genSemXMLConsListDecl (Decl (LHSConsListType typeName) _) = 
  [ "SEM ConsList_%1 [ | | pressXML : {[Presentation_Doc_Node_Clip_Token]} ]"
  , "  | Cons_%1     lhs.pressXML  = @head.presXML : @tail.pressXML"
  , "  | Nil_%1      lhs.pressXML  = []"
  , ""
  ] <~ [typeName]



  
genSemTree decls = genBanner "Sem functions for tree presentation" $
  concatMap genSemTreeDecl decls
 where genSemTreeDecl decl@(Decl (LHSBasicType _) _)    = genSemTreeBasicDecl decls decl
       genSemTreeDecl decl@(Decl (LHSListType _) _)     = genSemTreeListDecl decl
       genSemTreeDecl decl@(Decl (LHSConsListType _) _) = genSemTreeConsListDecl decl

-- TODO : use {(Presentation Document Node ClipDoc UseToken)}
genSemTreeBasicDecl decls (Decl (LHSBasicType typeName) prods) =  
  "SEM %1 [ || presTree : Presentation_Doc_Node_Clip_Token ]" <~ [typeName] : concat
  [ [ "  | %1"
    , "      lhs.presTree = presentElementTree @lhs.focusD (%1Node @self @lhs.path) @lhs.path \"%1\" [ %2 ] "
    ] <~ [ cnstrName, separateBy ", " $ map genField fields ] 
  | Prod _ cnstrName idpFields fields <- prods
  ] ++
  ([ "  | Hole%1     lhs.presTree = presHole @lhs.focusD \"%1\" (Hole%1Node @self @lhs.path) @lhs.path"
   , "  | ParseErr%1 lhs.presTree = presParseErr @presentation"
   , ""
   ] <~ [typeName])
 where genField (Field fieldName fieldType) = 
         (if isDeclaredType decls fieldType
          then "@%1.presTree"
          else "presentPrimTree%2 @%1") <~ [ fieldName, genTypeAG fieldType]  

{-
SEM Decl [ || presTree : Presentation_Doc_Node_Clip_Token ]
  | Decl
      lhs.presTree = presentElementTree @lhs.focusD (DeclNode @self @lhs.path) @lhs.path "Decl" [ 
      presentPrimTreeBool @expanded, presentPrimTreeBool @autoLayout, @ident.presTree, @exp.presTree ] 
  | BoardDecl
      lhs.presTree = presentElementTree @lhs.focusD (BoardDeclNode @self @lhs.path) @lhs.path "BoardDecl" [ @board.presTree ] 
  | PPPresentationDecl
      lhs.presTree = presentElementTree @lhs.focusD (PPPresentationDeclNode @self @lhs.path) @lhs.path "PPPresentationDecl" [ @pPPresentation.presTree ] 
  | HoleDecl     lhs.presTree = presHole @lhs.focusD "Decl" (HoleDeclNode @self @lhs.path) @lhs.path
  | ParseErrDecl lhs.presTree = presParseErr @presentation
-}

genSemTreeListDecl (Decl (LHSListType typeName) _) = 
  [ "SEM List_%1 [ || presTree : Presentation_Doc_Node_Clip_Token ]"
  , "  | List_%1"
  , "      lhs.presTree = loc (List_%1Node @self @lhs.path) $ structural $ presentFocus @lhs.focusD @lhs.path $"
  , "                       col @elts.pressTree"
  , "  | ParseErrList_%1"
  , "      lhs.presTree = loc (List_%1Node @self @lhs.path) $ structural $ presentFocus @lhs.focusD @lhs.path $"
  , "                       presParseErr @presentation"
  , "  | HoleList_%1"
  , "      lhs.presTree = loc (List_%1Node @self @lhs.path) $ structural $ presentFocus @lhs.focusD @lhs.path $"
  , "                       presHole @lhs.focusD \"List_%1\" (HoleList_%1Node @self @lhs.path) @lhs.path"
  , ""
  ] <~ [typeName]

genSemTreeConsListDecl (Decl (LHSConsListType typeName) _) = 
  [ "SEM ConsList_%1 [ | | pressTree : {[Presentation_Doc_Node_Clip_Token]} ]"
  , "  | Cons_%1     lhs.pressTree  = @head.presTree : @tail.pressTree"
  , "  | Nil_%1      lhs.pressTree  = []"
  , ""
  ] <~ [typeName]

  