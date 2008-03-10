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
-}

import TypesUtils

import List

generate :: DocumentType -> [String]
generate docType = genDataType (addHolesParseErrs (addConsListDecls docTypeWithLists))
                ++ genSem (addHolesParseErrs (addConsListDecls docTypeWithLists))
                
  where docTypeWithLists = addListDecls docType

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

genSem decls = genBanner "Sem functions" $
  concatMap genSemDecl decls
 where genSemDecl decl@(Decl (LHSBasicType _) _) = genSemBasicDecl decl
       genSemDecl decl@(Decl (LHSListType _) _) = genSemListDecl decl
       genSemDecl decl@(Decl (LHSConsListType _) _) = genSemConsListDecl decl

genSemBasicDecl decl = [""]
genSemListDecl decl = [""]
genSemConsListDecl (Decl lhsType _) = [""]
{-
SEM ConsList_Decl
  | Cons_Decl head.path  = @lhs.path++[@lhs.ix]
             tail.path = @lhs.path
                 lhs.press = @head.pres : @tail.press
                 head.pIdC = @lhs.pIdC + 30 -- NOT RIGHT, should be taken from document type def.
                 tail.pIdC = @head.pIdC
                 lhs.pIdC = @tail.pIdC
  | Nil_Decl      lhs.press = []
-}