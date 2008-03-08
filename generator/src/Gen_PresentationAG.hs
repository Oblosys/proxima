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