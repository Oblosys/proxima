-----------------------------------------------------------------------------------------
{-| Module      : Gen_DocUtils
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Gen_DocUtils where

import TypesUtils

generate :: DocumentType -> [String]
generate docType = []
  where docTypeWithLists = docType ++ genListDecls docType
