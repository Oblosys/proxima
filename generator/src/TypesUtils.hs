-----------------------------------------------------------------------------------------
{-| Module      : Types
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module TypesUtils where


data Decl = Decl TypeName [Prod] deriving Show

data Prod = Prod ConstructorName [Field] deriving Show

data Field = Field FieldType FieldName Type deriving Show

data FieldType = IDP | Regular deriving Show

data Type = BasicType TypeName
          | ListType  TypeName deriving Show
          
type TypeName = String

type ConstructorName = String

type FieldName = String

getTypeName (BasicType typeName) = typeName
getTypeName (ListType typeName)  = "List_"++typeName


