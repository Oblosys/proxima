-----------------------------------------------------------------------------------------
{-| Module      : Parser
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Parser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellDef )
import Char

import TypesUtils

type ParserState = ()

initState = ()

lexer :: P.TokenParser ParserState
lexer  = P.makeTokenParser $ haskellDef

whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
symbol     = P.symbol lexer
natural    = P.natural lexer
parens     = P.parens lexer
semi       = P.semi lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
braces     = P.braces lexer
squares    = P.squares lexer

parseDocumentType :: String -> IO DocumentType
parseDocumentType filePath =
 do { input <- readFile filePath
    ; case runParser pDocumentType initState filePath input of
        Right docType -> return docType
        Left  errors  -> stop $ "Parse error:\n"++show errors
    }

pDocumentType :: CharParser ParserState [Decl] 
pDocumentType =
 do { whiteSpace 
    ; decls <- many pDecl
    ; eof
    ; return decls
    }
    
pDecl =
 do { reserved "data"
    ; typeName <- ucIdentifier
    ; reservedOp "="
    ; prods    <- pProd `sepBy1` reservedOp "|" 
    ; return $ Decl (LHSBasicType typeName) prods
    }

pProd = 
 do { constructorName <- ucIdentifier
    ; fields <- many pField
    ; idpFields <- pIDPFields
    ; return $ Prod ExplicitProd constructorName idpFields fields
    }


pField =
 do { mFieldName <- option Nothing $ do { fieldName <- lcIdentifier
                                        ; reservedOp ":"
                                        ; return $ Just fieldName
                                        }
    ; tpe <- pType
    
    ; fieldName <- case mFieldName of
                     Nothing        -> return $ fieldNameFromType tpe
                     Just fieldName -> return fieldName
                 
    ; return $ Field fieldName tpe
    }
    


pIDPFields = option [] $ braces $ many $ pField

pType = choice
  [ do { typeName <- ucIdentifier
       ; return $ BasicType typeName
       }
  , do { typeName <- squares ucIdentifier 
       ; return $ ListType typeName
       } 
  ]

lcIdentifier = try $
 do { str <- identifier
    ; case str of
        c : _ -> if isLower c 
                 then return str
                 else fail "Lower case identifier expected"
        []     -> return str -- does not occur
    }
   
ucIdentifier = try $
 do { str <- identifier
    ; case str of
        c : _ -> if isUpper c 
                 then return str
                 else fail "Upper case identifier expected"
        []     -> return str -- does not occur
    }