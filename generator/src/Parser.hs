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

lexer :: P.TokenParser Int
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

parseDocumentType :: String -> IO ()
parseDocumentType filePath =
 do { input <- readFile filePath
    ; case runParser pDocumentType 0 filePath input of
        Right decls   -> print decls
                           
        Left  errors  -> do { putStr "parse error at "
                            ; print errors
                            } 
    }

pDocumentType :: CharParser Int [Decl] -- The state is for uniquely naming anonymous fields
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
    ; return $ Decl typeName prods
    }

pProd = 
 do { constructorName <- ucIdentifier
    ; setState 1
    ; fields <- many (pField Regular)
    ; idPFields <- pIDPFields
    ; return $ Prod constructorName (idPFields ++ fields)
    }


pField fieldType =
 do { mFieldName <- option Nothing $ do { fieldName <- lcIdentifier
                                        ; reservedOp ":"
                                        ; return $ Just fieldName
                                        }
    ; tpe <- pType
    
    ; fieldName <- case mFieldName of
                     Nothing        -> do { ix <- getState
                                          ; setState (ix+1)
                                          ; return $ mkFieldName (getTypeName tpe) ++ show ix
                                          }
                     Just fieldName -> return fieldName
                 
    ; return $ Field fieldType fieldName tpe
    }
    
mkFieldName ""     = error "Types.mkFieldName: empty typeName"
mkFieldName (c:cs) = toLower c : cs


pIDPFields = option [] $ braces $ many $ pField IDP

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