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
import Control.Monad (liftM)
import qualified Data.Map as Map
import Data.List ((\\), nub)
import Data.Maybe (isNothing)

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
    ; fields <- choice [ liftM concat $ try $ braces $ pRecord `sepBy1` reservedOp "," -- gerbo: added records
                       , pFields
                       ] 
                       -- order is important, otherwise records aren't parsed
    ; idpFields <- pIDPFields
    ; return $ Prod ExplicitProd constructorName idpFields fields
    }

-- generate only indexes for types that:
--   * don't have a name yet, and
--   * occur more than once in the field
pFields = 
 do { flds <- many pField
    ; let unnamedTys = map snd $ filter (isNothing . fst) flds -- all types without a field name
    ; let duplicates = Map.fromList $ zip (unnamedTys \\ nub unnamedTys) (repeat 0) -- all duplicates, with count 0
    ; return $ makeNames flds duplicates
    }
  where 
    makeNames ((fld,tpe):xs) cnts = Field fieldName tpe : makeNames xs counts
      where (fieldName,counts) = case fld of
                Just fn -> (fn, cnts)
                Nothing -> (fieldNameFromType tpe ++ (maybe "" show $ Map.lookup tpe counts')
                           , counts'
                           )
            counts'   = Map.adjust (+1) tpe cnts
    makeNames [] _ = []



pField =
 do { mFieldName <- option Nothing $ do { fieldName <- lcIdentifier
                                        ; reservedOp ":"
                                        ; return $ Just fieldName
                                        }
    ; tpe <- pType
    ; return $ (mFieldName, tpe)
    }
    
pRecord = -- gerbo 
 do { fieldNames <- lcIdentifier `sepBy1` reservedOp "," -- ``foo, bar
    ; reservedOp "::"                                    --   ::
    ; tpe <- pType                                       --     Zwoink''
    ; return $ map (\fn -> Field fn tpe) fieldNames      -- to ``[Field foo Zwoink,Field bar Zwoink]''
    }

pIDPFields = option [] $ braces pFields

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

