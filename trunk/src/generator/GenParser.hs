module GenParser where

import GenCommon
import UU.Parsing
import UU.Parsing.CharParser
import List

---------------------------------------------------------------------
--       P A R S E R                                               --
---------------------------------------------------------------------

parseDataTypesFile fname = parseFile show pDataDefs fname

pDataDefs :: CharParser  File --(String,DataDefs)
pDataDefs = (File "")  <$> (pSpaces *> pList_ng (pDecl))   -- pToken "module" <*> pConid <* pToken "where"                  

pDecl = Decl <$ pToken "data"
              <*> pConid
              <*  pSpec '='
              <*> pList1Sep (pSpec '|') pProd
              <*  (pDeriving <|> pSucceed ())
              <*> pSucceed (DeclDef)


pProd  = Prod <$> pConid <*> (pField <|> (const [] <$> pSpaces))

pField :: CharParser [Field]
pField =  flip (++) <$> pList (makeField <$> opt (Just <$> pVarid <* (pToken ":")) Nothing
                                                                 <*> (((\a->(a,(dataType a)))<$>pConid) <|> ((\a->(a++"s",List))<$>pDataList'))) 
                     <*> pComments

dataType a = if (elem a ["Bool","String","Int"]) then Prim else Data

makeField Nothing     (tpName, tp) = Field  (decapitalize tpName) tpName tp
makeField (Just name) (tpName, tp) = Field name tpName tp



pComments :: CharParser [Field]
pComments =  (pToken "{") *> pList (makeField 
                                      <$> opt (Just <$> pVarid <* (pToken ":")) Nothing -- pList ((\a b ->Field a (fst b) (snd b)) <$> pVarid <* (pToken ":") 
                                      <*> (  ((\a->(a, Id))<$>pConid) 
                                      <|> ((\a->(a++"s", Ids))
                                      <$> pDataList')))  
           <* (pToken "}")

pDataList :: CharParser String
pDataList  = (\a b c ->a++b++c) <$> (pToken "[") <*> pConid <*> (pToken "]")

pDataList' :: CharParser String
pDataList' = (\a ->a++"s") <$> (pToken "[") *> pConid <* (pToken "]")

 


pDeriving = () <$ pToken "deriving" <* (() <$ pConid <|> () <$ pParens (pList1Sep (pSpec ',') pConid))











-- CharParser ----------------------------------------------

lexeme :: IsParser p Char => p a -> p a
lexeme p = p <* pSpaces

pSpaces :: IsParser p Char => p ()
pSpaces = () <$ pList_ng (pSpace <|> pComment <|> pComment2)  <?> "whitespace"

pSpace :: IsParser p Char => p ()
pSpace = () <$ pAnySym (nub " \LF\CR\n\r\t\v\f") <?> "a space"

pComment :: IsParser p Char => p ()
pComment = () <$ pToks "--" <* pList pAnyChar <* pSym '\n'
  where pAnyChar = ' ' <..> '~' <|> pSym '\t' <?> "any character"


pComment2 :: IsParser p Char => p ()
--pComment2 = undefined 
pComment2 = () <$ pToks "{-" <* contents 
               

contents :: IsParser p Char => p ()
contents = (pAnyChar <|>  (() <$  pSym '}')) <* contents 
                 <|> pSym '-' *> complex
                 <|> pSym '{' *> complex2 
  where
        complex  :: IsParser p Char => p ()
        complex  = (() <$ pSym '}') <|> (pAnyChar <|> (() <$ pSym '{')) <* contents <|>  (pSym '-' *> complex)
        complex2 :: IsParser p Char => p ()
        complex2 = pSym '-' *> contents *> contents <|> (pAnyChar <|> (() <$pSym '}')) <* contents <|>  (pSym '{' *> complex2)
        pAnyChar :: IsParser p Char => p ()
        pAnyChar =  () <$ ( '!' <..> ','
                 <|> '.' <..> 'z'
                 <|> pSym '~' <|> pSym '|' )
                 <|> pSpace <?> "any character"


pSym' :: IsParser p Char => p ()
pSym' = (() <$ pSym '-') 


pUpper,pLower,pIdentLetter :: IsParser p Char => p Char
pUpper = 'A' <..> 'Z'
pLower = 'a' <..> 'z'
pIdentLetter = pUpper <|> pLower <|> '0' <..> '9' <|> pSym '_'

pInteger :: IsParser p Char => p Int
pInteger = lexeme ((\s v -> s (read v)) <$>  pSign <*> pList1 ('0' <..> '9')) <?> "an integer"
 where pSign = (id <$ pSym '+' <|> negate <$ pSym '-') `opt` id

pSpec :: IsParser p Char => Char -> p Char
pSpec x = lexeme ( pSym x)              <?> "symbol " ++ show x

pToken :: IsParser p Char => String -> p String
pToken t = lexeme (pToks t)              <?> "symbol " ++ show t

pComma :: IsParser p Char => p Char
pComma = pSpec ','

pParens :: IsParser p Char => p a -> p a
pParens p = pPacked (pSpec '(') (pSpec ')') p

pConid, pVarid, pString :: IsParser p Char => p String
pConid  = lexeme ((:) <$> pUpper <*> pList pIdentLetter) <?> "an uppercase identifier"

pVarid  = lexeme ((:) <$> pLower <*> pList pIdentLetter) <?> "an identifier"
pString = lexeme (pSym '"' *> pList pAnyChar <* pSym '"')
  where pAnyChar = ('#' <..> '~' <|> pSym '!'<|> pSym ' ') <?> "any character"

------------------------------------------------------------

{-

showAsList = printList "[]" "[" "]" ", "

printList e o c s xs = case xs of
  []     -> [e]
  (x:xs) -> (o ++ x) : map (s ++) xs  ++ [c]
-}

