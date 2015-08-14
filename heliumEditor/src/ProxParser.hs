{-# LANGUAGE FlexibleContexts #-}
module ProxParser (recognizeEnrichedDoc) where

import Common.CommonTypes
import Presentation.PresLayerTypes
import Presentation.PresLayerUtils
import Presentation.PresentationParsing
import Presentation.XprezLib

import UU.Parsing

import Data.List hiding (delete)


import ProxParser_Generated
import Evaluation.DocumentEdit

import Data.Char

import DocTypes_Generated
import DocUtils_Generated


-------------------- Proxima Parser/Structure Recognizer -------------------- 


recognizeEnrichedDoc :: ProxParser EnrichedDoc
recognizeEnrichedDoc = pStr $
          (\str rootE -> reuseRootEnr [str] (Just rootE) Nothing)
      <$> pStructuralTk Node_RootEnr
      <*> recognizeRootE

recognizeRootE :: ProxParser RootE
recognizeRootE = pStr $ 
          (\str idlistdecls decls-> reuseRootE [str] Nothing (Just decls) (Just idlistdecls))
      <$> pStructuralTk Node_RootE
      <*> parseIDListList_Decl {- <* (pStr' $ pStructuralTk Node_List_Decl) -}  <*> recognizeList_Decl
                                {- tree or xml view-}

parseIDListList_Decl :: ProxParser List_Decl
parseIDListList_Decl = pPrs $
          (\dcls -> reuseList_Decl [] (Just $ toConsList_Decl dcls)) 
      <$> pList recognizeIDListDecl
             
recognizeIDListDecl :: ProxParser Decl
recognizeIDListDecl = pStr $
          (\str ident -> reuseDecl [str] Nothing Nothing Nothing Nothing Nothing Nothing (Just ident) Nothing)
      <$> pStructuralTk Node_Decl
      <*> parseIdListIdent
  <|>     (\str -> reuseBoardDecl [str] Nothing Nothing Nothing)
      <$> pStructuralTk Node_BoardDecl
  <|>     (\str -> reusePPPresentationDecl [str] Nothing Nothing Nothing)
      <$> pStructuralTk Node_PPPresentationDecl
     {- <|>  
                      (\str -> HoleDecl
                  <$> pSym declHoleTk
-}       

parseIdListIdent :: ProxParser Ident
parseIdListIdent =  pPrs $
          (\strTk -> reuseIdent [strTk] Nothing Nothing (Just $ tokenString strTk))
      <$> pLIdent 

-------------------- Chess board parser:

parseBoard = 
      ((\_ -> initBoard) <$> pKey "board")
  <|>     (\str -> reuseBoard [str] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) 
      <$> pStructuralTk Node_Board -- don't descend into structure, so no pres edit


-------------------- Powerpoint parser:
   
parsePPPresentation = 
      ((\_ -> initPPPresentation) <$> pKey "pres")
  <|> recognizePPPresentation

recognizePPPresentation = pStr $                       -- viewTp
         (\str list_slide -> reusePPPresentation [str] Nothing (Just list_slide))
     <$> pStructuralTk Node_PPPresentation
     <*> recognizeList_Slide
 
recognizeList_Slide = pStr $
         (\str slides -> reuseList_Slide [str] (Just $ toConsList_Slide slides)) 
     <$> pStructuralTk Node_List_Slide
     <*> pList recognizeSlide
  
        -- maybe make a recognizeConsList_Slide?

recognizeSlide =  pStr $
         (\str title itemList -> reuseSlide [str] (Just title) (Just itemList))
     <$> pStructuralTk Node_Slide
     <*> (pPrs $ tokenString <$> pLIdent) <*> recognizeItemList

recognizeItemList = pStr $                         -- ListType
         (\str listType list_item -> reuseItemList [str] (Just listType) (Just list_item))
     <$> pStructuralTk Node_ItemList
     <*> recognizeListType <*> recognizeList_Item

recognizeListType = pStr $
         (\str -> reuseBullet [str])
     <$> pStructuralTk Node_Bullet
  <|>    (\str -> reuseNumber [str])
     <$> pStructuralTk Node_Number
  <|>    (\str -> reuseAlpha [str])
     <$> pStructuralTk Node_Alpha

recognizeList_Item = pStr $
         (\str items -> reuseList_Item [str] (Just $ toConsList_Item items)) 
     <$> pStructuralTk Node_List_Item
     <*> pList recognizeItem

recognizeItem = pStr $ 
         (\str string -> reuseStringItem [str] (Just string))
     <$> pStructuralTk Node_StringItem
     <*> (pPrs $ tokenString <$> pLIdent)
  <|>    (\str helium -> reuseHeliumItem [str] (Just helium))
     <$> pStructuralTk Node_HeliumItem
     <*> recognizeExp
  <|>    (\str helium -> reuseListItem [str] (Just helium))
     <$> pStructuralTk Node_ListItem
     <*> recognizeItemList


recognizeExp =
         recognizeExp'
  <|>    pPrs parseExp
         

recognizeExp' = 
         (pStrAlt Node_DivExp $
         (\str e1 e2 -> reuseDivExp [str] (Just $ getTokenIDP str) (Just e1) (Just e2))
     <$> pStructuralTk Node_DivExp
     <*> recognizeExp
     <*> recognizeExp)
  <|>    (pStrAlt Node_HoleExp $
         HoleExp
     <$ pStructuralTk Node_HoleExp)

-------------------- Helium parser:


-- List_Decl, Decl

recognizeList_Decl = pPrs $
          parseList_Decl
        
parseList_Decl = 
          (\decls -> reuseList_Decl [] (Just $ toConsList_Decl decls))
      <$> pList parseDecl

parseDecl  =                                                              -- IDD  "="                   ";"                       type sig              not used  expanded    auto-layout
          (\sig ident tk1 exp tk2 -> reuseDecl [tk1, tk2] (Just $ getTokenIDP tk1) (Just $ getTokenIDP tk2) (typeSigTokenIDP sig) Nothing (Just True) Nothing (Just ident) (Just exp))
      <$> pMaybe (pStructuralTk Node_Decl) -- type sig/value
      <*> parseIdent <*> pKey "=" <*> parseExp  <*> pKeyC 1 ";"
  <|>     (\sig ident tk1 tk2 -> reuseDecl [tk1, tk2] (Just $ getTokenIDP tk1) Nothing (typeSigTokenIDP sig) Nothing Nothing Nothing (Just ident) Nothing)--makeDecl' mtk0 tk1 tk2 ident) 
      <$> pMaybe (pStructuralTk Node_Decl) -- type sig/value
      <*> parseIdent <*> pKey "=" <*> pKey "..." -- bit weird what happens when typing ... maybe this must be done with a structural presentation (wasn't possible before with structural parser that was too general)
 <|>      (\tk board ->  BoardDecl (getTokenIDP tk) NoIDP board) 
      <$> pKey "Chess" <* pKey ":" <*> parseBoard        
 <|>      (\tk slides ->  PPPresentationDecl (getTokenIDP tk) NoIDP slides)
      <$> pKey "Slides" <* pKey ":" <*> parsePPPresentation
 where typeSigTokenIDP Nothing   = Nothing
       typeSigTokenIDP (Just tk) = Just (getTokenIDP tk)

-- List_Alt, Alt

parseList_Alt =
         (\alts -> reuseList_Alt [] (Just $ toConsList_Alt alts))
     <$> pList parseAlt

parseAlt  = 
         (\ident tk1 exp tk2 -> reuseAlt [tk1, tk2] (Just $ getTokenIDP tk1) (Just $ getTokenIDP tk2) (Just ident) (Just exp))
     <$> parseIdent <*> pArrow <*> parseExp  <*> pKeyC 4 ";"


-- Exp

parseIdentExp = 
         (\ident -> reuseIdentExp [] (Just ident))
     <$> parseIdent

parseIfExp = 
         (\tk1 c tk2 th tk3 el -> reuseIfExp [tk1, tk2,tk3] (Just $ getTokenIDP tk1) (Just $ getTokenIDP tk2) (Just $ getTokenIDP tk3) (Just c) (Just th) (Just el))
     <$> pIf <*> parseExp <*> pThen <*> parseExp <*> pElse <*> parseExp

parseLamExp = 
         (\tk1 a tk2 b -> reuseLamExp [tk1, tk2] (Just $ getTokenIDP tk1) (Just $ getTokenIDP tk2) (Just a) (Just b))
     <$> pLambda <*> parseIdent <*> pArrow <*> parseExp
            
parseCaseExp =
         (\tk1 a tk2 b -> reuseCaseExp [tk1, tk2] (Just $ getTokenIDP tk1) (Just $ getTokenIDP tk2) (Just a) (Just b))
     <$> pCase <*> parseExp <*> pOf <*> parseList_Alt

parseLetExp =
         (\tk1 a tk2 b -> reuseLetExp [tk1, tk2] (Just $ getTokenIDP tk1) (Just $ getTokenIDP tk2) (Just a) (Just b))
     <$> pLet <*> parseList_Decl <*> pIn <*> parseExp

      
-- lists with separators are still a bit awkward
parseListExp = 
         (\tk1 (tks, list_Exp) tk2 -> reuseListExp ([tk1, tk2]++tks) (Just $ getTokenIDP tk1) (Just $ getTokenIDP tk2) (Just $ map getTokenIDP tks) (Just $ list_Exp))
     <$> pKey "[" <*> parseList_Exp <*> pKey "]"

-- same here
parseParenExp = -- maybe we don't want to build a list for (exp), because now we have to remove it
         (\tk1 (tks, list_Exp) tk2 -> if arity list_Exp == 1 
                                      then let Clip_Exp exp = select [0] list_Exp -- unsafe match, but will never fail due to check
                                           in  reuseParenExp [tk1, tk2] (Just $ getTokenIDP tk1) (Just $ getTokenIDP tk2) (Just $ exp)
                                      else reuseProductExp ([tk1, tk2]++tks) (Just $ getTokenIDP tk1) (Just $ getTokenIDP tk2) (Just $ map getTokenIDP tks) (Just $ list_Exp))
     <$> pKey "(" <*> parseList_Exp <*> pKey ")"


-- returns list of separator tokens and a List_Exp the List_Exp is not reused through its separator tokens
-- because these do not belong to List_Exp, but to its parent
parseList_Exp :: ProxParser ([Token Document EnrichedDoc Node ClipDoc UserToken], List_Exp)
parseList_Exp =
    (\toksElts -> let (toks, elts) = case toksElts of
                                       Nothing        -> ([], [])
                                       Just (e, etks) -> let (tks, es) = unzip etks
                                                         in  (tks, e:es)
                  in  (toks, reuseList_Exp [] (Just $ toConsList_Exp elts)))
     <$>  pMaybe (     (\e etks -> (e,etks)) 
                  <$> parseExp <*> pList ((,) <$> pKey "," <*> parseExp))
         
         
    
parseExp = -- use chain!!  and fix associativity of application!
          parseExp'   -- e and t are flipped in lambda for <??>
     <??> (    (\tk e t-> reusePlusExp [tk] (Just $ getTokenIDP tk) (Just t) (Just e))
           <$> pKey "+" <*> parseExp 
          )
          
parseExp' = 
           parseTerm   -- e and t are flipped in lambda for <??>
      <??> (    (\tk e t->  reuseDivExp [tk] (Just $ getTokenIDP tk) (Just t) (Just e))
            <$> pKey "%" <*> parseExp' 
           )
           
parseTerm   = 
           parseFactor
      <??> (      (\tk t f-> reuseTimesExp [tk] (Just $ getTokenIDP tk) (Just f) (Just t))
              <$> pKey "*" 
              <*> parseTerm
           <|>    (\tk t f-> reuseDivExp [tk] (Just $ getTokenIDP tk) (Just f) (Just t))
              <$> pKey "/"
              <*> parseTerm
           )

parseFactor = 
           parseFactor'
      <??> (    (\tk f f' -> reusePowerExp [tk] (Just $ getTokenIDP tk) (Just f') (Just f))
            <$> pKey "^" <*> parseFactor
           <|>
                (\(PowerExp id _ e2) f' -> PowerExp id f' e2)
            <$> parsePowerr
           )

parsePowerr = (pStrAlt Node_PowerExp $
         (\str e2 -> reusePowerExp [str] (Just $ getTokenIDP str) Nothing (Just e2))
     <$> pStructuralTk Node_PowerExp
     <*> recognizeExp) 

parseFactor' =   
           parseFactor''
      <??> (    (\f' f'' -> reuseAppExp [] (Just f'') (Just f'))
            <$> parseFactor')

parseFactor'' =
      parseIntExp 
  <|> parseBoolExp
  <|> parseIdentExp
  <|> parseListExp
  <|> parseParenExp
  <|> parseIfExp
  <|> parseLamExp
  <|> parseCaseExp
  <|> parseLetExp
  <|> recognizeExp'
     
parseIdent = 
         (\strTk -> reuseIdent [strTk] (Just $ getTokenIDP strTk) Nothing (Just $ tokenString strTk))
     <$> pLIdent



-------------------- Keyword parsers, remember to keep these consistent with keywords

pIf     = pKey "if"

pThen   = pKey "then"

pElse   = pKey "else"

pLambda = pKey "\\" -- <|> pKey "l"

pArrow  = pKey "->" -- <|> pKey "\174"

pCase   = pKey "case"

pOf     = pKey "of"

pLet    = pKey "let"

pIn     = pKey "in"

pTrue   = pKey "True"

pFalse  = pKey "False"

-------------------- more or less primitive parsers: (because int & bool are unboxed) -- not anymore
{-
obsolete
-- parseString needs to parse the ParseToken, rewrite, so it doesn't use reuseString
parseString_ = pPrs $
--           (\strTk -> reuseString_ [] (Just $ tokenString strTk)) 
          mkString_
     <$>  pLIdent     
     <|> (HoleString_ <$ pStructuralTk Node_HoleString_)


parseInt_ = pStr $
--           (\strTk -> reuseString_ [] (Just $ tokenString strTk)) 
          mkInt_
     <$>  pInt
     <|> (HoleInt_ <$ pStructuralTk Node_HoleInt_)
-- bit hacky


parseBool_ = pStr $
--           (\strTk -> reuseString_ [] (Just $ tokenString strTk)) 
         mkBool_
     <$> (True <$ pTrue <|> False <$ pFalse)
     <|> (HoleBool_ <$ pStructuralTk Node_HoleBool_)
-}

parseIntExp =
         (\tk -> reuseIntExp [tk] (Just $ getTokenIDP tk) (Just $ read (tokenString tk)))
     <$> pInt

parseBoolExp = 
         (\tk -> reuseBoolExp [tk] (Just $ getTokenIDP tk) (Just True))
     <$> pTrue
  <|>    (\tk -> reuseBoolExp [tk] (Just $ getTokenIDP tk) (Just False))
     <$> pFalse

  
--------------------------------------------------------------
--- testing bits
{-
clparse str  = let (prs,layoutmap,counter) = tokenize 0 Nothing . ParsingP NoIDP . StringP NoIDP $ str
                   tokens = enrichedDocTk : postScanStr prs Nothing
                   result = runParser recognizeRootEnr tokens
               in  debug Err ("Parsing: "++show (tokens)++"\nhas result:") $
                   result 
clparsep p str  = let (prs,layoutmap,counter) = tokenize 0 Nothing . ParsingP NoIDP . StringP NoIDP $ str
                      tokens = postScanStr prs Nothing
                      result = runParser p tokens
                  in  debug Err ("Parsing: "++show (tokens)++"\nhas result:") $
                      result 

-}
--------------------------------------------------------------




-- UNCLEAR:
-- default: what to do with things like HeliumTypeInfo? answer: declare as Editable and specify hole

--Design issues with parsing and structure recognizing (choose different name? recognizer is usually parser with Bool result)

-- TODO: 


-- put "tokenNode" application in generated code, now it appears everywhere
-- put general extractFromNodes (rename to reuseFrom Tokens) in begin part of Parser_Generated (after new generater is used)

-- do things for parseErr and Hole in structure recognition. This should be done with a class,
-- so pStr can take care of it. See pStr definition
-- first, ignore, then do it explicitly in each recognizer, finally do it hidden in pStr


-- what do we do with "lIdentVal" in parseString_?


-- where to put pStr, for structurals inside and for parsers outside? parsers calling each other should
-- not have a pStr. (parsing in parsing does not (and should not) give rise to a Parsing node from postScan pres)
-- maybe have recognizeBla do the pStr, and have parseBla without


-- recognize should take into account the presentation, which must be present in the structural token somewhere


--PROBLEM: When several structural presentations for one type exist, we need a way to determine which recognizer to use.
--For example tree node with children or without. A parser would use the keyword "+" or "-", but in the recognizer
--we somehow have to look at the boolean expansion value of the recognized node since parsing an image of + or - is not
--an option.

-----------------------





{-
comments from old parser

The id of the origin is used to set the id of the doc element. 
(only succeeds if origin was of same type)
In case of several symbols (eg. if .. then .. else ..fi): if 1st fails, try 2nd, etc.


For each parse, reuse everything that is not in the parse

-}

 
-- ******** rename plus to sum
-- remember to that "Chess", "Slides", "board", and "pres" must be keywords in PresentationParsing


{-
Parser notes

inserted layout? 

a = + 2; -> a = HOLE + 2;        single space
a = 2 -> a = 2;                          no whitespace


f = 1;

= 2;

->

f = 1;

HOLE = 2;                                 copied from following token


-}










-- User token values that can be used to construct basic token parsers

keyTk str = UserTk 0 (KeyTk str) str Nothing (IDP (-1))
intTk     = UserTk 0 IntTk "0" Nothing (IDP (-1))
lIdentTk  = UserTk 0 LIdentTk "ident" Nothing (IDP (-1))
uIdentTk  = UserTk 0 UIdentTk "Ident" Nothing (IDP (-1))
opTk      = UserTk 0 OpTk "" Nothing (IDP (-1))
symTk     = UserTk 0 SymTk "" Nothing (IDP (-1))
-- (IDP (-1)) means inserted token. This should be handled by some kind of 'fresh' attribute
-- which is also required for copying of presentation subtrees (only if we use error correcting)






-- Basic parsers

pKey :: DocNode node => String -> ListParser doc enr node clip UserToken (Token doc enr node clip UserToken)
pKey str = pSym  (keyTk str)

pKeyC :: DocNode node => Int -> String -> ListParser doc enr node clip UserToken (Token doc enr node clip UserToken)
pKeyC c str = pSym  (keyTk str) -- pCSym c (keyTk str)

-- expensive, because we want holes to be inserted, not strings
pLIdent :: DocNode node => ListParser doc enr node clip UserToken (Token doc enr node clip UserToken)
pLIdent = pSym lIdentTk

-- todo return int from pInt, so unsafe intVal does not need to be used anywhere else
pInt :: DocNode node => ListParser doc enr node clip UserToken (Token doc enr node clip UserToken)
pInt = pSym  intTk

lIdentVal :: DocNode node => Token doc enr node clip UserToken -> String
lIdentVal (UserTk _ LIdentTk str _ _) = str
lIdentVal tk                          = debug Err ("PresentationParser.lIdentVal: no IdentTk " ++ show tk) "x"

  
intVal :: DocNode node => Token doc enr node clip UserToken -> Int
intVal (UserTk _ IntTk "" _ _)  = 0   -- may happen on parse error (although not likely since insert is expensive)
intVal (UserTk _ IntTk str _ _) = read str
intVal tk              = debug Err ("PresentationParser.intVal: no IntTk " ++ show tk) (-9999)

 
