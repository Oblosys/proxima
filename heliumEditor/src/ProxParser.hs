module ProxParser (recognizeRootEnr) where

import CommonTypes
import PresLayerTypes
import PresLayerUtils hiding ((<*),(<*>),(<$),(<$>))
import PresentationParsing
import XprezLib

import UU_Parsing hiding (Exp, parse)

import List hiding (delete)


import ProxParser_Generated
import DocumentEdit
import DocumentEdit_Generated

import qualified UU_Parsing
import Char

import DocTypes_Generated
import DocUtils_Generated (initBoard, initPPPresentation)


-------------------- Proxima Parser/Structure Recognizer -------------------- 


recognizeRootEnr :: ListParser Document Node ClipDoc UserToken EnrichedDoc
recognizeRootEnr = pStr $ 
          (\str root -> reuseRootEnr [str] (Just root) Nothing Nothing)
      <$> pSym (StructuralTk (Just $ RootEnrNode HoleEnrichedDoc []) empty [] NoIDP) -- EnrichedDoc is not instance of Editable
      <*> recognizeRootE
  <|>    RootEnr (error "doc hole was parsed") (error "doc hole was parsed") (error "doc hole was parsed")
     <$ pStructural HoleEnrichedDocNode

recognizeRootE :: ListParser Document Node ClipDoc UserToken RootE
recognizeRootE = pStr $ 
          (\str idlistdecls decls-> reuseRootE [str] Nothing (Just decls) (Just idlistdecls))
      <$> pStructural RootENode
      <*> parseIDListList_Decl {- <* (pStr' $ pStructural List_DeclNode) -}  <*> recognizeList_Decl
                                {- tree or xml view-}

-- ?remove pStr from this parser?
parseIDListList_Decl :: ListParser Document Node ClipDoc UserToken List_Decl
parseIDListList_Decl = pPrs $
          (\dcls -> reuseList_Decl [] (Just $ toConsList_Decl dcls)) 
      <$> pList recognizeIDListDecl
             
recognizeIDListDecl :: ListParser Document Node ClipDoc UserToken Decl
recognizeIDListDecl = pStr $
          (\str ident -> reuseDecl [str] Nothing Nothing Nothing Nothing Nothing Nothing (Just ident) Nothing)
      <$> pStructural DeclNode
      <*> parseIdListIdent
  <|>     (\str -> reuseBoardDecl [str] Nothing Nothing Nothing)
      <$> pStructural BoardDeclNode
  <|>     (\str -> reusePPPresentationDecl [str] Nothing Nothing Nothing)
      <$> pStructural PPPresentationDeclNode
     {- <|>  
                      (\str -> HoleDecl
                  <$> pSym declHoleTk
-}       

-- ?remove pStr from this parser?
parseIdListIdent :: ListParser Document Node ClipDoc UserToken Ident
parseIdListIdent =  pPrs $
          (\strTk -> reuseIdent [strTk] Nothing Nothing (Just $ tokenString strTk))
      <$> pLIdent 

-------------------- Chess board parser:

parseBoard = 
      ((\_ -> initBoard) <$> pKey "board")
  <|>     (\str -> reuseBoard [str] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) 
      <$> pStructural BoardNode -- don't descend into structure, so no pres edit


-------------------- Powerpoint parser:
   
parsePPPresentation = 
      ((\_ -> initPPPresentation) <$> pKey "pres")
  <|> recognizePPPresentation

recognizePPPresentation = pStr $                       -- viewTp
         (\str list_slide -> reusePPPresentation [str] Nothing (Just list_slide))
     <$> pStructural PPPresentationNode
     <*> recognizeList_Slide
 
recognizeList_Slide = pStr $
         (\str slides -> reuseList_Slide [str] (Just $ toConsList_Slide slides)) 
     <$> pStructural List_SlideNode
     <*> pList recognizeSlide
  
        -- maybe make a recognizeConsList_Slide?

recognizeSlide =  pStr $
         (\str title itemList -> reuseSlide [str] (Just $ tokenString title) (Just itemList))
     <$> pStructural SlideNode
     <*> pLIdent <*> recognizeItemList

recognizeItemList = pStr $                          -- ListType
         (\str listType list_item -> reuseItemList [str] (Just listType) (Just list_item))
     <$> pStructural ItemListNode
     <*> recognizeListType <*> recognizeList_Item

recognizeListType = pStr $
         (\str -> reuseBullet [str])
     <$> pStructural BulletNode
  <|>    (\str -> reuseNumber [str])
     <$> pStructural NumberNode
  <|>    (\str -> reuseAlpha [str])
     <$> pStructural AlphaNode

recognizeList_Item = pStr $
         (\str items -> reuseList_Item [str] (Just $ toConsList_Item items)) 
     <$> pStructural List_ItemNode
     <*> pList recognizeItem

recognizeItem = pStr $ 
         (\str string_ -> reuseStringItem [str] (Just $ tokenString string_))
     <$> pStructural StringItemNode
     <*> pLIdent
  <|>    (\str helium -> reuseHeliumItem [str] (Just helium))
     <$> pStructural HeliumItemNode
     <*> recognizeExp
  <|>    (\str helium -> reuseListItem [str] (Just helium))
     <$> pStructural ListItemNode
     <*> recognizeItemList


recognizeExp =
         recognizeExp'
  <|>    pPrs parseExp
         

recognizeExp' = pStr $
         (\str e1 e2 -> reuseDivExp [str] (Just $ tokenIDP str) (Just e1) (Just e2))
     <$> pStructural DivExpNode
     <*> recognizeExp
     <*> recognizeExp
  <|>    (\str e1 e2 -> reusePowerExp [str] (Just $ tokenIDP str) (Just e1) (Just e2))
     <$> pStructural PowerExpNode
     <*> recognizeExp
     <*> recognizeExp
  <|>    HoleExp
     <$ pStructural HoleExpNode

-------------------- Helium parser:


-- List_Decl, Decl

recognizeList_Decl = pPrs $
          parseList_Decl
        
parseList_Decl = 
          (\decls -> reuseList_Decl [] (Just $ toConsList_Decl decls))
      <$> pList parseDecl

parseDecl  =                                                              -- IDD  "="                   ";"                       type sig              not used  expanded    auto-layout
          (\sig ident tk1 exp tk2 -> reuseDecl [tk1, tk2] (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (typeSigTokenIDP sig) Nothing (Just True) Nothing (Just ident) (Just exp))
      <$> pMaybe (pStructural DeclNode) -- type sig/value
      <*> parseIdent <*> pKey "=" <*> parseExp  <*> pKeyC 1 ";"
  <|>     (\sig ident tk1 tk2 -> reuseDecl [tk1, tk2] (Just $ tokenIDP tk1) Nothing (typeSigTokenIDP sig) Nothing Nothing Nothing (Just ident) Nothing)--makeDecl' mtk0 tk1 tk2 ident) 
      <$> pMaybe (pStructural DeclNode) -- type sig/value
      <*> parseIdent <*> pKey "=" <*> pKey "..." -- bit weird what happens when typing ... maybe this must be done with a structural presentation (wasn't possible before with structural parser that was too general)
 <|>      (\tk board ->  BoardDecl (tokenIDP tk) NoIDP board) 
      <$> pKey "Chess" <* pKey ":" <*> parseBoard        
 <|>      (\tk slides ->  PPPresentationDecl (tokenIDP tk) NoIDP slides)
      <$> pKey "Slides" <* pKey ":" <*> parsePPPresentation
 where typeSigTokenIDP Nothing   = Nothing
       typeSigTokenIDP (Just tk) = Just (tokenIDP tk)

-- List_Alt, Alt

parseList_Alt =
         (\alts -> reuseList_Alt [] (Just $ toConsList_Alt alts))
     <$> pList parseAlt

parseAlt  = 
         (\ident tk1 exp tk2 -> reuseAlt [tk1, tk2] (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just ident) (Just exp))
     <$> parseIdent <*> pArrow <*> parseExp  <*> pKeyC 4 ";"


-- Exp

parseIdentExp = 
         (\ident -> reuseIdentExp [] (Just ident))
     <$> parseIdent

parseIfExp = 
         (\tk1 c tk2 th tk3 el -> reuseIfExp [tk1, tk2,tk3] (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just $ tokenIDP tk3) (Just c) (Just th) (Just el))
     <$> pIf <*> parseExp <*> pThen <*> parseExp <*> pElse <*> parseExp

parseLamExp = 
         (\tk1 a tk2 b -> reuseLamExp [tk1, tk2] (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just a) (Just b))
     <$> pLambda <*> parseIdent <*> pArrow <*> parseExp
            
parseCaseExp =
         (\tk1 a tk2 b -> reuseCaseExp [tk1, tk2] (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just a) (Just b))
     <$> pCase <*> parseExp <*> pOf <*> parseList_Alt

parseLetExp =
         (\tk1 a tk2 b -> reuseLetExp [tk1, tk2] (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just a) (Just b))
     <$> pLet <*> parseList_Decl <*> pIn <*> parseExp

      
-- lists with separators are still a bit awkward
parseListExp = 
         (\tk1 (tks, list_Exp) tk2 -> reuseListExp ([tk1, tk2]++tks) (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just $ map tokenIDP tks) (Just $ list_Exp))
     <$> pKey "[" <*> parseList_Exp <*> pKey "]"

-- same here
parseParenExp = -- maybe we don't want to build a list for (exp), because now we have to remove it
         (\tk1 (tks, list_Exp) tk2 -> if arity list_Exp == 1 
                                      then let Clip_Exp exp = select [0] list_Exp -- unsafe match, but will never fail due to check
                                           in  reuseParenExp [tk1, tk2] (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just $ exp)
                                      else reuseProductExp ([tk1, tk2]++tks) (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just $ map tokenIDP tks) (Just $ list_Exp))
     <$> pKey "(" <*> parseList_Exp <*> pKey ")"


-- returns list of separator tokens and a List_Exp the List_Exp is not reused through its separator tokens
-- because these do not belong to List_Exp, but to its parent
parseList_Exp :: ListParser Document Node ClipDoc UserToken ([Token Document Node ClipDoc UserToken], List_Exp)
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
     <??> (    (\tk e t-> reusePlusExp [tk] (Just $ tokenIDP tk) (Just t) (Just e))
           <$> pKey "+" <*> parseExp 
          )
          
parseExp' = 
           parseTerm   -- e and t are flipped in lambda for <??>
      <??> (    (\tk e t->  reuseDivExp [tk] (Just $ tokenIDP tk) (Just t) (Just e))
            <$> pKey "%" <*> parseExp' 
           )
           
parseTerm   = 
           parseFactor
      <??> (      (\tk t f-> reuseTimesExp [tk] (Just $ tokenIDP tk) (Just f) (Just t))
              <$> pKey "*" 
              <*> parseTerm
           <|>    (\tk t f-> reuseDivExp [tk] (Just $ tokenIDP tk) (Just f) (Just t))
              <$> pKey "/"
              <*> parseTerm
           )

parseFactor = 
           parseFactor'
      <??> (    (\tk f f' -> reusePowerExp [tk] (Just $ tokenIDP tk) (Just f') (Just f))
            <$> pKey "^" <*> parseFactor
           )

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
         (\strTk -> reuseIdent [strTk] (Just $ tokenIDP strTk) Nothing (Just $ tokenString strTk))
     <$> pLIdent


-- primitive, what to do with idp?

-- maybe make a primIDP ? that takes the idp out of the string_? Then string has an idp field,
-- but it is not used on presentation. (maybe it can be hidden from the user)


--pString_ = 
--         (\string -> reuseString_ [string] (Just $ tokenIDP string) (Just $ lIdentVal string))
--     <$> pLIdent
-- parser that ignores idp, we want to specify this at the parent level!
--pString__ = 
--         (\string -> reuseString_ [string] Nothing (Just $ lIdentVal string))
--     <$> pLIdent


-------------------- Keyword parsers, remember to keep these consistent with keywords

pIf     = pKey "if"

pThen   = pKey "then"

pElse   = pKey "else"

pLambda = pKey "\\" -- <|> pKey "l"

pArrow  = pKey "->" <|> pKey "\174"

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
     <|> (HoleString_ <$ pStructural HoleString_Node)


parseInt_ = pStr $
--           (\strTk -> reuseString_ [] (Just $ tokenString strTk)) 
          mkInt_
     <$>  pInt
     <|> (HoleInt_ <$ pStructural HoleInt_Node)
-- bit hacky


parseBool_ = pStr $
--           (\strTk -> reuseString_ [] (Just $ tokenString strTk)) 
         mkBool_
     <$> (True <$ pTrue <|> False <$ pFalse)
     <|> (HoleBool_ <$ pStructural HoleBool_Node)
-}

parseIntExp =
         (\tk -> reuseIntExp [tk] (Just $ tokenIDP tk) (Just $ read (tokenString tk)))
     <$> pInt

parseBoolExp = 
         (\tk -> reuseBoolExp [tk] (Just $ tokenIDP tk) (Just True))
     <$> pTrue
  <|>    (\tk -> reuseBoolExp [tk] (Just $ tokenIDP tk) (Just False))
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

strTk str = UserTk (StrTk str) str Nothing (IDP (-1))
intTk     = UserTk IntTk "0" Nothing (IDP (-1))
lIdentTk  = UserTk LIdentTk "ident" Nothing (IDP (-1))
uIdentTk  = UserTk UIdentTk "Ident" Nothing (IDP (-1))
opTk      = UserTk OpTk "" Nothing (IDP (-1))
symTk     = UserTk SymTk "" Nothing (IDP (-1))
-- (IDP (-1)) means inserted token. This should be handled by some kind of 'fresh' attribute
-- which is also required for copying of presentation subtrees






-- Basic parsers

pKey :: DocNode node => String -> ListParser doc node clip UserToken (Token doc node clip UserToken)
pKey str = pSym  (strTk str)

pKeyC :: DocNode node => Int -> String -> ListParser doc node clip UserToken (Token doc node clip UserToken)
pKeyC c str = pCSym c (strTk str)

-- expensive, because we want holes to be inserted, not strings
pLIdent :: DocNode node => ListParser doc node clip UserToken (Token doc node clip UserToken)
pLIdent = pCSym 20 lIdentTk

-- todo return int from pInt, so unsafe intVal does not need to be used anywhere else
pInt :: DocNode node => ListParser doc node clip UserToken (Token doc node clip UserToken)
pInt = pCSym 20 intTk

lIdentVal :: DocNode node => Token doc node clip UserToken -> String
lIdentVal (UserTk LIdentTk str _ _) = str
lIdentVal tk                 = debug Err ("PresentationParser.lIdentVal: no IdentTk " ++ show tk) "x"

  
intVal :: DocNode node => Token doc node clip UserToken -> Int
intVal (UserTk IntTk "" _ _)  = 0   -- may happen on parse error (although not likely since insert is expensive)
intVal (UserTk IntTk str _ _) = read str
intVal tk              = debug Err ("PresentationParser.intVal: no IntTk " ++ show tk) (-9999)

 
