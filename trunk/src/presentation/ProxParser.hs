module ProxParser where

import CommonTypes
import PresLayerTypes
import PresLayerUtils
import PresentationParsing
import XprezLib

import UU_Parsing hiding (Exp, parse, parseIO)

import List hiding (delete)
import Data.FiniteMap
import IOExts


import ProxParser_Generated
import DocumentEdit

import UU_Parsing hiding (Exp, parse, parseIO)
import qualified UU_Parsing
import Char

import Scanner (tokenize)

-- why is this here?
initLayout :: LayoutMap
initLayout = listToFM [(IDP (-1), (0,1))]


parsePres pres = let tokens = postScanStr pres Nothing
                     (enr,errs) = runParser recognizeRootEnr tokens
                 in --  showDebug' Err ("Parsing:\n"++concatMap (deepShow 0) (tokens)++"\nhas result:") $
                    if null errs then Just (enr, [], []) else Nothing
 where deepShow i tok@(Structural _ _ cs _) = indent i ++ show tok ++ "\n"
                                           ++ indent (i+1)++"[\n"
                                           ++ concatMap (deepShow (i+1)) cs 
                                           ++ indent (i+1)++" ]\n"
       deepShow i tok = indent i ++ show tok ++ "\n" 
       indent i = take i (repeat ' ')
       






-------------------- Proxima Parser/Structure Recognizer -------------------- 

recognizeRootEnr = pStr' $ 
          (\str idlistdcls decls-> reuseRootEnr [tokenNode str] Nothing Nothing (Just idlistdcls) (Just decls) Nothing Nothing)
      <$> pSym (Structural (Just $ RootEnrNode HoleEnrichedDoc []) empty [] NoIDP) -- EnrichedDoc is not instance of Editable

      <*> parseIDListList_Decl <* (pStr' $ pStructural List_DeclNode) <*> recognizeList_Decl
                                   {- tree or xml view-}
-- ?remove pStr from this parser?
parseIDListList_Decl :: ListParser List_Decl
parseIDListList_Decl = pStr $
          (\dcls -> reuseList_Decl [] Nothing (Just $ toConsList_Decl dcls)) 
      <$  pSym parsingTk
      <*> pList recognizeIDListDecl
             
recognizeIDListDecl :: ListParser Decl
recognizeIDListDecl = pStr $
          (\str ident -> reuseDecl [tokenNode str] Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just ident) Nothing)
      <$> pStructural DeclNode
      <*> parseIdListIdent
  <|>     (\str -> reuseBoardDecl [tokenNode str] Nothing Nothing Nothing Nothing)
      <$> pStructural BoardDeclNode
  <|>     (\str -> reusePPPresentationDecl [tokenNode str] Nothing Nothing Nothing Nothing)
      <$> pStructural PPPresentationDeclNode
      {- <|>  
                      (\str -> HoleDecl
                  <$> pSym declHoleTk
-}       

-- ?remove pStr from this parser?
parseIdListIdent :: ListParser Ident
parseIdListIdent =  pStr $
           (\string -> reuseIdent [tokenNode string] Nothing Nothing Nothing (Just $ lIdentVal string))
      <$   pSym parsingTk
      <*>  pLIdent

-------------------- Chess board parser:

parseBoard = 
      ((\_ -> initBoard) <$> pKey "board")
  <|>     (\str -> reuseBoard [tokenNode str] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) 
      <$> pStructural BoardNode -- don't descend into structure, so no pres edit

-------------------- Powerpoint parser:
   
parsePPPresentation = 
      ((\_ -> initPPPresentation) <$> pKey "pres")
  <|> recognizePPPresentation

recognizePPPresentation = pStr $                       -- IDD     viewTp
         (\str list_slide -> reusePPPresentation [tokenNode str] Nothing Nothing (Just list_slide))
     <$> pStructural PPPresentationNode
     <*> recognizeList_Slide
 
recognizeList_Slide = pStr $
         (\str slides -> reuseList_Slide [tokenNode str] Nothing (Just $ toConsList_Slide slides)) 
     <$> pStructural List_SlideNode
     <*> pList recognizeSlide
  
        -- maybe make a recognizeConsList_Slide?

recognizeSlide =  pStr $
         (\str title itemList -> reuseSlide [tokenNode str] Nothing (Just title) (Just itemList))
     <$> pStructural SlideNode
     <*> parseString_ <*> recognizeItemList
    
-- ?remove pStr from this parser?
parseString_ = pStr $
           (\string -> reuseString_ [tokenNode string] Nothing (Just $ lIdentVal string)) 
     <$   pSym parsingTk
     <*>  pLIdent

recognizeItemList = pStr $                          -- ListType
         (\str listType list_item -> reuseItemList [tokenNode str] Nothing (Just listType) (Just list_item))
     <$> pStructural ItemListNode
     <*> recognizeListType <*> recognizeList_Item

recognizeListType = pStr $
         (\str -> reuseBullet [tokenNode str] Nothing)
     <$> pStructural BulletNode
  <|>    (\str -> reuseNumber [tokenNode str] Nothing)
     <$> pStructural NumberNode
  <|>    (\str -> reuseAlpha [tokenNode str] Nothing)
     <$> pStructural AlphaNode

recognizeList_Item = pStr $
         (\str items -> reuseList_Item [tokenNode str] Nothing (Just $ toConsList_Item items)) 
     <$> pStructural List_ItemNode
     <*> pList recognizeItem

recognizeItem = pStr $ 
         (\str string_ -> reuseStringItem [tokenNode str] Nothing (Just string_))
     <$> pStructural StringItemNode
     <*> parseString_
  <|>    (\str helium -> reuseHeliumItem [tokenNode str] Nothing (Just helium))
     <$> pStructural HeliumItemNode
     <*> recognizeExp
  <|>    (\str helium -> reuseListItem [tokenNode str] Nothing (Just helium))
     <$> pStructural ListItemNode
     <*> recognizeItemList

recognizeExp = pStr $ -- div&power recognizers are copied here, separating is hard because parse cannot fail on structural token
         pSym parsingTk
      *> parseExp
  <|>    (\str e1 e2 -> reuseDivExp [tokenNode str] Nothing Nothing (Just e1) (Just e2))
     <$> pStructural DivExpNode
     <*> recognizeExp
     <*> recognizeExp
  <|>    (\str e1 e2 -> reusePowerExp [tokenNode str] Nothing Nothing (Just e1) (Just e2))
     <$> pStructural PowerExpNode
     <*> recognizeExp
     <*> recognizeExp

-------------------- Helium parser:


-- List_Decl, Decl

recognizeList_Decl = pStr $
          pSym parsingTk
       *> parseList_Decl
        
parseList_Decl = 
          (\decls -> reuseList_Decl [] Nothing (Just $ toConsList_Decl decls))
      <$> pList parseDecl

parseDecl  =                                                              -- IDD  "="                   ";"                       type sig              not used  expanded    auto-layout
          (\sig ident tk1 exp tk2 -> reuseDecl [tokenNode tk1, tokenNode tk2] Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (typeSigTokenIDP sig) Nothing (Just True) Nothing (Just ident) (Just exp))
      <$> pMaybe (pStructural DeclNode) -- type sig/value
      <*> parseIdent <*> pKey "=" <*> parseExp  <*> pKeyC 1 ";"
  <|>     (\sig ident tk1 tk2 -> reuseDecl [tokenNode tk1, tokenNode tk2] Nothing (Just $ tokenIDP tk1) Nothing (typeSigTokenIDP sig) Nothing Nothing Nothing (Just ident) Nothing)--makeDecl' mtk0 tk1 tk2 ident) 
      <$> pMaybe (pStructural DeclNode) -- type sig/value
      <*> parseIdent <*> pKey "=" <*> pKey "..."  -- bit weird what happens when typing ... maybe this must be done with a structural presentation (wasn't possible before with structural parser that was too general)
 <|>      (\t ->  BoardDecl NoIDD (tokenIDP t) NoIDP) 
      <$> pKey "Chess" <* pKey ":" <*> parseBoard        
 <|>      (\t ->  PPPresentationDecl NoIDD (tokenIDP t) NoIDP)
      <$> pKey "PPT" <* pKey ":" <*> parsePPPresentation
 where typeSigTokenIDP Nothing   = Nothing
       typeSigTokenIDP (Just tk) = Just (tokenIDP tk)

-- List_Alt, Alt

parseList_Alt =
         (\alts -> reuseList_Alt [] Nothing (Just $ toConsList_Alt alts))
     <$> pList parseAlt

parseAlt  = 
         (\ident tk1 exp tk2 -> reuseAlt [tokenNode tk1, tokenNode tk2] Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just ident) (Just exp))
     <$> parseIdent <*> pArrow <*> parseExp  <*> pKeyC 4 ";"


-- Exp

parseIdentExp = 
         (\ident -> reuseIdentExp [] Nothing (Just ident))
     <$> parseIdent

parseIfExp = 
         (\tk1 c tk2 th tk3 el -> reuseIfExp [tokenNode tk1, tokenNode tk2,tokenNode tk3] Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just $ tokenIDP tk3) (Just c) (Just th) (Just el))
     <$> pIf <*> parseExp <*> pThen <*> parseExp <*> pElse <*> parseExp

parseLamExp = 
         (\tk1 a tk2 b -> reuseLamExp [tokenNode tk1, tokenNode tk2] Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just a) (Just b))
     <$> pLambda <*> parseIdent <*> pArrow <*> parseExp
            
parseCaseExp =
         (\tk1 a tk2 b -> reuseCaseExp [tokenNode tk1, tokenNode tk2] Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just a) (Just b))
     <$> pCase <*> parseExp <*> pOf <*> parseList_Alt

parseLetExp =
         (\tk1 a tk2 b -> reuseLetExp [tokenNode tk1, tokenNode tk2] Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just a) (Just b))
     <$> pLet <*> parseList_Decl <*> pIn <*> parseExp

      
-- lists with separators are still a bit awkward
parseListExp = 
         (\tk1 (tks, list_Exp) tk2 -> reuseListExp ([tokenNode tk1, tokenNode tk2]++map tokenNode tks) Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just $ map tokenIDP tks) (Just $ list_Exp))
     <$> pKey "[" <*> parseList_Exp <*> pKey "]"

-- same here
parseParenExp = -- maybe we don't want to build a list for (exp), because now we have to remove it
         (\tk1 (tks, list_Exp) tk2 -> if arity list_Exp == 1 
                                      then let Clip_Exp exp = select [0] list_Exp -- unsafe match, but will never fail due to check
                                           in  reuseParenExp [tokenNode tk1, tokenNode tk2] Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just $ exp)
                                      else reuseProductExp ([tokenNode tk1, tokenNode tk2]++map tokenNode tks) Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just $ map tokenIDP tks) (Just $ list_Exp))
     <$> pKey "(" <*> parseList_Exp <*> pKey ")"


-- returns list of separator tokens and a List_Exp the List_Exp is not reused through its separator tokens
-- because these do not belong to List_Exp, but to its parent
parseList_Exp :: ListParser ([Token (Maybe Node)], List_Exp)
parseList_Exp =
    (\toksElts -> let (toks, elts) = case toksElts of
                                       Nothing        -> ([], [])
                                       Just (e, etks) -> let (tks, es) = unzip etks
                                                         in  (tks, e:es)
                  in  (toks, reuseList_Exp [] Nothing (Just $ toConsList_Exp elts)))
     <$>  pMaybe (     (\e etks -> (e,etks)) 
                  <$> parseExp <*> pList ((,) <$> pKey "," <*> parseExp))
         
         
    
parseExp = -- use chain!!
          parseExp'   -- e and t are flipped in lambda for <??>
     <??> (    (\tk e t-> reusePlusExp [tokenNode tk] Nothing (Just $ tokenIDP tk) (Just t) (Just e))
           <$> pKey "+" <*> parseExp 
          )
          
parseExp' = 
           parseTerm   -- e and t are flipped in lambda for <??>
      <??> (    (\tk e t->  reuseDivExp [tokenNode tk] Nothing (Just $ tokenIDP tk) (Just t) (Just e))
            <$> pKey "%" <*> parseExp' 
           )
           
parseTerm   = 
           parseFactor
      <??> (      (\tk t f-> reuseTimesExp [tokenNode tk] Nothing (Just $ tokenIDP tk) (Just f) (Just t))
              <$> pKey "*" 
              <*> parseTerm
           <|>    (\tk t f-> reuseDivExp [tokenNode tk] Nothing (Just $ tokenIDP tk) (Just f) (Just t))
              <$> pKey "/"
              <*> parseTerm
           )

parseFactor = 
           parseFactor'
      <??> (    (\tk f f' -> reusePowerExp [tokenNode tk] Nothing (Just $ tokenIDP tk) (Just f') (Just f))
            <$> pKey "^" <*> parseFactor
           )

parseFactor' =   
           parseFactor''
      <??> (    (\f' f'' -> reuseAppExp [] Nothing (Just f'') (Just f'))
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

-- **  ' can the two be merged?
recognizeExp' = pStr $ 
         (\str e1 e2 -> reuseDivExp [tokenNode str] Nothing Nothing (Just e1) (Just e2))
     <$> pStructural DivExpNode
     <*> recognizeExp
     <*> recognizeExp
  <|>    (\str e1 e2 -> reusePowerExp [tokenNode str] Nothing Nothing (Just e1) (Just e2))
     <$> pStructural PowerExpNode
     <*> recognizeExp
     <*> recognizeExp

parseIdent = 
         (\string -> reuseIdent [tokenNode string] Nothing (Just $ tokenIDP string) Nothing (Just $ lIdentVal string))
     <$> pLIdent

-------------------- Keyword parsers, remember to keep these consistent with keywords in Scanner.hs

pIf     = pKey "if"

pThen   = pKey "then"

pElse   = pKey "else"

pLambda = pKey "\\" 

pArrow  = pKey "->" <|> pKey "\174"

pCase   = pKey "case"

pOf     = pKey "of"

pLet    = pKey "let"

pIn     = pKey "in"

pTrue   = pKey "True"

pFalse  = pKey "False"

-------------------- more or less primitive parsers: (because int & bool are unboxed)

parseIntExp =
         (\tk -> reuseIntExp [tokenNode tk] Nothing (Just $ tokenIDP tk) (Just $ intVal tk))
     <$> pInt

parseBoolExp = 
         (\tk -> reuseBoolExp [tokenNode tk] Nothing (Just $ tokenIDP tk) (Just True))
     <$> pTrue
  <|>    (\tk -> reuseBoolExp [tokenNode tk] Nothing (Just $ tokenIDP tk) (Just False))
     <$> pFalse

--------------------------------------------------------------
--- testing bits

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


--------------------------------------------------------------



enrichedDocTk = (Structural (Just $ HoleEnrichedDocNode HoleEnrichedDoc []) empty [] NoIDP)
--boardDeclTk =  (Structural (Just $ BoardDeclNode hole []) [] NoIDP)
parsingTk = (Structural (Just $ NoNode) empty [] NoIDP)
--enrichedDocTk = StrTk "+" Nothing NoIDP -- (Structural (Just $ EnrichedDocNode HoleEnrichedDoc []) [] NoIDP)
--declTk = StrTk "*" Nothing NoIDP -- (Structural (Just $ DeclNode hole []) [] NoIDP)
--parsingTk = StrTk "%" Nothing NoIDP -- (Structural (Just $ NoNode) [] NoIDP)

toks = [ mkEnrichedDocTk
           [ mkParsingTk 
               [ mkDeclTk 
                    [ mkParsingTk 
                      [ LIdentTk "x" Nothing NoIDP
                      ]
                   ]
               ]
           , mkParsingTk
               [ mkDeclTk  [] 
               , LIdentTk "x" Nothing NoIDP
               , StrTk "=" Nothing NoIDP
               , IntTk "1" Nothing NoIDP
               , StrTk ";" Nothing NoIDP
               , LIdentTk "y" Nothing NoIDP
               , StrTk "=" Nothing NoIDP
               , IntTk "1" Nothing NoIDP
               , StrTk ";" Nothing NoIDP
               ]
               
           ]
       ]
 where mkEnrichedDocTk cs = (Structural (Just $ HoleEnrichedDocNode HoleEnrichedDoc []) empty cs NoIDP)
       mkDeclTk cs =  (Structural (Just $ DeclNode hole []) empty cs NoIDP)
       mkParsingTk cs = (Structural (Just $ NoNode) empty cs NoIDP)
        



-- UNCLEAR:
-- default: what to do with things like HeliumTypeInfo? answer: declare as Editable and specify hole

--Design issues with parsing and structure recognizing (choose different name? recognizer is usually parser with Bool result)

-- TODO: 



-- parsing token is now a Structural NoNode, this is a hack.

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
-- remember to that "Chess", "PPT", "board", and "pres" must be keywords in PresentationParsing


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

