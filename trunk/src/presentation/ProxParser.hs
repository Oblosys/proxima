module ProxParser (parsePres) where

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

import DocTypes_Generated (Node)



parsePres pres = let tokens = postScanStr pres Nothing
                     (enr,errs) = runParser recognizeRootEnr tokens
                 in   showDebug' Err ("Parsing:\n"++concatMap (deepShowTks 0) (tokens)++"\nhas result:") $
                    if null errs then Just enr else Nothing
       
deepShowTks i tok = case tok of
                      (StructuralTk _ _ cs _) -> indent i ++ show tok ++ "\n"
                                               ++ indent (i+1)++"[\n"
                                               ++ concatMap (deepShowTks (i+1)) cs 
                                               ++ indent (i+1)++" ]\n"
                      (ParsingTk _ cs _) -> indent i ++ show tok ++ "\n"
                                               ++ indent (i+1)++"[\n"
                                               ++ concatMap (deepShowTks (i+1)) cs 
                                               ++ indent (i+1)++" ]\n"
                      _                     -> indent i ++ show tok ++ "\n" 
 where indent i = take i (repeat ' ')
       





-------------------- Proxima Parser/Structure Recognizer -------------------- 

recognizeRootEnr :: ListParser Node EnrichedDoc
recognizeRootEnr = pStr $ 
          (\str idlistdcls decls-> reuseRootEnr [tokenNode str] Nothing Nothing (Just idlistdcls) (Just decls) Nothing Nothing)
      <$> pSym (StructuralTk (Just $ RootEnrNode HoleEnrichedDoc []) empty [] NoIDP) -- EnrichedDoc is not instance of Editable
      <*> parseIDListList_Decl  {- <* (pStr' $ pStructural List_DeclNode) -}  <*> recognizeList_Decl
                                {- tree or xml view-}

-- ?remove pStr from this parser?
parseIDListList_Decl :: ListParser Node List_Decl
parseIDListList_Decl = pStr $
          (\dcls -> reuseList_Decl [] Nothing (Just $ toConsList_Decl dcls)) 
      <$  pSym parsingTk
      <*> pList recognizeIDListDecl
             
recognizeIDListDecl :: ListParser Node Decl
recognizeIDListDecl = pStr $
          (\str ident -> reuseDecl [tokenNode str] Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just ident) Nothing)
      <$> pStructural DeclNode
      <*> parseIdListIdent
  <|>     (\str -> reuseBoardDecl [tokenNode str] Nothing Nothing Nothing Nothing)
      <$> pStructural BoardDeclNode
  <|>     (\str -> reusePPPresentationDecl [tokenNode str] Nothing Nothing Nothing Nothing)
      <$> pStructural PPPresentationDeclNode
  <|>     (\str -> reuseInvDecl [tokenNode str] Nothing Nothing Nothing Nothing)
      <$> pStructural InvDeclNode
     {- <|>  
                      (\str -> HoleDecl
                  <$> pSym declHoleTk
-}       

-- ?remove pStr from this parser?
parseIdListIdent :: ListParser Node Ident
parseIdListIdent =  pStr $
          (\strTk -> reuseIdent [tokenNode strTk] Nothing Nothing Nothing (Just $ mkString_ strTk))
      <$  pSym parsingTk
      <*> pLIdent 

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
         (\str list_slide -> reusePPPresentation [tokenNode str] reuse Nothing (Just list_slide))
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
  <|>    HoleExp
     <$ pStructural HoleExpNode

-------------------- Helium parser:


-- List_Decl, Decl

recognizeList_Decl = pPrs $
          pSym parsingTk
       *> parseList_Decl
        
parseList_Decl = 
          (\decls -> reuseList_Decl [] Nothing (Just $ toConsList_Decl decls))
      <$> pList parseDecl

parseDecl  =                                                              -- IDD  "="                   ";"                       type sig              not used  expanded    auto-layout
          (\sig ident tk1 exp tk2 -> reuseDecl [tokenNode tk1, tokenNode tk2] Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (typeSigTokenIDP sig) Nothing (Just $ mkBool_ True) Nothing (Just ident) (Just exp))
      <$> pMaybe (pStructural DeclNode) -- type sig/value
      <*> parseIdent <*> pKey "=" <*> parseExp  <*> pKeyC 1 ";"
  <|>     (\sig ident tk1 tk2 -> reuseDecl [tokenNode tk1, tokenNode tk2] Nothing (Just $ tokenIDP tk1) Nothing (typeSigTokenIDP sig) Nothing Nothing Nothing (Just ident) Nothing)--makeDecl' mtk0 tk1 tk2 ident) 
      <$> pMaybe (pStructural DeclNode) -- type sig/value
      <*> parseIdent <*> pKey "=" <*> pKey "..." -- bit weird what happens when typing ... maybe this must be done with a structural presentation (wasn't possible before with structural parser that was too general)
 <|>      (\tk board ->  BoardDecl NoIDD (tokenIDP tk) NoIDP board) 
      <$> pKey "Chess" <* pKey ":" <*> parseBoard        
 <|>      (\tk slides ->  PPPresentationDecl NoIDD (tokenIDP tk) NoIDP slides)
      <$> pKey "Slides" <* pKey ":" <*> parsePPPresentation
 <|>  parseInvDecl
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
parseList_Exp :: ListParser Node ([Token Node (Maybe Node)], List_Exp)
parseList_Exp =
    (\toksElts -> let (toks, elts) = case toksElts of
                                       Nothing        -> ([], [])
                                       Just (e, etks) -> let (tks, es) = unzip etks
                                                         in  (tks, e:es)
                  in  (toks, reuseList_Exp [] Nothing (Just $ toConsList_Exp elts)))
     <$>  pMaybe (     (\e etks -> (e,etks)) 
                  <$> parseExp <*> pList ((,) <$> pKey "," <*> parseExp))
         
         
    
parseExp = -- use chain!!  and fix associativity of application!
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

-- **  can recognizeExp and recognizeExp' be merged?
recognizeExp' = pStr $ 
         (\str e1 e2 -> reuseDivExp [tokenNode str] Nothing Nothing (Just e1) (Just e2))
     <$> pStructural PowerExpNode
     <*> recognizeExp
     <*> recognizeExp
  <|>    (\str e1 e2 -> reusePowerExp [tokenNode str] Nothing Nothing (Just e1) (Just e2))
     <$> pStructural PowerExpNode
     <*> recognizeExp
     <*> recognizeExp
  <|>    HoleExp
     <$ pStructural HoleExpNode
     
parseIdent = 
         (\strTk -> reuseIdent [tokenNode strTk] Nothing (Just $ tokenIDP strTk) Nothing (Just $ mkString_ strTk))
     <$> pLIdent


-- primitive, what to do with idp?

-- maybe make a primIDP ? that takes the idp out of the string_? Then string has an idp field,
-- but it is not used on presentation. (maybe it can be hidden from the user)



-- don't even have to use reuse now, since the IDD is never used. String_ NoIDD would be sufficient
mkString_ :: Show node => Token node (Maybe node) -> String_
mkString_ = (\strTk -> reuseString_ [] Nothing (Just $ strValTk strTk)) 

mkInt_ :: Show node => Token node (Maybe node) -> Int_
mkInt_ = (\intTk -> reuseInt_ [] Nothing (Just $ intVal intTk)) 

-- Extracting the value from the token is not necessary, since true and false have different
-- parsers, which can give the value as an argument
mkBool_ :: Bool -> Bool_
mkBool_ = (\bool -> reuseBool_ [] Nothing (Just bool)) 

--pString_ = 
--         (\string -> reuseString_ [tokenNode string] Nothing (Just $ tokenIDP string) (Just $ lIdentVal string))
--     <$> pLIdent
-- parser that ignores idp, we want to specify this at the parent level!
--pString__ = 
--         (\string -> reuseString_ [tokenNode string] Nothing Nothing (Just $ lIdentVal string))
--     <$> pLIdent


-------------------- Keyword parsers, remember to keep these consistent with keywords in PresentationParsing.hs

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

-- parseString needs to parse the ParseToken, rewrite, so it doesn't use reuseString
parseString_ = pStr $
--           (\strTk -> reuseString_ [] Nothing (Just $ strValTk strTk)) 
          mkString_
     <$   pSym parsingTk
     <*>  pLIdent     
     <|> (HoleString_ <$ pStructural HoleString_Node)


parseInt_ = pStr $
--           (\strTk -> reuseString_ [] Nothing (Just $ strValTk strTk)) 
          mkInt_
     <$   pSym parsingTk
     <*>  pInt
     <|> (HoleInt_ <$ pStructural HoleInt_Node)
-- bit hacky


parseBool_ = pStr $
--           (\strTk -> reuseString_ [] Nothing (Just $ strValTk strTk)) 
         mkBool_
     <$  pSym parsingTk
     <*> (True <$ pTrue <|> False <$ pFalse)
     <|> (HoleBool_ <$ pStructural HoleBool_Node)


parseIntExp =
         (\tk -> reuseIntExp [tokenNode tk] Nothing (Just $ tokenIDP tk) (Just $ mkInt_ tk))
     <$> pInt

parseBoolExp = 
         (\tk -> reuseBoolExp [tokenNode tk] Nothing (Just $ tokenIDP tk) (Just $ mkBool_ True))
     <$> pTrue
  <|>    (\tk -> reuseBoolExp [tokenNode tk] Nothing (Just $ tokenIDP tk) (Just $ mkBool_ False))
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



enrichedDocTk = (StructuralTk (Just $ HoleEnrichedDocNode HoleEnrichedDoc []) empty [] NoIDP)
--boardDeclTk =  (StructuralTk (Just $ BoardDeclNode hole []) [] NoIDP)


--enrichedDocTk = StrTk "+" Nothing NoIDP -- (StructuralTk (Just $ EnrichedDocNode HoleEnrichedDoc []) [] NoIDP)
--declTk = StrTk "*" Nothing NoIDP -- (StructuralTk (Just $ DeclNode hole []) [] NoIDP)
--parsingTk = StrTk "%" Nothing NoIDP -- (ParsingTk [] NoIDP)


toks = [ --mkEnrichedDocTk
          -- [
             {- mkParsingTk 
               [ mkDeclTk 
                    [ mkParsingTk 
                      [ LIdentTk "x" Nothing NoIDP
                      ]
                   ]
               ]
           , -} mkParsingTk
               [
                 LIdentTk "x" Nothing NoIDP
               , StrTk "=" Nothing NoIDP
               , IntTk "1" Nothing NoIDP
               , StrTk ";" Nothing NoIDP
               , LIdentTk "y" Nothing NoIDP
               , StrTk "=" Nothing NoIDP
               , IntTk "1" Nothing NoIDP
               , StrTk ";" Nothing NoIDP
               ]
               
         --  ]
       ]
 where mkEnrichedDocTk cs = (StructuralTk (Just $ RootEnrNode HoleEnrichedDoc []) empty cs NoIDP)
       mkDeclTk cs =  (StructuralTk (Just $ DeclNode hole []) empty cs NoIDP)
       mkParsingTk cs = (ParsingTk empty cs NoIDP)
        



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







-------------------- Inv parser:

reuse = Nothing
set = Just


{-
<|>      (\tk board ->  BoardDecl NoIDD (tokenIDP tk) NoIDP board) 
      <$> pKey "Chess" <* pKey ":" <*> parseBoard        
 
parseBoard = 
      ((\_ -> initBoard) <$> pKey "board")
  <|>     (\str -> reuseBoard [tokenNode str] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) 
      <$> pStructural BoardNode -- don't descend into structure, so no pres edit

-}

parseInvDecl = (\tk inv ->  InvDecl NoIDD (tokenIDP tk) NoIDP inv)
      <$> pKey "Inv" <* pKey ":" <*> parseInv

parseInv = 
         (\tk {-eval view-} -> initInv) 
     <$> pKey "inv"
{-     <*> pLIdent
     <*> parseView'
--     <*> parseString_  -}
  <|>    recognizeInv


initInv  = Inv NoIDD (LeftDocView NoIDD (String_ NoIDD "not evaluated yet"))
                        (pr (as "ichi" `cons` (as "ni" `cons` (as "san" `cons` nil))) 
                            ((as "ichi" `pr` an 1) `cons` ((as "ni" `pr` an 2) `cons` ((as "san" `pr` an 3) `cons` nil))))
                       (String_ NoIDD "toc")
                       (Skip NoIDD)
 where as str = AS NoIDD (String_ NoIDD str)
       an i   = AN NoIDD (Int_ NoIDD i)    
       cons x xs = Ls NoIDD x xs
       nil = ANil NoIDD      
       pr x y = Pr NoIDD x y
--                       view --(ANil NoIDD)
                       --(Pr NoIDD (ANil NoIDD) --(Ls NoIDD (AN NoIDD (Int_ NoIDD 1)) (ANil NoIDD))
                       --          (ANil NoIDD) --(Ls NoIDD (AS NoIDD (String_ NoIDD "bla")) (ANil NoIDD))
                       --)
-- toc < ( "one" : ( "two" : ( "three" : [] ) ) ) , ( < "one" , #1 > : ( < "two" , #2 > : ( < "three" , #3 > : [] ) ) )  >
                       
{- original versions, before modifying for PSD workshop demo
parseInv = 
         (\tk eval view -> initInv (mkString_ eval) view) 
     <$> pKey "inv"
     <*> pLIdent
     <*> parseView'
--     <*> parseString_  
  <|>    recognizeInv
  

initInv eval view = Inv NoIDD (LeftDocView NoIDD (String_ NoIDD "not evaluated yet"))
                       view --(ANil NoIDD)
                       --(Pr NoIDD (ANil NoIDD) --(Ls NoIDD (AN NoIDD (Int_ NoIDD 1)) (ANil NoIDD))
                       --          (ANil NoIDD) --(Ls NoIDD (AS NoIDD (String_ NoIDD "bla")) (ANil NoIDD))
                       --)
                       eval
                       (Skip NoIDD)
-}


recognizeInv = pStr $
         (\str eval errDoc enr -> reuseInv [tokenNode str] reuse reuse (set enr) (set eval) reuse)
     <$> pStructural InvNode
     <*> parseString_ 
     <*> recognizeEitherDocView
      <*> recognizeView
--       <* parseView'
--     <*> recognizeEvalButton
     
recognizeEitherDocView = pStr $
         (\str -> reuseLeftDocView [tokenNode str] reuse reuse)
     <$> pStructural LeftDocViewNode
  <|>    (\str vw -> reuseRightDocView [tokenNode str] reuse (set vw)) -- (set doc))
     <$> pStructural RightDocViewNode
     <*> recognizeView

recognizeEvalButton = pStr $
         (\str -> reuseReEvaluate1 [tokenNode str] reuse)
     <$> pStructural ReEvaluate1Node
  <|>    (\str -> reuseReEvaluate2 [tokenNode str] reuse)
     <$> pStructural ReEvaluate2Node
  <|>    (\str -> reuseSkip [tokenNode str] reuse)
     <$> pStructural SkipNode

recognizeView = pStr $
         pSym parsingTk
      *> parseView'
    
--
-- quickly written parser, needs some factorizing and cleaning up
--
-- all keywords parsed with pKey should be declared in PresentationParsing.hs: keywords::[String]

parseView' =
         (\vw mrk -> case mrk of Nothing -> vw
                                 Just _  -> Mark NoIDD vw)
     <$> parseView
     <*> (Just <$> pKey "*" `opt` Nothing)


parseView = 
         (\tk1 tk2 -> reuseANil [tokenNode tk1, tokenNode tk2] reuse)
     <$> pKey "[" <*> pKey "]"
  <|>    (\tk i -> reuseAN [tokenNode tk] reuse (set (mkInt_ i)))
     <$> pKey "#"
     <*> pInt 
  <|>    (\tk1 str tk2 -> reuseAS [tokenNode tk1, tokenNode tk2] reuse (set (mkString_ str)))
     <$> pKey "\""  -- cannot use "'" because string is lIdent, which may contain "'"
     <*> pLIdent
     <*> pKey "\""
  <|>    (\tk1 v1 tk2 v2 tk3  -> reusePr [tokenNode tk1, tokenNode tk2, tokenNode tk3] reuse (set v1) (set v2))
     <$> pKey "<"
     <*> parseView' 
     <*> pKey ","
     <*> parseView' 
     <*> pKey ">"
  <|>    (\tk1 tk2  v tk3 -> reuseL [tokenNode tk1, tokenNode tk2, tokenNode tk3] reuse (set v))
     <$> pKey "("
     <*> pKey "L"
     <*> parseView' 
     <*> pKey ")"
  <|>    (\tk1 tk2 v tk3 -> reuseR [tokenNode tk1, tokenNode tk2, tokenNode tk3] reuse (set v))
     <$> pKey "("
     <*> pKey "R"
     <*> parseView' 
     <*> pKey ")"
  <|>    (\tk1 v1 tk2 v2 tk3  -> reuseLs [tokenNode tk1, tokenNode tk2, tokenNode tk3] reuse (set v1) (set v2))
     <$> pKey "("
     <*> parseView' 
     <*> pKey ":"
     <*> parseView' 
     <*> pKey ")"
  <|>    (\tk1 v1 tk2 v2 tk3  -> reuseTr [tokenNode tk1, tokenNode tk2, tokenNode tk3] reuse (set v1) (set v2))
     <$> pKey "{"
     <*> parseView' 
     <*> pKey ","
     <*> parseView' 
     <*> pKey "}"
  <|>    (\tk1 v1 tk2 v2 tk3  -> reuseDelL [tokenNode tk1, tokenNode tk2, tokenNode tk3] reuse (set v1) (set v2))
     <$> pKey "("
     <*> parseView' 
     <*> pKey ":-"
     <*> parseView' 
     <*> pKey ")"
  <|>    (\tk1 v1 tk2 v2 tk3  -> reuseInsL [tokenNode tk1, tokenNode tk2, tokenNode tk3] reuse (set v1) (set v2))
     <$> pKey "("
     <*> parseView' 
     <*> pKey ":+"
     <*> parseView' 
     <*> pKey ")"
  <|>    (\tk1 v1 tk2 v2 tk3  -> reuseSndP [tokenNode tk1, tokenNode tk2, tokenNode tk3] reuse (set $ mkBool_ False)(set v1) (set v2))
     <$> pKey "<-"
     <*> parseView' 
     <*> pKey ","
     <*> parseView' 
     <*> pKey ">"
  <|>    (\tk1 v1 tk2 v2 tk3  -> reuseSndP [tokenNode tk1, tokenNode tk2, tokenNode tk3] reuse (set $ mkBool_ True)(set v1) (set v2))
     <$> pKey "<+"
     <*> parseView' 
     <*> pKey ","
     <*> parseView' 
     <*> pKey ">"
  <|>    (\tk1 v1 tk2 v2 tk3  -> reuseFstP [tokenNode tk1, tokenNode tk2, tokenNode tk3] reuse (set $ mkBool_ False)(set v1) (set v2))
     <$> pKey "<"
     <*> parseView' 
     <*> pKey ","
     <*> parseView' 
     <*> pKey "->"
  <|>    (\tk1 v1 tk2 v2 tk3  -> reuseFstP [tokenNode tk1, tokenNode tk2, tokenNode tk3] reuse (set $ mkBool_ True) (set v1) (set v2))
     <$> pKey "<"
     <*> parseView' 
     <*> pKey ","
     <*> parseView' 
     <*> pKey "+>"
  <|>    (\tk1 tk2 tk3 tk4 v tk5 -> reuseIfNil [tokenNode tk1, tokenNode tk2, tokenNode tk3, tokenNode tk4, tokenNode tk5] reuse (set $ mkBool_ True) (set v))
     <$> pKey "("
     <*> pKey "["
     <*> pKey "]"
     <*> pKey "+>"
     <*> parseView'      
     <*> pKey ")"
  <|>    (\tk1 tk2 tk3 tk4 v tk5 -> reuseIfNil [tokenNode tk1, tokenNode tk2, tokenNode tk3, tokenNode tk4, tokenNode tk5] reuse (set $ mkBool_ False) (set v))
     <$> pKey "("
     <*> pKey "["
     <*> pKey "]"
     <*> pKey "->"
     <*> parseView'      
     <*> pKey ")"
  <|>    (\tk -> reuseUndef [tokenNode tk] reuse)
     <$> (pKey "_|_" <|> pKey "\200")
  <|>    recognizeTree
  <|>    HoleView
     <$ pStructural HoleViewNode

recognizeTree = pStr $ -- div&power recognizers are copied here, separating is hard because parse cannot fail on structural token
--         pSym parsingTk
--      *> parseTree
--  <|>
         (\str v1 v2 -> reuseTr [tokenNode str ] reuse (set v1) (set v2))
     <$> pStructural TrNode
     <*> parseView' 
     <*> parseView' 


{- parseIntExp =
         (\tk -> reuseIntExp [tokenNode tk] Nothing (Just $ tokenIDP tk) (Just $ mkInt_ tk))
     <$> pInt
-}

{-
recognizeView = pStr $
  <|>    (\str s -> reuseAS [tokenNode str] reuse (set s))
     <$> pStructural ASNode
     <*> parseString_ 
  <|>    (\str v -> reuseL [tokenNode str] reuse (set v))
     <$> pStructural LNode
     <*> recognizeView 
  <|>    (\str v -> reuseR [tokenNode str] reuse (set v))
     <$> pStructural RNode
     <*> recognizeView 
  <|>    (\str v -> reuseMark [tokenNode str] reuse (set v))
     <$> pStructural MarkNode
     <*> recognizeView 
  <|>    (\str v1 v2 -> reuseInsL [tokenNode str] reuse (set v1) (set v2))
     <$> pStructural InsLNode
     <*> recognizeView 
     <*> recognizeView 
  <|>    (\str b v1 v2 -> reuseSndP [tokenNode str] reuse (set b) (set v1) (set v2))
     <$> pStructural SndPNode
     <*> parseBool_
     <*> recognizeView 
     <*> recognizeView 
  <|>    (\str b v1 v2 -> reuseFstP [tokenNode str] reuse (set b) (set v1) (set v2))
     <$> pStructural FstPNode
     <*> parseBool_
     <*> recognizeView 
     <*> recognizeView 
  <|>    (\str b v -> reuseIfNil [tokenNode str] reuse (set b) (set v))
     <$> pStructural IfNilNode
     <*> parseBool_
     <*> recognizeView 
  <|>    (\str -> reuseUndef [tokenNode str] reuse)
     <$> pStructural UndefNode
-}
{-
recognizeView = pStr $
         (\str -> reuseANil [tokenNode str] reuse)
     <$> pStructural ANilNode
  <|>    (\str i -> reuseAN [tokenNode str] reuse (set i))
     <$> pStructural ANNode
     <*> parseInt_ 
  <|>    (\str s -> reuseAS [tokenNode str] reuse (set s))
     <$> pStructural ASNode
     <*> parseString_ 
  <|>    (\str v1 v2 -> reusePr [tokenNode str] reuse (set v1) (set v2))
     <$> pStructural PrNode
     <*> recognizeView 
     <*> recognizeView 
  <|>    (\str v1 v2 -> reuseLs [tokenNode str] reuse (set v1) (set v2))
     <$> pStructural LsNode
     <*> recognizeView 
     <*> recognizeView 
  <|>    (\str v1 v2 -> reuseTr [tokenNode str] reuse (set v1) (set v2))
     <$> pStructural TrNode
     <*> recognizeView 
     <*> recognizeView 
  <|>    (\str v1 v2 -> reuseDelL [tokenNode str] reuse (set v1) (set v2))
     <$> pStructural DelLNode
     <*> recognizeView 
     <*> recognizeView 
  <|>    (\str v -> reuseL [tokenNode str] reuse (set v))
     <$> pStructural LNode
     <*> recognizeView 
  <|>    (\str v -> reuseR [tokenNode str] reuse (set v))
     <$> pStructural RNode
     <*> recognizeView 
  <|>    (\str v -> reuseMark [tokenNode str] reuse (set v))
     <$> pStructural MarkNode
     <*> recognizeView 
  <|>    (\str v1 v2 -> reuseInsL [tokenNode str] reuse (set v1) (set v2))
     <$> pStructural InsLNode
     <*> recognizeView 
     <*> recognizeView 
  <|>    (\str b v1 v2 -> reuseSndP [tokenNode str] reuse (set b) (set v1) (set v2))
     <$> pStructural SndPNode
     <*> parseBool_
     <*> recognizeView 
     <*> recognizeView 
  <|>    (\str b v1 v2 -> reuseFstP [tokenNode str] reuse (set b) (set v1) (set v2))
     <$> pStructural FstPNode
     <*> parseBool_
     <*> recognizeView 
     <*> recognizeView 
  <|>    (\str b v -> reuseIfNil [tokenNode str] reuse (set b) (set v))
     <$> pStructural IfNilNode
     <*> parseBool_
     <*> recognizeView 
  <|>    (\str -> reuseUndef [tokenNode str] reuse)
     <$> pStructural UndefNode
  <|>    HoleView
     <$ pStructural HoleViewNode
  <|>    (\str -> reuseUnit [tokenNode str] reuse)
     <$> pStructural UnitNode
-}


