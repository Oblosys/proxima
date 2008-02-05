module ProxParser (parsePres) where

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

-- TODO: move to PresentationParsing
reuse = Nothing
set = Just



parsePres pres = let tokens = postScanStr keywords Nothing pres
                     (enr,errs) = runParser recognizeRootEnr tokens
                 in --debug Err ("Parsing:\n"++concatMap (deepShowTks 0) (tokens)  
                    --           {- ++"\nhas result:"++show res -}) $
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
       




keywords :: [String]
keywords = 
  [ ":-"
  , ":+"
  , "_|_"
  , "#"
  , "<"
  , ">"
  , "L"
  , "R"
  , "<-"
  , "->"
  , "<+"
  , "+>"
  , "\""
  , "</"
  , "/>"
  , "," --
  , "(" --
  , ")" --
  , "{" --
  , "}" --
  , ";" --
  , "[" --
  , "]" --
  , "="
  , "%"
  , "+"
  , "-"
  , "*"
  , "/"
  , "^"
  , "\174"
  , "\\" 
--  , "l"      -- not very nice, just for demonstrating lambdas
  , "False"
  , "True"
  , "if"
  , "then"
  , "else"
  , "let"
  , "in"
  , "case"
  , "of"
  , "Chess"
  , "board"
  , "Slides"
  , "pres"
  , "Inv"
  , "inv"
  , ":"
  , "..."
  , "Form"
  , "what"
  ]

-------------------- Proxima Parser/Structure Recognizer -------------------- 


recognizeRootEnr :: ListParser Document Node ClipDoc UserToken EnrichedDoc
recognizeRootEnr = pStr $ 
          (\str root -> reuseRootEnr [str] Nothing (Just root) Nothing Nothing)
      <$> pSym (StructuralTk (Just $ RootEnrNode HoleEnrichedDoc []) empty [] NoIDP) -- EnrichedDoc is not instance of Editable
      <*> recognizeRootE
  <|>    RootEnr NoIDD (error "doc hole was parsed") (error "doc hole was parsed") (error "doc hole was parsed")
     <$ pStructural HoleEnrichedDocNode

recognizeRootE :: ListParser Document Node ClipDoc UserToken RootE
recognizeRootE = pStr $ 
          (\str idlistdecls decls-> reuseRootE [str] Nothing Nothing (Just decls) (Just idlistdecls))
      <$> pStructural RootENode
      <*> parseIDListList_Decl {- <* (pStr' $ pStructural List_DeclNode) -}  <*> recognizeList_Decl
                                {- tree or xml view-}

-- ?remove pStr from this parser?
parseIDListList_Decl :: ListParser Document Node ClipDoc UserToken List_Decl
parseIDListList_Decl = pPrs $
          (\dcls -> reuseList_Decl [] Nothing (Just $ toConsList_Decl dcls)) 
      <$> pList recognizeIDListDecl
             
recognizeIDListDecl :: ListParser Document Node ClipDoc UserToken Decl
recognizeIDListDecl = pStr $
          (\str ident -> reuseDecl [str] Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just ident) Nothing)
      <$> pStructural DeclNode
      <*> parseIdListIdent
  <|>     (\str -> reuseBoardDecl [str] Nothing Nothing Nothing Nothing)
      <$> pStructural BoardDeclNode
  <|>     (\str -> reusePPPresentationDecl [str] Nothing Nothing Nothing Nothing)
      <$> pStructural PPPresentationDeclNode
     {- <|>  
                      (\str -> HoleDecl
                  <$> pSym declHoleTk
-}       

-- ?remove pStr from this parser?
parseIdListIdent :: ListParser Document Node ClipDoc UserToken Ident
parseIdListIdent =  pPrs $
          (\strTk -> reuseIdent [strTk] Nothing Nothing Nothing (Just $ tokenString strTk))
      <$> pLIdent 

-------------------- Chess board parser:

parseBoard = 
      ((\_ -> initBoard) <$> pKey "board")
  <|>     (\str -> reuseBoard [str] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) 
      <$> pStructural BoardNode -- don't descend into structure, so no pres edit


-------------------- Powerpoint parser:
   
parsePPPresentation = 
      ((\_ -> initPPPresentation) <$> pKey "pres")
  <|> recognizePPPresentation

recognizePPPresentation = pStr $                       -- IDD     viewTp
         (\str list_slide -> reusePPPresentation [str] reuse Nothing (Just list_slide))
     <$> pStructural PPPresentationNode
     <*> recognizeList_Slide
 
recognizeList_Slide = pStr $
         (\str slides -> reuseList_Slide [str] Nothing (Just $ toConsList_Slide slides)) 
     <$> pStructural List_SlideNode
     <*> pList recognizeSlide
  
        -- maybe make a recognizeConsList_Slide?

recognizeSlide =  pStr $
         (\str title itemList -> reuseSlide [str] Nothing (Just $ tokenString title) (Just itemList))
     <$> pStructural SlideNode
     <*> pLIdent <*> recognizeItemList

recognizeItemList = pStr $                          -- ListType
         (\str listType list_item -> reuseItemList [str] Nothing (Just listType) (Just list_item))
     <$> pStructural ItemListNode
     <*> recognizeListType <*> recognizeList_Item

recognizeListType = pStr $
         (\str -> reuseBullet [str] Nothing)
     <$> pStructural BulletNode
  <|>    (\str -> reuseNumber [str] Nothing)
     <$> pStructural NumberNode
  <|>    (\str -> reuseAlpha [str] Nothing)
     <$> pStructural AlphaNode

recognizeList_Item = pStr $
         (\str items -> reuseList_Item [str] Nothing (Just $ toConsList_Item items)) 
     <$> pStructural List_ItemNode
     <*> pList recognizeItem

recognizeItem = pStr $ 
         (\str string_ -> reuseStringItem [str] Nothing (Just $ tokenString string_))
     <$> pStructural StringItemNode
     <*> pLIdent
  <|>    (\str helium -> reuseHeliumItem [str] Nothing (Just helium))
     <$> pStructural HeliumItemNode
     <*> recognizeExp
  <|>    (\str helium -> reuseListItem [str] Nothing (Just helium))
     <$> pStructural ListItemNode
     <*> recognizeItemList


recognizeExp =
         recognizeExp'
  <|>    pPrs parseExp
         

recognizeExp' = pStr $
         (\str e1 e2 -> reuseDivExp [str] Nothing Nothing (Just e1) (Just e2))
     <$> pStructural DivExpNode
     <*> recognizeExp
     <*> recognizeExp
  <|>    (\str e1 e2 -> reusePowerExp [str] Nothing Nothing (Just e1) (Just e2))
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
          (\decls -> reuseList_Decl [] Nothing (Just $ toConsList_Decl decls))
      <$> pList parseDecl

parseDecl  =                                                              -- IDD  "="                   ";"                       type sig              not used  expanded    auto-layout
          (\sig ident tk1 exp tk2 -> reuseDecl [tk1, tk2] Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (typeSigTokenIDP sig) Nothing (Just True) Nothing (Just ident) (Just exp))
      <$> pMaybe (pStructural DeclNode) -- type sig/value
      <*> parseIdent <*> pKey "=" <*> parseExp  <*> pKeyC 1 ";"
  <|>     (\sig ident tk1 tk2 -> reuseDecl [tk1, tk2] Nothing (Just $ tokenIDP tk1) Nothing (typeSigTokenIDP sig) Nothing Nothing Nothing (Just ident) Nothing)--makeDecl' mtk0 tk1 tk2 ident) 
      <$> pMaybe (pStructural DeclNode) -- type sig/value
      <*> parseIdent <*> pKey "=" <*> pKey "..." -- bit weird what happens when typing ... maybe this must be done with a structural presentation (wasn't possible before with structural parser that was too general)
 <|>      (\tk board ->  BoardDecl NoIDD (tokenIDP tk) NoIDP board) 
      <$> pKey "Chess" <* pKey ":" <*> parseBoard        
 <|>      (\tk slides ->  PPPresentationDecl NoIDD (tokenIDP tk) NoIDP slides)
      <$> pKey "Slides" <* pKey ":" <*> parsePPPresentation
 where typeSigTokenIDP Nothing   = Nothing
       typeSigTokenIDP (Just tk) = Just (tokenIDP tk)

-- List_Alt, Alt

parseList_Alt =
         (\alts -> reuseList_Alt [] Nothing (Just $ toConsList_Alt alts))
     <$> pList parseAlt

parseAlt  = 
         (\ident tk1 exp tk2 -> reuseAlt [tk1, tk2] Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just ident) (Just exp))
     <$> parseIdent <*> pArrow <*> parseExp  <*> pKeyC 4 ";"


-- Exp

parseIdentExp = 
         (\ident -> reuseIdentExp [] Nothing (Just ident))
     <$> parseIdent

parseIfExp = 
         (\tk1 c tk2 th tk3 el -> reuseIfExp [tk1, tk2,tk3] Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just $ tokenIDP tk3) (Just c) (Just th) (Just el))
     <$> pIf <*> parseExp <*> pThen <*> parseExp <*> pElse <*> parseExp

parseLamExp = 
         (\tk1 a tk2 b -> reuseLamExp [tk1, tk2] Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just a) (Just b))
     <$> pLambda <*> parseIdent <*> pArrow <*> parseExp
            
parseCaseExp =
         (\tk1 a tk2 b -> reuseCaseExp [tk1, tk2] Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just a) (Just b))
     <$> pCase <*> parseExp <*> pOf <*> parseList_Alt

parseLetExp =
         (\tk1 a tk2 b -> reuseLetExp [tk1, tk2] Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just a) (Just b))
     <$> pLet <*> parseList_Decl <*> pIn <*> parseExp

      
-- lists with separators are still a bit awkward
parseListExp = 
         (\tk1 (tks, list_Exp) tk2 -> reuseListExp ([tk1, tk2]++tks) Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just $ map tokenIDP tks) (Just $ list_Exp))
     <$> pKey "[" <*> parseList_Exp <*> pKey "]"

-- same here
parseParenExp = -- maybe we don't want to build a list for (exp), because now we have to remove it
         (\tk1 (tks, list_Exp) tk2 -> if arity list_Exp == 1 
                                      then let Clip_Exp exp = select [0] list_Exp -- unsafe match, but will never fail due to check
                                           in  reuseParenExp [tk1, tk2] Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just $ exp)
                                      else reuseProductExp ([tk1, tk2]++tks) Nothing (Just $ tokenIDP tk1) (Just $ tokenIDP tk2) (Just $ map tokenIDP tks) (Just $ list_Exp))
     <$> pKey "(" <*> parseList_Exp <*> pKey ")"


-- returns list of separator tokens and a List_Exp the List_Exp is not reused through its separator tokens
-- because these do not belong to List_Exp, but to its parent
parseList_Exp :: ListParser Document Node ClipDoc UserToken ([Token Document Node ClipDoc UserToken], List_Exp)
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
     <??> (    (\tk e t-> reusePlusExp [tk] Nothing (Just $ tokenIDP tk) (Just t) (Just e))
           <$> pKey "+" <*> parseExp 
          )
          
parseExp' = 
           parseTerm   -- e and t are flipped in lambda for <??>
      <??> (    (\tk e t->  reuseDivExp [tk] Nothing (Just $ tokenIDP tk) (Just t) (Just e))
            <$> pKey "%" <*> parseExp' 
           )
           
parseTerm   = 
           parseFactor
      <??> (      (\tk t f-> reuseTimesExp [tk] Nothing (Just $ tokenIDP tk) (Just f) (Just t))
              <$> pKey "*" 
              <*> parseTerm
           <|>    (\tk t f-> reuseDivExp [tk] Nothing (Just $ tokenIDP tk) (Just f) (Just t))
              <$> pKey "/"
              <*> parseTerm
           )

parseFactor = 
           parseFactor'
      <??> (    (\tk f f' -> reusePowerExp [tk] Nothing (Just $ tokenIDP tk) (Just f') (Just f))
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
     
parseIdent = 
         (\strTk -> reuseIdent [strTk] Nothing (Just $ tokenIDP strTk) Nothing (Just $ tokenString strTk))
     <$> pLIdent


-- primitive, what to do with idp?

-- maybe make a primIDP ? that takes the idp out of the string_? Then string has an idp field,
-- but it is not used on presentation. (maybe it can be hidden from the user)


--pString_ = 
--         (\string -> reuseString_ [string] Nothing (Just $ tokenIDP string) (Just $ lIdentVal string))
--     <$> pLIdent
-- parser that ignores idp, we want to specify this at the parent level!
--pString__ = 
--         (\string -> reuseString_ [string] Nothing Nothing (Just $ lIdentVal string))
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
--           (\strTk -> reuseString_ [] Nothing (Just $ tokenString strTk)) 
          mkString_
     <$>  pLIdent     
     <|> (HoleString_ <$ pStructural HoleString_Node)


parseInt_ = pStr $
--           (\strTk -> reuseString_ [] Nothing (Just $ tokenString strTk)) 
          mkInt_
     <$>  pInt
     <|> (HoleInt_ <$ pStructural HoleInt_Node)
-- bit hacky


parseBool_ = pStr $
--           (\strTk -> reuseString_ [] Nothing (Just $ tokenString strTk)) 
         mkBool_
     <$> (True <$ pTrue <|> False <$ pFalse)
     <|> (HoleBool_ <$ pStructural HoleBool_Node)
-}

parseIntExp =
         (\tk -> reuseIntExp [tk] Nothing (Just $ tokenIDP tk) (Just $ read (tokenString tk)))
     <$> pInt

parseBoolExp = 
         (\tk -> reuseBoolExp [tk] Nothing (Just $ tokenIDP tk) (Just True))
     <$> pTrue
  <|>    (\tk -> reuseBoolExp [tk] Nothing (Just $ tokenIDP tk) (Just False))
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

 








---- needs to be here temporarily due to mkToken (which will be transfered to scanner)
-- put all tokens in one big list
-- UNCLEAR: what happens when list is presented again? Will it ever? Maybe we can avoid it, even with the new correcting parser
-- TODO put keyword stuff in Scanner layer
--      check what happens with tokens without context info. It seems they get it from higher up
--      in the tree now, which seems wrong. 


-- Right now postScanStr puts the structural children in the list arg of the structural token
-- Maybe the entire functionality (including graph stuff) should be done in Scanner
postScanStr :: [String] -> Maybe node -> Presentation doc node clip UserToken -> [Token doc node clip UserToken]
postScanStr kwrds ctxt (EmptyP _)           = []
postScanStr kwrds ctxt (StringP _ _)        = []
postScanStr kwrds ctxt (TokenP _ _)         = debug Err ("*** PresentationParser.postScanStr: Token in structural presentation") []
postScanStr kwrds ctxt (ImageP _ _ _)         = []
postScanStr kwrds ctxt (PolyP _ _ _ _)        = []
postScanStr kwrds ctxt (RectangleP _ _ _ _ _) = []
postScanStr kwrds ctxt (EllipseP _ _ _ _ _)   = []
postScanStr kwrds ctxt (WithP _ pres)       = postScanStr kwrds ctxt pres
postScanStr kwrds ctxt (OverlayP _ [])      = []
postScanStr kwrds ctxt (OverlayP _ (pres:press)) = postScanStr kwrds ctxt pres
postScanStr kwrds ctxt (ColP i _ _ press)   = concatMap (postScanStr kwrds ctxt) press
postScanStr kwrds ctxt (RowP i _ press)     = concatMap (postScanStr kwrds ctxt) press
postScanStr kwrds ctxt (LocatorP l pres)    = postScanStr kwrds (Just l) pres  
postScanStr kwrds ctxt (GraphP i d _ _ es press) = GraphTk d es ctxt i : concatMap (postScanStr kwrds ctxt) press
postScanStr kwrds ctxt (VertexP i v x y _ pres)  = VertexTk v (x,y) ctxt i : postScanStr kwrds ctxt pres  
postScanStr kwrds ctxt (ParsingP i _ pres)     = [ParsingTk pres (postScanPrs kwrds ctxt pres) i]
--postScanStr kwrds ctxt (ParsingP i pres)   = [StructuralTk (Just NoNode) pres (postScanPrs kwrds ctxt pres ctxt) i]
postScanStr kwrds ctxt (StructuralP i pres)  = [StructuralTk ctxt pres (postScanStr kwrds ctxt pres) i]
postScanStr kwrds ctxt (FormatterP i press)  = concatMap (postScanStr kwrds ctxt) press
postScanStr kwrds ctxt pres = debug Err ("*** PresentationParser.postScanStr: unimplemented presentation: " ++ show pres) []


postScanPrs :: [String] -> Maybe node -> Presentation doc node clip UserToken -> [Token doc node clip UserToken]
postScanPrs kwrds ctxt (EmptyP _)           = []
postScanPrs kwrds ctxt (StringP _ "")       = []
postScanPrs kwrds ctxt (StringP i str)      = [mkToken kwrds str ctxt i]
postScanPrs kwrds ctxt (TokenP i (StructuralTk loc pres [] idp)) = [StructuralTk loc pres (postScanStr kwrds ctxt pres) idp]
postScanPrs kwrds ctxt (TokenP i t)         = [t]
postScanPrs kwrds ctxt (ImageP _ _ _)         = []
postScanPrs kwrds ctxt (PolyP _ _ _ _)        = []
postScanPrs kwrds ctxt (RectangleP _ _ _ _ _) = []
postScanPrs kwrds ctxt (EllipseP _ _ _ _ _)   = []
postScanPrs kwrds ctxt (WithP _ pres)       = postScanPrs kwrds ctxt pres
postScanPrs kwrds ctxt (OverlayP _ [])      = []
postScanPrs kwrds ctxt (OverlayP _ (pres:press)) = postScanPrs kwrds ctxt pres
postScanPrs kwrds ctxt (ColP i _ _ press)   = concatMap (postScanPrs kwrds ctxt) press
postScanPrs kwrds ctxt (RowP i _ press)     = concatMap (postScanPrs kwrds ctxt) press
postScanPrs kwrds ctxt (LocatorP l pres)    = postScanPrs kwrds (Just l) pres
postScanPrs kwrds ctxt (GraphP i _ _ _ _ press) = debug Err ("WARNING: presentation contains Graph that is not part of a structural presentation") []
postScanPrs kwrds ctxt (VertexP _ _ _ _ _ pres) = debug Err ("WARNING: presentation contains Vertex that is not part of a structural presentation") []
postScanPrs kwrds ctxt (ParsingP _ _ pres)    = postScanPrs kwrds ctxt pres
postScanPrs kwrds ctxt (StructuralP i pres) = [StructuralTk ctxt pres (postScanStr kwrds ctxt pres) i ]
postScanPrs kwrds ctxt (FormatterP i press) = concatMap (postScanPrs kwrds ctxt) press ++ [UserTk (StrTk "\n") "\n" Nothing NoIDP]
postScanPrs kwrds ctxt pres  = debug Err ("*** PresentationParser.postScanPrs: unimplemented presentation: " ++ show pres) []
-- ref to UserTk is now because scanner cannot easily add "\n". The AG scanner will be able to do
-- this and make this ref obsolete. (PostScanPrs will be obsolete when Token type is added to Presentation)



mkToken :: [String] -> String -> Maybe node -> IDP -> Token doc node clip UserToken
mkToken keywords str@(c:_)   ctxt i | str `elem` keywords = UserTk (StrTk str) str ctxt i
                                    | isDigit c           = UserTk IntTk str ctxt i
                                    | isLower c           = UserTk LIdentTk str ctxt i
                                    | isUpper c           = UserTk UIdentTk str ctxt i
                                    | otherwise           = UserTk OpTk str ctxt i

--makeToken str ctxt i = Tk str ctxt i

isSymbolChar c = c `elem` ";,(){}#_|"
