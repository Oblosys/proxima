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

{-
initDoc =  mkRoot $ mkDecls $ concat $ replicate 1 
                            [ mkDecl' (ID 300) (mkIdent "r") $
                                mkLam' (ID 200) (mkIdent "x") $
                                      (mkDiv (mkInt' (ID 209) 1)  (mkId "x"))
                            , mkDecl' (ID 201) (mkIdent' (ID 100) "f") $
                               mkLam' (ID 202) (mkIdent "b") $
                                 mkIf' (ID 203) (ID 204) (ID 205)
                                       (mkInt' (ID 206) 1) 
                                       (mkInt' (ID 207) 2) 
                                       (mkInt' (ID 208) 3)
                            ]

mkRoot = RootDoc NoIDD NoIDP
mkRoot' id = RootDoc NoIDD id
mkDecls = foldr (ConsDecls NoIDD) (NilDecls NoIDD) 
mkDecl = Decl NoIDD NoIDP NoIDP NoIDP NoIDP True
mkDecl' id = Decl NoIDD id NoIDP NoIDP NoIDP True
mkIdent = Ident NoIDD NoIDP
mkIdent' id = Ident NoIDD id
mkLam = LamExp NoIDD NoIDP NoIDP
mkLam' id = LamExp NoIDD id NoIDP
mkSum = PlusExp NoIDD NoIDP
mkDiv = DivExp NoIDD NoIDP
mkId str = IdentExp NoIDD (mkIdent str)
mkId' id str = IdentExp NoIDD (mkIdent' id str)
mkIf = IfExp NoIDD NoIDP NoIDP NoIDP
mkIf' id0 id1 id2 = IfExp NoIDD id0 id1 id2
mkInt = IntExp NoIDD NoIDP
mkInt' id = IntExp NoIDD id
mkProduct = ProductExp NoIDD NoIDP NoIDP []
mkProduct' i1 i2 is = ProductExp NoIDD (ID i1) (ID i2) (map ID is)
mkExps = foldr (ConsExps NoIDD) (NilExps NoIDD) 
-}


initLayout :: LayoutMap
initLayout = listToFM [(IDP (-1), (0,1))]

parsePres pres = {- showDebug' Err ("parsing: "++show pres ++" has result ") $ -} 
                 Just (startRecognize pres) 
                 --let (doc, success) = parsePresentation pres
                 --in  if success then Just doc else Nothing




{-

make functions.

Arguments are origin and pres ID of parsed symbols

The id of the origin is used to set the id of the doc element. 
(only succeeds if origin was of same type)
In case of several symbols (eg. if .. then .. else ..fi): if 1st fails, try 2nd, etc.


For each parse, reuse everything that is not in the parse


-}

 
-- this one has no presentation id's to reuse
makeDecls :: [Decl] -> Decls
makeDecls [] = NilDecls NoIDD
makeDecls (decl:decls) = ConsDecls NoIDD decl (makeDecls decls)


--makeDecl retrieves local state from tokens
makeDecl :: Maybe (Token (Maybe Node)) -> Token (Maybe Node) -> Token (Maybe Node) -> (Ident -> Exp -> Decl)
makeDecl mtoken0 token1 token2 =
  let tpdeclIDP           = case mtoken0 of Just tok -> tokenIDP tok; Nothing -> NoIDP
      (idD, (idP1, idP2)) = recoverIDs2 lamIDD token1 token2
      mOldDecl            = recoverDecl token1 token2
      (expanded, autoLayout) = case mOldDecl of
                                 Just (Decl _ _ _ _ _ exp aut _ _) -> (exp, aut)
                                 Nothing                       -> (True, False)
  in  Decl idD idP1 idP2 tpdeclIDP NoIDP expanded autoLayout

-- a collapsed decl. the ls mappings are taken from old node
makeDecl' ::  Maybe (Token (Maybe Node)) -> Token (Maybe Node) -> Token (Maybe Node) -> (Ident -> Decl)
makeDecl' mtoken0 token1 token2 =
  let tpdeclIDP           = case mtoken0 of Just tok -> tokenIDP tok; Nothing -> NoIDP
      (idD, (idP1, idP2)) = recoverIDs2 lamIDD token1 token2
      mOldDecl            = recoverDecl token1 token2
      (expanded, autoLayout, expr) = case mOldDecl of
                                       Just (Decl _ _ _ _ _ exp aut _ expr) -> (exp, aut, expr)
                                       Nothing                              -> (True, False, HoleExp)
  in  \ident -> Decl idD idP1 idP2 tpdeclIDP NoIDP False autoLayout ident expr
                                               -- this way, also typing ... will collapse the function

-- makeDecl and makeDecl' ************** new pres ids

makeAlts :: [Alt] -> Alts
makeAlts [] = NilAlts NoIDD
makeAlts (alt:alts) = ConsAlts NoIDD alt (makeAlts alts)

makeAlt :: Token (Maybe Node) -> Token (Maybe Node) -> (Ident -> Exp -> Alt)
makeAlt token1 token2 = let (idD, (idP1, idP2)) = recoverIDs2 altIDD token1 token2
                        in  Alt idD idP1 idP2 



makeIdent :: Token (Maybe Node) -> (String -> Ident)
makeIdent token = let (idD, idP) = recoverIDs1 identIDD token
                  in  Ident idD idP NoIDP 



-- Exp


makeIdentExp :: (Ident -> Exp)
makeIdentExp = IdentExp NoIDD


makeIntExp :: Token (Maybe Node) -> (Int -> Exp)
makeIntExp token = let (idD, idP) = recoverIDs1 intExpIDD token
                   in  IntExp idD idP 

makeBoolExp :: Token (Maybe Node) -> (Bool -> Exp)
makeBoolExp token = let (idD, idP) = recoverIDs1 boolExpIDD token
                    in  BoolExp idD idP 



-- ******** rename plus to sum

makeSum :: Token (Maybe Node) -> (Exp -> Exp -> Exp)
makeSum token = let (idD, idP) = recoverIDs1 sumIDD token
                in  PlusExp idD idP 


makeProd :: Token (Maybe Node) -> (Exp -> Exp -> Exp)
makeProd token= let (idD, idP) = recoverIDs1 prodIDD token
                in  TimesExp idD idP 



--makeDiv :: (Maybe Node) -> ID ->  (Exp -> Exp -> Exp)
--makeDiv (Just (ExpNode (DivExp id _ _ _) _)) idA = DivExp id idA
--makeDiv _                                      idA = DivExp NoIDP idA
makeDiv :: Token (Maybe Node) -> (Exp -> Exp -> Exp)
makeDiv token= let (idD, idP) = recoverIDs1 divIDD token
               in  DivExp idD idP 






makePower :: Token (Maybe Node) -> (Exp -> Exp -> Exp)
makePower token= let (idD, idP) = recoverIDs1 powerIDD token
                 in  PowerExp idD idP 


--makePower :: (Maybe Node) -> ID ->  (Exp -> Exp -> Exp)
--makePower (Just (ExpNode (DivExp id _ _ _) _)) idA = PowerExp id idA
--makePower _                                      idA = PowerExp NoIDP idA


makeParen :: Token (Maybe Node) -> Token (Maybe Node) -> (Exp -> Exp)
makeParen token1 token2 = let (idD, (idPopen, idPclose)) = recoverIDs2 parenIDD token1 token2
                          in  ParenExp idD idPopen idPclose


makeLam :: Token (Maybe Node) -> Token (Maybe Node) -> (Ident -> Exp -> Exp)
makeLam token1 token2 = let (idD, (idP1, idP2)) = recoverIDs2 lamIDD token1 token2
                        in  LamExp idD idP1 idP2 


makeApp :: (Exp -> Exp -> Exp)
makeApp = AppExp NoIDD

makeCase :: Token (Maybe Node) -> Token (Maybe Node) -> (Exp -> Alts -> Exp)
makeCase token1 token2 = let (idD, (idP1, idP2)) = recoverIDs2 caseIDD token1 token2
                         in  CaseExp idD idP1 idP2 

makeLet :: Token (Maybe Node) -> Token (Maybe Node) -> (Decls -> Exp -> Exp)
makeLet token1 token2 = let (idD, (idP1, idP2)) = recoverIDs2 letIDD token1 token2
                        in  LetExp idD idP1 idP2 


makeIf :: Token (Maybe Node) -> Token (Maybe Node) -> Token (Maybe Node) -> (Exp -> Exp -> Exp -> Exp)
makeIf token1 token2 token3 = let (idD, (idP1, idP2, idP3)) = recoverIDs3 ifIDD token1 token2 token3
                              in  IfExp idD idP1 idP2 idP3


makeList :: Token (Maybe Node) -> Token (Maybe Node) -> [Token (Maybe Node)]-> ([Exp] -> Exp)
makeList token1 token2 tokens = let -- TODO: recover node id properly
                                       idPopen = tokenIDP token1 
                                       idPclose = tokenIDP token2
                                       idPseps = map tokenIDP tokens
                                   in  ListExp NoIDD idPopen idPclose idPseps . makeExps

makeProduct :: Token (Maybe Node) -> Token (Maybe Node) -> [Token (Maybe Node)]-> ([Exp] -> Exp)
makeProduct token1 token2 tokens = let -- TODO: recover node id properly
                                       idPopen = tokenIDP token1 
                                       idPclose = tokenIDP token2
                                       idPseps = map tokenIDP tokens
                                   in  ProductExp NoIDD idPopen idPclose idPseps . makeExps


makeExps :: [Exp] -> Exps
makeExps [] = NilExps NoIDD
makeExps (exp:exps) = ConsExps NoIDD exp (makeExps exps)


recoverIDs1 :: (Node -> Maybe IDD) -> (Token (Maybe Node)) -> (IDD, IDP)
recoverIDs1 idOfNode t1 = 
  let idP = tokenIDP t1
  in  case do { node <- tokenNode t1 
              ; idOfNode node
              } of
        Just idD -> (idD,  idP)
        Nothing  -> (NoIDD, idP)

recoverIDs2 :: (Node-> Maybe IDD) -> (Token (Maybe Node)) -> (Token (Maybe Node)) -> (IDD, (IDP, IDP))
recoverIDs2 idOfNode t1 t2 = 
  let idP1 = tokenIDP t1
      idP2 = tokenIDP t2
  in  case do { node <- tokenNode t1 
              ; idOfNode node
              } of
        Just idD -> (idD, (idP1, idP2))
        Nothing  -> case do { node <- tokenNode t2
                            ; idOfNode node
                            } of
                      Just idD -> (idD, (idP1, idP2))
                      Nothing  -> (NoIDD, (idP1, idP2))

-- try to retrieve the original Decl from its tokens
recoverDecl :: (Token (Maybe Node)) -> (Token (Maybe Node)) -> Maybe Decl
recoverDecl t1 t2 = 
      case do { node <- tokenNode t1 
              ; case node of 
                  DeclNode decl@(Decl _ _ _ _ _ _ _ _ _) _ -> Just decl
                  _                                       -> Nothing
              } of
        Just decl -> Just decl
        Nothing  -> case do { node <- tokenNode t2
                            ; case node of 
                                DeclNode decl@(Decl _ _ _ _ _ _ _ _ _) _ -> Just decl
                                _                                        -> Nothing
                            } of
                      Just decl -> Just decl
                      Nothing  -> Nothing

recoverIDs3 :: (Node-> Maybe IDD) -> (Token (Maybe Node)) -> (Token (Maybe Node))-> (Token (Maybe Node))
            -> (IDD, (IDP, IDP, IDP))
recoverIDs3 idOfNode t1 t2 t3 = 
  let idP1 = tokenIDP t1
      idP2 = tokenIDP t2
      idP3 = tokenIDP t3
  in  case do { node <- tokenNode t1 
              ; idOfNode node
              } of
        Just idD -> (idD, (idP1, idP2, idP3))
        Nothing  -> case do { node <- tokenNode t2
                            ; idOfNode node
                            } of
                      Just idD -> (idD, (idP1, idP2, idP3))
                      Nothing  -> case do { node <- tokenNode t3
                                          ; idOfNode node
                                          } of
                                    Just idD -> (idD, (idP1, idP2, idP3))
                                    Nothing  -> (NoIDD, (idP1, idP2, idP3))
  

makeStructuralExp :: (Maybe Node) -> Presentation -> Exp
makeStructuralExp nd@(Just (ExpNode (DivExp idD idP _ _) _)) pres = 
  case gatherChildren pres Nothing of
    [c1, c2] ->
      let (c1', c2') = (recognizeExp c1, recognizeExp c2)
      in  DivExp idD idP c1' c2'                     -- where should this ID come from?
    _ -> debug Err ("PresentationParser.makeStructuralExp: structure was changed " ++ show pres) HoleExp    
makeStructuralExp nd@(Just (ExpNode (PowerExp idD idP _ _) _)) pres = 
  case gatherChildren pres Nothing of
    [c1, c2] ->
      let (c1', c2') = (recognizeExp c1, recognizeExp c2)
      in  PowerExp idD idP c1' c2'                     -- where should this ID come from?
    _ -> debug Err ("PresentationParser.makeStructuralExp: structure was changed " ++ show pres) HoleExp    
makeStructuralExp nd@(Just (ExpNode _ _)) pres = 
  debug Err ("PresentationParser.makeStructuralExp: structural parse error " ++ show nd) HoleExp    
makeStructuralExp mn _ = debug Err ("PresentationParser.makeStructuralExp: no exp node located " ++ show mn) HoleExp



startRecognize :: Presentation -> (EnrichedDoc, InsertedTokenList, DeletedTokenMap)
startRecognize pres = 
  case gatherChildren pres Nothing of
    [d] ->
      let (d', inss, dels) = recognizeEnr d
      in  (d', inss, dels)
    _ -> debug Err ("PresentationParser.startRecognize: incorrect Root presentation " ++ show pres) (HoleEnr, [], emptyFM)



recognizeEnr (Left _)                  = debug Err "recognizeEnr: No parser for Enr" (HoleEnr, [], emptyFM)
recognizeEnr (Right (Nothing, pres))   = debug Err "Problem" (HoleEnr, [], emptyFM)
recognizeEnr (Right (Just node, pres)) = -- structural
  makeStructuralEnr (Just node) pres      

makeStructuralEnr nd@(Just node@(EnrNode (RootEnr idD idP _ _ tpinfo doc) _)) pres = 
  case gatherChildren pres Nothing of
    [c0,c1] ->
      let (c0') = recognizeDeclsIdsPres c0 -- need a different recognizer for the id list
          (c1', inss, dels) = recognizeDecls c1
      in if null inss && isEmptyFM dels 
         then (RootEnr idD idP c0' c1' tpinfo doc, inss, dels) -- makeEnr
         else (ParseErrEnr node pres {-(map show inss ++ map show (fmToList dels))-}, [], emptyFM)
    cs -> debug Err ("PresentationParser.makeStructuralEnr: structure was changed " ++ show pres++show (length cs)) (HoleEnr, [], emptyFM) 
makeStructuralEnr mn _ = debug Err ("PresentationParser.makeStructuralEnr: no exp node located " ++ show mn) (HoleEnr, [], emptyFM)


recognizeDecls :: (Either (Maybe Node, Presentation) (Maybe Node, Presentation)) -> (Decls, InsertedTokenList, DeletedTokenMap)
recognizeDecls (Left (Nothing, pres))   = debug Err "Problem" (HoleDecls, [], emptyFM)
recognizeDecls (Left (Just node, pres)) = -- parsing
  let (decls, errs) = runParser parseDecls (ParsePres (LocatorP node pres))
  in  if null errs then (decls, [], emptyFM) else (ParseErrDecls node pres (map show errs), [], emptyFM)
--      dels = concatMap (extractDeletedTks pres) errs
--    in {- debug Err ({-"dels:"++show dels-}
--                  "parsed:"++show decls) -} (decls, [], listToFM dels)
recognizeDecls (Right _)                = debug Err "recognizeDoc: No structure recognizer for DeclS" (HoleDecls, [], emptyFM)



extractDeletedTks :: Presentation -> Message a -> [(IDP, Presentation)]
extractDeletedTks pres (Msg ('i':_, position, expecting)) = []
--                           "in unused part of input" we have a problem here
extractDeletedTks pres (Msg ('d':_, 'i':_, expecting))      = debug Err "ProxParser.processParseErr: delete at end" [] --Right (EmptyP NoIDP, NoIDP)
extractDeletedTks pres (Msg ('d':_, position, expecting)) = 
  let ((delId, _::String),(succId, _::String)) = read position
  in  case getPres delId pres of
        Nothing   -> debug Err "ProxParser.processParseErr: id not in presentation" [] --Right (EmptyP NoIDP, succId)
        Just delPres -> [(succId, delPres)]
{-
extractInsertedTks :: Presentation -> Message a -> [(IDP, Presentation)]
extractInsertedTks pres (Msg ('d':_, position, expecting)) = []
--                           "in unused part of input" we have a problem here
extractInsertedTks pres (Msg ('i':_, 'u':_, expecting))      = debug Err "ProxParser.processParseErr: delete at end" [] --Right (EmptyP NoIDP, NoIDP)
extractInsertedTks pres (Msg ('i':_, position, expecting)) = 
  let ((delId, _::String),(succId, _::String)) = read position
  in  case getPres delId pres of
        Nothing   -> debug Err "ProxParser.processParseErr: id not in presentation" [] --Right (EmptyP NoIDP, succId)
        Just delPres -> [(succId, delPres)]
-}

recognizeExp :: (Either (Maybe Node, Presentation) (Maybe Node, Presentation)) -> Exp
recognizeExp (Left (Nothing, pres))    = debug Err "Problem" HoleExp 
recognizeExp (Left (Just node, pres))  = -- parsing
  let (exp, errs) = runParser parseExp (ParsePres (LocatorP node pres))
  in  if null errs then exp else ParseErrExp node pres
recognizeExp (Right (Nothing, pres))   = debug Err "Problem" HoleExp 
recognizeExp (Right (Just node, pres)) = -- structural
  makeStructuralExp (Just node) pres      



--parseDoc :: TreeParser Document
parseEnr = (\ds -> RootEnr NoIDD NoIDP ds) <$> parseDecls

parseDecls :: TreeParser Decls
parseDecls  =    makeDecls 
             <$> pList parseDecl

makeStructuralDecl :: (Maybe Node) -> Presentation -> Decl
makeStructuralDecl nd@(Just (DeclNode d _)) pres = d  
makeStructuralDecl mn _ = debug Err ("PresentationParser.makeStructuralDecl: no decl node located " ++ show mn) HoleDecl


pMaybe parser = Just <$> parser `opt` Nothing


-- parsing structurals is still a bit tricky, because any structural will do, including holes
-- therefore, the {} are required around it. TODO
parseDecl :: TreeParser Decl
parseDecl  =  (\mtk0 ident tk1 exp tk2 -> makeDecl mtk0 tk1 tk2 ident exp) 
             <$> pMaybe (pSym (Structural Nothing (EmptyP NoIDP) NoIDP)) -- derived type sig
                 <*> parseIdent <*> pKey "=" <*> parseExp  <*> pKeyC 1 ";"
         <|>
              (\mtk0 ident tk1 tk2 -> makeDecl' mtk0 tk1 tk2 ident) 
             <$> pMaybe (pSym (Structural Nothing (EmptyP NoIDP) NoIDP)) -- derived type sig
                 <*> parseIdent <*> pKey "=" <*> pKey "..."

--          (\(Structural mn pr _) -> makeStructuralDecl mn pr)
--             <$ pKey "+" <*>   pSym (Structural Nothing (EmptyP NoIDP) NoIDP)       
             
       
       --  <|>     (\(Structural mn pr _) -> HoleDecl) -- matches with der. type sig, so ambiguous
       --      <$> pSym (Structural Nothing (EmptyP NoIDP) NoIDP) -- EmptyP+NoIDP are ignored in compare
-- chess board


-- remember to that "Chess", "PPT", "board", and "pres" must be keywords in PresentationParsing

          <|> (\t ->  BoardDecl NoIDD (tokenIDP t) NoIDP) <$> pKey "Chess" <* pKey ":" <*> pBoard
          <|> (\t ->  PPPresentationDecl NoIDD (tokenIDP t) NoIDP) <$> pKey "PPT" <* pKey ":" <*> pPPPresentation

pBoard :: TreeParser Board
pBoard =    ((\_ -> initBoard) <$> pKey "board")
         <|>(\(Structural mn pr _) -> makeStructuralBoard mn pr) <$> pSym (Structural Nothing (EmptyP NoIDP) NoIDP) 

makeStructuralBoard :: (Maybe Node) -> Presentation -> Board
makeStructuralBoard nd@(Just (BoardNode b _)) pres = b  
makeStructuralBoard mn _ = debug Err ("PresentationParser.makeStructuralExp: no exp node located " ++ show mn) initBoard


pPPPresentation :: TreeParser PPPresentation
pPPPresentation =  ((\_ -> initPPPresentation) <$> pKey "pres")
         <|>(\(Structural mn pr _) -> makeStructuralPPPresentation mn pr) <$> pSym (Structural Nothing (EmptyP NoIDP) NoIDP) 



-- makeStructuralExp is at end of file


-- end chess board
      
parseAlts :: TreeParser Alts
parseAlts  =    makeAlts 
             <$> pList parseAlt

-- parsing structurals is still a bit tricky, because any structural will do, including holes
-- therefore, the {} are required around it. Will be fixed when structurals have a type TODO
parseAlt :: TreeParser Alt
parseAlt  =  (\ident tk1 exp tk2 -> makeAlt tk1 tk2 ident exp) 
             <$> parseIdent <*> pArrow <*> parseExp  <*> pKeyC 4 ";"


parseIfExp :: TreeParser Exp
parseIfExp =     (\tk1 c tk2 th tk3 el -> makeIf tk1 tk2 tk3 c th el) 
             <$> pIf <*> parseExp <*> pThen <*> parseExp <*> pElse <*> parseExp

            
parseLamExp :: TreeParser Exp
parseLamExp =     (\tk1 a tk2 b -> makeLam tk1 tk2 a b) 
              <$> pLambda <*> parseIdent <*> pArrow <*> parseExp

            
parseCaseExp :: TreeParser Exp
parseCaseExp =     (\tk1 a tk2 b -> makeCase tk1 tk2 a b) 
              <$> pCase <*> parseExp <*> pOf <*> parseAlts

parseLetExp :: TreeParser Exp
parseLetExp =     (\tk1 a tk2 b -> makeLet tk1 tk2 a b) 
              <$> pLet <*> parseDecls <*> pIn <*> parseExp

parseExp :: TreeParser Exp
parseExp   = {-pMarkParseErr (ParseErrExp NoNode (EmptyP NoIDP)) ( -}   
                 parseExp'   -- e and t are flipped in lambda for <??>
            <??> ( (\tk e t-> makeSum tk t e)  <$> pKey "+" <*> parseExp )
             -- )

parseExp' :: TreeParser Exp
parseExp'   = {-pMarkParseErr (ParseErrExp NoNode (EmptyP NoIDP)) ( -}   
                 parseTerm   -- e and t are flipped in lambda for <??>
            <??> ( (\tk e t-> makeDiv tk t e)  <$> pKey "%" <*> parseExp' )
             -- )


parseTerm :: TreeParser Exp
parseTerm   =      parseFactor
              <??> (    (\tk t f-> makeProd tk f t) <$> pKey "*" <*> parseTerm
                    <|> (\tk t f-> makeDiv tk f t)  <$> pKey "/" <*> parseTerm
                   )

parseFactor :: TreeParser Exp
parseFactor =      parseFactor'
              <??> ((\tk f' f -> makePower tk f f') <$> pKey "^" <*> parseFactor)

parseFactor' :: TreeParser Exp
parseFactor' =   parseFactor''
             <??> ((\f' f -> makeApp f f') <$> parseFactor')

parseFactor'' :: TreeParser Exp
parseFactor'' = --  pHoleExp
            -- <|>
                 parseIntExp 
             <|> parseBoolExp
             <|> parseIdentExp
             <|> parseListExp
             <|> parseParenExp
             <|> parseIfExp
             <|> parseLamExp
             <|> parseCaseExp
             <|> parseLetExp
             <|> pStructureExp

pStructureExp :: TreeParser Exp
pStructureExp = (\(Structural mn pr _) -> makeStructuralExp mn pr) <$> pSym (Structural Nothing (EmptyP NoIDP) NoIDP) -- EmptyP+NoIDP are ignored in compare
-- laziness takes care that makeStructuralExp is only evaluated when it is part of the result of a successful
-- parse

-- still a bit flakey at the leaf, with the int that has a special case, but
-- this is an instance of the presentationless presentation which needs to be solved in general

-- TODO add empty
parseListExp =
    (\tk1 elts tk2 -> case elts of
                        Nothing        -> makeList tk1 tk2 [] []
                        Just (e, etks) -> let (tks, es) = unzip etks
                                          in  makeList tk1 tk2 tks (e:es))
               <$>     pKey "[" 
                   <*> ((     (\e etks -> Just (e,etks)) 
                          <$> parseExp <*> pList ((,) <$> pKey "," <*> parseExp)
                        ) `opt` Nothing)
                   <*> pKey "]"
               
parseParenExp = 
    (\tk1 e etks tk2 -> if null etks then makeParen tk1 tk2 e
                                     else let (tks, es) = unzip etks
                                          in  makeProduct tk1 tk2 tks (e:es))
                 <$> pKey "(" <*> parseExp <*> pList ((,) <$> pKey "," <*> parseExp) <*> pKey ")"



pIf :: TreeParser (Token (Maybe Node))
pIf = pKey "if"

pThen :: TreeParser (Token (Maybe Node))
pThen = pKey "then"

pElse :: TreeParser (Token (Maybe Node))
pElse = pKey "else"

pLambda :: TreeParser (Token (Maybe Node))
pLambda =    pKey "\\" 


pArrow :: TreeParser (Token (Maybe Node))
pArrow =     pKey "->"
         <|> pKey "\174"

pCase :: TreeParser (Token (Maybe Node))
pCase = pKey "case"

pOf :: TreeParser (Token (Maybe Node))
pOf = pKey "of"

pLet :: TreeParser (Token (Maybe Node))
pLet = pKey "let"

pIn :: TreeParser (Token (Maybe Node))
pIn = pKey "in"

pTrue :: TreeParser (Token (Maybe Node))
pTrue = pKey "True"

pFalse :: TreeParser (Token (Maybe Node))
pFalse = pKey "False"


parseBoolExp :: TreeParser Exp
parseBoolExp = (\tk -> makeBoolExp tk True)  <$> pTrue
           <|> (\tk -> makeBoolExp tk False) <$> pFalse

parseIntExp :: TreeParser Exp
parseIntExp = (\tk -> makeIntExp tk (intVal tk)) <$> pInt

parseIdentExp :: TreeParser Exp
parseIdentExp = makeIdentExp <$> parseIdent

parseIdent :: TreeParser Ident
parseIdent = -- pHoleIdent
         --    <|>
          (\tk -> makeIdent tk (lIdentVal tk)) <$> pLIdent
      --      



{-
pHoleExp :: TreeParser Exp
pHoleExp =  (\(Structural mn pr _) -> HoleExp) <$> pStruct 

pHoleIdent :: TreeParser Ident
pHoleIdent =  (\(Structural mn pr _) -> HoleIdent) <$> pStruct 

pHoleDecl :: TreeParser Decl
pHoleDecl =  (\(Structural mn pr _) -> HoleDecl) <$> pStruct 
-}


-- recognizer parser for the identifier list

-- parse the list of decls
recognizeDeclsIdsPres :: (Either (Maybe Node, Presentation) (Maybe Node, Presentation)) -> (Decls)
recognizeDeclsIdsPRes (Left (Nothing, pres))   = debug Err "Problem" (HoleDecls)
recognizeDeclsIdsPres (Left (Just node, pres)) = -- parsing
  let (decls, errs) = runParser parseDeclsIdsPres (ParsePres (LocatorP node pres))
  in  if null errs then decls else (ParseErrDecls node pres (map show errs))
recognizeDeclsIdsPres (Right _)                = debug Err "recognizeDeclsIdPres: No structure recognizer for DeclS" (HoleDecls)

parseDeclsIdsPres :: TreeParser Decls
parseDeclsIdsPres  = makeDecls
             <$>  pList parseDeclIdsPres

parseDeclIdsPres :: TreeParser Decl
parseDeclIdsPres  =  (\(Structural mn pr _) -> makeStructuralDeclIdsPres mn pr)
             <$>   pSym (Structural Nothing (EmptyP NoIDP) NoIDP)       

makeStructuralDeclIdsPres :: (Maybe Node) -> Presentation -> Decl
makeStructuralDeclIdsPres nd@(Just (DeclNode (Decl idD idP0 idP1 idP2 idP3 b1 b2 _ exp) _)) pres = 
  case gatherChildren pres Nothing of
    [c1] ->
      let (c1') = (recognizeIdentIdsPres c1)
      in  Decl idD idP0 idP1 idP2 idP3 b1 b2 c1' exp
    _ -> debug Err ("PresentationParser.makeStructuralItem: structure was changed " ++ show pres) HoleDecl
makeStructuralDeclIdsPres nd@(Just (DeclNode d _)) pres = d  
makeStructuralDeclIdsPres mn _ = debug Err ("PresentationParser.makeStructuralDeclIdsPres: no decl node located " ++ show mn) HoleDecl


recognizeIdentIdsPres :: (Either (Maybe Node, Presentation) (Maybe Node, Presentation)) -> Ident
recognizeIdentIdsPres (Left (Nothing, pres))   = debug Err "Problem" HoleIdent
recognizeIdentIdsPres (Left (Just node, pres)) = -- parsing
  let (ident, errs) = runParser parseIdentIdsPres (ParsePres (LocatorP node pres))
  in  if null errs then ident else ParseErrIdent node pres 
recognizeIdentIdsPres (Right _)                = debug Err "recognizeIdentIdsPres: No structure recognizer for String_" HoleIdent


-- special ident parser that reuses the original token (because layout in other pres is local state)
parseIdentIdsPres :: TreeParser Ident
parseIdentIdsPres = -- pHoleIdent
         --    <|>
          (\tk -> makeIdent' tk (lIdentVal tk)) <$> pLIdent
      --      


makeIdent' :: Token (Maybe Node) -> (String -> Ident)
makeIdent' token = let (idD, idP1, idP2) = recoverIdent token
                   in  Ident idD idP1 idP2 
 where recoverIdent token = case do { node <- tokenNode token 
                                     ; case node of 
                                         IdentNode ident@(Ident idD idP0 idP1 str) _ -> Just (idD, idP0, idP1)
                                         _                                           -> Nothing
                                     } of
                              Just ids -> ids
                              Nothing   ->  (NoIDD, NoIDP, NoIDP)


{-
case do { node <- tokenNode t1 
              ; case node of 
                  IdentNode ident@(Ident idD idP0 idP1 str) _ -> Just decl
                  _                                           -> Nothing
              } of
        Just decl -> Just ident
        Nothing  -> 

-}
{-
makeStructuralItem :: (Maybe Node) -> Presentation -> Item
makeStructuralItem nd@(Just (ItemNode (StringItem idD string) _)) pres = 
  case gatherChildren pres Nothing of
    [c1] ->
      let (c1') = (recognizeString_ c1)
      in  StringItem idD c1'
    _ -> debug Err ("PresentationParser.makeStructuralItem: structure was changed " ++ show pres) HoleItem

-}


--- structure recognition for PPPresentation


makeStructuralPPPresentation :: (Maybe Node) -> Presentation -> PPPresentation
makeStructuralPPPresentation nd@(Just (PPPresentationNode (PPPresentation idD viewtp _) _)) pres = 
  case gatherChildren pres Nothing of
    [c1] ->
      let (c1') = (recognizeSlides c1)
      in  PPPresentation idD viewtp c1'                     -- where should this ID come from?
    _ -> debug Err ("PresentationParser.makeStructuralPPPresentation: structure was changed " ++ show pres) HolePPPresentation
makeStructuralPPPresentation nd@(Just (PPPresentationNode _ _)) pres = 
  debug Err ("PresentationParser.makeStructuralPPPresentation: structural parse error " ++ show nd) HolePPPresentation
makeStructuralPPPresentation mn _ = debug Err ("PresentationParser.makeStructuralPPPresentation: no PPPresentation node located " ++ show mn) HolePPPresentation


recognizeSlides (Left _)                  = debug Err "recognizeSlides: No parser for Slides" HoleSlides
recognizeSlides (Right (Nothing, pres))   = debug Err "Problem" HoleSlides
recognizeSlides (Right (Just node, pres)) = -- structural
  makeStructuralSlides (Just node) pres      


-- for a list, makeStructural is different. because cons nodes sometimes have no presentation
-- there will not be structural nodes to recover them. Hence gatherChildren returns the children
-- of the list. The consNodes are recreated.
makeStructuralSlides :: (Maybe Node) -> Presentation -> Slides
makeStructuralSlides nd@(Just (SlidesNode (ConsSlides idD slide slides) _)) pres = 
  case gatherChildren pres Nothing of
    cs ->
      let cs' = map recognizeSlide cs
      in mkListSlides cs'
    _ -> debug Err ("PresentationParser.makeStructuralSlides: structure was changed " ++ show pres) HoleSlides
makeStructuralSlides nd@(Just (SlidesNode (NilSlides idD) _)) pres = 
          NilSlides idD
makeStructuralSlides nd@(Just (SlidesNode _ _)) pres = 
  debug Err ("PresentationParser.makeStructuralSlides: structural parse error " ++ show nd) HoleSlides
makeStructuralSlides mn _ = debug Err ("PresentationParser.makeStructuralSlides: no Slides node located " ++ show mn) HoleSlides

mkListSlides [] = NilSlides NoIDD  
mkListSlides (sl:sls) = ConsSlides NoIDD sl $ mkListSlides sls



recognizeItems (Left _)                  = debug Err "recognizeItems: No parser for Items" HoleItems
recognizeItems (Right (Nothing, pres))   = debug Err "Problem" HoleItems
recognizeItems (Right (Just node, pres)) = -- structural
  makeStructuralItems (Just node) pres      

makeStructuralItems :: (Maybe Node) -> Presentation -> Items
makeStructuralItems nd@(Just (ItemsNode (ConsItems idD item items) _)) pres = 
  case gatherChildren pres Nothing of
    cs ->
      let cs' = map recognizeItem cs
      in mkListItems cs'
    _ -> debug Err ("PresentationParser.makeStructuralItems: structure was changed " ++ show pres) HoleItems
makeStructuralItems nd@(Just (ItemsNode (NilItems idD) _)) pres = 
          NilItems idD
makeStructuralItems nd@(Just (ItemsNode _ _)) pres = 
  debug Err ("PresentationParser.makeStructuralItems: structural parse error " ++ show nd) HoleItems
makeStructuralItems mn _ = debug Err ("PresentationParser.makeStructuralItems: no Items node located " ++ show mn) HoleItems

mkListItems [] = NilItems NoIDD  
mkListItems (sl:sls) = ConsItems NoIDD sl $ mkListItems sls


-- 
recognizeSlide (Left _)                  = debug Err "recognizeSlide: No parser for Slide" HoleSlide
recognizeSlide (Right (Nothing, pres))   = debug Err "Problem" HoleSlide
recognizeSlide (Right (Just node, pres)) = -- structural
  makeStructuralSlide (Just node) pres      

makeStructuralSlide :: (Maybe Node) -> Presentation -> Slide
makeStructuralSlide nd@(Just (SlideNode (Slide idD title itemlist) _)) pres = 
  case gatherChildren pres Nothing of
    [c1, c2] ->
      let (c1', c2') = (recognizeString_ c1, recognizeItemList c2)
      in  Slide idD c1' c2'                     -- where should this ID come from?
    _ -> debug Err ("PresentationParser.makeStructuralSlide: structure was changed " ++ show pres) HoleSlide
makeStructuralSlide nd@(Just (SlideNode _ _)) pres = 
  debug Err ("PresentationParser.makeStructuralSlide: structural parse error " ++ show nd) HoleSlide
makeStructuralSlide mn _ = debug Err ("PresentationParser.makeStructuralSlide: no Slide node located " ++ show mn) HoleSlide

recognizeString_ :: (Either (Maybe Node, Presentation) (Maybe Node, Presentation)) -> String_
recognizeString_ (Left (Nothing, pres))   = debug Err "Problem" HoleString_
recognizeString_ (Left (Just node, pres)) = -- parsing
  let (string_, errs) = runParser parseString_ (ParsePres (LocatorP node pres))
  in  if null errs then string_ else ParseErrString_ node pres 
recognizeString_ (Right _)                = debug Err "recognizeDoc: No structure recognizer for String_" HoleString_

recognizeItemList (Left _)                  = debug Err "recognizeItemList: No parser for ItemList" HoleItemList
recognizeItemList (Right (Nothing, pres))   = debug Err "Problem" HoleItemList
recognizeItemList (Right (Just node, pres)) = -- structural
  makeStructuralItemList (Just node) pres      


makeStructuralItemList :: (Maybe Node) -> Presentation -> ItemList
makeStructuralItemList nd@(Just (ItemListNode (ItemList idD listType items) _)) pres = 
  case gatherChildren pres Nothing of
    [c1, c2] ->
      let (c1', c2') = (recognizeListType c1, recognizeItems c2)
      in  ItemList idD c1' c2'
    _ -> debug Err ("PresentationParser.makeStructuralItemList: structure was changed " ++ show pres) HoleItemList
makeStructuralItemList nd@(Just (ItemListNode _ _)) pres = 
  debug Err ("PresentationParser.makeStructuralItemList: structural parse error " ++ show nd) HoleItemList
makeStructuralItemList mn _ = debug Err ("PresentationParser.makeStructuralItemList: no ItemList node located " ++ show mn) HoleItemList


recognizeListType (Left _)                  = debug Err "recognizeListType: No parser for ListType" HoleListType
recognizeListType (Right (Nothing, pres))   = debug Err "Problem" HoleListType
recognizeListType (Right (Just node, pres)) = -- structural
  makeStructuralListType (Just node) pres      

makeStructuralListType :: (Maybe Node) -> Presentation -> ListType
makeStructuralListType nd@(Just (ListTypeNode (Bullet idD) _)) pres =  
         Bullet idD                      -- where should this ID come from?
makeStructuralListType nd@(Just (ListTypeNode (Number idD) _)) pres =  
         Number idD                      -- where should this ID come from?
makeStructuralListType nd@(Just (ListTypeNode (Alpha idD) _)) pres =  
         Alpha idD                      -- where should this ID come from?
makeStructuralListType nd@(Just (ListTypeNode _ _)) pres = 
  debug Err ("PresentationParser.makeStructuralListType: structural parse error " ++ show nd) HoleListType
makeStructuralListType mn _ = debug Err ("PresentationParser.makeStructuralListType: no ListType node located " ++ show mn) HoleListType


recognizeItem (Left _)                  = debug Err "recognizeItem: No parser for Item" HoleItem
recognizeItem (Right (Nothing, pres))   = debug Err "Problem" HoleItem
recognizeItem (Right (Just node, pres)) = -- structural
  makeStructuralItem (Just node) pres      

makeStructuralItem :: (Maybe Node) -> Presentation -> Item
makeStructuralItem nd@(Just (ItemNode (StringItem idD string) _)) pres = 
  case gatherChildren pres Nothing of
    [c1] ->
      let (c1') = (recognizeString_ c1)
      in  StringItem idD c1'
    _ -> debug Err ("PresentationParser.makeStructuralItem: structure was changed " ++ show pres) HoleItem
makeStructuralItem nd@(Just (ItemNode (HeliumItem idD exp) _)) pres = 
  case gatherChildren pres Nothing of
    [c1] ->
      let (c1') = (recognizeExp c1)
      in  HeliumItem idD c1'
    _ -> debug Err ("PresentationParser.makeStructuralItem: structure was changed " ++ show pres) HoleItem
makeStructuralItem nd@(Just (ItemNode (ListItem idD itemList) _)) pres = 
  case gatherChildren pres Nothing of
    [c1] ->
      let (c1') = (recognizeItemList c1)
      in  ListItem idD c1'
    _ -> debug Err ("PresentationParser.makeStructuralItem: structure was changed " ++ show pres) HoleItem
makeStructuralItem nd@(Just (ItemNode _ _)) pres = 
  debug Err ("PresentationParser.makeStructuralItem: structural parse error " ++ show nd) HoleItem
makeStructuralItem mn _ = debug Err ("PresentationParser.makeStructuralItem: no Item node located " ++ show mn) HoleItem




parseString_ :: TreeParser String_
parseString_ = -- pHoleIdent
         --    <|>
          (\tk -> makeString_ tk (lIdentVal tk)) <$> pLIdent
      --      

makeString_ :: Token (Maybe Node) -> (String -> String_)
makeString_ token = let (idD, idP) = recoverIDs1 string_IDD token
                  in  String_ idD

{-

recognizeDoc (Left _)                  = debug Err "recognizeDoc: No parser for Doc" (HoleDoc, [], emptyFM)
recognizeDoc (Right (Nothing, pres))   = debug Err "Problem" (HoleDoc, [], emptyFM)
recognizeDoc (Right (Just node, pres)) = -- structural
  makeStructuralDoc (Just node) pres      

recognizeDecls :: (Either (Maybe Node, Presentation) (Maybe Node, Presentation)) -> (Decls, InsertedTokenList, DeletedTokenMap)
recognizeDecls (Left (Nothing, pres))   = debug Err "Problem" (HoleDecls, [], emptyFM)
recognizeDecls (Left (Just node, pres)) = -- parsing
  let (decls, errs) = runParser parseDecls (ParsePres (LocatorP node pres))
  in  if null errs then (decls, [], emptyFM) else (ParseErrDecls node pres (map show errs), [], emptyFM)
--      dels = concatMap (extractDeletedTks pres) errs
--    in {- debug Err ({-"dels:"++show dels-}
--                  "parsed:"++show decls) -} (decls, [], listToFM dels)
recognizeDecls (Right _)                = debug Err "recognizeDoc: No structure recognizer for DeclS" (HoleDecls, [], emptyFM)


-}





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