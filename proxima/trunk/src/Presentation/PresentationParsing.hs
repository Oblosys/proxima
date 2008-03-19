module Presentation.PresentationParsing where

import Common.CommonTypes
import Presentation.PresTypes
import Presentation.PresLayerTypes
import Evaluation.DocumentEdit

import Common.UU_Parsing hiding (Exp, parse, parseIO)
import qualified Common.UU_Parsing as UU_Parsing
import Char

import Debug.Trace

{-


Design issues with parsing and structure recognizing (choose different name? recognizer is usually parser with Bool result)

Structural parser should take into account the type of the structure it contains. That way an ident hole will not be
mistaken for a decl hole.
Maybe this can be implemented without much effort, the structural token contains a ref to the node.
It will be a bit hacky though.


What about tokenizing structurals as

StructuralToken

StructuralToken      -- Child 0
EndStructuralToken --

StructuralToken      -- Child 1
EndStructuralToken --

etc


EndStructuralToken

The Scanner ensures correct nesting, so even when presentation is not correct, Endstructurals need no parameter
Parsing children can be surrounded by ParsingToken EndParsingTokens in a similar way

Now structure recognizing can be done with a parser
recognize =  parse structural of appropriateType
                   parse children
                   parse endStructural
                   build result

It's just like a parser, but with a very strict structure.

PROBLEM: When several structural presentations for one type exist, we need a way to determine which recognizer to use.
For example tree node with children or without. A parser would use the keyword "+" or "-", but in the recognizer
we somehow have to look at the boolean expansion value of the recognized node since parsing an image of + or - is not
an option.
-}

reuse = Nothing
set = Just


parsePres recognizeEnrichedDoc (TokenP _ (StructuralTk _ _ _ tokens _)) = 
  let (enr,errs) = runParser recognizeEnrichedDoc tokens
  in debug Err ("Parsing:\n"++concatMap (deepShowTks 0) (tokens) ) $ 
     --             ++"\nhas result:"++show enr ) $
     if null errs then Just enr else Nothing

parsePres _ _    = error "parsePres: scanned presentation has wrong format"

pMaybe parser = Just <$> parser `opt` Nothing

pStructuralTk nd = pSym (StructuralTk 0 (Just $ nd (error "This should not have happened") []) (EmptyP NoIDP) [] NoIDP)


applyDummyParameters nd = nd (error "This should not have happened") [] 
-- continues parsing on the children inside the structural token. the structural token is put in front
-- of the children, so reuse can be used on it just like in the normal parsers
--pStr ::  (Editable a doc node clip token, DocNode node, Ord token, Show token) =>
--         ListParser doc node clip token a -> ListParser doc node clip token a
pStr = pStr' (EmptyP NoIDP)

pStrVerbose str = pStr' (StringP NoIDP str)

pStr' prs p = unfoldStructure  
     <$> pSym (StructuralTk 0 Nothing prs [] NoIDP)
 where unfoldStructure structTk@(StructuralTk _ nd pr tokens _) = 
         let (res, errs) = runParser (addHoleParser p) (structTk : tokens) {- (p <|> hole/parseErr parser)-}
         in  if null errs then res else debug Err ("ERROR: Parse error in structural parser:"++(show errs)) parseErr (StructuralParseErr pr)
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."

-- The scoped type variable is necessary to get hole and holeNodeConstr of the same type a.
addHoleParser :: forall a doc node clip token . (DocNode node, Ord token, Show token, Editable a doc node clip token) => ListParser doc node clip token a -> ListParser doc node clip token a 
addHoleParser p =
  p <|> hole <$ pStructuralTk (holeNodeConstr :: a -> Path -> node)
  

pStr'' nodeC hole p = unfoldStructure  
     <$> pSym (StructuralTk 0 Nothing (EmptyP NoIDP) [] NoIDP)
 where unfoldStructure structTk@(StructuralTk _ nd pr tokens _) = 
         let pOrHole = p <|> hole <$ pStructuralTk nodeC
             (res, errs) = runParser pOrHole (structTk : tokens) {- (p <|> hole/parseErr parser)-}
         in  if null errs then res else debug Err ("ERROR: Parse error in structural parser:"++(show errs)) parseErr (StructuralParseErr pr)
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."



pStrAlt ndf p = unfoldStructure  
     <$> pSym (StructuralTk 0 (Just nd) (StringP NoIDP $ show nd) [] NoIDP)
 where unfoldStructure structTk@(StructuralTk _ nd pr tokens _) = 
         let (res, errs) = runParser p (structTk : tokens) {- (p <|> hole/parseErr parser)-}
          in if null errs then res else debug Err ("ERROR: Parse error in structural parser:"++(show errs)) parseErr (StructuralParseErr pr)
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."
       
       nd = applyDummyParameters ndf



-- in case of a parse error, the repaired result is used in the tree and an error message
-- is sent to the prompt.
-- ? parse error is tricky, since the structural parent of the parsing subtree should know
-- when an error occurred. Instead of Maybe, we need something like Reuse|(Set x)|(ParseErr [Err])
-- for structurals, the presentation is lost on a parse error, but structural parse errors
-- are an editor design error and will not arise during document
-- editing, so it's not a problem    parseErr node (row children) errs

-- maybe it will work when there is a separate Parsing token, that contains the old value.
-- but for now, just don't set the node. Do we ever use it?

-- what about presenting parse errors in another presentation than the one with the error?
-- maybe we do want the old value for that one? Right now the parse error presentation is presented
-- so a tree can contain source text (which fails on parsing)


pStrDirty ::  (Editable a doc node clip token, DocNode node, Ord token, Show token) => ListParser doc node clip token (a, Dirty) -> ListParser doc node clip token (a, Dirty)
pStrDirty p = pStrExtra Dirty p


-- pStrExtra is a variant of pStr that allows an extra parser result to be returned in a tuple.
-- extraDefault is a default value for this type in case of a parse error.
pStrExtra ::  (Editable a doc node clip token, DocNode node, Ord token, Show token) =>
              b -> ListParser doc node clip token (a, b) -> ListParser doc node clip token (a, b)
pStrExtra extraDefault p = unfoldStructure  
     <$> pSym (StructuralTk 0 Nothing (EmptyP NoIDP) [] NoIDP)
 where unfoldStructure structTk@(StructuralTk _ nd pr tokens _) = 
         let (res, errs) = runParser p (structTk : tokens) {- (p <|> hole/parseErr parser)-}
         in  if null errs 
             then res 
             else debug Err ("ERROR: Parse error in structural parser:"++(show errs))
                        (parseErr (StructuralParseErr pr),extraDefault)
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."

-- TODO: why do we need the 's in Editable?
pPrs ::  (Editable a doc node clip token, DocNode node, Ord token, Show token) => ListParser doc node clip token a -> ListParser doc node clip token a
pPrs p = unfoldStructure  
     <$> pSym (ParsingTk Nothing Nothing (EmptyP NoIDP) [] NoIDP)
 where unfoldStructure presTk@(ParsingTk _ _ pr tokens _) = 
         let (res, errs) = runParser p tokens
         in  if null errs then res else debug Err ("ERROR: Parse error"++(show errs)) $ parseErr (ParsingParseErr (mkErr errs) tokens (mkClipParser p))
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."

mkErr :: (DocNode node, Ord token, Show token) => [Message (Token doc node clip token)] -> (Int, String)
mkErr msgs =
 let messageText = show msgs
 in  ( case retrieveTokenPosition "Parse Error : before <" messageText of
         Just pos -> pos
         Nothing  -> case retrieveTokenPosition "Repaired by : deleting symbol <" messageText of
                       Just pos -> pos
                       Nothing  -> 0
     ,  messageText
     )

retrieveTokenPosition errStr messageText =
  case drop' errStr messageText of
    Just str -> Just $ read $ takeWhile isDigit str
    Nothing  -> Nothing

drop' :: Eq a => [a] -> [a] -> Maybe [a]
drop' [] ys = Just ys
drop' xs [] = Nothing
drop' xs (y:ys) = if xs `isPrefixOf` (y:ys)
                  then Just $ drop (length xs) $ y:ys
                  else drop' xs ys 
-- Does parseErr need a location? It used to be NoNode anyway.

-- hole parser
{-
       p
   <|>  (\_ -> DeclHole)
        pSym (StructuralTk (Just $ DeclHoleNode hole []) [] NoIDP)
 if we put holeNode and in Editable (maybe better in separate class Parseable)
 then
       (\_ -> hole) -- or reuse
   <$> pSym (StructuralTk (Just holeNode) [] NoIDP)


maybe just one HoleNode?

       (\_ -> hole) -- or reuse

parseErrs are not in the presentation, so we won't need ParseErrNodes

so Div (Parse Err (IntExp 1) "1_") (IntExp 2) is presented as  (StructuralTk "1_" "2")
and the node for the first child is (IntExp 1) There is never a ParseErrNode
-}










{-

Because tokens are not part of the Presentation type yet, we preprocess the the 
StringP values and make a list of tokens. This is closely linked to the scanning
process and should be done in the layout layer.
-}







newtype ParsePres doc node clip token a b c = ParsePres (Presentation doc node clip token) deriving Show

-- parsing bits




  




instance (DocNode node, Ord token, Show token) => Symbol (Token doc node clip token) where

runParser (pp) inp =
      let res = UU_Parsing.parse pp inp
          (Pair v final) = evalSteps (res) 
          errs = getMsgs (res) 
      in  (v, errs)



-- parser for token
pToken :: (DocNode node, Ord token, Show token) =>
          token -> ListParser doc node clip token (Token doc node clip token)
pToken token = pSym $ UserTk 0 token (show token) Nothing (IDP (-1))


-- holes are cheap. actually only holes should be cheap, but presently structurals are all the same
pStruct :: (DocNode node, Ord token, Show token) => ListParser doc node clip token (Token doc node clip token)
pStruct = pCSym 4 (StructuralTk 0 Nothing (EmptyP NoIDP) [] NoIDP)


-- pCostSym expects the parser twice
pCSym c p = pCostSym c p p




strucTk   = StructuralTk 0 Nothing (EmptyP NoIDP) [] (IDP (-1))
parsingTk = (ParsingTk Nothing Nothing (EmptyP NoIDP) [] NoIDP)
graphTk   = GraphTk Dirty [] Nothing (IDP (-1)) -- probably a graph will never be inserted by
vertexTk  = VertexTk (-1) (0,0) Nothing  (IDP (-1))  -- the parser, but if it is, it should be dirty




--- Automatic structure recognizer

class Construct doc node clip token where
  construct :: node -> (Token doc node clip token) -> [Maybe clip] -> clip


mkClipParser p = 
 let clipParser = 
       \tokens ->
         let (res, errs) = runParser p tokens
         in  toClip $ if null errs then res 
                      else debug Err ("ERROR: Parse error"++(show errs)) $ 
                             parseErr (ParsingParseErr (mkErr errs) tokens clipParser)
 in  clipParser




{-
TODO
type error in retrieveArg: more info
error handling in pStructuralEx

what about parseerr for constr? will they occur?
duplicates!
maybe pStructural should fail if wrong type is present?
-}


{- recognize parses a structural token and recognizes its structure. The parser will succeed
   on any structural token.
-}
pStructural :: (Editable a doc node clip token, Clip clip, Construct doc node clip token,
              DocNode node, Ord token, Show token, Show clip) =>
             ListParser doc node clip token a
pStructural = pStructuralEx Nothing


{- recognizeEx parses a structural token and reconizes its structure. The argument can be
   Nothing, in which case any structural token is matched, or it can be a Just node constructor,
   in which case the parser only succeeds on a structural token with that constructor.
-}   
pStructuralEx :: (Editable a doc node clip token, Clip clip, Construct doc node clip token,
                DocNode node, Ord token, Show token, Show clip) =>
                Maybe (a -> Path -> node) -> ListParser doc node clip token a
pStructuralEx mNodeConstr =  
          (\str -> let clip = recognizeClip str
                   in  case fromClip clip of 
                         Just enr -> enr
                         Nothing  -> error $ "Error"++show clip )
      <$> case mNodeConstr of
          Just nodeConstr -> pSym (StructuralTk 0 (Just $ nodeConstr (error "pStructural: Node argument evaluated") []) (EmptyP NoIDP) [] NoIDP)
          Nothing   -> pSym (StructuralTk 0 Nothing (EmptyP NoIDP) [] NoIDP)
          -- During parsing, only the constructor of node is taken into account. The error argument
          -- is never evaluated.

recognizeClip :: (Clip clip, Construct doc node clip token, DocNode node, Show token, Ord token) =>
             Token doc node clip token -> clip
recognizeClip strTk@(StructuralTk _ (Just node) _ childTokens _) = 
  if isListClip (construct node strTk []) 
  then 
  let thisPath = case pathNode node of
                   PathD path -> path
                   NoPathD    -> error $ "recognize: Encountered StructuralTk that has node without path:" ++ show strTk
      eltTokens = map (snd . tokenPath thisPath) childTokens -- we do the checks, but discard the numbers
      eltClips = map (Just. recognizeClip) eltTokens
  in  construct node strTk eltClips -- for lists construct does not call reuse
  else    
  let thisPath = case pathNode node of
                   PathD path -> path
                   NoPathD    -> error $ "recognize: Encountered StructuralTk that has node without path:" ++ show strTk
      numberedChildTokens = map (tokenPath thisPath) childTokens
      constructorArity = arityClip result -- no problem, since the constructor can be evaluated lazily
      initChildTokenGroups = replicate constructorArity []
      childTokenGroups = addChildTokens initChildTokenGroups numberedChildTokens 
      clipGroups       = map (map recognizeClip) childTokenGroups
      reuseArgs = [ case group of
                      (clip:_) -> Just clip -- cannot handle multiple occurrences yet. Now we just take
                                            -- the first, in the future, use dirty bits to take the updated one
                      []       -> Nothing
                  | group <- clipGroups
                  ]
      parsedChildren = map recognizeClip childTokens
      result = construct node strTk reuseArgs
  in  debug Prs ("\nThis path"++ show thisPath ++"\nChildren (max "++show constructorArity++"):\n"++show numberedChildTokens++"\n" ++ show childTokenGroups) $
      result
recognizeClip tk@(StructuralTk _ Nothing _ childTokens _) =
  error $ "recognize: Encountered StructuralTk without node: " ++ show tk
recognizeClip tk@(ParsingTk (Just parser) _ _ childTokens _) = 
  parser childTokens
recognizeClip tk@(ParsingTk Nothing _ _ childTokens _) = 
  error $ "recognize: Encountered ParsingTk without parser: " ++ show tk
recognizeClip tk =
  error $ "recognize: Encountered token other than StructuralTk or ParsingTk: " ++ show tk

tokenPath :: (Construct doc node clip token, DocNode node, Show token) => Path -> Token doc node clip token -> (Int, Token doc node clip token)
tokenPath parentPath tk =
  let node = case tk of 
               (StructuralTk _ (Just node) _ _ _) -> node
               (StructuralTk _ Nothing _ _ _)     -> error $ "tokenPath: childToken without node" ++ show tk
               (ParsingTk _ (Just node) _ _ _) -> node
               (ParsingTk _ Nothing _ _ _)     -> error $ "tokenPath: childToken without node" ++ show tk
  in case pathNode node of
       PathD path -> if parentPath `isPrefixOf` path 
                     then case drop (length parentPath) path of
                            [childNr] -> (childNr, tk)
                            _ -> error $ "encountered token that is not a child: tokenPath=" ++show path ++ 
                                         " parentPath=" ++ show parentPath ++ " token=" ++ show tk
                     else error $ "encountered token that is not a child: tokenPath=" ++show path ++ 
                                  " parentPath=" ++ show parentPath ++ " token=" ++ show tk
       NoPathD    -> error $ "tokenPath: childToken has node without path: " ++ show tk

addChildTokens childTokenGroups []  = childTokenGroups
addChildTokens childTokenGroups (childToken: childTokens) =
  addChildTokens (addChildToken childTokenGroups childToken) childTokens
 where addChildToken childTokenGroups (nr, childToken) =
         case splitAt nr childTokenGroups of
           (left, group:right) -> left ++ (group ++ [childToken]) : right
           _                   -> error $ "addChildToken: encountered child with number larger than parent's arity: nr="++show nr ++ " token="++show childToken 

     
-- if the argument is nothing, return nothing (for reuse), otherwise apply fromClip to the
-- clip
retrieveArg :: (Editable a doc node clip token, Show clip) => String -> Maybe clip -> Maybe a
retrieveArg parentCnstr (Just clip) =
  case fromClip clip of
    Just x  -> Just x
    Nothing -> debug Err ("\n\n\nCritical structural parse error: retrieveArg: Type error in "++show parentCnstr++
                          ". Encountered a " ++ show clip ) $ Nothing
retrieveArg parentCnstr Nothing     = Nothing
