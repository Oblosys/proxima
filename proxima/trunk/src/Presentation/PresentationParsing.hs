module Presentation.PresentationParsing where

import Common.CommonTypes
import Presentation.PresTypes
import Presentation.PresLayerTypes
import Evaluation.DocumentEdit
import Presentation.XprezLib

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


parsePres recognizeRootEnr (TokenP _ (StructuralTk _ _ tokens _)) = 
  let (enr,errs) = runParser recognizeRootEnr tokens
  in -- debug Err ("Parsing:\n"++concatMap (deepShowTks 0) (tokens)  
     --             ++"\nhas result:"++show enr ) $
     if null errs then Just enr else Nothing

parsePres _ _    = error "parsePres: scanned presentation has wrong format"

pMaybe parser = Just <$> parser `opt` Nothing

pStructural nd = pSym (StructuralTk (Just $ nd (error "This should not have happened") []) empty [] NoIDP)


applyDummyParameters nd = nd (error "This should not have happened") [] 
-- continues parsing on the children inside the structural token. the structural token is put in front
-- of the children, so reuse can be used on it just like in the normal parsers
pStr ::  (Editable a doc node clip token, DocNode node, Ord token, Show token) => ListParser doc node clip token a -> ListParser doc node clip token a
pStr = pStr' empty

pStrVerbose str = pStr' (text str)

pStr' prs p = unfoldStructure  
     <$> pSym (StructuralTk Nothing prs [] NoIDP)
 where unfoldStructure structTk@(StructuralTk nd pr children _) = 
         let (res, errs) = runParser p (structTk : children) {- (p <|> hole/parseErr parser)-}
             x = parseErr
         in  if null errs then res else debug Err ("ERROR: Parse error in structural parser:"++(show errs)) parseErr pr
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."


-- unfortunately, the first parser in p (which recognizes the structure token) cannot be used for the 
-- 'top-level' parsing. Hence, the parser succeeds on any structural token, and something like
-- pStr (pSym <DivExp> ...) <|> pStr (pSym <PowerExp> ...)  always takes the first alternative

pStrAlt ndf p = unfoldStructure  
     <$> pSym (StructuralTk (Just nd) (text $ show nd) [] NoIDP)
 where unfoldStructure structTk@(StructuralTk nd pr children _) = 
         let (res, errs) = runParser p (structTk : children) {- (p <|> hole/parseErr parser)-}
             x = parseErr
          in  if null errs then res else debug Err ("ERROR: Parse error in structural parser:"++(show errs)) parseErr pr
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
pStrExtra ::  (Editable a doc node clip token, DocNode node, Ord token, Show token) => b -> ListParser doc node clip token (a, b) -> ListParser doc node clip token (a, b)
pStrExtra extraDefault p = unfoldStructure  
     <$> pSym (StructuralTk Nothing empty [] NoIDP)
 where unfoldStructure structTk@(StructuralTk nd pr children _) = 
         let (res, errs) = runParser p (structTk : children) {- (p <|> hole/parseErr parser)-}
             x = parseErr
         in  if null errs then res else debug Err ("ERROR: Parse error in structural parser:"++(show errs)) (parseErr pr,extraDefault)
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."

-- TODO: why do we need the 's in Editable?
pPrs ::  (Editable a doc node clip token, DocNode node, Ord token, Show token) => ListParser doc node clip token a -> ListParser doc node clip token a
pPrs p = unfoldStructure  
     <$> pSym (ParsingTk empty [] NoIDP)
 where unfoldStructure presTk@(ParsingTk pr children _) = 
         let (res, errs) = runParser p children
         in  if null errs then res else debug Err ("ERROR: Parse error"++(show errs)) $ parseErr pr
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."

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






-- holes are cheap. actually only holes should be cheap, but presently structurals are all the same
pStruct :: (DocNode node, Ord token, Show token) => ListParser doc node clip token (Token doc node clip token)
pStruct = pCSym 4 (StructuralTk Nothing empty [] NoIDP)


-- pCostSym expects the parser twice
pCSym c p = pCostSym c p p




strucTk   = StructuralTk Nothing empty [] (IDP (-1))
parsingTk = (ParsingTk empty [] NoIDP)
graphTk   = GraphTk Dirty [] Nothing (IDP (-1)) -- probably a graph will never be inserted by
vertexTk  = VertexTk (-1) (0,0) Nothing  (IDP (-1))  -- the parser, but if it is, it should be dirty





