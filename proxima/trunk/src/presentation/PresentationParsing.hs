module PresentationParsing where

import CommonTypes
import PresTypes

import DocumentEdit
import XprezLib

import UU_Parsing hiding (Exp, parse, parseIO)
import qualified UU_Parsing
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


type ListParser doc node clip a = AnaParser [] Pair  (Token doc node clip) a 

pMaybe parser = Just <$> parser `opt` Nothing

pStructural nd = pSym (StructuralTk (Just $ nd (error "This should not have happened") []) empty [] NoIDP)


applyDummyParameters nd = nd (error "This should not have happened") [] 
-- continues parsing on the children inside the structural token. the structural token is put in front
-- of the children, so reuse can be used on it just like in the normal parsers
pStr ::  (Editable a doc node clip, DocNode node, Ord node, Show node) => ListParser doc node clip a -> ListParser doc node clip a
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


pStrDirty ::  (Editable a doc node clip, DocNode node, Ord node, Show node) => ListParser doc node clip (a, Dirty) -> ListParser doc node clip (a, Dirty)
pStrDirty p = pStrExtra Dirty p


-- pStrExtra is a variant of pStr that allows an extra parser result to be returned in a tuple.
-- extraDefault is a default value for this type in case of a parse error.
pStrExtra ::  (Editable a doc node clip, DocNode node, Ord node, Show node) => b -> ListParser doc node clip (a, b) -> ListParser doc node clip (a, b)
pStrExtra extraDefault p = unfoldStructure  
     <$> pSym (StructuralTk Nothing empty [] NoIDP)
 where unfoldStructure structTk@(StructuralTk nd pr children _) = 
         let (res, errs) = runParser p (structTk : children) {- (p <|> hole/parseErr parser)-}
             x = parseErr
         in  if null errs then res else debug Err ("ERROR: Parse error in structural parser:"++(show errs)) (parseErr pr,extraDefault)
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."

-- TODO: why do we need the 's in Editable?
pPrs ::  (Editable a doc node clip, Ord node, Show node) => ListParser doc node clip a -> ListParser doc node clip a
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



-- put all tokens in one big list
-- UNCLEAR: what happens when list is presented again? Will it ever? Maybe we can avoid it, even with the new correcting parser
-- TODO put keyword stuff in Scanner layer
--      check what happens with tokens without context info. It seems they get it from higher up
--      in the tree now, which seems wrong. 

postScanStr :: [String] -> Maybe node -> Presentation doc node clip -> [Token doc node clip]
postScanStr kwrds ctxt (EmptyP _)           = []
postScanStr kwrds ctxt (StringP _ _)        = []
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


postScanPrs :: [String] -> Maybe node -> Presentation doc node clip -> [Token doc node clip]
postScanPrs kwrds ctxt (EmptyP _)           = []
postScanPrs kwrds ctxt (StringP _ "")       = []
postScanPrs kwrds ctxt (StringP i str)      = [mkToken kwrds str ctxt i]
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




newtype ParsePres doc node clip a b c = ParsePres (Presentation doc node clip) deriving Show

-- parsing bits



data Token doc node clip = 
               UserTk UserToken String (Maybe node) IDP
             | StructuralTk (Maybe node) (Presentation doc node clip) [Token doc node clip] IDP
             | ParsingTk (Presentation doc node clip) [Token doc node clip] IDP -- deriving (Show)
             | GraphTk Dirty [(Int, Int)] (Maybe node) IDP
             | VertexTk Int (Int, Int) (Maybe node) IDP
-- ParsingTk token does not need a node (at least it didn't when it was encoded as a
-- (StructuralTk NoNode .. ) token)

instance Show node => Show (Token doc node clip) where
  show (UserTk u s _ _) = "<user:" ++show u ++ show s ++ ">"
  show (StructuralTk Nothing p _ _) = "<structural:Nothing:"++show p++">" 
  show (StructuralTk (Just nd) _ _ _) = "<structural:"++show nd++">" 
  show (ParsingTk _ _ _) = "<presentation>" 
  show (GraphTk _ edges _ _)  = "<graph:"++show edges++">"
  show (VertexTk id pos _ _)  = "<vertex "++show id++":"++show pos++">"
  
instance Eq node => Eq (Token doc node clip) where
  UserTk u1 _ _ _     == UserTk u2 _ _ _     = u1 == u2
  StructuralTk Nothing _ _ _    == StructuralTk _ _ _ _ = True       -- StructuralTks with no node always match
  StructuralTk _ _ _ _          == StructuralTk Nothing _ _ _ = True -- StructuralTks with no node always match
  StructuralTk (Just nd1) _ _ _ == StructuralTk (Just nd2) _ _ _ = nd1 == nd2
  ParsingTk _ _ _    == ParsingTk _ _ _ = True   
  GraphTk _ _ _ _  == GraphTk _ _ _ _  = True
  VertexTk _ _ _ _ == VertexTk _ _ _ _ = True -- if we want to recognize specific vertices, maybe some
  _              == _                  = False -- identifier will be added, which will be involved in eq. check

instance Ord node => Ord (Token doc node clip) where
  UserTk u1 _ _ _      <= UserTk u2 _ _ _    = u1 <= u2
  StructuralTk Nothing _ _ _    <= StructuralTk _ _ _ _ = True     
  StructuralTk _ _ _ _          <= StructuralTk Nothing _ _ _ = True
  StructuralTk (Just nd1) _ _ _ <= StructuralTk (Just nd2) _ _ _ = nd1 <= nd2
  StructuralTk _ _ _ _ <= UserTk _ _ _ _  = True
  
  ParsingTk _ _ _ <= ParsingTk _ _ _      = True
  ParsingTk _ _ _ <= StructuralTk _ _ _ _ = True
  ParsingTk _ _ _ <= UserTk _ _ _ _       = True

  GraphTk _ _ _ _ <= GraphTk _ _ _ _      = True
  GraphTk _ _ _ _ <= ParsingTk _ _ _      = True
  GraphTk _ _ _ _ <= StructuralTk _ _ _ _ = True
  GraphTk _ _ _ _ <= UserTk _ _ _ _       = True

  VertexTk _ _  _ _ <= VertexTk _ _ _ _    = True
  VertexTk _ _ _ _ <= GraphTk _ _ _ _      = True
  VertexTk _ _ _ _ <= ParsingTk _ _ _      = True
  VertexTk _ _ _ _ <= StructuralTk _ _ _ _ = True
  VertexTk _ _ _ _ <= UserTk _ _ _ _       = True

  _              <= _           = False


tokenString :: Token doc node clip -> String                  
tokenString (UserTk _ s n id)      = s
tokenString (StructuralTk n _ _ id) = "<structural token>"
tokenString (GraphTk d es n id) = "<graph token>"
tokenString (VertexTk i p n id) = "<vertex token>"
                             
tokenNode :: Token doc node clip -> Maybe node                 
tokenNode (StructuralTk n _ _ id) = n
tokenNode (GraphTk d es n id) = n
tokenNode (VertexTk i p n id) = n
tokenNode (UserTk u s n id)   = n

tokenIDP :: Token doc node clip -> IDP       
tokenIDP (UserTk u s n id) = id
tokenIDP (StructuralTk n _ _ id)  = id
tokenIDP (GraphTk d es n id) = id
tokenIDP (VertexTk i p n id) = id




instance (Ord node, Show node) => Symbol (Token doc node clip) where

runParser (pp) inp =
      let res = UU_Parsing.parse pp inp
          (Pair v final) = evalSteps (res) 
          errs = getMsgs (res) 
      in  (v, errs)






-- holes are cheap. actually only holes should be cheap, but presently structurals are all the same
pStruct :: (Ord node, Show node) => ListParser doc node clip (Token doc node clip)
pStruct = pCSym 4 (StructuralTk Nothing empty [] NoIDP)


-- pCostSym expects the parser twice
pCSym c p = pCostSym c p p




strucTk   = StructuralTk Nothing empty [] (IDP (-1))
parsingTk = (ParsingTk empty [] NoIDP)
graphTk   = GraphTk Dirty [] Nothing (IDP (-1)) -- probably a graph will never be inserted by
vertexTk  = VertexTk (-1) (0,0) Nothing  (IDP (-1))  -- the parser, but if it is, it should be dirty


------------------ User defined token part:

--data Token a = Tk Char a IDP | StructuralTk a (Presentation node) deriving Show

-- use a type field? instead of multiple constructors?

data UserToken = StrTk String  -- StrTk is for keywords, so eq takes the string value into account
               | IntTk
               | LIdentTk
               | UIdentTk
               | OpTk
               | SymTk deriving (Show, Eq, Ord)


-- probably have to split strTk in a symbol, an operator and a keyword variant.
-- TODO call strTk KeyTk


-- (IDP (-1)) means inserted token. This should be handled by some kind of 'fresh' attribute
-- which is also required for copying of presentation subtrees
strTk str = UserTk (StrTk str) str Nothing (IDP (-1))
intTk     = UserTk IntTk "0" Nothing (IDP (-1))
lIdentTk  = UserTk LIdentTk "ident" Nothing (IDP (-1))
uIdentTk  = UserTk UIdentTk "Ident" Nothing (IDP (-1))
opTk      = UserTk OpTk "" Nothing (IDP (-1))
symTk     = UserTk SymTk "" Nothing (IDP (-1))


mkToken :: [String] -> String -> Maybe node -> IDP -> Token doc node clip
mkToken keywords str@(c:_)   ctxt i | str `elem` keywords = UserTk (StrTk str) str ctxt i
                                    | isDigit c           = UserTk IntTk str ctxt i
                                    | isLower c           = UserTk LIdentTk str ctxt i
                                    | isUpper c           = UserTk UIdentTk str ctxt i
                                    | otherwise           = UserTk OpTk str ctxt i

--makeToken str ctxt i = Tk str ctxt i

isSymbolChar c = c `elem` ";,(){}#_|"


-- Basic parsers

pKey :: (Ord node, Show node) => String -> ListParser doc node clip (Token doc node clip)
pKey str = pSym  (strTk str)

pKeyC :: (Ord node, Show node) => Int -> String -> ListParser doc node clip (Token doc node clip)
pKeyC c str = pCSym c (strTk str)

-- expensive, because we want holes to be inserted, not strings
pLIdent :: (Ord node, Show node) => ListParser doc node clip (Token doc node clip)
pLIdent = pCSym 20 lIdentTk

-- todo return int from pInt, so unsafe intVal does not need to be used anywhere else
pInt :: (Ord node, Show node) => ListParser doc node clip (Token doc node clip)
pInt = pCSym 20 intTk

lIdentVal :: Show node => Token doc node clip -> String
lIdentVal (UserTk LIdentTk str _ _) = str
lIdentVal tk                 = debug Err ("PresentationParser.lIdentVal: no IdentTk " ++ show tk) "x"

  
intVal :: Show node => Token doc node clip -> Int
intVal (UserTk IntTk "" _ _)  = 0   -- may happen on parse error (although not likely since insert is expensive)
intVal (UserTk IntTk str _ _) = read str
intVal tk              = debug Err ("PresentationParser.intVal: no IntTk " ++ show tk) (-9999)

