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


type ListParser doc node clip a = AnaParser [] Pair  (Token doc node clip (Maybe node)) a 

pMaybe parser = Just <$> parser `opt` Nothing

pStructural nd = pSym (StructuralTk (Just $ nd (error "This should not have happened") []) empty [] NoIDP)


-- TODO: handle parse error in parsing presentation correctly. (use parseErr?)


-- continues parsing on the children inside the structural token. the structural token is put in front
-- of the children, so reuse can be used on it just like in the normal parsers
pStr ::  (Ord node, Show node) => ListParser doc node clip a -> ListParser doc node clip a
pStr p = unfoldStructure  
     <$> pSym (StructuralTk Nothing empty [] NoIDP)
 where unfoldStructure structTk@(StructuralTk nd pr children _) = 
         let (res, errs) = runParser p (structTk : children) {- (p <|> hole/parseErr parser)-}
         in  if null errs then res else debug Err ("ERROR: Parse error in structural parser:"++(show errs)) res
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."

-- unfortunately, the first parser in p (which recognizes the structure token) cannot be used for the 
-- 'top-level' parsing. Hence, the parser succeeds on any structural token, and something like
-- pStr (pSym <DivExp> ...) <|> pStr (pSym <PowerExp> ...)  always takes the first alternative


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











-- put all tokens in one big list
-- UNCLEAR: what happens when list is presented again? Will it ever? Maybe we can avoid it, even with the new correcting parser
-- old TODO: fix silly recursion (what did I mean by that?)
-- TODO put keyword stuff in Scanner layer

postScanStr :: [String] -> Maybe node -> Presentation doc node clip -> [Token doc node clip (Maybe node)]
postScanStr kwrds ctxt (EmptyP _)           = []
postScanStr kwrds ctxt (StringP _ _)        = []
postScanStr kwrds ctxt (ImageP _ _)         = []
postScanStr kwrds ctxt (PolyP _ _ _)        = []
postScanStr kwrds ctxt (RectangleP _ _ _ _) = []
postScanStr kwrds ctxt (EllipseP _ _ _ _)   = []
postScanStr kwrds ctxt (WithP _ pres)       = postScanStr kwrds ctxt pres
postScanStr kwrds ctxt (OverlayP _ [])      = []
postScanStr kwrds ctxt (OverlayP _ (pres:press)) = postScanStr kwrds ctxt pres
postScanStr kwrds ctxt (ColP i _ [])         = []
postScanStr kwrds ctxt (ColP i _ (p:ps))     = postScanStr kwrds ctxt p ++ postScanStr kwrds ctxt (RowP i 0 ps)
postScanStr kwrds ctxt (RowP i _ [])         = []
postScanStr kwrds ctxt (RowP i _ (p:ps))     = postScanStr kwrds ctxt p ++ postScanStr kwrds ctxt (RowP i 0 ps)
postScanStr kwrds ctxt (LocatorP l p)        = postScanStr kwrds (Just l) p  
postScanStr kwrds ctxt (ParsingP i pres)     = [ParsingTk pres (postScanPrs kwrds ctxt pres) i]
--postScanStr kwrds ctxt (ParsingP i pres)   = [StructuralTk (Just NoNode) pres (postScanPrs kwrds ctxt pres ctxt) i]
postScanStr kwrds ctxt (StructuralP i pres)  = [StructuralTk ctxt pres (postScanStr kwrds ctxt pres) i]
postScanStr kwrds ctxt pres = debug Err ("*** PresentationParser.postScanStr: unimplemented presentation: " ++ show pres) []


postScanPrs :: [String] -> Maybe node -> Presentation doc node clip -> [Token doc node clip (Maybe node)]
postScanPrs kwrds ctxt (EmptyP _)           = []
postScanPrs kwrds ctxt (StringP _ "")       = []
postScanPrs kwrds ctxt (StringP i str)      = [mkToken kwrds str ctxt i]
postScanPrs kwrds ctxt (ImageP _ _)         = []
postScanPrs kwrds ctxt (PolyP _ _ _)        = []
postScanPrs kwrds ctxt (RectangleP _ _ _ _) = []
postScanPrs kwrds ctxt (EllipseP _ _ _ _)   = []
postScanPrs kwrds ctxt (WithP _ pres)       = postScanPrs kwrds ctxt pres
postScanPrs kwrds ctxt (OverlayP _ [])      = []
postScanPrs kwrds ctxt (OverlayP _ (pres:press)) = postScanPrs kwrds ctxt pres
postScanPrs kwrds ctxt (ColP i _ [])        = []
postScanPrs kwrds ctxt (ColP i _ (p:ps))    = postScanPrs kwrds ctxt p ++ postScanPrs kwrds ctxt (RowP i 0 ps)
postScanPrs kwrds ctxt (RowP i _ [])        = []
postScanPrs kwrds ctxt (RowP i _ (p:ps))    = postScanPrs kwrds ctxt p ++ postScanPrs kwrds ctxt (RowP i 0 ps)
postScanPrs kwrds ctxt (LocatorP l p)       = postScanPrs kwrds (Just l) p   
postScanPrs kwrds ctxt (ParsingP _ pres)    = postScanPrs kwrds ctxt pres
postScanPrs kwrds ctxt (StructuralP i pres) = [StructuralTk ctxt pres (postScanStr kwrds ctxt pres) i ]
postScanPrs kwrds ctxt pres  = debug Err ("*** PresentationParser.postScanPrs: unimplemented presentation: " ++ show pres) []





pKey :: (Ord node, Show node) => String -> ListParser doc node clip (Token doc node clip (Maybe node))
pKey str = pSym  (strTk str)

pKeyC :: (Ord node, Show node) => Int -> String -> ListParser doc node clip (Token doc node clip (Maybe node))
pKeyC c str = pCSym c (strTk str)

-- expensive, because we want holes to be inserted, not strings
pLIdent :: (Ord node, Show node) => ListParser doc node clip (Token doc node clip (Maybe node))
pLIdent = pCSym 20 lIdentTk

-- todo return int from pInt, so unsafe intVal does not need to be used anywhere else
pInt :: (Ord node, Show node) => ListParser doc node clip (Token doc node clip (Maybe node))
pInt = pCSym 20 intTk

-- holes are cheap. actually only holes should be cheap, but presently structurals are all the same
pStruct :: (Ord node, Show node) => ListParser doc node clip (Token doc node clip (Maybe node))
pStruct = pCSym 4 (StructuralTk Nothing empty [] NoIDP)


-- pCostSym expects the parser twice
pCSym c p = pCostSym c p p

lIdentVal :: Show node => Token doc node clip (Maybe node) -> String
lIdentVal (LIdentTk str _ _) = str
lIdentVal tk                 = debug Err ("PresentationParser.lIdentVal: no IdentTk " ++ show tk) "x"

strValTk :: Show node => Token doc node clip (Maybe node) -> String
strValTk (StrTk str _ _)    = str
strValTk (IntTk str _ _)    = str
strValTk (LIdentTk str _ _) = str
strValTk (UIdentTk str _ _) = str
strValTk (OpTk str _ _)     = str
strValTk (SymTk str _ _)    = str
strValTk tk                 = debug Err ("PresentationParser.strValTk: StructuralToken " ++ show tk) $ show tk
  
intVal :: Show node => Token doc node clip (Maybe node) -> Int
intVal (IntTk "" _ _)  = 0   -- may happen on parse error (although not likely since insert is expensive)
intVal (IntTk str _ _) = read str
intVal tk              = debug Err ("PresentationParser.intVal: no IntTk " ++ show tk) (-9999)



{-
All this is a big mess.

TODO: Find out what the effects of these Ord and Enum classes are and what the instances should be
-}



newtype ParsePres doc node clip a b c = ParsePres (Presentation doc node clip) deriving Show

-- parsing bits


{-

Because tokens are not part of the Presentation type yet, we preprocess the the 
StringP values and make a list of tokens. This is closely linked to the scanning
process and should be done in the layout layer.
-}

--data Token a = Tk Char a IDP | StructuralTk a (Presentation node) deriving Show

-- use a type field? instead of multiple constructors?

data Token doc node clip a = StrTk String a IDP  -- StrTk is for keywords, so eq takes the string value into account
             | IntTk String a IDP
             | LIdentTk String a IDP
             | UIdentTk String a IDP
             | OpTk String a IDP
             | SymTk String a IDP
             | StructuralTk a (Presentation doc node clip) [Token doc node clip a] IDP
             | ParsingTk (Presentation doc node clip) [Token doc node clip a] IDP -- deriving (Show)
-- ParsingTk token does not need a node (at least it didn't when it was encoded as a
-- (StructuralTk NoNode .. ) token)

instance Show a => Show (Token doc node clip (Maybe a)) where
  show (StrTk str _ _)    = show str
  show (IntTk str _ _)    = show str
  show (LIdentTk str _ _) = show str
  show (UIdentTk str _ _) = show str
  show (OpTk str _ _)     = show str
  show (SymTk str _ _)    = show str
  show (StructuralTk Nothing _ _ _) = "<structural:Nothing>" 
  show (StructuralTk (Just nd) _ _ _) = "<structural:"++show nd++">" 
  show (ParsingTk _ _ _) = "<presentation>" 

instance Eq a => Eq (Token doc node clip (Maybe a)) where
  StrTk str1 _ _ == StrTk str2 _ _ = str1 == str2
  IntTk _ _ _    == IntTk _ _ _    = True
  LIdentTk _ _ _ == LIdentTk _ _ _ = True
  UIdentTk _ _ _ == UIdentTk _ _ _ = True
  OpTk _ _ _     == OpTk _ _ _     = True
  SymTk _ _ _    == SymTk _ _ _    = True
--  StructuralTk _ _ _    == StructuralTk _ _ _ = True       -- StructuralTks with no node always match
  StructuralTk Nothing _ _ _    == StructuralTk _ _ _ _ = True       -- StructuralTks with no node always match
  StructuralTk _ _ _ _          == StructuralTk Nothing _ _ _ = True -- StructuralTks with no node always match
  StructuralTk (Just nd1) _ _ _ == StructuralTk (Just nd2) _ _ _ = nd1 == nd2
  ParsingTk _ _ _    == ParsingTk _ _ _ = True   
  _              == _              = False

instance Ord a => Ord (Token doc node clip (Maybe a)) where
  compare x y | x==y      = EQ   --
	          | x<=y      = LT   -- From Doaitse's scanner. Find out why is this necessary?
	          | otherwise = GT   --
  StrTk str1 _ _ <= StrTk str2 _ _ = str1 <= str2

  IntTk _ _ _    <= IntTk _ _ _    = True
  IntTk _ _ _    <= StrTk _ _ _    = True

  LIdentTk _ _ _ <= LIdentTk _ _ _ = True
  LIdentTk _ _ _ <= IntTk _ _ _    = True
  LIdentTk _ _ _ <= StrTk _ _ _    = True

  UIdentTk _ _ _ <= UIdentTk _ _ _ = True
  UIdentTk _ _ _ <= LIdentTk _ _ _ = True
  UIdentTk _ _ _ <= IntTk _ _ _    = True
  UIdentTk _ _ _ <= StrTk _ _ _    = True

  OpTk _ _ _     <= OpTk _ _ _      = True
  OpTk _ _ _     <= UIdentTk _ _ _  = True
  OpTk _ _ _     <= LIdentTk _ _ _  = True
  OpTk _ _ _     <= IntTk _ _ _     = True
  OpTk _ _ _     <= StrTk _ _ _     = True
 
  SymTk _ _ _    <= SymTk _ _ _ = True
  SymTk _ _ _    <= OpTk _ _ _      = True
  SymTk _ _ _    <= UIdentTk _ _ _      = True
  SymTk _ _ _    <= LIdentTk _ _ _      = True
  SymTk _ _ _    <= IntTk _ _ _      = True
  SymTk _ _ _    <= StrTk _ _ _      = True

--  StructuralTk _ _ _ <= StructuralTk _ _ _     = True
  StructuralTk Nothing _ _ _    <= StructuralTk _ _ _ _ = True       -- ??
  StructuralTk _ _ _ _          <= StructuralTk Nothing _ _ _ = True -- ??
  StructuralTk (Just nd1) _ _ _ <= StructuralTk (Just nd2) _ _ _ = nd1 <= nd2
  StructuralTk _ _ _ _ <= SymTk _ _ _    = True
  StructuralTk _ _ _ _ <= OpTk _ _ _     = True
  StructuralTk _ _ _ _ <= UIdentTk _ _ _ = True
  StructuralTk _ _ _ _ <= LIdentTk _ _ _ = True
  StructuralTk _ _ _ _ <= IntTk _ _ _    = True
  StructuralTk _ _ _ _ <= StrTk _ _ _    = True

  ParsingTk _ _ _  <= ParsingTk _ _ _ = True
  ParsingTk _ _ _  <= StructuralTk _ _ _ _ = True
  ParsingTk _ _ _ <= SymTk _ _ _    = True
  ParsingTk _ _ _ <= OpTk _ _ _     = True
  ParsingTk _ _ _ <= UIdentTk _ _ _ = True
  ParsingTk _ _ _ <= LIdentTk _ _ _ = True
  ParsingTk _ _ _ <= IntTk _ _ _    = True
  ParsingTk _ _ _ <= StrTk _ _ _    = True

  _              <= _           = False


   
{- from Doaitse's Scanner
newtype Token = Tok (TokenType, String, String, Linenumber, Filename, String, [Token])

instance Eq Token where
  Tok (ttypel    , stringl, _, _, _, _, _ ) == Tok (ttyper    , stringr, _, _ , _, _, _) =  ttypel == ttyper && stringl == stringr

instance   Ord Token where
  compare x y | x==y      = EQ
	      | x<=y      = LT
	      | otherwise = GT
  Tok (ttypel    , stringl, _, _, _, _, _ ) <= Tok (ttyper   , stringr, _, _ , _, _, _ )
      =     ttypel <  ttyper
        || (ttypel == ttyper && stringl <= stringr)

-}
tokenString :: Token doc node clip (Maybe node) -> String                  
tokenString (StrTk s n id)      = s
tokenString (IntTk s n id)      = s
tokenString (LIdentTk s n id)   = s
tokenString (UIdentTk s n id)   = s
tokenString (OpTk s n id)       = s
tokenString (SymTk s n id)      = s
tokenString (StructuralTk n _ _ id) = "<structural token>"
                             
tokenNode :: Token doc node clip (Maybe node) -> Maybe node                 
tokenNode (StrTk s n id)      = n
tokenNode (IntTk s n id)      = n
tokenNode (LIdentTk s n id)   = n
tokenNode (UIdentTk s n id)   = n
tokenNode (OpTk s n id)       = n
tokenNode (SymTk s n id)      = n
tokenNode (StructuralTk n _ _ id) = n

tokenIDP :: Token doc node clip (Maybe node) -> IDP       
tokenIDP (StrTk s n id)    = id
tokenIDP (IntTk s n id)    = id
tokenIDP (LIdentTk s n id) = id
tokenIDP (UIdentTk s n id) = id
tokenIDP (OpTk s n id)     = id
tokenIDP (SymTk s n id)    = id
tokenIDP (StructuralTk n _ _ id)  = id


-- probably have to split strTk in a symbol, an operator and a keyword variant.
-- TODO call strTk KeyTk


-- (IDP (-1)) means inserted token. This should be handled by some kind of 'fresh' attribute
-- which is also required for copying of presentation subtrees
strTk str = StrTk str Nothing (IDP (-1))
intTk     = IntTk "0" Nothing (IDP (-1))
lIdentTk  = LIdentTk "ident" Nothing (IDP (-1))
uIdentTk  = UIdentTk "Ident" Nothing (IDP (-1))
opTk      = OpTk "" Nothing (IDP (-1))
symTk     = SymTk "" Nothing (IDP (-1))
strucTk   = StructuralTk Nothing empty [] (IDP (-1))
parsingTk = (ParsingTk empty [] NoIDP)
--parsingTk = StructuralTk (Just NoNode) empty [] NoIDP


mkToken :: [String] -> String -> Maybe node -> IDP -> Token doc node clip (Maybe node)
mkToken keywords str@(c:_)   ctxt i | str `elem` keywords = StrTk str ctxt i
                                    | isDigit c           = IntTk str ctxt i
                                    | isLower c           = LIdentTk str ctxt i
                                    | isUpper c           = UIdentTk str ctxt i
                                    | otherwise           = OpTk str ctxt i

--makeToken str ctxt i = Tk str ctxt i

isSymbolChar c = c `elem` ";,(){}#_|"



instance (Show a, Eq a, Ord a) => Symbol (Token doc node clip (Maybe a)) where

runParser (pp) inp =
      let res = UU_Parsing.parse pp inp
          (Pair v final) = evalSteps (res) 
          errs = getMsgs (res) 
      in  (v, errs)










{-



--instance Enum (Token doc node clip (Maybe a)) where            -- is this right?
--  toEnum   i = Tk (chr i) Nothing NoIDP  
--  fromEnum (Tk c _ _) = ord c
--  fromEnum _          = 0

instance (Show a) => Symbol (Token doc node clip (Maybe a)) where
--  symBefore = pred
--  symAfter = succ



instance InputState (ParsePres Document Node String) (Token doc node clip (Maybe Node)) where
 splitStateE tree   = case walk tree of
                         Nothing         -> Right' tree
                         Just (tk,tree') -> Left' tk tree'
 splitState  tree   = case walk tree of
                        --Nothing -> Nothing
                        Just (tk,tree') -> (tk, tree')
 firstState  tree   = case walk tree of
                        Nothing          -> Nothing
                        Just (tk,tree') -> Just tk
 getPosition tree   = case walk tree of
                        Nothing        -> "unexpected end of input"
                        Just (tk, tree') ->   "("++ show (tokenIDP tk, tokenString tk) 
                                            ++","++ case walk tree' of
                                                      Nothing      -> "(NoIDP,\"\")"
                                                      Just (tk2,_) -> show (tokenIDP tk2, tokenString tk2)
                                            ++")"


runParser (pp) inp =
      let res = UU_Parsing.parse pp inp
          (Pair v final) = evalSteps (res) 
          errs = getMsgs (res) 
      in  (v, errs)

{-
runParser (pp) inp =
       let (Pair v final) = evalSteps (parse pp inp) 
           err = evalStepsE (parse pp inp) 
       in  (v,err)

-}
-- can be tupled with result in evalSteps
evalStepsE :: Symbol b => Steps a b -> [String]
evalStepsE (OkVal v  rest    ) =    evalStepsE rest
evalStepsE (Ok       rest    ) =    evalStepsE rest
evalStepsE (Cost  _  rest    ) =    evalStepsE rest
evalStepsE (StRepair _ msg@(Msg (s1, s2, xp)) rest    ) = debug Prs ("Parse error: "++show msg) $ (show msg++"\n"++s1++"\n"++s2++"\n"++show xp): evalStepsE rest 
evalStepsE (Best _   rest _ _) =  evalStepsE rest
evalStepsE (NoMoreSteps v    ) =  []

{-
prr = LocatorP (NoNode) 
       (LocatorP (NoNode) 
         (WithP id
           (RowP NoIDP 0 [LocatorP NoNode (WithP id (StringP (IDP 200) "100"))
                        ,StringP (IDP 100) "+"
                        ,LocatorP NoNode (WithP id (StringP (IDP 300) "200"))])))

-}



{-

With nodes are hard to parse, so presentation parsing will probably not be able to use font attributes etc.

-}


ptest p inp = unsafePerformIO $
 do { result <- UU_Parsing.parseIO p (ParsePres inp)
    ; debugLnIO Par  $ "Result: "++show result
    }




-- experimental

pMarkParseErr :: Symbol s => a -> AnaParser state Pair s a -> AnaParser state Pair s a
pMarkParseErr prsErr = pMap f f'
 where f' = undefined             
       f p s resultSteps = let wr parser (b,r) = parser (if errsSinceCheck resultSteps then prsErr else b) r
                               resultSteps' = StRepair 0 (Msg ("","",EStr "check"))$  val (wr p)  resultSteps 
                           in  (s, resultSteps')
       




errsSinceCheck :: Symbol b => Steps a b -> Bool
errsSinceCheck (OkVal v  rest    ) = errsSinceCheck rest
errsSinceCheck (Ok       rest    ) = errsSinceCheck rest
errsSinceCheck (Cost  _  rest    ) = errsSinceCheck rest
errsSinceCheck (StRepair _ (Msg ("","",EStr "check")) rest    ) = False
errsSinceCheck (StRepair _ msg rest    ) = True
errsSinceCheck (Best _   rest _ _) = errsSinceCheck rest
errsSinceCheck (NoMoreSteps v    ) = False





{-
-- error recovery is bit weird here: etest parse1Exp' "1*2+2x43*4"
-- gives                                               1*2+243*4+0

-- always left factor, and preferably use chain

-}


-}