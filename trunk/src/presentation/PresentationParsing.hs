module PresentationParsing where

import CommonTypes
import DocTypes
import PresTypes

import DocumentEdit
import XprezLib

import UU_Parsing hiding (Exp, parse, parseIO)
import qualified UU_Parsing
import Char

import IOExts

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


type ListParser a = AnaParser [] Pair  (Token (Maybe Node)) a 

pMaybe parser = Just <$> parser `opt` Nothing

pStructural nd = pSym (Structural (Just $ nd hole []) empty [] NoIDP)




-- continues parsing on the children inside the structural token. the structural token is put in front
-- of the children, so reuse can be used on it just like in the normal parsers
pStr' :: ListParser a -> ListParser a
pStr' p = unfoldStructure  
     <$> pSym (Structural Nothing empty [] NoIDP)
 where unfoldStructure structTk@(Structural nd _ children _) = 
         let (res, errs) = runParser p (structTk : children) {- (p <|> hole/parseErr parser)-}
         in  if null errs then res else debug Err (show errs) res
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."

pStr :: Editable a => ListParser a -> ListParser a
pStr p = unfoldStructure  
     <$> pSym (Structural Nothing empty [] NoIDP)
 where unfoldStructure structTk@(Structural nd pr children _) = 
         let (res, errs) = runParser p (structTk : children) {- (p <|> hole/parseErr parser)-}
         in  if null errs then res else debug Err (show errs) $ parseErr NoNode pr
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."

-- unfortunately, the first parser in p (which recognizes the structure token) cannot be used for the 
-- 'top-level' parsing. Hence, the parser succeeds on any structural token, and something like
-- pStr (pSym <DivExp> ...) <|> pStr (pSym <PowerExp> ...)  always takes the first alternative


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

-- hole parser
{-
       p
   <|>  (\_ -> DeclHole)
        pSym (Structural (Just $ DeclHoleNode hole []) [] NoIDP)
 if we put holeNode and in Editable (maybe better in separate class Parseable)
 then
       (\_ -> hole) -- or reuse
   <$> pSym (Structural (Just holeNode) [] NoIDP)


maybe just one HoleNode?

       (\_ -> hole) -- or reuse

parseErrs are not in the presentation, so we won't need ParseErrNodes

so Div (Parse Err (IntExp 1) "1_") (IntExp 2) is presented as  (Structural "1_" "2")
and the node for the first child is (IntExp 1) There is never a ParseErrNode
-}











-- put all tokens in one big list
-- UNCLEAR: what happens when list is presented again? Will it ever? Maybe we can avoid it, even with the new correcting parser
-- TODO: switch pres & ctxt args, fix silly recursion


postScanStr :: Presentation -> Maybe Node -> [Token (Maybe Node)]
postScanStr (EmptyP _)    ctxt = []
postScanStr (StringP _ _) ctxt = []
postScanStr (ImageP _ _)  ctxt = []
postScanStr (PolyP _ _ _) ctxt = []
postScanStr (RectangleP _ _ _ _) ctxt = []
postScanStr (WithP _ pres)    ctxt = postScanStr pres ctxt
postScanStr (OverlayP _ []) ctxt = []
postScanStr (OverlayP _ (pres:press)) ctxt = postScanStr pres ctxt
postScanStr (ColP i _ [])      _    = []
postScanStr (ColP i _ (p:ps))  ctxt = postScanStr p ctxt ++ postScanStr (RowP i 0 ps) ctxt
postScanStr (RowP i _ [])      _    = []
postScanStr (RowP i _ (p:ps))  ctxt = postScanStr p ctxt ++ postScanStr (RowP i 0 ps) ctxt
postScanStr (LocatorP l p)     ctxt = postScanStr p (Just l)  
postScanStr (ParsingP _ pres) ctxt    = [Structural (Just NoNode) pres (postScanPrs pres ctxt) NoIDP]   -- HACK! Need a Parsing Token (only for parsing in Structural)
postScanStr (StructuralP i pres) ctxt = [Structural ctxt pres (postScanStr pres ctxt) i]
postScanStr pres _ = debug Err ("*** PresentationParser.postScanStr: unimplemented presentation: " ++ show pres) []

-- Structural (Just NoNode) is used as a hack for representing a ParsingToken

postScanPrs :: Presentation -> Maybe Node -> [Token (Maybe Node)]
postScanPrs (EmptyP _)    ctxt = []
postScanPrs (StringP _ "") ctxt = []
postScanPrs (StringP i str) ctxt = [mkToken str ctxt i]
postScanPrs (ImageP _ _)  ctxt = []
postScanPrs (PolyP _ _ _) ctxt = []
postScanPrs (RectangleP _ _ _ _) ctxt = []
postScanPrs (WithP _ pres)    ctxt = postScanPrs pres ctxt
postScanPrs (OverlayP _ []) ctxt = []
postScanPrs (OverlayP _ (pres:press)) ctxt = postScanPrs pres ctxt
postScanPrs (ColP i _ [])      _    = []
postScanPrs (ColP i _ (p:ps))  ctxt = postScanPrs p ctxt ++ postScanPrs (RowP i 0 ps) ctxt
postScanPrs (RowP i _ [])      _    = []
postScanPrs (RowP i _ (p:ps))  ctxt = postScanPrs p ctxt ++ postScanPrs (RowP i 0 ps) ctxt
postScanPrs (LocatorP l p)     ctxt = postScanPrs p (Just l)  
postScanPrs (ParsingP _ pres) ctxt    = postScanPrs pres ctxt
postScanPrs (StructuralP id pres) ctxt = [Structural ctxt pres (postScanStr pres ctxt) id ]
postScanPrs pres _ = debug Err ("*** PresentationParser.postScanPrs: unimplemented presentation: " ++ show pres) []




{- Small program that causes GHC panic

pInt :: TreeParser (Token (Maybe Node))
pInt = pCSym 20 intTk
intTk     = IntTk "0" Nothing (IDP (-1))
pCSym c p = pCostSym c p p

data Token a =  IntTk String a IDP deriving (Show,Ord,Eq)

instance Ord (Node)
instance Eq (Node)
type TreeParser a = AnaParser (ParsePres Node String) Pair  (Token (Maybe Node)) a 
newtype ParsePres a b c = ParsePres Presentation deriving Show
instance InputState (ParsePres Node String) (Token (Maybe Node)) 
instance (Show a,Ord a) => Symbol (Token (Maybe a)) where

-}


pKey :: String -> ListParser (Token (Maybe Node))
pKey str = pSym  (strTk str)

pKeyC :: Int -> String -> ListParser (Token (Maybe Node))
pKeyC c str = pCSym c (strTk str)

-- expensive, because we want holes to be inserted, not strings
pLIdent :: ListParser (Token (Maybe Node))
pLIdent = pCSym 20 (LIdentTk "ident" Nothing (IDP (-1)))
--pLIdent = pCSym 20 lIdentTk

-- todo return int from pInt, so unsafe intVal does not need to be used anywhere else
pInt :: ListParser (Token (Maybe Node))
pInt = pCSym 20 (IntTk "0" Nothing (IDP (-1)))
--pInt = pCSym 20 intTk


-- *********** BUG IN GHC 6.0
-- intTk and lIntTk calls are replaced by their bodies, otherwise GHC panics

-- holes are cheap. actually only holes should be cheap, but presently structurals are all the same
pStruct :: ListParser (Token (Maybe Node))
pStruct = pCSym 4 (Structural Nothing empty [] NoIDP)


-- pCostSym expects the parser twice
pCSym c p = pCostSym c p p

lIdentVal :: Token (Maybe Node) -> String
lIdentVal (LIdentTk str _ _) = str
lIdentVal tk                 = debug Err ("PresentationParser.lIdentVal: no IdentTk " ++ show tk) "x"

strValTk :: Token (Maybe Node) -> String
strValTk (StrTk str _ _)    = str
strValTk (IntTk str _ _)    = str
strValTk (LIdentTk str _ _) = str
strValTk (UIdentTk str _ _) = str
strValTk (OpTk str _ _)     = str
strValTk (SymTk str _ _)    = str
strValTk tk                 = debug Err ("PresentationParser.strValTk: StructuralToken " ++ show tk) $ show tk
  
intVal :: Token (Maybe Node) -> Int
intVal (IntTk "" _ _)  = 0   -- may happen on parse error (although not likely since insert is expensive)
intVal (IntTk str _ _) = read str
intVal tk              = debug Err ("PresentationParser.intVal: no IntTk " ++ show tk) (-9999)



{-
All this is a big mess.

TODO: Find out what the effects of these Ord and Enum classes are and what the instances should be
-}



newtype ParsePres a b c = ParsePres Presentation deriving Show

-- parsing bits


{-

Because tokens are not part of the Presentation type yet, we preprocess the the 
StringP values and make a list of tokens. This is closely linked to the scanning
process and should be done in the layout layer.
-}

--data Token a = Tk Char a IDP | Structural a Presentation deriving Show

-- use a type field? instead of multiple constructors?

data Token a = StrTk String a IDP  -- StrTk is for keywords, so eq takes the string value into account
             | IntTk String a IDP
             | LIdentTk String a IDP
             | UIdentTk String a IDP
             | OpTk String a IDP
             | SymTk String a IDP
             | Structural a Presentation [Token a] IDP -- deriving (Show)

instance Show a => Show (Token (Maybe a)) where
  show (StrTk str _ _)    = show str
  show (IntTk str _ _)    = show str
  show (LIdentTk str _ _) = show str
  show (UIdentTk str _ _) = show str
  show (OpTk str _ _)     = show str
  show (SymTk str _ _)    = show str
  show (Structural Nothing _ _ _) = "<structural:Nothing>" 
  show (Structural (Just nd) _ _ _) = "<structural:"++show nd++">" 

instance Eq a => Eq (Token (Maybe a)) where
  StrTk str1 _ _ == StrTk str2 _ _ = str1 == str2
  IntTk _ _ _    == IntTk _ _ _    = True
  LIdentTk _ _ _ == LIdentTk _ _ _ = True
  UIdentTk _ _ _ == UIdentTk _ _ _ = True
  OpTk _ _ _     == OpTk _ _ _     = True
  SymTk _ _ _    == SymTk _ _ _    = True
--  Structural _ _ _    == Structural _ _ _ = True       -- Structurals with no node always match
  Structural Nothing _ _ _    == Structural _ _ _ _ = True       -- Structurals with no node always match
  Structural _ _ _ _          == Structural Nothing _ _ _ = True -- Structurals with no node always match
  Structural (Just nd1) _ _ _ == Structural (Just nd2) _ _ _ = nd1 == nd2
  _              == _              = False

instance Ord a => Ord (Token (Maybe a)) where
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

--  Structural _ _ _ <= Structural _ _ _     = True
  Structural Nothing _ _ _    <= Structural _ _ _ _ = True       -- ??
  Structural _ _ _ _          <= Structural Nothing _ _ _ = True -- ??
  Structural (Just nd1) _ _ _ <= Structural (Just nd2) _ _ _ = nd1 <= nd2
  Structural _ _ _ _ <= SymTk _ _ _    = True
  Structural _ _ _ _ <= OpTk _ _ _     = True
  Structural _ _ _ _ <= UIdentTk _ _ _ = True
  Structural _ _ _ _ <= LIdentTk _ _ _ = True
  Structural _ _ _ _ <= IntTk _ _ _    = True
  Structural _ _ _ _ <= StrTk _ _ _    = True

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
tokenString :: Token (Maybe Node) -> String                  
tokenString (StrTk s n id)      = s
tokenString (IntTk s n id)      = s
tokenString (LIdentTk s n id)   = s
tokenString (UIdentTk s n id)   = s
tokenString (OpTk s n id)       = s
tokenString (SymTk s n id)      = s
tokenString (Structural n _ _ id) = "<structural token>"
                             
tokenNode :: Token (Maybe Node) -> Maybe Node                 
tokenNode (StrTk s n id)      = n
tokenNode (IntTk s n id)      = n
tokenNode (LIdentTk s n id)   = n
tokenNode (UIdentTk s n id)   = n
tokenNode (OpTk s n id)       = n
tokenNode (SymTk s n id)      = n
tokenNode (Structural n _ _ id) = n

tokenIDP :: Token (Maybe Node) -> IDP       
tokenIDP (StrTk s n id)    = id
tokenIDP (IntTk s n id)    = id
tokenIDP (LIdentTk s n id) = id
tokenIDP (UIdentTk s n id) = id
tokenIDP (OpTk s n id)     = id
tokenIDP (SymTk s n id)    = id
tokenIDP (Structural n _ _ id)  = id


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
strucTk   = Structural Nothing empty [] (IDP (-1))



mkToken :: String -> Maybe Node -> IDP -> Token (Maybe Node)
mkToken str@(c:_)   ctxt i | str `elem` keywords = StrTk str ctxt i
                           | isDigit c           = IntTk str ctxt i
                           | isLower c           = LIdentTk str ctxt i
                           | isUpper c           = UIdentTk str ctxt i
                           | otherwise           = OpTk str ctxt i

--makeToken str ctxt i = Tk str ctxt i

isSymbolChar c = c `elem` ";,(){}"


keywords = 
  [ "<"
  , ">"
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
  , "->"
  , "\174"
  , "\\"
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
  , "PPT"
  , "pres"
  , ":"
  , "..."
  ]


instance (Show a, Eq a, Ord a) => Symbol (Token (Maybe a)) where

runParser (pp) inp =
      let res = UU_Parsing.parse pp inp
          (Pair v final) = evalSteps (res) 
          errs = getMsgs (res) 
      in  (v, errs)










{-



--instance Enum (Token (Maybe a)) where            -- is this right?
--  toEnum   i = Tk (chr i) Nothing NoIDP  
--  fromEnum (Tk c _ _) = ord c
--  fromEnum _          = 0

instance (Show a) => Symbol (Token (Maybe a)) where
--  symBefore = pred
--  symAfter = succ



instance InputState (ParsePres Node String) (Token (Maybe Node)) where
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

prr = LocatorP (NoNode) 
       (LocatorP (NoNode) 
         (WithP id
           (RowP NoIDP 0 [LocatorP NoNode (WithP id (StringP (IDP 200) "100"))
                        ,StringP (IDP 100) "+"
                        ,LocatorP NoNode (WithP id (StringP (IDP 300) "200"))])))





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