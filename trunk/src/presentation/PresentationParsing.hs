module PresentationParsing where

import CommonTypes
import DocTypes
import PresTypes


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


pKey :: String -> TreeParser (Token (Maybe Node))
pKey str = pSym  (strTk str)

pKeyC :: Int -> String -> TreeParser (Token (Maybe Node))
pKeyC c str = pCSym c (strTk str)


-- expensive, because we want holes to be inserted, not strings
pLIdent :: TreeParser (Token (Maybe Node))
pLIdent = pCSym 20 (LIdentTk "ident" Nothing (IDP (-1)))
--pLIdent = pCSym 20 lIdentTk

-- todo return int from pInt, so unsafe intVal does not need to be used anywhere else
pInt :: TreeParser (Token (Maybe Node))
pInt = pCSym 20 (IntTk "0" Nothing (IDP (-1)))
--pInt = pCSym 20 intTk



-- *********** BUG IN GHC 6.0
-- intTk and lIntTk calls are replaced by their bodies, otherwise GHC panics



-- holes are cheap. actually only holes should be cheap, but presently structurals are all the same
pStruct :: TreeParser (Token (Maybe Node))
pStruct = pCSym 4 (Structural Nothing (EmptyP NoIDP) NoIDP)



-- pCostSym expects the parser twice
pCSym c p = pCostSym c p p

lIdentVal :: Token (Maybe Node) -> String
lIdentVal (LIdentTk "" _ _)  = "x"   -- may happen on parse error (although not likely since insert is expensive)
lIdentVal (LIdentTk str _ _) = str
lIdentVal tk                 = debug Err ("PresentationParser.lIdentVal: no IdentTk " ++ show tk) "x"


intVal :: Token (Maybe Node) -> Int
intVal (IntTk "" _ _)  = 0   -- may happen on parse error (although not likely since insert is expensive)
intVal (IntTk str _ _) = read str
intVal tk              = debug Err ("PresentationParser.intVal: no IntTk " ++ show tk) (-9999)





-- inefficient walk
walk :: ParsePres a b c -> Maybe (Token (Maybe Node), ParsePres a b c)
walk (ParsePres pres) = case walk' pres Nothing of
                  Just (tk, tree') -> Just(tk, ParsePres tree')
                  Nothing          -> Nothing

-- walk is not ok. can't always return a rest presentation (in case of structural) and Nothing is also not an option
-- switch params
walk' :: Presentation -> Maybe Node -> Maybe (Token (Maybe Node), Presentation)
--walk' (StringP i (c:cs)) ctxt = Just (Tk c ctxt i, StringP i cs)
walk' (StringP i [])     _    = Nothing
walk' (StringP i str) ctxt = Just (mkToken str ctxt i, StringP i "")
walk' (ParsingP _ pres)     ctxt = walk' pres ctxt
walk' (WithP _ pres)     ctxt = walk' pres ctxt
walk' (OverlayP _ []) ctxt = Nothing
walk' (OverlayP _ (pres:press)) ctxt = walk' pres ctxt
walk' (ColP i _ [])      _    = Nothing
walk' (ColP i _ (p:ps))  ctxt = case walk' p ctxt of
                                  Just (tk,p') -> Just (tk, ColP i 0 (p':ps))
                                  Nothing          -> walk' (ColP i 0 ps) ctxt
walk' (RowP i _ [])      _    = Nothing
walk' (RowP i _ (p:ps))  ctxt = case walk' p ctxt of
                                  Just (tk,p') -> Just (tk, RowP i 0 (p':ps))
                                  Nothing          -> walk' (RowP i 0 ps) ctxt
walk' (LocatorP l p)     ctxt = case walk' p (Just l) of 
                                  Just (tk, p') -> Just (tk, LocatorP l p')
                                  Nothing            -> Nothing
walk' (StructuralP id pres) ctxt = Just (Structural ctxt pres id, StringP NoIDP "")
walk' pres _ = debug Err ("*** PresentationParser.walk: unimplemented presentation: " ++ show pres) Nothing

--walkTree tree = case walk' tree Nothing of
--                  Just (Tk str ctxt i,tree') -> show ctxt++ str++ walkTree tree'
--                  Nothing         -> ""



gatherChildren :: Presentation -> Maybe Node -> [Either (Maybe Node,Presentation) (Maybe Node,Presentation)]
gatherChildren (EmptyP _)    ctxt = []
gatherChildren (StringP _ _) ctxt = []
gatherChildren (ImageP _ _)  ctxt = []
gatherChildren (PolyP _ _ _) ctxt = []
gatherChildren (RectangleP _ _ _ _) ctxt = []
gatherChildren (WithP _ pres)    ctxt = gatherChildren pres ctxt
gatherChildren (OverlayP _ []) ctxt = []
gatherChildren (OverlayP _ (pres:press)) ctxt = gatherChildren pres ctxt
gatherChildren (ColP i _ [])      _    = []
gatherChildren (ColP i _ (p:ps))  ctxt = gatherChildren p ctxt ++ gatherChildren (RowP i 0 ps) ctxt
gatherChildren (RowP i _ [])      _    = []
gatherChildren (RowP i _ (p:ps))  ctxt = gatherChildren p ctxt ++ gatherChildren (RowP i 0 ps) ctxt
gatherChildren (LocatorP l p)     ctxt = gatherChildren p (Just l)  
gatherChildren (ParsingP _ pres) ctxt    = [Left (ctxt, pres)]
gatherChildren (StructuralP _ pres) ctxt = [Right (ctxt, pres)]
gatherChildren pres _ = debug Err ("*** PresentationParser.gatherChildren: unimplemented presentation: " ++ show pres) []



type TreeParser a = AnaParser (ParsePres Node String) Pair  (Token (Maybe Node)) a 
--type TreeParser a = AnaParser (ParsePres Node Char) Pair  (Token (Maybe Node)) a 

{-

Weird trick with dummy parameter that matches the token type returned by walk. 


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
             | Structural a Presentation IDP -- deriving (Show)

instance Show (Token a) where
  show (StrTk str _ _)    = show str
  show (IntTk str _ _)    = show str
  show (LIdentTk str _ _) = show str
  show (UIdentTk str _ _) = show str
  show (OpTk str _ _)     = show str
  show (SymTk str _ _)    = show str
  show (Structural _ _ _) = "<structural>"
  
instance Eq (Token a) where
  StrTk str1 _ _ == StrTk str2 _ _ = str1 == str2
  IntTk _ _ _    == IntTk _ _ _    = True
  LIdentTk _ _ _ == LIdentTk _ _ _ = True
  UIdentTk _ _ _ == UIdentTk _ _ _ = True
  OpTk _ _ _     == OpTk _ _ _     = True
  SymTk _ _ _    == SymTk _ _ _    = True
  Structural _ _ _ == Structural _ _ _ = True   -- should only be true if both have the same type
  _              == _              = False

instance Ord (Token a) where
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

  Structural _ _ _ <= Structural _ _ _ = True
  Structural _ _ _ <= SymTk _ _ _    = True
  Structural _ _ _ <= OpTk _ _ _     = True
  Structural _ _ _ <= UIdentTk _ _ _ = True
  Structural _ _ _ <= LIdentTk _ _ _ = True
  Structural _ _ _ <= IntTk _ _ _    = True
  Structural _ _ _ <= StrTk _ _ _    = True

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
tokenString (Structural n _ id) = "<structural token>"
                             
tokenNode :: Token (Maybe Node) -> Maybe Node                 
tokenNode (StrTk s n id)      = n
tokenNode (IntTk s n id)      = n
tokenNode (LIdentTk s n id)   = n
tokenNode (UIdentTk s n id)   = n
tokenNode (OpTk s n id)       = n
tokenNode (SymTk s n id)      = n
tokenNode (Structural n _ id) = n

tokenIDP :: Token (Maybe Node) -> IDP       
tokenIDP (StrTk s n id)    = id
tokenIDP (IntTk s n id)    = id
tokenIDP (LIdentTk s n id) = id
tokenIDP (UIdentTk s n id) = id
tokenIDP (OpTk s n id)     = id
tokenIDP (SymTk s n id)    = id
tokenIDP (Structural n _ id)  = id

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
strucTk   = Structural Nothing (EmptyP NoIDP) (IDP (-1))



mkToken :: String -> Maybe Node -> IDP -> Token (Maybe Node)
mkToken str@(c:_)   ctxt i | str `elem` keywords = StrTk str ctxt i
                           | isDigit c           = IntTk str ctxt i
                           | isLower c           = LIdentTk str ctxt i
                           | isUpper c           = UIdentTk str ctxt i
                           | otherwise           = OpTk str ctxt i

--makeToken str ctxt i = Tk str ctxt i

isSymbolChar c = c `elem` ";,(){}"


keywords = 
  [ "," --
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



{-
prsr :: TreeParser Int
prsr =   (\_ -> 1) <$> pSym (IntTk)
     <|> (\_ -> 1)  <$> pSym (StrTk "a")
-}

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

Root usually has no presentation of its own, so current algorithm does not recover its id. This is bad

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
tks = [Tk '1' (Just (ExpNode (Int 0 9)))
      ,Tk '+' (Just (ExpNode (Sum 1 (Int 0 9)(Int 2 9))))
      ,Tk '2' (Just (ExpNode (Int 2 9)))]

instance Symbol Char where
 deleteCost b = 1{-I-}


-- simple parsers for checking error stuff

pInt' = (\ds -> Int (-1) (foldl (\r c-> 10*r+ord c-ord '0') 0 ds))<$> pList1 pDigit'

--pDigit :: Parser (Token (Maybe Node)) (Token (Maybe Node))

-- type sig required due to monomorphism restriction...
pDigit' :: Parser Char Char
pDigit' = pCostRange 5 '0' (Range '0' '9')

-- right associative, so we don't need to fix left recursion.
-- parser parses (1*3)*4 differently from 1*3*4 
{-
parse1Exp'   = parse1Term'  
            <|> (\t _ e -> Sum (-1) t e)  <$> parse1Term' <*> pSym' '+' <*> parse1Exp'
            
parse1Term'   = parse1Factor'
             <|> (\f _ t -> Prod (-1) f t) <$> parse1Factor' <*> pSym' '*' <*> parse1Term'
            
parse1Factor' = pInt'
             <|> pSym' '(' *> parse1Exp' <* pSym' ')'
-}

parse1Exp'   = parse1Term' 
             <??> ((\ _ e t -> Sum (-1) t e) <$> pSym' '+' <*> parse1Exp')

parse1Term'   = parse1Factor'
             <|> (\f _ t -> Prod (-1) f t) <$> parse1Factor' <*> pSym' '*' <*> parse1Term'
            
parse1Factor' = pInt'
             <|> pSym' '(' *> parse1Exp' <* pSym' ')'

pSym' c = pCostSym 5 c c






pres0 = row [row [text "1", text "+", text "2"], text "+", text "3"]

-- error recovery is bit weird here: etest parse1Exp' "1*2+2x43*4"
-- gives                                               1*2+243*4+0

-- always left factor, and preferably use chain

pHoleExp' :: Parser Char Exp
pHoleExp' = HoleExp <$ pCostSym 1 '#' '#'


-}

