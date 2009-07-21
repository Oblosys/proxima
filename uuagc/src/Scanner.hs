{-# OPTIONS -fglasgow-exts #-}

module Scanner where
import GHC.Prim
import TokenDef
import UU.Scanner.Position
import UU.Scanner.Token
import UU.Parsing(InputState(..),Either'(..))
import Maybe
import List
import Char
import UU.Scanner.GenToken
import Options

data Input = Input !Pos String (Maybe (Token, Input))  

instance InputState Input Token Pos where
 splitStateE input@(Input _ _ next) = 
                case next of
                     Nothing         -> Right' input
                     Just (s, rest)  -> Left' s rest
 splitState (Input _ _ next) = 
                case next of
                     Nothing         -> error "splitState on empty input"
                     Just (s, rest)  -> (# s, rest #)
 getPosition (Input pos _ next) =  case next of 
                                    Just (s,_) -> position s
                                    Nothing    -> pos -- end of file


input :: Options -> Pos -> String -> Input
input opts pos inp = Input pos 
                      inp 
                      (case scan opts pos inp of
                             Nothing      -> Nothing
                             Just (s,p,r) -> Just (s, input opts p r)
                      )

type Lexer s = Pos -> String -> Maybe (s,Pos,String)

scan :: Options -> Lexer Token
scan opts
  = scan
  where
    keywords' = if lcKeywords opts
                then map (map toLower) keywords
                else keywords
    mkKeyword s | s `elem` lowercaseKeywords = s
                | otherwise                  = map toUpper s
  
    scan :: Lexer Token
    scan p []                        = Nothing
    scan p ('-':'-':xs)              = let (com,rest) = span (/= '\n') xs
                                       in advc' (2+length com) p scan rest
    scan p ('{':'-':xs)              = advc' 2 p (ncomment scan) xs
    scan p ('{'    :xs)              = advc' 1 p codescrap xs
    scan p ('\CR':xs)                = case xs of
                                        '\LF':ys -> newl' p scanBeginOfLine ys --ms newline
                                        _        -> newl' p scanBeginOfLine xs --mac newline
    scan p ('\LF':xs)                =  newl' p scanBeginOfLine xs             --unix newline
    scan p (x:xs) | isSpace x        = updPos'  x p scan  xs
    scan p xs = Just (scan' xs)
      where scan' ('.' :rs)          = (reserved "." p, advc 1 p, rs)
            scan' ('@' :rs)          = (reserved "@" p, advc 1 p, rs)
            scan' (',' :rs)          = (reserved "," p, advc 1 p, rs)
            scan' ('_' :rs)          = (reserved "_" p, advc 1 p, rs)
            scan' ('~' :rs)          = (reserved "~" p, advc 1 p, rs)
            scan' ('<' :rs)          = (reserved "<" p, advc 1 p, rs)
            scan' ('[' :rs)          = (reserved "[" p, advc 1 p, rs)
            scan' (']' :rs)          = (reserved "]" p, advc 1 p, rs)
            scan' ('(' :rs)          = (reserved "(" p, advc 1 p, rs)
            scan' (')' :rs)          = (reserved ")" p, advc 1 p, rs)
    --        scan' ('{'    :rs)       = (OBrace      p, advc 1 p, rs)
    --        scan' ('}'    :rs)       = (CBrace      p, advc 1 p, rs)
    
            scan' ('\"' :rs)         = let isOk c = c /= '"' && c /= '\n'
                                           (str,rest) = span isOk rs
                                       in if null rest || head rest /= '"'
                                              then (errToken "unterminated string literal"   p
                                                   , advc (1+length str) p,rest)
                                              else (valueToken TkString str p, advc (2+length str) p, tail rest)
    
            scan' ('=' : '>' : rs)   = (reserved "=>" p, advc 2 p, rs)
            scan' ('=' :rs)          = (reserved "=" p, advc 1 p, rs)
            scan' (':':'=':rs)       = (reserved ":=" p, advc 2 p, rs)
    
            scan' (':' :rs) | not (doubleColons opts) = (reserved ":" p, advc 1 p, rs)
            scan' (':':':':rs) | doubleColons opts    = (reserved "::" p, advc 1 p, rs)
            scan' ('|' :rs)          = (reserved "|" p, advc 1 p, rs)
    
            scan' ('/':'\\':rs)      = (reserved "/\\" p, advc 2 p, rs)
            scan' ('-':'>' :rs)      = (reserved "->" p, advc 2 p, rs)
            scan' ('-'     :rs)      = (reserved "-" p, advc 1 p, rs)
            scan' ('*'     :rs)      = (reserved "*" p, advc 1 p, rs)
    
            scan' (x:rs) | isLower x = let (var,rest) = ident rs
                                           str        = (x:var)
                                           tok | str `elem` keywords' = reserved (mkKeyword str)
                                               | otherwise            = valueToken TkVarid str
                                       in (tok p, advc (length var+1) p, rest)
                         | isUpper x = let (var,rest) = ident rs
                                           str        = (x:var)
                                           tok | str `elem` keywords' = reserved (mkKeyword str)
                                               | otherwise            = valueToken TkConid str
                                       in (tok p, advc (length var+1) p,rest)
                         | otherwise = (errToken ("unexpected character " ++ show x) p, advc 1 p, rs)
    
    scanBeginOfLine :: Lexer Token
    scanBeginOfLine p ('{' : '-' : ' ' : 'L' : 'I' : 'N' : 'E' : ' ' : xs)
      | isOkBegin rs && isOkEnd rs'
          = scan (advc (8 + length r + 2 + length s + 4) p') (drop 4 rs')
      | otherwise
          = Just (errToken ("Invalid LINE pragma: " ++ show r) p, advc 8 p, xs)
      where
        (r,rs)   = span isDigit xs
        (s, rs') = span (/= '"') (drop 2 rs)
        p' = Pos (read r - 1) (column p) s    -- LINE pragma indicates the line number of the /next/ line!
    
        isOkBegin (' ' : '"' : _) = True
        isOkBegin _               = False
    
        isOkEnd ('"' : ' ' : '-' : '}' : _) = True
        isOkEnd _         = False
    scanBeginOfLine p xs
      = scan p xs
 

ident = span isValid
 where isValid x = isAlphaNum x || x =='_' || x == '\''
lowercaseKeywords = ["loc","lhs", "inst"]
keywords = lowercaseKeywords ++
           [ "DATA", "EXT", "ATTR", "SEM","TYPE", "USE", "INCLUDE"
           , "SET","DERIVING","FOR", "WRAPPER", "MAYBE", "EITHER", "MAP", "INTMAP"
           , "PRAGMA", "SEMPRAGMA", "MODULE", "ATTACH", "UNIQUEREF", "INH", "SYN"
           ]

ncomment c p ('-':'}':xs) = advc' 2 p c  xs
ncomment c p ('{':'-':xs) = advc' 2 p (ncomment (ncomment c)) xs
ncomment c p (x:xs)       = updPos' x p (ncomment c)  xs
ncomment c p []           = Just (errToken "unterminated nested comment" p, p,[])

codescrap p xs = let (p2,xs2,sc) = codescrap' 1 p xs
                 in case xs2 of
                         ('}':rest) -> Just (valueToken TkTextln sc p,advc 1 p2,rest)
                         _          -> Just (errToken "unterminated codescrap" p,p2,xs2)


codescrap' d p [] = (p,[],[])
{-
codescrap' d p ('{':'{':xs) = let (p2,xs2,sc) = advc' 2 p (codescrap' d) xs
                              in (p2,xs2,'{':' ':sc)
codescrap' d p ('}':'}':xs) = let (p2,xs2,sc) = advc' 2 p (codescrap' d) xs
                              in (p2,xs2,'}':' ':sc)
-}                              
codescrap' d p ('{':xs)     = let (p2,xs2,sc) = advc' 1 p (codescrap' (d+1)) xs
                              in (p2,xs2,'{' : sc)
codescrap' d p ('}':xs)     | d == 1 = (p,'}':xs,[])
                            | otherwise = let (p2,xs2,sc) = advc' 1 p (codescrap' (d-1)) xs
                                          in (p2,xs2,'}' : sc)
codescrap' d p (x  :xs)     = let (p2,xs2,sc) = updPos' x p (codescrap' d) xs
                              in (p2,xs2,x:sc)
--Literate Mode
scanLit xs = (fs, foldr insNL (const "") codeLns 1)
  where insNL (n,line) rec = \n1 -> replicate (n-n1) '\n' ++ line ++ rec n
        (fs,codeLns,_) = getBlocks ([1..] `zip`  toLines xs)
        getBlocks [] = ([],[],[])
        getBlocks xs = let (files1,txt1,r1) = getBlock xs
                           (files2,txt2,r2) = getBlocks r1
                       in (files1++files2, txt1++txt2, r2)


        getBlock = getLines . dropWhile comment
        getLines [] = ([],[],[])
        getLines ((n,l):ls) | "\\begin{code}" `isPrefixOf` l = let (lns,rest) = codelines ls
                                                               in ([],lns,rest)
                            | "\\begin{Code}" `isPrefixOf` l = let (lns,rest) = codeLines ls
                                                               in ([],lns,rest)
                            | "\\IN{" `isPrefixOf` l        =
                                     let name = getName l
                                     in  ([name],[],ls)
                            | otherwise = getBlock ls
        comment = not . ("\\" `isPrefixOf`) .snd

toLines     :: String -> [String]
toLines ""   = []
toLines s    = let (l,s') = breakLine s
               in l :  toLines s'
breakLine xs = case xs of
                '\CR' : ys -> case ys of
                                '\LF' : zs -> ([],zs) 
                                _          -> ([],ys)
                '\LF' : ys -> ([], ys)
                x     : ys -> let (l,s) = breakLine ys
                              in (x:l,s)
                []         -> ([],[])
 
codelines [] = error "Unterminated literate code block"
codelines ((n,l):ls) | "\\end{code}" `isPrefixOf` l = ([],ls)
                     | otherwise                    = let (lns,r) = codelines ls
                                                      in ((n,l):lns,r)

codeLines [] = error "Unterminated literate Code block"
codeLines ((n,l):ls) | "\\end{Code}" `isPrefixOf` l = ([],ls)
                     | otherwise                    = let (lns,r) = codeLines ls
                                                      in ((n,l):lns,r)

getName l = case r of
   ('}':_) -> nm
   _       -> error $ "missing '}' in \\IN"
 where (nm,r) = span (/='}') (drop 4 l)
