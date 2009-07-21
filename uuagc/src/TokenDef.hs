{-# OPTIONS -fglasgow-exts #-}
module TokenDef where

import GHC.Prim
import UU.Scanner.Token
import UU.Scanner.GenToken
import UU.Scanner.GenTokenOrd
import UU.Scanner.Position
import UU.Parsing.MachineInterface(Symbol(..))
import Char(isPrint,ord)
import HsToken
import CommonTypes



instance Symbol Token  where
 deleteCost (Reserved key _) = case key of
                "DATA"         -> 7#
                "EXT"          -> 7#
                "ATTR"         -> 7#
                "SEM"          -> 7#
                "USE"          -> 7#
                "INCLUDE"      -> 7#
                _              -> 5#
 deleteCost (ValToken v _  _) = case v of
                TkError -> 0#
                _       -> 5#


tokensToStrings :: [HsToken] -> [(Pos,String)]
tokensToStrings
  = map tokenToString

tokenToString :: HsToken -> (Pos, String)
tokenToString tk
  = case tk of
      AGLocal var pos _        -> (pos, "@" ++ getName var)
      AGField field attr pos _ -> (pos, "@" ++ getName field ++ "." ++ getName attr)
      HsToken value pos        -> (pos, value)
      CharToken value pos      -> (pos, show value)
      StrToken value pos       -> (pos, show value)
      Err mesg pos             -> (pos, " ***" ++ mesg ++ "*** ")

showTokens :: [(Pos,String)] -> [String]
showTokens [] = []
showTokens xs = map showLine . shiftLeft . getLines $ xs

getLines []         = []
getLines ((p,t):xs) =       let (txs,rest)     = span sameLine xs
                                sameLine (q,_) = line p == line q
                            in ((p,t):txs) : getLines rest

shiftLeft lns =        let sh = let m = minimum . checkEmpty . filter (>=1) . map (column.fst.head) $ lns
                                    checkEmpty [] = [1]
                                    checkEmpty x  = x
                                in if m >= 1 then m-1 else 0
                           shift (p,t) = (if column p >= 1 then case p of (Pos l c f) -> Pos l (c - sh) f else p, t)
                       in map (map shift) lns

showLine ts =        let f (p,t) r = let ct = column p
                                     in \c -> spaces (ct-c) ++ t ++ r (length t+ct)
                         spaces x | x < 0 = ""
                                  | otherwise = replicate x ' '
                     in foldr f (const "") ts 1


showStrShort xs = "\"" ++ concatMap f xs ++ "\""
  where f '"' = "\\\""
        f x   = showCharShort' x

showCharShort '\'' = "'" ++ "\\'" ++ "'"
showCharShort c    = "'" ++ showCharShort' c ++ "'"

showCharShort' '\a'  = "\\a"
showCharShort' '\b'  = "\\b"
showCharShort' '\t'  = "\\t"
showCharShort' '\n'  = "\\n"
showCharShort' '\r'  = "\\r"
showCharShort' '\f'  = "\\f"
showCharShort' '\v'  = "\\v"
showCharShort' '\\'  = "\\\\"
showCharShort' x | isPrint x = [x]
                 | otherwise = '\\' : show (ord x)

