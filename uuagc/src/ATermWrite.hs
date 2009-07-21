module ATermWrite where

import ATermAbstractSyntax

writeATerm t            = writeAT 0 t

writeAT 		:: Int -> ATerm -> String
writeAT n (AAppl c ts) = (if (n > 0) then "\n" else "")
                         ++ replicate n ' '  
                         ++ writeATermAux c (map (writeAT (n+2)) ts)
writeAT n (AList ts) 	=  bracket (commaSep (map (writeAT n) ts))
writeAT n (AInt i)	 =  show i
writeAT n (AString s) =  quote s

writeATermAux c []	=  c++(parenthesise "")
writeATermAux c ts	=  c++(parenthesise (commaSep ts))

sepBy sep (x:y:ys)	=  x:sep:sepBy sep (y:ys)
sepBy sep ys            =  ys

commaSep strs		=  concat (sepBy "," strs)
bracket str		= "["++str++"]"
parenthesise str	= "("++str++")"
quote str		= "\""++str++"\""