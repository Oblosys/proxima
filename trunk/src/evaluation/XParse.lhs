> module XParse where

> import Text.ParserCombinators.Parsec
> import Text.ParserCombinators.Parsec.Token 
> import Text.ParserCombinators.Parsec.Language( haskellDef )
> import Text.ParserCombinators.Parsec.Expr
> 
> import X hiding ((<|>))
> import View

> lexer ::TokenParser ()
> lexer = makeTokenParser
>          (haskellDef 
>           { reservedOpNames = ["*", "**", ";", "|","^o","%", ",","."],
>             reservedNames = ["id", "swap", "assocl", "assocr",
>			       "nil","cons", "node",
>			       "zero","succ",
>			       "inl", "inr",
>			       "dup", "delta", "fst", "snd",
>			       "cmp", "neq", "lt", "leq" ,"geq" ,"gt"]
>	    } )

> parensL = parens lexer
> stringLiteralL = stringLiteral lexer
> reservedOpL = reservedOp lexer
> reservedL = reserved lexer
> identifierL = identifier lexer


> type SymTable a = [(String, ([String], X a))]

 type SymTable' a = Either ParseError (SymTable a)

> defns :: Parser (SymTable a)
> defns = do dfns <- sepBy1 defn (reservedOpL ",")
>            reservedOpL "."
>            return dfns

> defn  = do (f,xs) <- lhs
>            reservedOpL "="
>            e <- expr
>            return (f,(xs,(Define f e)))   
>  where lhs = do f <- identifierL
>                 ((do xs <-parensL (sepBy1 identifierL (reservedOpL ","))
>                      return (f,xs)) 
>                  <|> return (f,[]))


> expr :: Parser (X a)
> expr = buildExpressionParser table term

> table = [[op ";" (:.:) AssocLeft],
>          [op "**" (:**:) AssocLeft, op "*" (:*:) AssocLeft],
>          [op "|" (:|:) AssocLeft]]
>  where op s f assoc = 
>          Infix (do { reservedOpL s; return f }) assoc

> term :: Parser (X a)
> term = do f <- term' 
>           ((do reservedOpL "^o"
>                return (Inv f))
>            <|> return f )

> term' = parensL expr
>           <|> rev "id" Id
>           <|> rev "swap" Swap 
>           <|> rev "assocl" Assocl 
>           <|> rev "assocr" Assocr
>           <|> rev "nil" Nil
>           <|> rev "cons" Cons
>     	    <|> rev "zero" Zero
>           <|> rev "succ" Succ
>	    <|> rev "inl" Inl
>	    <|> rev "inr" Inr
>	    <|> rev "node" Node
>           <|> rev "delta" (Dup (DP []))
>           <|> rev "neq" (Cmpr Neq)
>           <|> rev "lt" (Cmpr Lt)
>           <|> rev "leq" (Cmpr Leq)
>           <|> rev "geq" (Cmpr Geq)
>           <|> rev "gt" (Cmpr Gt)
>           <|> pDup
>           <|> pFix
>	    <|> pIdentifier

>  where rev s r = do { reservedL s; return r}     

> pDup = do reservedL "dup"
>           (do { reservedL "nil"; return (Dup DNil) } <|>
>            do { reservedL "zero"; return (Dup DZero) } <|>
>            parensL (
>              do { s <- pDStr; return (Dup (DStr s)) } <|>
>              do { p <- pDP;   return (Dup (DP p)) } ))
>  where pDStr = do reservedL "str"
>                   s <- stringLiteralL
>		    return s
>        pDP = sepBy1 (rev "fst" DFst <|>
>                      rev "snd" DSnd <|>
>                      do { reservedL "cons"; reservedOpL "^o"; return DCons } <|>
>                      do { reservedL "node"; reservedOpL "^o"; return DNode })
>                     (reservedOpL ";")
>	 rev s r = do { reservedL s; return r}   

> pIdentifier = 
>    do f <- identifierL
>       ((do xs <-parensL (sepBy1 expr (reservedOpL ","))
>            return (Ident f xs)) 
>        <|> return (Ident f []))

> pFix = 
>    do reservedOpL "%"
>       parensL (
>         do v <- identifierL
>            reservedOpL ":"
>            f <- pFun v
>            return (Fix f) )
>  where pFun v = do e <- expr
>                    return (\x -> beta v x e)  -- HACK!

> fromRight ~(Right x) = x
> fromJust ~(Just x) = x