> module XPrelude where

> import View
> import X

Basics.

> dup = Define "dup" (Dup (DP []))
> eq = Define "eq" (Inv (Dup (DP [])))
> dupfst = Define "dup fst" (Dup (DP [DFst]))
> eqw = Inv . Dup

> wrap = Define "wrap" (Dup DNil <.> Cons)

> subr = Define "subr" (Assocl <.> (Swap:*:Id) <.> Assocr)
> trans = Define "trans" (Assocr <.> (Id:*:subr) <.> Assocl)
> distr = Define "distr" ((dup :*: Id) <.> trans)

List processing.

> fold f g = Define ("fold " 
>                     ++ (showsPrec 6 f . (' ':) . showsPrec 6 g $ ""))
>             (Fix (\x -> Inv Nil <.> g   <|>
>                         Inv Cons <.> (Id :*: x) <.> f))

> mapX f = Define ("map "++ showsPrec 6 f "")
>            (Fix (\x -> Inv Nil <.> Nil  <|>
>                        Inv Cons <.> (f :*: x) <.> Cons))

> mapX2 f = fold ((f :*: Id) <.> Cons) Nil

Snoc and reverse.

> snoc = Define "snoc"
>         (Fix (\x -> Inv (Dup DNil) <.> Dup DNil <.> Cons <|>
>                     (Id :*: Inv Cons) <.> subr <.> (Id :*: x) <.>
>                        Cons))

> rev = Define "rev" (fold snoc Nil)


The canonical example unzip.

> unzipX = Define "unzip" (Inv zipX)
> zipX = Define "zip" 
>         (Fix (\x -> (Inv Nil :*: Inv Nil) <.> eq <.> Nil <|>
>                     (Inv Cons :**: Inv Cons) <.> trans <.>
>                       (Id :*: x) <.> Cons                ))

<evens, odds>

> evenOdd = Define "evenOdd"
>            (Fix (\x -> Inv Nil <.> dup <.> (Nil :*: Nil) <|>
>                        Inv Cons <.> (Id :*: x <.> Swap) <.> 
>                          Assocl <.> (Cons :*: Id)))

Merge

> unmerge = Define "unmerge" (Inv merge)
> merge = 
>  Define "merge"
>    (Fix (\x -> eqw DNil <.> mapX Inl <|> 
>                Swap <.> eqw DNil <.> mapX Inr <|>
>                (Inv Cons :*: Inv Cons) <.> trans <.>
>                  ((leq :*: Id) <.> Assocr <.> 
>                     (Id :*: subr <.> (Id :*: Cons) <.> x) <.> (Inl:*:Id) <|>
>                   (gt :*: Id) <.> (Swap :*: Id) <.> Assocr <.>
>                     (Id :*: Assocl <.> (Cons :*: Id) <.> x) <.> (Inr:*:Id)) <.>
>                  Cons))

> lt  = Cmpr Lt
> leq = Cmpr Leq
> geq = Cmpr Geq
> gt = Cmpr Gt


filter in Fun. Let L be false and R be true.

 filter = u(X: Inv Nil <.> Nil   <|>
               Inv Cons <.> (Inv Inl :*: Id) <.> Snd <.> X    <|>
               Inv Cons <.> (Inv Inr :*: X) <.> Cons)

filter in Inv

> filterX = 
>  Define "filter"
>   (Fix (\x -> Inv Nil <.> dup <.> (Nil :*: Nil)    <|>
>               Inv Cons <.> (Inv Inl :*: Id) <.> (Inl :*: x) <.>
>                   subr <.> (Id :*: Cons)   <|>
>               Inv Cons <.> (Inv Inr :*: Id) <.> (Dup DNil :*: x) <.>
>                   trans <.> (Id:*:(Inr:*:Id)) <.> (Cons :**: Cons)))




Tree processing.

> type Name = String

> renameS :: [(Name,Name)] -> X View
> renameS fm = Inj ((const True, ren fm), (const True, ren fm'))
>   where ren fm (AS str) = 
>          case lookup str fm of
>             Just str' -> AS str'
>             Nothing -> AS str
>         fm' = map swap fm

> swap (a,b) = (b,a)

> mkname name = Dup (DStr name) :.: Swap

> hoist tag = Inv (unhoist tag) 
> unhoist tag = wrap :.: mkname tag :.: Node 

> pivot :: Name -> X View
> pivot name = Inv (unpivot name)
> unpivot name = Inv Node :.: ((Dup DNil :.: Node):*:Id)
>                :.: Cons :.: mkname name :.: Node


> ex = Inv Node :.: (Id :*: Inv Cons) :.: subr :.: (Id :*: Cons) :.: Node
