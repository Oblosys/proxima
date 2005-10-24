> module EvaluateInv where

> import View
> import X
> import XParse (defns)
> import XPrelude

> import qualified GetPut 
> import Text.ParserCombinators.Parsec (parseFromFile)

> import IOExts

> import qualified DocTypes_Generated as P
> import DocTypes_Generated (IDD (..), String_ (..), Int_ (..), Bool_ (..) )
> import CommonTypes


> prox2inv :: P.View -> View.View
> prox2inv (P.ANil _) = ANil
> prox2inv (P.AN _ i) = AN (prox2invInt i)
> prox2inv (P.AS _ s) = AS (prox2invString s)
> prox2inv (P.Pr _ v1 v2) = Pr (prox2inv v1) (prox2inv v2)
> prox2inv (P.Ls _ v1 v2) = Ls (prox2inv v1) (prox2inv v2)
> prox2inv (P.L _ v) = L (prox2inv v)
> prox2inv (P.R _ v) = R (prox2inv v)
> prox2inv (P.Mark _ v) = Mark (prox2inv v)
> prox2inv (P.DelL _ v1 v2) = DelL (prox2inv v1) (prox2inv v2)
> prox2inv (P.InsL _ v1 v2) = InsL (prox2inv v1) (prox2inv v2)
> prox2inv (P.SndP _ b v1 v2) = SndP (prox2invBool b) (prox2inv v1) (prox2inv v2)
> prox2inv (P.FstP _ b v1 v2) = FstP (prox2invBool b) (prox2inv v1) (prox2inv v2)
> prox2inv (P.IfNil _ b v) = IfNil (prox2invBool b) (prox2inv v)
> prox2inv (P.Undef _) = Undef
> prox2inv v = debug Err ("prox2inv: unhandled View" ++ show v) Undef

> prox2invInt (Int_ _ i) = i
> prox2invInt _ = 0

> prox2invString (String_ _ s) = s
> prox2invString _ = "ERR"

> prox2invBool (Bool_ _ b) = b
> prox2invBool _ = False

> inv2prox :: View -> P.View
> inv2prox ANil = P.ANil NoIDD
> inv2prox (AN i) = P.AN NoIDD (inv2proxInt i)
> inv2prox (AS s) = P.AS NoIDD (inv2proxString s)
> inv2prox (Pr v1 v2) = P.Pr NoIDD (inv2prox v1)  (inv2prox v2)
> inv2prox (Ls v1 v2) = P.Ls NoIDD (inv2prox v1)  (inv2prox v2)
> inv2prox (L v) = P.L NoIDD (inv2prox v)
> inv2prox (R v) = P.R NoIDD (inv2prox v)
> inv2prox (Mark v) = P.Mark NoIDD (inv2prox v)
> inv2prox (DelL v1 v2) = P.DelL NoIDD (inv2prox v1)  (inv2prox v2)
> inv2prox (InsL v1 v2) = P.InsL NoIDD (inv2prox v1)  (inv2prox v2)
> inv2prox (SndP b v1 v2) = P.SndP NoIDD (inv2proxBool b) (inv2prox v1)  (inv2prox v2)
> inv2prox (FstP b v1 v2) = P.FstP NoIDD (inv2proxBool b) (inv2prox v1)  (inv2prox v2)
> inv2prox (IfNil b v) = P.IfNil NoIDD (inv2proxBool b) (inv2prox v)
> inv2prox v = debug Err ("inv2prox: unhandled View" ++ show v) (P.Undef NoIDD)

> inv2proxInt :: Int -> Int_
> inv2proxInt i  = (Int_ NoIDD i)

> inv2proxString :: String -> String_
> inv2proxString s  = (String_ NoIDD s)

> inv2proxBool :: Bool -> Bool_
> inv2proxBool b  = (Bool_ NoIDD b)

----


> evaluate :: String -> P.EitherDocView -> P.View -> IO P.View
> evaluate evalFn (P.LeftDocView _ err) oldEnr              = return $ oldEnr
> evaluate evalFn (P.RightDocView _ docView) oldEnr = 
>  do { prel <- prelude
>     ; return $
>         case GetPut.get prel (Ident evalFn []) (prox2inv docView) of
>           Left e  -> debug Err ("evaluate: "++ show e) oldEnr -- this should not happen
>           Right enrView -> (inv2prox enrView)
>     }
> evaluate evalFn errDoc oldEnr =  return $ debug Err ("inv2prox: unhandled case: " ++ show errDoc) oldEnr


> reduce :: String -> P.View -> IO P.EitherDocView
> reduce evalFn enrView =
>  do { prel <- prelude
>     ; return $
>         case GetPut.put prel (Ident evalFn []) (prox2inv enrView) of
>           Left err  -> P.LeftDocView NoIDD (String_ NoIDD (show err))
>           Right docView -> P.RightDocView NoIDD (inv2prox docView)
>     }
    
 eval f = GetPut.eval f . read
 get f = GetPut.get f . read
 put f = GetPut.put f . read   



> type SymTable a = [(String, ([String], X a))]


> preludeFilename = "XPrelude.inv"

> prelude :: IO (SymTable View)
> prelude =  (do est <- parseFromFile defns preludeFilename
>                case est of
>                  Right st -> return st
>                  Left s -> error (show s))



> f = dup <.> (Dup (DF len) <.> Swap :*: Id) 
 
> len = AN . len'
>  where len' ANil = 0
>        len' (Ls a x) = 1 + len' x

> t = "'neko':'inu':'kani':[]"

 ----------

> dfst = DP [DFst]

> mfstid = mapX (Dup dfst) <.> (Inv zipX)

> t2 = "<1,'neko'>:<2,'inu'>:<3,'kani'>:[]"
> f2 = dup <.> (Id :*: mfstid)

Example:

(Getting)
Main> get f2 t2
Right <<1,'neko'>:<2,'inu'>:<3,'kani'>:[],<<1,'neko'>:<2,'inu'>:<3,'kani'>:[],1:2:3:[]>>

(Changing 2 to 4)
Main> put f2 "<<1,'neko'>:<2,'inu'>:<3,'kani'>:[],<<1,'neko'>:<2,'inu'>:<3,'kani'>:[],1:*4:3:[]>>"
Right (<1,'neko'>:<*4,'inu'>:<3,'kani'>:[])

(Deleting 2)
Main> put f2 "<<1,'neko'>:<2,'inu'>:<3,'kani'>:[],<<1,'neko'>:<2,'inu'>:<3,'kani'>:[],1:\\(3:[])>>"
Right (<1,'neko'>:\(<3,'kani'>:[]))


> revidx = mapX dupfst <.> unzipX <.> (Id :*: rev)


Main> eval revidx "<1,'a'>:<2,'b'>:<3,'c'>:[]"
Right <<1,'a'>:<2,'b'>:<3,'c'>:[],3:2:1:[]>

 ----------

> t3 :: View
> t3 = read ("{ 'addrbook', { 'takeichi', {'takeichi@mist',[]}:[]}:" ++
>            "              {'hu', {'hu@mist',[]}:[]}: "++
>            "              {'mu', {'mu@ipl',[]}:[]}: []}")

> f3 = Inv Node <.> (Id :*: dupnames) <.> subr <.>
>      (Dup (DF len) <.> Swap :*: Node)
> dupnames = Define "dupnames"
>             (Fix (\x -> Inv Nil <.> Nil <.> dup <|>
>                         Inv Cons <.> 
>                          (Inv Node <.> Dup dfst <.> Swap <.> (Id :*: Node) 
>                             :*: x) <.>
>                          trans <.> (Cons :*: Cons)))

(n,ts) ((n,ts),n) (n,(n,ts)


