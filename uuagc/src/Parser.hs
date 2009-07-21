module Parser
where
import Data.Maybe
import UU.Parsing
import UU.Parsing.Machine(RealParser(..),RealRecogn(..),anaDynE,mkPR)
import ConcreteSyntax
import CommonTypes
import Patterns
import UU.Pretty(text,PP_Doc,empty,(>-<))
import TokenDef
import List (intersperse)
import Char
import Scanner (Input(..),scanLit,input)
import List
import Expression
import UU.Scanner.Token
import UU.Scanner.TokenParser
import UU.Scanner.GenToken
import UU.Scanner.GenTokenParser
import UU.Scanner.Position
import UU.Scanner.TokenShow()
import System.Directory
import HsTokenScanner
import Options


type AGParser = AnaParser Input  Pair Token Pos

pIdentifier, pIdentifierU :: AGParser Identifier
pIdentifierU = uncurry Ident <$> pConidPos
pIdentifier   = uncurry Ident <$> pVaridPos


parseAG :: Options -> [FilePath] -> String -> IO (AG,[Message Token Pos])
parseAG opts searchPath file 
              = do (es,_,_,mesg) <- parseFile opts searchPath file
                   return (AG es, mesg)

depsAG :: Options -> [FilePath] -> String -> IO ([String], [Message Token Pos])
depsAG opts searchPath file
  = do (_,_,fs,mesgs) <- parseFile opts searchPath file
       return (fs, mesgs)

parseFile :: Options -> [FilePath] -> String -> IO  ([Elem],[String],[String],[Message Token Pos ])
parseFile opts searchPath file 
               = do txt <- readFile file
                    let litMode = ".lag" `isSuffixOf` file
                        (files,text) = if litMode then scanLit txt
                                       else ([],txt)
                        tokens       = input opts (initPos file) text

                        steps = parse (pElemsFiles opts) tokens
                        stop (_,fs,_,_) = null fs
                        cont (es,fs,allfs,msg)
                          = do files <- mapM (resolveFile searchPath) fs
                               res <- mapM (parseFile opts searchPath) files
                               let (ess,fss,allfss, msgs) = unzip4 res
                               return (es ++ concat ess, concat fss, concat allfss ++ allfs, msg ++ concat msgs)
                    let (Pair (es,fls) _ ,mesg) = evalStepsMessages steps
                    let allfs = files ++ fls
                    loopp stop cont (es,allfs,allfs,mesg)

resolveFile :: [FilePath] -> FilePath -> IO FilePath
resolveFile path fname = search (path ++ ["."])                                                  
 where search (p:ps) = do dExists <- doesDirectoryExist p
                          if dExists
                             then do let filename = p++pathSeparator++fname
                                     fExists <- doesFileExist filename
                                     if fExists
                                        then return filename
                                        else search ps      
                             else search ps
       search []     = error ("File: " ++ show fname ++ " not found in search path: " ++ show (concat (intersperse ";" (path ++ ["."]))) )                      

pathSeparator = "/"

evalStepsMessages :: (Eq s, Show s, Show p) => Steps a s p -> (a,[Message s p])
evalStepsMessages steps = case steps of
  OkVal v        rest -> let (arg,ms) = evalStepsMessages rest
                         in (v arg,ms)
  Ok             rest -> evalStepsMessages rest
  Cost  _        rest -> evalStepsMessages rest
  StRepair _ msg rest -> let (v,ms) = evalStepsMessages rest
                         in (v, msg:ms)
  Best _   rest   _   -> evalStepsMessages rest
  NoMoreSteps v       -> (v,[])

loopp ::(a->Bool) -> (a->IO a) -> a ->  IO a
loopp pred cont x | pred x = return x
                  | otherwise = do x' <- cont x
                                   loopp pred cont x'
                                  
pElemsFiles :: Options -> AGParser ([Elem],[String])
pElemsFiles opts = pFoldr (($),([],[])) pElem'
   where pElem' =  addElem <$> pElem opts
               <|> pINCLUDE *> (addInc <$> pStringPos)
         addElem e      (es,fs) = (e:es,   fs)
         addInc  (fn,_) (es,fs) = (  es,fn:fs)

pCodescrapL opts = (\(ValToken _ str pos) -> (str, pos))<$>
            parseScrapL opts  <?> "a code block"

parseScrapL :: Options -> AGParser Token
parseScrapL opts
            = let p acc =  (\k (Input pos str next) ->
                            let (sc,rest) = case next of
                                  Just (t@(ValToken TkTextln _ _), rs) -> (t,rs)
                                  _ -> let (tok,p2,inp2) = codescrapL pos str
                                       in (tok, input opts p2 inp2)
                                steps   = k ( rest)
                            in  (val (acc sc)  steps)
                       )
              in anaDynE  (mkPR (P (p  ), R (p (const id))))

codescrapL p []                 = (valueToken TkTextln "" p,p,[])
codescrapL p (x:xs) | isSpace x = (updPos'  x p)  codescrapL xs
                    | otherwise = let refcol = column p
                                      (p',sc,rest) = scrapL refcol p  (x:xs)
                                  in (valueToken TkTextln sc p,p',rest)

scrapL ref p (x:xs) | isSpace x || column p >= ref =
                          let (p'',sc,inp) = updPos'  x p (scrapL ref)  xs
                          in  (p'',x:sc,inp)
                    | otherwise       =(p,[],x:xs)
scrapL ref p []     = (p,[],[])

pNontSet = set0
  where set0 = pChainr (Intersect <$ pIntersect) set1
        set1 = pChainl (Difference <$ pMinus) set2
        set2 = pChainr (pSucceed Union) set3
        set3 = pIdentifierU <**> opt (flip Path <$ pArrow <*> pIdentifierU) NamedSet 
            <|> All <$ pStar
            <|> pParens set0

pNames :: AGParser [Identifier]
pNames = pList1 pIdentifier

-- pAG  :: Options -> AGParser AG
-- pAG opts = AG <$> pElems opts

pElems :: Options -> AGParser Elems
pElems opts = pList_ng (pElem opts)

pComplexType opts =  List   <$> pBracks pTypeEncapsulated 
            <|> Maybe  <$ pMAYBE <*> pType
            <|> Either <$ pEITHER <*> pType <*> pType
            <|> Map    <$ pMAP <*> pTypePrimitive <*> pType
            <|> IntMap <$ pINTMAP <*> pType
            <|> tuple  <$> pParens (pListSep pComma field)
 where field = (,) <$> ((Just <$> pIdentifier <* pTypeColon opts) `opt` Nothing) <*> pTypeEncapsulated
       tuple xs = Tuple [(fromMaybe (Ident ("x"++show n) noPos) f, t) 
                        | (n,(f,t)) <- zip [1..] xs
                        ]
pElem :: Options -> AGParser Elem
pElem opts
      =  Data <$> pDATA
              <*> pOptClassContext
              <*> pNontSet
              <*> pList pIdentifier
              <*> pOptAttrs opts
              <*> pAlts opts
             <*> pSucceed False
     <|> Attr <$> pATTR
              <*> pOptClassContext
              <*> pNontSet
              <*> pAttrs opts
     <|> Type <$> pTYPE
              <*> pOptClassContext
              <*> pIdentifierU
              <*> pList pIdentifier
              <*  pEquals
              <*> pComplexType opts
     <|> Sem  <$> pSEM
              <*> pOptClassContext
              <*> pNontSet
              <*> pOptAttrs opts
              <*> pSemAlts opts
     <|> Set  <$> pSET
              <*> pIdentifierU
              <*  pEquals
              <*> pNontSet
     <|> Deriving
              <$> pDERIVING
              <*> pNontSet
              <*  pColon
              <*> pListSep pComma pIdentifierU
     <|> Wrapper
              <$> pWRAPPER
              <*> pNontSet
     <|> Pragma
              <$> pPRAGMA
              <*> pNames
     <|> Module
              <$> pMODULE
              <*> pCodescrap'
              <*> pCodescrap'
              <*> pCodescrap'
     <|> codeBlock <$> (pIdentifier <|> pSucceed (Ident "" noPos)) <*> ((Just <$ pATTACH <*> pIdentifierU) <|> pSucceed Nothing) <*> pCodeBlock <?> "a statement"
           where codeBlock nm mbNt (txt,pos) = Txt pos nm mbNt (lines txt)


-- Insertion is expensive for pCodeBlock in order to prevent infinite inserts.
pCodeBlock ::  AGParser (String,Pos)
pCodeBlock   = pCostValToken 90 TkTextln "" <?> "a code block"


pOptClassContext :: AGParser ClassContext
pOptClassContext
  =   pClassContext <* pDoubleArrow
  <|> pSucceed []

pClassContext :: AGParser ClassContext
pClassContext
  = pListSep pComma ((,) <$> pIdentifierU <*> pList pTypeHaskellAnyAsString)

pAttrs :: Options -> AGParser Attrs
pAttrs opts
    = Attrs <$> pOBrackPos <*> (concat <$> pList (pInhAttrNames opts) <?> "inherited attribute declarations")
                              <* pBar    <*> (concat <$> pList (pAttrNames opts) <?> "chained attribute declarations"  )
                              <* pBar    <*> (concat <$> pList (pAttrNames opts) <?> "synthesised attribute declarations"  )
               <*  pCBrack
       <|> (\ds -> Attrs (fst $ head ds) [n | (_,Left nms) <- ds, n <- nms] [] [n | (_,Right nms) <- ds, n <- nms]) <$> pList1 (pSingleAttrDefs opts)

pSingleAttrDefs :: Options -> AGParser (Pos, Either AttrNames AttrNames)
pSingleAttrDefs opts
  =    (\p is -> (p, Left is))  <$> pINH <*> pList1Sep pComma (pSingleInhAttrDef opts)
  <|>  (\p is -> (p, Right is)) <$> pSYN <*> pList1Sep pComma (pSingleSynAttrDef opts)

pSingleInhAttrDef :: Options -> AGParser (Identifier,Type,(String,String,String))
pSingleInhAttrDef opts
  = (\v tp -> (v,tp,("","",""))) <$> pIdentifier <* pTypeColon opts <*> pType <?> "inh attribute declaration"

pSingleSynAttrDef :: Options -> AGParser (Identifier,Type,(String,String,String))
pSingleSynAttrDef opts
  = (\v u tp -> (v,tp,u)) <$> pIdentifier <*> pUse <* pTypeColon opts <*> pType <?> "syn attribute declaration"

pOptAttrs :: Options -> AGParser Attrs
pOptAttrs opts = pAttrs opts `opt` Attrs noPos [] [] []

pTypeNt :: AGParser Type
pTypeNt
  =   ((\nt -> NT nt []) <$> pIdentifierU <?> "nonterminal name (no brackets)")
  <|> (pParens (NT <$> pIdentifierU <*> pList pTypeHaskellAnyAsString) <?> "nonterminal name with parameters (using parenthesis)")

pTypeHaskellAnyAsString :: AGParser String
pTypeHaskellAnyAsString
  =   getName <$> pIdentifier
  <|> getName <$> pIdentifierU
  <|> pCodescrap' <?> "a type"

-- if the type is within some kind of parentheses or brackets (then we allow lowercase identifiers as well)
pTypeEncapsulated :: AGParser Type
pTypeEncapsulated
  =   pParens pTypeEncapsulated
  <|> NT <$> pIdentifierU <*> pList pTypeHaskellAnyAsString
  <|> (Haskell . getName) <$> pIdentifier
  <|> pTypePrimitive

pTypePrimitive :: AGParser Type
pTypePrimitive
  = Haskell <$> pCodescrap'  <?> "a type"

pType :: AGParser Type
pType =  pTypeNt
     <|> pTypePrimitive


pInhAttrNames :: Options -> AGParser AttrNames
pInhAttrNames opts
               = (\vs tp -> map (\v -> (v,tp,("","",""))) vs)
                  <$> pIdentifiers <*  pTypeColon opts <*> pType <?> "attribute declarations"

pIdentifiers :: AGParser [Identifier]
pIdentifiers =  pList1Sep pComma pIdentifier <?> "lowercase identifiers"


pAttrNames :: Options -> AGParser AttrNames
pAttrNames opts 
         = (\vs use tp -> map (\v -> (v,tp,use)) vs)
             <$> pIdentifiers <*> pUse <* pTypeColon opts <*> pType <?> "attribute declarations"

pUse :: AGParser (String,String,String)
pUse = (  (\u x y->(x,y,show u)) <$> pUSE <*> pCodescrap'  <*> pCodescrap')` opt` ("","","") <?> "USE declaration"

pAlt :: Options -> AGParser Alt
pAlt opts =  Alt <$> pBar <*> pSimpleConstructorSet <*> pFields opts <?> "a datatype alternative"

pAlts :: Options -> AGParser Alts
pAlts opts =  pList_ng (pAlt opts) <?> "datatype alternatives"

pFields    :: Options -> AGParser Fields
pFields opts = concat <$> pList_ng (pField opts) <?> "fields"

pField     :: Options -> AGParser Fields
pField opts
        =  (\nms tp -> map (flip (,) tp) nms)
           <$> pIdentifiers <* pTypeColon opts <*> pType
           <|> (\s -> [(Ident (mklower (getName s)) (getPos s) ,NT s [])]) <$> pIdentifierU

mklower :: String -> String
mklower (x:xs) = toLower x : xs
mklower []     = []

pSemAlt :: Options -> AGParser SemAlt
pSemAlt opts = SemAlt <$> pBar <*> pConstructorSet <*> pSemDefs opts <?> "SEM alternative"

pSimpleConstructorSet :: AGParser ConstructorSet
pSimpleConstructorSet =  CName <$> pIdentifierU
                     <|> CAll  <$  pStar
                     <|> pParens pConstructorSet

pConstructorSet :: AGParser ConstructorSet
pConstructorSet =  pChainl (CDifference <$ pMinus) term2
  where term2 =  pChainr (pSucceed CUnion) term1
        term1 =  CName <$> pIdentifierU
             <|> CAll  <$  pStar
               

pSemAlts :: Options -> AGParser SemAlts
pSemAlts opts =  pList (pSemAlt opts) <?> "SEM alternatives"

pFieldIdentifier :: AGParser Identifier
pFieldIdentifier =  pIdentifier
                <|> Ident "lhs"  <$> pLHS
                <|> Ident "loc"  <$> pLOC
                <|> Ident "inst" <$> pINST

pSemDef :: Options -> AGParser [SemDef]
pSemDef opts
      =   (\x fs -> map ($ x) fs)<$> pFieldIdentifier <*> pList1 (pAttrDef opts)
      <|>                            pLOC              *> pList1 (pLocDecl opts)
      <|>                            pINST             *> pList1 (pInstDecl opts)
      <|>  pSEMPRAGMA *> pList1 (SemPragma <$> pNames)
      <|> (\a b -> [AttrOrderBefore a [b]]) <$> pList1 pAttr <* pSmaller <*> pAttr
      <|> (\pat owrt exp -> [Def (pat ()) exp owrt]) <$> pPattern (const <$> pAttr) <*> pAssign <*> pExpr opts
 
pAttr = (,) <$> pFieldIdentifier <* pDot <*> pIdentifier
 
pAttrDef :: Options -> AGParser (Identifier -> SemDef)
pAttrDef opts
  = (\pat owrt exp fld -> Def (pat fld) exp owrt)
           <$ pDot <*> pattern <*> pAssign <*> pExpr opts
  where pattern =  pPattern pVar
               <|> (\ir a fld -> ir $ Alias fld a (Underscore noPos) []) <$> ((Irrefutable <$ pTilde) `opt` id) <*> pIdentifier


nl2sp :: Char -> Char
nl2sp '\n' = ' '
nl2sp '\r' = ' '
nl2sp x = x

pLocDecl :: Options -> AGParser SemDef
pLocDecl opts = pDot <**> (pIdentifier <**> (pTypeColon opts <**> (   (\tp _ ident _  -> TypeDef ident tp) <$> pLocType
                                                                  <|> (\ref _ ident _ -> UniqueDef ident ref) <$ pUNIQUEREF <*> pIdentifier )))

pLocType = (Haskell . getName) <$> pIdentifierU
       <|> Haskell <$> pCodescrap'  <?> "a type"

pInstDecl :: Options -> AGParser SemDef
pInstDecl opts = (\ident tp -> TypeDef ident tp)
                  <$ pDot <*> pIdentifier <* pTypeColon opts <*> pTypeNt

pSemDefs :: Options -> AGParser SemDefs
pSemDefs opts =  concat <$> pList_ng (pSemDef opts) <?> "attribute rules"

pVar :: AGParser (Identifier -> (Identifier, Identifier))
pVar = (\att fld -> (fld,att)) <$> pIdentifier

 
pExpr :: Options -> AGParser Expression
pExpr opts = (\(str,pos) ->  Expression pos (lexTokens pos str)) <$> pCodescrapL opts <?> "an expression"

pAssign :: AGParser Bool
pAssign =  False <$ pReserved "="
       <|> True  <$ pReserved ":="
       
-- pAttrDefs :: Options -> AGParser (Identifier -> [SemDef])
-- pAttrDefs opts = (\fs field -> map ($ field) fs) <$> pList1 (pAttrDef opts) <?> "attribute definitions"

pPattern :: AGParser (a -> (Identifier,Identifier)) -> AGParser (a -> Pattern)
pPattern pvar = pPattern2 where
  pPattern0 =  (\i pats a -> Constr i (map ($ a) pats))
               <$> pIdentifierU <*> pList  pPattern1
               <|> pPattern1 <?> "a pattern"
  pPattern1 =  pvariable
           <|> pPattern2
  pvariable = (\ir var pat a -> case var a of (fld,att) -> ir $ Alias fld att (pat a) []) 
           <$> ((Irrefutable <$ pTilde) `opt` id) <*> pvar <*> ((pAt *> pPattern1) `opt` const (Underscore noPos)) 
  pPattern2 = (mkTuple <$> pOParenPos <*> pListSep pComma pPattern0 <* pCParen )
          <|> (const . Underscore) <$> pUScore <?> "a pattern"
    where mkTuple _ [x] a = x a
          mkTuple p xs  a = Product p (map ($ a) xs)

pCostSym' c t = pCostSym c t t

pCodescrap' ::  AGParser String
pCodescrap' = fst <$> pCodescrap

pCodescrap ::  AGParser (String,Pos)
pCodescrap   = pCodeBlock

pTypeColon :: Options -> AGParser Pos
pTypeColon opts
  = if doubleColons opts
    then pDoubleColon
    else pColon


pSEM, pATTR, pDATA, pUSE, pLOC,pINCLUDE, pTYPE, pEquals, pColonEquals, pTilde,
      pBar, pColon, pLHS,pINST,pSET,pDERIVING,pMinus,pIntersect,pDoubleArrow,pArrow,
      pDot, pUScore, pEXT,pAt,pStar, pSmaller, pWRAPPER, pPRAGMA, pMAYBE, pEITHER, pMAP, pINTMAP,
      pMODULE, pATTACH, pUNIQUEREF, pINH, pSYN
      :: AGParser Pos
pSET         = pCostReserved 90 "SET"     <?> "SET"
pDERIVING    = pCostReserved 90 "DERIVING"<?> "DERIVING"
pWRAPPER     = pCostReserved 90 "WRAPPER" <?> "WRAPPER"
pPRAGMA      = pCostReserved 90 "PRAGMA"  <?> "PRAGMA"
pSEMPRAGMA   = pCostReserved 90 "SEMPRAGMA" <?> "SEMPRAGMA"
pATTACH      = pCostReserved 90 "ATTACH"  <?> "ATTACH"
pDATA        = pCostReserved 90 "DATA"    <?> "DATA"
pEXT         = pCostReserved 90 "EXT"     <?> "EXT"
pATTR        = pCostReserved 90 "ATTR"    <?> "ATTR"
pSEM         = pCostReserved 90 "SEM"     <?> "SEM"
pINCLUDE     = pCostReserved 90 "INCLUDE" <?> "INCLUDE"
pTYPE        = pCostReserved 90 "TYPE"    <?> "TYPE"
pINH         = pCostReserved 90 "INH"     <?> "INH"
pSYN         = pCostReserved 90 "SYN"     <?> "SYN"
pMAYBE       = pCostReserved 5  "MAYBE"   <?> "MAYBE"
pEITHER      = pCostReserved 5  "EITHER"  <?> "EITHER"
pMAP         = pCostReserved 5  "MAP"     <?> "MAP"
pINTMAP      = pCostReserved 5  "INTMAP"  <?> "INTMAP"
pUSE         = pCostReserved 5  "USE"     <?> "USE"
pLOC         = pCostReserved 5  "loc"     <?> "loc"
pLHS         = pCostReserved 5  "lhs"     <?> "loc"
pINST        = pCostReserved 5  "inst"    <?> "inst"
pAt          = pCostReserved 5  "@"       <?> "@"
pDot         = pCostReserved 5  "."       <?> "."
pUScore      = pCostReserved 5  "_"       <?> "_"
pColon       = pCostReserved 5  ":"       <?> ":"
pDoubleColon = pCostReserved 5  "::"      <?> "::"
pEquals      = pCostReserved 5  "="       <?> "="
pColonEquals = pCostReserved 5  ":="      <?> ":="
pTilde       = pCostReserved 5  "~"       <?> "~"
pBar         = pCostReserved 5  "|"       <?> "|"
pIntersect   = pCostReserved 5  "/\\"     <?> "/\\"
pMinus       = pCostReserved 5  "-"       <?> "-"
pDoubleArrow = pCostReserved 5  "=>"      <?> "=>"
pArrow       = pCostReserved 5  "->"      <?> "->"
pStar        = pCostReserved 5  "*"       <?> "*"
pSmaller     = pCostReserved 5  "<"       <?> "<"
pMODULE      = pCostReserved 5  "MODULE"  <?> "MODULE"
pUNIQUEREF   = pCostReserved 5  "UNIQUEREF" <?> "UNIQUEREF"

