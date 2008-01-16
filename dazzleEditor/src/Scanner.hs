module Scanner where

import CommonTypes
import qualified Data.Map as Map
import LayLayerTypes
import LayLayerUtils

import DocTypes_Generated (Node (..))




scanner :: Int -> Layout doc Node clip ->
           (Presentation doc Node clip token, WhitespaceMap, Int)
scanner i pres = -- debug Err (show pres) $
            tokenize LexHaskell -- default is Haskell scanner
                     i Nothing pres

{-
 Redesigning this module has a high priority. The differences between haskell and free text lexers are are
 implemented in an ad hoc way. Only for strings, a distinction is made, and in tokenizeCol'. Other presentations
 besides strings in the free text lexer are not handled correctly.
 
 Moreover, it is not doable to extend the current implementation for storing focus in the tokens.




 tokenize on formatters only records spaces, but no line-endings
 
 
 tokenize for free text traverses the entire parsing subtree and creates tokens for:
   spaces
   newlines
   sequences of non-whitespace characters
   structural subpresentations
   
   NB similar to Haskell scanner, spaces after newlines are ignored because whitespace is represented 
   by (newlines, spaces) in the scanner.


 Scanner works together with postScanPres in PresentationParsing. postScanPres converts all strings
 into actual tokens (according to 'keywords' in the presentation sheet) and removes empty string tokens.
 
 When Presentation has a special constructor TokenP, the scanner can do all these things and make postScanPres
 obsolete. (although some processing is needed to convert the presentation with presentation and parsing parts to the
 structuralToken/ list of tokens tree structure that the parser needs.
-}

-- tokenize traverses the structural parts of the presentation tree. On the parsing branches,
-- tokenize' is called.
tokenize :: Lexer -> Int -> Maybe Node -> Layout doc Node clip ->
            (Presentation doc Node clip token, WhitespaceMap, Int)
-- tokenize _ pres = (pres, [])       -- skip tokenize
tokenize lx i loc (ParsingP id lx' pres)  = 
 let lex = case lx' of
             LexInherited -> lx
             _            -> lx'
     (lc, layout, id, str, tokens, lm, i') = tokenize' lex i (loc,Prelude.id) (debug Err "Undefined token used" IntToken,Nothing, Prelude.id) (0,0) NoIDP "" pres
    
 in --debug Lay ("tokenize with lexer "++ show lex ++" on " ++show pres) $
    case lex of
      LexHaskell ->
        let   (tok, lm', i'') = makeToken i' lc layout id str

        in  (ParsingP id lx' $ RowP NoIDP 0 $ (tokens++[tok]), lm `Map.union` lm', i'')
      LexFreeText ->
        let   (toks, i'') = makeTokenFree i' lc layout id str
        in  (ParsingP id lx' $ RowP NoIDP 0 $ (tokens++toks), lm, i'')
tokenize lx i loc (EmptyP idd)           = (EmptyP idd, Map.empty, i)
tokenize lx i loc (StringP idd str)      = (StringP idd str, Map.empty, i)
tokenize lx i loc pres@(ImageP idd istr st)         = (ImageP idd istr st, Map.empty, i)
tokenize lx i loc pres@(PolyP idd pts w st)        = (PolyP idd pts w st, Map.empty, i)
tokenize lx i loc pres@(RectangleP idd w h lw st) = (RectangleP idd w h lw st, Map.empty, i)
tokenize lx i loc pres@(EllipseP idd w h lw st) = (EllipseP idd w h lw st, Map.empty, i)
tokenize lx i loc (RowP id rf press) = let (press', lm, i') = tokenizeLst lx i loc press
                                    in  (RowP id rf press', lm, i')
tokenize lx i loc (ColP id rf f press) = let (press', lm, i') = tokenizeLst lx i loc press
                                      in  (ColP id rf f press', lm, i')
tokenize lx i loc (OverlayP id (pres:press)) = let (pres', lm, i') = tokenize lx i loc pres
                                            in  (OverlayP id (pres' : map castLayToPres press), lm, i')
tokenize lx i loc (WithP ar pres)            = let (pres', lm, i') = tokenize lx i loc pres
                                            in  (WithP ar pres', lm, i')
tokenize lx i loc (StructuralP id pres)      = let (pres', lm, i') = tokenize lx i loc pres
                                            in  (StructuralP id pres', lm, i')
tokenize lx i _   (LocatorP loc pres)        = let (pres', lm, i') = tokenize lx i (Just loc) pres
                                            in  (LocatorP loc pres', lm, i')
tokenize lx i loc (GraphP id d w h edges press) = let (press', lm, i') = tokenizeLst lx i loc press
                                               in  (GraphP id d w h edges press', lm, i')
tokenize lx i loc (VertexP id v x y o pres)     = let (pres', lm, i') = tokenize lx i loc pres
                                               in  (VertexP id v x y o pres', lm, i')              
tokenize lx i loc (FormatterP id press) = let (press', lm, i') = tokenizeLst lx i loc press
                                    in  (FormatterP id press', lm, i')
tokenize lx i loc pr = debug Err ("Scanner.tokenize: can't handle "++ show pr) (castLayToPres pr, Map.empty, i)

tokenizeLst lx i loc []           = ([], Map.empty, i)
tokenizeLst lx i loc (pres:press) = let (pres', lm0, i') = tokenize lx i loc pres
                                        (press', lm1, i'') = tokenizeLst lx i' loc press
                                    in  (pres':press', lm1 `Map.union` lm0, i'')
-- tokenize is tricky. The information for the token that is being built is threaded through the computation
-- (lc layout id str), but also the current location (/= the location of the first char of the current token) 
-- and for tokenizeStr, the id of the string (/= the id of the first character of the token) have to be passed
-- although these are just inherited, not threaded

-- What happens if new pres id's are used. This would greatly simplify the scanning process




-- TODO: use AG
-- when integrating, take care that:
-- if string has no idd, but empty "" before it has, then idd is reused from ""
-- when setting loc, don't do anything for String_, Int_, etc.




-- at token start, we set the id from the context, but if there is none, then we use the id that
-- was set during the layout part of the token. Do we need to do this for loc as well?

-- CONFLICT: if token start has no id, whitespace id is used, but what if rest of token has id?
-- so [id 1: "  "] "a" [id 2: "bc"]??  This will now be   (id 1 "abc"), but (id 2: abc) is probably better
-- if abc came from inserting a b in front. 

-- Locators will cause similar problems as id's when copied

-- TODO: make the tuple into a data structure
data TokenType = IntToken | LowerIdentToken | UpperIdentToken | SymbolToken | OperatorToken  
--tokenType 'l'                = OperatorToken
tokenType c | isDigit c      = IntToken
            | isLower c      = LowerIdentToken
            | isUpper c      = UpperIdentToken
            | isSymbolChar c = SymbolToken
            | otherwise      = OperatorToken

symbolChars = ";,(){}[]"
identChars  = "_'"
isIdentChar c = isAlphaNum c || elem c identChars
isSymbolChar c = c `elem` symbolChars


-- tokenizeString chooses either the Haskell like tokenization, or a free text tokenization
-- in which each kind of whitespace gets its own token, and text as well: "  bla\n" -> [" "," ", "bla", "\n"]
tokenizeString :: Lexer -> Int -> (Maybe node, AttrRule doc clip) -> IDP ->
               (TokenType, Maybe node, AttrRule doc clip) -> Whitespace -> IDP -> String -> String ->
               ( (TokenType, Maybe node, AttrRule doc clip), Whitespace, IDP, String
               , [Presentation doc node clip token], WhitespaceMap, Int )
tokenizeString LexHaskell = tokenizeStr
tokenizeString LexFreeText = tokenizeStrFree




tokenizeStr :: Int -> (Maybe node, AttrRule doc clip) -> IDP -> 
               (TokenType, Maybe node, AttrRule doc clip) -> Whitespace -> IDP -> String -> String ->
               ( (TokenType, Maybe node, AttrRule doc clip), Whitespace, IDP, String
               , [Presentation doc node clip token], WhitespaceMap, Int )
-- end of string, not started scanning a token, so set the id (only if cid non-empty)
tokenizeStr i loc cid lc layout id "" "" = (lc, layout, if cid == NoIDP then id else cid, "", [], Map.empty, i)

-- end of string while scanning a token, so keep token's id
tokenizeStr i loc cid lc layout id str "" = (lc, layout, id, str, [], Map.empty, i)

-- white space, not started scanning a token, so set the id (if non-empty)
tokenizeStr i loc cid lc (brks,spcs) id ""  (' ':cs) = tokenizeStr i loc cid undefTk (brks,spcs+1) (if cid == NoIDP then id else cid) "" cs --2nd loc is not used

-- white space, while started scanning a token, make token and set id for new scan (if cid empty, still use it, because id is alread used for token)
tokenizeStr i loc cid lc layout id str (' ':cs) =            -- do we want this? See **
  let (lc', layout', id', str', tokens', lm, i') = tokenizeStr i loc cid undefTk (0,1) cid "" cs   -- 2nd loc is not used
      (tok, lm', i'') = makeToken i' lc layout id str           
  in  (lc', layout', id', str', tok : tokens', lm `Map.union` lm', i'')

-- first character of token. if there is no Id, use the one from the whitespace, also clear this strings id because it is used now
tokenizeStr i loc@(l,ar) cid lc layout id ""  (c:cs)   = tokenizeStr i loc NoIDP (tokenType c, l,ar) layout (if cid == NoIDP then id else cid) [c] cs
-- scanning string                         
tokenizeStr i loc@(l,ar) cid lc@(tt,_,_) layout id (tc:tcs) (c:cs) = 
  case tt of 
    IntToken -> if isDigit c then tokenizeStr i loc cid lc layout id (c:tc:tcs) cs
                             else startNewToken
    LowerIdentToken -> if isIdentChar c then tokenizeStr i loc cid lc layout id (c:tc:tcs) cs
                                        else startNewToken
    UpperIdentToken -> if isIdentChar c then tokenizeStr i loc cid lc layout id (c:tc:tcs) cs
                                        else startNewToken
    SymbolToken -> startNewToken
    OperatorToken -> if not (isAlphaNum c) {-int and idents -}&& not (isSymbolChar c)
                     then tokenizeStr i loc cid lc layout id (c:tc:tcs) cs
                     else startNewToken  
  --                                                       The strings id is cleared anyway, because this token has used it now
  where startNewToken =
          let (loc', layout', id', str', tokens', lm, i') = tokenizeStr i loc NoIDP (tokenType c,l,ar) (0,0) cid [c] cs
              (tok, lm', i'') = makeToken i' lc layout id (tc:tcs)
          in  (loc', layout', id', str', tok : tokens', lm `Map.union` lm', i'')

--(isAlpha tc && isAlpha c) || (isNum tc && isNum c) || 

tokenizeRow' lx i loc lc layout id str []     = (lc, layout, id, str, [], Map.empty, i)
tokenizeRow' lx i loc lc layout id str (pres:press) = let (lc', layout', id', str', tokens0, lm0, i') = tokenize' lx i loc lc layout id str pres
                                                          (lc'', layout'', id'', str'', tokens1, lm1, i'') = tokenizeRow' lx i' loc lc' layout' id' str' press
                                                      in  (lc'', layout'', id'', str'', tokens0++tokens1, lm1 `Map.union` lm0, i'')


-- add case for [pres]
-- make token here? what about one line column, don't want to tokenize, but more line column, we do, probably
tokenizeCol' lx i loc lc layout id str []     = (lc, layout, id, str, [], Map.empty, i)
--tokenizeCol' i loc lc layout id str (pres:press) = let (lc', layout', id', str', tokens0, lm0,i') = tokenize' i loc lc layout id str pres
--                                                       (lc'', layout'', id'', str'', tokens1, lm1,i'') = tokenizeCol' i' loc lc' layout' id' str' press
--                                                   in  (lc'', layout'', id'', str'', tokens0++tokens1, lm0++lm1, i'')
tokenizeCol' lx i loc lc layout id str (pres:press) = 
  let (lc', (brks,spcs), id', str', tokens0, lm0,i') = tokenize' lx i loc lc layout id str pres
      lineBreaks = if null press then 0 else 1 -- only add a line break between lines in a column, not at the end
  in  if null str' then -- still collecting whitespace
        let (lc'', layout'', id'', str'', tokens1, lm1,i'') = tokenizeCol' lx i' loc lc' (brks+lineBreaks,0) id' str' press
        in  (lc'', layout'', id'', str'', tokens0++tokens1, lm1 `Map.union` lm0, i'')
      else
        let (tok, lm, i'') = case lx of 
               LexHaskell  -> let (t,l,id'') = makeToken i' lc' (brks,spcs) id' str'
                              in ([t],l,id'')
               LexFreeText -> let (ts,id'') = makeTokenFree i' lc' (brks,spcs) id' str'
                              in (ts,Map.empty,id'')
            (lc'', layout'', id'', str'', tokens1, lm1,i''') = tokenizeCol' lx i'' loc undefTk (lineBreaks,0) NoIDP "" press
        in  (lc'', layout'', id'', str'', tokens0++tok++tokens1, lm `Map.union` lm0 `Map.union` lm1, i''')


-- loc is threaded, lc is just inherited. 
tokenize' :: Lexer -> Int -> (Maybe Node, AttrRule doc clip) -> (TokenType, Maybe Node, AttrRule doc clip) ->
             Whitespace -> IDP -> String -> Layout doc Node clip ->
             ( (TokenType, Maybe Node, AttrRule doc clip), Whitespace, IDP, String
             , [Presentation doc Node clip token], WhitespaceMap, Int)
tokenize' lx i loc lc layout id str (EmptyP _) = (lc, layout, id, str, [], Map.empty, i)
tokenize' lx i loc lc layout id str (StringP cid str') = tokenizeString lx i loc cid lc layout id str str'
tokenize' lx i loc lc layout id str (ImageP idd istr st)       = let (tok, lm, i') = makeToken i lc layout id str
                                                                 in  (undefTk, (0,0), NoIDP, "", [tok,ImageP idd istr st],lm, i')
tokenize' lx i loc lc layout id str (PolyP idd pts w st)      = let (tok, lm, i') = makeToken i lc layout id str
                                                                in  (undefTk, (0,0), NoIDP, "", [tok,PolyP id pts w st],lm, i')
tokenize' lx i loc lc layout id str (RectangleP idd w h lw st) = let (tok, lm, i') = makeToken i lc layout id str
                                                                in  (undefTk, (0,0), NoIDP, "", [tok,RectangleP idd w h lw st],lm, i')
tokenize' lx i loc lc layout id str (EllipseP idd w h lw st)   = let (tok, lm, i') = makeToken i lc layout id str
                                                               in  (undefTk, (0,0), NoIDP, "", [tok,EllipseP idd w h lw st],lm, i')
tokenize' lx i loc lc layout id str (GraphP idd d w h es press) = let (tok, lm, i') = makeToken i lc layout id str
                                                             in  (undefTk, (0,0), NoIDP, "", [tok,GraphP idd d w h es (map castLayToPres press)],lm, i')
tokenize' lx i loc lc layout id str (VertexP idd v x y ol pres) = let (tok, lm, i') = makeToken i lc layout id str  -- Unclear: should we recursively tokenize for graph & vertex?
                                                              in  (undefTk, (0,0), NoIDP, "", [tok,VertexP idd v x y ol (castLayToPres pres)],lm, i')
tokenize' lx i loc lc layout id str pres@(FormatterP _ press) = tokenizeRow'  lx i loc lc layout id str press
tokenize' lx i (loc,ar) lc layout id str (WithP ar' pres)       = tokenize' lx i (loc,ar'.ar) lc layout id str pres
tokenize' lx i loc lc layout id str (OverlayP _ [])      = (lc, layout, id, str, [],Map.empty,i)
tokenize' lx i loc lc layout id str (OverlayP _ (pres:press)) = tokenize' lx i loc lc layout id str pres
tokenize' lx i loc lc layout id str (ColP _ _ _ press)   = tokenizeCol' lx i loc lc layout id str press
tokenize' lx i loc lc layout id str (RowP _ _ press)     = tokenizeRow' lx i loc lc layout id str press
tokenize' lx i loc lc layout id str (LocatorP (String_Node _ _) pres)  = tokenize' lx i loc lc layout id str pres
tokenize' lx i loc lc layout id str (LocatorP (HoleString_Node _ _) pres)  = tokenize' lx i loc lc layout id str pres -- hole also necessary?
tokenize' lx i loc lc layout id str (LocatorP (Int_Node _ _) pres)  = tokenize' lx i loc lc layout id str pres
tokenize' lx i loc lc layout id str (LocatorP (HoleInt_Node _ _) pres)  = tokenize' lx i loc lc layout id str pres -- hole also necessary?
tokenize' lx i loc lc layout id str (LocatorP (Bool_Node _ _) pres)  = tokenize' lx i loc lc layout id str pres
tokenize' lx i loc lc layout id str (LocatorP (HoleBool_Node _ _) pres)  = tokenize' lx i loc lc layout id str pres -- hole also necessary?
tokenize' lx i (_,ar)   lc layout id str (LocatorP loc pres)  = tokenize' lx i (Just loc,ar) lc layout id str pres
tokenize' lx i loc lc layout id str (ParsingP _ LexInherited pres)    = tokenize' lx i loc lc layout id str pres
tokenize' _  i loc lc layout id str (ParsingP _ lx pres)    = tokenize' lx i loc lc layout id str pres
tokenize' lx i (Nothing,ar) lc layout id "" (StructuralP id' pres)    =
  let (pres', lm1, i'') = tokenize lx i Nothing pres
  in  (undefTk, (0,0), NoIDP, "", [StructuralP id' $ pres'], 
       lm1 `Map.union` (if id' == NoIDP then Map.empty else Map.singleton id' layout),i'')
tokenize' lx i (Just loc,ar) lc layout id "" (StructuralP id' pres) =
  let (pres', lm1, i'') = tokenize lx i (Just loc) pres
  in  (undefTk, (0,0), NoIDP, "", [LocatorP loc $ StructuralP id' $ pres'],
       lm1 `Map.union` (if id' == NoIDP then Map.empty else Map.singleton id' layout),i'')
tokenize' lx i (Nothing,ar) lc layout id str (StructuralP id' pres)    =
  let (tok, lm0, i') = makeToken i lc layout id str
      (pres', lm1, i'') = tokenize lx i' Nothing pres
  in  (undefTk, (0,0), NoIDP, "", [tok, StructuralP id' $ pres'], lm1 `Map.union` lm0,i'')
tokenize' lx i (Just loc,ar) lc layout id str (StructuralP id' pres) =
  let (tok, lm0, i') = makeToken i lc layout id str
      (pres', lm1, i'') = tokenize lx i' (Just loc) pres
  in  (undefTk, (0,0), NoIDP, "", [tok, LocatorP loc $ StructuralP id' $ pres'], lm1 `Map.union` lm0,i'')
tokenize' lx i loc lc layout id str pres        = debug Err ("*** PresentationParser.walk: unimplemented presentation: " ++ show pres) (lc, layout, id, str, [], Map.empty, i)

undefTk = (debug Err "Undefined token used" IntToken,Nothing,Prelude.id)

-- can't use tokentype here, for that we need a special Presentation element TokenP that can hold the information.
-- for now the type is only used to scan correctly, which is impossible without keeping track of the token type
-- (or we have to inspect the token every time we read a character)
makeToken :: Int -> (TokenType, Maybe node,AttrRule doc clip) -> Whitespace -> IDP -> String ->
             (Presentation doc node clip token, WhitespaceMap,Int)
makeToken i l             layout NoIDP str = makeToken (i+1) l layout (IDP i) str  -- make new ID
makeToken i (_,Nothing,ar)  layout id str = (WithP ar $ StringP id (reverse str), Map.singleton id layout,i)
makeToken i (_,Just loc,ar) layout id str = (LocatorP loc $ WithP ar $ StringP id (reverse str), Map.singleton id layout,i)
-- TODO: For no location, maybe a NoNode should be used as locator? Now postScanPres

-- all makeTokens must update lm
-- noids in img etc are wrong


-- attrRule is also about Synthesized, is this a problem?


-- ** case means that {0:"12"} {1:"3 "} {"+"} is scanned as {0:"123"} {1:"+"} Maybe this is not what we want

-- PROBLEM: {0:"12"} {1:""} {"3+"} loses the 1 and gives {0:"123"} {"+"}

-- RETHINK IDENTITY BUSINESS when to recover them, what's the semantics of paste & delete, etc.


-- remove deleted tokens from presentation: 
-- Probably this will be done with some kind of marker (like StructuralP and ParsingP) in
-- a future version. Now we just look in local state of deleted tokens.
-- Editing in deleted tokens will not work.







-- tokenizeStrFree is crudely derived from tokenizedStr. It is only temporary, since an improved scanner
-- will allow different tokenizations to be specified with regular expressions.

-- end of string, not started scanning a token, so set the id (only if cid non-empty)
tokenizeStrFree i loc cid lc layout id "" "" = (lc, layout, if cid == NoIDP then id else cid, "", [], Map.empty, i)

-- end of string while scanning a token, so keep token's id
tokenizeStrFree i loc cid lc layout id str "" = (lc, layout, id, str, [], Map.empty, i)

-- white space, not started scanning a token, so set the id (if non-empty)
tokenizeStrFree i loc cid lc (brks,spcs) id ""  (' ':cs) = tokenizeStrFree i loc cid undefTk (brks,spcs+1) (if cid == NoIDP then id else cid) "" cs --2nd loc is not used

-- white space, while started scanning a token, make token and set id for new scan (if cid empty, still use it, because id is alread used for token)
tokenizeStrFree i loc cid lc layout id str (' ':cs) =            -- do we want this? See **
  let (lc', layout', id', str', tokens', lm, i') = tokenizeStrFree i loc cid undefTk (0,1) cid "" cs   -- 2nd loc is not used
      (toks, i'') = makeTokenFree i' lc layout id str           
  in  (lc', layout', id', str', toks ++ tokens', lm, i'')

-- first character of token. if there is no Id, use the one from the whitespace, also clear this strings id because it is used now
tokenizeStrFree i loc@(l,ar) cid lc layout id ""  (c:cs)   = tokenizeStrFree i loc NoIDP (tokenType c, l,ar) layout (if cid == NoIDP then id else cid) [c] cs
-- scanning string                         
tokenizeStrFree i loc@(l,ar) cid lc@(tt,_,_) layout id (tc:tcs) (c:cs) = 
  tokenizeStrFree i loc cid lc layout id (c:tc:tcs) cs
  -- no distinction on characters, we continue to recognize the token until whitespace
  -- is encountered

-- layout is not stored, but returned as explicit tokens
makeTokenFree :: Int -> (TokenType, Maybe node,AttrRule doc clip) -> Whitespace -> IDP -> String ->
             ([Presentation doc node clip token],Int)
makeTokenFree i l             layout NoIDP str = makeTokenFree (i+1) l layout (IDP i) str  -- make new ID
makeTokenFree i (_,Nothing,ar) layout id str =debug Lay ("Making token with layout :"++show (layout, str)) $
  (map (WithP ar) $ whitespaceTokens layout ++ [ StringP id (reverse str) ],i)
makeTokenFree i (_,Just loc,ar) layout id str = debug Lay ("Making token with layout :"++show (layout, str)) $
  (map (LocatorP loc . WithP ar) $ whitespaceTokens layout ++[StringP id (reverse str)],i)


whitespaceTokens (newlines, spaces) = 
  replicate newlines (StringP NoIDP "\n") ++ replicate spaces (StringP NoIDP  " ")  
  

