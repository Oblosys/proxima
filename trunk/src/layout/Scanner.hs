module Scanner where

import CommonTypes
import LayLayerTypes
import LayLayerUtils

import DocTypes -- for Node


tokenize :: Int -> Maybe Node -> Presentation -> (Presentation, LayoutMap, Int)
-- tokenize _ pres = (pres, [])       -- skip tokenize
tokenize i loc (ParsingP id pres)  = let (lc, layout, id, str, tokens, lm, i') = tokenize' i (loc,Prelude.id) (debug Err "Undefined token used" IntToken,Nothing, Prelude.id) (0,0) NoIDP "" pres
                                         (tok, lm', i'') = makeToken i' lc layout id str
                                     in  (ParsingP id $ RowP NoIDP 0 $ (tokens++[tok]), lm' `plusFM` lm, i'')
tokenize i loc pres@(EmptyP _)           = (pres, emptyFM, i)
tokenize i loc pres@(StringP _ str)      = (pres, emptyFM, i)
tokenize i loc pres@(ImageP _ _)         = (pres, emptyFM, i)
tokenize i loc pres@(PolyP _ _ _)        = (pres, emptyFM, i)
tokenize i loc pres@(RectangleP _ _ _ _) = (pres, emptyFM, i)
tokenize i loc (RowP id rf press) = let (press', lm, i') = tokenizeLst i loc press
                                    in  (RowP id rf press', lm, i')
tokenize i loc (ColP id rf press) = let (press', lm, i') = tokenizeLst i loc press
                                    in  (ColP id rf press', lm, i')
tokenize i loc (OverlayP id (pres:press)) = let (pres', lm, i') = tokenize i loc pres
                                            in  (OverlayP id (pres' : press), lm, i')
tokenize i loc (WithP ar pres)            = let (pres', lm, i') = tokenize i loc pres
                                            in  (WithP ar pres', lm, i')
tokenize i loc (StructuralP id pres)      = let (pres', lm, i') = tokenize i loc pres
                                            in  (StructuralP id pres', lm, i')
tokenize i _   (LocatorP loc pres)        = let (pres', lm, i') = tokenize i (Just loc) pres
                                            in  (LocatorP loc pres', lm, i')
tokenize i loc pr = debug Err ("TreeEditPres.tokenize: can't handle "++ show pr) (pr, emptyFM, i)

tokenizeLst i loc []           = ([], emptyFM, i)
tokenizeLst i loc (pres:press) = let (pres', lm0, i') = tokenize i loc pres
                                     (press', lm1, i'') = tokenizeLst i' loc press
                                 in  (pres':press', lm0 `plusFM` lm1, i'')
-- tokenize is tricky. The information for the token that is being built is threaded through the computation
-- (lc layout id str), but also the current location (/= the location of the first char of the current token) 
-- and for tokenizeStr, the id of the string (/= the id of the first character of the token) have to be passed
-- although these are just inherited, not threaded

-- What happens if new pres id's are used. This would greatly simplify the scanning process




-- TODO: use AG

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
            | isUpper c      = LowerIdentToken
            | isSymbolChar c = SymbolToken
            | otherwise      = OperatorToken

symbolChars = ";,(){}[]"
identChars  = "_'"
isIdentChar c = isAlphaNum c || elem c identChars
isSymbolChar c = c `elem` symbolChars
 
tokenizeStr :: Int -> (Maybe Node,AttrRule) -> IDP -> (TokenType, Maybe Node,AttrRule) -> Layout -> IDP -> String -> String 
            -> ((TokenType, Maybe Node,AttrRule), Layout, IDP, String, [Presentation], LayoutMap, Int)

-- end of string, not started scanning a token, so set the id (only if cid non-empty)
tokenizeStr i loc cid lc layout id "" "" = (lc, layout, if cid == NoIDP then id else cid, "", [], emptyFM, i)

-- end of string while scanning a token, so keep token's id
tokenizeStr i loc cid lc layout id str "" = (lc, layout, id, str, [], emptyFM, i)

-- white space, not started scanning a token, so set the id (if non-empty)
tokenizeStr i loc cid lc (brks,spcs) id ""  (' ':cs) = tokenizeStr i loc cid undefTk (brks,spcs+1) (if cid == NoIDP then id else cid) "" cs --2nd loc is not used

-- white space, while started scanning a token, make token and set id for new scan (if cid empty, still use it, because id is alread used for token)
tokenizeStr i loc cid lc layout id str (' ':cs) =            -- do we want this? See **
  let (lc', layout', id', str', tokens', lm, i') = tokenizeStr i loc cid undefTk (0,1) cid "" cs   -- 2nd loc is not used
      (tok, lm', i'') = makeToken i' lc layout id str           
  in  (lc', layout', id', str', tok : tokens', lm' `plusFM` lm, i'')

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
          in  (loc', layout', id', str', tok : tokens', lm' `plusFM` lm, i'')

--(isAlpha tc && isAlpha c) || (isNum tc && isNum c) || 

tokenizeRow' i loc lc layout id str []     = (lc, layout, id, str, [], emptyFM, i)
tokenizeRow' i loc lc layout id str (pres:press) = let (lc', layout', id', str', tokens0, lm0, i') = tokenize' i loc lc layout id str pres
                                                       (lc'', layout'', id'', str'', tokens1, lm1, i'') = tokenizeRow' i' loc lc' layout' id' str' press
                                                   in  (lc'', layout'', id'', str'', tokens0++tokens1, lm0 `plusFM` lm1, i'')


-- add case for [pres]
-- make token here? what about one line column, don't want to tokenize, but more line column, we do, probably
tokenizeCol' i loc lc layout id str []     = (lc, layout, id, str, [], emptyFM, i)
--tokenizeCol' i loc lc layout id str (pres:press) = let (lc', layout', id', str', tokens0, lm0,i') = tokenize' i loc lc layout id str pres
--                                                       (lc'', layout'', id'', str'', tokens1, lm1,i'') = tokenizeCol' i' loc lc' layout' id' str' press
--                                                   in  (lc'', layout'', id'', str'', tokens0++tokens1, lm0++lm1, i'')
tokenizeCol' i loc lc layout id str (pres:press) = 
  let (lc', (brks,spcs), id', str', tokens0, lm0,i') = tokenize' i loc lc layout id str pres
  in  if null str' then -- still collecting whitespace
        let (lc'', layout'', id'', str'', tokens1, lm1,i'') = tokenizeCol' i' loc lc' (brks+1,0) id' str' press
        in  (lc'', layout'', id'', str'', tokens0++tokens1, lm0 `plusFM` lm1, i'')
      else
        let (tok, lm, i'') = makeToken i' lc' (brks,spcs) id' str'
            (lc'', layout'', id'', str'', tokens1, lm1,i''') = tokenizeCol' i'' loc undefTk (1,0) NoIDP "" press
        in  (lc'', layout'', id'', str'', tokens0++[tok]++tokens1, lm0 `plusFM` lm `plusFM` lm1, i''')

 

-- loc is threaded, lc is just inherited. 
tokenize' :: Int -> (Maybe Node,AttrRule) -> (TokenType,Maybe Node,AttrRule) -> Layout -> IDP -> String -> Presentation 
          -> ((TokenType,Maybe Node,AttrRule), Layout, IDP, String, [Presentation], LayoutMap, Int)
tokenize' i loc lc layout id str (EmptyP _) = (lc, layout, id, str, [], emptyFM, i)
tokenize' i loc lc layout id str (StringP cid str') = tokenizeStr i loc cid lc layout id str str'
tokenize' i loc lc layout id str pres@(ImageP _ _)         = let (tok, lm, i') = makeToken i lc layout id str
                                                             in  (undefTk, (0,0), NoIDP, "", [tok,pres],lm, i')
tokenize' i loc lc layout id str pres@(PolyP _ _ _)        = let (tok, lm, i') = makeToken i lc layout id str
                                                             in  (undefTk, (0,0), NoIDP, "", [tok,pres],lm, i')
tokenize' i loc lc layout id str pres@(RectangleP _ _ _ _) = let (tok, lm, i') = makeToken i lc layout id str
                                                             in  (undefTk, (0,0), NoIDP, "", [tok,pres],lm, i')
tokenize' i (loc,ar) lc layout id str (WithP ar' pres)       = tokenize' i (loc,ar'.ar) lc layout id str pres
tokenize' i loc lc layout id str (OverlayP _ [])      = (lc, layout, id, str, [],emptyFM,i)
tokenize' i loc lc layout id str (OverlayP _ (pres:press)) = tokenize' i loc lc layout id str pres
tokenize' i loc lc layout id str (ColP _ _ press)     = tokenizeCol' i loc lc layout id str press
tokenize' i loc lc layout id str (RowP _ _ press)     = tokenizeRow' i loc lc layout id str press
tokenize' i loc lc layout id str (LocatorP (String_Node _ _) pres)  = tokenize' i loc lc layout id str pres
tokenize' i loc lc layout id str (LocatorP (HoleString_Node _ _) pres)  = tokenize' i loc lc layout id str pres -- hole also necessary?
tokenize' i loc lc layout id str (LocatorP (Int_Node _ _) pres)  = tokenize' i loc lc layout id str pres
tokenize' i loc lc layout id str (LocatorP (HoleInt_Node _ _) pres)  = tokenize' i loc lc layout id str pres -- hole also necessary?
tokenize' i loc lc layout id str (LocatorP (Bool_Node _ _) pres)  = tokenize' i loc lc layout id str pres
tokenize' i loc lc layout id str (LocatorP (HoleBool_Node _ _) pres)  = tokenize' i loc lc layout id str pres -- hole also necessary?
tokenize' i (_,ar)   lc layout id str (LocatorP loc pres)  = tokenize' i (Just loc,ar) lc layout id str pres
tokenize' i loc lc layout id str (ParsingP _ pres)    = tokenize' i loc lc layout id str pres
tokenize' i (Nothing,ar) lc layout id "" (StructuralP id' pres)    =
  let (pres', lm1, i'') = tokenize i Nothing pres
  in  (undefTk, (0,0), NoIDP, "", [StructuralP id' $ pres'], 
       (if id' == NoIDP then emptyFM else unitFM id' layout) `plusFM` lm1,i'')
tokenize' i (Just loc,ar) lc layout id "" (StructuralP id' pres) =
  let (pres', lm1, i'') = tokenize i (Just loc) pres
  in  (undefTk, (0,0), NoIDP, "", [LocatorP loc $ StructuralP id' $ pres'],
       (if id' == NoIDP then emptyFM else unitFM id' layout) `plusFM` lm1,i'')
tokenize' i (Nothing,ar) lc layout id str (StructuralP id' pres)    =
  let (tok, lm0, i') = makeToken i lc layout id str
      (pres', lm1, i'') = tokenize i' Nothing pres
  in  (undefTk, (0,0), NoIDP, "", [tok, StructuralP id' $ pres'], lm0 `plusFM` lm1,i'')
tokenize' i (Just loc,ar) lc layout id str (StructuralP id' pres) =
  let (tok, lm0, i') = makeToken i lc layout id str
      (pres', lm1, i'') = tokenize i' (Just loc) pres
  in  (undefTk, (0,0), NoIDP, "", [tok, LocatorP loc $ StructuralP id' $ pres'], lm0 `plusFM` lm1,i'')
tokenize' i loc lc layout id str pres        = debug Err ("*** PresentationParser.walk: unimplemented presentation: " ++ show pres) (lc, layout, id, str, [], emptyFM, i)

undefTk = (debug Err "Undefined token used" IntToken,Nothing,Prelude.id)
-- can't use tokentype here, for that we need a special Presentation element TokenP that can hold the information.
-- for now the type is only used to scan correctly, which is impossible without keeping track of the token type
-- (or we have to inspect the token every time we read a character)
makeToken :: Int -> (TokenType, Maybe Node,AttrRule) -> Layout -> IDP -> String -> (Presentation, LayoutMap,Int)
makeToken i l             layout NoIDP str = makeToken (i+1) l layout (IDP i) str  -- make new ID
makeToken i (_,Nothing,ar)  layout id str = (WithP ar $ StringP id (reverse str), unitFM id layout,i)
makeToken i (_,Just loc,ar) layout id str = (LocatorP loc $ WithP ar $ StringP id (reverse str), unitFM id layout,i)

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


