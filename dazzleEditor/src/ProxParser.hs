module ProxParser (recognizeRootEnr) where

import CommonTypes hiding (Dirty (..))
import qualified CommonTypes
import PresLayerTypes
import PresLayerUtils hiding ((<*),(<*>),(<$),(<$>))
import PresentationParsing
import XprezLib

import UU_Parsing hiding (Exp, parse)

import List hiding (delete)


import ProxParser_Generated
import DocumentEdit
import DocumentEdit_Generated

import qualified UU_Parsing
import Char

import DocTypes_Generated
import DocUtils_Generated

              
-------------------- Proxima Parser/Structure Recognizer -------------------- 

recognizeRootEnr :: ListParser Document Node ClipDoc UserToken EnrichedDoc
recognizeRootEnr = pStr $ 
          (\str root-> reuseRootEnr [str] Nothing (Just root) Nothing)
      <$> pSym (StructuralTk (Just $ RootEnrNode HoleEnrichedDoc []) empty [] NoIDP) -- EnrichedDoc is not instance of Editable
      <*> recognizeRoot
  <|>    RootEnr NoIDD (error "doc hole was parsed") (error "doc hole was parsed")
     <$ pStructural HoleEnrichedDocNode
-- TODO: Why do we need this hole parser here?


recognizeRoot :: ListParser Document Node ClipDoc UserToken Root
recognizeRoot = pStr $
         (\str graph title sections ->
          reuseRoot [str] Nothing (Just graph) (Just (String_ NoIDD title)) (Just (toList_Section sections)) )
      <$> pStructural RootNode
      <*> recognizeGraph
      <*> pPrs pLine 
      <*> pList recognizeSection 

{- -- for presentation that only contains a parsing formatter
          (\str ->
          reuseRoot [str] Nothing Nothing Nothing 
                                            Nothing )
      <$> pStructural RootNode
      <*  (pPrs $ Word NoIDD (String_ NoIDD "")
      <$
          pList (pKey " ")
      <*  pList (pText <* pKey " "))
-}    
  
recognizeSection :: ListParser Document Node ClipDoc UserToken Section
recognizeSection = pStrAlt SectionNode $
          (\str t ps ss -> reuseSection [str] Nothing (Just (String_ NoIDD t)) (Just ps) (Just $ toList_Subsection ss))
      <$> pStructural SectionNode
      <*> pPrs pLine 
      <*> pPrs parseParagraphs
      <*> pList recognizeSubsection
          
recognizeSubsection :: ListParser Document Node ClipDoc UserToken Subsection
recognizeSubsection =
  pStrAlt SubsectionNode $
          (\str t ps sss -> reuseSubsection [str] Nothing (Just (String_ NoIDD t)) (Just ps) (Just $ toList_Subsubsection sss))
      <$> pStructural SubsectionNode
      <*> pPrs pLine 
      <*> pPrs parseParagraphs
      <*> pList recognizeSubsubsection
      

recognizeSubsubsection :: ListParser Document Node ClipDoc UserToken Subsubsection
recognizeSubsubsection =
  pStrAlt SubsubsectionNode $
          (\str t ps -> reuseSubsubsection [str] Nothing (Just (String_ NoIDD t)) (Just ps))
      <$> pStructural SubsubsectionNode
      <*> pPrs pLine 
      <*> pPrs parseParagraphs


-- TODO: parsed edges are now on index in vertexlist, fix it so they are on vertex nr
--       - add vertex nr to VertexP, and take care of indexing in lower layers (so presentation ag
--         does not have to do this)
recognizeGraph :: ListParser Document Node ClipDoc UserToken Graph
recognizeGraph = pStrVerbose "Graph" $
          (\str gt vs -> reuseGraph [str] Nothing (Just $ getGraphTkDirty gt) 
                                   (Just $ List_Vertex NoIDD $ toConsList_Vertex vs)
                                   (Just $ List_Edge NoIDD $ toConsList_Edge $ 
                                   [ Edge NoIDD (Int_ NoIDD f) (Int_ NoIDD t) |  (f,t) <- getGraphTkEdges gt]))
                        
                                          
      <$> pStructural GraphNode
      <*> pSym graphTk
      <*> pList recognizeVertex

-- labels in vertex? Or just in presentation?
-- before we can parse them, the scanner needs to be modified to handle free text
recognizeVertex :: ListParser Document Node ClipDoc UserToken Vertex
recognizeVertex = pStrVerbose "Vertex" $
          (\str vt lab -> reuseVertex [str] Nothing (Just lab) Nothing Nothing
                                  (Just $ getVertexTkX vt) (Just $ getVertexTkY vt))
      <$> pStructural VertexNode
      <*> pSym vertexTk
      <*> parseLabel
  <|>     (\str vt -> reuseVertex [str] Nothing (Just $ String_ NoIDD "<new>") (Just $ Circle NoIDD)
                                  (Just $ getVertexTkId vt) (Just $ getVertexTkX vt) (Just $ getVertexTkY vt))
      <$> pStructural (\_ _ -> NoNode)
      <*> pSym vertexTk

parseLabel :: ListParser Document Node ClipDoc UserToken String_
parseLabel = pPrs $
          (\str -> String_ NoIDD str)
      <$> pText

recognizeSubgraph :: ListParser Document Node ClipDoc UserToken Subgraph
recognizeSubgraph = pStrVerbose "Subgraph" $
          (\str gt vs -> reuseSubgraph [str] Nothing (Just $ getGraphTkDirty gt)  
                                     (Just $ List_Vertex NoIDD $ toConsList_Vertex vs)
                                     (Just $ List_Edge NoIDD $ toConsList_Edge $ 
                                     [ Edge NoIDD (Int_ NoIDD f) (Int_ NoIDD t) |  (f,t) <- getGraphTkEdges gt])
                      )
      <$> pStructural SubgraphNode
      <*> pSym graphTk
      <*> pList recognizeVertex

getGraphTkDirty :: Show node => Token doc node clip UserToken -> Dirty
getGraphTkDirty (GraphTk dirty _ _ _) = if isClean dirty then Clean NoIDD else Dirty NoIDD
getGraphTkDirty tk = debug Err ("ERROR: getGraphTkDirty: called on non GraphTk: "++show tk++"\n") $ Dirty NoIDD

getGraphTkEdges :: Show node => Token doc node clip UserToken -> [(Int,Int)]
getGraphTkEdges (GraphTk _ edges _ _) = edges
getGraphTkEdges tk = debug Err ("ERROR: getGraphTkEdges: called on non GraphTk: "++show tk++"\n") $ []

getVertexTkId :: Show node => Token doc node clip UserToken -> Int_
getVertexTkId (VertexTk i (x,y) _ _) = Int_ NoIDD i
getVertexTkId tk = debug Err ("ERROR: getVertexTkId: called on non VertexTk: "++show tk++"\n") $ Int_ NoIDD 0

getVertexTkX :: Show node => Token doc node clip UserToken -> Int_
getVertexTkX (VertexTk _ (x,y) _ _) = Int_ NoIDD x
getVertexTkX tk = debug Err ("ERROR: getVertexTkX: called on non VertexTk: "++show tk++"\n") $ Int_ NoIDD 0

getVertexTkY :: Show node => Token doc node clip UserToken -> Int_
getVertexTkY (VertexTk _ (x,y) _ _) = Int_ NoIDD y
getVertexTkY tk = debug Err ("ERROR: getVertexTkY: called on non VertexTk: "++show tk++"\n") $ Int_ NoIDD 0

keywords :: [String]
keywords = 
  [ " "
  , "\n"
  , "\\graph"
  ]

-- this must be a pList1Sep, otherwise we get an error. In this case there is always at least one, but
-- it should be possible to have an empty list two. Unclear why the parser disallows this.
parseParagraphs = toList_Paragraph 
      <$> pList1Sep (pKey "\n") parseParagraph

parseParagraph =
          (\ws -> reuseParagraph [] Nothing (Just (toList_Word ws)))
      <$  pList (pKey " ") 
      <*> pList parseWord
  <|> 
          (reuseSubgraphPara [] (Just NoIDD) (Just $ Subgraph NoIDD (Dirty NoIDD)
                                                              (List_Vertex NoIDD Nil_Vertex)
                                                              (List_Edge NoIDD Nil_Edge))) -- we need a FreshIDD here    
      <$  pKey "\\graph"
  <|>
      (   pStrAlt SubgraphParaNode $
          (\str sg -> reuseSubgraphPara [str] Nothing (Just sg))
      <$> pStructural SubgraphParaNode
      <*> recognizeSubgraph
      )


parseWord = 
          (\str -> reuseWord [] Nothing (Just $ String_ NoIDD str))
      <$> pText
      <*  pList (pKey " ")  
      -- the Scanner produces " " tokens, which are converted to key tokens
      -- split adds a " \n", so maybe we encounter two spaces

pLine = 
      (\wrds -> concat wrds)
  <$> pList ((++) <$> pText <*> pSpaces)

pSpaces = concat <$> pList (const " " <$> pKey " " <|> const "" <$> pKey "\n") -- ignore linebreaks

-- parse any consecutive piece of text (remember that spaces and "\n"'s are tokens)
pText = concat <$> pList1 (tokenString <$> (pLIdent <|> pUIdent <|> pOp <|> pSymm <|> pInt))

pUIdent = pCSym 20 uIdentTk
pOp = pCSym 20 opTk
pSymm = pCSym 20 symTk
--



-- don't even have to use reuse now, since the IDD is never used. String_ NoIDD would be sufficient
mkString_ :: DocNode node => Token doc node clip UserToken -> String_
mkString_ = (\strTk -> reuseString_ [] Nothing (Just $ tokenString strTk)) 

mkInt_ :: DocNode node => Token doc node clip UserToken -> Int_
mkInt_ = (\intTk -> reuseInt_ [] Nothing (Just $ intVal intTk)) 

-- Extracting the value from the token is not necessary, since true and false have different
-- parsers, which can give the value as an argument
mkBool_ :: Bool -> Bool_
mkBool_ = (\bool -> reuseBool_ [] Nothing (Just bool)) 



--- Stuff from PresentationParsing



-- (IDP (-1)) means inserted token. This should be handled by some kind of 'fresh' attribute
-- which is also required for copying of presentation subtrees
strTk str = UserTk (StrTk str) str Nothing (IDP (-1))
intTk     = UserTk IntTk "0" Nothing (IDP (-1))
lIdentTk  = UserTk LIdentTk "ident" Nothing (IDP (-1))
uIdentTk  = UserTk UIdentTk "Ident" Nothing (IDP (-1))
opTk      = UserTk OpTk "" Nothing (IDP (-1))
symTk     = UserTk SymTk "" Nothing (IDP (-1))


mkToken :: [String] -> String -> Maybe node -> IDP -> Token doc node clip UserToken
mkToken keywords str@(c:_)   ctxt i | str `elem` keywords = UserTk (StrTk str) str ctxt i
                                    | isDigit c           = UserTk IntTk str ctxt i
                                    | isLower c           = UserTk LIdentTk str ctxt i
                                    | isUpper c           = UserTk UIdentTk str ctxt i
                                    | otherwise           = UserTk OpTk str ctxt i

--makeToken str ctxt i = Tk str ctxt i

isSymbolChar c = c `elem` ";,(){}#_|"


-- Basic parsers

pKey :: DocNode node => String -> ListParser doc node clip UserToken (Token doc node clip UserToken)
pKey str = pSym  (strTk str)

pKeyC :: DocNode node => Int -> String -> ListParser doc node clip UserToken (Token doc node clip UserToken)
pKeyC c str = pCSym c (strTk str)

-- expensive, because we want holes to be inserted, not strings
pLIdent :: DocNode node => ListParser doc node clip UserToken (Token doc node clip UserToken)
pLIdent = pCSym 20 lIdentTk

-- todo return int from pInt, so unsafe intVal does not need to be used anywhere else
pInt :: DocNode node => ListParser doc node clip UserToken (Token doc node clip UserToken)
pInt = pCSym 20 intTk

lIdentVal :: DocNode node => Token doc node clip UserToken -> String
lIdentVal (UserTk LIdentTk str _ _) = str
lIdentVal tk                 = debug Err ("PresentationParser.lIdentVal: no IdentTk " ++ show tk) "x"

  
intVal :: DocNode node => Token doc node clip UserToken -> Int
intVal (UserTk IntTk "" _ _)  = 0   -- may happen on parse error (although not likely since insert is expensive)
intVal (UserTk IntTk str _ _) = read str
intVal tk              = debug Err ("PresentationParser.intVal: no IntTk " ++ show tk) (-9999)

 








---- needs to be here temporarily due to mkToken (which will be transfered to scanner)
-- put all tokens in one big list
-- UNCLEAR: what happens when list is presented again? Will it ever? Maybe we can avoid it, even with the new correcting parser
-- TODO put keyword stuff in Scanner layer
--      check what happens with tokens without context info. It seems they get it from higher up
--      in the tree now, which seems wrong. 

postScanStr :: [String] -> Maybe node -> Presentation doc node clip UserToken -> [Token doc node clip UserToken]
postScanStr kwrds ctxt (EmptyP _)           = []
postScanStr kwrds ctxt (StringP _ _)        = []
postScanStr kwrds ctxt (TokenP _ _)         = debug Err ("*** PresentationParser.postScanStr: Token in structural presentation") []
postScanStr kwrds ctxt (ImageP _ _ _)         = []
postScanStr kwrds ctxt (PolyP _ _ _ _)        = []
postScanStr kwrds ctxt (RectangleP _ _ _ _ _) = []
postScanStr kwrds ctxt (EllipseP _ _ _ _ _)   = []
postScanStr kwrds ctxt (WithP _ pres)       = postScanStr kwrds ctxt pres
postScanStr kwrds ctxt (OverlayP _ [])      = []
postScanStr kwrds ctxt (OverlayP _ (pres:press)) = postScanStr kwrds ctxt pres
postScanStr kwrds ctxt (ColP i _ _ press)   = concatMap (postScanStr kwrds ctxt) press
postScanStr kwrds ctxt (RowP i _ press)     = concatMap (postScanStr kwrds ctxt) press
postScanStr kwrds ctxt (LocatorP l pres)    = postScanStr kwrds (Just l) pres  
postScanStr kwrds ctxt (GraphP i d _ _ es press) = GraphTk d es ctxt i : concatMap (postScanStr kwrds ctxt) press
postScanStr kwrds ctxt (VertexP i v x y _ pres)  = VertexTk v (x,y) ctxt i : postScanStr kwrds ctxt pres  
postScanStr kwrds ctxt (ParsingP i _ pres)     = [ParsingTk pres (postScanPrs kwrds ctxt pres) i]
--postScanStr kwrds ctxt (ParsingP i pres)   = [StructuralTk (Just NoNode) pres (postScanPrs kwrds ctxt pres ctxt) i]
postScanStr kwrds ctxt (StructuralP i pres)  = [StructuralTk ctxt pres (postScanStr kwrds ctxt pres) i]
postScanStr kwrds ctxt (FormatterP i press)  = concatMap (postScanStr kwrds ctxt) press
postScanStr kwrds ctxt pres = debug Err ("*** PresentationParser.postScanStr: unimplemented presentation: " ++ show pres) []


postScanPrs :: [String] -> Maybe node -> Presentation doc node clip UserToken -> [Token doc node clip UserToken]
postScanPrs kwrds ctxt (EmptyP _)           = []
postScanPrs kwrds ctxt (StringP _ "")       = []
postScanPrs kwrds ctxt (StringP i str)      = [mkToken kwrds str ctxt i]
postScanPrs kwrds ctxt (TokenP i t)         = [t]
postScanPrs kwrds ctxt (ImageP _ _ _)         = []
postScanPrs kwrds ctxt (PolyP _ _ _ _)        = []
postScanPrs kwrds ctxt (RectangleP _ _ _ _ _) = []
postScanPrs kwrds ctxt (EllipseP _ _ _ _ _)   = []
postScanPrs kwrds ctxt (WithP _ pres)       = postScanPrs kwrds ctxt pres
postScanPrs kwrds ctxt (OverlayP _ [])      = []
postScanPrs kwrds ctxt (OverlayP _ (pres:press)) = postScanPrs kwrds ctxt pres
postScanPrs kwrds ctxt (ColP i _ _ press)   = concatMap (postScanPrs kwrds ctxt) press
postScanPrs kwrds ctxt (RowP i _ press)     = concatMap (postScanPrs kwrds ctxt) press
postScanPrs kwrds ctxt (LocatorP l pres)    = postScanPrs kwrds (Just l) pres
postScanPrs kwrds ctxt (GraphP i _ _ _ _ press) = debug Err ("WARNING: presentation contains Graph that is not part of a structural presentation") []
postScanPrs kwrds ctxt (VertexP _ _ _ _ _ pres) = debug Err ("WARNING: presentation contains Vertex that is not part of a structural presentation") []
postScanPrs kwrds ctxt (ParsingP _ _ pres)    = postScanPrs kwrds ctxt pres
postScanPrs kwrds ctxt (StructuralP i pres) = [StructuralTk ctxt pres (postScanStr kwrds ctxt pres) i ]
postScanPrs kwrds ctxt (FormatterP i press) = concatMap (postScanPrs kwrds ctxt) press ++ [UserTk (StrTk "\n") "\n" Nothing NoIDP]
postScanPrs kwrds ctxt pres  = debug Err ("*** PresentationParser.postScanPrs: unimplemented presentation: " ++ show pres) []
-- ref to UserTk is now because scanner cannot easily add "\n". The AG scanner will be able to do
-- this and make this ref obsolete. (PostScanPrs will be obsolete when Token type is added to Presentation)



