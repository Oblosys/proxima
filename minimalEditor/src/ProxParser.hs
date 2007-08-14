module ProxParser (parsePres) where

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

-- TODO: move to PresentationParsing
reuse = Nothing
set = Just

parsePres pres = let tokens = postScanStr keywords Nothing pres
                     (enr,errs) = runParser recognizeRootEnr tokens
                 in  -- showDebug' Prs ("Parsing:\n"++concatMap (deepShowTks 0) (tokens)++"with errs"{-++show errs-}++"\nhas result:") $
                     (if null errs then Just enr else Nothing)
       
deepShowTks i tok = case tok of
                      (StructuralTk _ _ cs _) -> indent i ++ show tok ++ "\n"
                                               ++ indent (i+1)++"[\n"
                                               ++ concatMap (deepShowTks (i+1)) cs 
                                               ++ indent (i+1)++" ]\n"
                      (ParsingTk _ cs _) -> indent i ++ show tok ++ "\n"
                                               ++ indent (i+1)++"[\n"
                                               ++ concatMap (deepShowTks (i+1)) cs 
                                               ++ indent (i+1)++" ]\n"
                      _                     -> indent i ++ show tok ++ "\n" 
 where indent i = take i (repeat ' ')
       





-------------------- Proxima Parser/Structure Recognizer -------------------- 

recognizeRootEnr :: ListParser Document Node ClipDoc EnrichedDoc
recognizeRootEnr = pStr $ 
          (\str decls-> reuseRootEnr [tokenNode str] Nothing (Just decls) Nothing)
      <$> pSym (StructuralTk (Just $ RootEnrNode HoleEnrichedDoc []) empty [] NoIDP) -- EnrichedDoc is not instance of Editable
      <*> recognizeRoot
  <|>    RootEnr NoIDD (error "doc hole was parsed") (error "doc hole was parsed")
     <$ pStructural HoleEnrichedDocNode
-- TODO: Why do we need this hole parser here?


recognizeRoot :: ListParser Document Node ClipDoc Root
recognizeRoot = pStr $
{-          (\str graph tree sections  ->
          reuseRoot [tokenNode str] Nothing (Just tree) (Just graph)
                                           (Just (toList_Section sections)) )
      <$> pStructural RootNode
      <*> recognizeGraph
      <*> pPrs parseTree {- recognizeTree -}
      <*> pList recognizeSection -}
          (\str graph ->
          reuseRoot [tokenNode str] Nothing Nothing (Just graph)
                                           Nothing )
      <$> pStructural RootNode
      <*> recognizeGraph
      

parseTree :: ListParser Document Node ClipDoc Tree
parseTree = 
          (\po t1 b t2 pc -> reuseBin [tokenNode po, tokenNode b, tokenNode pc] Nothing (Just t1) (Just t2))
      <$> pKey "("
      <*> parseTree
      <*> pKey "Bin"
      <*> parseTree
      <*> pKey ")"
  <|>     (\l -> reuseLeaf [tokenNode l] Nothing)
      <$> pKey "Leaf"

recognizeTree :: ListParser Document Node ClipDoc Tree
recognizeTree = pStr $
          (\str t1 t2 -> reuseBin [tokenNode str] Nothing (Just t1) (Just t2))
      <$> pStructural BinNode
      <*> recognizeTree
      <*> recognizeTree
  <|>     (\str -> reuseLeaf [tokenNode str] Nothing)
      <$> pStructural LeafNode


recognizeSection :: ListParser Document Node ClipDoc Section
recognizeSection = pStr $
          (\str ps sg -> reuseSection [tokenNode str] Nothing (Just ps) (Just sg))
      <$> pStructural SectionNode
      <*> pPrs parseParagraphs
      <*> recognizeSubgraph
      
-- TODO: parsed edges are now on index in vertexlist, fix it so they are on vertex nr
--       - add vertex nr to VertexP, and take care of indexing in lower layers (so presentation ag
--         does not have to do this)
recognizeGraph :: ListParser Document Node ClipDoc Graph
recognizeGraph = pStr $
          (\str gt vs -> reuseGraph [tokenNode str] Nothing (Just $ getGraphTkDirty gt) 
                                   (Just $ List_Vertex NoIDD $ toConsList_Vertex vs)
                                   (Just $ List_Edge NoIDD $ toConsList_Edge $ 
                                   [ Edge NoIDD (Int_ NoIDD f) (Int_ NoIDD t) |  (f,t) <- getGraphTkEdges gt]))
                        
                                          
      <$> pStructural GraphNode
      <*> pSym graphTk
      <*> pList recognizeVertex

-- labels in vertex? Or just in presentation?
-- before we can parse them, the scanner needs to be modified to handle free text
recognizeVertex :: ListParser Document Node ClipDoc Vertex
recognizeVertex = pStr $
          (\str vt lab -> reuseVertex [tokenNode str] Nothing (Just lab) Nothing 
                                  (Just $ getVertexTkX vt) (Just $ getVertexTkY vt))
      <$> pStructural VertexNode
      <*> pSym vertexTk
      <*> parseLabel
  <|>     (\str vt -> reuseVertex [tokenNode str] Nothing (Just $ String_ NoIDD "<new>")
                                  (Just $ getVertexTkId vt) (Just $ getVertexTkX vt) (Just $ getVertexTkY vt))
      <$> pStructural (\_ _ -> NoNode)
      <*> pSym vertexTk

parseLabel :: ListParser Document Node ClipDoc String_
parseLabel = pPrs $
          (\str -> String_ NoIDD str)
      <$> pText

recognizeSubgraph :: ListParser Document Node ClipDoc Subgraph
recognizeSubgraph = pStr $
          (\str gt vs -> reuseSubgraph [tokenNode str] Nothing (Just $ getGraphTkDirty gt)  
                                     (Just $ List_Vertex NoIDD $ toConsList_Vertex vs)
                                     (Just $ List_Edge NoIDD $ toConsList_Edge $ 
                                     [ Edge NoIDD (Int_ NoIDD f) (Int_ NoIDD t) |  (f,t) <- getGraphTkEdges gt])
                      )
      <$> pStructural SubgraphNode
      <*> pSym graphTk
      <*> pList recognizeVertex

getGraphTkDirty :: Show node => Token doc node clip (Maybe node)-> Dirty
getGraphTkDirty (GraphTk dirty _ _ _) = if isClean dirty then Clean NoIDD else Dirty NoIDD
getGraphTkDirty tk = debug Err ("ERROR: getGraphTkDirty: called on non GraphTk: "++show tk++"\n") $ Dirty NoIDD

getGraphTkEdges :: Show node => Token doc node clip (Maybe node)-> [(Int,Int)]
getGraphTkEdges (GraphTk _ edges _ _) = edges
getGraphTkEdges tk = debug Err ("ERROR: getGraphTkEdges: called on non GraphTk: "++show tk++"\n") $ []

getVertexTkId :: Show node => Token doc node clip (Maybe node)-> Int_
getVertexTkId (VertexTk i (x,y) _ _) = Int_ NoIDD i
getVertexTkId tk = debug Err ("ERROR: getVertexTkId: called on non VertexTk: "++show tk++"\n") $ Int_ NoIDD 0

getVertexTkX :: Show node => Token doc node clip (Maybe node)-> Int_
getVertexTkX (VertexTk _ (x,y) _ _) = Int_ NoIDD x
getVertexTkX tk = debug Err ("ERROR: getVertexTkX: called on non VertexTk: "++show tk++"\n") $ Int_ NoIDD 0

getVertexTkY :: Show node => Token doc node clip (Maybe node)-> Int_
getVertexTkY (VertexTk _ (x,y) _ _) = Int_ NoIDD y
getVertexTkY tk = debug Err ("ERROR: getVertexTkY: called on non VertexTk: "++show tk++"\n") $ Int_ NoIDD 0

keywords :: [String]
keywords = 
  [ "Leaf"
  , "Bin"
  , " "  -- The formatter scanner now produces " " tokens which are handled as keywords
  , "\n"
  ]


parseParagraphs = toList_Paragraph 
      <$> pList parseParagraph

parseParagraph =
          (\ws -> reuseParagraph [] Nothing (Just (toList_Word ws)))
      <$  pList (pKey " ")
      <*> pList parseWord
      <*  pKey "\n"

parseWord = 
          (\str -> reuseWord [] Nothing (Just $ String_ NoIDD str))
      <$> pText
      <*  pList (pKey " ")  
      -- the Scanner produces " " tokens, which are converted to key tokens
      -- split adds a " \n", so maybe we encounter two spaces



-- simple free text parsing. Spaces are tokenized away.
pText = concat <$> pList1 (tokenString <$> (pLIdent <|> pUIdent <|> pOp <|> pSymm <|> pInt <|> pKey "Leaf" <|> pKey "Bin"))

pUIdent = pCSym 20 uIdentTk
pOp = pCSym 20 opTk
pSymm = pCSym 20 symTk
--



-- don't even have to use reuse now, since the IDD is never used. String_ NoIDD would be sufficient
mkString_ :: Show node => Token doc node clip (Maybe node) -> String_
mkString_ = (\strTk -> reuseString_ [] Nothing (Just $ strValTk strTk)) 

mkInt_ :: Show node => Token doc node clip (Maybe node) -> Int_
mkInt_ = (\intTk -> reuseInt_ [] Nothing (Just $ intVal intTk)) 

-- Extracting the value from the token is not necessary, since true and false have different
-- parsers, which can give the value as an argument
mkBool_ :: Bool -> Bool_
mkBool_ = (\bool -> reuseBool_ [] Nothing (Just bool)) 


