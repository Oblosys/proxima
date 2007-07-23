module ProxParser (parsePres) where

import CommonTypes
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
                 in  --showDebug' Err ("Parsing:\n"++concatMap (deepShowTks 0) (tokens)++"with errs"{-++show errs-}++"\nhas result:") $
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
          (\str (d1,graph1) (d2,graph2) tree (ds,subGraph)  ->
              let (dsup, superGraph) = resolveCopies (d1,graph1) (d2,graph2)
                  (superGraph',subGraph') = resolveSubgraph (dsup,superGraph) (ds, subGraph)
              in debug Prs ("\n\nparsedGraph: "++show (d1,d2,ds)) $ 
                 reuseRoot [tokenNode str] Nothing (Just tree) (Just superGraph')
                                           (Just subGraph') )
      <$> pStructural RootNode
      <*> recognizeGraph
      <*> recognizeGraph
      <*> pPrs parseTree {- recognizeTree -}
      <*> recognizeSubGraph
 where resolveCopies (Dirty,g1) (_,g2) = (Dirty,g1)
       resolveCopies (Clean,g1) (Dirty,g2) = (Dirty, g2)
       resolveCopies (Clean,g1) (Clean,g2) = (Clean, g1)
       
       resolveSubgraph (Dirty, super@(Graph _ vs _)) (_, SubGraph id' vs' es') =
         let superGraphIDs = map getID_Vertex $ fromList_Vertex vs
             subGraphVertices = toList_Vertex $ filter (\v -> getID_Vertex v `elem` superGraphIDs) $
                                                       fromList_Vertex vs'
         in  (super, SubGraph id' subGraphVertices es') -- es' is ignored in presentation, so we don't have to remove edges to non-existing nodes
       resolveSubgraph (Clean, super) (Clean, sub) = (super, sub)
       resolveSubgraph (Clean, Graph id vs es) (Dirty, SubGraph id' vs' es') =
         let superGraphIDs = map getID_Vertex $ fromList_Vertex vs
             subGraphIDs = filter (`elem` superGraphIDs) $ map getID_Vertex $ fromList_Vertex vs'
             edgesWithoutSubgraphNodes = filter (\e -> getFrom_Edge e `notElem` subGraphIDs || getTo_Edge e `notElem` subGraphIDs) 
                                                (fromList_Edge es)
             graphEdges = toList_Edge $ edgesWithoutSubgraphNodes ++ fromList_Edge es'
         in (Graph id vs graphEdges, SubGraph id' vs' es') 
       {- resolve (Dirty,g1) _ _ = g1      
       resolve (Clean,Graph id (List_Vertex _ vs) es) (Dirty,SubGraph _ (List_Vertex _ vs')) _ = 
         Graph id (List_Vertex NoIDD (toConsList_Vertex (fromConsList_Vertex vs ++ fromConsList_Vertex vs'))) es
       resolve (Clean,Graph id (List_Vertex _ vs) es) _ (Dirty,SubGraph _ (List_Vertex _ vs')) = 
         Graph id (List_Vertex NoIDD (toConsList_Vertex (fromConsList_Vertex vs ++ fromConsList_Vertex vs'))) es
       resolve (_,g1) _ _ = g1      
       -}

getFrom_Edge (Edge _ (Int_ _ fromV) _) = fromV
getTo_Edge (Edge _ _ (Int_ _ toV)) = toV

getID_Vertex (Vertex _ _ (Int_ _ id) _ _) = id

toList_Vertex vs = List_Vertex NoIDD (toConsList_Vertex vs)

fromList_Vertex (List_Vertex _ vs) = fromConsList_Vertex vs
fromList_Vertex _                  = []

toList_Edge vs = List_Edge NoIDD (toConsList_Edge vs)

fromList_Edge (List_Edge _ vs) = fromConsList_Edge vs
fromList_Edge _                  = []

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



-- TODO: parsed edges are now on index in vertexlist, fix it so they are on vertex nr
--       - add vertex nr to VertexP, and take care of indexing in lower layers (so presentation ag
--         does not have to do this)
recognizeGraph :: ListParser Document Node ClipDoc (Dirty, Graph)
recognizeGraph = pStrDirty $
          (\str gt vs ->(getGraphTkDirty gt
                        ,reuseGraph [tokenNode str] Nothing 
                                   (Just $ List_Vertex NoIDD $ toConsList_Vertex vs)
                                   (Just $ List_Edge NoIDD $ toConsList_Edge $ 
                                   [ Edge NoIDD (Int_ NoIDD f) (Int_ NoIDD t) |  (f,t) <- getGraphTkEdges gt]))
                        )
                                          
      <$> pStructural GraphNode
      <*> pSym graphTk
      <*> pList recognizeVertex

-- labels in vertex? Or just in presentation?
-- before we can parse them, the scanner needs to be modified to handle free text
recognizeVertex :: ListParser Document Node ClipDoc Vertex
recognizeVertex = pStr $
          (\str vt lab -> reuseVertex [tokenNode str] Nothing Nothing Nothing 
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
          (\strTk -> mkString_ strTk)
      <$> pLIdent 

recognizeSubGraph :: ListParser Document Node ClipDoc (Dirty, SubGraph)
recognizeSubGraph = pStrDirty $
          (\str gt vs -> (getGraphTkDirty gt
                      ,reuseSubGraph [tokenNode str] Nothing (Just $ List_Vertex NoIDD $ toConsList_Vertex vs)
                                     (Just $ List_Edge NoIDD $ toConsList_Edge $ 
                                     [ Edge NoIDD (Int_ NoIDD f) (Int_ NoIDD t) |  (f,t) <- getGraphTkEdges gt])
                      ))
      <$> pStructural SubGraphNode
      <*> pSym graphTk
      <*> pList recognizeVertex

getGraphTkDirty :: Show node => Token doc node clip (Maybe node)-> Dirty
getGraphTkDirty (GraphTk dirty _ _ _) = dirty
getGraphTkDirty tk = debug Err ("ERROR: getGraphTkDirty: called on non GraphTk: "++show tk++"\n") $ Dirty

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
  [ ":-"
  , ":+"
  , "_|_"
  , "#"
  , "<"
  , ">"
  , "L"
  , "R"
  , "<-"
  , "->"
  , "<+"
  , "+>"
  , "\""
  , "</"
  , "/>"
  , "," --
  , "(" --
  , ")" --
  , "{" --
  , "}" --
  , ";" --
  , "[" --
  , "]" --
  , "="
  , "%"
  , "+"
  , "-"
  , "*"
  , "/"
  , "^"
  , "\174"
  , "\\" 
--  , "l"      -- not very nice, just for demonstrating lambdas
  , "False"
  , "True"
  , "if"
  , "then"
  , "else"
  , "let"
  , "in"
  , "case"
  , "of"
  , "Chess"
  , "board"
  , "Slides"
  , "pres"
  , "Inv"
  , "inv"
  , ":"
  , "..."
  , "Form"
  , "what"
  , "Leaf"
  , "Bin"
  ]





-- don't even have to use reuse now, since the IDD is never used. String_ NoIDD would be sufficient
mkString_ :: Show node => Token doc node clip (Maybe node) -> String_
mkString_ = (\strTk -> reuseString_ [] Nothing (Just $ strValTk strTk)) 

mkInt_ :: Show node => Token doc node clip (Maybe node) -> Int_
mkInt_ = (\intTk -> reuseInt_ [] Nothing (Just $ intVal intTk)) 

-- Extracting the value from the token is not necessary, since true and false have different
-- parsers, which can give the value as an argument
mkBool_ :: Bool -> Bool_
mkBool_ = (\bool -> reuseBool_ [] Nothing (Just bool)) 


