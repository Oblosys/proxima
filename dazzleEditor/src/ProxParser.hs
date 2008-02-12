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
          (\str root-> reuseRootEnr [str] (Just root) Nothing)
      <$> pSym (StructuralTk (Just $ RootEnrNode HoleEnrichedDoc []) empty [] NoIDP) -- EnrichedDoc is not instance of Editable
      <*> recognizeRoot
  <|>    RootEnr (error "doc hole was parsed") (error "doc hole was parsed")
     <$ pStructural HoleEnrichedDocNode
-- TODO: Why do we need this hole parser here?


recognizeRoot :: ListParser Document Node ClipDoc UserToken Root
recognizeRoot = pStr $
         (\str graph title sections ->
          reuseRoot [str] (Just graph) (Just title) (Just (toList_Section sections)))
      <$> pStructural RootNode
      <*> recognizeGraph
      <*> pPrs pLine 
      <*> pList recognizeSection 

{- -- for presentation that only contains a parsing formatter
          (\str ->
          reuseRoot [str] Nothing Nothing Nothing 
                                            Nothing )
      <$> pStructural RootNode
      <*  (pPrs $ Word (String_ "")
      <$
          pList (pKey " ")
      <*  pList (pText <* pKey " "))
-}    
  
recognizeSection :: ListParser Document Node ClipDoc UserToken Section
recognizeSection = pStrAlt SectionNode $
          (\str t ps ss -> reuseSection [str] (Just t) (Just ps) (Just $ toList_Subsection ss))
      <$> pStructural SectionNode
      <*> pPrs pLine 
      <*> pPrs parseParagraphs
      <*> pList recognizeSubsection
          
recognizeSubsection :: ListParser Document Node ClipDoc UserToken Subsection
recognizeSubsection =
  pStrAlt SubsectionNode $
          (\str t ps sss -> reuseSubsection [str] (Just t) (Just ps) (Just $ toList_Subsubsection sss))
      <$> pStructural SubsectionNode
      <*> pPrs pLine 
      <*> pPrs parseParagraphs
      <*> pList recognizeSubsubsection
      

recognizeSubsubsection :: ListParser Document Node ClipDoc UserToken Subsubsection
recognizeSubsubsection =
  pStrAlt SubsubsectionNode $
          (\str t ps -> reuseSubsubsection [str] (Just t) (Just ps))
      <$> pStructural SubsubsectionNode
      <*> pPrs pLine 
      <*> pPrs parseParagraphs


-- TODO: parsed edges are now on index in vertexlist, fix it so they are on vertex nr
--       - add vertex nr to VertexP, and take care of indexing in lower layers (so presentation ag
--         does not have to do this)
recognizeGraph :: ListParser Document Node ClipDoc UserToken Graph
recognizeGraph = pStrVerbose "Graph" $
          (\str gt vs -> reuseGraph [str] (Just $ getGraphTkDirty gt) 
                                   (Just $ List_Vertex $ toConsList_Vertex vs)
                                   (Just $ List_Edge $ toConsList_Edge $ 
                                   [ Edge f t |  (f,t) <- getGraphTkEdges gt]))
                        
                                          
      <$> pStructural GraphNode
      <*> pSym graphTk
      <*> pList recognizeVertex

-- labels in vertex? Or just in presentation?
-- before we can parse them, the scanner needs to be modified to handle free text
recognizeVertex :: ListParser Document Node ClipDoc UserToken Vertex
recognizeVertex = pStrVerbose "Vertex" $
          (\str vt lab -> reuseVertex [str] (Just lab) Nothing Nothing
                                  (Just $ getVertexTkX vt) (Just $ getVertexTkY vt))
      <$> pStructural VertexNode
      <*> pSym vertexTk
      <*> pPrs pText
  <|>     (\str vt -> reuseVertex [str] (Just "<new>") (Just Circle)
                                  (Just $ getVertexTkId vt) (Just $ getVertexTkX vt) (Just $ getVertexTkY vt))
      <$> pStructural (\_ _ -> NoNode)
      <*> pSym vertexTk
          
recognizeSubgraph :: ListParser Document Node ClipDoc UserToken Subgraph
recognizeSubgraph = pStrVerbose "Subgraph" $
          (\str gt vs -> reuseSubgraph [str] (Just $ getGraphTkDirty gt)  
                                     (Just $ List_Vertex $ toConsList_Vertex vs)
                                     (Just $ List_Edge $ toConsList_Edge $ 
                                     [ Edge f t |  (f,t) <- getGraphTkEdges gt])
                      )
      <$> pStructural SubgraphNode
      <*> pSym graphTk
      <*> pList recognizeVertex

getGraphTkDirty :: Show node => Token doc node clip UserToken -> Dirty
getGraphTkDirty (GraphTk dirty _ _ _) = if isClean dirty then Clean else Dirty
getGraphTkDirty tk = debug Err ("ERROR: getGraphTkDirty: called on non GraphTk: "++show tk++"\n") $ Dirty

getGraphTkEdges :: Show node => Token doc node clip UserToken -> [(Int,Int)]
getGraphTkEdges (GraphTk _ edges _ _) = edges
getGraphTkEdges tk = debug Err ("ERROR: getGraphTkEdges: called on non GraphTk: "++show tk++"\n") $ []

getVertexTkId :: Show node => Token doc node clip UserToken -> Int
getVertexTkId (VertexTk i (x,y) _ _) = i
getVertexTkId tk = debug Err ("ERROR: getVertexTkId: called on non VertexTk: "++show tk++"\n") $ 0

getVertexTkX :: Show node => Token doc node clip UserToken -> Int
getVertexTkX (VertexTk _ (x,y) _ _) = x
getVertexTkX tk = debug Err ("ERROR: getVertexTkX: called on non VertexTk: "++show tk++"\n") $ 0

getVertexTkY :: Show node => Token doc node clip UserToken -> Int
getVertexTkY (VertexTk _ (x,y) _ _) = y
getVertexTkY tk = debug Err ("ERROR: getVertexTkY: called on non VertexTk: "++show tk++"\n") $ 0

-- this must be a pList1Sep, otherwise we get an error. In this case there is always at least one, but
-- it should be possible to have an empty list too. Unclear why the parser disallows this.
parseParagraphs = toList_Paragraph 
      <$> pList1Sep (pKey "\n") parseParagraph

  
parseParagraph =
          (\ws -> reuseParagraph [] (Just (toList_Word ws)))
      <$  pList (pKey " ") 
      <*> pList parseWord
  <|> 
          (reuseSubgraphPara [] (Just $ Subgraph Dirty
                                          (List_Vertex Nil_Vertex)
                                          (List_Edge Nil_Edge))) -- we need a FreshIDD here    
      <$  pKey "\\graph"
  <|>
      (   pStrAlt SubgraphParaNode $
          (\str sg -> reuseSubgraphPara [str] (Just sg))
      <$> pStructural SubgraphParaNode
      <*> recognizeSubgraph
      )


parseWord = 
          (\str -> reuseWord [] (Just str))
      <$> pText
      <*  pList (pKey " ")  
  <|>
          (\str -> reuseNodeRef [] (Just $ tokenString str))
      <$> pNodeRef
      <*  pList (pKey " ")  
  <|>
          (\str -> reuseLabel [] (Just $ tokenString str))
      <$> pLabel
      <*  pList (pKey " ")  
  <|>
          (\str -> reuseLabelRef [] (Just $ tokenString str))
      <$> pLabelRef
      <*  pList (pKey " ")  
      -- the Scanner produces " " tokens, which are converted to key tokens
      -- split adds a " \n", so maybe we encounter two spaces

pLine = 
      (\wrds -> concat wrds)
  <$> pList ((++) <$> pText <*> pSpaces)

pSpaces = concat <$> pList (const " " <$> pKey " " <|> const "" <$> pKey "\n") -- ignore linebreaks

-- parse any consecutive piece of text (remember that spaces and "\n"'s are tokens)
pText = tokenString <$> pWord






-- (IDP (-1)) means inserted token. This should be handled by some kind of 'fresh' attribute
-- which is also required for copying of presentation subtrees
keyTk str = UserTk (KeyTk str) str Nothing (IDP (-1))
wordTk    = UserTk WordTk "word" Nothing (IDP (-1))
nodeRefTk = UserTk NodeRefTk "nodeRef" Nothing (IDP (-1))
labelTk   = UserTk LabelTk "labelRef" Nothing (IDP (-1))
labelRefTk = UserTk LabelRefTk "labelRef" Nothing (IDP (-1))

-- Basic parsers

pKey :: DocNode node => String -> ListParser doc node clip UserToken (Token doc node clip UserToken)
pKey str = pSym (keyTk str)

pKeyC :: DocNode node => Int -> String -> ListParser doc node clip UserToken (Token doc node clip UserToken)
pKeyC c str = pCSym c (keyTk str)

pWord :: DocNode node => ListParser doc node clip UserToken (Token doc node clip UserToken)
pWord = pSym wordTk

pNodeRef :: DocNode node => ListParser doc node clip UserToken (Token doc node clip UserToken)
pNodeRef = pSym nodeRefTk

pLabelRef :: DocNode node => ListParser doc node clip UserToken (Token doc node clip UserToken)
pLabelRef = pSym labelRefTk

pLabel :: DocNode node => ListParser doc node clip UserToken (Token doc node clip UserToken)
pLabel = pSym labelTk
