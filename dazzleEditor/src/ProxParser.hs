module ProxParser (recognizeEnrichedDoc) where

import Common.CommonTypes hiding (Dirty (..))
import qualified Common.CommonTypes
import Presentation.PresLayerTypes
import Presentation.PresLayerUtils
import Presentation.PresentationParsing
import Presentation.XprezLib

import List hiding (delete)


import ProxParser_Generated
import Evaluation.DocumentEdit

import Char

import DocTypes_Generated
import DocUtils_Generated

              
-------------------- Proxima Parser/Structure Recognizer -------------------- 

recognizeEnrichedDoc :: ListParser Document Node ClipDoc UserToken EnrichedDoc
recognizeEnrichedDoc = pStr $ 
          (\str root-> reuseRootEnr [str] (Just root))
      <$> pStructuralTk Node_RootEnr
      <*> recognizeRoot

recognizeRoot :: ListParser Document Node ClipDoc UserToken Root
recognizeRoot = pStr $
          (\str graph title sections ->
          reuseRoot [str] (Just graph) Nothing (Just title) (Just sections))
      <$> pStructuralTk Node_Root
      <*> recognizeGraph
      <*> pPrs pLine 
      <*> recognizeList_Section

recognizeList_Section = pStr $ 
          (\str lst -> List_Section (toConsList_Section lst))
      <$> pStructuralTk Node_List_Section
      <*> pList recognizeSection
      
{- -- for presentation that only contains a parsing formatter
          (\str ->
          reuseRoot [str] Nothing Nothing Nothing 
                                            Nothing )
      <$> pStructuralTk Node_Root
      <*  (pPrs $ Word (String_ "")
      <$
          pList (pKey " ")
      <*  pList (pText <* pKey " "))
-}    
  
recognizeSection :: ListParser Document Node ClipDoc UserToken Section
recognizeSection = pStrAlt Node_Section $
          (\str t ps ss -> reuseSection [str] (Just t) (Just ps) (Just ss))
      <$> pStructuralTk Node_Section
      <*> pPrs pLine 
      <*> pPrs parseParagraphs
      <*> recognizeList_Subsection
          
recognizeList_Subsection = pStr $ 
          (\str lst -> List_Subsection (toConsList_Subsection lst))
      <$> pStructuralTk Node_List_Subsection
      <*> pList recognizeSubsection

recognizeSubsection :: ListParser Document Node ClipDoc UserToken Subsection
recognizeSubsection =
  pStrAlt Node_Subsection $
          (\str t ps sss -> reuseSubsection [str] (Just t) (Just ps) (Just sss))
      <$> pStructuralTk Node_Subsection
      <*> pPrs pLine 
      <*> pPrs parseParagraphs
      <*> recognizeList_Subsubsection
      
recognizeList_Subsubsection = pStr $ 
          (\str lst -> List_Subsubsection (toConsList_Subsubsection lst))
      <$> pStructuralTk Node_List_Subsubsection
      <*> pList recognizeSubsubsection

recognizeSubsubsection :: ListParser Document Node ClipDoc UserToken Subsubsection
recognizeSubsubsection =
  pStrAlt Node_Subsubsection $
          (\str t ps -> reuseSubsubsection [str] (Just t) (Just ps))
      <$> pStructuralTk Node_Subsubsection
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
                        
                                          
      <$> pStructuralTk Node_Graph
      <*> pSym graphTk
      <*> pList recognizeVertex

-- labels in vertex? Or just in presentation?
-- before we can parse them, the scanner needs to be modified to handle free text
recognizeVertex :: ListParser Document Node ClipDoc UserToken Vertex
recognizeVertex = pStrVerbose "Vertex" $
          (\str vt lab -> reuseVertex [str] (Just lab) Nothing Nothing
                                  (Just $ getVertexTkX vt) (Just $ getVertexTkY vt))
      <$> pStructuralTk Node_Vertex
      <*> pSym vertexTk
      <*> pPrs pText
  <|>     (\str vt -> reuseVertex [str] (Just "<new>") (Just Circle)
                                  (Just $ getVertexTkId vt) (Just $ getVertexTkX vt) (Just $ getVertexTkY vt))
      <$> pStructuralTk (\_ _ -> NoNode)
      <*> pSym vertexTk
          
recognizeSubgraph :: ListParser Document Node ClipDoc UserToken Subgraph
recognizeSubgraph = pStrVerbose "Subgraph" $
          (\str gt vs -> reuseSubgraph [str] (Just $ getGraphTkDirty gt)  
                                     (Just $ List_Vertex $ toConsList_Vertex vs)
                                     (Just $ List_Edge $ toConsList_Edge $ 
                                     [ Edge f t |  (f,t) <- getGraphTkEdges gt])
                      )
      <$> pStructuralTk Node_Subgraph
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
      (   pStrAlt Node_SubgraphPara $
          (\str sg -> reuseSubgraphPara [str] (Just sg))
      <$> pStructuralTk Node_SubgraphPara
      <*> recognizeSubgraph
      )
  <|>
      (   pStrAlt Node_ProbtablePara $
          (\str sg -> reuseProbtablePara [str] (Just sg))
      <$> pStructuralTk Node_ProbtablePara
      <*> recognizeProbtable
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


recognizeProbtable :: ListParser Document Node ClipDoc UserToken Probtable
recognizeProbtable = pStrVerbose "Probtable" $
          (\str vals probs -> reuseProbtable [str] Nothing (Just vals) (Just probs))
      <$> pStructuralTk Node_Probtable
      <*> recognizeList_Value
      <*> recognizeTable

recognizeList_Value = pStr $ 
          (\str lst -> List_Value (toConsList_Value lst))
      <$> pStructuralTk Node_List_Value
      <*> pList parseValue
      
parseValue :: ListParser Document Node ClipDoc UserToken Value
parseValue = pPrs $ Value . tokenString 
      <$> pWord
{-
recognizeList_Probability = pStr $ 
          (\str lst -> List_Probability (toConsList_Probability lst))
      <$> pStructuralTk Node_List_Probability
      <*> pList parseProbability
-}    
parseProbability :: ListParser Document Node ClipDoc UserToken Probability
parseProbability = pPrs $ Probability . tokenString 
      <$> pWord
     
recognizeTable :: ListParser Document Node ClipDoc UserToken Table
recognizeTable = pStr $ 
          (\str probs -> reuseTable [str] Nothing (Just $ toList_Probability probs))
      <$> pStructuralTk Node_Table
      <*> pList parseProbability



pKey str  = pToken (KeyTk str)
pWord     = pToken WordTk
pNodeRef  = pToken NodeRefTk
pLabel    = pToken LabelTk
pLabelRef = pToken LabelRefTk
pInt      = pToken IntTk
