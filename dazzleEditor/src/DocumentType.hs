data EnrichedDoc = RootEnr root:Root Document
-- document ref is for popups only

data Dummy = Dummy [Dummy] String Bool Int -- necessary because of design error in generator

-- Don't remove the declarations above

data Root = Root graph:Graph title:String sections:[Section]

data Section = Section title:String paragraphs:[Paragraph] subsections:[Subsection]

data Subsection = Subsection title:String paragraphs:[Paragraph] subsubsections:[Subsubsection]

data Subsubsection = Subsubsection title:String paragraphs:[Paragraph]

data Paragraph = Paragraph words:[Word]
               | SubgraphPara subgraph:Subgraph
               
data Word = Word word:String
          | NodeRef nodeName:String
          | Label label:String
          | LabelRef label:String
          
data Graph = Graph dirty:Dirty vertices:[Vertex] edges:[Edge]

data Vertex = Vertex name:String shape:Shape id:Int x:Int y:Int

data Shape = Circle | Square

data Edge = Edge from:Int to:Int

data Subgraph = Subgraph dirty:Dirty vertices:[Vertex] edges:[Edge]
            
data Dirty = Dirty
           | Clean