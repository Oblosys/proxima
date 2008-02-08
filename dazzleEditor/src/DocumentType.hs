data EnrichedDoc = RootEnr root:Root Document { id:IDD } 
-- document ref is for popups only

data Dummy = Dummy [Dummy] String Bool Int  { idd : IDD } -- necessary because of design error in generator

-- Don't remove the declarations above

data Root = Root graph:Graph title:String sections:[Section] { idd : IDD }

data Section = Section title:String paragraphs:[Paragraph] subsections:[Subsection] { idd : IDD }

data Subsection = Subsection title:String paragraphs:[Paragraph] subsubsections:[Subsubsection] { idd : IDD }

data Subsubsection = Subsubsection title:String paragraphs:[Paragraph] { idd : IDD }

data Paragraph = Paragraph words:[Word]                  { idd : IDD }
               | SubgraphPara subgraph:Subgraph          { idd : IDD }

data Word = Word word:String                                { idd : IDD }
          | NodeRef nodeName:String                         { idd : IDD } 
          | Label label:String                              { idd : IDD } 
          | LabelRef label:String                           { idd : IDD } 
          
data Graph = Graph dirty:Dirty vertices:[Vertex] edges:[Edge] { idd : IDD }

data Vertex = Vertex name:String shape:Shape id:Int x:Int y:Int     { idd : IDD }

data Shape = Circle { idd : IDD } | Square { idd : IDD }

data Edge = Edge from:Int to:Int       { idd : IDD }

data Subgraph = Subgraph dirty:Dirty vertices:[Vertex] edges:[Edge] { idd : IDD }

data Dirty = Dirty { idd : IDD }
           | Clean { idd : IDD }