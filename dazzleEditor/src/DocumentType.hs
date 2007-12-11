data EnrichedDoc = RootEnr root:Root Document { id:IDD } 
-- document ref is for popups only

data String_ = String_ string : String                   { idd : IDD }
data Bool_   = Bool_   bool   : Bool                     { idd : IDD }
data Int_    = Int_    int    : Int                      { idd : IDD }

data Dummy = Dummy [Dummy] String_ Bool_ Int_ { idd : IDD } -- necessary because of design error in generator

-- Don't remove the declarations above

data Root = Root graph:Graph title:String_ sections:[Section] { idd : IDD }

data Section = Section title:String_ paragraphs:[Paragraph] subsections:[Subsection] { idd : IDD }

data Subsection = Subsection title:String_ paragraphs:[Paragraph] subsubsections:[Subsubsection] { idd : IDD }

data Subsubsection = Subsubsection title:String_ paragraphs:[Paragraph] { idd : IDD }

data Paragraph = Paragraph words:[Word]                  { idd : IDD }
               | SubgraphPara subgraph:Subgraph          { idd : IDD }

data Word = Word word:String_                                { idd : IDD }

data Graph = Graph dirty:Dirty vertices:[Vertex] edges:[Edge] { idd : IDD }

data Vertex = Vertex name:String_ shape:Shape id:Int_ x:Int_ y:Int_     { idd : IDD }

data Shape = Circle { idd : IDD } | Square { idd : IDD }

data Edge = Edge from:Int_ to:Int_       { idd : IDD }

data Subgraph = Subgraph dirty:Dirty vertices:[Vertex] edges:[Edge] { idd : IDD }

data Dirty = Dirty { idd : IDD }
           | Clean { idd : IDD }