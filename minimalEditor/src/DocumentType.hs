data EnrichedDoc = RootEnr root:Root Document { id:IDD } 

data String_ = String_ string : String                   { idd : IDD }
data Bool_   = Bool_   bool   : Bool                     { idd : IDD }
data Int_    = Int_    int    : Int                      { idd : IDD }

data Dummy = Dummy [Dummy] String_ Bool_ Int_ { idd : IDD } -- necessary because of design error in generator

-- Don't remove the declarations above


data Root = Root Tree { idd : IDD }

data Tree = Bin left:Tree right:Tree { idd : IDD }
          | Leaf                     { idd : IDD }