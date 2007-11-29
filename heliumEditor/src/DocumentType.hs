data String_ = String_ string : String                   { idd : IDD }
data Bool_   = Bool_   bool   : Bool                     { idd : IDD }
data Int_    = Int_    int    : Int                      { idd : IDD }

data Dummy = Dummy Root [Dummy] String_ Bool_ Int_ { idd : IDD } -- necessary because of design error in generator

-- Don't remove the declarations above

data EnrichedDoc = RootEnr root:RootE HeliumTypeInfo Document { id:IDD } 
-- document ref is for popups only

data Root = Root decls:[Decl] { id:IDD idP:IDP } 

data RootE = RootE decls:[Decl] idListDecls:[Decl] { id:IDD idP:IDP }
-- We put the decls first, so the paths in the enriched document and the document are the same.
-- This is necessary because there is no translation on paths yet.

-- lists have no pres elts of its own, so no ids for them. This also means we won't be able to recover Decls's id

--  4 pres elts: "=", ";", TypeDecl, and "[...]"
data Decl = Decl expanded:Bool_ autoLayout:Bool_ Ident Exp { idD:IDD idP0:IDP idP1:IDP idP2:IDP idP3:IDP }
          | BoardDecl Board                              { idD: IDD idP0:IDP idP1:IDP }
          | PPPresentationDecl PPPresentation            { idD: IDD idP0:IDP idP1:IDP }

-- one pres elt for in program source, other for in list
-- however, only the one for source is used, the other has no layout
data Ident = Ident String_                               { idD:IDD idP0:IDP idP1:IDP }

data Exp = PlusExp exp1:Exp exp2:Exp                     { idD:IDD idP0:IDP }
         | TimesExp  exp1:Exp exp2:Exp                   { idD:IDD idP0:IDP }
         | DivExp exp1:Exp exp2:Exp                      { idD:IDD idP0:IDP }
         | PowerExp exp1:Exp exp2:Exp                    { idD:IDD idP0:IDP }
         | BoolExp  Bool_                                { idD:IDD idP0:IDP }
         | IntExp Int_                                   { idD:IDD idP0:IDP }
         | LamExp Ident Exp                              { idD:IDD idP0:IDP idP1:IDP }
         | AppExp exp1:Exp exp2:Exp                      { idD:IDD }
         | CaseExp Exp alts:[Alt]                        { idD:IDD idP0:IDP idP1:IDP }
         | LetExp [Decl] Exp                             { idD:IDD idP0:IDP idP1:IDP }
         | IdentExp Ident                                { idd:IDD }
         | IfExp exp1:Exp exp2:Exp exp3:Exp              { idD:IDD idP0:IDP idP1:IDP idP2:IDP }
         | ParenExp Exp                                  { idD:IDD idP0:IDP idP1:IDP }
         | ListExp exps:[Exp]                            { idD:IDD idP0:IDP idP1:IDP ids:[IDP] }
         | ProductExp exps:[Exp]                         { idD:IDD idP0:IDP idP1:IDP ids:[IDP] }

data Alt = Alt Ident Exp                                 { idD:IDD idP0:IDP idP1:IDP }

data Board       = Board    r1:BoardRow r2:BoardRow r3:BoardRow r4:BoardRow
                            r5:BoardRow r6:BoardRow r7:BoardRow r8:BoardRow { idD: IDD }
                            
data BoardRow    = BoardRow ca:BoardSquare cb:BoardSquare cc:BoardSquare cd:BoardSquare
                            ce:BoardSquare cf:BoardSquare cg:BoardSquare ch:BoardSquare { idD: IDD }

data BoardSquare = Queen color  : Bool_                  { idD : IDD }
                 | King  color  : Bool_                  { idD : IDD }
                 | Bishop color : Bool_                  { idD : IDD }
                 | Knight color : Bool_                  { idD : IDD }
                 | Rook color   : Bool_                  { idD : IDD }
                 | Pawn color   : Bool_                  { idD : IDD }
                 | Empty                                 { }


data PPPresentation = PPPresentation viewType : Bool_ [Slide] { idd : IDD }

data Slide = Slide title : String_ ItemList              { idd : IDD }

data ItemList = ItemList ListType items:[Item]                  { idd : IDD }

data ListType = Bullet                                  { idd : IDD }
              | Number                                  { idd : IDD }
              | Alpha                                   { idd : IDD }

data Item = StringItem string : String_                  { idd : IDD }
          | HeliumItem Exp                               { idd : IDD }
          | ListItem ItemList                            { idd : IDD }

     