{- comment -}

-- comment

data Document =         RootDoc decls:[Decl]                            { idd:IDD idp0:IDP                              }

data Decl = Decl        bool0:Bool bool1:Bool ident:Ident exp:Exp       { idd:IDD idp0:IDP idp1:IDP idp2:IDP idp3:IDP   }
          | BoardDecl   board:Board                                     { idd:IDD idp0:IDP idp1:IDP                     }

data Alt   = Alt        ident:Ident exp:Exp                             { idd:IDD idp0:IDP idp1:IDP                     }

data Ident = Ident      String                                          { IDD idp0:IDP                              }

data Exp = PlusExp      Exp exp1:Exp                                    { idd:IDD idp0:IDP                              }
         | TimesExp     exp0:Exp exp1:Exp                               { idd:IDD idp0:IDP                              }
         | DivExp       exp0:Exp exp1:Exp                               { idd:IDD idp0:IDP                              }
         | PowerExp     exp0:Exp exp1:Exp                               { idd:IDD idp0:IDP                              }
         | BoolExp      bool:Bool                                       { idd:IDD idp0:IDP                              }
         | IntExp       int:Int                                         { idd:IDD idp0:IDP                              }
         | LamExp       ident:Ident exp:Exp                             { idd:IDD idp0:IDP idp1:IDP                     }
         | AppExp       exp0:Exp exp1:Exp                               { idd:IDD                                       }
         | IdentExp     ident:Ident                                     { idd:IDD                                       }
         | IfExp        exp0:Exp exp1:Exp exp2:Exp                      { idd:IDD idp0:IDP idp1:IDP idp2:IDP            }
         | ParenExp     exp0:Exp                                        { idd:IDD idp0:IDP idp1:IDP                     }
         | CaseExp      exp0:Exp  alts:[Alt]                            { idd:IDD idp0:IDP idp1:[IDP]                   }
         | LetExp       decls:[Decl] exp:Exp                            { idd:IDD idp0:[IDP] idp1:IDP                   } 
         | ListExp      exps:[Exp]                                      { idd:IDD idp0:IDP idp1:IDP idpl:[IDP]          }
         | ProductExp   exps:[Exp]                                      { idd:IDD idp0:IDP idp1:IDP idpl:[IDP]          }
         | Empty


 