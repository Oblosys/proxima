module DocUtils_Generated where

import DocTypes
import PresTypes

--instance Show Node where
--  show NoNode = "<>"
--  show (DocNode doc pth) = "<"++shallowShowDoc1 doc++","++show pth++">"
--  show (ExpNode exp pth) = "<"++shallowShowExp1 exp++","++show pth++">"
--  show _ = "<NODE>"

shallowShowDoc0 (RootDoc id pid exp) = "RootDoc "++show id++" "++show pid++" <Exp>"
shallowShowDoc0 HoleDoc                = "HoleDoc"
shallowShowDoc0 (ParseErrDoc _ _)      = "ParseErrDoc <Node> <Presentation>"


shallowShowExp0 (PlusExp id pid _ _)  = "PlusExp "++show id++" "++show pid++" <Color> <Exp> <Exp> "
shallowShowExp0 (TimesExp id pid _ _) = "TimesExp "++show id++" "++show pid++" <Color> <Exp> <Exp> "
shallowShowExp0 (DivExp id pid _ _)   = "DivExp "++show id++" "++show pid++" <Color> <Exp> <Exp> "
shallowShowExp0 (PowerExp id pid _ _) = "PowerExp "++show id++" "++show pid++" <Color> <Exp> <Exp> "
shallowShowExp0 (IntExp id pid i)     = "IntExp "++show id++" "++show pid++" <Color> "++show i
shallowShowExp0 HoleExp                 = "HoleExp"
shallowShowExp0 (ParseErrExp _ _)       = "ParseErrExp <Node> <Presentation>"

-- less verbose version
shallowShowDoc1 (RootDoc id pid exp) = "RootDoc"
shallowShowDoc1 HoleDoc                = "HoleDoc"
shallowShowDoc1 (ParseErrDoc _ _)      = "ParseErrDoc"


shallowShowExp1 (PlusExp id pid _ _)  = "PlusExp"
shallowShowExp1 (TimesExp id pid _ _) = "TimesExp"
shallowShowExp1 (DivExp id pid _ _)   = "DivExp"
shallowShowExp1 (PowerExp id pid _ _) = "PowerExp"
shallowShowExp1 (IntExp id pid i)     = "IntExp"
shallowShowExp1 HoleExp                 = "HoleExp"
shallowShowExp1 (ParseErrExp _ _)       = "ParseErrExp"

shallowShowExp1 _                       = "<EXP>"







data XML = Elt String [(String, String)] [XML] | PCData String | EmptyElt

showXML xml = 
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  ++ case xml of (Elt tg _ _) -> "<!DOCTYPE "++tg++" SYSTEM \""++tg++".dtd\" >\n"
                 _            -> ""
  ++ showXML' 0 xml
 where showXML' i (Elt tag ps []) = replicate i ' ' ++"<"++tag++showProperties ps++"/>\n"
       showXML' i (Elt tag ps [PCData str]) = replicate i ' ' ++"<"++tag++showProperties ps++">"++str++"</"++tag++">\n" 
       showXML' i (Elt tag ps cs) = replicate i ' ' ++"<"++tag++showProperties ps++">\n"
                              ++ concatMap (showXML' (i+2)) cs
                              ++ replicate i ' ' ++"</"++tag++">\n" 
       showXML' i (EmptyElt)     = replicate i ' ' ++"Empty\n"
       showXML' i (PCData str)   = replicate i ' ' ++str++"\n"
       showProperties [] = ""
       showProperties ((p,v):ps) = " "++p++"="++show v++ showProperties ps
-- element with one child PCDATA is displayed on one line. For more than one child it's too much of a hassle
-- because this function is only temporary


toXMLRoot (RootDoc _ _ dcls)  = Elt "Module" [] $ toXMLDecls dcls
toXMLRoot _                    =  Elt "ErrRoot" [] []

toXMLExps (ConsExps _ exp exps) = toXMLExp exp : toXMLExps exps
toXMLExps (NilExps _)           = []
toXMLExps _                     = []


toXMLDecls (ConsDecls _ decl decls) = toXMLDecl decl : toXMLDecls decls
toXMLDecls (NilDecls _)             = []
toXMLDecls _                        = []

toXMLAlts (ConsAlts _ alt alts) = toXMLAlt alt : toXMLAlts alts
toXMLAlts (NilAlts _)           = []
toXMLAlts _                     = []

toXMLExp (PlusExp _ _ x1 x2)      = Elt "Sum" [] $ map toXMLExp [x1,x2] 
toXMLExp (TimesExp _ _ x1 x2)     = Elt "Product" [] $ map toXMLExp [x1,x2] 
toXMLExp (DivExp _ _ x1 x2)       = Elt "Quotient" [] $ map toXMLExp [x1,x2]
toXMLExp (PowerExp _ _ x1 x2)     = Elt "Power" [] $ map toXMLExp [x1,x2]
toXMLExp (BoolExp _ _ x1)         = Elt "Bool" [] [toXMLBool x1]
toXMLExp (IntExp _ _ x1)          = Elt "Int" [] [toXMLInt x1]
toXMLExp (CaseExp _ _ _ x1 x2)    = Elt "Case" [] $ [toXMLExp x1]++ toXMLAlts x2
toXMLExp (LetExp _ _ _ dcls x2)   = Elt "Let" [] $ toXMLDecls dcls ++ [toXMLExp x2]
toXMLExp (LamExp _ _ _ x1 x2)     = Elt "Lambda" [] $ [toXMLIdent x1, toXMLExp x2]
toXMLExp (AppExp _ x1 x2)         = Elt "Application" [] $ map toXMLExp [x1,x2]
toXMLExp (IdentExp _ x1)          = Elt "Ident" [] [toXMLIdent x1]
toXMLExp (IfExp _ _ _ _ x1 x2 x3) = Elt "If" [] $ map toXMLExp [x1,x2,x3]
toXMLExp (ParenExp _ _ _ x1)      = Elt "Paren" [] $ [toXMLExp x1]
toXMLExp (ProductExp _ _ _ _ x1)  = Elt "Tuple" [] $ toXMLExps x1
toXMLExp _                        = Elt "ErrExp" [] []

toXMLBool True = Elt "True" [] [] 
toXMLBool False = Elt "False" [] [] 

toXMLInt i = Elt "Integer" [("val", show i)] []

toXMLIdent (Ident _ _ _ str) = Elt "Identifier" [("val", str)] []
toXMLIDent _                 = Elt "ErrIdentifier" [] []


toXMLDecl (Decl _ _ _ _ _ _ _ i x)      = Elt "Declaration" [] $ [toXMLIdent i,toXMLExp x] 
toXMLDecl (BoardDecl _ _ _ b)      = Elt "ChessBoard" [] $ [toXMLBoard b] 
toXMLDecl _                    =  Elt "ErrDeclaration" [] []

toXMLAlt (Alt _ _ _ i x)      = Elt "Alternative" [] $ [toXMLIdent i,toXMLExp x] 
toXMLAlt _                    =  Elt "ErrAlt" [] []


toXMLBoard       (Board    _ r0 r1 r2 r3 r4 r5 r6 r7) =
  Elt "Board" [] $ map toXMLBoardRow [r0, r1, r2, r3, r4, r5, r6, r7]
toXMLBoardRow    (BoardRow _ ca cb cc cd ce cf cg ch) =
  Elt "BoardRow" [] $ map toXMLBoardSquare [ca, cb, cc, cd, ce, cf, cg, ch]
toXMLBoardSquare (Queen _ b) = Elt "Queen"[("color",if b then "white" else "black")] []
toXMLBoardSquare (King  _ b) = Elt "King" [("color",if b then "white" else "black")] []
toXMLBoardSquare (Bishop _ b) = Elt "Bishop" [("color",if b then "white" else "black")] []
toXMLBoardSquare (Knight _ b) = Elt "Knight" [("color",if b then "white" else "black")] []
toXMLBoardSquare (Rook _ b) = Elt "Rook" [("color",if b then "white" else "black")] []
toXMLBoardSquare (Pawn _ b) = Elt "Pawn" [("color",if b then "white" else "black")] []
toXMLBoardSquare (Empty) = Elt "Empty" [] []







---- begin: Generated code


pathNode :: Node -> PathDoc
pathNode NoNode            = NoPathD
pathNode (DocumentNode _ pth)   = PathD pth
pathNode (EnrichedDocNode _ pth)   = PathD pth
pathNode (DeclsNode _ pth) = PathD pth
pathNode (DeclNode _ pth)  = PathD pth
pathNode (IdentNode _ pth) = PathD pth
pathNode (ExpNode _ pth)   = PathD pth
pathNode (ExpsNode _ pth)  = PathD pth
pathNode (AltsNode _ pth) = PathD pth
pathNode (AltNode _ pth)  = PathD pth
pathNode (BoardNode _ pth)  = PathD pth
pathNode (BoardRowNode _ pth)  = PathD pth
pathNode (BoardSquareNode _ pth)  = PathD pth
pathNode (PPPresentationNode _ pth)  = PathD pth
pathNode (SlidesNode _ pth)  = PathD pth
pathNode (SlideNode _ pth)  = PathD pth
pathNode (ItemListNode _ pth)  = PathD pth
pathNode (ListTypeNode _ pth)  = PathD pth
pathNode (ItemsNode _ pth)  = PathD pth
pathNode (ItemNode _ pth)  = PathD pth
pathNode (String_Node _ pth)  = PathD pth
-- id functions: 

-- returns Just id if node is a Bool, otherwise Nothing

altIDD :: Node -> Maybe IDD
altIDD (AltNode (Alt idD _ _ _ _) _) = Just idD
altIDD _                             = Nothing

declIDD :: Node -> Maybe IDD
declIDD (DeclNode (Decl idD _ _ _ _ _ _ _ _) _) = Just idD
declIDD _                                   = Nothing

identIDD :: Node -> Maybe IDD
identIDD (IdentNode (Ident idD _ _ _) _) = Just idD
identIDD _                             = Nothing

intExpIDD :: Node -> Maybe IDD
intExpIDD (ExpNode (IntExp idD _ _) _) = Just idD
intExpIDD _                            = Nothing

boolExpIDD :: Node -> Maybe IDD
boolExpIDD (ExpNode (BoolExp idD _ _) _) = Just idD
boolExpIDD _                             = Nothing


sumIDD :: Node -> Maybe IDD
sumIDD (ExpNode (PlusExp idD _ _ _) _) = Just idD
sumIDD _                               = Nothing

prodIDD :: Node -> Maybe IDD
prodIDD (ExpNode (TimesExp idD _ _ _) _) = Just idD
prodIDD _                                = Nothing

divIDD :: Node -> Maybe IDD
divIDD (ExpNode (DivExp idD _ _ _) _) = Just idD
divIDD _                              = Nothing

powerIDD :: Node -> Maybe IDD
powerIDD (ExpNode (PowerExp idD _ _ _) _) = Just idD
powerIDD _                                = Nothing

parenIDD :: Node -> Maybe IDD
parenIDD (ExpNode (ParenExp idD _ _ _) _) = Just idD
parenIDD _                                = Nothing

lamIDD :: Node -> Maybe IDD
lamIDD (ExpNode (LamExp idD _ _ _ _) _) = Just idD
lamIDD _                                = Nothing


caseIDD :: Node -> Maybe IDD
caseIDD (ExpNode (CaseExp idD _ _ _ _) _) = Just idD
caseIDD _                                 = Nothing

letIDD :: Node -> Maybe IDD
letIDD (ExpNode (LetExp idD _ _ _ _) _) = Just idD
letIDD _                                = Nothing

ifIDD :: Node -> Maybe IDD
ifIDD (ExpNode (IfExp idD _ _ _ _ _ _) _) = Just idD
ifIDD _                                   = Nothing

listIDD :: Node -> Maybe IDD
listIDD (ExpNode (ListExp idD _ _ _ _) _) = Just idD
listIDD _                                    = Nothing

productIDD :: Node -> Maybe IDD
productIDD (ExpNode (ProductExp idD _ _ _ _) _) = Just idD
productIDD _                                    = Nothing

string_IDD :: Node -> Maybe IDD
string_IDD (String_Node (String_ idD _) _) = Just idD
string_IDD _                                    = Nothing





---- end: Generated code



