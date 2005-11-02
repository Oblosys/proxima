module DocumentEdit_Generated where

import CommonTypes
import DocTypes
import DocUtils
import PresTypes

import IOExts

-- not entirely generated: hole is special for chessboard and pppresentation, because hole is initialized
-- This is not good. hole must be just a hole for cut operations.
-- an initialized value must be specified separately class member initialValue?

arityClip :: ClipDoc -> Int
arityClip (Clip_Int x)     = arity x
arityClip (Clip_String x)  = arity x
arityClip (Clip_Bool x)    = arity x

arityClip (Clip_Decls x)   = arity x
arityClip (Clip_Decl x)    = arity x
arityClip (Clip_Ident x)   = arity x
arityClip (Clip_Exp x)     = arity x
arityClip (Clip_Exps x)    = arity x
arityClip (Clip_Decls x)   = arity x
arityClip (Clip_Alt x)     = arity x
arityClip (Clip_Alts x)    = arity x

arityClip (Clip_Board x)       = arity x
arityClip (Clip_BoardRow x)    = arity x
arityClip (Clip_BoardSquare x) = arity x

arityClip (Clip_PPPresentation x) = arity x
arityClip (Clip_Slides x) = arity x
arityClip (Clip_Slide x) = arity x
arityClip (Clip_ItemList x) = arity x
arityClip (Clip_ListType x) = arity x
arityClip (Clip_Items x) = arity x
arityClip (Clip_Item x) = arity x
arityClip (Clip_String_ x) = arity x

arityClip (Clip_Nothing)   = -1


alternativesClip :: ClipDoc -> [ (String, ClipDoc) ]
alternativesClip (Clip_Int x)     = alternatives x
alternativesClip (Clip_String x)  = alternatives x
alternativesClip (Clip_Bool x)    = alternatives x

alternativesClip (Clip_Decls x)   = alternatives x
alternativesClip (Clip_Decl x)    = alternatives x
alternativesClip (Clip_Ident x)   = alternatives x
alternativesClip (Clip_Exp x)     = alternatives x
alternativesClip (Clip_Exps x)    = alternatives x
alternativesClip (Clip_Alts x)    = alternatives x
alternativesClip (Clip_Alt x)     = alternatives x

alternativesClip (Clip_Board x)       = alternatives x
alternativesClip (Clip_BoardRow x)    = alternatives x
alternativesClip (Clip_BoardSquare x) = alternatives x

alternativesClip (Clip_PPPresentation x) = alternatives x
alternativesClip (Clip_Slides x) = alternatives x
alternativesClip (Clip_Slide x) = alternatives x
alternativesClip (Clip_ItemList x) = alternatives x
alternativesClip (Clip_ListType x) = alternatives x
alternativesClip (Clip_Items x) = alternatives x
alternativesClip (Clip_Item x) = alternatives x
alternativesClip (Clip_String_ x) = alternatives x

alternativesClip (Clip_Nothing)   = []




-- Constructor can be put in class as toClip or inject
holeClip :: ClipDoc -> ClipDoc
holeClip (Clip_Int x)     = Clip_Int hole
holeClip (Clip_String x)  = Clip_String hole
holeClip (Clip_Bool x)    = Clip_Bool hole

holeClip (Clip_Decls x)   = Clip_Decls hole
holeClip (Clip_Decl x)    = Clip_Decl hole
holeClip (Clip_Ident x)   = Clip_Ident hole
holeClip (Clip_Exp x)     = Clip_Exp hole
holeClip (Clip_Exps x)    = Clip_Exps hole
holeClip (Clip_Alts x)    = Clip_Alts hole
holeClip (Clip_Alt x)     = Clip_Alt hole

holeClip (Clip_Board x)       = Clip_Board hole
holeClip (Clip_BoardRow x)    = Clip_BoardRow hole
holeClip (Clip_BoardSquare x) = Clip_BoardSquare hole

holeClip (Clip_PPPresentation x) = Clip_PPPresentation hole
holeClip (Clip_Slides x) = Clip_Slides hole
holeClip (Clip_Slide x) = Clip_Slide hole
holeClip (Clip_ItemList x) = Clip_ItemList hole
holeClip (Clip_ListType x) = Clip_ListType hole
holeClip (Clip_Items x) = Clip_Items hole
holeClip (Clip_Item x) = Clip_Item hole
holeClip (Clip_String_ x) = Clip_String_ hole

holeClip Clip_Nothing     = Clip_Nothing


isListClip :: ClipDoc -> Bool
isListClip (Clip_Int x)     = isList x

isListClip (Clip_String x)  = isList x
isListClip (Clip_Bool x)    = isList x

isListClip (Clip_Decls x)   = isList x
isListClip (Clip_Decl x)    = isList x
isListClip (Clip_Ident x)   = isList x
isListClip (Clip_Exp x)     = isList x
isListClip (Clip_Exps x)    = isList x
isListClip (Clip_Alts x)    = isList x
isListClip (Clip_Alt x)     = isList x

isListClip (Clip_Board x)       = isList x
isListClip (Clip_BoardRow x)    = isList x
isListClip (Clip_BoardSquare x) = isList x

isListClip (Clip_PPPresentation x) = isList x
isListClip (Clip_Slides x) = isList x
isListClip (Clip_Slide x) = isList x
isListClip (Clip_ItemList x) = isList x
isListClip (Clip_ListType x) = isList x
isListClip (Clip_Items x) = isList x
isListClip (Clip_Item x) = isList x
isListClip (Clip_String_ x) = isList x


isListClip (Clip_Nothing)   = False

insertListClip :: Int -> ClipDoc -> ClipDoc -> ClipDoc
insertListClip i c (Clip_Int x)     = insertList i c x
insertListClip i c (Clip_String x)  = insertList i c x
insertListClip i c (Clip_Bool x)    = insertList i c x

insertListClip i c (Clip_Decls x)   = insertList i c x
insertListClip i c (Clip_Decl x)    = insertList i c x
insertListClip i c (Clip_Ident x)   = insertList i c x
insertListClip i c (Clip_Exp x)     = insertList i c x
insertListClip i c (Clip_Exps x)    = insertList i c x
insertListClip i c (Clip_Alts x)    = insertList i c x
insertListClip i c (Clip_Alt x)     = insertList i c x
insertListClip i c (Clip_Board x)       = insertList i c x
insertListClip i c (Clip_BoardRow x)    = insertList i c x
insertListClip i c (Clip_BoardSquare x) = insertList i c x

insertListClip i c (Clip_PPPresentation x) = insertList i c x
insertListClip i c (Clip_Slides x) = insertList i c x
insertListClip i c (Clip_Slide x) = insertList i c x
insertListClip i c (Clip_ItemList x) = insertList i c x
insertListClip i c (Clip_ListType x) = insertList i c x
insertListClip i c (Clip_Items x) = insertList i c x
insertListClip i c (Clip_Item x) = insertList i c x
insertListClip i c (Clip_String_ x) = insertList i c x

insertListClip i c (Clip_Nothing)   = Clip_Nothing


removeListClip :: Int -> ClipDoc -> ClipDoc
removeListClip i (Clip_Int x)     = removeList i x
removeListClip i (Clip_String x)  = removeList i x
removeListClip i (Clip_Bool x)    = removeList i x

removeListClip i (Clip_Decls x)   = removeList i x
removeListClip i (Clip_Decl x)    = removeList i x
removeListClip i (Clip_Ident x)   = removeList i x
removeListClip i (Clip_Exp x)     = removeList i x
removeListClip i (Clip_Exps x)    = removeList i x
removeListClip i (Clip_Alts x)    = removeList i x
removeListClip i (Clip_Alt x)     = removeList i x
 
removeListClip i (Clip_Board x)       = removeList i x
removeListClip i (Clip_BoardRow x)    = removeList i x
removeListClip i (Clip_BoardSquare x) = removeList i x

removeListClip i (Clip_PPPresentation x) = removeList i x
removeListClip i (Clip_Slides x) = removeList i x
removeListClip i (Clip_Slide x) = removeList i x
removeListClip i (Clip_ItemList x) = removeList i x
removeListClip i (Clip_ListType x) = removeList i x
removeListClip i (Clip_Items x) = removeList i x
removeListClip i (Clip_Item x) = removeList i x
removeListClip i (Clip_String_ x) = removeList i x

removeListClip i (Clip_Nothing)   = Clip_Nothing


  
instance Editable Int where
  select [] x = Clip_Int x
  select _  _ = Clip_Nothing
  paste [] (Clip_Int c) x = c
  paste [] c            x =  trace ("Type error: pasting "++show c++" on Int") x
  
  alternatives _ = [ ("0", Clip_Int 0)
                   , ("1", Clip_Int 1)
                   , ("{Int}", Clip_Int hole) ]
  
  arity _ = 0
  hole = 0
{-  
instance Editable String where
  select [] x = Clip_String x
  select _  _ = Clip_Nothing
  paste [] (Clip_String c) x = c
  paste [] c             x =  trace ("Type error: pasting "++show c++" on String") x

  alternatives _ = [ ("a", Clip_String "a")
                   , ("ab", Clip_String "ab")
                   , ("{String}", Clip_String hole) ]

  arity _ = 0
  hole = "x"
  

instance Editable Bool where
  select [] x = Clip_Bool x
  select _  _ = Clip_Nothing
  paste [] (Clip_Bool c) x = c
  paste [] c             x =  trace ("Type error: pasting "++show c++" on Bool") x

  alternatives _ = [ ("True", Clip_Bool True)
                   , ("False", Clip_Bool False)
                   , ("{Bool}", Clip_Bool hole) ]

  arity _ = 0
  hole = False

-}

{-
data Decls = ConsDecls IDD Decl Decls
           | NilDecls IDD
           | HoleDecls
           | ParseErrDecls Node Presentation deriving Show

List handling:
For now, no special ListDecls node
the edit functions always work on the topmost list node,
even though they are defined on the others as well

insert is always after the element. inserting at the head is done by paste on the list itself

These things will be fixed if document focus can be between list elements, but before it can, it
should be more clear how such a focus is presented.

-}

instance Editable Decls where
  select []    x                  = Clip_Decls x
  select (0:p) (ConsDecls _ x _)  = select p x
  select (n:p) (ConsDecls _ _ xs) = select (n-1:p) xs
  select _     _                  = Clip_Nothing

  paste [] (Clip_Decls c) xs        = c
  paste [] c              xs        = trace ("Type error: pasting "++show c++" on Decls") xs
  paste (0:p) c (ConsDecls i1 x xs) = ConsDecls i1 (paste p c x) xs
  paste (n:p) c (ConsDecls i1 x xs) = ConsDecls i1 x (paste (n-1:p) c xs) 
  paste _     _ xs                  = xs                   -- error

  alternatives _ = [ ("add {Decl}", Clip_Decl $ hole)
                   ]
                   --, ("[]",      Clip_Decls $ hole ) ]
  
  
  arity (ConsDecls _ _ xs) = 1 + arity xs
  arity _            = 0
  
  hole = NilDecls NoIDD

  isList _ = True
  
  insertList 0 (Clip_Decl x)  xs = Clip_Decls $ ConsDecls NoIDD x xs
  insertList 0 _              xs = trace "Type error, no paste" $ Clip_Decls $ xs
  insertList n c (ConsDecls i1 x xs) = let (Clip_Decls xs') = insertList (n-1) c xs
                                       in  Clip_Decls $ ConsDecls i1 x xs'
  insertList _ c xs                   = Clip_Decls xs

  removeList 0 (ConsDecls i1 x xs) = Clip_Decls $ xs
  removeList n (ConsDecls i1 x xs) = let (Clip_Decls xs') = removeList (n-1) xs
                                     in  Clip_Decls $ ConsDecls i1 x xs'
  removeList _ xs                  = Clip_Decls xs
 

instance Editable Decl where
  select []    x              = Clip_Decl x
  select (0:p) (Decl _ _ _ _ _ _ _ x1 x2) = select p x1
  select (0:p) (BoardDecl _ _ _ x1) = select p x1
  select (0:p) (PPPresentationDecl _ _ _ x1) = select p x1
  select (1:p) (Decl _ _ _ _ _ _ _ x1 x2) = select p x2
  select _     _              = Clip_Nothing

  paste [] (Clip_Decl c) x      = c
  paste [] c             x      =  trace ("Type error: pasting "++show c++" on Decl") x
  paste (0:p) c (Decl i1 i2 i3 i4 i5 ls1 ls2 x1 x2) = Decl i1 i2 i3 i4 i5 ls1 ls2 (paste p c x1) x2
  paste (0:p) c (BoardDecl i1 i2 i3 x1) = BoardDecl i1 i2 i3 (paste p c x1)
  paste (0:p) c (PPPresentationDecl i1 i2 i3 x1) = PPPresentationDecl i1 i2 i3 (paste p c x1)
  paste (1:p) c (Decl i1 i2 i3 i4 i5 ls1 ls2 x1 x2) = Decl i1 i2 i3 i4 i5 ls1 ls2 x1 (paste p c x2)
  paste _     _ x                = x                   -- error

  alternatives _ = [ ("Decl Ident Exp", Clip_Decl $ Decl NoIDD NoIDP NoIDP NoIDP NoIDP True False hole hole)
                   , ("BoardDecl Board", Clip_Decl $ BoardDecl NoIDD NoIDP NoIDP hole)
                   , ("PowerPointDecl", Clip_Decl $ PPPresentationDecl NoIDD NoIDP NoIDP hole)
                   , ("{Decl}",      Clip_Decl $ hole ) ]
  
  arity (Decl _ _ _ _ _ _ _ _ _) = 2
  arity (BoardDecl _ _ _ _) = 1
  arity (PPPresentationDecl _ _ _ _) = 1
  arity _            = 0
  
  hole = HoleDecl
  



instance Editable Ident where
  select []    x              = Clip_Ident x
  select (0:p) (Ident _ _ _ x1) = select p x1
  select _     _              = Clip_Nothing

  paste [] (Clip_Ident c) x      = c
  paste [] c             x      =  trace ("Type error: pasting "++show c++" on Ident") x
  paste (0:p) c (Ident i1 i2 i3 x1) = Ident i1 i2 i3 (paste p c x1) 
  paste _     _ x                = x                   -- error

  alternatives _ = [ ("Ident String", Clip_Ident $ Ident NoIDD NoIDP NoIDP hole)
                   , ("{Ident}",      Clip_Ident $ hole ) ]
  
  arity (Ident _ _ _ _) = 1
  arity _             = 0
  
  hole = HoleIdent



instance Editable Exp where
  select []    x                        = Clip_Exp x
  select (0:p) (PlusExp _ _ x1 x2)      = select p x1
  select (0:p) (TimesExp _ _ x1 x2)     = select p x1
  select (0:p) (DivExp _ _ x1 x2)       = select p x1
  select (0:p) (PowerExp _ _ x1 x2)     = select p x1
  select (0:p) (BoolExp _ _ x1)         = select p x1
  select (0:p) (IntExp _ _ x1)          = select p x1
  select (0:p) (CaseExp _ _ _ x1 x2)     = select p x1
  select (0:p) (LetExp _ _ _ x1 x2)     = select p x1
  select (0:p) (LamExp _ _ _ x1 x2)     = select p x1
  select (0:p) (AppExp _ x1 x2)         = select p x1
  select (0:p) (IdentExp _ x1)             = select p x1
  select (0:p) (IfExp _ _ _ _ x1 x2 x3) = select p x1
  select (0:p) (ParenExp _ _ _ x1)      = select p x1
  select (0:p) (ListExp _ _ _ _ x1)    = select p x1
  select (0:p) (ProductExp _ _ _ _ x1)    = select p x1
  select (1:p) (PlusExp _ _ x1 x2)      = select p x2
  select (1:p) (TimesExp _ _ x1 x2)     = select p x2
  select (1:p) (DivExp _ _ x1 x2)       = select p x2
  select (1:p) (PowerExp _ _ x1 x2)     = select p x2
  select (1:p) (CaseExp _ _ _ x1 x2)    = select p x2
  select (1:p) (LetExp _ _ _ x1 x2)     = select p x2
  select (1:p) (LamExp _ _ _ x1 x2)     = select p x2
  select (1:p) (AppExp _ x1 x2)         = select p x2
  select (1:p) (IfExp _ _ _ _ x1 x2 x3) = select p x2
  select (2:p) (IfExp _ _ _ _ x1 x2 x3) = select p x3
  select _     _                        = Clip_Nothing         -- error
  
  paste [] (Clip_Exp c) _                    = c
  paste [] c             x                   = trace ("Type error: pasting "++show c++" on Exp") x
  paste (0:p) c (PlusExp i1 i2 x1 x2)        = PlusExp i1 i2 (paste p c x1) x2
  paste (0:p) c (TimesExp i1 i2 x1 x2)       = TimesExp i1 i2 (paste p c x1) x2
  paste (0:p) c (DivExp i1 i2 x1 x2)         = DivExp i1 i2 (paste p c x1) x2
  paste (0:p) c (PowerExp i1 i2 x1 x2)       = PowerExp i1 i2 (paste p c x1) x2
  paste (0:p) c (BoolExp i1 i2 x1)           = BoolExp i1 i2 (paste p c x1)
  paste (0:p) c (IntExp i1 i2 x1)            = IntExp i1 i2 (paste p c x1)
  paste (0:p) c (CaseExp i1 i2 i3 x1 x2)     = CaseExp i1 i2 i3 (paste p c x1) x2
  paste (0:p) c (LetExp i1 i2 i3 x1 x2)      = LetExp i1 i2 i3 (paste p c x1) x2
  paste (0:p) c (LamExp i1 i2 i3 x1 x2)      = LamExp i1 i2 i3 (paste p c x1) x2
  paste (0:p) c (AppExp i1 x1 x2)            = AppExp i1 (paste p c x1) x2
  paste (0:p) c (IdentExp i1 x1)             = IdentExp i1 (paste p c x1)
  paste (0:p) c (IfExp i1 i2 i3 i4 x1 x2 x3) = IfExp i1 i2 i3 i4 (paste p c x1) x2 x3
  paste (0:p) c (ParenExp i1 i2 i3 x1)       = ParenExp i1 i2 i3 (paste p c x1)
  paste (0:p) c (ListExp i1 i2 i3 is x1)     = ListExp i1 i2 i3 is (paste p c x1)
  paste (0:p) c (ProductExp i1 i2 i3 is x1)  = ProductExp i1 i2 i3 is (paste p c x1)
  paste (1:p) c (PlusExp i1 i2 x1 x2)        = PlusExp i1 i2 x1 (paste p c x2)
  paste (1:p) c (TimesExp i1 i2 x1 x2)       = TimesExp i1 i2 x1 (paste p c x2)
  paste (1:p) c (DivExp i1 i2 x1 x2)         = DivExp i1 i2 x1 (paste p c x2)
  paste (1:p) c (PowerExp i1 i2 x1 x2)       = PowerExp i1 i2 x1 (paste p c x2)
  paste (1:p) c (CaseExp i1 i2 i3 x1 x2)     = CaseExp i1 i2 i3 x1 (paste p c x2)
  paste (1:p) c (LetExp i1 i2 i3 x1 x2)      = LetExp i1 i2 i3 x1 (paste p c x2)
  paste (1:p) c (LamExp i1 i2 i3 x1 x2)      = LamExp i1 i2 i3 x1 (paste p c x2)
  paste (1:p) c (AppExp i1 x1 x2)            = AppExp i1 x1 (paste p c x2)
  paste (1:p) c (IfExp i1 i2 i3 i4 x1 x2 x3) = IfExp i1 i2 i3 i4 x1 (paste p c x2) x3  
  paste (2:p) c (IfExp i1 i2 i3 i4 x1 x2 x3) = IfExp i1 i2 i3 i4 x1 x2 (paste p c x3)  
  paste _     _ x                            = x
  alternatives _ = [ ("PlusExp {Exp} {Exp}",  Clip_Exp $ PlusExp NoIDD NoIDP hole hole) 
                   , ("TimesExp {Exp} {Exp}", Clip_Exp $ TimesExp NoIDD NoIDP hole hole)
                   , ("DivExp {Exp} {Exp}",   Clip_Exp $ DivExp   NoIDD NoIDP hole hole)
                   , ("PowerExp {Exp} {Exp}", Clip_Exp $PowerExp NoIDD NoIDP hole hole)
                   , ("BoolExp {Bool}", Clip_Exp $BoolExp NoIDD NoIDP hole)
                   , ("IntExp {Int}", Clip_Exp $IntExp NoIDD NoIDP hole)
                   , ("CaseExp {Ident} {Exp}", Clip_Exp $CaseExp NoIDD NoIDP NoIDP hole hole)
                   , ("LetExp {Ident} {Exp}", Clip_Exp $LetExp NoIDD NoIDP NoIDP hole hole)
                   , ("LamExp {Ident} {Exp}", Clip_Exp $LamExp NoIDD NoIDP NoIDP hole hole)
                   , ("AppExp {Exp}", Clip_Exp $AppExp NoIDD hole hole)
                   , ("IdentExp {Ident}", Clip_Exp $IdentExp NoIDD hole)
                   , ("IfExp {Exp} {Exp} {Exp}", Clip_Exp $IfExp NoIDD NoIDP NoIDP NoIDP hole hole hole)
                   , ("ParenExp {Exp}", Clip_Exp $ ParenExp NoIDD NoIDP NoIDP hole)
                   , ("ListExp {Exps}", Clip_Exp $ ListExp NoIDD NoIDP NoIDP [] hole)
                   , ("ProductExp {Exps}", Clip_Exp $ ProductExp NoIDD NoIDP NoIDP [] hole)
                   , ("{Exp}", Clip_Exp hole)
                   ]
                   

  arity (PlusExp _ _ x1 x2)      = 2
  arity (TimesExp _ _ x1 x2)     = 2
  arity (DivExp _ _ x1 x2)       = 2
  arity (PowerExp _ _ x1 x2)     = 2
  arity (BoolExp _ _ x1)         = 1
  arity (IntExp _ _ x1)          = 1
  arity (CaseExp _ _ _ x1 x2)     = 2
  arity (LetExp _ _ _ x1 x2)     = 2
  arity (LamExp _ _ _ x1 x2)     = 2
  arity (AppExp _ x1 x2)         = 2
  arity (IdentExp _ x1)          = 1
  arity (IfExp _ _ _ _ x1 x2 x3) = 3
  arity (ParenExp _ _ _ x1)      = 1
  arity (ListExp _ _ _ _ x1)  = 1
  arity (ProductExp _ _ _ _ x1)  = 1
  arity _                        = 0
  
  hole = HoleExp


instance Editable Exps where
  select []    x                  = Clip_Exps x
  select (0:p) (ConsExps _ x _)   = select p x
  select (n:p) (ConsExps _ _ xs)  = select (n-1:p) xs
  select _     _                  = Clip_Nothing

  paste [] (Clip_Exps c) xs        = c
  paste [] c              xs       = trace ("Type error: pasting "++show c++" on Exps") xs
  paste (0:p) c (ConsExps i1 x xs) = ConsExps i1 (paste p c x) xs
  paste (n:p) c (ConsExps i1 x xs) = ConsExps i1 x (paste (n-1:p) c xs) 
  paste _     _ xs                 = xs                   -- error

  alternatives _ = [ ("add {Exp}", Clip_Exp $ hole)
                   ]
                   --, ("[]",      Clip_Exps $ hole ) ]
  
  
  arity (ConsExps _ _ xs) = 1 + arity xs
  arity _            = 0
  
  hole = NilExps NoIDD

  isList _ = True
  
  insertList 0 (Clip_Exp x)  xs = Clip_Exps $ ConsExps NoIDD x xs
  insertList 0 _              xs = trace "Type error, no paste" $ Clip_Exps $ xs
  insertList n c (ConsExps i1 x xs) = let (Clip_Exps xs') = insertList (n-1) c xs
                                       in  Clip_Exps $ ConsExps i1 x xs'
  insertList _ c xs                   = Clip_Exps xs

  removeList 0 (ConsExps i1 x xs) = Clip_Exps $ xs
  removeList n (ConsExps i1 x xs) = let (Clip_Exps xs') = removeList (n-1) xs
                                     in  Clip_Exps $ ConsExps i1 x xs'
  removeList _ xs                  = Clip_Exps xs
 



instance Editable Alts where
  select []    x                  = Clip_Alts x
  select (0:p) (ConsAlts _ x _)  = select p x
  select (n:p) (ConsAlts _ _ xs) = select (n-1:p) xs
  select _     _                  = Clip_Nothing

  paste [] (Clip_Alts c) xs        = c
  paste [] c              xs        = trace ("Type error: pasting "++show c++" on Alts") xs
  paste (0:p) c (ConsAlts i1 x xs) = ConsAlts i1 (paste p c x) xs
  paste (n:p) c (ConsAlts i1 x xs) = ConsAlts i1 x (paste (n-1:p) c xs) 
  paste _     _ xs                  = xs                   -- error

  alternatives _ = [ ("add {Alt}", Clip_Alt $ hole)
                   ]
                   --, ("[]",      Clip_Alts $ hole ) ]
  
  
  arity (ConsAlts _ _ xs) = 1 + arity xs
  arity _            = 0
  
  hole = NilAlts NoIDD

  isList _ = True
  
  insertList 0 (Clip_Alt x)  xs = Clip_Alts $ ConsAlts NoIDD x xs
  insertList 0 _              xs = trace "Type error, no paste" $ Clip_Alts $ xs
  insertList n c (ConsAlts i1 x xs) = let (Clip_Alts xs') = insertList (n-1) c xs
                                       in  Clip_Alts $ ConsAlts i1 x xs'
  insertList _ c xs                   = Clip_Alts xs

  removeList 0 (ConsAlts i1 x xs) = Clip_Alts $ xs
  removeList n (ConsAlts i1 x xs) = let (Clip_Alts xs') = removeList (n-1) xs
                                     in  Clip_Alts $ ConsAlts i1 x xs'
  removeList _ xs                  = Clip_Alts xs
 

instance Editable Alt where
  select []    x              = Clip_Alt x
  select (0:p) (Alt _ _ _ x1 x2) = select p x1
  select (1:p) (Alt _ _ _ x1 x2) = select p x2
  select _     _              = Clip_Nothing

  paste [] (Clip_Alt c) x      = c
  paste [] c             x      =  trace ("Type error: pasting "++show c++" on Alt") x
  paste (0:p) c (Alt i1 i2 i3 x1 x2) = Alt i1 i2 i3 (paste p c x1) x2
  paste (1:p) c (Alt i1 i2 i3 x1 x2) = Alt i1 i2 i3 x1 (paste p c x2)
  paste _     _ x                = x                   -- error

  alternatives _ = [ ("Alt Ident Exp", Clip_Alt $ Alt NoIDD NoIDP NoIDP hole hole)
                   , ("{Alt}",      Clip_Alt $ hole ) ]
  
  arity (Alt _ _ _ _ _) = 2
  arity _            = 0
  
  hole = HoleAlt
  



---------------------------------------------------------------------------------------

instance Editable Board where
  select []    x              = Clip_Board x
  select (0:p) (Board _ x1 x2 x3 x4 x5 x6 x7 x8) = select p x1
  select (1:p) (Board _ x1 x2 x3 x4 x5 x6 x7 x8) = select p x2
  select (2:p) (Board _ x1 x2 x3 x4 x5 x6 x7 x8) = select p x3
  select (3:p) (Board _ x1 x2 x3 x4 x5 x6 x7 x8) = select p x4
  select (4:p) (Board _ x1 x2 x3 x4 x5 x6 x7 x8) = select p x5
  select (5:p) (Board _ x1 x2 x3 x4 x5 x6 x7 x8) = select p x6
  select (6:p) (Board _ x1 x2 x3 x4 x5 x6 x7 x8) = select p x7
  select (7:p) (Board _ x1 x2 x3 x4 x5 x6 x7 x8) = select p x8
  select _     _              = Clip_Nothing

  paste [] (Clip_Board c) x     = c
  paste [] c             x      =  trace ("Type error: pasting "++show c++" on Decl") x
  paste (0:p) c (Board i1 x1 x2 x3 x4 x5 x6 x7 x8) = Board i1 (paste p c x1) x2 x3 x4 x5 x6 x7 x8
  paste (1:p) c (Board i1 x1 x2 x3 x4 x5 x6 x7 x8) = Board i1 x1 (paste p c x2) x3 x4 x5 x6 x7 x8
  paste (2:p) c (Board i1 x1 x2 x3 x4 x5 x6 x7 x8) = Board i1 x1 x2 (paste p c x3) x4 x5 x6 x7 x8
  paste (3:p) c (Board i1 x1 x2 x3 x4 x5 x6 x7 x8) = Board i1 x1 x2 x3 (paste p c x4) x5 x6 x7 x8
  paste (4:p) c (Board i1 x1 x2 x3 x4 x5 x6 x7 x8) = Board i1 x1 x2 x3 x4 (paste p c x5) x6 x7 x8
  paste (5:p) c (Board i1 x1 x2 x3 x4 x5 x6 x7 x8) = Board i1 x1 x2 x3 x4 x5 (paste p c x6) x7 x8
  paste (6:p) c (Board i1 x1 x2 x3 x4 x5 x6 x7 x8) = Board i1 x1 x2 x3 x4 x5 x6 (paste p c x7) x8
  paste (7:p) c (Board i1 x1 x2 x3 x4 x5 x6 x7 x8) = Board i1 x1 x2 x3 x4 x5 x6 x7 (paste p c x8)
  paste _     _ x                = x                   -- error

  alternatives _ = [("board",Clip_Nothing)]
  
  arity _        = 8
  
  hole = initBoard -- no hole in datatype
  


instance Editable BoardRow where
  select []    x              = Clip_BoardRow x
  select (0:p) (BoardRow _ x1 x2 x3 x4 x5 x6 x7 x8) = select p x1
  select (1:p) (BoardRow _ x1 x2 x3 x4 x5 x6 x7 x8) = select p x2
  select (2:p) (BoardRow _ x1 x2 x3 x4 x5 x6 x7 x8) = select p x3
  select (3:p) (BoardRow _ x1 x2 x3 x4 x5 x6 x7 x8) = select p x4
  select (4:p) (BoardRow _ x1 x2 x3 x4 x5 x6 x7 x8) = select p x5
  select (5:p) (BoardRow _ x1 x2 x3 x4 x5 x6 x7 x8) = select p x6
  select (6:p) (BoardRow _ x1 x2 x3 x4 x5 x6 x7 x8) = select p x7
  select (7:p) (BoardRow _ x1 x2 x3 x4 x5 x6 x7 x8) = select p x8
  select _     _              = Clip_Nothing

  paste [] (Clip_BoardRow c) x     = c
  paste [] c             x      =  trace ("Type error: pasting "++show c++" on Decl") x
  paste (0:p) c (BoardRow i1 x1 x2 x3 x4 x5 x6 x7 x8) = BoardRow i1 (paste p c x1) x2 x3 x4 x5 x6 x7 x8
  paste (1:p) c (BoardRow i1 x1 x2 x3 x4 x5 x6 x7 x8) = BoardRow i1 x1 (paste p c x2) x3 x4 x5 x6 x7 x8
  paste (2:p) c (BoardRow i1 x1 x2 x3 x4 x5 x6 x7 x8) = BoardRow i1 x1 x2 (paste p c x3) x4 x5 x6 x7 x8
  paste (3:p) c (BoardRow i1 x1 x2 x3 x4 x5 x6 x7 x8) = BoardRow i1 x1 x2 x3 (paste p c x4) x5 x6 x7 x8
  paste (4:p) c (BoardRow i1 x1 x2 x3 x4 x5 x6 x7 x8) = BoardRow i1 x1 x2 x3 x4 (paste p c x5) x6 x7 x8
  paste (5:p) c (BoardRow i1 x1 x2 x3 x4 x5 x6 x7 x8) = BoardRow i1 x1 x2 x3 x4 x5 (paste p c x6) x7 x8
  paste (6:p) c (BoardRow i1 x1 x2 x3 x4 x5 x6 x7 x8) = BoardRow i1 x1 x2 x3 x4 x5 x6 (paste p c x7) x8
  paste (7:p) c (BoardRow i1 x1 x2 x3 x4 x5 x6 x7 x8) = BoardRow i1 x1 x2 x3 x4 x5 x6 x7 (paste p c x8)
  paste _     _ x                = x                   -- error

  alternatives _ = [("row",Clip_Nothing)]
  
  arity _        = 8
  
  hole = emptyRow
  


instance Editable BoardSquare where
  select []    x              = Clip_BoardSquare x
  select _     _              = Clip_Nothing

  paste [] (Clip_BoardSquare c) x     = c
  paste [] c                    x     = trace ("Type error: pasting "++show c++" on Decl") x
  paste _     _ x                     = x                   -- error

  alternatives _ = [ ("Wit:Koning", Clip_BoardSquare $ King NoIDD True)
                   , ("Wit:Dame", Clip_BoardSquare $ Queen NoIDD True)
                   , ("Wit:Loper", Clip_BoardSquare $ Bishop NoIDD True)
                   , ("Wit:Paard", Clip_BoardSquare $ Knight NoIDD True)
                   , ("Wit:Toren", Clip_BoardSquare $ Rook NoIDD True)
                   , ("Wit:Pion", Clip_BoardSquare $ Pawn NoIDD True)
                   , ("Zwart:Koning", Clip_BoardSquare $ King NoIDD False)
                   , ("Zwart:Dame", Clip_BoardSquare $ Queen NoIDD False)
                   , ("Zwart:Loper", Clip_BoardSquare $ Bishop NoIDD False)
                   , ("Zwart:Paard", Clip_BoardSquare $ Knight NoIDD False)
                   , ("Zwart:Toren", Clip_BoardSquare $ Rook NoIDD False)
                   , ("Zwart:Pion", Clip_BoardSquare $ Pawn NoIDD False)
                   , ("Leeg",   Clip_BoardSquare $ hole ) ]
  
  arity _            = 0
  
  hole = Empty 
  

---------------------------------------------------------------------------------------


instance Editable String_ where
  select []    x              = Clip_String_ x
  select (0:p) (String_ _ x1) = select p x1
  select _     _              = Clip_Nothing

  paste [] (Clip_String_ c) x      = c
  paste [] c             x      =  trace ("Type error: pasting "++show c++" on String_") x
  paste (0:p) c (String_ i1 x1) = String_ i1 (paste p c x1) 
  paste _     _ x                = x                   -- error

  alternatives _ = [ ("String_ String", Clip_String_ $ String_ NoIDD hole)
                   , ("{String_}",      Clip_String_ $ hole ) ]
  
  arity (String_ _ _) = 1
  arity _             = 0
  
  hole = HoleString_





-- RUI does not generate special list stuff. 


instance Editable Slides where
  select []    x                  = Clip_Slides x
  select (0:p) (ConsSlides _ x _)   = select p x
  select (n:p) (ConsSlides _ _ xs)  = select (n-1:p) xs
  select _     _                  = Clip_Nothing

  paste [] (Clip_Slides c) xs        = c
  paste [] c              xs       = trace ("Type error: pasting "++show c++" on Slides") xs
  paste (0:p) c (ConsSlides i1 x xs) = ConsSlides i1 (paste p c x) xs
  paste (n:p) c (ConsSlides i1 x xs) = ConsSlides i1 x (paste (n-1:p) c xs) 
  paste _     _ xs                 = xs                   -- error

  alternatives _ = [ ("add {Slide}", Clip_Slide $ hole)
                   ]
                   --, ("[]",      Clip_Slides $ hole ) ]
  
  
  arity (ConsSlides _ _ xs) = 1 + arity xs
  arity _            = 0
  
  hole = NilSlides NoIDD

  isList _ = True
  
  insertList 0 (Clip_Slide x)  xs = Clip_Slides $ ConsSlides NoIDD x xs
  insertList 0 _              xs = trace "Type error, no paste" $ Clip_Slides $ xs
  insertList n c (ConsSlides i1 x xs) = let (Clip_Slides xs') = insertList (n-1) c xs
                                       in  Clip_Slides $ ConsSlides i1 x xs'
  insertList _ c xs                   = Clip_Slides xs

  removeList 0 (ConsSlides i1 x xs) = Clip_Slides $ xs
  removeList n (ConsSlides i1 x xs) = let (Clip_Slides xs') = removeList (n-1) xs
                                     in  Clip_Slides $ ConsSlides i1 x xs'
  removeList _ xs                  = Clip_Slides xs
 


instance Editable Items where
  select []    x                  = Clip_Items x
  select (0:p) (ConsItems _ x _)   = select p x
  select (n:p) (ConsItems _ _ xs)  = select (n-1:p) xs
  select _     _                  = Clip_Nothing

  paste [] (Clip_Items c) xs        = c
  paste [] c              xs       = trace ("Type error: pasting "++show c++" on Items") xs
  paste (0:p) c (ConsItems i1 x xs) = ConsItems i1 (paste p c x) xs
  paste (n:p) c (ConsItems i1 x xs) = ConsItems i1 x (paste (n-1:p) c xs) 
  paste _     _ xs                 = xs                   -- error

  alternatives _ = [ ("add {Item}", Clip_Item $ hole)
                   ]
                   --, ("[]",      Clip_Items $ hole ) ]
  
  
  arity (ConsItems _ _ xs) = 1 + arity xs
  arity _            = 0
  
  hole = NilItems NoIDD

  isList _ = True
  
  insertList 0 (Clip_Item x)  xs = Clip_Items $ ConsItems NoIDD x xs
  insertList 0 _              xs = trace "Type error, no paste" $ Clip_Items $ xs
  insertList n c (ConsItems i1 x xs) = let (Clip_Items xs') = insertList (n-1) c xs
                                       in  Clip_Items $ ConsItems i1 x xs'
  insertList _ c xs                   = Clip_Items xs

  removeList 0 (ConsItems i1 x xs) = Clip_Items $ xs
  removeList n (ConsItems i1 x xs) = let (Clip_Items xs') = removeList (n-1) xs
                                     in  Clip_Items $ ConsItems i1 x xs'
  removeList _ xs                  = Clip_Items xs
 



------- Generated


instance Editable PPPresentation where
  select []    x                   = Clip_PPPresentation x
  select (0:p) (PPPresentation _ x1 x2) = select p x1
  select (1:p) (PPPresentation _ x1 x2) = select p x2
  select _     _                   = Clip_Nothing

  paste [] (Clip_PPPresentation c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on PPPresentation")   x
  paste (0:p) c (PPPresentation i1 x1 x2) = PPPresentation i1 (paste p c x1) x2
  paste (1:p) c (PPPresentation i1 x1 x2) = PPPresentation i1 x1 (paste p c x2)
  paste _  _  x                    = x

  alternatives _ = [("PPPresentation  {Bool}{Slides}"  , Clip_PPPresentation $ PPPresentation NoIDD hole hole)
                   ,("{PPPresentation}", Clip_PPPresentation hole)
                   ]

  arity (PPPresentation _ x1 x2) = 2
  arity _                        = 0

  hole = initPPPresentation




instance Editable Slide where
  select []    x                   = Clip_Slide x
  select (0:p) (Slide _ x1 x2) = select p x1
  select (1:p) (Slide _ x1 x2) = select p x2
  select _     _                   = Clip_Nothing

  paste [] (Clip_Slide c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Slide")   x
  paste (0:p) c (Slide i1 x1 x2) = Slide i1 (paste p c x1) x2
  paste (1:p) c (Slide i1 x1 x2) = Slide i1 x1 (paste p c x2)
  paste _  _  x                    = x

  alternatives _ = [("Slide  {String}{ItemList}"  , Clip_Slide $ Slide NoIDD hole hole)
                   ,("{Slide}", Clip_Slide hole)
                   ]

  arity (Slide _ x1 x2) = 2
  arity _                        = 0

  hole = Slide NoIDD (String_ NoIDD "<new>") hole


instance Editable ItemList where
  select []    x                   = Clip_ItemList x
  select (0:p) (ItemList _ x1 x2) = select p x1
  select (1:p) (ItemList _ x1 x2) = select p x2
  select _     _                   = Clip_Nothing

  paste [] (Clip_ItemList c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on ItemList")   x
  paste (0:p) c (ItemList i1 x1 x2) = ItemList i1 (paste p c x1) x2
  paste (1:p) c (ItemList i1 x1 x2) = ItemList i1 x1 (paste p c x2)
  paste _  _  x                    = x

  alternatives _ = [("ItemList  {ListType}{Items}"  , Clip_ItemList $ ItemList NoIDD hole hole)
                   ,("{ItemList}", Clip_ItemList hole)
                   ]

  arity (ItemList _ x1 x2) = 2
  arity _                        = 0

  hole = ItemList NoIDD (Bullet NoIDD) (ConsItems NoIDD hole (NilItems NoIDD))


instance Editable ListType where
  select []    x                   = Clip_ListType x
  select _     _                   = Clip_Nothing

  paste [] (Clip_ListType c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on ListType")   x
  paste _  _  x                    = x

  alternatives _ = [("Bullet  "  , Clip_ListType $ Bullet NoIDD)
                   ,("Number  "  , Clip_ListType $ Number NoIDD)
                   ,("Alpha  "  , Clip_ListType $ Alpha NoIDD)
                   ,("{ListType}", Clip_ListType hole)
                   ]

  arity (Bullet _) = 0
  arity (Number _) = 0
  arity (Alpha _) = 0
  arity _                        = 0

  hole = Bullet NoIDD




instance Editable Item where
  select []    x                   = Clip_Item x
  select (0:p) (StringItem _ x1) = select p x1
  select (0:p) (HeliumItem _ x1) = select p x1
  select (0:p) (ListItem _ x1) = select p x1
  select _     _                   = Clip_Nothing

  paste [] (Clip_Item c) _      = c
  paste [] c  x                    = trace ("Type error: pasting "++show c++" on Item")   x
  paste (0:p) c (StringItem i1 x1) = StringItem i1 (paste p c x1)
  paste (0:p) c (HeliumItem i1 x1) = HeliumItem i1 (paste p c x1)
  paste (0:p) c (ListItem i1 x1) = ListItem i1 (paste p c x1)
  paste _  _  x                    = x

  alternatives _ = [("StringItem  {String}"  , Clip_Item $ StringItem NoIDD hole)
                   ,("HeliumItem  {Exp}"  , Clip_Item $ HeliumItem NoIDD hole)
                   ,("ListItem  {ItemList}"  , Clip_Item $ ListItem NoIDD hole)
                   ,("{Item}", Clip_Item hole)
                   ]

  arity (StringItem _ x1) = 1
  arity (HeliumItem _ x1) = 1
  arity (ListItem _ x1) = 1
  arity _                        = 0

  hole = HoleItem




instance Editable Bool where
                         
  select [] x = Clip_Bool x                            
  select _  _ = Clip_Nothing                           
  paste [] (Clip_Bool c) x = c                         
  paste [] c             x =  trace ("Type error: pasting "++show c++" on Bool") x

  alternatives _ = [ ("True", Clip_Bool True)        
                   , ("False", Clip_Bool False)      
                   , ("{Bool}", Clip_Bool hole) ]    

  arity _ = 0                                          
  hole = False                                         


instance Editable String where
  select [] x = Clip_String x
  select _  _ = Clip_Nothing
  paste [] (Clip_String c) x = c
  paste [] c             x =  trace ("Type error: pasting "++show c++" on String") x

  alternatives _ = [ ("a", Clip_String "a")
                   , ("ab", Clip_String "ab")
                   , ("{String}", Clip_String hole) ] 

  arity _ = 0
  hole = "{String}"


