module DocumentEdit where


{-

Defines document edit (structure edit) functions


Observations:

Document root is different from the rest of the elements. It cannot be selected or deleted, so no hole is needed either.

Basic values should probably be boxed in some way, so there are holes for them.

-}

import CommonTypes
import DocTypes
import DocUtils
import PresTypes

import List
import IOExts

import DocumentEdit_Generated 


{-
type FocusDoc = PathDoc
data PathDoc = NoPathD | PathD PathD deriving Show


data ID = NoID | ID Int deriving Show
type Presentation = Int
type Node = Int

data DocumentLevel = DocumentLevel Document FocusDoc ClipDoc


data Document = RootDoc ID ID Decls

data Decls = ConsDecls ID Decl Decls
           | NilDecls ID
           | HoleDecls
           | ParseErrDecls Node Presentation

data Decl = Decl ID ID ID Ident Exp
          | HoleDecl
          | ParseErrDecl Node Presentation


data Ident = Ident ID ID String
           | HoleIdent
           | ParseErrIdent Node Presentation

data Exp = PlusExp ID ID Exp Exp 
         | TimesExp ID ID Exp Exp
         | DivExp ID ID Exp Exp
         | PowerExp ID ID Exp Exp
         | BoolExp ID ID Bool
         | IntExp ID ID Int
         | CaseExp ID ID ID Exp Alts
         | LetExp ID ID ID Decls Exp
         | LamExp ID ID ID Ident Exp
         | AppExp ID Exp Exp
         | IdentExp ID Ident
         | IfExp ID ID ID ID Exp Exp Exp
         | ParenExp ID ID ID Exp
         | ProductExp ID ID ID [ID] Exps
         | HoleExp
         | ParseErrExp Node Presentation
         


instance Show Document where
  show (RootDoc _ _ decls) = "Document\n"++show decls

instance Show Decls where
  show (ConsDecls _ d ds) = show d ++ show ds
  show (NilDecls _)      = ""
  show HoleDecls           = "{Decls}"
  show (ParseErrDecls _ _) = "<<PARSE ERROR DECLS>>"

instance Show Decl where
 show (Decl _ _ _ ident exp)     = show ident ++ " = " ++ show exp ++ ";\n"
 show HoleDecl           = "{Decl}"
 show (ParseErrDecl _ _) = "<<PARSE ERROR DECL>>"

instance Show Ident where
 show (Ident _ _ str)     = str
 show HoleIdent           = "{Ident}"
 show (ParseErrIdent _ _) = "<<PARSE ERROR IDENT>>"
 
instance Show Exp where
  show (PlusExp _ _ exp1 exp2)       = show exp1 ++ "+" ++ show exp2
  show (TimesExp _ _ exp1 exp2)      = show exp1 ++ "*" ++ show exp2
  show (DivExp _ _ exp1 exp2)        = show exp1 ++ "/" ++ show exp2
  show (PowerExp _ _ exp1 exp2)      = show exp1 ++ "^" ++ show exp2
  show (BoolExp _ _ bool)          = show bool
  show (IntExp _ _ int)            = show int
  show (CaseExp _ _ _ exp alts)    = "case" ++ show exp ++ " of " ++ show alts
  show (LetExp _ _ _ decls exp)    = "let" ++ show decls ++ " in " ++ show exp
  show (LamExp _ _ _ ident exp)    = "\\" ++ show ident ++ " -> " ++ show exp
  show (AppExp _ exp1 exp2)          = show exp1 ++ " " ++ show exp2
  show (IdentExp _ ident)          = show ident
  show (IfExp _ _ _ _ exp1 exp2 exp3) = "if " ++ show exp1 ++ " then " ++ show exp2  ++ " else " ++ show exp3
  show (ParenExp _ _ _ exp)        = "(" ++ show exp ++ ")"
  show (ProductExp _ _ _ _ exps)   = "(" ++ concat (intersperse ", " (map show (lstFromExps exps))) ++ ")"
  show HoleExp                     = "{Exp}"
  show (ParseErrExp _ _)           = "<<PARSE ERROR EXP>>"
  
lstFromExps :: Exps -> [Exp]
lstFromExps (ConsExps _ e es) = e:lstFromExps es
lstFromExps (NilExps _)      = []
lstFromExps HoleExps           = []
lstFromExps (ParseErrExps _ _) = []

data Exps = ConsExps ID Exp Exps
           | NilExps ID
           | HoleExps
           | ParseErrExps Node Presentation

instance Show Exps where
  show (ConsExps _ d ds) = show d ++ show ds
  show (NilExps _)      = ""
  show HoleExps           = "{Exps}"
  show (ParseErrExps _ _) = "<<PARSE ERROR EXPS>>"





-- clip is not a good name, since nothing is clipped, maybe node is ok after all
data ClipDoc = 
            Clip_Int Int
          | Clip_String String
          | Clip_Bool Bool
          
          | Clip_Decls Decls
          | Clip_Decl Decl
          | Clip_Ident Ident
          | Clip_Exp Exp
          | Clip_Exps Exps
          | Clip_Alts Alts
          | Clip_Alt Alt
          
          | Clip_Nothing deriving Show
-}



-- Generic part

  
navigateUpD :: FocusDoc -> Document -> FocusDoc
navigateUpD NoPathD         _  = NoPathD
navigateUpD (PathD [])      _  = NoPathD
navigateUpD (PathD p@(_:_)) _  = PathD $ init p
navigateUpD pd              _ = pd

navigateDownD :: FocusDoc -> Document -> FocusDoc
navigateDownD NoPathD   d = PathD []
navigateDownD (PathD p) d = PathD $ if arityD p d > 0 then p++[0] else p
 
navigateLeftD :: FocusDoc -> Document -> FocusDoc
navigateLeftD NoPathD         _ = NoPathD
navigateLeftD (PathD p@(_:_)) d = PathD $ let i = last p
                                          in  if i > 0 then init p ++ [i-1] else p
navigateLeftD pd              _ = pd

navigateRightD :: FocusDoc -> Document -> FocusDoc
navigateRightD NoPathD         _ = NoPathD
navigateRightD (PathD p@(_:_)) d = PathD $ let i = last p
                                               a = arityD (init p) d
                                           in  if i < a-1 then init p ++ [i+1] else p
navigateRightD pd              _ = pd




editCopyD :: DocumentLevel -> DocumentLevel
editCopyD (DocumentLevel doc NoPathD clip)        = DocumentLevel doc NoPathD clip
editCopyD (DocumentLevel doc pd@(PathD pth) clip) = DocumentLevel doc pd (selectD pth doc)

editCutD :: DocumentLevel -> DocumentLevel
editCutD  (DocumentLevel doc NoPathD clip)           = DocumentLevel doc NoPathD clip
editCutD  (DocumentLevel doc pd@(PathD pth) clip)    = let (doc', pd') = deleteD pth doc
                                                       in  DocumentLevel doc' pd' (selectD pth doc)

editDeleteD :: DocumentLevel -> DocumentLevel
editDeleteD (DocumentLevel doc NoPathD clip)        = DocumentLevel doc NoPathD clip
editDeleteD (DocumentLevel doc pd@(PathD pth) clip) =  let (doc', pd') = deleteD pth doc
                                                       in  DocumentLevel doc' pd' clip

editPasteD :: DocumentLevel -> DocumentLevel
editPasteD (DocumentLevel doc NoPathD clip)         = DocumentLevel doc NoPathD clip
editPasteD (DocumentLevel doc pd@(PathD pth) clip)  = DocumentLevel (pasteD pth clip doc) pd clip

-- menuD is probably not a good name
menuD :: PathDoc -> Document -> [ (String, DocumentLevel -> DocumentLevel) ]
menuD NoPathD _              = []
menuD path@(PathD p) d@(RootDoc id1 id2  x) = 
  let alts = alternativesD p d
      mkItem (s,c) = (s, \(DocumentLevel _ pth clip) -> DocumentLevel (pasteD p c d) pth clip)
  in  [ ("<cut>", \(DocumentLevel d _ clip) -> let (d',p') = deleteD p d 
                                               in  DocumentLevel d' p' (selectD p d) ) 
      , ("<copy>", \(DocumentLevel d _ clip) -> DocumentLevel d path (selectD p d) ) 
      , ("<paste>", \(DocumentLevel d _ clip) -> DocumentLevel (pasteD p clip d) path clip )
      , ("<select>", \(DocumentLevel d _ clip) -> DocumentLevel d path clip ) ]
      ++ map mkItem alts   -- use this one or the one in the argument? They should be the same
      ++ if null p then [] else
           let parent = (selectD (init p) d)
           in if not (isListClip parent) then [] else
                let alts2 = [ ("add "++s, insertListClip (last p+1) c parent) | (s,c) <- alts]
                    mkItem2 (s,c) = (s, \(DocumentLevel _ pth clip) -> DocumentLevel (pasteD (init p) c d) pth clip)
                    pasteBefore = ("<paste before>", \(DocumentLevel _ pth clip) -> 
                                                     DocumentLevel (pasteD (init p) (insertListClip (last p) clip parent) d) pth clip )
                    pasteAfter = ("<paste after>", \(DocumentLevel _ pth clip) -> 
                                                     DocumentLevel (pasteD (init p) (insertListClip (last p+1) clip parent) d) pth clip )
                in  map mkItem2 alts2 ++ [pasteBefore,pasteAfter]



selectD p (RootDoc id1 id2  x) = select p x

pasteD p c (RootDoc id1 id2  x) = RootDoc id1 id2 $ paste p c x

-- ugly mix of levels, find out how to do it nicely
deleteD :: PathD -> Document -> (Document, PathDoc)
deleteD p d = if not (null p) && isListClip (selectD (init p) d) -- if parent is list, then delete is remove from list
              then (pasteD (init p) (removeListClip (last p) (selectD (init p) d)) d, NoPathD)
              else let newhole = holeClip (selectD p d)
                   in  (pasteD p newhole d, PathD p) 

arityD :: PathD -> Document -> Int
arityD p (RootDoc id1 id2  x) = arityClip (select p x)

alternativesD :: PathD -> Document -> [ (String, ClipDoc) ]
alternativesD p (RootDoc id1 id2  x) = alternativesClip (select p x)

 
-- a simple structure editor:

main = do { putStrLn "\n\n\n\n****** Simple structural editor for testing DocumentEdit module ******"
          ; edit (DocumentLevel sample NoPathD Clip_Nothing)
          }

edit :: DocumentLevel -> IO ()
edit doclvl@(DocumentLevel doc path clip) =
 do { putStrLn $ "\n\ndoc  "++ show doc
    ; putStrLn $ "\npath "++ show path ++ case path of NoPathD   -> ""
                                                       PathD pth -> " = " ++ show (selectD pth doc) 
    ; putStrLn $ "\nclip "++ show clip
    
    ; c <- getChar
    ; putStrLn ""
    
    ; (doclvl', str) <- case c of '\'' -> return (DocumentLevel doc (navigateUpD path doc) clip, "Up")
                                  '/' -> return (DocumentLevel doc (navigateDownD path doc) clip, "Down")
                                  ',' -> return (DocumentLevel doc (navigateLeftD path doc) clip, "Left")
                                  '.' -> return (DocumentLevel doc (navigateRightD path doc) clip, "Right")
                                  'c' -> return (editCopyD doclvl, "Copy")
                                  'v' -> return (editPasteD doclvl, "Paste")
                                  'x' -> return (editCutD doclvl, "Cut")
                                  'i' -> insertElt doclvl 0
                                  'q' -> return (doclvl, "Quit")
                                  _   -> return (doclvl, "Unknown command")
    ; putStrLn str
    ; if c /= 'q' then edit doclvl' else return ()
    }
    
insertElt :: DocumentLevel -> Int -> IO (DocumentLevel, String)
insertElt doclvl@(DocumentLevel doc path clp) i =
 do { let menu = menuD path doc
    ; putStrLn $ concat $ intersperse " | " $
                   [ mark (i==ix) str | ((str, clp),ix) <- zip menu [0..] ]
    ; c <- getChar
    ; case c of '\'' -> insertElt doclvl (if i > 0 then (i-1) else i)
                '/'  -> insertElt doclvl (if i < length menu - 1 then (i+1) else i)
                ','  -> insertElt doclvl (if i > 0 then (i-1) else i)
                '.'  -> insertElt doclvl (if i < length menu - 1 then (i+1) else i)
                '\n' -> let (itemStr, upd) = menu !! i in return (upd doclvl, "paste: "++itemStr)
                _    -> insertElt doclvl i
    }
 where mark True  str = ">"++str++"<"
       mark False str = " "++str++" " 

-- Document type specific part

sample = RootDoc NoIDD NoIDP $
           ConsDecls NoIDD (Decl NoIDD NoIDP NoIDP NoIDP NoIDP True True (Ident NoIDD NoIDP NoIDP "tup") 
                                (ProductExp NoIDD NoIDP NoIDP [] $ 
                                   ConsExps NoIDD (PlusExp NoIDD NoIDP (IntExp NoIDD NoIDP 2) (IntExp NoIDD NoIDP  3)) $
                                   ConsExps NoIDD (IntExp NoIDD NoIDP 2) $
                                   NilExps NoIDD) 
                          ) $
           ConsDecls NoIDD (Decl NoIDD NoIDP NoIDP NoIDP NoIDP True True (Ident NoIDD NoIDP NoIDP "f") (PlusExp NoIDD NoIDP (IntExp NoIDD NoIDP 2) (IntExp NoIDD NoIDP  3))
                          ) $
           ConsDecls NoIDD (Decl NoIDD NoIDP NoIDP NoIDP NoIDP True True (Ident NoIDD NoIDP NoIDP "g") (IntExp NoIDD NoIDP 1) 
                          ) $
           NilDecls NoIDD


-- functions that work with select need a ..Clip definition. Maybe this can be eliminated by using
-- some type trickery

