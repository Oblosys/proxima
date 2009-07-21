

-- UUAGC 0.9.10 (ErrorMessages.ag)
module ErrorMessages where

import UU.Scanner.Position(Pos)
import Pretty
import DepTypes(Trace)
import CodeSyntax
import CommonTypes
-- Error -------------------------------------------------------
{-
   alternatives:
      alternative ChildAsLocal:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child var            : {Identifier}
      alternative CustomError:
         child isWarning      : {Bool}
         child pos            : {Pos}
         child mesg           : {PP_Doc}
      alternative CyclicSet:
         child name           : {Identifier}
      alternative DirectCirc:
         child nt             : {NontermIdent}
         child o_visit        : {Bool}
         child cyclic         : {[((Identifier,Identifier),[String],[String])]}
      alternative DupAlt:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child occ1           : {ConstructorIdent}
      alternative DupChild:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child name           : {Identifier}
         child occ1           : {Identifier}
      alternative DupInhAttr:
         child nt             : {NontermIdent}
         child attr           : {Identifier}
         child occ1           : {Identifier}
      alternative DupRule:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child attr           : {Identifier}
         child occ1           : {Identifier}
      alternative DupSet:
         child name           : {NontermIdent}
         child occ1           : {NontermIdent}
      alternative DupSig:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
      alternative DupSynAttr:
         child nt             : {NontermIdent}
         child attr           : {Identifier}
         child occ1           : {Identifier}
      alternative DupSynonym:
         child nt             : {NontermIdent}
         child occ1           : {NontermIdent}
      alternative DupUnique:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
      alternative InducedCirc:
         child nt             : {NontermIdent}
         child cinter         : {CInterface}
         child cyclic         : {[((Identifier,Identifier),[String],[String])]}
      alternative InstCirc:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
         child o_visit        : {Bool}
         child path           : {[String]}
      alternative LocalCirc:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
         child o_visit        : {Bool}
         child path           : {[String]}
      alternative MissingInstSig:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
      alternative MissingRule:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child attr           : {Identifier}
      alternative MissingTypeSig:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
      alternative MissingUnique:
         child nt             : {NontermIdent}
         child attr           : {Identifier}
      alternative ParserError:
         child pos            : {Pos}
         child problem        : {String}
         child action         : {String}
      alternative SuperfluousRule:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child attr           : {Identifier}
      alternative UndefAlt:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
      alternative UndefAttr:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child attr           : {Identifier}
         child isOut          : {Bool}
      alternative UndefChild:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child name           : {Identifier}
      alternative UndefLocal:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child var            : {Identifier}
      alternative UndefNont:
         child nt             : {NontermIdent}
-}
data Error  = ChildAsLocal (NontermIdent) (ConstructorIdent) (Identifier) 
            | CustomError (Bool) (Pos) (PP_Doc) 
            | CyclicSet (Identifier) 
            | DirectCirc (NontermIdent) (Bool) ([((Identifier,Identifier),[String],[String])]) 
            | DupAlt (NontermIdent) (ConstructorIdent) (ConstructorIdent) 
            | DupChild (NontermIdent) (ConstructorIdent) (Identifier) (Identifier) 
            | DupInhAttr (NontermIdent) (Identifier) (Identifier) 
            | DupRule (NontermIdent) (ConstructorIdent) (Identifier) (Identifier) (Identifier) 
            | DupSet (NontermIdent) (NontermIdent) 
            | DupSig (NontermIdent) (ConstructorIdent) (Identifier) 
            | DupSynAttr (NontermIdent) (Identifier) (Identifier) 
            | DupSynonym (NontermIdent) (NontermIdent) 
            | DupUnique (NontermIdent) (ConstructorIdent) (Identifier) 
            | InducedCirc (NontermIdent) (CInterface) ([((Identifier,Identifier),[String],[String])]) 
            | InstCirc (NontermIdent) (ConstructorIdent) (Identifier) (Bool) ([String]) 
            | LocalCirc (NontermIdent) (ConstructorIdent) (Identifier) (Bool) ([String]) 
            | MissingInstSig (NontermIdent) (ConstructorIdent) (Identifier) 
            | MissingRule (NontermIdent) (ConstructorIdent) (Identifier) (Identifier) 
            | MissingTypeSig (NontermIdent) (ConstructorIdent) (Identifier) 
            | MissingUnique (NontermIdent) (Identifier) 
            | ParserError (Pos) (String) (String) 
            | SuperfluousRule (NontermIdent) (ConstructorIdent) (Identifier) (Identifier) 
            | UndefAlt (NontermIdent) (ConstructorIdent) 
            | UndefAttr (NontermIdent) (ConstructorIdent) (Identifier) (Identifier) (Bool) 
            | UndefChild (NontermIdent) (ConstructorIdent) (Identifier) 
            | UndefLocal (NontermIdent) (ConstructorIdent) (Identifier) 
            | UndefNont (NontermIdent) 
-- Errors ------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Error 
         child tl             : Errors 
      alternative Nil:
-}
type Errors  = [(Error)]