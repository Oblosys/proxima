

-- UUAGC 0.9.10 (Interfaces.ag)
module Interfaces where

import CommonTypes
import SequentialTypes
-- IRoot -------------------------------------------------------
{-
   alternatives:
      alternative IRoot:
         child inters         : Interfaces 
-}
data IRoot  = IRoot (Interfaces) 
-- Interface ---------------------------------------------------
{-
   alternatives:
      alternative Interface:
         child nt             : {NontermIdent}
         child cons           : {[ConstructorIdent]}
         child seg            : Segments 
-}
data Interface  = Interface (NontermIdent) ([ConstructorIdent]) (Segments) 
-- Interfaces --------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Interface 
         child tl             : Interfaces 
      alternative Nil:
-}
type Interfaces  = [(Interface)]
-- Segment -----------------------------------------------------
{-
   alternatives:
      alternative Segment:
         child inh            : {[Vertex]}
         child syn            : {[Vertex]}
-}
data Segment  = Segment ([Vertex]) ([Vertex]) 
-- Segments ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Segment 
         child tl             : Segments 
      alternative Nil:
-}
type Segments  = [(Segment)]