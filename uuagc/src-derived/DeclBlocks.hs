

-- UUAGC 0.9.10 (DeclBlocks.ag)
module DeclBlocks where

import Code (Decl,Expr)
-- DeclBlocks --------------------------------------------------
{-
   alternatives:
      alternative DeclBlock:
         child defs           : {[Decl]}
         child visit          : {Decl}
         child next           : DeclBlocks 
      alternative DeclTerminator:
         child defs           : {[Decl]}
         child result         : {Expr}
-}
data DeclBlocks  = DeclBlock ([Decl]) (Decl) (DeclBlocks) 
                 | DeclTerminator ([Decl]) (Expr) 
-- DeclBlocksRoot ----------------------------------------------
{-
   alternatives:
      alternative DeclBlocksRoot:
         child blocks         : DeclBlocks 
-}
data DeclBlocksRoot  = DeclBlocksRoot (DeclBlocks) 