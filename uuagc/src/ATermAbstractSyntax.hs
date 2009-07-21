{-----------------------------------------------------------------------------

	Haskell ATerm Library
		
	Joost Visser
	CWI, Amsterdam

  This module is part of the ATerm library for Haskell. It defines the
  abstract syntax of ATerms as a Haskell datatype.
  
------------------------------------------------------------------------------}

module ATermAbstractSyntax where

-- Abstract syntax -----------------------------------------------------------

data ATerm = AAppl String [ATerm]
           | AList [ATerm]
           | AInt Integer
           | AString String
           deriving (Read,Show,Eq,Ord)

------------------------------------------------------------------------------
