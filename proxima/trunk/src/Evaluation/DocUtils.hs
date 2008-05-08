module Evaluation.DocUtils where


import Evaluation.DocTypes
import Presentation.PresTypes
import Proxima.Wrap

import UU.Parsing
import UU.Parsing.CharParser

import Common.CommonTypes

redirect (SkipDoc i)     = (SkipDoc' i)
redirect (SetDoc doc)    = (SetDoc' doc)
redirect UndoDoc         = UndoDoc'
redirect RedoDoc         = RedoDoc'
redirect (UpdateDoc upd) = UpdateDoc' upd
redirect (NavPathDoc path) = NavPathDoc' path
redirect NavUpDoc        = NavUpDoc'
redirect NavDownDoc      = NavDownDoc'
redirect NavLeftDoc      = NavLeftDoc'
redirect NavRightDoc     = NavRightDoc'
redirect CutDoc          = CutDoc'
redirect CopyDoc         = CopyDoc'
redirect PasteDoc        = PasteDoc'
redirect DeleteDoc       = DeleteDoc'
redirect EvaluateDoc     = EvaluateDoc'
redirect (WrapDoc wrapped) = unwrap wrapped
redirect _               = (SkipDoc' 0)





showXML xml = 
{-     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  ++ case xml of (Elt tg _ _) -> "<!DOCTYPE "++tg++" SYSTEM \""++tg++".dtd\" >\n"
                 _            -> ""
  ++ -} showXML' 0 xml
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


-- Parsing


mkList listCns consCns nilCns lst = listCns $ foldr consCns nilCns lst

{- for the moment, a parseErr in the XML is parsed as a Hole, otherwise, the XML file needs to have
   an XML representation of the presentation argument of the parse error node.
-}
parseHoleAndParseErr typeStr holeConstructor = 
      holeConstructor <$ emptyTag ("Hole"++typeStr)
  <|> holeConstructor <$ emptyTag ("ParseErr"++typeStr)

     
startTag eltName = pCharSpaces *> (pCharString $ "<"++eltName++">") 
endTag   eltName = pCharSpaces *> (pCharString $ "</"++eltName++">")
emptyTag eltName = pCharSpaces *> (pCharString $ "<"++eltName++"/>")

pCharSpaces = pList (pSym ' ' <|> pSym '\n' <|> pSym '\r')

pCharString :: String -> CharParser String
pCharString str = pToks' str <?> str
 where pToks' :: (IsParser p s) => [s] -> p [s]
       pToks' []     = pSucceed []
       pToks' (a:as) = (:) <$> (pSym a <?> str) <*> pToks' as

