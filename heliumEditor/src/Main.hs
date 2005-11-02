module Main where

import Proxima

import Evaluator
import Reducer
import PresentationAG
import ProxParser
import Scanner

import DocTypes
import DocTypes_Generated
import EnrTypes
import PresTypes
import LayTypes
import ArrTypes
import CommonTypes



data C = CA A | CB B

data A = A B

data B = B


class E a c  where
  ed :: a -> c
  se :: c -> a

instance E A C where
  ed a = CA a
  se (CA a) = a

instance E B C where
  ed b = CB b
  se (CB b) = b

--test :: E a C => a
test = se (ed (A B))


gain = main -- when typing during compilation GHCI replaces the first command line char by 'g'

main = proxima Evaluator.evaluationSheet
               Reducer.reductionSheet
               PresentationAG.sem_EnrichedDoc 
               ProxParser.parsePres 
               Scanner.tokenize -- sheet parameters
               (DocumentLevel HoleDocument NoPathD Clip_Nothing)   
               (EnrichedDocLevel HoleEnrichedDoc NoPathD)   
