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

import Control.Exception

gain = main -- when typing during compilation GHCI replaces the first command line char by 'g'

main = proxima Evaluator.evaluationSheet
               Reducer.reductionSheet
               PresentationAG.sem_EnrichedDoc
               ProxParser.parsePres 
               Scanner.tokenize -- sheet parameters
               (DocumentLevel HoleDocument NoPathD Clip_Nothing)   
               (EnrichedDocLevel HoleEnrichedDoc NoPathD)   
  `Control.Exception.catch`
   \err -> 
    do { putStrLn "\n\n\nProxima main terminated abnormally:\n" 
       ; print err
       ; putStrLn "\n<Press a key to exit>"
       ; getLine
       ; return ()
       } -- This way, the dos window on Windows does not exit until the user can see the error.
