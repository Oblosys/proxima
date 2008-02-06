module Main where

import Proxima

import Evaluator
import Reducer
import PresentationAG
import ProxParser
import ScannerSheet

import DocTypes
import DocTypes_Generated
import EnrTypes
import PresTypes
import LayTypes
import ArrTypes
import CommonTypes

gain = main -- when typing during compilation GHCI replaces the first command line char by 'g'

main = proxima PresentationAG.sem_EnrichedDoc
               ProxParser.recognizeRootEnr
               ScannerSheet.scanner 
               -- sheet parameters (evaluation and reduction sheets are passed implicitly through
               -- instances of Evaluation/ReductionSheet classes)
               --
               (DocumentLevel HoleDocument NoPathD Clip_Nothing)   
               (EnrichedDocLevel HoleEnrichedDoc NoPathD)   
