module Main where

import Proxima.Proxima

import Settings
import Evaluator
import Reducer
import PresentationAG
import ProxParser
import ScannerSheetHS

import Evaluation.DocTypes
import DocTypes_Generated
import Evaluation.EnrTypes
import Presentation.PresTypes
import Layout.LayTypes
import Arrangement.ArrTypes
import Common.CommonTypes

gain = main -- when typing during compilation GHCI replaces the first command line char by 'g'

main = proxima Settings.settings
               PresentationAG.presentationSheet
               ProxParser.recognizeEnrichedDoc
               ScannerSheetHS.scanner 
               -- sheet parameters (evaluation and reduction sheets are passed implicitly through
               -- instances of Evaluation/ReductionSheet classes)
               --
               (DocumentLevel HoleDocument NoPathD Clip_Nothing)   
               (EnrichedDocLevel HoleEnrichedDoc NoPathD HoleDocument)   

-- Note: this file does not need to be changed when instantiating an editor.
