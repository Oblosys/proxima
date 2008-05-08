module Evaluation.EnrTypes (module Evaluation.DocTypes, module Evaluation.EnrTypes) where

import Common.CommonTypes
import Evaluation.DocTypes 

import Common.CommonUtils


-- EnrTypes imports and exports DocTypes because the level type is the same for both
-- levels. A separate module imported by both DocTypes and EnrTypes would be a bit
-- cleaner, as Document specific definitions (eg. DocumentLevel) are visible at
-- EnrichedDocLevel. This is not really a problem, however.

data EnrichedDocLevel enr doc = EnrichedDocLevel enr FocusDoc doc deriving Show
                                                       -- Document is here because of the popupMenu hack

-- data EnrichedDoc = ...
-- EnrichedDoc is defined in DocumentTypes_Generated because of node datatype dependency. 
-- TODO figure out where node should go, and clean up

data EditEnrichedDoc'_ wrapped doc enr node clip token =
    SetEnr' (EnrichedDocLevel enr doc)
  | SkipEnr' Int
  | WrapEnr' wrapped deriving Show

data EditEnrichedDoc_ wrapped doc enr node clip token =
     InitEnr
   | SetEnr (EnrichedDocLevel enr doc)
   | SkipEnr Int
   | OpenFileEnr String
   | SaveFileEnr String
   | WrapEnr wrapped deriving Show

