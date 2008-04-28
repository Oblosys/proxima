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

data EditEnrichedDoc' enr doc =
    SetEnr' (EnrichedDocLevel enr doc)
  | SkipEnr' Int deriving Show

data EditEnrichedDoc documentLevel enr doc =
     InitEnr
   | SetEnr (EnrichedDocLevel enr doc)
   | SkipEnr Int
   | OpenFileEnr String
   | SaveFileEnr String
   | EvaluateDocEnr
   | UndoDocEnr
   | RedoDocEnr
   | UpdateDocEnr (documentLevel -> documentLevel) -- should encapsulate these so they automatically go to doc level
   | NavPathDocEnr PathDoc
   | NavUpDocEnr
   | NavDownDocEnr
   | NavLeftDocEnr
   | NavRightDocEnr
   | CutDocEnr
   | CopyDocEnr
   | PasteDocEnr
   | DeleteDocEnr deriving Show

