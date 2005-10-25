module EnrTypes (module DocTypes, module EnrTypes, FocusDoc) where

import CommonTypes
import DocTypes 
import DocTypes_Generated

import CommonUtils

import PresTypes -- for inserted/deleted token hack

-- EnrTypes imports and exports DocTypes because the level type is the same for both
-- levels. A separate module imported by both DocTypes and EnrTypes would be a bit
-- cleaner, as Document specific definitions (eg. DocumentLevel) are visible at
-- EnrichedDocLevel. This is not really a problem, however.

data EnrichedDocLevel enr = EnrichedDocLevel enr FocusDoc deriving Show


-- data EnrichedDoc = ...
-- EnrichedDoc is defined in DocumentTypes_Generated because of node datatype dependency. 
-- TODO figure out where node should go, and clean up

data EditEnrichedDoc' enr =
    SetEnr' (EnrichedDocLevel enr) -- (InsertedTokenList, DeletedTokenMap)
  | SkipEnr' Int

data EditEnrichedDoc documentLevel enr =
     InitEnr
   | SetEnr (EnrichedDocLevel enr)
   | SkipEnr Int
   | OpenFileEnr String
   | SaveFileEnr String
   | EvaluateDocEnr
   | UpdateDocEnr (documentLevel -> documentLevel) -- should encapsulate these so they automatically go to doc level
   | NavUpDocEnr
   | NavDownDocEnr
   | NavLeftDocEnr
   | NavRightDocEnr
   | CutDocEnr
   | CopyDocEnr
   | PasteDocEnr
   | DeleteDocEnr deriving Show

