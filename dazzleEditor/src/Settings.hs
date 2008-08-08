-----------------------------------------------------------------------------------------
{-| Module      : Settings
    Copyright   : (c) 2007 Martijn Schrage
    License     : All Rights Reserved

    Maintainer  : martijn@cs.uu.nl
    Stability   : experimental
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Settings (settings) where

import Common.CommonTypes

-- Constants for switching on incrementality, special markings in the rendering, etc.

-- Settings for the ArrangerAG are still defined in CommonUtils, since it would add to the
-- run-time cost of the arranger to pass the record around as an inherited attribute.

                
settings = defaultSettings { applicationName = "Dazzle documentation editor"

                           , rendererIncrementality = False
                           , arrangerIncrementality = True

                               -- use a smaller rectangle as viewed area to see what happens outside it
                           , reducedViewedArea = False            

                               -- updated parts of the rendering are surrounded by red rectangles
                           , markUpdatedRenderingArea = False
                           , serverMode = True
                           }
           