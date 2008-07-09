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

-- Constants for switching on incrementality, background coloring, etc.
                
settings =
  Settings { rendererIncrementality = False
           , arrangerIncrementality = False

             -- use a smaller rectangle as viewed area to see what happens outside it
           , reducedViewedArea = False            

             -- mark what parts of the arrangement were reused from the previous one
           , markArrangementBackground = False

             -- updated parts of the rendering are surrounded by red rectangles
           , markUpdatedRenderingArea = False
           }