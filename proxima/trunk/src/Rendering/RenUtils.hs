{-# LANGUAGE CPP #-}
module Rendering.RenUtils where

import Common.CommonTypes
import Rendering.RenTypes

import Graphics.UI.Gtk

mkMenu :: [(String, IO ())] -> IO Menu
mkMenu items =
 do { menu <- menuNew
    ; menuItems <- mapM mkMenuItem items 
    ; mapM (menuShellAppend menu) menuItems
    ; return menu
    }
 where mkMenuItem (label, action) =
        do { menuItem <- menuItemNewWithMnemonic label
           ; onActivateLeaf menuItem action
           ; return menuItem
           }

-- | Convert an rgb 3-tuple to a GTK color
gtkColor :: Common.CommonTypes.Color -> Graphics.UI.Gtk.Color
gtkColor (r, g, b) = Color (256*fromIntegral r) (256*fromIntegral g) (256*fromIntegral b)
