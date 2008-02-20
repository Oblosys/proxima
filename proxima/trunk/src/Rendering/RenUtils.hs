module Rendering.RenUtils where

import Common.CommonTypes
--import RenTypesGTK
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