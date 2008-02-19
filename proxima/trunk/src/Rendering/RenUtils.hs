module RenUtils where

import CommonTypes
--import RenTypesGTK
import RenTypes

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
