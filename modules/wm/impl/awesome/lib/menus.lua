local menus = {}

local awful = require("awful")
local beautiful = require("beautiful")
local menubar = require("menubar")

-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", "@defaultVTExecCmd@ -e man awesome" },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", "@defaultVTCmd@" },
                                    { "open browser", "firefox" },
                                    { "open Emacs", "emacs" }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = "@defaultVTCmd@" -- Set the terminal for applications that require it

function menus:show_apps_menu()
   -- If you want to always position the menu on the same place set coordinates
   awful.menu.menu_keys.down = { "Down", "Alt_L" }
   local cmenu = awful.menu.clients({width=245}, { keygrabber=true, coords={x=525, y=330} })
end


return menus
