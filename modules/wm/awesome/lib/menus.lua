local menus = {}

local awful = require("awful")
local beautiful = require("beautiful")
local menubar = require("menubar")

local defs = require("defs")

-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", defs.terminal .. " -e man awesome" },
   { "edit config", defs.editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", defs.terminal },
                                    { "open browser", "firefox" },
                                    { "open Emacs", "emacs" }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = defs.terminal -- Set the terminal for applications that require it

function menus:show_apps_menu()
   -- If you want to always position the menu on the same place set coordinates
   awful.menu.menu_keys.down = { "Down", "Alt_L" }
   local cmenu = awful.menu.clients({width=245}, { keygrabber=true, coords={x=525, y=330} })
end


return menus
