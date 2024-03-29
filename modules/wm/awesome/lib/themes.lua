local themes = {}

local beautiful = require("beautiful") -- Theme handling library
local gears = require("gears") -- Theme handling library
local naughty = require("naughty")

local user_themes =
{
   "blue", "crown", "dk-grey",
   "dust", "lined", "matrix",
   "powerarrow-darker", "rbown",
   "smoked", "steamburn", "wombat",
   "zenburn-custom", "zenburn-red"
}
beautiful.init("/home/@mainUserName@/.config/awesome/themes/" .. user_themes[12] .. "/theme.lua")

theme.font = "@wmFontSimple@"

theme.taglist_font = "@wmFontSimple@"
theme.tasklist_font = "@wmFontSimple@"
theme.border_width = 2

naughty.config.defaults.font = "@wmFontSimple@"

theme.menu_height = "30"
theme.menu_width  = "400"

function themes:set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", themes.set_wallpaper)

return themes
