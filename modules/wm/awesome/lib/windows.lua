local windows = {}

local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")

local lain = require("lain")

local controls = require("controls")
local defs = require("defs")
local themes = require("themes")
local widgets = require("widgets")

windows.layouts = {
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.spiral,
    awful.layout.suit.max,
    lain.layout.termfair,
    lain.layout.centerfair,
    lain.layout.cascade,
    lain.layout.cascadetile,
    lain.layout.centerwork,
}

-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = controls.clientkeys,
                     buttons = controls.clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
    } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    { rule = { class = "Firefox", instance = "Navigator" },
      properties = { tag = "ω", floating = false } },
    { rule = { class = "Firefox", instance = "Dialog" },
      properties = { floating = true },
      callback = function(t)
         client.focus.move_to_screen(client.focus.screen)
         client.focus.move_to_tag(t)
    end },
    { rule = { class = "Google-chrome" },
      properties = { tag = "∀" } },
    { rule = { class = "Emacs" },
      properties = { tag = "λ" } },
    { rule = { class = "URxvt" },
      properties = { tag = "λ" } },
    { rule = { class = "Sakura" },
      properties = { tag = "λ" } },
    { rule = { class = "FBReader" },
      properties = { tag = "⧉" } },
    { rule = { class = "Zathura" },
      properties = { tag = "⧉" } },
    { rule = { class = "Vlc" },
      properties = { tag = "∀" } },
}

-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) and defs.sloppy_focus then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.connect_signal("focus", function(c) c.border_color = "#4286f4" end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

client.connect_signal("unfocus", function(c) c.fullscreen = false end )

awful.screen.connect_for_each_screen(function(s)
    themes:set_wallpaper(s)
    awful.tag({ "ω", "λ", "⧉", "∀" }, s, windows.layouts[2])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(awful.util.table.join(
                              awful.button({ }, 1, function () awful.layout.inc(windows.layouts, 1) end),
                              awful.button({ }, 3, function () awful.layout.inc(windows.layouts, -1) end),
                              awful.button({ }, 4, function () awful.layout.inc(windows.layouts, 1) end),
                              awful.button({ }, 5, function () awful.layout.inc(windows.layouts, -1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, controls.taglist_buttons)

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, controls.tasklist_buttons)

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(s.mytaglist)
    left_layout:add(s.mypromptbox)

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    -- right_layout:add(mpdwidget)
    -- right_layout:add(separator)
    right_layout:add(wibox.widget.systray())
    right_layout:add(separator)
    right_layout:add(wlan_widget)
    right_layout:add(separator)
    right_layout:add(volume_widget)
    right_layout:add(separator)
    right_layout:add(cpuwidget)
    right_layout:add(separator)
    right_layout:add(batwidget)
    right_layout:add(separator)
    right_layout:add(kbdwidget)
    right_layout:add(mytextclock)
    right_layout:add(s.mylayoutbox)

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(s.mytasklist)
    layout:set_right(right_layout)

    s.mywibox:set_widget(layout)
end)

beautiful.master_count = 2

return windows
