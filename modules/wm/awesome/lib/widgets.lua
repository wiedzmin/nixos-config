local awful = require("awful")
local wibox = require("wibox")
local vicious = require("vicious")
local beautiful = require("beautiful")
local lain = require("lain")

mytextclock = wibox.widget.textclock()
theme.cal = lain.widget.cal({
    attach_to = { mytextclock },
    notification_preset = {
        font = "@wmFontSimple@",
        fg   = white,
        bg   = theme.bg_normal,
        followmouse = true
    }
})



cpuwidget = wibox.widget.graph()
cpuwidget:set_width(50)
cpuwidget:set_background_color("#494B4F")
cpuwidget:set_color({ type = "linear", from = { 0, 0 }, to = { 10,0 }, stops = { {0, "#FF5656"}, {0.5, "#88A175"},
                    {1, "#AECF96" }}})
vicious.register(cpuwidget, vicious.widgets.cpu, "$1")

kbdwidget = wibox.widget.textbox(" Eng ")
kbdwidget.border_width = 1
kbdwidget.border_color = beautiful.fg_normal
kbdwidget:set_text(" Eng ")
kbdstrings = {[0] = " Eng ",
              [1] = " Рус "}
dbus.request_name("session", "ru.gentoo.kbdd")
dbus.add_match("session", "interface='ru.gentoo.kbdd',member='layoutChanged'")
dbus.connect_signal("ru.gentoo.kbdd", function(...)
    local data = {...}
    local layout = data[2]
    kbdwidget:set_markup(kbdstrings[layout])
    end
)

mpdwidget = wibox.widget.textbox()
vicious.register(mpdwidget, vicious.widgets.mpd,
    function (mpdwidget, args)
        if args["{state}"] == "Stop" then
            return " - "
        else
            return args["{Artist}"]..' - '.. args["{Title}"]
        end
    end, 10)

separator = wibox.widget.textbox()
separator:set_text(" | ")


batwidget = wibox.widget.progressbar()
-- Create wibox with batwidget
batbox = wibox.container.margin(
    wibox.widget{ { max_value = 1, widget = batwidget,
                    border_width = 0.5, border_color = "#000000",
                    color = { type = "linear",
                              from = { 0, 0 },
                              to = { 0, 30 },
                              stops = { { 0, "#AECF96" },
                                        { 1, "#FF5656" } } } },
                  forced_height = 10, forced_width = 8,
                  direction = 'east', color = beautiful.fg_widget,
                  layout = wibox.container.rotate },
    1, 1, 3, 3)
-- Register battery widget
vicious.register(batwidget, vicious.widgets.bat, "$2", 61, "BAT0")


-- TODO: maybe try DBus solution later
-- see https://awesome.naquadah.org/wiki/Volume_control_and_display for details
volume_widget = wibox.widget.textbox()
volume_widget:set_align("right")

function update_volume(widget)
   local fd = io.popen("amixer sget Master")
   local status = fd:read("*all")
   fd:close()

   -- local volume = tonumber(string.match(status, "(%d?%d?%d)%%")) / 100
   local volume = string.match(status, "(%d?%d?%d)%%")
   volume = string.format("% 3d", volume)

   status = string.match(status, "%[(o[^%]]*)%]")

   if string.find(status, "on", 1, true) then
       -- For the volume numbers
       volume = volume .. "%"
   else
       -- For the mute button
       volume = volume .. "M"

   end
   widget:set_markup(volume)
end

update_volume(volume_widget)

-- TODO: find widget for wlan somewhere

mytimer = timer({ timeout = 0.2 })
mytimer:connect_signal("timeout", function () update_volume(volume_widget) end)
mytimer:start()
