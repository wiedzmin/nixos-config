
--[[
                                                  
     Licensed under GNU General Public License v2 
      * (c) 2013, Luke Bonham                     
      * (c) 2013, Rman                            
                                                  
--]]

local newtimer     = require("lain.helpers").newtimer
local read_pipe    = require("lain.helpers").read_pipe

local awful        = require("awful")
local beautiful    = require("beautiful")
local naughty      = require("naughty")

local math         = { modf   = math.modf }
local mouse        = mouse
local string       = { format = string.format,
                       match  = string.match,
                       rep    = string.rep }
local tonumber     = tonumber

local setmetatable = setmetatable

-- ALSA volume bar
-- lain.widgets.alsabar
local alsabar = {
    channel = "Master",
    step    = "1%",

    colors = {
        background = beautiful.bg_normal,
        mute       = "#EB8F8F",
        unmute     = "#A4CE8A"
    },

    terminal = terminal or "xterm",
    mixer    = terminal .. " -e alsamixer",

    notifications = {
        font      = beautiful.font:sub(beautiful.font:find(""), beautiful.font:find(" ")),
        font_size = "11",
        color     = beautiful.fg_normal,
        bar_size  = 18,
        screen    = 1
    },

    _current_level = 0,
    _muted         = false
}

function alsabar.notify()
    alsabar.update()

    local preset = {
        title   = "",
        text    = "",
        timeout = 5,
        screen  = alsabar.notifications.screen,
        font    = alsabar.notifications.font .. " " ..
                  alsabar.notifications.font_size,
        fg      = alsabar.notifications.color
    }

    if alsabar._muted
    then
        preset.title = alsabar.channel .. " - Muted"
    else
        preset.title = alsabar.channel .. " - " .. alsabar._current_level .. "%"
    end

    int = math.modf((alsabar._current_level / 100) * alsabar.notifications.bar_size)
    preset.text = "["
                .. string.rep("|", int)
                .. string.rep(" ", alsabar.notifications.bar_size - int)
                .. "]"

    if alsabar.followmouse then
        preset.screen = mouse.screen
    end

    if alsabar._notify ~= nil then
        alsabar._notify = naughty.notify ({
            replaces_id = alsabar._notify.id,
            preset      = preset,
        })
    else
        alsabar._notify = naughty.notify ({
            preset = preset,
        })
    end
end

local function worker(args)
    local args       = args or {}
    local timeout    = args.timeout or 5
    local settings   = args.settings or function() end
    local width      = args.width or 63
    local height     = args.heigth or 1
    local ticks      = args.ticks or false
    local ticks_size = args.ticks_size or 7
    local vertical   = args.vertical or false

    alsabar.cmd           = args.cmd or "amixer"
    alsabar.channel       = args.channel or alsabar.channel
    alsabar.togglechannel = args.togglechannel
    alsabar.step          = args.step or alsabar.step
    alsabar.colors        = args.colors or alsabar.colors
    alsabar.notifications = args.notifications or alsabar.notifications
    alsabar.followmouse   = args.followmouse or false

    alsabar.bar = awful.widget.progressbar()

    alsabar.bar:set_background_color(alsabar.colors.background)
    alsabar.bar:set_color(alsabar.colors.unmute)
    alsabar.tooltip = awful.tooltip({ objects = { alsabar.bar } })
    alsabar.bar:set_width(width)
    alsabar.bar:set_height(height)
    alsabar.bar:set_ticks(ticks)
    alsabar.bar:set_ticks_size(ticks_size)
    alsabar.bar:set_vertical(vertical)

    function alsabar.update()
        -- Get mixer control contents
        local mixer = read_pipe(string.format("%s get %s", alsabar.cmd, alsabar.channel))

        -- Capture mixer control state:          [5%] ... ... [on]
        local volu, mute = string.match(mixer, "([%d]+)%%.*%[([%l]*)")

        -- HDMIs can have a channel different from Master for toggling mute
        if alsabar.togglechannel then
            mute = string.match(read_pipe(string.format("%s get %s", alsabar.cmd, alsabar.togglechannel)), "%[(%a+)%]")
        end

        if (volu and tonumber(volu) ~= alsabar._current_level) or (mute and string.match(mute, "on") ~= alsabar._muted)
        then
            alsabar._current_level = tonumber(volu) or alsabar._current_level
            alsabar.bar:set_value(alsabar._current_level / 100)
            if not mute and tonumber(volu) == 0 or mute == "off"
            then
                alsabar._muted = true
                alsabar.tooltip:set_text (" [Muted] ")
                alsabar.bar:set_color(alsabar.colors.mute)
            else
                alsabar._muted = false
                alsabar.tooltip:set_text(string.format(" %s:%s ", alsabar.channel, volu))
                alsabar.bar:set_color(alsabar.colors.unmute)
            end

            volume_now = {}
            volume_now.level = tonumber(volu)
            volume_now.status = mute
            settings()
        end
    end

    alsabar.bar:buttons (awful.util.table.join (
          awful.button ({}, 1, function()
            awful.util.spawn(alsabar.mixer)
          end),
          awful.button ({}, 3, function()
            awful.util.spawn(string.format("%s set %s toggle", alsabar.cmd, alsabar.channel))
            alsabar.update()
          end),
          awful.button ({}, 4, function()
            awful.util.spawn(string.format("%s set %s %s+", alsabar.cmd, alsabar.channel, alsabar.step))
            alsabar.update()
          end),
          awful.button ({}, 5, function()
            awful.util.spawn(string.format("%s set %s %s-", alsabar.cmd, alsabar.channel, alsabar.step))
            alsabar.update()
          end)
    ))

    timer_id = string.format("alsabar-%s-%s", alsabar.cmd, alsabar.channel)

    newtimer(timer_id, timeout, alsabar.update)

    return alsabar
end

return setmetatable(alsabar, { __call = function(_, ...) return worker(...) end })
