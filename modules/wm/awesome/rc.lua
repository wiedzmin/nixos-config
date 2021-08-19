local awful = require("awful")
local config_path = awful.util.getdir("config") -- config basedir
package.path = config_path .. "/?.lua;" .. package.path
package.path = config_path .. "/?/init.lua;" .. package.path
package.path = config_path .. "/lib/?.lua;" .. package.path
package.path = config_path .. "/lib/?/?.lua;" .. package.path
package.path = config_path .. "/lib/?/init.lua;" .. package.path


-- Standard awesome library
local wibox = require("wibox") -- Widget and layout library
local beautiful = require("beautiful") -- Theme handling library
local gears = require("gears")
local naughty = require("naughty") -- Notification library

local hints = require("hints")
require("awful.autofocus")

-- include custom modules
local controls = require("controls")
local windows = require("windows")

-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end
-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end

hints.init()

globalkeys = controls.globalkeys
clientkeys = controls.clientkeys
root.keys(globalkeys)
root.buttons(controls.mouse_bindings)
