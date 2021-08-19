
--[[
                                                     
        Licensed under GNU General Public License v2 
        * (c) 2016, Alexandre Terrien                
                                                     
--]]

local helpers = require("lain.helpers")
local json    = require("lain.util.dkjson")
local pread   = require("awful.util").pread
local naughty = require("naughty")
local wibox   = require("wibox")
local mouse   = mouse
local os      = { getenv = os.getenv }

local setmetatable = setmetatable

-- Google Play Music Desktop infos
-- lain.widget.contrib.gpmdp
local gpmdp = {}

local function worker(args)
    local args          = args or {}
    local timeout       = args.timeout or 2
    local notify        = args.notify or "off"
    local followmouse   = args.followmouse or false
    local file_location = args.file_location or
                          os.getenv("HOME") .. "/.config/Google Play Music Desktop Player/json_store/playback.json"
    local settings      = args.settings or function() end

    gpmdp.widget = wibox.widget.textbox('')

    gpmdp_notification_preset = {
        title   = "Now playing",
        timeout = 6
    }

    helpers.set_map("gpmdp_current", nil)

    function gpmdp.update()
        file, err = io.open(file_location, "r")
        if not file
        then
            gpm_now = { running = false, playing = false }
        else
            dict, pos, err = json.decode(file:read "*a", 1, nil)
            file:close()
            gpm_now = {}
            gpm_now.artist    = dict.song.artist
            gpm_now.album     = dict.song.album
            gpm_now.title     = dict.song.title
            gpm_now.cover_url = dict.song.albumArt
            gpm_now.playing   = dict.playing
        end

        if (pread("pidof 'Google Play Music Desktop Player'") ~= '') then
            gpm_now.running = true
        else
            gpm_now.running = false
        end

        gpmdp_notification_preset.text = string.format("%s (%s) - %s", gpm_now.artist, gpm_now.album, gpm_now.title)
        widget = gpmdp.widget
        settings()

        if gpm_now.playing
        then
            if notify == "on" and gpm_now.title ~= helpers.get_map("gpmdp_current")
            then
                helpers.set_map("gpmdp_current", gpm_now.title)
                os.execute("curl " .. gpm_now.cover_url .. " -o /tmp/gpmcover.png")

                if followmouse then
                    gpmdp_notification_preset.screen = mouse.screen
                end

                gpmdp.id = naughty.notify({
                    preset = gpmdp_notification_preset,
                    icon = "/tmp/gpmcover.png",
                    replaces_id = gpmdp.id,
                }).id
            end
        elseif not gpm_now.running
        then
            helpers.set_map("gpmdp_current", nil)
        end
    end

    helpers.newtimer("gpmdp", timeout, gpmdp.update)

    return setmetatable(gpmdp, { __index = gpmdp.widget })
end

return setmetatable(gpmdp, { __call = function(_, ...) return worker(...) end })
