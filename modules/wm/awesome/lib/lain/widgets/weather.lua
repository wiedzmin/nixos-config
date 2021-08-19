
--[[
                                                  
     Licensed under GNU General Public License v2 
      * (c) 2015, Luke Bonham                     
                                                  
--]]

local newtimer     = require("lain.helpers").newtimer
local read_pipe    = require("lain.helpers").read_pipe

local async        = require("lain.asyncshell")
local json         = require("lain.util").dkjson
local lain_icons   = require("lain.helpers").icons_dir

local naughty      = require("naughty")
local wibox        = require("wibox")

local math         = { floor    = math.floor }
local os           = { time     = os.time,
                       date     = os.date,
                       difftime = os.difftime }
local string       = { format   = string.format,
                       gsub     = string.gsub }

local mouse        = mouse
local tonumber     = tonumber
local setmetatable = setmetatable

-- OpenWeatherMap
-- current weather and X-days forecast
-- lain.widgets.weather

local function worker(args)
    local weather               = {}
    local args                  = args or {}
    local APPID                 = args.APPID or "3e321f9414eaedbfab34983bda77a66e" -- lain default
    local timeout               = args.timeout or 900   -- 15 min
    local timeout_forecast      = args.timeout or 86400 -- 24 hrs
    local current_call          = args.current_call  or "curl -s 'http://api.openweathermap.org/data/2.5/weather?id=%s&units=%s&lang=%s&APPID=%s'"
    local forecast_call         = args.forecast_call or "curl -s 'http://api.openweathermap.org/data/2.5/forecast/daily?id=%s&units=%s&lang=%s&cnt=%s&APPID=%s'"
    local city_id               = args.city_id or 0 -- placeholder
    local utc_offset            = args.utc_offset or
                                  function ()
                                      local now = os.time()
                                      return os.difftime(now, os.time(os.date("!*t", now))) + ((os.date("*t").isdst and 1 or 0) * 3600)
                                  end
    local units                 = args.units or "metric"
    local lang                  = args.lang or "en"
    local cnt                   = args.cnt or 5
    local date_cmd              = args.date_cmd or "date -u -d @%d +'%%a %%d'"
    local icons_path            = args.icons_path or lain_icons .. "openweathermap/"
    local notification_preset   = args.notification_preset or {}
    local notification_text_fun = args.notification_text_fun or
                                  function (wn)
                                      local day = string.gsub(read_pipe(string.format(date_cmd, wn["dt"])), "\n", "")
                                      local tmin = math.floor(wn["temp"]["min"])
                                      local tmax = math.floor(wn["temp"]["max"])
                                      local desc = wn["weather"][1]["description"]
                                      return string.format("<b>%s</b>: %s, %d - %d ", day, desc, tmin, tmax)
                                  end
    local weather_na_markup     = args.weather_na_markup or " N/A "
    local followmouse           = args.followmouse or false
    local settings              = args.settings or function() end

    weather.widget    = wibox.widget.textbox(weather_na_markup)
    weather.icon_path = icons_path .. "na.png"
    weather.icon      = wibox.widget.imagebox(weather.icon_path)

    function weather.show(t_out)
        weather.hide()

        if followmouse then
            notification_preset.screen = mouse.screen
        end

        if not weather.notification_text then
            weather.forecast_update()
        end

        weather.notification = naughty.notify({
            text    = weather.notification_text,
            icon    = weather.icon_path,
            timeout = t_out,
            preset  = notification_preset
        })
    end

    function weather.hide()
        if weather.notification then
            naughty.destroy(weather.notification)
            weather.notification = nil
        end
    end

    function weather.attach(obj)
        obj:connect_signal("mouse::enter", function()
            weather.show(0)
        end)
        obj:connect_signal("mouse::leave", function()
            weather.hide()
        end)
    end

    function weather.forecast_update()
        local cmd = string.format(forecast_call, city_id, units, lang, cnt, APPID)
        async.request(cmd, function(f)
            local pos, err
            weather_now, pos, err = json.decode(f, 1, nil)

            if not err and type(weather_now) == "table" and tonumber(weather_now["cod"]) == 200 then
                weather.notification_text = ''
                for i = 1, weather_now["cnt"] do
                    weather.notification_text = weather.notification_text ..
                                                notification_text_fun(weather_now["list"][i])

                    if i < weather_now["cnt"] then
                        weather.notification_text = weather.notification_text .. "\n"
                    end
                end
            end
        end)
    end

    function weather.update()
        local cmd = string.format(current_call, city_id, units, lang, APPID)
        async.request(cmd, function(f)
            local pos, err, icon
            weather_now, pos, err = json.decode(f, 1, nil)

            if not err and type(weather_now) == "table" and tonumber(weather_now["cod"]) == 200 then
                -- weather icon based on localtime
                local now     = os.time()
                local sunrise = tonumber(weather_now["sys"]["sunrise"])
                local sunset  = tonumber(weather_now["sys"]["sunset"])
                local icon    = weather_now["weather"][1]["icon"]
                local utc_m   = string.gsub(read_pipe(string.format("date -u -d 'today 00:00:00' +'%%s'")), "\n", "")
                local loc_m   = string.gsub(read_pipe(string.format("date -d 'today 00:00:00' +'%%s'")), "\n", "")

                loc_m  = tonumber(loc_m)
                utc_m  = tonumber(utc_m)
                offset = utc_offset()

                -- if we are 1 day after the GMT, return 1 day back, and viceversa
                if offset > 0 and loc_m >= utc_m then
                    now = now - 86400
                elseif offset < 0 and loc_m <= utc_m then
                    now = now + 86400
                end

                if sunrise <= now and now <= sunset then
                    icon = string.gsub(icon, "n", "d")
                else
                    icon = string.gsub(icon, "d", "n")
                end

                weather.icon_path = icons_path .. icon .. ".png"
                widget = weather.widget
                settings()
            else
                weather.icon_path = icons_path .. "na.png"
                weather.widget:set_markup(weather_na_markup)
            end

            weather.icon:set_image(weather.icon_path)
        end)
    end

    weather.attach(weather.widget)

    newtimer("weather-" .. city_id, timeout, weather.update)
    newtimer("weather_forecast-" .. city_id, timeout, weather.forecast_update)

    return setmetatable(weather, { __index = weather.widget })
end

return setmetatable({}, { __call = function(_, ...) return worker(...) end })
