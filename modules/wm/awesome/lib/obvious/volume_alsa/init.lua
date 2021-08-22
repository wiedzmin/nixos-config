--------------------------------
-- Author: Gregor Best        --
-- Copyright 2009 Gregor Best --
--------------------------------

local setmetatable = setmetatable
local tonumber = tonumber
local pairs = pairs
local io = {
  popen = io.popen
}
local string = {
  match  = string.match,
  find   = string.find,
  format = string.format
}
local table = {
  insert = table.insert
}
local awful = require("awful")
local wibox = require("wibox")
local lib = {
  hooks = require("obvious.lib.hooks"),
  markup = require("obvious.lib.markup")
}

local volume_alsa = {}

local objects = { }

function volume_alsa.get_data(cardid, channel)
  local rv = { }
  local fd = io.popen("amixer -c " .. cardid .. " -- sget " .. channel)
  if not fd then return end
  local status = fd:read("*all")
  fd:close()

  rv.volume = tonumber(string.match(status, "(%d?%d?%d)%%"))
  if not rv.volume then
    rv.volume = ""
  end

  status = string.match(status, "%[(o[^%]]*)%]")
  if not status then status = "on" end
  if string.find(status, "on", 1, true) then
    rv.mute = false
  else
    rv.mute = true
  end

  return rv
end

local function update(obj)
  local status = volume_alsa.get_data(obj.cardid, obj.channel) or { mute = true, volume = 0 }

  local color = "#900000"
  if not status.mute then
    color = "#009000"
  end

  local format = " %03d%%"
  if status.volume == "" then
    format = ""
  end

  obj.widget:set_markup(lib.markup.fg.color(color, "" .. obj.abrv .. "" ) .. string.format(format, status.volume))
end

local function update_by_values(cardid, channel)
  for i, v in pairs(objects) do
    if v.channel == channel and v.cardid == cardid then
      update(v)
    end
  end
end

function volume_alsa.raise(cardid, channel, v)
  v = v or 1
  awful.util.spawn("amixer -q -c " .. cardid .. " sset " .. channel .. " " .. v .. "+", false)
  update_by_values(cardid, channel)
end

function volume_alsa.lower(cardid, channel, v)
  v = v or 1
  awful.util.spawn("amixer -q -c " .. cardid .. " sset " .. channel .. " " .. v .. "-", false)
  update_by_values(cardid, channel)
end

function volume_alsa.mute(cardid, channel)
  awful.util.spawn("amixer -c " .. cardid .. " sset " .. channel .. " toggle", false)
  update_by_values(cardid, channel)
end

local function mixer(term, cardid)
  awful.util.spawn(term .. " -e 'alsamixer -c " .. cardid .. "'")
end

local function create(_, cardid, channel, abrv)
  local cardid = cardid or 0
  local channel = channel or "Master"
  local abrv = abrv or "M"

  local obj = {
    cardid = cardid,
    channel = channel,
    abrv = abrv,
    term = "x-terminal-emulator -T Mixer"
  }

  local widget = wibox.widget.textbox()
  obj.widget = widget
  obj[1] = widget
  obj.update = function() update(obj) end

  widget:buttons(awful.util.table.join(
    awful.button({ }, 4, function () volume_alsa.raise(obj.cardid, obj.channel, 1) obj.update() end),
    awful.button({ }, 5, function () volume_alsa.lower(obj.cardid, obj.channel, 1) obj.update() end),
    awful.button({ "Shift" }, 4, function () volume_alsa.raise(obj.cardid, obj.channel, 10) obj.update() end),
    awful.button({ "Shift" }, 5, function () volume_alsa.lower(obj.cardid, obj.channel, 10) obj.update() end),
    awful.button({ "Control" }, 4, function () volume_alsa.raise(obj.cardid, obj.channel, 5) obj.update() end),
    awful.button({ "Control" }, 5, function () volume_alsa.lower(obj.cardid, obj.channel, 5) obj.update() end),
    awful.button({ }, 1, function () volume_alsa.mute(obj.cardid, obj.channel)     obj.update() end),
    awful.button({ }, 3, function () mixer(obj.term, obj.cardid)       obj.update() end)
  ))

  obj.set_layout  = function(obj, layout) obj.layout = layout               return obj end
  obj.set_cardid  = function(obj, id)     obj.cardid = id  obj.update()     return obj end
  obj.set_channel = function(obj, id)     obj.channel = id obj.update()     return obj end
  obj.set_abrv    = function(obj, id)     obj.abrv = id    obj.update()     return obj end
  obj.set_term    = function(obj, term)   obj.term = term                   return obj end
  obj.raise       = function(obj, v)      volume_alsa.raise(obj.cardid, obj.channel, v) return obj end
  obj.lower       = function(obj, v) volume_alsa.lower(obj.cardid, obj.channel, v)      return obj end
  obj.mute        = function(obj, v) volume_alsa.mute(obj.cardid, obj.channel, v)       return obj end

  obj.update()
  lib.hooks.timer.register(10, 30, obj.update, "Update for the volume widget")
  lib.hooks.timer.start(obj.update)

  table.insert(objects, obj)
  return widget
end

return setmetatable(volume_alsa, { __call = create })

-- vim:ft=lua:ts=2:sw=2:sts=2:tw=80:et
