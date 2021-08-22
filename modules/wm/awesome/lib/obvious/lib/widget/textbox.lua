-----------------------------------
-- Author: Uli Schlachter        --
-- Copyright 2009 Uli Schlachter --
-----------------------------------

local type = type
local pcall = pcall
local wibox = require("wibox")
local string = {
  format = string.format
}

local function create(data, layout)
  local obj = { }

  obj.data = data
  obj.widget = wibox.widget.textbox()
  obj.format = "%3d%%"
  obj.layout = layout

  obj.update = function(obj)
    local max = obj.data.max or 1
    local val = obj.data:get() or max
    local perc = val / max * 100
    if type(obj.format) == "function" then
      if not pcall(function () obj.widget:set_markup(obj.format(perc)) end) then
        obj.widget:set_text(obj.format(perc))
      end
    else
      if not pcall(function () obj.widget:set_markup(string.format(obj.format, perc)) end) then
        obj.widget:set_text(string.format(obj.format, perc))
      end
    end
  end

  obj.set_format = function(obj, format)
    obj.format = format
    obj:update()
    return obj
  end

  return obj
end

return {
  create = create,
}

-- vim:ft=lua:ts=2:sw=2:sts=2:tw=80:et
