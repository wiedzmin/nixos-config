-----------------------------------
-- Author: Uli Schlachter        --
-- Copyright 2009 Uli Schlachter --
-----------------------------------

local beautiful = require("beautiful")
local awful = {
  widget = require("awful.widget")
}
local setmetatable = setmetatable

local function graph(layout, scale)
  local theme = beautiful.get()
  local color = theme.graph_fg_color or theme.widget_fg_color or theme.fg_normal
  local back  = theme.graph_bg_color or theme.widget_bg_color or theme.bg_normal
  local border= theme.graph_border or theme.widget_border or theme.border_normal
  local width = theme.graph_width or theme.widget_width

  local widget = awful.widget.graph({ layout = layout })
  widget:set_color(color)
  widget:set_border_color(border)
  widget:set_background_color(back)

  if width then
    widget:set_width(width)
  end

  if scale then
    widget:set_scale(true)
  end

  return widget
end

local function create(data, layout)
  local scale = true
  if data.max then
    scale = false
  end

  local widget = graph(layout, scale)

  widget.update = function(widget)
    local max = widget.data.max or 1
    local val = widget.data:get() or max
    widget:add_value(val / max)
  end

  widget.data = data

  return widget
end

return setmetatable({
  create = create,
}, { __call = function (_, ...) return graph(...) end })

-- vim:ft=lua:ts=2:sw=2:sts=2:tw=80:et
