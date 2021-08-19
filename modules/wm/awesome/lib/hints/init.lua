local awful     = require("awful")
local client    = client
local keygrabber= keygrabber
local naughty   = require("naughty")
local pairs     = pairs
local beautiful = require("beautiful")
local wibox     = require("wibox")


local hints = {}

charorder = "asdfghjkl"
hintbox = {} -- Table of letter wiboxes with characters as the keys

fontcolor_fallback = "red"

function debuginfo( message )
  nid = naughty.notify({ text = message, timeout = 10 })
end

-- Create the wiboxes, but don't show them
function hints.init()
  hintsize = 60
  local fontcolor = beautiful.fg_normal or fontcolor_fallback
  local letterbox = {}
  for i = 1, #charorder do
    local char = charorder:sub(i,i)
    hintbox[char] = wibox({fg=beautiful.fg_normal, bg=beautiful.bg_focus, border_color=beautiful.border_focus, border_width=beautiful.border_width})
    hintbox[char].ontop = true
    hintbox[char].width = hintsize
    hintbox[char].height = hintsize
    letterbox[char] = wibox.widget.textbox()
    letterbox[char]:set_markup("<span color=\"" .. fontcolor .. "\"" .. ">" .. char.upper(char) .. "</span>")
    letterbox[char]:set_font("dejavu sans mono 40")
    letterbox[char]:set_align("center")
    hintbox[char]:set_widget(letterbox[char])
  end
end

function hints.focus()
  local hintindex = {} -- Table of visible clients with the hint letter as the keys
  local clientlist = awful.client.visible()
  for i,thisclient in pairs(clientlist) do -- Move wiboxes to center of visible windows and populate hintindex
    local char = charorder:sub(i,i)
    if char and char ~= '' then
        hintindex[char] = thisclient
        local geom = thisclient:geometry()
            hintbox[char].visible = true
            hintbox[char].x = math.floor(geom['x'] + geom['width']/2 - hintsize/2)
            hintbox[char].y = math.floor(geom.y + geom.height/2 - hintsize/2)
            hintbox[char].screen = thisclient.screen
    end
  end
  keygrabber.run( function(mod,key,event)
    if event == "release" then return true end
    keygrabber.stop()
    if hintindex[key] then
      client.focus = hintindex[key]
      awful.screen.focus(hintindex[key].screen)
      hintindex[key]:raise()
    end
    for i,j in pairs(hintindex) do
      hintbox[i].visible = false
    end
  end)

end

--function debuginfo( message )
    --if type(message) == "table" then
        --for k,v in pairs(message) do
            --naughty.notify({ text = "key: "..k.." value: "..tostring(message), timeout = 10 })
        --end
    --else
        --nid = naughty.notify({ text = message, timeout = 10 })
    --end
--end

local function debuginfo( message )

    mm = message

    if not message then
        mm = "false"
    end

    nid = naughty.notify({ text = tostring(mm), timeout = 10 })
end

setmetatable(hints, { __call = function(_, ...) return hints.focus(...) end })

return hints
