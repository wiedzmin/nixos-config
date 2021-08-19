-- ezconfig.lua --- easy key and button setup for awesome
--
-- Author: Georgi Valkov <georgi.t.valkov@gmail.com>
-- License: GPLv2
-- Version: 1.0
-- URL: https://github.com/gvalkov/awesome-ezconfig
--
-- Usage:
--   ezconfig = require('ezconfig')
--
--   ezconfig.modkey = 'Mod4'
--   ezconfig.altkey = 'Mod1'
--
--   globalkeys = ezconfig.keytable.join({
--      ['M-<Left>'] = awful.tag.viewprev,
--      ['M-S-j'] = {awful.client.swap.byidx, 1},
--      ['M-C-k'] = {awful.screen.focus_relative, -1},
--      ['M-m'] = function () menus.main:show() end,
--   })
--
--   -- The above is functionally equivalent to:
--   key = ezconfig.key
--   globalkeys = awful.util.table.join(
--      key('M-<Left>', awful.tag.viewprev),
--      key('M-S-j', {awful.client.swap.byidx, 1}),
--   )
--
--   -- Which is in turn equivalent to:
--   globalkeys = awful.util.table.join(
--      awful.key({modkey}, 'Left', awful.tag.viewprev),
--      awful.key({modkey, 'Shift'}, 'j', function () awful.client.swap.byidx(1) end),
--   )
--
--   -- It is also possible to configure mouse buttons:
--   clientbuttons = ezconfig.btntable.join({
--      ['1'] = function (c) client.focus = c; c:raise() end,
--      ['M-1'] = awful.mouse.client.move,
--      ['M-3'] = awful.mouse.client.resize,
--   })

local awful = require('awful')

local ezconfig = {}
ezconfig.btntable = {}
ezconfig.keytable = {}
ezconfig.modkey = 'Mod4'
ezconfig.altkey = 'Mod1'


local modifier_keys = {
   ['M'] = ezconfig.modkey,
   ['S'] = 'Shift',
   ['C'] = 'Control',
   ['A'] = ezconfig.altkey,
}

local function split(s, sep)
   if sep == nil then
      sep = "%s"
   end

   local res = {}
   local i = 1
   for m in string.gmatch(s, '([^' .. sep ..  ']+)') do
      res[i] = m
      i = i + 1
   end
   return res
end

-- Returns an anonymous function if `callback` is a table:
--   function () callback[0](unpack(callback[1:])) end
local function cb_from_table(callback)
   if type(callback) == 'table' then
      local func = table.remove(callback, 1)
      local args = callback
      callback = function ()
         return func(unpack(args))
      end
   end

   return callback
end

function ezconfig.key(keydef, callback, newkeyfunc)
   if newkeyfunc == nil then
      newkeyfunc = awful.key
   end

   local modkeys = {}
   local keys = {}

   for _, key in ipairs(split(keydef, '-')) do
      if modifier_keys[key] ~= nil then
         table.insert(modkeys, modifier_keys[key])
      else
         if #key == 1 or string.sub(key, 1, 1) == "#" then
            table.insert(keys, key)
         else
            _, _, key = string.find(key, '<(%w+)>')
            table.insert(keys, key)
         end
      end
   end

   callback =  cb_from_table(callback)
   return newkeyfunc(modkeys, table.concat(keys, ''), callback)
end

function ezconfig.btn(btndef, callback, newbtnfunc)
   if newbtnfunc == nil then
      newbtnfunc = awful.button
   end

   callback =  cb_from_table(callback)
   if type(btndef) == 'number' then
      return newbtnfunc({}, btndef, callback)
   end

   local modkeys = {}
   local button = nil

   for _, key in ipairs(split(btndef, '-')) do
      if modifier_keys[key] ~= nil then
         table.insert(modkeys, modifier_keys[key])
      else
         button = tonumber(key)
         return newbtnfunc(modkeys, button, callback)
      end
   end
end

function ezconfig.keytable.join(tbl)
   local res = {}
   for keydef, callback in pairs(tbl) do
      table.insert(res, ezconfig.key(keydef, callback))
   end

   return awful.util.table.join(unpack(res))
end

function ezconfig.btntable.join(tbl)
   local res = {}
   for btndef, callback in pairs(tbl) do
      table.insert(res, ezconfig.btn(btndef, callback))
   end

   return awful.util.table.join(unpack(res))
end


return ezconfig
