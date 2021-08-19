local utils = {}

local awful = require("awful")
local screen = require("screen")

local defs = require("defs")


local qwerty_mapping = {
   ['й'] = 'q',
   ['ц'] = 'w',
   ['у'] = 'e',
   ['к'] = 'r',
   ['е'] = 't',
   ['н'] = 'y',
   ['г'] = 'u',
   ['ш'] = 'i',
   ['щ'] = 'o',
   ['з'] = 'p',
   ['х'] = '[',
   ['ъ'] = ']',
   ['э'] = '\'',
   ['ф'] = 'a',
   ['ы'] = 's',
   ['в'] = 'd',
   ['а'] = 'f',
   ['п'] = 'g',
   ['р'] = 'h',
   ['о'] = 'j',
   ['л'] = 'k',
   ['д'] = 'l',
   ['ж'] = ';',
   ['я'] = 'z',
   ['ч'] = 'x',
   ['с'] = 'c',
   ['м'] = 'v',
   ['и'] = 'b',
   ['т'] = 'n',
   ['ь'] = 'm',
   ['б'] = ',',
   ['ю'] = '.',
   ['.'] = '/',
}

function translate_key(key)
   return qwerty_mapping[key] or key
end

function simple_run_or_raise(klass, command)
   local matcher = function (c)
      return awful.rules.match(c, {class = klass})
   end
   awful.client.run_or_raise(command, matcher)
end


function utils:run_or_raise_map(apps)
   local grabber = keygrabber.run(function(mod, key, event)
         if event == "release" then return end
         keygrabber.stop(grabber)
         key = translate_key(key)
         if apps[key] then
             simple_run_or_raise(apps[key][1], apps[key][2])
         end
   end)
end

function utils:webjumps_map(webjumps, browser)
   local grabber = keygrabber.run(function(mod, key, event)
           if event == "release" then return end
           keygrabber.stop(grabber)
           key = translate_key(key)
           local term
           local browser_class = browser.class
           local browser_command = browser.command
           local browser_params = browser.params
           if key == 'o' then
               term = selection()
           elseif webjumps[key] then
               def = webjumps[key]
               if type(def) == "table" then
                   term = def[1]
                   browser_class = def[2][1]
                   browser_command = def[2][2]
               else
                   term = webjumps[key]
               end
           end
           if term then
               awful.util.spawn(browser_command .. " " .. browser_params .. " " .. term)
               simple_run_or_raise(browser_class, browser_command)
           end
   end)
end

local function _search(search_url, browser, search_data)
   awful.util.spawn(browser.command .. " " .. browser.params .. " " .. string.format(search_url, string.gsub(search_data, " ", "+")))
   simple_run_or_raise(browser.class, browser.command)
end

local function _search_prompt(search_url, browser, search_data)
   awful.prompt.run {
      prompt       = "Search for: ", -- TODO: make titled searches
      textbox      = awful.screen.focused().mypromptbox.widget,
      exe_callback = function(data)
          if data then
              _search(search_url, browser, data)
          end
      end,
      history_path = awful.util.get_cache_dir() .. "/history_searches"
   }
end

function utils:websearches_map(searches, browser, type)
    local grabber = keygrabber.run(function(mod, key, event)
            if event == "release" then return end
            keygrabber.stop(grabber)
            key = translate_key(key)
            if searches[key] then
                if type == "selection" then
                    search_data = selection()
                    if search_data then
                        _search(searches[key], browser, search_data)
                    end
                else
                    client.focus.fullscreen = false
                    _search_prompt(searches[key], browser, search_data)
                end
            end
    end)
end

function utils:xrandr_map(choices)
   local grabber = keygrabber.run(function(mod, key, event)
           if event == "release" then return end
           keygrabber.stop(grabber)
           key = translate_key(key)
           choice = choices[key]
           if choice then
              if type(choice) == "function" then
                    choice()
              else
                 awful.spawn.with_shell(choice)
              end
           end
   end)
end

-- TODO: abstract away common code from keygrabber maps
function utils:vpn_map(commands)
   local grabber = keygrabber.run(function(mod, key, event)
           if event == "release" then return end
           keygrabber.stop(grabber)
           key = translate_key(key)
           if commands[key] then
              awful.spawn.with_shell(commands[key])
           end
   end)
end

do
    fake_input = root.fake_input
    function utils:toggle_keyboard_layout()
        -- for future Emacs windows handling, see example below
        -- root.fake_input fails for some reason
        -- local c = client.focus
        -- if c.class == "Emacs" or c.class == "emacs" then
        --     fake_input('key_press', 37);   fake_input('key_press', 51)
        --     fake_input("key_release", 37); fake_input('key_release', 51)
        --     fake_input('key_press', 37)
        --     return
        -- end
        os.execute(defs.kbdd_dbus_next_cmd)
    end
end

function utils:focus_window_from_list(dir) -- 1/-1
   awful.client.focus.byidx(dir)
   if client.focus then
      client.focus:raise()
   end
end

-- TODO: normalize forms value, e.g. use some ,@ analog
function utils:with_emacs_noninteractive(forms)
   awful.spawn.with_shell("emacsclient --eval '" .. forms .. "'")
end

function utils.update_emacs_frames()
   utils:with_emacs_noninteractive("(custom/update-frames " .. screen:count() .. ")")
end

function utils:hostname()
   local f = io.popen("hostname")
   local res = f:read()
   f:close()
   return res
end

function utils:get_filelist(path)
    local i, t, popen = 0, {}, io.popen
    local pfile = popen('find ' .. path .. ' -type f | sort | uniq')
    for filename in pfile:lines() do
        t[#t+1] = filename
    end
    pfile:close()
    return t
end

return utils
