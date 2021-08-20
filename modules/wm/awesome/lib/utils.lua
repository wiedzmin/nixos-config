local utils = {}

local awful = require("awful")
-- local screen = require("screen")

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
        os.execute("dbus-send --dest=ru.gentoo.KbddService /ru/gentoo/KbddService ru.gentoo.kbdd.next_layout")
    end
end

return utils
