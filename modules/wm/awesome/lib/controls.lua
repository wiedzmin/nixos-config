local controls = {}

local awful = require("awful")
local hints = require("hints")
local menubar = require("menubar")
local ezconfig = require("ezconfig")

local defs = require('defs')
local menus = require('menus')
local bookshelf = require('bookshelf')
local utils = require('utils')

-- TODO: add utility function for translating layouts as below, in background
controls.apps = {
    ["f"]={'Firefox', 'firefox-bin'},
    ["c"]={'Google-chrome', 'google-chrome-stable'},
    ["e"]={'Emacs', 'emacs'},
    ["t"]={'URxvt', 'urxvt'},
    ["t"]={'Sakura', 'sakura'},
    ["z"]={'Zathura', 'zathura'},
    ["l"]={'Vlc', 'vlc'},
    ["g"]={'Telegram', 'Telegram'},
    ["2"]={'Fbreader', 'fbreader'}
}

-- TODO: add utility function for translating layouts as below, in background
controls.webjumps = {
    ["m"]='https://mail.google.com/mail/u/0/#inbox',
    ["g"]='https://github.com/wiedzmin',
    ["y"]='http://yandex.ru',
    ["f"]={'https://facebook.com/', {'Google-chrome', 'google-chrome-stable'}}, -- TODO: abstract away low-level details
    ["t"]='http://www.multitran.ru/',
    ["b"]={'https://www.youtube.com/', {'Google-chrome', 'google-chrome-stable'}},
    ["d"]='https://feedly.com/',
    ["p"]='https://getpocket.com/a/queue/',
    ["i"]='https://todoist.com/app',
    ["r"]='http://relax-fm.ru',
}

-- TODO: add utility function for translating layouts as below, in background
controls.websearches = {
    ["g"] = 'http://www.google.com/search?num=100&q=%s',
    ["y"] = 'http://yandex.ru/yandsearch?text=%s',
    ["t"] = 'http://www.multitran.ru/c/M.exe?CL=1&l1=1&s=%s',
    ["z"] = 'http://gpo.zugaina.org/Search?search=%s',
    ["c"] = 'https://searchcode.com/?q=%s',
    ["l"] = 'https://luarocks.org/search?q=%s',
}

controls.xrandr_choices = {
    ['Right'] = "xrandr --output VGA1 --auto --right-of LVDS1",
    ['Left'] = "xrandr --output VGA1 --auto --left-of LVDS1",
    ['Up'] = "xrandr --output VGA1 --auto --above LVDS1",
    ['Down'] = "xrandr --output VGA1 --off",
    ['u'] = function() utils.update_emacs_frames() end
}

-- TODO: make command for vpn service restarting
controls.vpn_service_commands = {
    ['Up'] = "sudo /etc/init.d/job-vpn start",
    ['Down'] = "sudo /etc/init.d/job-vpn stop",
    ['s'] = "sudo /etc/init.d/job-vpn status"
}

ezconfig.modkey = defs.modkey
ezconfig.altkey = defs.altkey

controls.globalkeys = ezconfig.keytable.join({
    ['M-<Escape>'] = awful.tag.history.restore,
    ['M-<Left>'] = function () awful.client.focus.global_bydirection('left') end,
    ['M-<Right>'] = function () awful.client.focus.global_bydirection('right') end,
    ['M-<Up>'] = function () awful.client.focus.global_bydirection('up') end,
    ['M-<Down>'] = function () awful.client.focus.global_bydirection('down') end,
    ['M-C-<Left>'] = function () awful.client.swap.global_bydirection('left') end,
    ['M-C-<Right>'] = function () awful.client.swap.global_bydirection('right') end,
    ['M-C-<Up>'] = function () awful.client.swap.global_bydirection('up') end,
    ['M-C-<Down>'] = function () awful.client.swap.global_bydirection('down') end,
    ['M-,'] = function () awful.screen.focus_bydirection("left") end,
    ['M-.'] = function () awful.screen.focus_bydirection("right") end,
    ['M-u'] = awful.client.urgent.jumpto,
    ['M-h'] = function () awful.tag.incmwfact(-0.05) end,
    ['M-l'] = function () awful.tag.incmwfact(0.05) end,
    ['M-S-h'] = function () awful.tag.incnmaster(1) end,
    ['M-S-l'] = function () awful.tag.incnmaster(-1) end,
    ['M-C-h'] = function () awful.tag.incncol(1) end,
    ['M-C-l'] = function () awful.tag.incncol(-1) end,
    -- ['M-<Space>'] = function () awful.layout.inc(layouts, 1) end, -- TODO: fix signature
    -- ['M-S-<Space>'] = function () awful.layout.inc(layouts, -1) end, -- TODO: fix signature
    ['M-x'] = function()
       awful.prompt.run {
          prompt       = "Run Lua code: ",
          textbox      = awful.screen.focused().mypromptbox.widget,
          exe_callback = awful.util.eval,
          history_path = awful.util.get_cache_dir() .. "/history_eval"
       }
    end,
    ['M-p'] = function() menubar.show() end,
    ['M-<Escape>'] = function() menus.show_apps_menu() end,
    ['<XF86AudioPrev>'] = function () awful.spawn("mpc prev") end,
    ['<XF86AudioPlay>'] = function () awful.spawn("mpc toggle") end,
    ['<XF86AudioNext>'] = function () awful.spawn("mpc next") end,
    ['<XF86AudioRaiseVolume>'] = function () awful.spawn("amixer -c 0 set Master 10+") end,
    ['<XF86AudioLowerVolume>'] = function () awful.spawn("amixer -c 0 set Master 10-") end,
    ['<XF86AudioMute>'] = function () awful.spawn("amixer set Master toggle >> /dev/null") end,
    ['<XF86MonBrightnessUp>'] = function () awful.spawn("xbacklight -inc 10") end,
    ['<XF86MonBrightnessDown>'] = function () awful.spawn("xbacklight -dec 10") end,
    ['M-C-l'] = function () awful.spawn.with_shell("i3lock -c 232729 && sleep 1 && xset dpms force off") end,
    ['M-`'] = function () utils:xrandr_map(controls.xrandr_choices) end,
    ['M-v'] = function () utils:vpn_map(controls.vpn_service_commands) end,
    ['M-z'] = function () utils:run_or_raise_map(controls.apps) end,
    ['M-w'] = function () utils:webjumps_map(controls.webjumps, defs.browser) end,
    ['M-/'] = function () utils:websearches_map(controls.websearches, defs.browser, "selection") end,
    ['M-C-/'] = function () utils:websearches_map(controls.websearches, defs.browser, "prompt") end,
    ['C-\\'] = utils.toggle_keyboard_layout,
    ['M-b'] = function() bookshelf:show_books_menu() end,
    ['M-e'] = function () hints.focus() end,
    ['M-S-p'] = function () awful.spawn("gmrun") end,
    -- ['M-S-/'] = function() cheeky.util.switcher() end,
    ['<Print>'] = function () awful.spawn("scrot -e 'mv $f ~/screenshots/ 2>/dev/null'") end,
    ['M-C-r'] = awesome.restart,
    ['M-S-q'] = function() awesome.quit() end,
})

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    controls.globalkeys = awful.util.table.join(controls.globalkeys,
        -- View tag only.
        awful.key({ defs.modkey }, "#" .. i + 9,
            function ()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    tag:view_only()
                end
        end),
        -- View tag on left screen.
        awful.key({ defs.modkey, "Shift" }, "#" .. i + 9,
            function ()
                awful.screen.focus_bydirection("left")
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    tag:view_only()
                end
        end),
        -- View tag on right screen.
        awful.key({ defs.modkey, "Mod1" }, "#" .. i + 9,
            function ()
                awful.screen.focus_bydirection("right")
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    tag:view_only()
                end
        end),
        -- Toggle tag display.
        awful.key({ defs.modkey, "Control" }, "F" .. i,
            function ()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    awful.tag.viewtoggle(tag)
                end
        end),
        -- Move client to tag.
        awful.key({ defs.modkey, "Shift" }, "F" .. i,
            function ()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:move_to_tag(tag)
                    end
                end
        end),
        -- Toggle tag.
        awful.key({ defs.modkey, "Control", "Shift" }, "F" .. i + 9,
            function ()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:toggle_tag(tag)
                    end
                end
    end))
end

-- Create a wibox for each screen and add it
controls.taglist_buttons = awful.util.table.join(
    awful.button({ }, 1, function(t) t:view_only() end),
    awful.button({ defs.modkey }, 1,
        function(t)
            if client.focus then
                client.focus:move_to_tag(t)
            end
    end),
    awful.button({ }, 3, awful.tag.viewtoggle),
    awful.button({ defs.modkey }, 3,
        function(t)
            if client.focus then
                client.focus.toggle_tag(t)
            end
    end),
    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

controls.tasklist_buttons = awful.util.table.join(
    awful.button({ }, 1, function (c)
            if c == client.focus then
                c.minimized = true
            else
                -- Without this, the following
                -- :isvisible() makes no sense
                c.minimized = false
                if not c:isvisible() and c.first_tag then
                    c.first_tag:view_only()
                end
                -- This will also un-minimize
                -- the client, if needed
                client.focus = c
                c:raise()
            end
    end),
    awful.button({ }, 3, function ()
            if instance then
                instance:hide()
                instance = nil
            else
                instance = awful.menu.clients({
                        theme = { width = 250 }
                })
            end
    end),
    awful.button({ }, 4, function ()
            awful.client.focus.byidx(1)
            if client.focus then client.focus:raise() end
    end),
    awful.button({ }, 5, function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
end))

controls.mouse_bindings = awful.util.table.join(
   awful.button({ }, 3, function () mymainmenu:toggle() end),
   awful.button({ }, 4, awful.tag.viewnext),
   awful.button({ }, 5, awful.tag.viewprev)
)

controls.clientkeys = awful.util.table.join(
   awful.key({ defs.modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
   awful.key({ defs.modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
   awful.key({ defs.modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
   awful.key({ defs.modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
   awful.key({ defs.modkey, "Shift"   }, ",",      function (c) c:move_to_screen(1) end),
   awful.key({ defs.modkey, "Shift"   }, ".",      function (c) c:move_to_screen(2) end),
   awful.key({ defs.modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
   awful.key({ defs.modkey,           }, "m",
       function (c)
           c.maximized_horizontal = not c.maximized_horizontal
           c.maximized_vertical   = not c.maximized_vertical
   end)
)

controls.clientbuttons = awful.util.table.join(
   awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
   awful.button({ defs.modkey }, 1, awful.mouse.client.move),
   awful.button({ defs.modkey }, 3, awful.mouse.client.resize)
)

return controls
