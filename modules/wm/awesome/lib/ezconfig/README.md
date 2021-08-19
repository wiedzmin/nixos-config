awesome-ezconfig
================

Easier key and button setup for the [Awesome][0] tiling window
manager. Inspired by [Xmonad's EZConfig][1] and Emacs's kbd function.

Usage
=====

```lua
ezconfig = require('ezconfig')

ezconfig.modkey = 'Mod4'
ezconfig.altkey = 'Mod1'

globalkeys = ezconfig.keytable.join({
   ['M-<Left>'] = awful.tag.viewprev,
   ['M-S-j'] = {awful.client.swap.byidx, 1},
   ['M-C-k'] = {awful.screen.focus_relative, -1},
   ['M-m'] = function () menus.main:show() end,
})

-- The above is functionally equivalent to:
key = ezconfig.key
globalkeys = awful.util.table.join(
   key('M-<Left>', awful.tag.viewprev),
   key('M-S-j', {awful.client.swap.byidx, 1}),
)

-- Which is in turn equivalent to:
globalkeys = awful.util.table.join(
   awful.key({modkey}, 'Left', awful.tag.viewprev),
   awful.key({modkey, 'Shift'}, 'j', function () awful.client.swap.byidx(1) end),
)

-- It is also possible to configure mouse buttons:
clientbuttons = ezconfig.btntable.join({
   ['1'] = function (c) client.focus = c; c:raise() end,
   ['M-1'] = awful.mouse.client.move,
   ['M-3'] = awful.mouse.client.resize,
})
```

License
=======

This lua module is licensed under the same terms as Awesome itself -
the [GNU GPLv2][2].

[0]: http://awesome.naquadah.org/
[1]: http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-EZConfig.html
[2]: https://raw.githubusercontent.com/gvalkov/awesome-ezconfig/master/LICENSE
