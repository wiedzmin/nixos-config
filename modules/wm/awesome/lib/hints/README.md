hints
=====

Window picker with hints for Awesome

Installation
============

Put hints.lua in the awesome config directory (~/.config/awesome)

Load hints module (in rc.lua):
```lua
require("hints")
```
Initialize (in rc.lua, must be after beautiful.init()):
```lua
hints.init()
```
Add a keybinding to run hints.focus() (in rc.lua in globalkeys declaration):
```lua
awful.key({ modkey }, "j", function () hints.focus() end),
```
Usage
=====
Now, when you press modkey + j, letters will pop up on each window and you can select a window to focus by pressing the corresponding letter.
