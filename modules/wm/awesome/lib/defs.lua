local defs = {}

defs.modkey = "Mod4"
defs.altkey = "Mod1"

defs.kbdd_dbus_prev_cmd = "dbus-send --dest=ru.gentoo.KbddService /ru/gentoo/KbddService ru.gentoo.kbdd.prev_layout"
defs.kbdd_dbus_next_cmd = "dbus-send --dest=ru.gentoo.KbddService /ru/gentoo/KbddService ru.gentoo.kbdd.next_layout"

-- defs.taglist_font = "Consolas 14"
-- defs.tasklist_font = "Consolas 12"
defs.default_font = "mononoki 10"

defs.taglist_font = "mononoki 14"
defs.tasklist_font = "mononoki 12"
defs.notifications_font = "mononoki 12"

defs.sloppy_focus = false

defs.terminal = "urxvt"
defs.editor = os.getenv("EDITOR") or "vim"
defs.editor_cmd = defs.terminal .. " -e " .. defs.editor
defs.browser = {
   command = "firefox-bin",
   class = "Firefox",
   params = "-new-tab"
}

return defs
