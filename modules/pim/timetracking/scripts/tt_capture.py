import os
import datetime
import time

from Xlib import X, display, Xatom
from Xlib.error import XError
from cbor2 import dumps, loads
import pytz

from pystdlib import shell_cmd


d = display.Display()
root = d.screen().root

current_time = time.time()
tz_offset_sec = datetime.datetime.now(pytz.timezone("Europe/Moscow")).utcoffset().total_seconds() # <val>/60/60 for hours

_NET_CURRENT_DESKTOP = d.intern_atom("_NET_CURRENT_DESKTOP")
_NET_DESKTOP_NAMES = d.intern_atom("_NET_DESKTOP_NAMES")
_NET_ACTIVE_WINDOW = d.intern_atom("_NET_ACTIVE_WINDOW")
_NET_WM_NAME = d.intern_atom("_NET_WM_NAME")

current_desktop_id = root.get_full_property(_NET_CURRENT_DESKTOP, Xatom.CARDINAL).value.pop()
desktop_names = root.get_full_property(_NET_DESKTOP_NAMES,
                                       X.AnyPropertyType).value.decode().strip('\x00').split('\x00')
current_desktop = desktop_names[current_desktop_id]

active_window = root.get_full_property(_NET_ACTIVE_WINDOW, X.AnyPropertyType).value.pop()
active_window_name = None
active_window_class = None
try:
    active_window_obj = d.create_resource_object("window", active_window)
    active_window_name = active_window_obj.get_full_property(_NET_WM_NAME, X.AnyPropertyType).value.decode("ascii")
    active_window_class = active_window_obj.get_wm_class()[0] or ""
except XError:
    pass

idle_time = shell_cmd("xprintidle", env={"DISPLAY": os.getenv("DISPLAY"),
                                         "XAUTHORITY": os.getenv("XAUTHORITY")})

data = dumps([current_time, tz_offset_sec, current_desktop, active_window_name, active_window_class, idle_time])

# debug printing below
print(current_desktop, active_window_name, active_window_class, idle_time)
print(data)
print(loads(data))
print(tz_offset_sec)
print(dumps(tz_offset_sec))
