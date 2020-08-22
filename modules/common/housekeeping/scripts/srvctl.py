import os
import sys

import redis

from pystdlib.uishim import get_selection, notify
from pystdlib.systemd import list_services, unit_perform, unit_show

services = []

operations = [
    "stop",
    "restart",
    "journal",
    "status",
]


r = redis.Redis(host='localhost', port=6379, db=0)

services = r.lrange("system/services", 0, -1)
if not services:
    services = list_units()
    r.lpush("system/services", *services)

service = get_selection(sorted(list(dict.fromkeys([service.decode() for service in services]))), 'service', lines=20, font="@wmFontDmenu@")
if not service:
    sys.exit(1)
operation = get_selection(operations, '> ', lines=5, font="@wmFontDmenu@")
if not operation:
    sys.exit(1)

if operation in ["journal", "status"]:
    unit_show(service, operation, user=('user' in service),
              shell=["@defaultTerminal@", "-e"], tmux_session="@tmuxDefaultSession@")
    switch_desktop(1)
else:
    unit_perform(service, operation, user=('user' in service))
notify(f"[srvctl :: {operation}]", f"{service}", timeout=5000)
