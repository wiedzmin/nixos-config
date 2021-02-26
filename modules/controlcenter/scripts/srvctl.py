import argparse
import os
import sys

import redis

from pystdlib import shell_cmd
from pystdlib.uishim import get_selection, notify
from pystdlib.systemd import list_units, unit_perform, unit_show
from pystdlib.xlib import switch_desktop

services = []

operations = [
    "stop",
    "restart",
    "journal",
    "journal/follow",
    "status",
]

parser = argparse.ArgumentParser(description="SystemD services management.")
parser.add_argument("--invalidate-cache", "-i", dest="invalidate", action="store_true",
                    help="drop units cache from Redis")

args = parser.parse_args()

r = redis.Redis(host='localhost', port=6379, db=0)


if args.invalidate:
    shell_cmd("pkexec systemctl daemon-reload", shell=True,
              stdout=sys.stdout, stderr=sys.stdout)
    r.delete("system/services")
    sys.exit(0)

if not r.exists("system/services"):
    r.lpush("system/services", *list_units())

service = get_selection(sorted(list(dict.fromkeys([service.decode() for service in r.lrange("system/services", 0, -1)]))),
                        'service', lines=20, font="@wmFontDmenu@")
if not service:
    sys.exit(1)
operation = get_selection(operations, '> ', lines=5, font="@wmFontDmenu@")
if not operation:
    sys.exit(1)

if operation in ["journal", "journal/follow", "status"]:
    if "follow" in operation:
        unit_show(service, "journal", user=('user' in service), follow=True,
                  shell="@defaultVTCmd@", tmux_session="@tmuxDefaultSession@")
    else:
        unit_show(service, operation, user=('user' in service),
                  shell="@defaultVTCmd@", tmux_session="@tmuxDefaultSession@")
    switch_desktop(1)
else:
    unit_perform(service, operation, user=('user' in service))
notify(f"[srvctl :: {operation}]", f"{service}", timeout=5000)
