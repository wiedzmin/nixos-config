import os
import subprocess
import sys

import dmenu
import libtmux
import redis

services = []

operations = [
    "stop",
    "restart",
    "journal",
    "status",
]

@pythonPatchUIShim@
@pythonPatchXlib@

r = redis.Redis(host='localhost', port=6379, db=0)

services = r.lrange("system/services", 0, -1)

if not services:
    system_units_task = subprocess.Popen("systemctl list-unit-files", shell=True, stdout=subprocess.PIPE)
    services.extend([f"{unit.split()[0].split('.')[0]} [system]"
                     for unit in system_units_task.stdout.read().decode().split("\n")[1:-3]
                     if unit.split()[0].endswith("service")])
    assert system_units_task.wait() == 0

    user_units_task = subprocess.Popen("systemctl --user list-unit-files", shell=True, stdout=subprocess.PIPE)
    services.extend([f"{unit.split()[0].split('.')[0]} [user]"
                     for unit in user_units_task.stdout.read().decode().split("\n")[1:-3]
                     if unit.split()[0].endswith("service")])
    assert system_units_task.wait() == 0

    r.lpush("system/services", *services)

service = dmenu.show(sorted(list(dict.fromkeys([service.decode() for service in services]))), prompt='service', lines=20)
if not service:
    sys.exit(1)
operation = dmenu.show(operations, prompt='> ', lines=5)
if not operation:
    sys.exit(1)
if operation == "stop":
    os.system(f"systemctl {'--user ' if 'user' in service else ''}stop {service.split()[0]}")
    notify("[srvctl]", f"Stopped {service}", urgency=URGENCY_CRITICAL, timeout=5000)
elif operation == "restart":
    os.system(f"systemctl {'--user ' if 'user' in service else ''}restart {service.split()[0]}")
    notify("[srvctl]", f"Restarted {service}", urgency=URGENCY_NORMAL, timeout=5000)
elif operation == "status":
    tmux_server = libtmux.Server()
    tmux_session = tmux_server.find_where({ "session_name": "@tmuxDefaultSession@}" })
    status_window = tmux_session.new_window(
        attach=True, window_name=f"status for {service}",
        window_shell=f"sh -c 'systemctl {'--user ' if 'user' in service else ''} status {service.split()[0]}; read'")
    switch_desktop(1)
else:
    tmux_server = libtmux.Server()
    tmux_session = tmux_server.find_where({ "session_name": "@tmuxDefaultSession@" })
    journal_window = tmux_session.new_window(
        attach=True, window_name=f"journal for {service}",
        window_shell=f"sh -c 'journalctl {'--user ' if 'user' in service else ''}-u {service.split()[0]}; read'")
    switch_desktop(1)
