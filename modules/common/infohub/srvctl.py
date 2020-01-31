import os
import subprocess
import sys

from notify2 import URGENCY_NORMAL, URGENCY_CRITICAL
import dmenu
import libtmux
import notify2
import redis

services = []

operations = [
    "stop",
    "restart",
    "journal",
    "status",
]

@pythonLibPatch@

notify2.init("srvctl")
r = redis.Redis(host='localhost', port=6379, db=0)

services = r.lrange("system/services", 0, -1)

if not services:
    system_units_task = subprocess.Popen("systemctl list-unit-files", shell=True, stdout=subprocess.PIPE)
    services.extend(["{0} [system]".format(unit.split()[0].split(".")[0])
                     for unit in system_units_task.stdout.read().decode().split("\n")[1:-3]
                     if unit.split()[0].endswith("service")])
    assert system_units_task.wait() == 0

    user_units_task = subprocess.Popen("systemctl --user list-unit-files", shell=True, stdout=subprocess.PIPE)
    services.extend(["{0} [user]".format(unit.split()[0].split(".")[0])
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
    os.system("systemctl {0}stop {1}".format("--user " if "user" in service else "", service.split()[0]))
    n = notify2.Notification("[pkgsctl]", "Stopped {0}".format(service))
    n.set_urgency(URGENCY_CRITICAL)
    n.set_timeout(5000)
    n.show()
elif operation == "restart":
    os.system("systemctl {0}restart {1}".format("--user " if "user" in service else "", service.split()[0]))
    n = notify2.Notification("[pkgsctl]", "Restarted {0}".format(service))
    n.set_urgency(URGENCY_NORMAL)
    n.set_timeout(5000)
    n.show()
elif operation == "status":
    tmux_server = libtmux.Server()
    tmux_session = tmux_server.find_where({ "session_name": "@tmuxDefaultSession@}" })
    status_window = tmux_session.new_window(attach=True, window_name="status for {0}".format(service),
                                                 window_shell="sh -c 'systemctl {0} status {1}; read'".format("--user " if "user" in service else "",
                                                                                                service.split()[0]))
    switch_desktop(1)
else:
    tmux_server = libtmux.Server()
    tmux_session = tmux_server.find_where({ "session_name": "@tmuxDefaultSession@" })
    journal_window = tmux_session.new_window(attach=True, window_name="journal for {0}".format(service),
                                                  window_shell="sh -c 'journalctl {0}-u {1}; read'".format("--user " if "user" in service else "",
                                                                                             service.split()[0]))
    switch_desktop(1)
