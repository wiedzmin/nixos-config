import argparse
import json
import subprocess
import sys

import dmenu
from libtmux import Server
from libtmux.exc import LibTmuxException
import redis


parser = argparse.ArgumentParser(description="Execute command over SSH.")
parser.add_argument("--choices", dest="show_choices", action="store_true",
                   default=False, help="show predefined command choices")

args = parser.parse_args()

r = redis.Redis(host='localhost', port=6379, db=0)

extra_hosts_data = json.loads(r.get("net/extra_hosts"))
extra_hosts = []
for host in extra_hosts_data.values():
    extra_hosts.extend(host)

host = dmenu.show(extra_hosts, prompt="ssh to",
                  case_insensitive=True, lines=10)

if host:
    cmd = "@sshBinary@ {0}".format(host)
    if args.show_choices:
        command_choices = json.loads(r.get("net/command_choices"))
        choice = dmenu.show(command_choices, prompt="execute", case_insensitive=True, lines=5)
        if choice:
           cmd += " -t '{0}'".format(choice)
        else:
           sys.exit(1)

    tmux_server = Server()
    try:
        tmux_session = tmux_server.find_where({ "session_name": "@tmuxDefaultSession@" })
        ssh_window = tmux_session.new_window(attach=True, window_name=host,
                                             window_shell=cmd)
    except LibTmuxException:
        pparams = ["@defaultTerminal@", "-e"]
        pparams.extend(cmd.split())
        subprocess.Popen(pparams)
