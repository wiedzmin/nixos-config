import argparse
import json
import subprocess
import sys

from pystdlib.uishim import get_selection
from libtmux import Server
from libtmux.exc import LibTmuxException
import redis


parser = argparse.ArgumentParser(description="Execute command over SSH.")
parser.add_argument("--choices", dest="show_choices", action="store_true",
                   default=False, help="show predefined command choices")
parser.add_argument("--ignore-tmux", dest="ignore_tmux", action="store_true",
                   default=False, help="open connection in new terminal window rather than tmux pane")

args = parser.parse_args()

r = redis.Redis(host='localhost', port=6379, db=0)

extra_hosts_data = json.loads(r.get("net/extra_hosts"))
extra_hosts = []
for host in extra_hosts_data.values():
    extra_hosts.append(host)

host = get_selection(extra_hosts, "ssh to", case_insensitive=True, lines=10, font="@wmFontDmenu@")


def open_terminal(cmd):
    pparams = ["@defaultTerminal@", "-e"]
    pparams.extend(cmd.split())
    subprocess.Popen(pparams)


if host:
    vpn_meta = json.loads(r.get("net/hosts_vpn"))
    host_vpn = vpn_meta.get(host, None)
    if host_vpn:
        vpn_start_task = subprocess.Popen(f"vpnctl --start {host_vpn}",
                                          shell=True, stdout=subprocess.PIPE)
        assert vpn_start_task.wait() == 0
    cmd = f"ssh {host}"
    if args.show_choices:
        command_choices = json.loads(r.get("net/command_choices"))
        choice = get_selection(command_choices, "execute", case_insensitive=True, lines=5, font="@wmFontDmenu@")
        if choice:
           cmd += f" -t '{choice}'"
        else:
           sys.exit(1)

    if args.ignore_tmux:
        open_terminal(cmd)
    else:
        tmux_sessions = json.loads(r.get("tmux/extra_hosts"))
        session_name = tmux_sessions.get(host) or "@tmuxDefaultSession@"
        tmux_server = Server()
        try:
            tmux_session = tmux_server.find_where({ "session_name": session_name })
            if not tmux_session:
                open_terminal(cmd)
            else:
                tmux_session.switch_client()
                ssh_window = tmux_session.new_window(attach=True, window_name=host,
                                                     window_shell=cmd)
        except LibTmuxException:
            open_terminal(cmd)
