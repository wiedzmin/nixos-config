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

host = get_selection(extra_hosts_data.keys(), "ssh to", case_insensitive=True, lines=10, font="@wmFontDmenu@")

def open_terminal(cmd):
    pparams = ["@defaultTerminal@", "-e"]
    pparams.extend(cmd.split())
    subprocess.Popen(pparams)


if host:
    host_meta = extra_hosts_data[host]
    host_vpn = host_meta.get("vpn", None)
    if host_vpn:
        vpn_start_task = subprocess.Popen(f"vpnctl --start {host_vpn}",
                                          shell=True, stdout=subprocess.PIPE)
        assert vpn_start_task.wait() == 0
    ssh_user = host_meta.get("user", None)
    ssh_port = host_meta.get("port", None)
    cmd = f"ssh{' -l ' + ssh_user if ssh_user else ''}{' -p ' + str(ssh_port) if ssh_port else ''} {host_meta['ips'][0]}"
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
        session_name = host_meta.get("tmux", "@tmuxDefaultSession@")
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
