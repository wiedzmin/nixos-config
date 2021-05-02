import argparse
import json
import sys

import redis

from pystdlib.uishim import get_selection, notify, URGENCY_CRITICAL
from pystdlib.shell import term_create_window, tmux_create_window
from pystdlib import shell_cmd


def format_host_meta(host, meta, terse=False):
    ips = meta.get('ips')
    if not ips:
        notify("[sshmenu]", "missing '{ips}' attribute for '{host}'", urgency=URGENCY_CRITICAL)
        sys.exit(1)
    ip = ips[0]
    user = meta.get("user")
    port = meta.get("port")
    result = f"{' -l ' + user if user else ''}{' -p ' + str(port) if port else ''} {ip}"
    if terse:
        result = f"{user + '@' if user else ''}{ip}{':' + str(port) if port else ''}"
    return result


def list_jump_hosts(host_meta, extra_hosts_data):
    result = []
    meta = host_meta
    while True:
        jump = meta.get("jump")
        if not jump:
            break
        jump_meta = extra_hosts_data.get(jump)
        if not jump_meta:
            notify("[sshmenu]", "missing host definition for `{jump}`", urgency=URGENCY_CRITICAL)
            sys.exit(1)
        jump_meta.update({"host": jump})
        result.insert(0, jump_meta)
        meta = jump_meta
    return result


parser = argparse.ArgumentParser(description="Execute command over SSH.")
parser.add_argument("--choices", dest="show_choices", action="store_true",
                   default=False, help="show predefined command choices")
parser.add_argument('--term-command', dest="term_command", type=str, help="Terminal command")
parser.add_argument('--tmux-session', dest="tmux_session", default="main", type=str, help="Fallback tmux session name")
parser.add_argument("--ignore-tmux", dest="ignore_tmux", action="store_true",
                   default=False, help="open connection in new terminal window rather than tmux pane")
parser.add_argument('--dmenu-font', dest="dmenu_font", type=str, help="Dmenu font")
args = parser.parse_args()

r = redis.Redis(host='localhost', port=6379, db=0)
extra_hosts_data = json.loads(r.get("net/extra_hosts"))

host = get_selection(extra_hosts_data.keys(), "ssh to", case_insensitive=True, lines=10, font=args.dmenu_font)


if host:
    host_meta = extra_hosts_data[host]
    host_vpn = host_meta.get("vpn")
    if host_vpn:
        shell_cmd(f"vpnctl --start {host_vpn}")
    jump_hosts = list_jump_hosts(host_meta, extra_hosts_data)
    cmd = f"ssh {' '.join(['-J ' + format_host_meta(host_meta['host'], host_meta, terse=True) for host_meta in jump_hosts])} {format_host_meta(host, host_meta)}"
    if args.show_choices:
        command_choices = json.loads(r.get("net/command_choices"))
        choice = get_selection(command_choices, "execute", case_insensitive=True, lines=5, font=args.dmenu_font)
        if choice:
           cmd += f" -t '{choice}'"
        else:
           sys.exit(1)

    if args.ignore_tmux:
        term_create_window(cmd, term_cmd=args.term_command)
    else:
        target_session = host_meta.get("tmux", args.tmux_session)
        result = tmux_create_window(cmd, session_name=target_session, window_title=f"ssh :: {host}",
                                    attach=False)
        if not result:
            notify("[sshmenu]", "error creating tmux window", urgency=URGENCY_CRITICAL)
            sys.exit(1)
        else:
            result = term_create_window(f"tmux attach -t {target_session}", term_cmd=args.term_command)
