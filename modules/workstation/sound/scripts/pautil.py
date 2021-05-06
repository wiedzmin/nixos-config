import argparse
import re
import sys

from more_itertools import chunked

from pystdlib import shell_cmd
from pystdlib.uishim import get_selection_rofi, notify, URGENCY_NORMAL

def construct_grep_re(tokens):
    return " ".join([f'-e "{token}"' for token in tokens])


def get_endpoints_meta(type="sinks"):
    fields = []
    if type == "sources":
        grep_re_fields = ["index", "state", "alsa.card_name", "device.description"]
    elif type == "sinks":
        grep_re_fields = ["index", "state", "alsa.card_name"]
    else:
        raise ValueError("Wrong endpoint type")
    endpoints_meta = shell_cmd(f'pacmd list-{type} | grep {construct_grep_re(grep_re_fields)}').split("\n")
    endpoints_grouped_lines = chunked([meta.strip() for meta in endpoints_meta], len(grep_re_fields))
    result = {}
    for group in endpoints_grouped_lines:
        name = None
        meta = {}
        for entry in group:
            splitted_entry = [x.strip() for x in re.split('[:=]', entry)]
            if splitted_entry[0] == "alsa.card_name":
                name = splitted_entry[1][1:-1]
            if splitted_entry[0].startswith("*"):
                splitted_entry[0] = splitted_entry[0][2:]
                meta["is_default"] = True
            if splitted_entry[0] not in ["index", "state"]:
                meta[splitted_entry[0]] = splitted_entry[1][1:-1]
            else:
                meta[splitted_entry[0]] = splitted_entry[1]
        result[name + " | " + meta["state"]] = meta
    return result


def get_default_endpoint_meta(type="sinks"):
    endpoints_meta = get_endpoints_meta(type=type)
    result = None
    for name in endpoints_meta:
        if endpoints_meta[name].get("is_default", False):
            result = endpoints_meta[name]
            break
    return result


def set_default_sink(args):
    sinks_meta = get_endpoints_meta()
    sink = get_selection_rofi(sorted(sinks_meta.keys()), 'sink')
    if sink:
        meta = sinks_meta[sink]
        shell_cmd(f"pactl set-default-sink {meta['index']}", oneshot=True)


def toggle_suspend_default_sink():
    default_meta = get_default_endpoint_meta(type="sinks")
    if default_meta["state"] in ["IDLE", "RUNNING"]:
        code = "1"
    elif default_meta["state"] in ["SUSPENDED"]:
        code = "0"
    shell_cmd(f"pacmd suspend-sink @DEFAULT_SINK@ {code}", oneshot=True)


def set_default_source(args):
    sources_meta = get_endpoints_meta(type="sources")
    source = get_selection_rofi(sorted(sources_meta.keys()), 'source')
    if source:
        meta = sources_meta[source]
        shell_cmd(f"pactl set-default-source {meta['index']}", oneshot=True)


def toggle_suspend_default_source():
    default_meta = get_default_endpoint_meta(type="source")
    if default_meta["state"] in ["IDLE", "RUNNING"]:
        code = "1"
    elif default_meta["state"] in ["SUSPENDED"]:
        code = "0"
    shell_cmd(f"pacmd suspend-source @DEFAULT_SINK@ {code}", oneshot=True)


def show_status():
    result = []
    sinks_meta = get_endpoints_meta(type="sinks")
    sources_meta = get_endpoints_meta(type="sources")
    result.append("sinks:")
    for name in sinks_meta:
        result.append(f"{name} | {sinks_meta[name]['state']} | {'*' if sinks_meta[name].get('is_default', False) else '-'}")
    result.append("sources:")
    for name in sources_meta:
        result.append(f"{name} | {sources_meta[name]['state']} | {'*' if sources_meta[name].get('is_default', False) else '-'}")
    notify("[pulseaudio]", "\n".join(result), urgency=URGENCY_NORMAL, timeout=7000)


parser = argparse.ArgumentParser(description="Pulseaudio management")
parser.add_argument('--selector-font', dest="selector_font", type=str, help="Selector font")
subparsers = parser.add_subparsers(help="command", dest="cmd")

parser_sink = subparsers.add_parser("sink", help="Operations with sinks")
parser_sink.add_argument('--set-default', dest="sink_set_default", action="store_true",
                           help="Set default sink")
parser_sink.add_argument('--suspend-toggle', dest="sink_suspend_toggle", action="store_true",
                           help="Toggle default sink state")
parser_source = subparsers.add_parser("source", help="Operations with sinks")
parser_source.add_argument('--set-default', dest="source_set_default", action="store_true",
                           help="Set default source")
parser_source.add_argument('--suspend-toggle', dest="source_suspend_toggle", action="store_true",
                           help="Toggle default source state")
parser_status = subparsers.add_parser("status", help="Show PA status")

args = parser.parse_args()


if args.cmd == "status":
    show_status()
    sys.exit(0)
if args.cmd == "sink":
    if args.sink_set_default:
        set_default_sink(args)
        sys.exit(0)
    if args.sink_suspend_toggle:
        toggle_suspend_default_sink()
        sys.exit(0)
if args.cmd == "source":
    if args.source_set_default:
        set_default_source(args)
        sys.exit(0)
    if args.source_suspend_toggle:
        toggle_suspend_default_source
        sys.exit(0)
