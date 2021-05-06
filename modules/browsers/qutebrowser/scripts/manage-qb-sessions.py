import argparse
import sys

from pystdlib.uishim import get_selection_rofi, notify, URGENCY_CRITICAL
from pystdlib import shell_cmd
from pystdlib.browser import collect_sessions, collect_sessions_with_size, \
    collect_session_urls, init_mgmt_argparser, rotate_sessions


parser = init_mgmt_argparser("qutebrowser-session-auto")
parser.add_argument('--selector-font', dest="selector_font", type=str, help="Selector font")
args = parser.parse_args()

if not args.sessions_path:
    notify("[qutebrowser]", f"No sessions base path provided", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)
if args.save_session:
    session_name = get_selection_rofi([], "save as")
    if session_name:
        shell_cmd(f"qb-dump-session {session_name}")
elif args.open_session:
    session_name = get_selection_rofi(sorted(collect_sessions(args.sessions_path)), "open")
    if session_name:
        urls, _ = collect_session_urls(args.sessions_path, session_name)
        shell_cmd(f"emacsclient -c {args.sessions_path}/{session_name}")
elif args.delete_session:
    session_name = get_selection_rofi(sorted(collect_sessions(args.sessions_path)), "delete")
    if session_name:
        shell_cmd(f"rm {args.sessions_path}/{session_name}")
        notify("[qutebrowser]", f"Removed {session_name}", timeout=5000)
elif args.rotate_sessions:
    rotate_sessions(args.sessions_path, args.sessions_name_template, int(args.sessions_history_length))
