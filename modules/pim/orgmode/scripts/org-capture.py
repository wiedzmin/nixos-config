import argparse
import os
import sys

import urllib.parse as up
from pystdlib import shell_cmd


def urlencode(src):
    return up.quote_plus(src.strip())


def construct_proto_uri(template=None, url=None, title=None, body=None, dont_encode=None, in_tmux=False):
    if not url:
        print("error: no URL provided")
        sys.exit(1)
    raw_inputs = dont_encode.split(",")

    params = [f"url={urlencode(url) if url not in raw_inputs else url}"]

    if template:
        params.append(f"template={template}")
    if title:
        actual_title = title
        if in_tmux:
            actual_title = shell_cmd("tmux display-message -p '#S'")
        params.append(f"title={urlencode(actual_title) if actual_title not in raw_inputs else actual_title}")
    if body:
        actual_body = body
        if in_tmux:
            actual_body = shell_cmd('tmux send -X copy-pipe-and-cancel "xsel -i --primary"')
        params.append(f"body={urlencode(body) if body not in raw_inputs else body}")

    return f"org-protocol://capture?{'&'.join(params)}"


DEBUG_FILE = f'{os.environ["HOME"]}/org-capture.log'

parser = argparse.ArgumentParser(description="Org-capture proxy.")
parser.add_argument("--debug", "-d", dest="debug", action="store_true",
                    help="dump org-protocol URI to <DEBUG_FILE>")
parser.add_argument("--template", "-k", dest="template", help="org-capture template key")
parser.add_argument("--url", "-u", dest="url", help="url to store")
parser.add_argument("--title", "-t", dest="title", help="title to store")
parser.add_argument("--body", "-b", dest="body", help="selection to attach")
parser.add_argument("--dont-encode", "-e", dest="dont_encode",
                    help="comma-separated list of fields that should be used as is")

args = parser.parse_args()
in_tmux = os.environ.get("TMUX", None)

capture_uri = construct_proto_uri(template=args.template, url=args.url, title=args.title,
                                  body=args.body, dont_encode=args.dont_encode)

if args.debug:
    with open(DEBUG_FILE, "w") as debug:
        debug.write(capture_uri + "\n")

shell_cmd(f'emacsclient -s /run/user/{os.getuid()}/emacs/server "{capture_uri}"')
