import argparse
import os
import subprocess
import sys

import urllib.parse as up


def urlencode(src):
    return up.quote_plus(src.strip())


def construct_proto_uri(template=None, url=None, title=None, body=None, dont_encode=None, in_tmux=False):
    if not url:
        print("error: no URL provided")
        sys.exit(1)
    raw_inputs = dont_encode.split(",")

    params = ["url={0}".format(urlencode(url) if url not in raw_inputs else url)]

    if template:
        params.append("template={0}".format(template))
    if title:
        actual_title = title
        if in_tmux:
            tmux_session_task = subprocess.Popen("tmux display-message -p '#S'",
                                                 shell=True, stdout=subprocess.PIPE)
            actual_title = tmux_session_task.stdout.read().decode().strip()
            assert tmux_session_task.wait() == 0
        params.append("title={0}".format(urlencode(actual_title) if actual_title not in raw_inputs else actual_title))
    if body:
        actual_body = body
        if in_tmux:
            tmux_selection_task = subprocess.Popen('tmux send -X copy-pipe-and-cancel "xsel -i --primary"',
                                                   shell=True, stdout=subprocess.PIPE)
            actual_body = tmux_selection_task.stdout.read().decode().strip()
            assert tmux_selection_task.wait() == 0
        params.append("body={0}".format(urlencode(body) if body not in raw_inputs else body))

    return "org-protocol://capture?{0}".format("&".join(params))


DEBUG_FILE = "{0}/org-capture.log".format(os.environ["HOME"])

parser = argparse.ArgumentParser(description="Manage Firefox stored sessions.")
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

capture_uri_task = subprocess.Popen('emacsclient -c "{0}"'.format(capture_uri), shell=True, stdout=subprocess.PIPE)
assert capture_uri_task.wait() == 0
