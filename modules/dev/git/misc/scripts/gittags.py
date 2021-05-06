import argparse
import fnmatch
import json
import os
import sys

from pygit2 import Repository
import redis

from pystdlib.uishim import is_interactive, get_selection_rofi, log_error
from pystdlib.git import is_repo, resolve_remote, collect_tags, build_auth_callbacks


r = redis.Redis(host='localhost', port=6379, db=0)
credentials_mapping = json.loads(r.get("git/credentials_mapping"))

parser = argparse.ArgumentParser(description="Some git automation")
subparsers = parser.add_subparsers(help="command", dest="cmd")

parser.add_argument("--respect-hooks", dest="respect_hooks", action="store_true",
                    default=False, help="respect pre-{commit/push} hooks")
parser.add_argument("--dry-run", dest="dry_run", action="store_true",
                    default=False, help="Dry run")
parser.add_argument("--remote", dest="remote",
                    default="origin", help="Git remote to work with")
parser.add_argument('--selector-font', dest="selector_font", type=str, help="Selector font")
parser_tags = subparsers.add_parser("tags", help="Working with tags")
parser_tags.add_argument("--sync", dest="tags_sync", action="store_true",
                        default=False, help="Sync tags with selected remote")
parser_tags.add_argument("--checkout", dest="tags_checkout", action="store_true",
                        default=False, help="Interactively select and checkout tag locally")
parser_tags.add_argument("--tag", dest="tags_name",
                         default="", help="Tag to checkout in non-interactive mode")

args = parser.parse_args()

if not is_repo(os.getcwd()):
    log_error("Not a git repo")
    sys.exit(1)

repo = Repository(os.getcwd() + "/.git")
config = repo.config

remote_url = repo.remotes[args.remote].url
pass_path = None
for glob in credentials_mapping.keys():
    if fnmatch.fnmatch(remote_url, glob):
        pass_path = credentials_mapping[glob]["target"]


if args.tags_sync:
    remote = resolve_remote(repo, args.remote)
    if not remote:
        log_error(f"cannot find remote '{args.remote}'")
        sys.exit(1)
    remote.fetch(refspecs=["refs/tags/*:refs/tags/*"])
    remote.push(specs=collect_tags(repo), callbacks=build_auth_callbacks(repo, pass_path))
elif args.tags_checkout:
    tag_name = args.tags_name
    if is_interactive:
        tag_name = get_selection_rofi(collect_tags(repo), "")
    if not tag_name:
        log_error("No tag to checkout")
        sys.exit(1)
    repo.checkout(tag_name)
