import argparse
import fnmatch
import json
import os
from datetime import datetime

from pygit2 import Repository, Signature
import redis

from pystdlib.uishim import log_info, log_error
from pystdlib.git import is_repo, is_main_branch_active, is_main_branch_protected, resolve_remote, get_diff_size, \
    build_auth_callbacks
from pystdlib.xlib import is_idle_enough


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
parser.add_argument("--lines-changed", dest="lines_changed",
                    default=100, help="Minimum changed lines count threshold")
# TODO: consider providing per-repo configuration
parser.add_argument("--all", dest="wip_all", action="store_true",
                        default=False, help="Stage and commit all dirty state")
parser.add_argument("--push", dest="wip_push", action="store_true",
                        default=False, help="Push WIP to default upstream")
parser.add_argument("--force", dest="wip_force", action="store_true",
                        default=False, help="Force saving WIP (ignore idle time)")
parser.add_argument("--branch-ref", dest="wip_add_branch_name", action="store_true",
                        default=False, help="Prepend WIP commit message with current branch name")


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


if not args.wip_force and not is_idle_enough("xprintidle"):
    sys.exit(0)
diff_size = get_diff_size(repo)
if diff_size == 0:
    log_info("no changes to commit")
    sys.exit(0)
if diff_size > lines_changed or args.wip_force:
    branch = f"{repo.references['HEAD'].resolve().split('/')[-1]}: " if args.wip_add_branch_name else ""
    wip_message = f"{branch}WIP {datetime.now().strftime('%a %d/%m/%Y %H:%M:%S')}"
    index = repo.index
    index.read()
    index.add_all()
    index.write()
    # user = repo.default_signature
    name = list(config.get_multivar('user.name'))[0]
    email = list(config.get_multivar('user.email'))[0]
    author = committer = Signature(name, email)
    parents = [repo.references['HEAD'].resolve().target]
    tree = index.write_tree()
    wip_commit = repo.create_commit("HEAD", author, committer, wip_message, tree, parents)
    if args.wip_push:
        if is_main_branch_active(repo) and is_main_branch_protected():
            log_info("main branch is untouchable")
            sys.exit(1)
        else:
            remote = resolve_remote(repo, args.remote)
            remote.push(specs=["HEAD"], callbacks=build_auth_callbacks(repo, pass_path))
