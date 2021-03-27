import argparse
import fnmatch
import json
import os
import sys

from pygit2 import GIT_BRANCH_REMOTE, GIT_RESET_HARD, Repository
import redis

from pystdlib.uishim import log_error
from pystdlib.git import is_repo, get_active_branch, resolve_remote


r = redis.Redis(host='localhost', port=6379, db=0)
credentials_mapping = json.loads(r.get("git/credentials_mapping"))

parser = argparse.ArgumentParser(description="Some git automation")

parser.add_argument("--respect-hooks", dest="respect_hooks", action="store_true",
                    default=False, help="respect pre-commit hooks")
parser.add_argument("--dry-run", dest="dry_run", action="store_true",
                    default=False, help="Dry run")
parser.add_argument("--remote", dest="remote",
                    default="origin", help="Git remote to work with")
parser.add_argument("--op", dest="update_op",
                    default="merge", choices=["fetch", "merge", "rebase"],
                    help="Operation to perform")
parser.add_argument("--branch", dest="update_source_branch",
                    default="", help="Branch to get updates from")

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


source_branch_name = args.update_source_branch if args.update_source_branch != "" else get_active_branch(repo)
remote = resolve_remote(repo, args.remote)
if not remote:
    log_error(f"cannot find remote '{args.remote}'")
    sys.exit(1)
if args.update_op == "fetch":
    remote.fetch(refspecs=[f"refs/heads/*:refs/heads/*"])
elif args.update_op == "merge":
    source_branch_head = repo.references[source_branch_name].resolve().target
    repo.merge(source_branch_head)
elif args.update_op == "rebase":
    source_branch = repo.lookup_branch(source_branch_name, GIT_BRANCH_REMOTE)
    dest_branch = repo.lookup_branch(get_active_branch(repo))
    dest_branch.set_target(source_branch.target)
    # Fast-forwarding with set_target() leaves the index and the working tree
    # in their old state. That's why we need to checkout() and reset()
    repo.checkout(f"refs/heads/{dest_branch.name}")
    repo.reset(dest_branch.target, GIT_RESET_HARD)
