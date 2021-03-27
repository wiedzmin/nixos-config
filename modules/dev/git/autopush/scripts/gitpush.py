import argparse
import fnmatch
import json
import os

from pygit2 import Repository
import redis

from pystdlib.uishim import log_error
from pystdlib.git import is_repo, get_active_branch, resolve_remote, build_auth_callbacks


r = redis.Redis(host='localhost', port=6379, db=0)
credentials_mapping = json.loads(r.get("git/credentials_mapping"))

parser = argparse.ArgumentParser(description="Some git automation")

parser.add_argument("--respect-hooks", dest="respect_hooks", action="store_true",
                    default=False, help="respect pre-{commit/push} hooks")
parser.add_argument("--dry-run", dest="dry_run", action="store_true",
                    default=False, help="Dry run")
parser.add_argument("--remote", dest="remote",
                    default="origin", help="Git remote to work with")parser.add_argument("--branch", dest="update_source_branch",
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

remote.push(specs=["HEAD"], callbacks=build_auth_callbacks(repo, pass_path))
