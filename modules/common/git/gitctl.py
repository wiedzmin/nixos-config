import argparse
import fnmatch
import json
import os
import subprocess
from datetime import datetime

from pygit2 import Repository, GIT_STATUS_CURRENT, GIT_STATUS_IGNORED, \
    GIT_DIFF_STATS_FULL, Tag, Signature, RemoteCallbacks, UserPass
import redis


@pythonPatchNotify@
@pythonPatchMenu@
@pythonPatchPass@


def is_idle_enough():
    xprintidle_task = subprocess.Popen("@xprintidleBinary@", env={"DISPLAY": os.getenv("DISPLAY"),
                                                                  "XAUTHORITY": os.getenv("XAUTHORITY")},
                                       shell=True, stdout=subprocess.PIPE)
    result = xprintidle_task.wait()
    if result != 0:
        return False
    idle_time = xprintidle_task.stdout.read().decode().strip()
    if int(idle_time) >= int("@gitWipIdletimeTreshold@") * 1000:
        return True
    else:
        return False


def is_git_repo(path=None):
    if not path:
        return False
    root = os.path.abspath(path) + "/.git"
    if os.path.exists(root) and os.path.isdir(root):
        return True
    return False


def is_main_active(repo):
    head = repo.references['HEAD'].resolve()
    return not head.endswith("@gitDefaultMainBranchName@")


def resolve_remote(repo, remote_name):
    remote = None
    try:
        remote = repo.remotes[remote_name]
    except KeyError:
        return False
    return remote


def collect_tags(repo):
    result = []
    for refname in repo.listall_references():
        ref = repo.revparse_single(refname)
        if isinstance(ref, Tag):
            result.append(refname)
    return result


def build_auth_callbacks(repo, credentials):
    remote_url = repo.remotes[args.remote].url
    pass_path = None
    for glob in credentials.keys():
        if fnmatch.fnmatch(remote_url, glob):
            pass_path = credentials[glob]["target"]
            break

    if not pass_path:
        print("pass entry not found")
        sys.exit(1)

    entry_data = read_entry(pass_path)
    password = extract_specific_line(entry_data, 0)
    username = extract_by_regex(entry_data, field_regex_mappings["login"])

    if not username:
        print("username not found")
        sys.exit(1)
    if not password:
        print("password not found")
        sys.exit(1)

    return RemoteCallbacks(credentials=UserPass(username, password))


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

# TODO: consider providing per-repo configuration
parser_wip = subparsers.add_parser("wip", help="Managing WIP")
parser_wip.add_argument("--all", dest="wip_all", action="store_true",
                        default=False, help="Stage and commit all dirty state")
parser_wip.add_argument("--push", dest="wip_push", action="store_true",
                        default=False, help="Push WIP to default upstream")
parser_wip.add_argument("--force", dest="wip_force", action="store_true",
                        default=False, help="Force saving WIP (ignore idle time)")
parser_wip.add_argument("--branch-ref", dest="wip_add_branch_name", action="store_true",
                        default=False, help="Prepend WIP commit message with current branch name")


parser_tags = subparsers.add_parser("tags", help="Working with tags")
parser_tags.add_argument("--sync", dest="tags_sync", action="store_true",
                        default=False, help="Sync tags with selected remote")
parser_tags.add_argument("--checkout", dest="tags_checkout", action="store_true",
                        default=False, help="Interactively select and checkout tag locally")
parser_tags.add_argument("--tag", dest="tags_name", # TODO: think of params restructuring
                         default="", help="Tag to checkout in non-interactive mode")

parser_update = subparsers.add_parser("update", help="Updating WIP branches")
parser_update.add_argument("--op", dest="update_op",
                           default="merge", help="Merge|Rebase") # TODO: use choices
parser_update.add_argument("--branch", dest="update_branch",
                           default="master", help="Branch to get updates from")
parser_update.add_argument("--abort", dest="update_abort", action="store_true",
                           default=False, help="Abort unfinished operation")

args = parser.parse_args()

if not is_git_repo(os.getcwd()):
    print("Not a git repo")
    sys.exit(1)

repo = Repository(os.getcwd() + "/.git")
config = repo.config

if args.cmd == "wip":
    if not args.wip_force and not is_idle_enough():
        sys.exit(0)
    if is_main_active(repo):
        print("@gitDefaultMainBranchName@ is untouchable")
        sys.exit(1)
    diff = repo.diff("HEAD")
    changed_lines = diff.stats.insertions + diff.stats.deletions
    if changed_lines > @gitWipChangedLinesTreshold@ or args.wip_force:
        branch = f"{repo.references['HEAD'].resolve().split('/')[-1]}: " if args.wip_add_branch_name else ""
        wip_message = f"{branch}WIP {datetime.now().strftime('%a %d/%m/%Y %H:%M:%S')}"
        index = repo.index
        index.read()
        index.add_all()
        index.write()
        name, email = config.get_multivar("user")
        author = committer = Signature(name, email)
        parents = [repo.head.get_object().hex]
        tree = index.write_tree()
        wip_commit = repo.create_commit("HEAD", author, committer, wip_message, tree, parents)
        if args.wip_push:
            remote.push(specs=["HEAD"], callbacks=build_auth_callbacks(repo, credentials_mapping))
elif args.cmd == "tags":
    if args.tags_sync:
        remote = resolve_remote(repo, args.remote)
        if not remote:
            print("error")
            sys.exit(1)
        remote.fetch(refspecs=["refs/tags/*:refs/tags/*"])
        remote.push(specs=collect_tags(repo), callbacks=build_auth_callbacks(repo, credentials_mapping))
    elif args.tags_checkout:
        tag_name = args.tags_name
        if is_interactive:
            tag_name = dmenu.show(collect_tags(repo), lines=10) # TODO: consider using (py)fzf
        if not tag_name:
            print("No tag to checkout")
            sys.exit(1)
        repo.checkout(tag_name)
elif args.cmd == "update":
    # merge or rebase upstream/origin (elaborate on this)
    print("update")
else:
    print("No command issued")
    sys.exit(0)

# notify(header, msg, urgency=URGENCY_NORMAL, timeout=3000)
