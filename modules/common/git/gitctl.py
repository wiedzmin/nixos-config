import argparse
import fnmatch
import json
import os
import subprocess
from datetime import datetime

from pygit2 import GIT_BRANCH_REMOTE, GIT_DIFF_STATS_FULL, GIT_RESET_HARD, GIT_STATUS_CURRENT, \
    GIT_STATUS_IGNORED, RemoteCallbacks, Repository, Signature, Tag, UserPass
import redis


@pythonPatchNotify@
@pythonPatchUIShim@
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
    return os.path.exists(root) and os.path.isdir(root)


def get_active_branch(repo):
    head = None
    try:
        head = repo.references['HEAD'].resolve()
    except KeyError as e:
        return head
    return head.name


def is_main_branch_active(repo):
    return get_active_branch(repo).endswith("@gitDefaultMainBranchName@")


def is_main_branch_protected():
    allow_token = os.path.abspath(os.getcwd()) + "/.unseal_@gitDefaultMainBranchName@"
    return os.path.exists(allow_token) and (os.path.isfile(allow_token) or os.path.islink(allow_token))


def get_diff_size(repo):
    active_branch = get_active_branch(repo)
    if not active_branch:
        print("probably empty repo")
        return 0
    diff = repo.diff()
    return diff.stats.insertions + diff.stats.deletions


def resolve_remote(repo, remote_name):
    remote = None
    try:
        remote = repo.remotes[remote_name]
    except KeyError:
        pass
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
                           default="merge", choices=["fetch", "merge", "rebase", "push"],
                           help="Operation to perform")
parser_update.add_argument("--branch", dest="update_source_branch",
                           default="", help="Branch to get updates from")

args = parser.parse_args()

if not is_git_repo(os.getcwd()):
    print("Not a git repo")
    sys.exit(1)

repo = Repository(os.getcwd() + "/.git")
config = repo.config


# FIXME: user identity (name + email) is not always set at repo level
# that said, we need a SPOT for git identities as used/implemented
# in git-identity emacs package
if args.cmd == "wip":
    if not args.wip_force and not is_idle_enough():
        sys.exit(0)
    diff_size = get_diff_size(repo)
    if diff_size == 0:
        print("no changes to commit")
        sys.exit(0)
    if diff_size > @gitWipChangedLinesTreshold@ or args.wip_force:
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
                print("@gitDefaultMainBranchName@ is untouchable")
                sys.exit(1)
            else:
                remote = resolve_remote(repo, args.remote)
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
    source_branch_name = args.update_source_branch if args.update_source_branch != "" else get_active_branch(repo)
    remote = resolve_remote(repo, args.remote)
    if not remote:
        print("error")
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
    elif args.update_op == "push":
        remote.push(specs=["HEAD"], callbacks=build_auth_callbacks(repo, credentials_mapping))
else:
    print("No command issued")
    sys.exit(0)
