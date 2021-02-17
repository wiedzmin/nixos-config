import argparse
import json
import os
import sys

import redis
from yaml import dump

from pystdlib.uishim import get_selection
from pystdlib import shell_cmd

# TODO: consider moving to nix level
markers = [
    ".aux",
    ".devenv",
    ".envrc",
    "flake.nix",
    "shell.nix"
]
settings_file = "settings.yaml"


r = redis.Redis(host='localhost', port=6379, db=0)
project_templates = json.loads(r.get("projectenv/templates"))


def markers_exist(path, files):
    result = False
    for f in files:
        mpath = path + "/" + f
        if (os.path.exists(mpath) and os.path.isfile(mpath)):
            result = True
            break
    return result


def execute_commands(commands):
    for cmd in commands:
        try:
            shell_cmd(cmd)
        except:
            print(f"'{cmd}' failed")
            sys.exit(1)


parser = argparse.ArgumentParser(description="Devenv automation")
parser.add_argument("--seed", dest="populate_template", action="store_true",
                    default=False, help="Add settings for template")
parser.add_argument("--save", dest="save_devenv", action="store_true",
                    default=False, help="Save devenv to StGit patch")
parser.add_argument("--export", dest="export_devenv", action="store_true",
                    default=False, help="Export devenv as patch file")
parser.add_argument("--reset", dest="reset_devenv", action="store_true",
                    default=False, help="Remove all devenv-related state")
parser.add_argument("--repair", dest="repair_devenv", action="store_true",
                    default=False, help="repair devenv commit interconnection (mostly when something was commited above)")
parser.add_argument("--hide", dest="hide_devenv", action="store_true",
                    default=False, help="Hide devenv (i.e. to not push upstream accidentally)")
parser.add_argument("--unhide", dest="unhide_devenv", action="store_true",
                    default=False, help="Unhide devenv")

args = parser.parse_args()

current_dir = os.getcwd()
if args.populate_template:
    if markers_exist(current_dir, markers):
        print("project already initialized")
        sys.exit(1)
    settings = json.loads(r.get("projectenv/settings"))
    token = get_selection(settings.keys(), "settings: ", lines=5, font="@wmFontDmenu@")
    if not token:
        print("no settings to instantiate")
        sys.exit(1)
    with open(f"{current_dir}/{settings_file}", "w") as f:
        f.write(dump(settings[token]))
    template = get_selection(project_templates.keys(), "template: ", lines=5, font="@wmFontDmenu@")
    if template:
        template_source_path = project_templates[template]
        devenv_template_files = os.listdir(template_source_path)
        if markers_exist(current_dir, [".pre-commit-config.yaml"]):
            devenv_template_files.remove(".pre-commit-config.yaml") # should not touch existing one
        for f in devenv_template_files:
            shell_cmd(f"renderizer --settings={settings_file} {template_source_path}/{f} > {current_dir}/{f}")
    devenv_filelist = None
    with open(current_dir + "/.devenv", "r") as f:
        devenv_filelist = f.read().strip().split("\n")
    devenv_filelist.remove("flake.lock") # does not yet exist
    devenv_files = ' '.join(devenv_filelist)
    populate_devenv_commands = [
        "git branch -D master.stgit || exit 0",
        "stg init &> /dev/null || exit 0",
    ]
    execute_commands(populate_devenv_commands)
    shell_cmd(f"git add -- {devenv_files}",)
    os.remove(current_dir + "/" + settings_file)
    sys.exit(0)
elif args.save_devenv:
    if not markers_exist(current_dir, markers):
        print("project not initialized")
        sys.exit(1)
    devenv_output = shell_cmd('git log --pretty=format:%s -1').strip()
    if devenv_output == "devenv":
        print("devenv already saved")
        sys.exit(1)
    devenv_filelist = None
    with open(current_dir + "/.devenv", "r") as f:
        devenv_filelist = f.read().strip().split("\n")
    devenv_files = ' '.join(devenv_filelist)
    save_devenv_commands = [ # FIXME: exclude current changes from devenv patch
        "stg new devenv -m 'devenv' --no-verify",
        "git reset",
        f"git add -- {devenv_files}",
        "stg refresh --force",
    ]
    execute_commands(save_devenv_commands)
    sys.exit(0)
elif args.reset_devenv:
    if not markers_exist(current_dir, markers):
        print("project not initialized")
        sys.exit(1)
    execute_commands([
        "stg repair &> /dev/null || exit 0",
        "stg delete devenv",
    ])
elif args.repair_devenv:
    if not markers_exist(current_dir, markers):
        print("project not initialized")
        sys.exit(1)
    execute_commands([
        "git reset --soft HEAD~1",
        "git stash",
        "devenv --hide",
        "git stash pop --index",
        "git commit -m 'repaired HEAD'",
    ])
elif args.export_devenv:
    execute_commands(["stg export -p -n -- devenv"])
elif args.hide_devenv:
    execute_commands(["command -v stg &> /dev/null && stg pop devenv || exit 0"])
elif args.unhide_devenv:
    execute_commands(["command -v stg &> /dev/null && stg push devenv || exit 0"])
