import argparse
import datetime
import glob
import json
import os
import sys
from shutil import copyfile

import redis
from yaml import dump

from pystdlib.uishim import get_selection
from pystdlib import shell_cmd

# TODO: consider moving to nix level
markers = [
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


def get_devenv_files_spaced(path, locked_flake=False):
    devenv_filelist = []
    if markers_exist(path, [".devenv"]):
        with open(path + "/.devenv", "r") as f:
            devenv_filelist = f.read().strip().split("\n")
        if not locked_flake:
            devenv_filelist.remove("flake.lock")
    return ' '.join(devenv_filelist)


def get_devenv_stash_token(path):
    os.chdir(path)
    devenv_stash_output = shell_cmd('git stash list --max-count=1 --grep="@projectEnvStashName@"').strip()
    if "@projectEnvStashName@" in devenv_stash_output:
        return devenv_stash_output.split(": ")[0]
    return ""


def execute_commands(path, commands, fail=True):
    os.chdir(path)
    for cmd in commands:
        try:
            shell_cmd(cmd)
        except:
            print(f"'{cmd}' failed")
            if fail:
                sys.exit(1)


def hide_devenv(path):
    devenv_filelist = get_devenv_files_spaced(path, locked_flake=True)
    execute_commands(path, [
        "git reset",
        f"git add -- {devenv_filelist}",
        f"git stash -m '@projectEnvStashName@' -- {devenv_filelist}",
    ], fail=False)


def unhide_devenv(path, devenv_stash_token):
    if devenv_stash_token:
        execute_commands(path, [f'git stash pop {devenv_stash_token}'], fail=False)
    else:
        print("project not initialized")


def project_prefix(path):
    return "_".join(os.path.normpath(path).split(os.sep)[-2:])


def construct_patch_name(path):
    timestamp = datetime.datetime.now().strftime("%d-%m-%Y-%H-%M-%S")
    return f"{project_prefix(path)}-{timestamp}.patch"


parser = argparse.ArgumentParser(description="Devenv automation")
parser.add_argument("--seed", dest="seed_devenv", action="store_true",
                    default=False, help="Create devenv from selected template with selected settings")
parser.add_argument("--remove", dest="remove_devenv", action="store_true",
                    default=False, help="Remove all devenv-related state")
parser.add_argument("--export", dest="export_devenv", action="store_true",
                    default=False, help="Export devenv as patch file")
parser.add_argument("--import", dest="import_devenv", action="store_true",
                    default=False, help="Import saved devenv as patch file (not auto-applied)")
parser.add_argument("--hide", dest="hide_devenv", action="store_true",
                    default=False, help="Hide devenv (i.e. to not push upstream accidentally)")
parser.add_argument("--unhide", dest="unhide_devenv", action="store_true",
                    default=False, help="Unhide devenv")

args = parser.parse_args()

current_dir = os.getcwd()
if args.seed_devenv:
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
        for f in devenv_template_files:
            shell_cmd(f"renderizer --settings={settings_file} {template_source_path}/{f} > {current_dir}/{f}")
    shell_cmd(f"git add -- {get_devenv_files_spaced(current_dir)}",)
    os.remove(current_dir + "/" + settings_file)
    sys.exit(0)
elif args.hide_devenv:
    hide_devenv(current_dir)
elif args.unhide_devenv:
    devenv_stash_token = get_devenv_stash_token(current_dir)
    unhide_devenv(current_dir, devenv_stash_token)
elif args.remove_devenv:
    if markers_exist(current_dir, markers):
        hide_devenv(current_dir)
    devenv_stash_token = get_devenv_stash_token(current_dir)
    if devenv_stash_token:
        execute_commands(current_dir, [f'git stash drop {devenv_stash_token}'])
    else:
        print("project not initialized")
        sys.exit(1)
elif args.export_devenv:
    if markers_exist(current_dir, markers):
        hide_devenv(current_dir)
    devenv_stash_token = get_devenv_stash_token(current_dir)
    if devenv_stash_token:
        execute_commands(current_dir, [f'git stash show -p {devenv_stash_token} > @projectEnvBackupRoot@/{construct_patch_name(current_dir)}'])
    else:
        print("project not initialized")
        sys.exit(1)
    unhide_devenv(current_dir, devenv_stash_token)
elif args.import_devenv:
    prefix = project_prefix(current_dir)
    envs = [os.path.basename(env) for env in glob.glob(f"@projectEnvBackupRoot@/{prefix}*")]
    env = get_selection(envs, "envs: ", lines=5, font="@wmFontDmenu@")
    if env:
        copyfile(f"@projectEnvBackupRoot@/{env}", f"{current_dir}/{env}")
    else:
        print("nothing selected")