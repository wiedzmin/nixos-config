import glob
import os
import subprocess
import sys
import time

from gnupg import GPG
from pyfzf.pyfzf import FzfPrompt
import dmenu


def guess_machine_name():
    return os.readlink("/etc/nixos/configuration.nix").split("/")[1]


def get_selection(seq, prompt, lines=5, fzf=FzfPrompt()):
    if os.environ.get("CONFCTL_USE_FZF") == "yes":
        return fzf.prompt(seq, '--cycle')[0]
    else:
        return dmenu.show(seq, prompt=prompt, lines=lines)


def locate_nixpkgs():
    locate_nixpkgs_task = subprocess.Popen("nix-build /etc/nixos/nix/sources.nix -A nixpkgs --no-out-link",
                                           shell=True, stdout=subprocess.PIPE)
    nixpkgs_path = locate_nixpkgs_task.stdout.read().decode().strip()
    assert locate_nixpkgs_task.wait() == 0
    return nixpkgs_path


def get_generations():
    get_generations_task = subprocess.Popen("pkexec nix-env -p /nix/var/nix/profiles/system --list-generations",
                                           shell=True, stdout=subprocess.PIPE)
    generations = get_generations_task.stdout.read().decode().strip().split("\n")
    assert get_generations_task.wait() == 0
    return generations


def parse_generation_meta(meta):
    meta_items = meta.split()
    is_current = False
    if len(meta_items) == 4:
        is_current = True
    generation = meta_items[0]
    timestamp = " ".join(meta_items[1:])
    return generation, timestamp, is_current


def format_config():
    format_config_task = subprocess.Popen("format-config",
                                           shell=True, stdout=subprocess.PIPE)
    assert format_config_task.wait() == 0


def build_configuration(path=None, debug=False):
    build_configuration_task = subprocess.Popen(
        f'nix build -f {locate_nixpkgs()}/nixos system{" --show-trace" if debug else ""} -I nixos-config="{path if path else '/etc/nixos/configuration.nix'}"',
        shell=True, executable="/run/current-system/sw/bin/zsh", stdout=sys.stdout, stderr=sys.stdout)
    result = build_configuration_task.wait()
    if result in [1, 100]:
        sys.exit(1)


def rollback_configuration():
    generation = get_selection(get_generations(), prompt='>', lines=30)
    parse_generation_meta(generation)
    generation, timestamp, is_current = parse_generation_meta(generation)
    if is_current:
        print("Skipping rollback to *current* configuration")
        sys.exit(0)
    rollback_target_path = f"/nix/var/nix/profiles/system-{generation}-link"
    print(generation, timestamp, rollback_target_path)
    switch_configuration(root=rollback_target_path)


def switch_configuration(root=None):
    try:
        new_system_path = os.readlink(f"{os.getcwd()}/result" if not root else root)
    except FileNotFoundError as e:
        new_system_path = None
        print(f"Error switching configuration: {e}")
        sys.exit(1)
    switch_configuration_task = subprocess.Popen(
        f"pkexec nix-env --profile /nix/var/nix/profiles/system --set {new_system_path} && pkexec {new_system_path}/bin/switch-to-configuration switch",
        shell=True, executable="/run/current-system/sw/bin/zsh", stdout=sys.stdout, stderr=sys.stdout)
    result = switch_configuration_task.wait()
    if result != 0:
        print(f"Error switching configuration, details below:\n{switch_configuration_task.stderr.read().decode()}")
        sys.exit(1)


def ensure_kernel_update():
    current_kernel = os.readlink("/run/current-system/kernel")
    booted_kernel = os.readlink("/run/booted-system/kernel")

    if current_kernel != booted_kernel:
        print("Rebooting in 5 sec...")
        time.sleep(5)
        os.system("reboot")


machine = guess_machine_name()

operations = [
    "Update current configuration",
    "Update current configuration (debug)",
    "Update current configuration + nixfmt beforehand",
    "Rollback current configuration",
    "Select and build configuration",
    "Select and build configuration (debug)",
    "Link configuration"
]

os.environ["CONFCTL_USE_FZF"] = "no"

if not os.environ.get("DISPLAY"):
    # fallback option, in case xserver is broken in some way
    os.environ["CONFCTL_USE_FZF"] = "yes"

operation = get_selection(operations, prompt='>', lines=10)

if operation == "Update current configuration":
    os.chdir("/etc/nixos")
    build_configuration()
    switch_configuration()
    # ensure_kernel_update()
if operation == "Rollback current configuration":
    rollback_configuration()
elif operation == "Update current configuration (debug)":
    os.chdir("/etc/nixos")
    build_configuration(debug=True)
elif operation == "Update current configuration + nixfmt beforehand":
    os.chdir("/etc/nixos")
    format_config()
    build_configuration()
    switch_configuration()
elif operation == "Select and build configuration":
    MACHINES_CONFIG_PATH = "/etc/nixos/machines"
    config_entries = os.listdir(MACHINES_CONFIG_PATH)
    configs = { k: v for (k, v) in zip([entry[:-4] if entry.endswith("nix") else entry for entry in config_entries],
                config_entries)}
    result = get_selection(configs.keys(), prompt='config', lines=5)
    if result:
        build_configuration(path=f"{MACHINES_CONFIG_PATH}/{configs[result]}")
elif operation == "Select and build configuration (debug)":
    MACHINES_CONFIG_PATH = "/etc/nixos/machines"
    config_entries = os.listdir(MACHINES_CONFIG_PATH)
    configs = { k: v for (k, v) in zip([entry[:-4] if entry.endswith("nix") else entry for entry in config_entries],
                config_entries)}
    result = get_selection(configs.keys(), prompt='config', lines=5)
    if result:
        build_configuration(path=f"{MACHINES_CONFIG_PATH}/{configs[result]}", debug=True)
elif operation == "Link configuration":
    os.chdir("/etc/nixos")
    machines = os.listdir("machines")
    result = get_selection(machines, prompt='config', lines=5)
    if result:
        os.remove("configuration.nix")
        os.symlink(os.path.relpath(f"machines/{result}/default.nix"), "configuration.nix")


# TODO list
# implement building VM (and probably some other choices from nixos-rebuild)
