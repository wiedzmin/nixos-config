import glob
import os
import sys
import time

from gnupg import GPG

from pystdlib.uishim import get_selection
from pystdlib import shell_cmd


def guess_machine_name():
    return os.readlink("/etc/nixos/configuration.nix").split("/")[1]


def parse_generation_meta(meta):
    meta_items = meta.split()
    is_current = False
    if len(meta_items) == 4:
        is_current = True
    generation = meta_items[0]
    timestamp = " ".join(meta_items[1:])
    return generation, timestamp, is_current


def build_configuration(path="/etc/nixos/configuration.nix", debug=False):
    nixpkgs_path = shell_cmd("nix-build /etc/nixos/nix/sources.nix -A nixpkgs --no-out-link")
    shell_cmd(f'nix build -f {nixpkgs_path}/nixos system{" --show-trace" if debug else ""} -I nixos-config="{path}" -o @configResultPath@',
              shell=True, executable="/run/current-system/sw/bin/zsh",
              stdout=sys.stdout, stderr=sys.stdout,
              exit_error_codes=[1, 100])


def rollback_configuration():
    generations = shell_cmd("pkexec nix-env -p /nix/var/nix/profiles/system --list-generations",
                            split_output="\n")
    generation = get_selection(generations, prompt='>', lines=30, font="@wmFontDmenu@")
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
        new_system_path = os.readlink("@configResultPath@" if not root else root)
    except FileNotFoundError as e:
        new_system_path = None
        print(f"Error switching configuration: {e}")
        sys.exit(1)
    try:
        shell_cmd(f"pkexec nix-env --profile /nix/var/nix/profiles/system --set {new_system_path} && pkexec {new_system_path}/bin/switch-to-configuration switch",
                  shell=True, executable="/run/current-system/sw/bin/zsh",
                  stdout=sys.stdout, stderr=sys.stdout)
    except:
        print(f"Error switching configuration")
        sys.exit(1)


def ensure_kernel_update():
    current_kernel = os.readlink("/run/current-system/kernel")
    booted_kernel = os.readlink("/run/booted-system/kernel")

    if current_kernel != booted_kernel:
        print("Rebooting in 5 sec...")
        time.sleep(5)
        shell_cmd("reboot", oneshot=True)


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

operation = get_selection(operations, prompt='>', lines=10, font="@wmFontDmenu@")

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
    shell_cmd("format-config")
    build_configuration()
    switch_configuration()
elif operation == "Select and build configuration":
    MACHINES_CONFIG_PATH = "/etc/nixos/machines"
    config_entries = os.listdir(MACHINES_CONFIG_PATH)
    configs = { k: v for (k, v) in zip([entry[:-4] if entry.endswith("nix") else entry for entry in config_entries],
                config_entries)}
    result = get_selection(configs.keys(), prompt='config', lines=5, font="@wmFontDmenu@")
    if result:
        build_configuration(path=f"{MACHINES_CONFIG_PATH}/{configs[result]}")
elif operation == "Select and build configuration (debug)":
    MACHINES_CONFIG_PATH = "/etc/nixos/machines"
    config_entries = os.listdir(MACHINES_CONFIG_PATH)
    configs = { k: v for (k, v) in zip([entry[:-4] if entry.endswith("nix") else entry for entry in config_entries],
                config_entries)}
    result = get_selection(configs.keys(), prompt='config', lines=5, font="@wmFontDmenu@")
    if result:
        build_configuration(path=f"{MACHINES_CONFIG_PATH}/{configs[result]}", debug=True)
elif operation == "Link configuration":
    os.chdir("/etc/nixos")
    machines = os.listdir("machines")
    result = get_selection(machines, prompt='config', lines=5, font="@wmFontDmenu@")
    if result:
        os.remove("configuration.nix")
        os.symlink(os.path.relpath(f"machines/{result}/default.nix"), "configuration.nix")


# TODO list
# implement building VM (and probably some other choices from nixos-rebuild)
