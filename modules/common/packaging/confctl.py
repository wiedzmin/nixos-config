import glob
import os
import subprocess
import sys
import time

from gnupg import GPG
import dmenu


def guess_machine_name():
    return os.readlink("/etc/nixos/configuration.nix").split("/")[1]


def locate_nixpkgs():
    locate_nixpkgs_task = subprocess.Popen("nix-build /etc/nixos/nix/sources.nix -A nixpkgs --no-out-link",
                                           shell=True, stdout=subprocess.PIPE)
    nixpkgs_path = locate_nixpkgs_task.stdout.read().decode().strip()
    assert locate_nixpkgs_task.wait() == 0
    return nixpkgs_path


def format_config():
    format_config_task = subprocess.Popen("format-config",
                                           shell=True, stdout=subprocess.PIPE)
    assert format_config_task.wait() == 0

def build_configuration(path=None, debug=False):
    build_configuration_task = subprocess.Popen("nix build -f {0}/nixos system{1}{2}".format(
        locate_nixpkgs(),
        " --show-trace" if debug else "",
        ' -I nixos-config="{0}"'.format(path if path else "/etc/nixos/configuration.nix")
    ), shell=True, executable="/run/current-system/sw/bin/zsh", stdout=sys.stdout, stderr=sys.stdout)
    result = build_configuration_task.wait()
    if result in [1, 100]:
        sys.exit(1)


def switch_configuration():
    try:
        new_system_path = os.readlink("{0}/result".format(os.getcwd()))
    except FileNotFoundError as e:
        new_system_path = None
        print("Error switching configuration: {0}".format(e))
        sys.exit(1)
    switch_configuration_task = subprocess.Popen("pkexec nix-env --profile /nix/var/nix/profiles/system --set {0} && pkexec {0}/bin/switch-to-configuration switch".format(
                                                 new_system_path), shell=True, executable="/run/current-system/sw/bin/zsh", stdout=sys.stdout, stderr=sys.stdout)
    result = switch_configuration_task.wait()
    if result != 0:
        print("Error switching configuration") # TODO: add details
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
    "Select and build configuration",
    "Select and build configuration (debug)",
    "Link configuration"
]

# fallback option, in case xserver is broken in some way
# TODO: consider using fzf in such case
if not os.environ.get("DISPLAY"):
    os.chdir("/etc/nixos")
    build_configuration()
    switch_configuration()
    sys.exit(0)

operation = dmenu.show(operations, prompt='>', lines=10)

if operation == "Update current configuration":
    os.chdir("/etc/nixos")
    build_configuration()
    switch_configuration()
    # ensure_kernel_update()
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
    result = dmenu.show(configs.keys(), prompt='config', lines=5)
    if result:
        build_configuration(path="{0}/{1}".format(MACHINES_CONFIG_PATH, configs[result]))
elif operation == "Select and build configuration (debug)":
    MACHINES_CONFIG_PATH = "/etc/nixos/machines"
    config_entries = os.listdir(MACHINES_CONFIG_PATH)
    configs = { k: v for (k, v) in zip([entry[:-4] if entry.endswith("nix") else entry for entry in config_entries],
                config_entries)}
    result = dmenu.show(configs.keys(), prompt='config', lines=5)
    if result:
        build_configuration(path="{0}/{1}".format(MACHINES_CONFIG_PATH, configs[result]), debug=True)
elif operation == "Link configuration":
    os.chdir("/etc/nixos")
    machines = os.listdir("machines")
    result = dmenu.show(machines, prompt='config', lines=5)
    if result:
        os.remove("configuration.nix")
        os.symlink(os.path.relpath("machines/{0}/default.nix".format(result)), "configuration.nix")


# TODO list
# implement building VM (and probably some other choices from nixos-rebuild)
