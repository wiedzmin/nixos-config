{ config, lib, pkgs, ... }:
with import ../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.custom.packaging;
  get-pr-override = pkgs.writeShellScriptBin "get-pr-override" ''
    PR_NO=$1
    HASH=$(curl -sL https://github.com/NixOS/nixpkgs/pull/''${PR_NO}.patch \
           | head -n 1 | grep -o -E -e "[0-9a-f]{40}")
    echo pr''${PR_NO} = "import (fetchTarball"
    echo "  \"\''${config.attributes.paths.nixpkgs.archive}''${HASH}.tar.gz\")"
    echo "    { config = config.nixpkgs.config; };"
  '';
  format-config = pkgs.writeShellScriptBin "format-config" ''
    sources=$(${pkgs.fd}/bin/fd -t file nix -E forges -E "*.gpg*" /etc/nixos)
    for file in "$sources"; do
      ${pkgs.nixfmt}/bin/nixfmt -w 120 $file
    done
  '';
  make-package-diff = pkgs.writeShellScriptBin "make-package-diff" ''
    PACKAGE=$1
    TMP=`${pkgs.coreutils}/bin/mktemp -d`
    cd $TMP
    {
        ${pkgs.nix}/bin/nix-shell "<nixpkgs>" -A $PACKAGE --run "unpackPhase"
        ${pkgs.coreutils}/bin/mv * a
        ${pkgs.coreutils}/bin/cp -r a b
        $EDITOR b
    } 2>&1 > /dev/null
    ${pkgs.diffutils}/bin/diff -u --suppress-common-lines a b
  '';
  confctl = writePythonScriptWithPythonPackages "confctl" [  # TODO: consider making relative config paths
    pkgs.python3Packages.dmenu-python
    pkgs.python3Packages.python-gnupg
  ] ''
    import glob
    import os
    import subprocess
    import sys

    from gnupg import GPG
    import dmenu


    def guess_machine_name():
        return os.readlink("/etc/nixos/configuration.nix").split("/")[1]


    def decrypt_secrets(machine):
        secrets_dir = "/etc/nixos/machines/{0}/secrets".format(machine)
        secrets = glob.glob('{0}/*.gpg'.format(secrets_dir))
        gpg = GPG()
        for secret in secrets:
            secret_decrypted = os.path.splitext(secret)[0]
            with open(secret, "rb") as src:
                with open(secret_decrypted, "w") as dst:
                      dst.write(str(gpg.decrypt_file(src)))

    def locate_nixpkgs():
        locate_nixpkgs_task = subprocess.Popen("nix-build /etc/nixos/nix/sources.nix -A nixpkgs --no-out-link",
                                               shell=True, stdout=subprocess.PIPE)
        nixpkgs_path = locate_nixpkgs_task.stdout.read().decode().strip()
        assert locate_nixpkgs_task.wait() == 0
        return nixpkgs_path


    def build_configuration(path=None, debug=False):
        build_configuration_task = subprocess.Popen("${pkgs.nix}/bin/nix build -f {0}/nixos system{1}{2}".format(
                                                    locate_nixpkgs(),
                                                    " --show-trace" if debug else "",
                                                    ' -I nixos-config="{0}"'.format(path) if path else ""), shell=True,
                                                    stdout=sys.stdout, stderr=sys.stdout)
        result = build_configuration_task.wait()
        if result in [1, 100]:
            sys.exit(1)


    def switch_configuration():
        cwd = os.getcwd()
        try:
            new_system_path = os.readlink("{0}/result".format(cwd))
        except FileNotFoundError as e:
            new_system_path = None
            print("Error switching configuration: {0}".format(e))
            sys.exit(1)
        switch_configuration_task = subprocess.Popen("pkexec ${pkgs.nix}/bin/nix-env --profile /nix/var/nix/profiles/system --set {0} && pkexec {1}/bin/switch-to-configuration switch".format(
                                                     new_system_path, new_system_path), shell=True,
                                                     stdout=sys.stdout, stderr=sys.stdout)
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
        "Select and build configuration",
        "Select and build configuration (debug)",
        "Link configuration"
    ] # TODO: add submodules updating

    operation = dmenu.show(operations, prompt='>', lines=10)

    if operation == "Update current configuration":
        decrypt_secrets(machine)
        build_configuration()
        switch_configuration()
        ensure_kernel_update()
    elif operation == "Update current configuration (debug)":
        decrypt_secrets(machine)
        build_configuration(debug=True)
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
  '';
  emacsPackagingSetup = ''
    (use-package nix-mode
      :ensure t
      :mode (("\\.nix$" . nix-mode)
             ((rx (eval "configuration.nix") (zero-or-more anything) eol) . nix-mode)))

    (use-package company-nixos-options
      :ensure t
      :disabled
      :config
      (add-to-list 'company-backends 'company-nixos-options))
  '';
in {
  options = {
    custom.packaging = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable packaging infra.";
      };
      nix.helpers.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Nix helper tools.";
      };
      nix.srcfmt.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Nix source formatting tools.";
      };
      nix.importers.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable tools to convert package definitions to Nix ones.";
      };
      nix.search.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Nix searching helper tools.";
      };
      misc.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable misc packaging tools.";
      };
      scripts.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable custom scripts.";
      };
      homeManagerBackups.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to try backing up existing files in the way of HM symlinks.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable packaging-related Emacs setup.";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.homeManagerBackups.enable) {
      environment.variables.HOME_MANAGER_BACKUP_EXT = "hm_backup";
    })
    (mkIf (cfg.enable && cfg.nix.helpers.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nix-prefetch
          nix-prefetch-github
          nix-prefetch-scripts
          nixos-generators
        ];
      };
    })
    (mkIf (cfg.enable && cfg.nix.srcfmt.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nixfmt
        ];
      };
    })
    (mkIf (cfg.enable && cfg.nix.importers.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nodePackages.node2nix
          pypi2nix
        ];
      };
    })
    (mkIf (cfg.enable && cfg.nix.search.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nix-index # TODO: maybe make easier shell alias
        ];
      };
    })
    (mkIf (cfg.enable && cfg.misc.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          cachix
          dotnet-sdk # for building some binary releases
          nix-zsh-completions
          nix-review # https://github.com/Mic92/nix-review
          make-package-diff
        ] ++ lib.optionals (config.attributes.staging.enable) [
          niv
        ];
      };
    })
    (mkIf (cfg.enable && cfg.scripts.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          get-pr-override
          confctl
        ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          cachix
          dotnet-sdk # for building some binary releases
          nix-zsh-completions
          nix-review # https://github.com/Mic92/nix-review
        ];
        programs.emacs.extraPackages = epkgs: [
          epkgs.company-nixos-options
          epkgs.nix-mode
        ];
      };
      ide.emacs.config = ''${emacsPackagingSetup}'';
    })
  ];
}
