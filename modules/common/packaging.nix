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
  update_pkgs_status = writePythonScriptWithPythonPackages "update_pkgs_status" [
    pkgs.python3Packages.GitPython
    pkgs.python3Packages.redis
  ] ''
    import os
    import subprocess
    import sys

    from git import Repo
    from git.cmd import Git
    import redis

    if os.system("/run/wrappers/bin/ping -c 1 github.com") != 0:
        sys.exit(0)

    r = redis.Redis(host='localhost', port=6379, db=0)

    repos_meta = [
        {
            "name": "home-manager",
            "repo_path": "${config.attributes.paths.home-manager}",
            "branch": "${config.attributes.branches.home-manager}"
        },
        {
            "name": "nixpkgs-channels",
            "repo_path": "${config.attributes.paths.nixpkgs.local}",
            "branch": "${config.attributes.branches.nixpkgs}"
        },
    ]

    current_system_hash = os.readlink("/run/current-system").split(".")[-1]

    for meta in repos_meta:
        repo_path = meta["repo_path"]
        branch = meta["branch"]

        repo = Repo(repo_path)
        origin = repo.remotes.origin

        subprocess.run("cd {0} && ${pkgs.gitAndTools.stgit}/bin/stg pop -a".format(
            repo_path
        ).split(), shell=True)

        try:
            local_head = repo.heads[branch].object.hexsha
            r.set("version/{0}/local".format(meta["name"]), local_head)
            remote_head = origin.refs[branch].object.hexsha
            r.set("version/{0}/remote".format(meta["name"]), remote_head)

            origin.fetch()

            if local_head != remote_head:
                r.set("version/{0}/updatable".format(meta["name"]), "yes")
                g = Git(repo_path)
                new_commits = g.log("{0}..{1}".format(local_head, remote_head), pretty="oneline")
                r.set("version/{0}/updates_log".format(meta["name"]), new_commits)
            elif not local_head.startswith(current_system_hash):
                r.set("version/{0}/updatable".format(meta["name"]), "yes")
            else:
                r.set("version/{0}/updatable".format(meta["name"]), "no")
        finally:
            subprocess.run("cd {0} && ${pkgs.gitAndTools.stgit}/bin/stg push -a".format(
                repo_path
            ).split(), shell=True)
  '';
  show_home_manager_status = writePythonScriptWithPythonPackages "show_home_manager_status" [
    pkgs.python3Packages.libtmux
    pkgs.python3Packages.notify2
    pkgs.python3Packages.redis
  ] ''
    import libtmux
    import notify2
    import redis

    from notify2 import URGENCY_NORMAL, URGENCY_CRITICAL

    r = redis.Redis(host='localhost', port=6379, db=0)
    notify2.init("show_home_manager_status")

    if r.get("version/home-manager/updatable").decode() == "yes":
        updates_log = r.get("version/home-manager/updates_log")
        with open("/tmp/home_manager_updates", "w") as f:
            f.write(updates_log.decode())
        n = notify2.Notification("[home-manager]", "new commits!")
        n.set_urgency(URGENCY_NORMAL)
        n.set_timeout(15000)
        n.show()
        tmux_server = libtmux.Server()
        tmux_session = tmux_server.find_where({ "session_name": "${config.attributes.tmux.defaultSession}" })
        commits_window = tmux_session.new_window(attach=True, window_name="commits")
        commits_pane = commits_window.attached_pane
        commits_pane.send_keys("cd ${config.attributes.paths.home-manager}")
        commits_pane.send_keys("cat /tmp/home_manager_updates")
    else:
        n = notify2.Notification("[home-manager] nothing new", "Come again later ;)")
        n.set_urgency(URGENCY_NORMAL)
        n.set_timeout(15000)
        n.show()
  '';
  show_nixpkgs_status = writePythonScriptWithPythonPackages "show_nixpkgs_status" [
    pkgs.python3Packages.libtmux
    pkgs.python3Packages.notify2
    pkgs.python3Packages.redis
  ] ''
    import libtmux
    import notify2
    import redis

    from notify2 import URGENCY_NORMAL, URGENCY_CRITICAL

    r = redis.Redis(host='localhost', port=6379, db=0)
    notify2.init("show_nixpkgs_status")

    if r.get("version/nixpkgs-channels/updatable").decode() == "yes":
        updates_log = r.get("version/nixpkgs-channels/updates_log")
        new_packages = [ " ".join(entry.split()[1:]) for entry in updates_log.decode().split("\n")
                         if "init" in entry and "Merge" not in entry and "pythonPackages" not in entry and
                         "python3Packages" not in entry and "python37Packages" not in entry and "ocamlPackages" not in entry]
        with open("/tmp/nixpkgs_updates_new", "w") as f:
            f.write("\n".join(new_packages))
        package_updates = [ " ".join(entry.split()[1:]) for entry in updates_log.decode().split("\n")
                         if "init" not in entry and "Merge" not in entry]
        with open("/tmp/nixpkgs_updates_installed", "w") as f:
            f.write("\n".join(package_updates))
        n = notify2.Notification("[nixpkgs]", "Updated!")
        n.set_urgency(URGENCY_CRITICAL)
        n.set_timeout(15000)
        n.show()
        tmux_server = libtmux.Server()
        tmux_session = tmux_server.find_where({ "session_name": "${config.attributes.tmux.defaultSession}" })
        new_packages_window = tmux_session.new_window(attach=True, window_name="new packages")
        new_packages_pane = new_packages_window.attached_pane
        new_packages_pane.send_keys("cd ${config.attributes.paths.nixpkgs.local}")
        new_packages_pane.send_keys("cat /tmp/nixpkgs_updates_new")
        updated_packages_window = tmux_session.new_window(attach=False, window_name="updated packages")
        updated_packages_pane = updated_packages_window.attached_pane
        updated_packages_pane.send_keys("cd ${config.attributes.paths.nixpkgs.local}")
        updated_packages_pane.send_keys("cat /tmp/nixpkgs_updates_installed")
    else:
        n = notify2.Notification("[nixpkgs] nothing new", "Come again later ;)")
        n.set_urgency(URGENCY_NORMAL)
        n.set_timeout(15000)
        n.show()
  '';
  confctl = writePythonScriptWithPythonPackages "confctl" [  # TODO: consider making relative config paths
    pkgs.python3Packages.GitPython
    pkgs.python3Packages.libtmux
    pkgs.python3Packages.notify2
    pkgs.python3Packages.dmenu-python
    pkgs.python3Packages.python-gnupg
    pkgs.python3Packages.redis
  ] ''
    import glob
    import os
    import subprocess
    import sys

    from git import Repo
    from git.cmd import Git
    from git.exc import InvalidGitRepositoryError
    from git.refs.tag import TagReference
    from gnupg import GPG
    import dmenu


    def guess_machine_name():
        return os.readlink("/etc/nixos/configuration.nix").split("/")[1]


    def locate_nixpkgs():
        locate_nixpkgs_task = subprocess.Popen("${pkgs.nix}/bin/nix-instantiate --find-file nixpkgs",
                                               shell=True, stdout=subprocess.PIPE)
        nixpkgs_path = locate_nixpkgs_task.stdout.read().decode().strip()
        assert locate_nixpkgs_task.wait() == 0
        return nixpkgs_path


    def decrypt_secrets(machine):
        secrets_dir = "/etc/nixos/machines/{0}/secrets".format(machine)
        secrets = glob.glob('{0}/*.gpg'.format(secrets_dir))
        gpg = GPG()
        for secret in secrets:
            secret_decrypted = os.path.splitext(secret)[0]
            with open(secret, "rb") as src:
                with open(secret_decrypted, "w") as dst:
                      dst.write(str(gpg.decrypt_file(src)))


    def update_nixpkgs_suffix():
        nixpkgs_path = locate_nixpkgs()
        current_rev = os.readlink("/run/current-system").split(".")[-1]
        new_rev = None
        try:
            nixpkgs_git = Git(nixpkgs_path)
            new_rev = nixpkgs_git.rev_parse("--short", "HEAD")
            if "dirty" in nixpkgs_git.describe("--always", "--dirty"):
                new_rev += "M"
        except InvalidGitRepositoryError:
            pass
        if new_rev:
            with open("{0}/.version-suffix".format(nixpkgs_path), "w") as suffix:
                suffix.write(".git.{0}".format(new_rev))
        if current_rev != new_rev:
            print("Updating Nixpkgs suffix: {0} --> {1}".format(current_rev, new_rev))


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


    # unfinished
    def manage_current_ref_tags(repo_path):
        repo = Repo(repo_path)
        tagrefs = TagReference.list_items(repo)
        active_head = repo.heads[0]

        operations = [
            "tag active HEAD",
            "remove tags",
            "checkout tag",
            "checkout HEAD",
        ] # TODO: think of some kind of submodule reset semantics (e.g. like `git submodule update --init`)

        tag_choices = [
            "current time",
            "custom"
        ]

        operation = dmenu.show(operations, prompt='>', lines=5)
        if not operation:
            sys.exit(1)
        if operation == "tag active HEAD":
            tag_type = dmenu.show(tag_choices, prompt='tag:', lines=5)
            if not operation:
                sys.exit(1)
        elif operation == "remove tags":
            pass
        elif operation == "checkout tag":
            pass
        elif operation == "checkout HEAD":
            pass


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
        update_nixpkgs_suffix()
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
          confctl
          get-pr-override
        ];
      };
      system.activationScripts.ensureFilesPermissions = mkBefore ''
        /run/wrappers/bin/sudo chmod -R a+rw /etc/nixos/.git/modules/pkgs/forges/github.com/NixOS/nixpkgs-channels/
        /run/wrappers/bin/sudo chmod -R a+rw /etc/nixos/.git/modules/pkgs/forges/github.com/NixOS/nixos-hardware/
        /run/wrappers/bin/sudo chmod -R a+rw /etc/nixos/.git/modules/pkgs/forges/github.com/rycee/home-manager/
        /run/wrappers/bin/sudo chmod a+rw /etc/nixos/pkgs/forges/github.com/NixOS/nixpkgs-channels/.version-suffix
      '';
      systemd.services.redis.postStart = ''
        ${update_pkgs_status}/bin/update_pkgs_status
      '';
      systemd.timers."update_pkgs_status" = {
        description = "Update nixpkgs/home-manager versions status";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnBootSec = "5min";
          OnUnitActiveSec = "1h";
        };
      };
      systemd.services."update_pkgs_status" = {
        description = "Update nixpkgs/home-manager versions status";
        serviceConfig = {
          Type = "oneshot";
          Environment = [
            "HOME=/home/${config.attributes.mainUser.name}"
          ];
          ExecStart = "${update_pkgs_status}/bin/update_pkgs_status";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
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
    (mkIf (cfg.enable && cfg.xmonad.enable) {
      wm.xmonad.keybindings = {
        "M-S-m" = ''spawn "${show_home_manager_status}/bin/show_home_manager_status"'';
        "M-S-n" = ''spawn "${show_nixpkgs_status}/bin/show_nixpkgs_status"'';
      };
    })
  ];
}
