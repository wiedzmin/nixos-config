{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.packaging;
  bootstrap_custom_config = pkgs.writeShellScriptBin "bootstrap_custom_config" ''
    ROOT_PARTITION_LABEL=nixos-root
    BOOT_PARTITION_LABEL=nixos-boot
    MACHINE=''${1:-laptoptop}
    USERNAME=''${2:-alex3rd}
    PRIVATE_STORAGE_HOST=''${3:-localhost}
    PRIVATE_STORAGE_HOST_PORT=''${4:-8080}

    echo "Mounting root partition"
    mount /dev/disk/by-label/$ROOT_PARTITION_LABEL /mnt
    echo "creating /boot"
    mkdir -p /mnt/boot
    echo "Mounting boot partition"
    mount /dev/disk/by-label/$BOOT_PARTITION_LABEL /mnt/boot

    echo "Installing essential tools"
    nix-env -iA nixos.pkgs.gitAndTools.gitFull
    nix-env -iA nixos.wget

    # TODO: handle secrets, see partial example below
    # gpg -dq secret.nix.gpg > secret.nix

    echo "removing existing configuration tree, if any"
    rm -rf /mnt/etc/nixos

    git clone https://github.com/wiedzmin/nixos-config nixos

    # prepare config (fetch home-manager + nixpkgs)
    echo "preparing submodules"
    cd /mnt/etc/nixos && git submodule init && git submodule update
    echo "fetching private user data"
    # TODO: make more declarative/self-contained
    # TODO: harden in terms of security
    cd "/mnt/etc/nixos/users/$USERNAME" && wget "http://$PRIVATE_STORAGE_HOST:$PRIVATE_STORAGE_HOST_PORT/''${USERNAME}_private.tar.gz"
    if [ $? -ne 0 ]; then
        echo "Error fetching private data, check manually"
        exit 1
    fi

    echo "symlinking configuration root"
    cd /mnt/etc/nixos && ln -rsvf "machines/$MACHINE.nix" ./configuration.nix

    echo "actually installing"
    # home-manager and nixpkgs paths are either absent in NIX_PATH or point to wrong locations
    nixos-install --root /mnt -I home-manager=/mnt/etc/nixos/pkgs/home-manager-proposed -I nixpkgs=/mnt/etc/nixos/pkgs/nixpkgs-channels
  '';
  build_iso = pkgs.writeShellScriptBin "build_iso" ''
    ${pkgs.nix}/bin/nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=/etc/nixos/contrib/iso/iso.nix
  '';
  burn_iso = pkgs.writeShellScriptBin "burn_iso" ''
    pause () {
        echo
        read -n 1 -s -r -p "Press any key to continue..."
        echo
    }

    RESULT_SYMLINK=''${1:-/etc/nixos/result}
    if [ ! -f "$RESULT_SYMLINK" ]; then
        echo "Cannot find symlink to recently built system, exiting"
        exit 1
    fi
    ISO_DIRNAME=$(readlink $RESULT_SYMLINK)/iso
    ISO_BASENAME=$(ls $ISO_DIRNAME)

    ISO_DEVICE=''${2:-/dev/sdb}
    if [ ! -b "$ISO_DEVICE" ]; then
        echo "USB drive NOT found, exiting"
        exit 1
    else
        echo "Going to burn $ISO_DIRNAME/$ISO_BASENAME to $ISO_DEVICE"
        pause
        sudo dd bs=4M if="$ISO_DIRNAME/$ISO_BASENAME" of="$ISO_DEVICE"
        # TODO: investigate unmounting/data flush issues
        echo "Burned successfully, unmounting pending"
        pause
        sudo umount "$ISO_DEVICE"
    fi
  '';
  pkgsctl = pkgs.writeShellScriptBin "pkgsctl" ''
    #! /usr/bin/env nix-shell
    #! nix-shell -i python3 -p python3 python3Packages.dmenu-python python3Packages.GitPython
    import os
    import re
    import subprocess
    import datetime

    import dmenu
    from git import Repo

    package_name_regexp = re.compile(
        r"(?:/nix/store/[0-9a-z]{32}-)"                 # prefix with hash - always present
        r"(?:python|perl|node_)?(?:[0-9\.\-]*)?"        # package subsets by languages (with version) - optional
        r"([0-9{1}A-Za-z\.\_\-]*)"                      # package name itself
    )
    package_version_start_regexp = re.compile(r"-v?[0-9]|-unstable|-with-packages|-with-plugins|-git")
    current_system_regexp = re.compile(r"git\.([0-9abcdef]+)M?\$?")

    nixpkgs_path = "/etc/nixos/pkgs/forges/github.com/NixOS/nixpkgs-channels/"
    nixpkgs_branch = "nixos-unstable"
    nixpkgs_fallback_branch = "nixos-unstable-working"
    nixpkgs_upstream_remote = "upstream"

    nixpkgs_proposed_path = "/etc/nixos/pkgs/forges/github.com/wiedzmin/nixpkgs/"
    nixpkgs_proposed_branch = "master"


    def get_installed_packages(home_manager=True,nixpkgs=True):
        installed_paths = []
        result = {}
        if nixpkgs:
            nixpkgs_list_task = subprocess.Popen("nix-store -q --references /run/current-system/sw | grep -v nixos",
                                             shell=True, stdout=subprocess.PIPE)
            nixpkgs_paths = nixpkgs_list_task.stdout.read().decode().split("\n")[:-1]
            assert nixpkgs_list_task.wait() == 0
            installed_paths.extend(nixpkgs_paths)
        if home_manager:
            home_manager_list_task = subprocess.Popen('nix-store -q --references "$(nix-store -q --references '
                                                      '/home/{0}/.nix-profile | grep home-manager-path)"'.format(
                                                          os.environ["USER"]), shell=True, stdout=subprocess.PIPE)
            home_manager_paths = home_manager_list_task.stdout.read().decode().split("\n")[:-1]
            assert home_manager_list_task.wait() == 0
            installed_paths.extend(home_manager_paths)
        for path in installed_paths:
            try:
                pn_match = package_name_regexp.search(path)
                package_name_postfix = pn_match.group(1)
                pvs_match = package_version_start_regexp.search(package_name_postfix)
                package_name = package_name_postfix[:pvs_match.span()[0]]
                result[package_name] = True
            except AttributeError:
                continue
        return [k for k in result]


    def update_nixpkgs():
        branch = "nixos-unstable"
        fallback_branch = "nixos-unstable-working"

        current_system_path = os.readlink("/run/current-system")
        system_git_version = current_system_regexp.search(current_system_path).group(1)

        nixpkgs_repo = Repo(nixpkgs_path)
        nixpkgs_origin = nixpkgs_repo.remotes.origin
        local_head_ref = nixpkgs_repo.heads[nixpkgs_branch].object
        local_head = local_head_ref.hexsha
        remote_head = nixpkgs_origin.refs[nixpkgs_branch].object.hexsha
        try:
            fallback_head = nixpkgs_repo.heads[nixpkgs_fallback_branch].object.hexsha
        except IndexError:
            fallback_head = None

        should_rebase = local_head != remote_head
        should_rebuild = not local_head.startswith(system_git_version)
        should_force_fallback = not fallback_head or fallback_head and not fallback_head.startswith(system_git_version)

        fetch_results = list(nixpkgs_origin.fetch())
        if should_rebase:
            for fetch_info in nixpkgs_origin.fetch():
                print("Updated %s to %s" % (fetch_info.ref, fetch_info.commit))

        try:
            if should_rebase:
                while True:
                    reply = str(input("Update local '{0}'? (y/n): ".format(nixpkgs_branch))).lower().strip()
                    if reply[0] == 'y':
                        nixpkgs_repo.git.rebase("origin", nixpkgs_branch)
                        print("Updated '{0}': {1} --> {2}".format(nixpkgs_branch, local_head, remote_head))
                        head_ts = datetime.datetime.utcfromtimestamp(local_head_ref.authored_date).isoformat()
                        checkpoint = nixpkgs_repo.create_tag("v_{0}".format(local_head_ref.authored_date),
                                                             ref=local_head_ref,
                                                             message="checkpoint at {0}".format(head_ts))
                        break
                    if reply[0] == 'n':
                        break
            if should_force_fallback:
                fallback_branch_ref = nixpkgs_repo.create_head(nixpkgs_fallback_branch,
                                                               commit=system_git_version,
                                                               force=True)
                last_working = nixpkgs_repo.create_tag("last_working", ref=system_git_version,
                                                       force=True,
                                                       message="last nixpkgs built and working")
                print("Updated '{0}': {1} --> {2}".format(nixpkgs_fallback_branch, fallback_head, system_git_version))
        except KeyboardInterrupt: # TODO: add types
            # TODO: rollback
            pass
        if should_rebuild:
            print("Nixpkgs updated, consider rebuilding!")


    def update_nixpkgs_proposed():
        nixpkgs_proposed_repo = Repo(nixpkgs_proposed_path)
        nixpkgs_proposed_origin = nixpkgs_proposed_repo.remotes.origin

        # check if fetch remote present
        # if not present use dmenu to input and add it


        # cd /etc/nixos/pkgs/nixpkgs-proposed
        # remote_fetch_meta=$(git remote -v | grep fetch)
        # has_upstream=$(echo $remote_fetch_meta | grep $upstream_remote_name)
        # if [ -z $has_upstream ]; then
        #     echo "No $upstream_remote_name remote found."
        #     echo "Use 'git remote add $upstream_remote_name <upstream url>' to add it."
        #     exit 1
        # fi
        # git fetch $upstream_remote_name
        # git merge $upstream_remote_name/$branch


    def update_home_manager():
        # branch=master
        # branch_hash=$(git rev-parse --short $branch)
        # current_hm_hash=$(cat /etc/current-home-manager)

        # cd /etc/nixos/pkgs/home-manager
        # git tag -a -s --force last_working -m "last home-manager built and working" $current_hm_hash
        # git fetch origin
        # git rebase origin/$branch
        # branch_hash=$(git rev-parse --short $branch)
        # if [ "$branch_hash" != "$current_hm_hash" ]; then
        #     head_ts=$(git show -s --format=%ct HEAD)
        #     git tag -a -s --force "v_$head_ts" -m "checkpoint at $(LC_ALL=C date -d @$head_ts)"
        # fi
        # exit 0
        # ;;
        pass

    def update_home_manager_proposed():
        # upstream_remote_name=upstream
        # branch=master

        # cd /etc/nixos/pkgs/home-manager-proposed
        # remote_fetch_meta=$(git remote -v | grep fetch)
        # has_upstream=$(echo $remote_fetch_meta | grep $upstream_remote_name)
        # if [ -z $has_upstream ]; then
        #     echo "No $upstream_remote_name remote found."
        #     echo "Use 'git remote add $upstream_remote_name <upstream url>' to add it."
        #     exit 1
        # fi
        # git fetch $upstream_remote_name
        # git merge $upstream_remote_name/$branch

        # exit 0
        # ;;
        pass

    def list_installed_packages_updates():
        # TODO: remove duplication
        installed_packages = get_installed_packages()
        nixpkgs_repo = Repo(nixpkgs_path)
        local_head = nixpkgs_repo.heads[nixpkgs_branch].object.hexsha
        fallback_head = nixpkgs_repo.heads[nixpkgs_fallback_branch].object.hexsha
        git_log = nixpkgs_repo.git.log("--pretty=oneline","{0}...{1}".format(local_head, fallback_head))

        for entry in git_log.split("\n"):
            for package in installed_packages:
                if package in entry and not "Merge" in entry and not "init" in entry:
                    print(entry)
                    break


    def list_new_packages():
        # TODO: remove duplication
        nixpkgs_repo = Repo(nixpkgs_path)
        local_head = nixpkgs_repo.heads[nixpkgs_branch].object.hexsha
        fallback_head = nixpkgs_repo.heads[nixpkgs_fallback_branch].object.hexsha
        git_log = nixpkgs_repo.git.log("--pretty=oneline","{0}...{1}".format(local_head, fallback_head))
        for entry in git_log.split("\n"):
            if "init" in entry and not "Merge" in entry:
                print(entry)


    def review_home_manager_updates():
        # cd /etc/nixos/pkgs/home-manager
        # if [ "$branch_hash" = "$current_hm_hash" ]; then
        #     echo "No fresh updates, try again a bit later"
        #     exit 0
        # fi
        # echo "showing commits $branch_hash...$current_hm_hash"
        # git log --pretty=oneline $branch_hash...$current_hm_hash | grep -v Merge | fzf --reverse | xargs git show
        # exit 0
        # ;;
        pass

    def show_nixpkgs_git_tags():
        # git tag | fzf --reverse | xargs git show
        # exit 0
        # ;;
        pass

    def show_home_manager_git_tags():
        # cd /etc/nixos/pkgs/home-manager
        # git tag | fzf --reverse | xargs git show
        # exit 0
        # ;;
        pass

    operations = {
        "update nixpkgs": update_nixpkgs,
        "update nixpkgs-proposed": update_nixpkgs_proposed,
        "updates to installed packages": list_installed_packages_updates,
        "new packages": list_new_packages,
        "home-manager git tags": "huy"
    }


    if __name__ == "__main__":
        result = dmenu.show(operations.keys(), prompt='perform:', lines=10)
        if result:
            operations[result]()

    # TODO: git submodule update --init (or some kind of versions pinning)
  '';
  format-config = pkgs.writeShellScriptBin "format-config" ''
    sources=$(${pkgs.fd}/bin/fd -t file nix -E forges -E "*.gpg*" /etc/nixos)
    for file in "$sources"; do
      ${pkgs.nixfmt}/bin/nixfmt -w 120 $file
    done
  '';
  current_system_hash = pkgs.writeShellScriptBin "current_system_hash" ''
    current_system_commit_hash=`${pkgs.coreutils}/bin/readlink -f /run/current-system | ${pkgs.coreutils}/bin/cut -f4 -d.`
    cd ${config.attributes.paths.nixpkgs}
    nixpkgs_current_branch=$(${pkgs.git}/bin/git symbolic-ref --short HEAD)
    cd ${config.attributes.paths.home-manager}
    hm_current_branch=$(${pkgs.git}/bin/git symbolic-ref --short HEAD)
    hm_current_hash=$(${pkgs.git}/bin/git rev-parse --short HEAD)
    ${pkgs.dunst}/bin/dunstify -t 15000 "nixpkgs: $current_system_commit_hash/$nixpkgs_current_branch
    HM: $hm_current_hash/$hm_current_branch"
  '';
  update-system = pkgs.writeShellScriptBin "update-system" ''
    nixpkgs=$(${pkgs.nix}/bin/nix-instantiate --find-file nixpkgs)
    if [[ $? -ne 0 ]]; then
        echo "Could not find nixpkgs"
        exit 1
    fi

    decrypt_secrets() {
        echo "Updating secrets"
        SECRETS_DIR="/etc/nixos/users/$USER/secrets"
        for secret in "$SECRETS_DIR"/*.gpg
        do
            SECRET_NAME=$(basename "$secret")
            DECRYPTED_NAME="''${SECRET_NAME%.*}"
            ${pkgs.gnupg}/bin/gpg -dq "$SECRETS_DIR/$SECRET_NAME" > "$SECRETS_DIR/$DECRYPTED_NAME"
            echo "$SECRETS_DIR/$DECRYPTED_NAME"
        done
    }

    build_configuration() {
        ${pkgs.nix}/bin/nix build -f $nixpkgs/nixos system $@
        result=$?
        if [[ $result == 1 ]] || [[ $result == 100 ]]
        then
            exit 1
        fi
    }

    switch_configuration() {
        dir=$(pwd)
        pkexec ${pkgs.nix}/bin/nix-env --profile /nix/var/nix/profiles/system --set $(readlink $dir/result)
        pkexec $dir/result/bin/switch-to-configuration switch
    }

    update_nixpkgs_suffix() {
        current_rev=$(${pkgs.coreutils}/bin/readlink -f /run/current-system | ${pkgs.coreutils}/bin/cut -f4 -d.)
        rev=
        if [ -e "$nixpkgs/.git" ]; then
            cd $nixpkgs
            rev=$(${pkgs.git}/bin/git rev-parse --short HEAD)
            if ${pkgs.git}/bin/git describe --always --dirty | grep -q dirty; then
                rev+=M
            fi
        fi
        if [ -n "$rev" ]; then
            suffix=".git.$rev"
            pkexec ${pkgs.bash}/bin/bash -c "echo -n $suffix > $nixpkgs/.version-suffix" || true
        fi
        if ! [[ $current_rev =~ $rev ]]
        then
            echo "Updating Nixpkgs suffix: $current_rev --> $rev"
        fi
    }

    ensure_kernel_update() {
        current=$(readlink -f /run/current-system/kernel)
        booted=$(readlink -f /run/booted-system/kernel)

        if [ "$current" != "$booted" ]; then
            read -p "Kernel changed, reboot? " -n 1 -r
            if [[ $REPLY =~ ^[Yy]$ ]]
            then
                echo "Rebooting in 5 sec..."
                sleep 5
                echo "kernel changed, reboot" | ${pkgs.systemd}/bin/systemd-cat --identifier "post-upgrade-check";
                reboot
            fi
        else
            echo "same kernel, do not reboot" | ${pkgs.systemd}/bin/systemd-cat --identifier "post-upgrade-check";
        fi
    }

    update_nixpkgs_suffix
    build_configuration
    switch_configuration
    ensure_kernel_update
  '';
  gen-nix-du = pkgs.writeShellScriptBin "gen-nix-du" ''
    set -eu

    # TODO: think of moving these params to module level
    nixDuBasedir=/tmp
    nixDuFilename=nix-du
    nixDuFileFormat=svg
    nixDuSizeThreshold=500MB

    ${pkgs.nix-du}/bin/nix-du -s $nixDuSizeThreshold | \
    ${pkgs.graphviz}/bin/dot -T$nixDuFileFormat > $nixDuBasedir/$nixDuFilename.$nixDuFileFormat
  '';
  confctl = pkgs.writeScriptBin "confctl" ''
    #! /usr/bin/env nix-shell
    #! nix-shell -i python3 -p python3 python3Packages.dmenu-python
    import os
    import dmenu

    MACHINES_CONFIG_PATH = "/etc/nixos/machines"

    config_entries = os.listdir(MACHINES_CONFIG_PATH)
    configs = { k: v for (k, v) in zip([entry[:-4] if entry.endswith("nix") else entry for entry in config_entries], config_entries)}
    result = dmenu.show(configs.keys(), prompt='config', lines=5)
    if result:
        # TODO: migrate to update system after implementing (VM) builds there
        os.system('sudo nixos-rebuild -I nixos-config="{0}/{1}" build'.format(
            MACHINES_CONFIG_PATH, configs[result]))
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
    (mkIf (cfg.enable && cfg.nix.helpers.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nix-prefetch
          nix-prefetch-github
          nix-prefetch-scripts
          nixos-generators
          gen-nix-du
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
          # custom.gen-nix-du
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
        ] ++ lib.optionals (config.attributes.staging.enable) [
          niv
        ];
      };
    })
    (mkIf (cfg.enable && cfg.scripts.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          confctl
          update-system
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
    (mkIf (cfg.enable && cfg.xmonad.enable) {
      wm.xmonad.keybindings = {
        "M-S-h" = ''spawn "${current_system_hash}/bin/current_system_hash"'';
      };
    })
  ];
}


# https://github.com/jwiegley/nix-config/blob/master/bin/nixpkgs-bisect - integrate into pkgsctl workflow
