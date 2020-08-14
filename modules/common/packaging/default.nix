let
  deps = import ../../../nix/sources.nix;
  nixpkgs-pinned-16_04_20 = import deps.nixpkgs-pinned-16_04_20 { config.allowUnfree = true; };
  nixpkgs-proposed = import deps.nixpkgs-proposed { config.allowUnfree = true; };
in { config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let cfg = config.custom.packaging;
in {
  options = {
    custom.packaging = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable packaging infra.";
      };
      configResultPath = mkOption {
        type = types.str;
        default = homePrefix "result";
        description = "Path of the symlink to the build result.";
      };
      nix.helpers.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Nix helper tools.";
      };
      nix.search.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Nix searching helper tools.";
      };
      gc.dates = mkOption {
        type = types.str;
        default = "weekly";
        description = "How often Nix GC should be run.";
      };
      gc.howold = mkOption {
        type = types.str;
        default = "7d";
        description = "How old store content should be to become collected by Nix GC.";
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
      staging.packages = mkOption {
        type = types.listOf types.package;
        default = [ ];
        description = "List of staging packages.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      nix = {
        maxJobs = lib.mkDefault config.attributes.nix.jobs;
        buildCores = lib.mkDefault config.attributes.hardware.cores;
        optimise.automatic = false;
        gc = {
          automatic = true;
          dates = cfg.gc.dates;
          options = "--delete-older-than ${cfg.gc.howold}";
        };
      };
      nixpkgs.config = {
        allowUnfree = true;
        allowUnfreeRedistributable = true;

        oraclejdk.accept_license = true;

        packageOverrides = _: rec {
          get-pr-override = mkShellScriptWithDeps "get-pr-override" (with pkgs; [ coreutils curl gnugrep ])
            (builtins.readFile (pkgs.substituteAll
              ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./scripts/get-pr-override.sh; })));
          make-package-diff = mkShellScriptWithDeps "make-package-diff" (with pkgs; [ coreutils diffutils nix ])
            (builtins.readFile (pkgs.substituteAll
              ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./scripts/make-package-diff.sh; })));
          confctl =
            mkPythonScriptWithDepsAndConflicts "confctl" (with pkgs; [ pyfzf pystdlib python3Packages.python-gnupg ])
            (builtins.readFile (pkgs.substituteAll
              ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./scripts/confctl.py; })));
          rollback = mkShellScriptWithDeps "rollback" (with pkgs; [ fzf ]) ''
            GENERATION=$(pkexec nix-env -p /nix/var/nix/profiles/system --list-generations | fzf --tac)
            GENERATION_PATH=/nix/var/nix/profiles/system-$(echo $GENERATION | cut -d\  -f1)-link
            pkexec nix-env --profile /nix/var/nix/profiles/system --set $GENERATION_PATH && pkexec $GENERATION_PATH/bin/switch-to-configuration switch
          '';
        };
      };
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ niv rollback ]; };
    })
    (mkIf (cfg.enable && cfg.homeManagerBackups.enable) {
      environment.variables.HOME_MANAGER_BACKUP_EXT = "hm_backup";
    })
    (mkIf (cfg.enable && cfg.nix.helpers.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ nix-prefetch nix-prefetch-github nix-prefetch-scripts nixos-generators ];
      };
    })
    (mkIf (cfg.enable && cfg.nix.search.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.zsh.shellAliases = { nlo = "${pkgs.nix-index}/bin/nix-locate --"; };
      };
      systemd.user.services."nix-update-index" = {
        description = "Update nix packages metadata index";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.nix-index}/bin/nix-index";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.user.timers."nix-update-index" = renderTimer "Update nix packages metadata index" "1h" "12h" "";
    })
    (mkIf (cfg.enable && cfg.misc.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs;
          [
            nixpkgs-pinned-16_04_20.cachix
            # nix-zsh-completions # NOTE: collision emerged in last unstable
            nix-review # https://github.com/Mic92/nix-review
            make-package-diff
          ] ++ lib.optionals (cfg.staging.packages != [ ]) cfg.staging.packages;
      };
    })
    (mkIf (cfg.enable && cfg.scripts.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ confctl get-pr-override ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.extraPackages = epkgs: [ epkgs.company-nixos-options epkgs.nix-mode ];
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./emacs/packaging.el; }));
    })
  ];
}
