let
  deps = import ../../../nix/sources.nix;
  nixpkgs-pinned-05_12_19 = import deps.nixpkgs-pinned-05_12_19 { config.allowUnfree = true; };
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
      # TODO: consider semantically split this option per-tool (e.g. python packaging,
      # golang packaging, etc., with moving to respective modules)
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
      staging.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable staging settings.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      nix = { # TODO: extract options
        maxJobs = lib.mkDefault config.attributes.hardware.cores;
        buildCores = lib.mkDefault config.attributes.hardware.cores;
        optimise.automatic = false;
        gc = {
          automatic = true;
          dates = "weekly";
          options = "--delete-older-than 7d";
        };
      };
      nixpkgs.config = {
        allowUnfree = true;
        allowUnfreeRedistributable = true;

        oraclejdk.accept_license = true;

        packageOverrides = _: rec {
          format-config = pkgs.writeScriptBin "format-config" (builtins.readFile
            (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./format-config.sh; })));
          get-pr-override = pkgs.writeScriptBin "get-pr-override" (builtins.readFile (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./get-pr-override.sh; })));
          make-package-diff = pkgs.writeScriptBin "make-package-diff" (builtins.readFile (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./make-package-diff.sh; })));
          confctl = writePythonScriptWithPythonPackages "confctl" [
            pkgs.python3Packages.dmenu-python
            pkgs.python3Packages.python-gnupg
          ] (builtins.readFile
            (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./confctl.py; })));
        };
      };
    })
    (mkIf (cfg.enable && cfg.homeManagerBackups.enable) {
      environment.variables.HOME_MANAGER_BACKUP_EXT = "hm_backup";
    })
    (mkIf (cfg.enable && cfg.nix.helpers.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ nix-prefetch nix-prefetch-github nix-prefetch-scripts nixos-generators ];
      };
    })
    (mkIf (cfg.enable && cfg.nix.srcfmt.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ nixfmt ]; };
    })
    (mkIf (cfg.enable && cfg.nix.importers.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ nodePackages.node2nix pypi2nix ];
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
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
      };
      systemd.user.timers."nix-update-index" = {
        description = "Update nix packages metadata index";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnBootSec = "1h";
          OnUnitActiveSec = "12h";
        };
      };
    })
    (mkIf (cfg.enable && cfg.misc.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs;
          [
            nixpkgs-pinned-05_12_19.cachix
            dotnet-sdk # for building some binary releases
            nix-zsh-completions
            nix-review # https://github.com/Mic92/nix-review
            make-package-diff
          ] ++ lib.optionals (cfg.staging.enable) [ niv nix-linter ];
      };
    })
    (mkIf (cfg.enable && cfg.scripts.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ get-pr-override confctl format-config ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.emacs.extraPackages = epkgs: [ epkgs.company-nixos-options epkgs.nix-mode ];
      };
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./packaging.el; }));
    })
  ];
}
