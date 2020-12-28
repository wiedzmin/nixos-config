{ config, inputs, lib, pkgs, ... }:
with import ../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.custom.packaging;
  user = config.attributes.mainUser.name;
  stable = import inputs.stable ({
    config = config.nixpkgs.config // { allowUnfree = true; };
    localSystem = { system = "x86_64-linux"; };
  });
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
      cachix.configuration = mkOption {
        type = types.str;
        default = "";
        description = "Cachix configuration data.";
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
        package = inputs.unstable.legacyPackages.x86_64-linux.nixUnstable;
        nixPath = lib.mkForce [ "nixpkgs=/etc/nixpkgs" ];
        useSandbox = true;
        readOnlyStore = true;
        requireSignedBinaryCaches = true;
        binaryCachePublicKeys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
        binaryCaches = [ "https://cache.nixos.org" "https://${user}.cachix.org" ];
        extraOptions = ''
          auto-optimise-store = true
          keep-outputs = true
          keep-derivations = true
          http-connections = 10
          experimental-features = nix-command flakes
        '';
        registry = {
          config.flake = inputs.self;
          emacs.flake = inputs.emacs;
          unstable.flake = inputs.unstable;
          stable.flake = inputs.stable;
        };

        maxJobs = lib.mkDefault config.attributes.nix.jobs;
        buildCores = lib.mkDefault config.attributes.hardware.cores;
        optimise.automatic = false;
        gc = {
          automatic = true;
          dates = cfg.gc.dates;
          options = "--delete-older-than ${cfg.gc.howold}";
        };
      };

      environment.etc.nixpkgs.source = inputs.unstable;
      nixpkgs.config = {
        allowUnfree = true;
        allowUnfreeRedistributable = true;

        oraclejdk.accept_license = true;

        permittedInsecurePackages = [ "openssl-1.0.2u" ];

        packageOverrides = _: rec {
          get-pr-override = mkShellScriptWithDeps "get-pr-override" (with pkgs; [ coreutils curl gnugrep ])
            (readSubstituted ../subst.nix ./scripts/get-pr-override.sh);
          make-package-diff = mkShellScriptWithDeps "make-package-diff" (with pkgs; [ coreutils diffutils nix ])
            (readSubstituted ../subst.nix ./scripts/make-package-diff.sh);
          rollback = mkShellScriptWithDeps "rollback" (with pkgs; [ fzf ]) ''
            GENERATION=$(pkexec nix-env -p /nix/var/nix/profiles/system --list-generations | fzf --tac)
            GENERATION_PATH=/nix/var/nix/profiles/system-$(echo $GENERATION | cut -d\  -f1)-link
            pkexec nix-env --profile /nix/var/nix/profiles/system --set $GENERATION_PATH && pkexec $GENERATION_PATH/bin/switch-to-configuration switch
          '';
          nix-doc-lookup = mkShellScriptWithDeps "nix-doc-lookup" (with pkgs; [ fzf gnused manix ]) ''
            manix "" | grep '^# ' | sed 's/^# \(.*\) (.*/\1/;s/ (.*//;s/^# //' | fzf --preview="manix '{}'" | xargs manix
          '';
        };
      };
      home-manager.users.${user} = { home.packages = with pkgs; [ rollback ]; };

      # TODO: add script + automate list of packages, that should be cached, examples: below
      # cachix push ${user} $(dirname $(dirname $(readlink -f $(which i3lock-color))))
      # cachix push ${user} $(dirname $(dirname $(readlink -f $(which emacs))))

      dev.misc.timeTracking.extensions.dev = { "nix" = "coding:nix"; };
      custom.pim.timeTracking.rules = ''
        current window $title =~ /nixos-rebuild/ ==> tag packaging:nixos-rebuild,

        current window ($program == "emacs" && $title =~ m!(?:/etc)/nixos/!) ==> tag project:nixos-config,
      '';

      systemd.services.nix-daemon = {
        environment.TMPDIR = "/tmp/buildroot";
        preStart = ''
          mkdir -p /tmp/buildroot
        '';
      };
    })
    (mkIf (cfg.enable && cfg.homeManagerBackups.enable) {
      environment.variables.HOME_MANAGER_BACKUP_EXT = "hm_backup";
    })
    (mkIf (cfg.enable && cfg.nix.helpers.enable) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [
          nix-prefetch
          nix-prefetch-github
          nix-prefetch-scripts
          nixos-generators
          nix-template
        ];
      };
    })
    (mkIf (cfg.enable && cfg.nix.search.enable) {
      home-manager.users.${user} = { programs.zsh.shellAliases = { nlo = "${pkgs.nix-index}/bin/nix-locate --"; }; };
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
      home-manager.users.${user} = {
        home.packages = with pkgs;
          [
            stable.cachix
            # nix-zsh-completions # NOTE: collision emerged in last unstable
            nix-review # https://github.com/Mic92/nix-review
            make-package-diff
            nix-doc-lookup
          ] ++ lib.optionals (cfg.staging.packages != [ ]) cfg.staging.packages;
        xdg.configFile."cachix/cachix.dhall".text =
          lib.optionalString (cfg.cachix.configuration != "") cfg.cachix.configuration;
      };
    })
    (mkIf (cfg.enable && cfg.scripts.enable) {
      home-manager.users.${user} = { home.packages = with pkgs; [ get-pr-override ]; };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.company-nixos-options epkgs.nix-mode ];
      ide.emacs.core.config = readSubstituted ../subst.nix ./emacs/packaging.el;
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = { home.packages = with pkgs; [ get-pr-override make-package-diff rollback ]; };
    })
  ];
}
