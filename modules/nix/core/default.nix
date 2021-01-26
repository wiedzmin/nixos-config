{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ext.nix.core;
  user = config.attributes.mainUser.name;
  stable = import inputs.stable ({
    config = config.nixpkgs.config // { allowUnfree = true; };
    localSystem = { system = "x86_64-linux"; };
  });
in {
  options = {
    ext.nix.core = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable core nix setup";
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
      permittedInsecurePackages = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Insecure packages exceptions list";
      };
      shell.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell completions";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Nix-related Emacs setup";
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
        binaryCaches = [ "https://cache.nixos.org" ];
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

        trustedUsers = [ "root" user ];
      };

      environment.etc.nixpkgs.source = inputs.unstable;
      nixpkgs.config = {
        allowUnfree = true;
        allowUnfreeRedistributable = true;

        oraclejdk.accept_license = true;

        permittedInsecurePackages = cfg.permittedInsecurePackages;

        packageOverrides = _: rec {
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
      home-manager.users.${user} = { home.packages = with pkgs; [ nix-doc-lookup rollback ]; };

      systemd.services.nix-daemon = {
        environment.TMPDIR = "/tmp/buildroot";
        preStart = ''
          mkdir -p /tmp/buildroot
        '';
      };

      dev.misc.timeTracking.extensions.dev = { "nix" = "coding:nix"; };
      pim.timetracking.rules = ''
        current window $title =~ /nixos-rebuild/ ==> tag packaging:nixos-rebuild,

        current window ($program == "emacs" && $title =~ m!(?:/etc)/nixos/!) ==> tag project:nixos-config,
      '';
    })
    (mkIf (cfg.enable && cfg.shell.enable) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ stable.nix-zsh-completions ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.company-nixos-options epkgs.nix-mode ];
      ide.emacs.core.config = readSubstituted ../../subst.nix ./emacs/nix.el;
    })
  ];
}
