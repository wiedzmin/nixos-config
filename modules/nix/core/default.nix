{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with config.navigation.bookmarks.workspaces;
with lib;

let
  cfg = config.ext.nix.core;
  user = config.attributes.mainUser.name;
in
{
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
      # TODO: consider adding system-wide option for enabling heavy resources consumers and AND it here and there
      lsp.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable LSP functionality";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nix = {
        settings = {
          cores = lib.mkDefault config.attributes.hardware.cores;
          max-jobs = lib.mkDefault config.attributes.nix.jobs;
          require-sigs = true;
          sandbox = true;
          substituters = [ "https://cache.nixos.org" ];
          trusted-public-keys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
          trusted-users = [ "root" user ];
        };
        package = pkgs.nixUnstable;
        readOnlyStore = true;
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

        optimise.automatic = false;
        gc = {
          automatic = true;
          inherit (cfg.gc) dates;
          options = "--delete-older-than ${cfg.gc.howold}";
        };
      };

      environment.etc.nixpkgs.source = inputs.unstable;
      nixpkgs.config = {
        allowUnfree = true;
        allowUnfreeRedistributable = true;

        oraclejdk.accept_license = true;

        inherit (cfg) permittedInsecurePackages;

        packageOverrides = _: rec {
          # TODO: try https://github.com/lf-/nix-doc
          rollback = pkgs.writeShellApplication {
            name = "rollback";
            runtimeInputs = with pkgs; [ fzf ];
            text = ''
              GENERATION=$(pkexec nix-env -p /nix/var/nix/profiles/system --list-generations | fzf --tac)
              if [ -n "$GENERATION" ]; then
                GENERATION_PATH=/nix/var/nix/profiles/system-$(echo "$GENERATION" | cut -d\  -f1)-link
                pkexec nix-env --profile /nix/var/nix/profiles/system --set "$GENERATION_PATH" && pkexec "$GENERATION_PATH/bin/switch-to-configuration switch"
              fi
            '';
          };
          nix-doc-lookup = pkgs.writeShellApplication {
            name = "nix-doc-lookup";
            runtimeInputs = with pkgs; [ fzf gnused manix ];
            text = ''
              manix "" | grep '^# ' | sed 's/^# \(.*\) (.*/\1/;s/ (.*//;s/^# //' | fzf --preview="manix '{}'" | xargs manix
            '';
          };
        };
      };

      programs.nix-ld.enable = false; # enable on-demand, see https://github.com/Mic92/nix-ld for reference

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ cargo /*for unpackaged Rust tools*/ nix-doc-lookup rollback statix ] ++
          lib.optionals cfg.lsp.enable [ rnix-lsp ];
        home.sessionPath = [ (homePrefix user ".cargo/bin") ];
      };

      systemd.services.nix-daemon = {
        environment.TMPDIR = "/tmp/buildroot";
        preStart = ''
          mkdir -p /tmp/buildroot
        '';
      };

      dev.misc.timeTracking.extensions.dev = { "nix" = "coding:nix"; };
      pim.timetracking.rules = mkArbttProgramTitleRule [ "emacs" ] [ "/nixos-config/" ] "project:nixos-config";
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile."espanso/user/nix-core.yml".text = ''
          name: nix-core
          parent: default
          filter_title: ".*${config.shell.tmux.defaultSession}.*${config.attributes.machine.name}.*"

          matches:
            - trigger: ":nsp"
              replace: "nix shell \"nixpkgs#$|$\""

            - trigger: ":ns2"
              replace: " \"nixpkgs#$|$\""

            - trigger: ":pkgs"
              replace: "inputs.unstable.legacyPackages.x86_64-linux.$|$"

            # TODO: script and reference to it, to collect machines names and select among it (try if it is even possible)
            - trigger: ":cfg"
              replace: "nixosConfigurations.laptoptop.config.$|$"

            - trigger: ":elt"
              replace: "builtins.head (inputs.unstable.lib.sublist 1$|$ 1 nixosConfigurations.laptoptop.config.wmCommon.wsMapping.rules)"

            - trigger: ":nrep"
              replace: "cd ${wsRoot roots "github"}/wiedzmin/nixos-config && nix repl ./flake-repl.nix"

            - trigger: ":nsfd"
              replace: "nix shell \"nixpkgs#fd\" -c fd $|$ /nix/store"
        '';
      };
    })
    (mkIf (cfg.enable && cfg.shell.enable) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ stable.nix-zsh-completions ]; };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.company-nixos-options epkgs.nix-mode ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./emacs/nix.el ];
    })
  ];
}
