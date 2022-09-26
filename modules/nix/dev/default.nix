{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.ext.nix.dev;
  user = config.attributes.mainUser.name;
in
{
  options = {
    ext.nix.dev = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Nix dev setup.";
      };
      scripts.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable custom scripts.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          nix-prefetch
          nix-prefetch-github
          nix-prefetch-scripts
          nixos-generators
          nix-template
          nix-review # https://github.com/Mic92/nix-review
        ];
      };
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile."espanso/user/nix-dev.yml".text = ''
          name: nix-dev
          parent: default
          filter_title: ".*${config.shell.tmux.defaultSession}.*${config.attributes.machine.name}.*"

          matches:
            - trigger: ":nsd"
              replace: "nix show-derivation 'nixpkgs/nixos-unstable#$|$'"
        '';
      };
    })
    (mkIf (cfg.enable && cfg.scripts.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        get-pr-override = pkgs.writeShellApplication {
          name = "get-pr-override";
          runtimeInputs = with pkgs; [ coreutils curl gnugrep ];
          text = builtins.readFile ./scripts/get-pr-override.sh;
        };
        make-package-diff = pkgs.writeShellApplication {
          name = "make-package-diff";
          runtimeInputs = with pkgs; [ coreutils diffutils nix ];
          text = builtins.readFile ./scripts/make-package-diff.sh;
        };
      };

      home-manager.users."${user}" = { home.packages = with pkgs; [ get-pr-override make-package-diff ]; };
    })
  ];
}
