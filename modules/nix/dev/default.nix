{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ext.nix.dev;
  user = config.attributes.mainUser.name;
in {
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
    (mkIf (cfg.enable) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [
          nix-prefetch
          nix-prefetch-github
          nix-prefetch-scripts
          nixos-generators
          nix-template
          nix-review # https://github.com/Mic92/nix-review
        ];
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
        get-pr-override = mkShellScriptWithDeps "get-pr-override" (with pkgs; [ coreutils curl gnugrep ])
          (builtins.readFile ./scripts/get-pr-override.sh);
        make-package-diff = mkShellScriptWithDeps "make-package-diff" (with pkgs; [ coreutils diffutils nix ])
          (builtins.readFile ./scripts/make-package-diff.sh);
      };

      home-manager.users.${user} = { home.packages = with pkgs; [ get-pr-override make-package-diff ]; };
    })
  ];
}
