{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ext.nix.dev;
  user = config.attributes.mainUser.name;
  stable = import inputs.stable ({
    config = config.nixpkgs.config // { allowUnfree = true; };
    localSystem = { system = "x86_64-linux"; };
  });
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
      };
    })
    (mkIf (cfg.enable && cfg.scripts.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        get-pr-override = mkShellScriptWithDeps "get-pr-override" (with pkgs; [ coreutils curl gnugrep ])
          (readSubstituted ../../subst.nix ./scripts/get-pr-override.sh);
        make-package-diff = mkShellScriptWithDeps "make-package-diff" (with pkgs; [ coreutils diffutils nix ])
          (readSubstituted ../../subst.nix ./scripts/make-package-diff.sh);
      };

      home-manager.users.${user} = { home.packages = with pkgs; [ get-pr-override make-package-diff ]; };
    })
  ];
}
