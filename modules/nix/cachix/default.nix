{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ext.nix.cachix;
  user = config.attributes.mainUser.name;
  stable = import inputs.stable ({
    config = config.nixpkgs.config // { allowUnfree = true; };
    localSystem = { system = "x86_64-linux"; };
  });
in {
  options = {
    ext.nix.cachix = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Cachix";
      };
      configuration = mkOption {
        type = types.str;
        default = "";
        description = "Cachix configuration data.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      # TODO: add script + automate list of packages, that should be cached, examples: below
      # cachix push ${user} $(dirname $(dirname $(readlink -f $(which i3lock-color))))
      # cachix push ${user} $(dirname $(dirname $(readlink -f $(which emacs))))

      home-manager.users.${user} = {
        home.packages = [ stable.cachix ];
        xdg.configFile."cachix/cachix.dhall".text =
          lib.optionalString (cfg.configuration != "") cfg.configuration;
      };
    })
  ];
}
