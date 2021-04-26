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
in
{
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
        description = "Cachix configuration data";
      };
      username = mkOption {
        type = types.str;
        default = "";
        description = "Cachix username to push under";
      };
      packageBinariesToCache = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = ''
          List of binaries ,linked to userspace, that represent currently used derivations for respective packages.

          Those derivations will be cached.
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      assertions = [{
        assertion = cfg.username != "";
        message = "nix/cachix: missing username.";
      }];

      home-manager.users.${user} = {
        home.packages = [ stable.cachix ];
        xdg.configFile."cachix/cachix.dhall".text =
          optionalString (cfg.configuration != "") cfg.configuration;
        home.activation.pushToCachix = {
          after = [ "checkLinkTargets" ];
          before = [ ];
          data = concatStringsSep "\n"
            (forEach cfg.packageBinariesToCache
              (binary: "cachix push ${cfg.username} $(dirname $(dirname $(readlink -f $(which ${binary}))))"));
        };
      };
    })
  ];
}
