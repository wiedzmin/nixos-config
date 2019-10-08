{ config, lib, pkgs, ... }:
with lib;

let cfg = config.network;
in {
  options = {
    network = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable networking support";
      };
      tools.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable network monitoring/debug tools";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable && cfg.tools.enable {
      programs = {
        mtr.enable = true;
        wireshark = {
          enable = true;
          package = pkgs.wireshark-qt;
        };
        wavemon.enable = true;
      };
      users.extraUsers."${config.attributes.mainUser.name}".extraGroups = [ "wireshark" ];
    })
  ];
}
