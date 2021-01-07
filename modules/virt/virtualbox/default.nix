{ config, inputs, lib, pkgs, ... }:
with import ../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ext.virtualization.virtualbox;
  user = config.attributes.mainUser.name;
in {
  options = {
    ext.virtualization.virtualbox = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable VirtualBox";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      virtualisation.virtualbox.host.enable = true;
      users.users.${user}.extraGroups = [ "vboxusers" ];
    })
  ];
}
