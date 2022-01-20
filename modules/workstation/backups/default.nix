{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.workstation.backups;
in {
  options = {
    workstation.backups = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable backups infra";
      };
    };
  };

  config = mkMerge [ (mkIf cfg.enable { }) ];
}
