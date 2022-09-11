{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.workstation.backups;
in
{
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
