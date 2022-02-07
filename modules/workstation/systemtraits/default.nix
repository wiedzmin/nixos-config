{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.workstation.systemtraits;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  options = {
    workstation.systemtraits = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable system traits maintainence.

          Essential for custom scripts, etc.
        '';
      };
      instructions = mkOption {
        type = types.lines;
        default = "";
        description = ''
          Set of commands needed to initialize system traits cache.

          Currently, Redis is used.
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      services.redis.enable = true;
      systemd.services.redis.postStart = cfg.instructions;

      home-manager.users."${user}" = { home.packages = with pkgs; [ nurpkgs.redis-tui usbview lsb-release ]; };
    })
  ];
}
