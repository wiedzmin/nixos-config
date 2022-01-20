{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ext.networking.vpn;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  options = {
    ext.networking.vpn = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable VPN support";
      };
      meta = mkOption {
        type = types.attrs;
        description = "VPN metadata.";
        default = { };
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      workstation.systemtraits.instructions = ''
        ${pkgs.redis}/bin/redis-cli set net/vpn_meta ${lib.strings.escapeNixString (builtins.toJSON cfg.meta)}
      '';
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ "v" ];
          cmd = "${nurpkgs.toolbox}/bin/vpn --status";
          mode = "network";
        }
        {
          key = [ "Shift" "v" ];
          cmd = "${nurpkgs.toolbox}/bin/vpn --stop-all";
          mode = "network";
        }
      ];
    })
  ];
}
