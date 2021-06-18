{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ext.networking.vpn;
  user = config.attributes.mainUser.name;
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
    (mkIf (cfg.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        vpnctl = mkPythonScriptWithDeps "vpnctl" (with pkgs; [ networkmanager nurpkgs.pystdlib python3Packages.redis ])
          (builtins.readFile ./scripts/vpnctl.py);
      };
      workstation.systemtraits.instructions = ''
        ${pkgs.redis}/bin/redis-cli set net/vpn_meta ${lib.strings.escapeNixString (builtins.toJSON cfg.meta)}
      '';
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ "v" ];
          cmd = "${nurpkgs.toolbox}/bin/vpnstatus";
          mode = "network";
        }
        {
          key = [ "Shift" "v" ];
          cmd = "${nurpkgs.toolbox}/bin/vpnstatus --stop-all";
          mode = "network";
        }
      ];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ vpnctl ]; };
    })
  ];
}
