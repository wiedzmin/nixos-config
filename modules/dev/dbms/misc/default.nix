{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dbms.misc;
  user = config.attributes.mainUser.name;
in
{
  options = {
    dbms.misc = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable misc helper DBMS tools.";
      };
      controlCenter.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable various DBMS instances access point.";
      };
      controlCenter.meta = mkOption {
        type = types.attrsOf types.attrs;
        default = { };
        description = "Various dbms access metadata.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable development infra for Emacs";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = { home.packages = with pkgs; [ sq sqlite ]; };
    })
    (mkIf (cfg.enable && cfg.controlCenter.enable) {
      assertions = [
        {
          assertion = config.workstation.systemtraits.enable;
          message = "dev/dbms/misc: must enable systemtraits maintenance.";
        }
        {
          assertion = config.ext.networking.vpn.enable;
          message = "dev/dbms/misc: must enable vpn functionality.";
        }
      ];

      workstation.systemtraits.instructions = ''
        ${pkgs.redis}/bin/redis-cli set misc/dbms_meta ${mkRedisJSON cfg.controlCenter.meta}
      '';

      home-manager.users."${user}" = { home.packages = lib.optionals (cfg.controlCenter.meta != { }) (with pkgs; [ beekeeper-studio ]); };
    })
    (mkIf cfg.emacs.enable {
      ide.emacs.core.treesitter.grammars = {
        sql = "https://github.com/m-novikov/tree-sitter-sql";
      };
    })
  ];
}
