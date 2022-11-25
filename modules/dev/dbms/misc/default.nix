{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dbms.misc;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
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
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      dev.projectenv.templates.entries = {
        "tools.dbms" = configPrefix config.navigation.bookmarks.workspaces.roots "modules/dev/dbms/misc/templates/dbms";
      };
    })
    (mkIf (cfg.enable && cfg.controlCenter.enable) {
      assertions = [
        {
          assertion = config.workstation.systemtraits.enable;
          message = "dev/dbms/misc: must enable systemtraits maintainence.";
        }
        {
          assertion = config.ext.networking.vpn.enable;
          message = "dev/dbms/misc: must enable vpn functionality.";
        }
      ];

      nixpkgs.config.packageOverrides = _: rec {
        dbms = mkPythonScriptWithDeps pkgs "dbms" (with pkgs; [ pass nurpkgs.pystdlib python3Packages.redis tmux nurpkgs.toolbox ])
          (builtins.readFile ./scripts/dbms.py);
      };

      workstation.systemtraits.instructions = ''
        ${pkgs.redis}/bin/redis-cli set misc/dbms_meta ${
          lib.strings.escapeNixString (builtins.toJSON cfg.controlCenter.meta)
        }
      '';

      home-manager.users."${user}" = { home.packages = lib.optionals (cfg.controlCenter.meta != { }) (with pkgs; [ dbms beekeeper-studio ]); };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.common = lib.optionals (cfg.controlCenter.meta != { }) [{
        key = [ "d" ];
        cmd = ''${pkgs.dbms}/bin/dbms --term-command "${
          lib.concatStringsSep " " config.attributes.vt.default.cmd}" '';
        mode = "dev";
      }];
    })
    (mkIf config.attributes.debug.scripts { home-manager.users."${user}" = { home.packages = with pkgs; [ dbms ]; }; })
  ];
}
