{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.timeTracking;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in {
  options = {
    dev.timeTracking = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable dev timetracking infra.";
      };
      identities = mkOption {
        type = types.attrs;
        default = { };
        example = {
          "id" = {
            jira = {
              creds = [ "login" "password" ];
              server = "<server url>";
              project = "foo";
            };
            workspaceRoot = "/path/to/root";
            meta = { bar = "quux"; };
          };
        };
        description = "Timetracking identities collection.";
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
      assertions = [{
        assertion = config.localinfra.systemtraits.enable;
        message = "dev/timetracking: must enable systemtraits maintainence.";
      }];

      nixpkgs.config.packageOverrides = _: rec {
        ttctl = mkPythonScriptWithDeps "ttctl"
          (with pkgs; [ python3Packages.jira python3Packages.pytz python3Packages.redis nurpkgs.pystdlib yad ])
          (readSubstituted ../../subst.nix ./scripts/ttctl.py);
      };
      localinfra.systemtraits.instructions = ''
        ${pkgs.redis}/bin/redis-cli set timetracking/identities ${
          lib.strings.escapeNixString (builtins.toJSON cfg.identities)
        }
      '';
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [{
        key = [ "t" ];
        cmd = "${pkgs.ttctl}/bin/ttctl";
        mode = "dev";
      }];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = { home.packages = with pkgs; [ ttctl ]; };
    })
  ];
}
