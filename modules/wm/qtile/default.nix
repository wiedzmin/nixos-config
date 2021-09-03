{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with import ../../wmutil.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.wm.qtile;
  user = config.attributes.mainUser.name;
in
{
  options = {
    wm.qtile = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Qtile";
      };
      config.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable custom config for Qtile";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      shell.core.variables = [{ CURRENT_WM = "qtile"; global = true; emacs = true; }];
    })
    (mkIf cfg.config.enable {
      fonts.fonts = with pkgs; [ font-awesome ];

      nixpkgs.config.packageOverrides = _: rec {
        debug-qtile = mkWMDebugScript "debug-qtile" pkgs.qtile "qtile start -c $XDG_CONFIG_HOME/qtile/config.py";
      };

      home-manager.users.${user} = {
        home.packages = with pkgs; [ debug-qtile ];
      };
    })
  ];
}
