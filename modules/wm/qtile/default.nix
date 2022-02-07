{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
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
      # FIXME: review all `CURRENT_WM` usages, then correct
      shell.core.variables = [{ CURRENT_WM = "i3"; global = true; emacs = true; }];

      services.xserver = {
        windowManager = {
          qtile = {
            enable = true;
          };
        };
        displayManager = { defaultSession = "none+qtile"; };
      };
    })
    (mkIf cfg.config.enable {
      fonts.fonts = with pkgs; [ font-awesome ];

      nixpkgs.config.packageOverrides = _: rec {
        debug-qtile = mkWMDebugScript
          pkgs "debug-qtile" pkgs.qtile config.attributes.hardware.monitors.internalHead
            ''qtile start -c "$XDG_CONFIG_HOME/qtile/config.py"'';
      };

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ debug-qtile ];
        xdg.configFile = {
          "qtile/config.py".text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./config.py ];
        };
      };
    })
  ];
}
