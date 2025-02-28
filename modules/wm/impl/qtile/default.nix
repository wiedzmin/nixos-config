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
      isDefault = mkOption {
        type = types.bool;
        default = false;
        description = "Set `Qtile` as default WM";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      fonts.packages = with pkgs; [ font-awesome ];

      nixpkgs.config.packageOverrides = _: {
        debug-qtile = mkWMDebugScript
          pkgs "debug-qtile"
          pkgs.qtile-unwrapped
          [ ]
          config.attributes.hardware.monitors.internalHead
          ''qtile start -c "$XDG_CONFIG_HOME/qtile/config.py"'';
      };

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ debug-qtile ];
        xdg.configFile = {
          "qtile/config.py".text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./config.py ];
        };
      };
    })
    (mkIf (cfg.enable && cfg.isDefault) {
      assertions = [{
        assertion = !config.wm.awesome.isDefault && !config.wm.i3.isDefault && !config.wm.herbstluft.isDefault;
        message = "qtile: exactly one WM could be the default.";
      }];

      shell.core.variables = [{ CURRENT_WM = "qtile"; global = true; emacs = true; }];

      services.xserver = {
        windowManager = {
          qtile = {
            enable = true;
            package = pkgs.qtile-unwrapped;
            extraPackages = python3Packages: with python3Packages; [
              qtile-extras
            ];
          };
        };
        displayManager = { defaultSession = "none+qtile"; };
      };
    })
  ];
}
