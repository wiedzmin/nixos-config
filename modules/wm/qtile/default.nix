{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.wm.qtile;
  user = config.attributes.mainUser.name;
  nixpkgs-last-unbroken = import inputs.nixpkgs-last-unbroken {
    config = config.nixpkgs.config // { allowUnfree = true; };
    localSystem = { system = "x86_64-linux"; };
  };
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
      fonts.fonts = with pkgs; [ font-awesome ];

      nixpkgs.config.packageOverrides = _: rec {
        debug-qtile = mkWMDebugScript
          pkgs "debug-qtile"
          nixpkgs-last-unbroken.qtile
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
        assertion = !config.wm.awesome.isDefault && !config.wm.i3.isDefault && !config.wm.stumpwm.isDefault && !config.wm.xmonad.isDefault;
        message = "qtile: exactly one WM could be the default.";
      }];

      # FIXME: review all `CURRENT_WM` usages, then correct
      shell.core.variables = [{ CURRENT_WM = "qtile"; global = true; emacs = true; }];

      services.xserver = {
        windowManager = {
          qtile = {
            enable = true;
            package = nixpkgs-last-unbroken.qtile;
          };
        };
        displayManager = { defaultSession = "none+qtile"; };
      };
    })
  ];
}
