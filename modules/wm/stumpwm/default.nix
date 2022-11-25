{ config, lib, ... }:
with lib;

let
  cfg = config.wm.stumpwm;
  user = config.attributes.mainUser.name;
in
{
  options = {
    wm.stumpwm = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable stumpwm";
      };
      isDefault = mkOption {
        type = types.bool;
        default = false;
        description = "Set `StumpWM` as default WM";
      };
    };
  };

  # FIXME: debug script + respective harness
  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        home.file = {
          ".stumpwm.d/layouts" = {
            source = ./layouts;
            recursive = true;
          };
          ".stumpwm.d/custom.lisp".source = ./custom.lisp;
          ".stumpwm.d/defs.lisp".source = ./defs.lisp;
          ".stumpwm.d/init.lisp".source = ./init.lisp;
          ".stumpwm.d/keydefs.lisp".source = ./keydefs.lisp;
        };
      };
    })
    (mkIf (cfg.enable && cfg.isDefault) {
      assertions = [{
        assertion = !config.wm.awesome.isDefault && !config.wm.i3.isDefault && !config.wm.qtile.isDefault && !config.wm.xmonad.isDefault;
        message = "stumpwm: exactly one WM could be the default.";
      }];

      shell.core.variables = [{ CURRENT_WM = "stumpwm"; global = true; emacs = true; }];

      services.xserver = {
        windowManager.stumpwm.enable = true;
        displayManager = { defaultSession = "none+stumpwm"; };
      };
    })
  ];
}
