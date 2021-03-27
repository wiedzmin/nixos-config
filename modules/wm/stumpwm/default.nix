{ config, inputs, lib, pkgs, ... }:
with lib;

let
  cfg = config.wm.stumpwm;
  user = config.attributes.mainUser.name;
in {
  options = {
    wm.stumpwm = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable stumpwm.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = (!config.wm.xmonad.enable && !config.wm.i3.enable);
      message = "stumpwm: exactly one WM could be enabled.";
    }];

    ide.emacs.core.environment = { CURRENT_WM = "stumpwm"; };

    services.xserver.windowManager = {
      default = "stumpwm";
      stumpwm.enable = true;
    };

    home-manager.users.${user} = {
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
  };
}
