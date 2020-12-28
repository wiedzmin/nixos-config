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
      assertion = (!config.wm.xmonad.enable && !config.wm.i3.enable); # FIXME: update
      message = "stumpwm: exactly one WM could be enabled.";
    }];

    ide.emacs.core.environment = { CURRENT_WM = "stumpwm"; };

    services.xserver.windowManager = {
      default = "stumpwm";
      stumpwm.enable = true;
    };

    # FIXME: rework with regard to inputs, or slurp
    # home-manager.users.${user} = { home.file = { ".stumpwm.d".source = inputs.stumpwm-config; }; };
  };
}
