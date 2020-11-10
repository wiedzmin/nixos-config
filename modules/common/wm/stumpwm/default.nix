let deps = import ../../nix/sources.nix;
in { config, lib, pkgs, ... }:
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

    ide.emacs.environment = { CURRENT_WM = "stumpwm"; };

    services.xserver.windowManager = {
      default = "stumpwm";
      stumpwm.enable = true;
    };

    home-manager.users.${user} = {
      home.file = { ".stumpwm.d".source = deps.stumpwm-config; };
    };
  };
}
