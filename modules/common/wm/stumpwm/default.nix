{ config, lib, pkgs, ... }:
with lib;

let cfg = config.wm.stumpwm;
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

    services.xserver.windowManager = {
      default = "stumpwm";
      stumpwm.enable = true;
    };

    home-manager.users."${config.attributes.mainUser.name}" = {
      home.file = {
        ".stumpwm.d".source = pkgs.fetchFromGitHub {
          owner = "wiedzmin";
          repo = "stumpwm-config";
          rev = "832513053bb70fc0d24620bc4607b0031193f526";
          sha256 = "0yx3xnw33zkmgl92jnzvzqqvqb7q80gz9ysm3ngbba3hawn2vmma";
        };
      };
    };
  };
}
