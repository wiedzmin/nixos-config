{ config, lib, pkgs, ... }:
with lib;

let cfg = config.tools.messengers;
in {
  options = {
    tools.messengers = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether install messengers.";
      };
      auxTools.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable auxillary messengers/tools.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          skype
          tdesktop
        ];
      };
    })
    (mkIf (cfg.enable && cfg.auxTools.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          skype-call-recorder
          slack
          wire-desktop
          zoom-us
        ];
      };
    })
  ];
}
