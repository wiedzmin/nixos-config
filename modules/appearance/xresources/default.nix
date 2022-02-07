{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.appearance.xresources;
  user = config.attributes.mainUser.name;
in {
  options = {
    appearance.xresources = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Xresources customization.";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        xresources.properties = {
          "Xmessage*Buttons" = "Quit";
          "Xmessage*defaultButton" = "Quit";
          "Xmessage*international" = true;

          "urgentOnBell" = true;
          "visualBell" = true;

          "Xft.antialias" = true;
          "Xft.autohint" = false;
          "Xft.dpi" = "120";
          "Xft.hinting" = true;
          "Xft.hintstyle" = "hintmedium";
        };
        home.activation.xrdb = {
          after = [ "linkGeneration" ];
          before = [ ];
          data = "DISPLAY=:0 ${pkgs.xorg.xrdb}/bin/xrdb ${homePrefix user ".Xresources"} || exit 0";
        };
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ "Shift" "r" ];
          cmd = "${pkgs.xorg.xrdb}/bin/xrdb $HOME/.Xresources";
          mode = "xserver";
        }
      ];
    })
  ];
}
