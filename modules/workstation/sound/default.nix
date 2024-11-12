{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

# TODO: https://github.com/graysky2/pulseaudio-ctl
# TODO: https://gitlab.com/DamienCassou/rofi-pulse-select
let
  cfg = config.workstation.sound;
  user = config.attributes.mainUser.name;
  inherit (config.wmCommon) prefix;
in
{
  options = {
    workstation.sound = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Pulseaudio";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      users.users."${user}".extraGroups = [ "pipewire" ];

      home-manager.users."${user}" = { home.packages = with pkgs; [ alsa-utils ]; };

      services.pipewire = {
        enable = true;
        pulse.enable = true;
        alsa.enable = true;
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      # TODO: find a handy balance between PW and mpris (taking respective keybindings into account)
      wmCommon.keybindings.entries = [
        {
          key = [ "XF86AudioMute" ];
          cmd = "${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
          mode = "root";
        }
        {
          key = [ "XF86AudioMicMute" ];
          cmd = "${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle";
          mode = "root";
        }
        {
          key = [ prefix "p" ];
          cmd = "${pkgs.lxqt.pavucontrol-qt}/bin/pavucontrol-qt";
          mode = "root";
        }
      ];
    })
  ];
}
