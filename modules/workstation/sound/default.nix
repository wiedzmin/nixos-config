{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

# TODO: https://github.com/graysky2/pulseaudio-ctl
# TODO: https://gitlab.com/DamienCassou/rofi-pulse-select
let
  cfg = config.workstation.sound.pa;
  user = config.attributes.mainUser.name;
  inherit (config.wmCommon) prefix;
in
{
  options = {
    workstation.sound.pa = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Pulseaudio";
      };
      daemonConfig = mkOption {
        type = types.attrs;
        default = { };
        description = "Pulseaudio daemon configuration";
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
      users.users."${user}".extraGroups = [ "audio" ];

      hardware.pulseaudio = {
        enable = true;
        support32Bit = true;
        package = pkgs.pulseaudioFull; # 'full' for e.g. bluetooth
        systemWide = false;
        daemon.config = cfg.daemonConfig;
        extraConfig = ''
          load-module module-bluetooth-policy auto_switch=2
          load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1
        '';
      };
      environment.systemPackages = with pkgs; [ pasystray lxqt.pavucontrol-qt ];
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      # TODO: find a handy balance between PA and mpris (taking respective keybindings into account)
      wmCommon.keys = [
        {
          key = [ "XF86AudioMute" ];
          cmd = "${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
          mode = "root";
        }
        {
          key = [ "XF86AudioMicMute" ];
          cmd = "${pkgs.pulseaudio}/bin/pactl set-source-mute @DEFAULT_SOURCE@ toggle";
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
