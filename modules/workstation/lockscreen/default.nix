{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.workstation.lockscreen;
  user = config.attributes.mainUser.name;
in
{
  options = {
    workstation.lockscreen = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable automatic screen locking";
      };
      respect.playback = mkOption {
        type = types.bool;
        default = true;
        description = "Do not lock, while playing media";
      };
      respect.fullscreen = mkOption {
        type = types.bool;
        default = true;
        description = "Do not lock, when active window is fullscreen";
      };
      notification.urgency = mkOption {
        type = types.enum [ "normal" "critical" ];
        default = "critical";
        description = "Notification urgency level";
      };
      notification.timeout = mkOption {
        type = types.int;
        default = 7000;
        description = "Notification timeout";
      };
      timers.alert = mkOption {
        type = types.int;
        default = 150;
        description = "Seconds of idle time, before notification fires";
      };
      timers.lock = mkOption {
        type = types.int;
        default = 30;
        description = "Seconds of idle time between alert and locking.";
      };
      command.notify = mkOption {
        type = types.str;
        default = ''
          ${pkgs.libnotify}/bin/notify-send -t ${builtins.toString cfg.notification.timeout} \
                                     -u ${cfg.notification.urgency} 'Locking in ${builtins.toString cfg.timers.lock} seconds' '';
        description = "Command to use for notification display";
      };
      command.lock = mkOption {
        type = types.str;
        default = "${pkgs.i3lock-color}/bin/i3lock-color --keylayout 2 -c 232729 --pass-media-keys && sleep 1 && ${pkgs.xorg.xset}/bin/xset dpms force off";
        description = "Command to use for screen locking";
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
        services.xidlehook = {
          enable = true;
          not-when-fullscreen = cfg.respect.fullscreen;
          not-when-audio = cfg.respect.playback;
          timers = [
            {
              delay = cfg.timers.alert;
              command = cfg.command.notify;
            }
            {
              delay = cfg.timers.lock;
              command = cfg.command.lock;
            }
          ];
        };
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.entries = [{
        key = (if config.attributes.hardware.dmiSystemVersion == "ThinkPad X270" then [ "XF86Tools" ] else [ "XF86ScreenSaver" ]);
        cmd = "${cfg.command.lock}";
        mode = "root";
      }];
    })
  ];
}
