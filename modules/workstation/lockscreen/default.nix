{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };

with lib;

let
  cfg = config.workstation.lockscreen;
in {
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
          ${pkgs.dunst}/bin/dunstify -t ${builtins.toString cfg.notification.timeout} \
                                     -u ${cfg.notification.urgency} 'Locking in ${builtins.toString cfg.timers.lock} seconds' '';
        description = "Command to use for notification display";
      };
      command.lock = mkOption {
        type = types.str;
        default = "${pkgs.i3lock-color}/bin/i3lock-color --keylayout 2 -c 232729 --pass-media-keys && ${pkgs.xorg.xset}/bin/xset dpms force off";
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
      systemd.user.services."screenlocker" = {
        description = "Lock the screen automatically after a timeout";
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        path = [ pkgs.bash ];
        serviceConfig = {
          Type = "simple";
          ExecStartPre = "${config.systemd.package}/bin/systemctl --user import-environment DISPLAY XAUTHORITY";
          ExecStart = ''
            ${pkgs.xidlehook}/bin/xidlehook \
                  ${optionalString cfg.respect.playback "--not-when-audio"} \
                  ${optionalString cfg.respect.fullscreen "--not-when-fullscreen"} \
                  --timer ${builtins.toString cfg.timers.alert} "${cfg.command.notify}" "" \
                  --timer ${builtins.toString cfg.timers.lock} "${cfg.command.lock}" ""
          '';
        };
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [{
        key = [ "XF86ScreenSaver" ];
        cmd = "${cfg.command.lock}";
        mode = "root";
      }];
    })
  ];
}
