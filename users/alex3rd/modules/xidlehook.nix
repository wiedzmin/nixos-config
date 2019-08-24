{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.services.xidlehook;
in
{
  options = {
    services.xidlehook = {
      enable = mkOption {
        type = types.bool;
        default = false;
        example = true;
        description = ''
          Whether to enable xidlehook.
        '';
      };
      respectPlayback = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Do not lock, while playing media.
        '';
      };
      respectFullscreen = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Do not lock, when active window is fullscreen.
        '';
      };
      notificationUrgency = mkOption {
        type = types.str;
        default = "critical";
        description = ''
          Notification urgency level.
        '';
      };
      notificationTimeout = mkOption {
        type = types.int;
        default = 7000;
        description = ''
          Notification timeout.
        '';
      };
      alertingTimerSec = mkOption {
        type = types.int;
        default = 150;
        description = ''
          Seconds of idle time, before notification fires.
        '';
      };
      lockingTimerSec = mkOption {
        type = types.int;
        default = 30;
        description = ''
          Seconds of idle time between alert and locking.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services."xidlehook" = {
      description = "Lock the screen automatically after a timeout";
      after = [ "graphical-session-pre.target" ];
      partOf = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
      path = [ pkgs.bash ];
      serviceConfig = {
        Type = "simple";
        PassEnvironment = "DISPLAY";
        ExecStart = ''
          ${pkgs.xidlehook}/bin/xidlehook \
                ${optionalString cfg.respectPlayback "--not-when-audio"} \
                ${optionalString cfg.respectFullscreen "--not-when-fullscreen"} \
                --timer normal ${builtins.toString cfg.alertingTimerSec} "${pkgs.dunst}/bin/dunstify \
                               -t ${builtins.toString cfg.notificationTimeout} \
                               -u ${cfg.notificationUrgency} \
                               'Locking in ${builtins.toString cfg.lockingTimerSec} seconds'" "" \
                --timer primary ${builtins.toString cfg.lockingTimerSec} \
                               "${pkgs.i3lock-color}/bin/i3lock-color -c 232729 && ${pkgs.xorg.xset}/bin/xset dpms force off" ""
        '';
      };
    };
  };
}
