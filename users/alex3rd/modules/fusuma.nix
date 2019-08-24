{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.services.fusuma;
in
{
  options = {
    services.fusuma = {
      enable = mkOption {
        type = types.bool;
        default = false;
        example = true;
        description = ''
          Whether to enable fusuma input method.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services."fusuma" = let
      fusumaConfig = pkgs.writeText "fusuma.yml" (
        builtins.toJSON {
          # TODO: maybe extract some parameters from below
          "swipe" = {
            "3" = {
              "left" = {
                "command" = "${pkgs.xdotool}/bin/xdotool key alt+.";
              };
              "right" = {
                "command" = "${pkgs.xdotool}/bin/xdotool key alt+,";
              };
              "up" = {
                "command" = "${pkgs.xdotool}/bin/xdotool key ctrl+t";
                "threshold" = "1.5";
              };
              "down" = {
                "command" = "${pkgs.xdotool}/bin/xdotool key ctrl+w";
                "threshold" = "1.5";
              };
            };
            "4" = {
              "left" = {
                "command" = "${pkgs.xdotool}/bin/xdotool key super+Left";
              };
              "right" = {
                "command" = "${pkgs.xdotool}/bin/xdotool key super+Right";
              };
              "up" = {
                "command" = "${pkgs.xdotool}/bin/xdotool key super+a";
              };
              "down" = {
                "command" = "${pkgs.xdotool}/bin/xdotool key super+s";
              };
            };
          };
          "pinch" = {
            "2" = {
              "in" = {
                "command" = "${pkgs.xdotool}/bin/xdotool key ctrl+plus";
                "threshold" = "0.1";
              };
              "out" = {
                "command" = "${pkgs.xdotool}/bin/xdotool key ctrl+minus";
                "threshold" = "0.1";
              };
            };
          };
          "threshold" = {
            "swipe" = "1";
            "pinch" = "1";
          };
          "interval" = {
            "swipe" = "1";
            "pinch" = "1";
          };
        }
      );
    in
      {
        description = "Fusuma input method";
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = {
          PIDFile = "/run/fusuma.pid";
          Restart = "always";
          RestartSec = 1;
          ExecStart = "${pkgs.fusuma}/bin/fusuma -c ${fusumaConfig}";
        };
      };
  };
}
