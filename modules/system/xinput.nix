{ config, lib, pkgs, ... }:
with lib;

let cfg = config.xinput;
in {
  options = {
    xinput = {
      gestures.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable mouse gestures using Fusuma input method.";
      };
      keynav.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable controlling mouse with keyboard.";
      };
      xkeysnail.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable xkeysnail.";
      };
      xkeysnail.configFile = mkOption {
        type = types.str;
        default = "";
        description = ''
          Config file absolute path.
        '';
      };
      xkeysnail.inputDevices = mkOption {
        type = types.listOf types.str;
        default = [ ];
        example = literalExample ''
          [
              "/dev/input/event3"
          ]
        '';
        description = ''
          Keyboard devices to remap (if omitted,
          xkeysnail will choose proper keyboard
          devices)
        '';
      };
      constraintMouse.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to constraint mouse within xrandr screen(s).";
      };
      constraintMouse.top = mkOption {
        type = types.int;
        default = 25;
        description = "Top margin size.";
      };
      constraintMouse.left = mkOption {
        type = types.int;
        default = 0;
        description = "Left margin size.";
      };
      constraintMouse.right = mkOption {
        type = types.int;
        default = 0;
        description = "Right margin size.";
      };
      constraintMouse.bottom = mkOption {
        type = types.int;
        default = 0;
        description = "Bottom margin size.";
      };
      xmodmap.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to use xmodmap";
      };
      xmodmap.rc = mkOption {
        type = types.lines;
        default = "";
        description = "Xmodmaprc contents";
      };
      debug.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to add some X-related tools to PATH";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.gestures.enable {
      systemd.user.services."fusuma" = let
        fusumaConfig = pkgs.writeText "fusuma.yml" (builtins.toJSON {
          # TODO: maybe extract some parameters from below
          "swipe" = {
            "3" = {
              "left" = { "command" = "${pkgs.xdotool}/bin/xdotool key alt+."; };
              "right" = { "command" = "${pkgs.xdotool}/bin/xdotool key alt+,"; };
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
              "left" = { "command" = "${pkgs.xdotool}/bin/xdotool key super+Left"; };
              "right" = { "command" = "${pkgs.xdotool}/bin/xdotool key super+Right"; };
              "up" = { "command" = "${pkgs.xdotool}/bin/xdotool key super+a"; };
              "down" = { "command" = "${pkgs.xdotool}/bin/xdotool key super+s"; };
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
        });
      in {
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
    })
    (mkIf cfg.keynav.enable {
      systemd.user.services."keynav" = let
        keynavConfig = pkgs.writeText "keynav.conf" (''
          clear
          grid-nav on
          ctrl+semicolon start
          Escape end
          ctrl+bracketleft end
          q record ~/.keynav_macros
          shift+at playback
          a history-back
          Left cut-left
          Down cut-down
          Up cut-up
          Right cut-right
          shift+Right move-right
          shift+Left move-left
          shift+Down move-down
          shift+Up move-up
          space warp
          Return warp,click 1,end
          semicolon warp,end
          w warp
          t windowzoom
          c cursorzoom 300 300
          e end
          1 click 1
          2 click 2
          3 click 3
          ctrl+h cut-left
          ctrl+j cut-down
          ctrl+k cut-up
          ctrl+l cut-right
          y cut-left,cut-up
          u cut-right,cut-up
          b cut-left,cut-down
          n cut-right,cut-down
          shift+y move-left,move-up
          shift+u move-right,move-up
          shift+b move-left,move-down
          shift+n move-right,move-down
          ctrl+y cut-left,cut-up
          ctrl+u cut-right,cut-up
          ctrl+b cut-left,cut-down
          ctrl+n cut-right,cut-down
        '');
      in {
        description = "Navigate mouse with keyboard";
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = {
          Type = "simple";
          ExecStartPre = "${config.systemd.package}/bin/systemctl --user import-environment DISPLAY XAUTHORITY";
          ExecStart = ''${pkgs.keynav}/bin/keynav "loadconfig ${keynavConfig}"'';
        };
      };
    })
    (mkIf cfg.xkeysnail.enable {
      assertions = [{
        assertion = cfg.xkeysnail.configFile != "";
        message = "XKeysnail: must provide config file path.";
      }];

      systemd.user.services."xkeysnail" = {
        description = "Xkeysnail";
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = {
          PIDFile = "/run/xkeysnail.pid";
          Restart = "always";
          RestartSec = 1;
          ExecStart = "/run/wrappers/bin/sudo ${pkgs.xkeysnail}/bin/xkeysnail ${
            optionalString (cfg.xkeysnail.inputDevices != [ ]) "--devices ${lib.concatStringsSep " " cfg.xkeysnail.inputDevices}"
          } ${cfg.xkeysnail.configFile}";
        };
      };
    })
    (mkIf cfg.constraintMouse.enable {
      systemd.user.services."xpointerbarrier" = {
        description = "Create pointer barriers around each XRandR screen";
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = {
            Type = "simple";
            ExecStartPre = "${config.systemd.package}/bin/systemctl --user import-environment DISPLAY XAUTHORITY";
            ExecStart = ''
              ${pkgs.xpointerbarrier}/bin/xpointerbarrier ${builtins.toString cfg.constraintMouse.top} \
                                                          ${builtins.toString cfg.constraintMouse.left} \
                                                          ${builtins.toString cfg.constraintMouse.right} \
                                                          ${builtins.toString cfg.constraintMouse.bottom}
            '';
        };
      };
    })
    (mkIf cfg.xmodmap.enable {
      services.xserver.displayManager.sessionCommands = let
        xmodmaprc = pkgs.writeText "xmodmaprc" "${cfg.xmodmap.rc}";
        in ''
          ${pkgs.xlibs.xmodmap}/bin/xmodmap ${xmodmaprc}
          ${pkgs.xlibs.xmodmap}/bin/xmodmap -e "clear Lock"
        '';
    })
    (mkIf cfg.debug.enable {
      environment.systemPackages = with pkgs; [
        xlibs.xev
        xlibs.xprop
      ];
    })
  ];
}
