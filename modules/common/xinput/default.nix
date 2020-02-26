let
  deps = import ../../../nix/sources.nix;
  proposed = import deps.nixpkgs-proposed { config.allowUnfree = true; };
in { config, lib, pkgs, ... }:
with lib;

let cfg = config.custom.xinput;
in {
  options = {
    custom.xinput = {
      hardware.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to customize hardware settings (mouse, etc.).";
      };
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
        default = "/home/${config.attributes.mainUser.name}/.config/xkeysnail/config.py";
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
    };
  };

  config = mkMerge [
    (mkIf cfg.hardware.enable {
      hardware = {
        trackpoint = {
          enable = true;
          sensitivity = 255;
          speed = 200;
          emulateWheel = true;
        };
      };
      services.xserver.libinput = {
        enable = true;
        naturalScrolling = true;
        disableWhileTyping = true;
        tapping = false;
        tappingDragLock = false;
        accelSpeed = "0.6";
      };
    })
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

      security.wrappers.sudo = {
        source = "${pkgs.sudo}/bin/sudo";
        owner = "root";
        permissions = "u+s";
      };

      systemd.user.services."xkeysnail" = {
        description = "Xkeysnail";
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = {
          PIDFile = "/run/xkeysnail.pid";
          Restart = "always";
          RestartSec = 1;
          ExecStart = "/run/wrappers/bin/sudo ${proposed.xkeysnail}/bin/xkeysnail ${
              optionalString (cfg.xkeysnail.inputDevices != [ ])
              "--devices ${lib.concatStringsSep " " cfg.xkeysnail.inputDevices}"
            } ${cfg.xkeysnail.configFile}";
        };
      };

      users.users."${config.attributes.mainUser.name}".extraGroups = [ "input" ];

      home-manager.users."${config.attributes.mainUser.name}" = {
        xdg.configFile."xkeysnail/config.py".text = ''
          # -*- coding: utf-8 -*-

          import re
          from xkeysnail.transform import *

          define_conditional_modmap(re.compile(r'Emacs'), {
              Key.RIGHT_CTRL: Key.ESC,
          })

          define_keymap(re.compile("Firefox"), {
              K("C-j"): K("C-f6"), # Type C-j to focus to the content
              K("C-g"): K("f5"),
              K("C-n"): K("C-g"),
              K("C-Shift-n"): K("C-Shift-g"),
              K("M-comma"): K("M-Left"),
              K("M-dot"): K("M-Right"),
              K("C-x"): {
                  K("b"): K("b"),
                  K("k"): K("C-w"),
                  K("u"): K("C-Shift-t"),
                  K("C-s"): K("C-s"),
                  K("C-c"): K("C-q"),
              },
          }, "Firefox")

          define_keymap(re.compile("TelegramDesktop"), {
              K("C-x"): {
                  K("C-c"): K("C-q"),
              },
              K("C-s"): K("Esc"),
              K("C-t"): [K("Shift-Left"), K("C-x"), K("Left"), K("C-v"), K("Right")],
          }, "Telegram")

          define_keymap(re.compile("Alacritty"), {
              K("C-x"): {
                  K("k"): K("C-d"),
              },
          }, "Alacritty")

          # Emacs-like keybindings in non-Emacs applications
          define_keymap(lambda wm_class: wm_class not in ("Emacs", "URxvt", "Alacritty"), {
              # Cursor
              K("C-b"): with_mark(K("left")),
              K("C-f"): with_mark(K("right")),
              K("C-p"): with_mark(K("up")),
              K("C-n"): with_mark(K("down")),
              K("C-h"): with_mark(K("backspace")),
              # Forward/Backward word
              K("M-b"): with_mark(K("C-left")),
              K("M-f"): with_mark(K("C-right")),
              # Beginning/End of line
              K("C-a"): with_mark(K("home")),
              K("C-e"): with_mark(K("end")),
              # Page up/down
              K("M-v"): with_mark(K("page_up")),
              K("C-v"): with_mark(K("page_down")),
              # Beginning/End of file
              K("M-Shift-comma"): with_mark(K("C-home")),
              K("M-Shift-dot"): with_mark(K("C-end")),
              # Newline
              K("C-m"): K("enter"),
              K("C-j"): K("enter"),
              K("C-o"): [K("enter"), K("left")],
              # Copy
              K("C-w"): [K("C-x"), set_mark(False)],
              K("M-w"): [K("C-c"), set_mark(False)],
              K("C-y"): [K("C-v"), set_mark(False)],
              # Delete
              K("C-d"): [K("delete"), set_mark(False)],
              K("M-d"): [K("C-delete"), set_mark(False)],
              # Kill line
              K("C-k"): [K("Shift-end"), K("C-x"), set_mark(False)],
              # Undo
              K("C-slash"): [K("C-z"), set_mark(False)],
              K("C-Shift-ro"): K("C-z"),
              # Mark
              K("C-space"): set_mark(True),
              #K("C-M-space"): with_or_set_mark(K("C-right")),
              # Search
              K("C-s"): K("F3"),
              K("C-r"): K("Shift-F3"),
              K("M-Shift-key_5"): K("C-h"),
              # Cancel
              K("C-g"): [K("esc"), set_mark(False)],
              # Escape
              K("C-q"): escape_next_key,
              # C-x YYY
              K("C-x"): {
                  # C-x h (select all)
                  K("h"): [K("C-home"), K("C-a"), set_mark(True)],
                  # C-x C-f (open)
                  K("C-f"): K("C-o"),
                  # C-x C-s (save)
                  # K("C-s"): K("C-s"),
                  # C-x k (kill tab)
                  K("k"): K("C-f4"),
                  # C-x C-c (exit)
                  K("C-c"): K("C-q"),
                  # cancel
                  K("C-g"): pass_through_key,
                  # C-x u (undo)
                  K("u"): [K("C-z"), set_mark(False)],
              }
          }, "Emacs-like keys")
        '';
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
      services.xserver.displayManager.sessionCommands = let xmodmaprc = pkgs.writeText "xmodmaprc" cfg.xmodmap.rc;
      in ''
        ${pkgs.xlibs.xmodmap}/bin/xmodmap ${xmodmaprc}
        ${pkgs.xlibs.xmodmap}/bin/xmodmap -e "clear Lock"
      '';
    })
  ];
}
