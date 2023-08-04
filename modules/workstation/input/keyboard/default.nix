{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.workstation.input.keyboard;
  user = config.attributes.mainUser.name;
in
{
  options = {
    workstation.input.keyboard = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable various auxiliary tools
          for controlling mouse input
        '';
      };
      remappingTool = mkOption {
        type = types.enum [ "xkeysnail" "keyd" ];
        default = "xkeysnail";
        description = "Keyboard remapping tool to use";
      };
      xkeysnail.configPath = mkOption {
        type = types.str;
        default = homePrefix user ".config/xkeysnail/config.py";
        description = "Config file absolute path";
      };
      xkeysnail.inputDevices = mkOption {
        type = types.listOf types.str;
        default = [ ];
        example = literalExpression ''
          [
              "/dev/input/event3"
          ]
        '';
        description = ''
          Keyboard devices to control (if omitted,
          xkeysnail will choose proper keyboard
          devices)
        '';
      };
      xkeysnail.rc = mkOption {
        type = types.lines;
        default = "";
        description = "xkeysnail customizations";
      };
      xkeysnail.rcCommon = mkOption {
        type = types.lines;
        default = ''
          # Emacs-like keybindings in non-Emacs applications
          define_keymap(lambda wm_class: wm_class not in ("Emacs", "URxvt", "Alacritty", "Nyxt"), {
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
        description = "common xkeysnail customizations";
      };
      xkeysnail.setupText = mkOption {
        type = types.lines;
        default = ''
          # -*- coding: utf-8 -*-

          import re
          from xkeysnail.transform import *

          ${cfg.xkeysnail.rc}
          ${cfg.xkeysnail.rcCommon}
        '';
        visible = false;
        internal = true;
        readOnly = true;
        description = "xkeysnail final config.py contents";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.remappingTool == "xkeysnail") {
      assertions = [{
        assertion = cfg.xkeysnail.configPath != "";
        message = "input/keyboard/XKeysnail: must provide path to config file";
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
              optionalString (cfg.xkeysnail.inputDevices != [ ])
              "--devices ${lib.concatStringsSep " " cfg.xkeysnail.inputDevices}"
            } ${cfg.xkeysnail.configPath}";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      users.users."${user}".extraGroups = [ "input" ];
      home-manager.users."${user}" = { xdg.configFile."xkeysnail/config.py".text = cfg.xkeysnail.setupText; };
    })
    # (mkIf (cfg.enable && cfg.remappingTool == "keyd") {
    # })
  ];
}
