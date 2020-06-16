{ config, lib, pkgs, ... }:
with import ../../../util.nix { inherit config lib pkgs; };
with import ../wmutil.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.wm.i3;
  prefix = config.wmCommon.prefix;
in {
  options = {
    wm.i3 = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable i3.";
      };
      settings = mkOption {
        type = types.lines;
        default = ''
          font ${config.wmCommon.fonts.default}
          floating_modifier ${prefix}
          hide_edge_borders smart
          workspace_layout stacked
        '';
        description = "Custom settings for i3.";
      };
      keys = mkOption {
        type = types.listOf types.attrs;
        default = [
          {
            key = [ prefix "Shift" "q" ];
            cmd = ''exec "i3-msg reload"'';
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "q" ];
            cmd = "restart";
            mode = "root";
            raw = true;
          }
          {
            key = [ "Control" "backslash" ];
            cmd = "nop";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Left" ];
            cmd = "focus left";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Down" ];
            cmd = "focus down";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Up" ];
            cmd = "focus up";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Right" ];
            cmd = "focus right";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" "Left" ];
            cmd = "move left";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" "Down" ];
            cmd = "move down";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" "Up" ];
            cmd = "move up";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" "Right" ];
            cmd = "move right";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "bar" ];
            cmd = "split h";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "minus" ];
            cmd = "split v";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "f" ];
            cmd = "fullscreen toggle";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" "f" ];
            cmd = "floating toggle";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "t" ];
            cmd = "focus mode_toggle";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" "a" ];
            cmd = "focus parent";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" "d" ];
            cmd = "focus child";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "F12" ];
            cmd = "kill";
            mode = "root";
            raw = true;
          }
          {
            key = [ "s" ];
            cmd = "layout stacking";
            mode = "layout";
            raw = true;
          }
          {
            key = [ "t" ];
            cmd = "layout tabbed";
            mode = "layout";
            raw = true;
          }
          {
            key = [ "w" ];
            cmd = "layout toggle split";
            mode = "layout";
            raw = true;
          }
          {
            key = [ "Left" ];
            cmd = "resize shrink width 10 px or 10 ppt";
            mode = "resize";
            raw = true;
            sticky = true;
          }
          {
            key = [ "Down" ];
            cmd = "resize grow height 10 px or 10 ppt";
            mode = "resize";
            raw = true;
            sticky = true;
          }
          {
            key = [ "Up" ];
            cmd = "resize shrink height 10 px or 10 ppt";
            mode = "resize";
            raw = true;
            sticky = true;
          }
          {
            key = [ "Right" ];
            cmd = "resize grow width 10 px or 10 ppt";
            mode = "resize";
            raw = true;
            sticky = true;
          }
          {
            key = [ prefix "F11" ];
            cmd = ''mode "default"'';
            mode = "Passthrough Mode - Press M+F11 to exit";
            raw = true;
          }
        ];
        description = "i3-related keybindings.";
      };
      modeBindings = mkOption {
        type = types.attrs;
        default = { };
        description = "Modes keybindings.";
      };
      modeExitBindings = mkOption {
        type = types.listOf (types.listOf types.str);
        default = [ [ "q" ] [ "Escape" ] [ "Control" "g" ] ];
        description = "Unified collection of keybindings used to exit to default mode.";
      };
      autostart.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Start some applications automatically.";
      };
      autostart.entries = mkOption {
        type = types.listOf types.str;
        default =
          [ "alacritty" "emacs" "nm-applet" "qutebrowser -P default --class qb-default" "slack" "telegram-desktop" ];
        description = "Applications to start automatically.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = (!config.wm.xmonad.enable && !config.wm.stumpwm.enable);
        message = "i3: exactly one WM could be enabled.";
      }];

      environment.pathsToLink = [ "/libexec" ]; # for i3blocks (later)

      services.xserver = {
        windowManager = {
          i3 = {
            enable = true;
            extraPackages = with pkgs; [ i3status python3Packages.py3status ];
          };
        };
        displayManager = { defaultSession = "none+i3"; };
      };

      nixpkgs.config.packageOverrides = _: rec {
        kbdctl = writePythonScriptWithPythonPackages "kbdctl" [
          pkgs.python3Packages.i3ipc
          pkgs.xdotool
          pkgs.emacs
          pkgs.xkb-switch
        ] (builtins.readFile
          (pkgs.substituteAll ((import ../../subst.nix { inherit config pkgs lib; }) // { src = ./kbdctl.py; })));
      };

      wmCommon.modeBindings = {
        "layout" = [ prefix "less" ];
        "resize" = [ prefix "z" ];
        "run" = [ prefix "r" ];
        "network" = [ prefix "n" ];
        "virt" = [ prefix "d" ];
        "services" = [ prefix "s" ];
        "windows" = [ prefix "w" ];
        "vpn" = [ prefix "v" ];
        "Passthrough Mode - Press M+F11 to exit" = [ prefix "F11" ];
      };

      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = [ pkgs.kbdctl ];
        xdg.configFile = {
          # TODO: review and adopt https://github.com/guillaumecherel/i3-modal
          "i3/config".text = ''
            # i3 config file (v4)

            ${cfg.settings}
            ${mkKeybindingsI3 (cfg.keys ++ config.wmCommon.keys) config.wmCommon.modeBindings cfg.modeExitBindings}
            ${mkWorkspacesI3 config.wmCommon.workspaces prefix}
            ${lib.concatStringsSep "\n" (lib.forEach cfg.autostart.entries (e: "exec --no-startup-id ${e}"))}

            ${with config.wmCommon; mkPlacementRulesI3 workspaces wsMapping.rules}

            ${mkWindowRulesFloatI3 config.wmCommon.wsMapping.rules}

            ${mkKeybindingsFocusI3 config.wmCommon.wsMapping.rules}

            bindsym ${prefix}+Tab workspace back_and_forth

            exec_always --no-startup-id ${pkgs.kbdctl}/bin/kbdctl

            bar {
                tray_output LVDS-1
                mode hide
                modifier ${prefix}
                workspace_buttons yes
                strip_workspace_numbers yes
                font ${config.wmCommon.fonts.statusbar}
                status_command py3status
            }
          '';
          "i3status/config".text = ''
            general {
              colors = true
              interval = 5
            }

            order += "load"
            order += "disk /"
            order += "wireless wlan0"
            order += "battery 0"
            order += "tztime local"
            order += "keyboard_layout"

            wireless wlan0 {
              format_up = "%essid:%quality"
              format_down = "❌"
            }

            battery 0 {
              format = "%status%percentage %remaining"
              format_down = "❌"
              status_chr = "⚡"
              status_bat = "🔋"
              status_unk = "?"
              status_full = "☻"
              path = "/sys/class/power_supply/BAT%d/uevent"
              low_threshold = 10
              integer_battery_capacity = true
              last_full_capacity = true
            }

            tztime local {
              format = "%d-%m-%Y %H:%M"
              hide_if_equals_localtime = false
            }

            load {
              format = "%5min"
            }

            disk "/" {
              format = "%free"
            }

            keyboard_layout {
              layouts = ['us', 'ru']
            }
          '';
        };
      };
    })
  ];
}
