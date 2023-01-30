{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.wm.i3;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  statusBarImplToCmd = {
    "py3" = "py3status";
    "i3-rs" = "i3status-rs";
    "blocks" = "i3blocks";
  };
  toml = pkgs.formats.toml { };
  inherit (config.wmCommon) prefix;
in
{
  options = {
    wm.i3 = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable i3.";
      };
      isDefault = mkOption {
        type = types.bool;
        default = false;
        description = "Set `i3` as default WM";
      };
      containerLayout = mkOption {
        type = types.enum [ "default" "stacking" "tabbed" ];
        default = "default";
        description = "Default container layout.";
      };
      settings = mkOption {
        type = types.lines;
        # TODO: play with `default_orientation auto`
        default = ''
          font ${config.wmCommon.fonts.default}
          floating_modifier ${prefix}
          hide_edge_borders smart
          workspace_layout ${cfg.containerLayout}

          mouse_warping output
          focus_follows_mouse no
          bindsym Button4 nop
          bindsym button5 nop

          force_display_urgency_hint 1500 ms
          focus_on_window_activation smart
        '';
        description = "Custom settings for i3.";
      };
      windowRules.method = mkOption {
        type = types.enum [ "internal" "ipc" ];
        default = "internal";
        description = ''
          How to enforce windows placement according to mapping rules

          `internal` - use `for_window` clauses in configuration
          `ipc` - rely on IPC service, listening for window titles changes
        '';
      };
      mouseFollowsFocus = mkOption {
        type = types.bool;
        default = true;
        description = "Does mouse cursor follow window focus?";
      };
      statusbar.impl = mkOption {
        type = types.enum [ "py3" "i3-rs" "blocks" ];
        default = "py3";
        description = "Statusbar implementation";
      };
      statusbar.deps = mkOption {
        type = types.listOf types.package;
        default = with pkgs; [ dbus dunst gawk iproute2 iw kbdd openvpn perl xdotool yad ];
        visible = false;
        readOnly = true;
        internal = true;
        description = "Extra dependencies, mostly for used statusbar implementations";
      };
      modeExitBindings = mkOption {
        type = types.listOf (types.listOf types.str);
        default = [ [ "q" ] [ "Escape" ] [ "Control" "g" ] ];
        description = "Unified collection of keybindings used to exit to default mode.";
      };
      theme.client = mkOption {
        type = types.lines;
        default = "";
        description = "Clients theming";
      };
      theme.bar = mkOption {
        type = types.lines;
        default = "";
        description = "Bar(s) theming";
      };
      theme.i3status-rs = mkOption {
        type = types.attrs;
        default = { };
        description = "i3status-rust theming";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      environment.pathsToLink = optionals (cfg.statusbar.impl == "blocks") [ "/libexec" ];
      fonts.fonts = optionals (cfg.statusbar.impl == "blocks") (with pkgs; [ font-awesome ]);

      wmCommon = {
        enable = true;
        modeBindings = {
          "Passthrough Mode - Press M+F11 to exit" = [ prefix "F11" ];
          "scratchpad" = [ prefix "grave" ];
        };
        keybindings.common = [
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
            key = [ prefix "Shift" "s" ];
            cmd = "sticky toggle";
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
            key = [ prefix "Shift" "t" ];
            cmd = "split toggle";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" "<" ];
            cmd = "focus parent";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" ">" ];
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
            key = [ "b" ];
            cmd = "${pkgs.i3-balance-workspace}/bin/i3_balance_workspace";
            mode = "layout";
          }
          {
            key = [ "Shift" "b" ];
            cmd = "${pkgs.i3-balance-workspace}/bin/i3_balance_workspace --scope focus";
            mode = "layout";
          }
          {
            key = [ prefix "Tab" ];
            cmd = ''${pkgs.wmfocus}/bin/wmfocus --chars qweasdzxc --textcoloralt "#eeeeee"'';
            mode = "root";
          }
          {
            key = [ prefix "Shift" "bracketleft" ];
            cmd = ''move workspace to output ${config.attributes.hardware.monitors.externalPrimaryHead.name}'';
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" "bracketright" ];
            cmd = ''move workspace to output ${config.attributes.hardware.monitors.externalSecondaryHead.name}'';
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" "backslash" ];
            cmd = ''move workspace to output ${config.attributes.hardware.monitors.internalHead.name}'';
            mode = "root";
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
          {
            key = [ "h" ];
            cmd = "move scratchpad";
            mode = "scratchpad";
            raw = true;
          }
          {
            key = [ "s" ];
            cmd = "scratchpad show";
            mode = "scratchpad";
            raw = true;
          }
          {
            key = [ prefix "y" ];
            cmd = "fullscreen disable; floating enable; resize set 40 ppt 40 ppt; sticky enable; move position 60 ppt 4 ppt";
            mode = "root";
            raw = true;
          }
        ];
      };

      navigation.bookmarks.entries = {
        i3-userguide = {
          desc = "i3 userguide";
          remote.url = "https://i3wm.org/docs/userguide.html";
        };
      };

      home-manager.users."${user}" = {
        xdg.configFile = {
          "i3/config".text = ''
            # i3 config file (v4)

            ${cfg.settings}
            ${bindkeysI3 config.wmCommon.keybindings.common config.wmCommon.modeBindings cfg.modeExitBindings
            config.wmCommon.workspaces config.controlcenter.commandsDebugLogRoot}
            ${mkWorkspacesI3 config.wmCommon.workspaces prefix}

            ${optionalString (cfg.windowRules.method == "internal")
              (with config.wmCommon; genPlacementRulesI3
                (windowRulesFromBookmarks config.navigation.bookmarks.entries ++ wsMapping.rules) workspaces)}

            for_window [class="Rofi"] floating enable

            ${genWindowRulesFloatI3 config.wmCommon.wsMapping.rules}

            ${bindkeysFocusI3 config.wmCommon.wsMapping.rules}

            ${with config.wmCommon; genScratchpadSettingsI3 wsMapping.rules keybindings.common cfg.modeExitBindings workspaces config.controlcenter.commandsDebugLogRoot}

            bindsym ${prefix}+Return workspace back_and_forth

            ${lib.concatStringsSep "\n"
              (lib.forEach (builtins.filter (e: !builtins.hasAttr "restart" e || (builtins.hasAttr "restart" e && !e.restart))
                config.wmCommon.autostart.entries)
                (e: "exec --no-startup-id ${e.cmd}"))}

            ${lib.concatStringsSep "\n"
              (lib.forEach (builtins.filter (e: builtins.hasAttr "restart" e && e.restart)
                config.wmCommon.autostart.entries)
                (e: "exec_always --no-startup-id ${e.cmd}"))}

            ${optionalString (cfg.theme.client != "") cfg.theme.client}

            bar {
                position top
                tray_output ${config.attributes.hardware.monitors.internalHead.name}
                mode dock
                modifier ${prefix}
                workspace_buttons yes
                strip_workspace_numbers yes
                font ${config.wmCommon.fonts.statusbar}
                status_command ${statusBarImplToCmd."${cfg.statusbar.impl}"}
                ${optionalString (cfg.theme.bar != "") ''
                colors {
                ${cfg.theme.bar}
                }
                ''}
                bindsym button4 nop
                bindsym button5 nop
            }
          '';
        } // optionalAttrs (cfg.statusbar.impl == "blocks") {
          # FIXME: currently broken
          # TODO: create derivation for accessing prebuilt C blocklets
          # TODO: tune kbdd_layout output (either patch packages or extract script)
          # TODO: ${inputs.i3blocks-contrib}/dunst/dunst - reimplement and unwire some meta (fonts, etc)
          # TODO: make homebrew script for openvpn/nm-vpn, refer to `toolbox/vpn`
          # TODO: review https://github.com/vivien/i3blocks-contrib blocklets
          "i3blocks/config".text = ''
            [disk]
            command=${inputs.i3blocks-contrib}/disk/disk
            LABEL=H:
            DIR=$HOME/${user}
            ALERT_LOW=10
            interval=30

            [bandwidth3]
            command=${inputs.i3blocks-contrib}/bandwidth3/bandwidth3
            unit=Kb
            interval=persist
            markup=pango
            PRINTF_COMMAND=printf " %-3.1f/%3.1f %s/s\n", rx, wx, unit

            [calendar]
            command=${inputs.i3blocks-contrib}/calendar/calendar
            interval=1
            DATEFMT=+%a %H:%M:%S
            HEIGHT=180
            WIDTH=220

            [kbdd_layout]
            command=${inputs.i3blocks-contrib}/kbdd_layout/kbdd_layout
            interval=persist
          '';
        } // optionalAttrs (cfg.statusbar.impl == "py3") {
          "i3status/config".text = ''
            general {
              colors = true
              interval = 5
            }

            order += "load"
            order += "disk /"
            order += "wireless wlan0"
            order += "battery 0"
            order += "clock"
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

            clock {
              format_time = "%a %d-%m-%Y %H:%M"
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
        } // lib.optionalAttrs (cfg.statusbar.impl == "i3-rs") {
          "i3status-rust/config.toml".source = toml.generate "config.toml" ({
            icons = "awesome";
            block = [
              {
                block = "cpu";
                interval = 1;
                format = " {utilization}";
              }
              {
                block = "load";
                interval = 1;
                format = " {1m}";
              }
              {
                block = "memory";
                format_mem = "{mem_free;M}";
                display_type = "memory";
                icons = true;
                clickable = false;
                interval = 5;
                warning_mem = 80;
                warning_swap = 80;
                critical_mem = 95;
                critical_swap = 95;
              }
              {
                block = "disk_space";
                path = "/";
                alias = "/";
                info_type = "available";
                unit = "GB";
                interval = 20;
              }
              {
                block = "net";
                device = "wlan0";
                format = "{ssid} {signal_strength}";
                interval = 5;
              }
              ({
                block = "battery";
                format = " {percentage} {time}";
              } // optionalAttrs config.services.upower.enable {
                driver = "upower";
              } // optionalAttrs (with config; services.upower.enable && attributes.hardware.dmiSystemVersion == "ThinkPad X270") {
                device = "DisplayDevice";
              } // optionalAttrs (!config.services.upower.enable) {
                driver = "sysfs";
                device = "BAT0";
              })
            ] ++ lib.optionals true forEach config.ext.networking.wireless.bluetooth.devices
              (dev: {
                block = "bluetooth";
                mac = dev.mac;
                format = "${dev.name} {percentage}";
                format_unavailable = "${dev.name} x";
                hide_disconnected = true;
              }) ++
            [{
              block = "sound";
              mappings = {
                # TODO: adjust icons
                "alsa_output.usb-Logitech_Logitech_USB_Headset_000000000000-00.analog-stereo" = "🔈";
                "alsa_output.pci-0000_00_1b.0.analog-stereo" = "🎧";
              };
            }
              {
                block = "time";
                interval = 60;
                format = " %a %d-%m-%Y %R";
                timezone = config.time.timeZone;
                locale = "ru_RU";
              }
              {
                block = "keyboard_layout";
                driver = "kbddbus";
              }
              # TODO: block = "music"
            ];
          } // lib.optionalAttrs (cfg.theme.i3status-rs != "") {
            theme = cfg.theme.i3status-rs;
          });
        };
      };
    })
    (mkIf (cfg.enable && cfg.isDefault) {
      assertions = [{
        assertion = !config.wm.awesome.isDefault && !config.wm.qtile.isDefault && !config.wm.stumpwm.isDefault && !config.wm.xmonad.isDefault;
        message = "i3: exactly one WM could be the default.";
      }];

      shell.core.variables = [{ CURRENT_WM = "i3"; global = true; emacs = true; }];

      wmCommon = {
        autostart.entries = [{ cmd = "${pkgs.i3-auto-layout}/bin/i3-auto-layout"; restart = true; }];
      };

      workstation.systemtraits.instructions = ''
        ${pkgs.redis}/bin/redis-cli set wm/keybindings ${
          lib.strings.escapeNixString (builtins.toJSON config.wmCommon.keybindings.common)
        }
        ${pkgs.redis}/bin/redis-cli set wm/modebindings ${
          lib.strings.escapeNixString (builtins.toJSON config.wmCommon.modeBindings)
        }
      '';

      systemd.user.services.i3-kbdswitcher = {
        description = "i3 KBD switcher";
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        path = [ pkgs.xkb-switch pkgs.i3 pkgs.bash ];
        serviceConfig = {
          Type = "simple";
          ExecStart = "${nurpkgs.wmtools}/bin/i3-kbd";
          Restart = "on-failure";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };

      systemd.user.services.i3-desktops = optionalAttrs (cfg.windowRules.method == "ipc") {
        description = "i3 windows mapper";
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        path = [ pkgs.i3 pkgs.bash ];
        serviceConfig = {
          Type = "simple";
          ExecStart = "${nurpkgs.wmtools}/bin/i3-desktops";
          Restart = "always";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };

      systemd.user.services.i3-mousewarp = optionalAttrs cfg.mouseFollowsFocus {
        description = "mouse warp for i3";
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        path = [ pkgs.i3 pkgs.bash ];
        serviceConfig = {
          Type = "simple";
          ExecStart = "${nurpkgs.wmtools}/bin/i3-mousewarp";
          Restart = "always";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };

      services.xserver = {
        windowManager = {
          i3 = {
            enable = true;
            extraPackages = with pkgs;
              lib.optionals (cfg.statusbar.impl == "py3") [ i3status python3Packages.py3status file ]
              ++ lib.optionals (cfg.statusbar.impl == "i3-rs") [ i3status-rust ]
              ++ lib.optionals (cfg.statusbar.impl == "blocks") [ i3blocks ] ++ cfg.statusbar.deps;
          };
        };
        displayManager = { defaultSession = "none+i3"; };
      };
    })
  ];
}
