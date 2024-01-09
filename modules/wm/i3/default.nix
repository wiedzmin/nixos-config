{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.wm.i3;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  statusBarImplToCmd = {
    "py3" = "py3status";
    "i3-rs" = "i3status-rs";
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
      titleAlignment = mkOption {
        type = types.enum [ "left" "center" "right" ];
        default = "left";
        description = "Window titles alignment";
      };
      urgencyHintDuration = mkOption {
        type = types.int;
        default = 500;
        description = "Urgency hint display duration, in ms";
      };
      focusWrappingMode = mkOption {
        type = types.enum [ "yes" "no" "force" "workspace" ];
        default = "workspace";
        description = "Window focus wrapping mode";
      };
      focusOnWindowActivation = mkOption {
        type = types.enum [ "smart" "urgent" "focus" "none" ];
        default = "smart";
        description = "How to handle focus on window activation";
      };
      popupDuringFullscreen = mkOption {
        type = types.enum [ "smart" "leave_fullscreen" ];
        default = "leave_fullscreen";
        description = "How to behave on popup creation in fullscreen mode";
      };
      autoBackAndForth = mkOption {
        type = types.enum [ "yes" "no" ];
        default = "no";
        description = "Automatic back-and-forth when switching to the current workspace";
      };
      gaps.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable gaps between windows";
      };
      gaps.inner.size = mkOption {
        type = types.int;
        default = 3;
        description = "Inner gaps size in pixels";
      };
      gaps.outer.size = mkOption {
        type = types.int;
        default = 3;
        description = "Outer gaps size in pixels";
      };
      settings = mkOption {
        type = types.lines;
        default = ''
          font ${config.wmCommon.fonts.default}
          floating_modifier ${prefix}
          default_orientation auto
          workspace_layout ${cfg.containerLayout}

          title_align ${cfg.titleAlignment}

          hide_edge_borders none

          ${optionalString (config.workstation.input.mouse.keynavTool != "warpd") "mouse_warping output"}
          focus_follows_mouse no
          bindsym Button4 nop
          bindsym button5 nop
          tiling_drag modifier titlebar

          force_display_urgency_hint ${toString cfg.urgencyHintDuration} ms
          focus_on_window_activation ${cfg.focusOnWindowActivation}
          focus_wrapping ${cfg.focusWrappingMode}
          popup_during_fullscreen ${cfg.popupDuringFullscreen}

          workspace_auto_back_and_forth ${cfg.autoBackAndForth}

          ${optionalString cfg.gaps.enable "gaps inner ${toString cfg.gaps.inner.size} px"}
          ${optionalString cfg.gaps.enable "gaps outer ${toString cfg.gaps.outer.size} px"}
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
      windowFocus.impl = mkOption {
        type = types.enum [ "wmfocus" "easyfocus" ];
        default = "wmfocus";
        description = "Which tool to use for hinted windows navigation";
      };
      windowFocus.fontSize = mkOption {
        type = types.int;
        default = 72;
        description = "Windows navigation hints font size";
      };
      mouseFollowsFocus = mkOption {
        type = types.bool;
        default = true;
        description = "Does mouse cursor follow window focus?";
      };
      statusbar.impl = mkOption {
        type = types.enum [ "py3" "i3-rs" ];
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
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable i3wm-related Emacs infra";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      wmCommon = {
        modeBindings = {
          "Passthrough Mode - Press M+F11 to exit" = [ prefix "F11" ];
          "scratchpad" = [ prefix "grave" ];
        };
        keybindings.entries = (forEach [
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
            key = [ prefix "Prior" ];
            cmd = "focus parent";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Next" ];
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
            key = [ prefix "semicolon" ];
            cmd = "layout stacking";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "apostrophe" ];
            cmd = "layout tabbed";
            mode = "root";
            raw = true;
          }
          {
            key = [ "=" ];
            cmd = "${pkgs.i3-balance-workspace}/bin/i3_balance_workspace";
            mode = "resize";
            sticky = true;
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
            key = [ prefix "w" ];
            cmd = "layout toggle split";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" "w" ];
            cmd = "layout toggle stacking tabbed";
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "0" ];
            cmd = "layout toggle all";
            mode = "root";
            raw = true;
          }
          {
            key = [ "Left" ];
            cmd = "resize grow width 10 px or 10 ppt";
            mode = "resize";
            raw = true;
            sticky = true;
          }
          {
            key = [ "Down" ];
            cmd = "resize shrink height 10 px or 10 ppt";
            mode = "resize";
            raw = true;
            sticky = true;
          }
          {
            key = [ "Up" ];
            cmd = "resize grow height 10 px or 10 ppt";
            mode = "resize";
            raw = true;
            sticky = true;
          }
          {
            key = [ "Right" ];
            cmd = "resize shrink width 10 px or 10 ppt";
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
        ]
          (e: e // { wm = "i3"; })) ++ optionals (cfg.windowFocus.impl == "wmfocus") [
          {
            key = [ prefix "Tab" ];
            cmd = ''${pkgs.wmfocus}/bin/wmfocus --halign center --valign center --chars qweasdzxc --font ${
              config.wmCommon.fonts.familySize.large} --textcoloralt "#eeeeee"'';
            mode = "root";
          }
        ] ++ optionals (cfg.windowFocus.impl == "easyfocus") [
          {
            key = [ prefix "Tab" ];
            cmd = ''${pkgs.i3-easyfocus}/bin/i3-easyfocus --all --keys alpha --font ${config.wmCommon.fonts.xlfd.large}'';
            mode = "root";
          }
        ];
      };

      navigation.bookmarks.entries = {
        i3-userguide = {
          desc = "i3 userguide";
          remote.url = "https://i3wm.org/docs/userguide.html";
        };
        i3-debugging = {
          desc = "i3 userguide";
          remote.url = "https://i3wm.org/docs/debugging.html";
        };
      };

      home-manager.users."${user}" = {
        xdg.configFile = {
          "i3/config".text = ''
            # i3 config file (v4)

            ${cfg.settings}
            ${bindkeysI3 config.wmCommon.keybindings.entries config.wmCommon.modeBindings cfg.modeExitBindings
            config.wmCommon.workspaces config.controlcenter.commandsDebugLogRoot}
            ${mkWorkspacesI3 config.wmCommon.workspaces prefix}

            ${optionalString (cfg.windowRules.method == "internal")
              (with config.wmCommon; genPlacementRulesI3
                (windowRulesFromBookmarks config.navigation.bookmarks.entries ++ wsMapping.rules) workspaces)}

            for_window [class="Rofi"] floating enable

            ${genWindowRulesFloatI3 config.wmCommon.wsMapping.rules}
            ${genWindowRulesFullscreenI3 config.wmCommon.wsMapping.rules}

            ${bindkeysFocusI3 config.wmCommon.wsMapping.rules}

            ${with config.wmCommon; genScratchpadSettingsI3 wsMapping.rules keybindings.entries cfg.modeExitBindings workspaces config.controlcenter.commandsDebugLogRoot}

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
                tray_output ${config.attributes.hardware.monitors.externalPrimaryHead.name}
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
            icons = {
              icons = "awesome6";
            };
            block = [
              {
                block = "cpu";
                interval = 1;
                format = " $icon $utilization ";
                format_alt = " $icon $barchart $frequency{ $boost|} ";
              }
              {
                block = "load";
                interval = 1;
                format = " $icon $1m.eng(w:4) ";
              }
              ({
                block = "memory";
                format = " $icon $mem_free.eng(w:3,u:B,p:M) ";
                format_alt = " $icon $mem_avail.eng(w:3,u:B,p:M) ";
                interval = 5;
                warning_mem = 80;
                critical_mem = 95;
              } // optionalAttrs (config.attributes.hardware.memory.swap.enable) {
                warning_swap = 80;
                critical_swap = 95;
              })
              {
                block = "disk_space";
                path = "/";
                format = " $icon $available ";
                format_alt = " $icon $available / $total ";
                info_type = "available";
                alert_unit = "GB";
                alert = 10.0;
                warning = 15.0;
                interval = 20;
              }
              {
                block = "net";
                device = "wlan0";
                format = " $icon $ssid $signal_strength ";
                format_alt = " $icon $device $frequency $bitrate $ip";
                interval = 5;
              }
            ] ++ (mapAttrsToList
              (vpn: meta: {
                block = "service_status";
                service = "openvpn-${vpn}";
                active_format = " ${meta.title} VPN ";
                inactive_format = " ${meta.title} VPN ";
                active_state = "Good";
                inactive_state = "Warning";
              })
              config.ext.networking.vpn.meta)
            ++ [
              ({
                block = "battery";
                format = " $icon $percentage $time $power ";
                missing_format = "";
              } // optionalAttrs config.services.upower.enable {
                driver = "upower";
              } // optionalAttrs (with config; services.upower.enable && attributes.hardware.dmiSystemVersion == "ThinkPad X270") {
                device = "DisplayDevice";
              } // optionalAttrs (!config.services.upower.enable) {
                driver = "sysfs";
                device = "BAT0";
              })
            ] ++ lib.optionals true mapAttrsToList
              (d: m: {
                block = "bluetooth";
                mac = m;
                format = " $icon ${d} ";
                disconnected_format = "";
              })
              config.ext.networking.wireless.bluetooth.devices
            ++ [
              {
                block = "sound";
                format = " $output_name{ $volume|} ";
                headphones_indicator = true;
                # FIXME: unhardcode mappings
                mappings = {
                  "alsa_output.pci-0000_00_1f.3.analog-stereo" = "🔈";
                  "alsa_output.pci-0000_00_1b.0.analog-stereo" = "🎧";
                  "bluez_sink.${macUnderscore config.ext.networking.wireless.bluetooth.defaultHeadset}.a2dp_sink" = "🎧";
                };
              }
              {
                block = "time";
                interval = 60;
                format = " $icon $timestamp.datetime(f:'%a %d-%m-%Y %R', l:ru_RU) ";
                timezone = config.time.timeZone;
              }
              {
                block = "keyboard_layout";
                driver = "kbddbus";
                mappings = {
                  "English (US)" = "en";
                  "Russian (N/A)" = "ru";
                };
              }
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
        ${pkgs.redis}/bin/redis-cli set wm/keybindings ${mkRedisJSON config.wmCommon.keybindings.entries}
        ${pkgs.redis}/bin/redis-cli set wm/modebindings ${mkRedisJSON config.wmCommon.modeBindings}
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
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.i3wm epkgs.i3wm-config-mode ];
      ide.emacs.core.config = builtins.readFile ./elisp/i3wm.el;
    })
  ];
}
