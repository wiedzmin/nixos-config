{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with import ../../wmutil.nix { inherit config inputs lib pkgs; };
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
  prefix = config.wmCommon.prefix;
in
{
  options = {
    wm.i3 = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable i3.";
      };
      containerLayout = mkOption {
        type = types.enum [ "default" "stacking" "tabbed" ];
        default = "tabbed";
        description = "Default container layout.";
      };
      ipcClients = mkOption {
        type = types.listOf types.str;
        default = [ "${pkgs.kbdctl}/bin/kbdctl" "${pkgs.i3-ratiosplit}/bin/ratiosplit" "${pkgs.i3-auto-layout}/bin/i3-auto-layout" ];
        description = "IPC clients to start along with i3";
      };
      settings = mkOption {
        type = types.lines;
        default = ''
          font ${config.wmCommon.fonts.default}
          floating_modifier ${prefix}
          hide_edge_borders smart
          workspace_layout ${cfg.containerLayout}

          mouse_warping output
          focus_follows_mouse no
        '';
        description = "Custom settings for i3.";
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
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = (!config.wm.xmonad.enable && !config.wm.stumpwm.enable);
        message = "i3: exactly one WM could be enabled.";
      }];

      environment.pathsToLink = optionals (cfg.statusbar.impl == "blocks") [ "/libexec" ];
      fonts.fonts = optionals (cfg.statusbar.impl == "blocks") (with pkgs; [ font-awesome ]);

      wmCommon = {
        enable = true;
        modeBindings = {
          # TODO: check if we can unwire this from i3
          "Passthrough Mode - Press M+F11 to exit" = [ prefix "F11" ];
          "browser" = [ prefix "b" ];
          "dev" = [ prefix "d" ];
          "layout" = [ prefix "<" ];
          "network" = [ prefix "n" ];
          "resize" = [ prefix "=" ];
          "run" = [ prefix "r" ];
          "select" = [ prefix "." ];
          "services" = [ prefix "s" ];
          "sound" = [ prefix "Home" ];
          "virt" = [ prefix "v" ];
          "window" = [ prefix "w" ];
          "scratchpad" = [ prefix "grave" ];
          "xserver" = [ prefix "x" ];
        };
        keys = [
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
            key = [ "b" ];
            cmd = "${pkgs.i3-balance-workspace}/bin/i3_balance_workspace --scope workspace";
            mode = "layout";
          }
          {
            key = [ "Shift" "b" ];
            cmd = "${pkgs.i3-balance-workspace}/bin/i3_balance_workspace --scope workspace";
            mode = "layout";
          }
          {
            key = [ prefix "F7" ];
            cmd = ''${pkgs.wmfocus}/bin/wmfocus --chars 123456789 --textcoloralt "#eeeeee"'';
            mode = "root";
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
        ];
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

      ide.emacs.core.environment = { CURRENT_WM = "i3"; };
      environment.sessionVariables.CURRENT_WM = [ "i3" ];

      nixpkgs.config.packageOverrides = _: rec {
        kbdctl = mkPythonScriptWithDeps "kbdctl"
          (with pkgs; [ nurpkgs.pystdlib python3Packages.i3ipc xdotool emacs xkb-switch ])
          (readSubstituted ../../subst.nix ./scripts/kbdctl.py);
      };

      workstation.systemtraits.instructions = ''
        ${pkgs.redis}/bin/redis-cli set wm/keybindings ${
          lib.strings.escapeNixString (builtins.toJSON config.wmCommon.keys)
        }
        ${pkgs.redis}/bin/redis-cli set wm/modebindings ${
          lib.strings.escapeNixString (builtins.toJSON config.wmCommon.modeBindings)
        }
      '';

      home-manager.users.${user} = {
        xdg.configFile = {
          "i3/ratiosplit.ini".text = lib.generators.toINI { } {
            main = {
              # NOTE: log levels are: off, error, warn, info, debug, trace
              ratio = 0.33;
              log_file_level = "info";
              log_file = "~/.local/share/i3/ratiosplit.log";
              log_console_level = "off";
            };
          };
          "i3/config".text = ''
            # i3 config file (v4)

            ${cfg.settings}
            ${bindkeysI3 config.wmCommon.keys config.wmCommon.modeBindings cfg.modeExitBindings
            config.wmCommon.workspaces}
            ${mkWorkspacesI3 config.wmCommon.workspaces prefix}

            ${with config.wmCommon; genPlacementRulesI3 wsMapping.rules workspaces}

            ${genWindowRulesFloatI3 config.wmCommon.wsMapping.rules}

            ${bindkeysFocusI3 config.wmCommon.wsMapping.rules}

            ${with config.wmCommon; genScratchpadSettingsI3 wsMapping.rules keys cfg.modeExitBindings workspaces}

            bindsym ${prefix}+Tab workspace back_and_forth

            ${lib.concatStringsSep "\n"
              (lib.forEach (config.wmCommon.autostart.entries ++ cfg.ipcClients) (e: "exec --no-startup-id ${e}"))}

            bar {
                tray_output ${config.attributes.hardware.monitors.internalHead.name}
                mode dock
                modifier ${prefix}
                workspace_buttons yes
                strip_workspace_numbers yes
                font ${config.wmCommon.fonts.statusbar}
                status_command ${statusBarImplToCmd.${cfg.statusbar.impl}}
            }
          '';
        } // optionalAttrs (cfg.statusbar.impl == "blocks") {
          # FIXME: currently broken
          # TODO: create derivation for accessing prebuilt C blocklets
          # TODO: tune kbdd_layout output (either patch packages or extract script)
          # TODO: ${inputs.i3blocks-contrib}/dunst/dunst - reimplement and unwire some meta (fonts, etc)
          # TODO: make homebrew script for openvpn/nm-vpn, refer to vpnctl
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
          "i3status-rust/config.toml".text = toToml {
            theme = "solarized-dark";
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
                format_mem = "{Mum}MB/{MTm}MB";
                format_swap = "{SUm}MB/{STm}MB";
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
                use_bits = false;
              }
              ({
                block = "battery";
                driver = "upower";
                format = " {percentage}% {time}";
              } // optionalAttrs (config.attributes.hardware.dmiSystemVersion == "ThinkPad X270") {
                device = "DisplayDevice";
              })
              {
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
          };
        };
      };
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = { home.packages = with pkgs; [ kbdctl ]; };
    })
  ];
}
