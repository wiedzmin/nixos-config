{ config, lib, pkgs, ... }:
with import ../../../util.nix { inherit config lib pkgs; };
with import ../wmutil.nix { inherit config lib pkgs; };
with lib;

# FIXME: integrate into common WM infra after config stabilization
let cfg = config.wm.i3;
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
          floating_modifier ${cfg.prefix}
          hide_edge_borders smart
          workspace_layout stacked
        '';
        description = "Custom settings for i3.";
      };
      prefix = mkOption {
        type = types.str;
        default = "Mod4";
        description = "WM prefix key";
      };
      keys = mkOption {
        type = types.listOf types.attrs;
        default = [
          {
            key = "${cfg.prefix}+Shift+q";
            cmd = ''exec "i3-msg reload"'';
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+q";
            cmd = "restart";
            mode = "root";
            raw = true;
          }
          {
            key = "Control+backslash";
            cmd = "nop";
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+Left";
            cmd = "focus left";
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+Down";
            cmd = "focus down";
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+Up";
            cmd = "focus up";
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+Right";
            cmd = "focus right";
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+Shift+Left";
            cmd = "move left";
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+Shift+Down";
            cmd = "move down";
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+Shift+Up";
            cmd = "move up";
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+Shift+Right";
            cmd = "move right";
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+bar";
            cmd = "split h";
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+minus";
            cmd = "split v";
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+f";
            cmd = "fullscreen toggle";
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+Shift+f";
            cmd = "floating toggle";
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+t";
            cmd = "focus mode_toggle";
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+Shift+a";
            cmd = "focus parent";
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+Shift+d";
            cmd = "focus child";
            mode = "root";
            raw = true;
          }
          {
            key = "${cfg.prefix}+F12";
            cmd = "kill";
            mode = "root";
            raw = true;
          }
          {
            key = "s";
            cmd = ''layout stacking; mode "default"'';
            mode = "layout";
            raw = true;
          }
          {
            key = "t";
            cmd = ''layout tabbed; mode "default"'';
            mode = "layout";
            raw = true;
          }
          {
            key = "w";
            cmd = ''layout toggle split; mode "default"'';
            mode = "layout";
            raw = true;
          }
          {
            key = "Left";
            cmd = "resize shrink width 10 px or 10 ppt";
            mode = "resize";
            raw = true;
          }
          {
            key = "Down";
            cmd = "resize grow height 10 px or 10 ppt";
            mode = "resize";
            raw = true;
          }
          {
            key = "Up";
            cmd = "resize shrink height 10 px or 10 ppt";
            mode = "resize";
            raw = true;
          }
          {
            key = "Right";
            cmd = "resize grow width 10 px or 10 ppt";
            mode = "resize";
            raw = true;
          }
          {
            key = "Return";
            cmd = ''mode "default"'';
            mode = "resize";
            raw = true;
          }
          {
            key = "Escape";
            cmd = ''mode "default"'';
            mode = "resize";
            raw = true;
          }
          {
            key = "${cfg.prefix}+F11";
            cmd = ''mode "default"'';
            mode = "Passthrough Mode - Press M+F11 to exit";
            raw = true;
          }
        ];
        description = "i3-related keybindings.";
      };
      commonKeys = mkOption { # TODO: strip WM traits (i.e. exec, etc.) later
        type = types.listOf types.attrs;
        default = [
          {
            key = "${cfg.prefix}+Shift+Return";
            cmd = "alacritty";
            mode = "root";
          }
          {
            key = "${cfg.prefix}+Shift+p";
            cmd = "${dmenu_runapps}/bin/dmenu_runapps -fn '${config.wmCommon.fonts.dmenu}'";
            mode = "root";
          }
          {
            key = "t";
            cmd = ''${pkgs.tmuxp_sessions}/bin/tmuxp_sessions; mode "default"'';
            mode = "run";
          }
          {
            key = "d";
            cmd = ''${pkgs.dbms}/bin/dbms; workspace $ws_shell; mode "default"'';
            mode = "run";
          }
          {
            key = "a";
            cmd = ''${pkgs.autorandr_profiles}/bin/autorandr_profiles; mode "default"'';
            mode = "run";
          }
          {
            key = "c";
            cmd = ''${pkgs.systemd}/bin/systemctl --user restart compton.service; mode "default"'';
            mode = "run";
          }
          {
            key = "r";
            cmd = ''${pkgs.reposearch}/bin/reposearch; mode "default"'';
            mode = "run";
          }
          {
            key = "p";
            cmd = ''${pkgs.rofi-pass}/bin/rofi-pass; mode "default"'';
            mode = "run";
          }
          {
            key = "b";
            cmd = ''${pkgs.bookshelf}/bin/bookshelf; mode "default"'';
            mode = "run";
          }
          {
            key = "Shift-f";
            cmd = ''${pkgs.sshfsmenu}/bin/sshfsmenu --mode unmount; mode "default"'';
            mode = "network";
          }
          {
            key = "f";
            cmd = ''${pkgs.sshfsmenu}/bin/sshfsmenu --mode mount; mode "default"'';
            mode = "network";
          }
          {
            key = "i";
            cmd = ''${pkgs.ifconfless}/bin/ifconfless; mode "default"'';
            mode = "network";
          }
          {
            key = "d";
            cmd = ''${pkgs.sshmenu}/bin/sshmenu --choices; workspace $ws_shell; mode "default"'';
            mode = "network";
          }
          {
            key = "s";
            cmd = ''${pkgs.sshmenu}/bin/sshmenu; workspace $ws_shell; mode "default"'';
            mode = "network";
          }
          {
            key = "t";
            cmd = ''${pkgs.sshmenu}/bin/sshmenu --ignore-tmux; workspace $ws_shell; mode "default"'';
            mode = "network";
          }
          {
            key = "u";
            cmd = ''tmux new-window ${pkgs.networkmanager}/bin/nmtui; workspace $ws_shell; mode "default"'';
            mode = "network";
          }
          {
            key = "n";
            cmd = ''${pkgs.systemd}/bin/systemctl restart nscd.service; mode "default"'';
            mode = "services";
          }
          {
            key = "${config.custom.networking.secrets.vpn.mnemoChar}";
            cmd = ''${config.custom.networking.secrets.vpn.upCommand}; mode "default"'';
            mode = "services";
          }
          {
            key = "Shift-${config.custom.networking.secrets.vpn.mnemoChar}";
            cmd = ''${config.custom.networking.secrets.vpn.downCommand}; mode "default"'';
            mode = "services";
          }
          {
            key = "${config.job."14f7646bef".secrets.vpn.mnemoChar}";
            cmd = ''${config.job."14f7646bef".secrets.vpn.upCommand}; mode "default"'';
            mode = "services";
          }
          {
            key = "Shift-${config.job."14f7646bef".secrets.vpn.mnemoChar}";
            cmd = ''${config.job."14f7646bef".secrets.vpn.downCommand}; mode "default"'';
            mode = "services";
          }
          {
            key = "t";
            cmd =
              ''${pkgs.docker_containers_traits}/bin/docker_containers_traits; workspace $ws_shell; mode "default"'';
            mode = "virt";
          }
          {
            key = "s";
            cmd = ''${pkgs.docker_shell}/bin/docker_shell; workspace $ws_shell; mode "default"'';
            mode = "virt";
          }
          {
            key = "i";
            cmd = ''
              ${pkgs.docker_swarm_services_info}/bin/docker_swarm_services_info; workspace $ws_shell; mode "default"'';
            mode = "virt";
          }
          {
            key = "e";
            cmd = ''${pkgs.emacs}/bin/emacsclient -c -a emacs; mode "default"'';
            mode = "windows";
          }
          {
            key = "w";
            cmd = ''${dmenu_select_windows}/bin/dmenu_select_windows; mode "default"'';
            mode = "windows";
          }
          {
            key = "s";
            cmd = ''${pkgs.insert_snippet}/bin/insert_snippet; mode "default"'';
            mode = "windows";
          }
          {
            key = "XF86ScreenSaver";
            cmd = "${config.custom.security.lockScreenCommand}";
            mode = "root";
          }
          {
            key = "${cfg.prefix}+p";
            cmd = "${pkgs.lxqt.pavucontrol-qt}/bin/pavucontrol-qt";
            mode = "root";
          }
          {
            key = "${cfg.prefix}+Control+j";
            cmd = "${pkgs.srvctl}/bin/srvctl";
            mode = "root";
          }
          {
            key = "${cfg.prefix}+Shift+u";
            cmd = "${pkgs.uptime_info}/bin/uptime_info";
            mode = "root";
          }
          {
            key = "Print";
            cmd = "${pkgs.screenshot_active_window}/bin/screenshot_active_window";
            mode = "root";
          }
          {
            key = "Control+Print";
            cmd = "${pkgs.screenshot_full}/bin/screenshot_full";
            mode = "root";
          }
          {
            key = "${cfg.prefix}+Print";
            cmd = "${pkgs.screenshot_region}/bin/screenshot_region";
            mode = "root";
          }
          {
            key = "XF86MonBrightnessDown";
            cmd = "${pkgs.light}/bin/light -U ${toString config.custom.video.backlightDelta}";
            mode = "root";
          }
          {
            key = "XF86MonBrightnessUp";
            cmd = "${pkgs.light}/bin/light -A ${toString config.custom.video.backlightDelta}";
            mode = "root";
          }
          {
            key = "XF86AudioRaiseVolume";
            cmd = "${pkgs.playerctl}/bin/playerctl --all-players volume ${
                builtins.toString config.custom.sound.volume.deltaFraction
              }+";
            mode = "root";
          }
          {
            key = "XF86AudioLowerVolume";
            cmd = "${pkgs.playerctl}/bin/playerctl --all-players volume ${
                builtins.toString config.custom.sound.volume.deltaFraction
              }-";
            mode = "root";
          }
          {
            key = "XF86AudioPrev";
            cmd = "${pkgs.playerctl}/bin/playerctl --all-players previous";
            mode = "root";
          }
          {
            key = "XF86AudioPlay";
            cmd = "${pkgs.playerctl}/bin/playerctl --all-players play-pause";
            mode = "root";
          }
          {
            key = "XF86AudioNext";
            cmd = "${pkgs.playerctl}/bin/playerctl --all-players next";
            mode = "root";
          }
          {
            key = "XF86AudioMute";
            cmd = "${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
            mode = "root";
          }
          {
            key = "XF86AudioMicMute";
            cmd = "${pkgs.pulseaudio}/bin/pactl set-source-mute @DEFAULT_SOURCE@ toggle";
            mode = "root";
          }
          {
            key = "Control+XF86MonBrightnessDown";
            cmd = "${pkgs.light}/bin/light -S 20";
            mode = "root";
          }
          {
            key = "Control+XF86MonBrightnessUp";
            cmd = "${pkgs.light}/bin/light -S 100";
            mode = "root";
          }
          {
            key = "${cfg.prefix}+XF86AudioNext";
            cmd = "${pkgs.playerctl}/bin/playerctl --all-players position ${
                builtins.toString config.custom.content.players.deltaSeconds
              }+";
            mode = "root";
          }
          {
            key = "${cfg.prefix}+XF86AudioPrev";
            cmd = "${pkgs.playerctl}/bin/playerctl --all-players position ${
                builtins.toString config.custom.content.players.deltaSeconds
              }-";
            mode = "root";
          }
          {
            key = "${cfg.prefix}+slash";
            cmd = "${pkgs.search_selection}/bin/search_selection; workspace $ws_web";
            mode = "root";
          }
          {
            key = "${cfg.prefix}+Control+slash";
            cmd = "${pkgs.search_prompt}/bin/search_prompt; workspace $ws_web";
            mode = "root";
          }
          {
            key = "${cfg.prefix}+j";
            cmd = "${pkgs.webjumps}/bin/webjumps; workspace $ws_web";
            mode = "root";
          }
        ];
        description = "Common keybindings.";
      };
      modeBindings = mkOption {
        type = types.attrs;
        default = {
          "layout" = "${cfg.prefix}+less";
          "resize" = "${cfg.prefix}+z";
          "run" = "${cfg.prefix}+r";
          "network" = "${cfg.prefix}+n";
          "virt" = "${cfg.prefix}+v";
          "services" = "${cfg.prefix}+s";
          "windows" = "${cfg.prefix}+w";
          "Passthrough Mode - Press M+F11 to exit" = "${cfg.prefix}+F11";
        };
        description = "Modes keybindings.";
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
      wm.xmonad.enable = false;
      wm.stumpwm.enable = false;

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

      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = [ pkgs.kbdctl ];
        xdg.configFile = {
          # TODO: review and adopt https://github.com/guillaumecherel/i3-modal
          "i3/config".text = ''
            # i3 config file (v4)

            ${cfg.settings}
            ${mkKeysI3 (cfg.keys ++ cfg.commonKeys)}
            ${mkModeBindsI3 cfg.modeBindings}

            ${mkWorkspacesI3 config.wmCommon.workspaces.primary cfg.prefix}
            ${mkWorkspacesI3 config.wmCommon.workspaces.secondary cfg.prefix}
            ${mkWorkspacesI3 config.wmCommon.workspaces.tertiary cfg.prefix}

            ${lib.concatStringsSep "\n" (lib.forEach cfg.autostart.entries (e: "exec --no-startup-id ${e}"))}

            assign [class=".*athura.*"] $ws_read
            assign [class="^(Chromium-browser|qutebrowser)"] $ws_web
            assign [class="^(Gimp|aft-linux-qt)"] $ws_scratch
            assign [class="^(Skype|qutebrowser|Slack|TelegramDesktop|zoom|quassel)"] $ws_im
            assign [class="^(Soffice|calibre)"] $ws_read
            assign [class="^(Virt-manager|Virt-viewer|VirtualBox Manager)"] $ws_tools
            assign [class="^Alacritty"] $ws_shell
            assign [class="^Emacs"] $ws_work
            assign [class="^Xsane"] $ws_scan
            assign [class="^mpv"] $ws_media

            exec_always --no-startup-id ${pkgs.kbdctl}/bin/kbdctl

            bar {
                tray_output LVDS-1
                mode hide
                modifier ${cfg.prefix}
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
