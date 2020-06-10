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

          set $mod Mod4
          floating_modifier $mod
          hide_edge_borders smart

          workspace_layout stacked

          # gaps inner 5
          # gaps outer 5
          mode "Passthrough Mode - Press M+F11 to exit" {
            bindsym $mod+F11 mode "default"
          }
          bindsym $mod+F11 mode "Passthrough Mode - Press M+F11 to exit"
        '';
        description = "Custom settings for i3.";
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

            bindsym $mod+Shift+q exec "i3-msg reload"
            bindsym $mod+q restart
            ${cfg.settings}

            bindsym $mod+Shift+Return exec alacritty
            bindsym $mod+F12 kill
            bindsym $mod+Shift+p exec ${dmenu_runapps}/bin/dmenu_runapps -fn '${config.wmCommon.fonts.dmenu}'
            bindsym Control+backslash nop

            bindsym $mod+Left focus left
            bindsym $mod+Down focus down
            bindsym $mod+Up focus up
            bindsym $mod+Right focus right

            bindsym $mod+Shift+Left move left
            bindsym $mod+Shift+Down move down
            bindsym $mod+Shift+Up move up
            bindsym $mod+Shift+Right move right

            bindsym $mod+bar split h
            bindsym $mod+minus split v

            bindsym $mod+f fullscreen toggle
            bindsym $mod+Shift+f floating toggle

            bindsym $mod+t focus mode_toggle
            bindsym $mod+a focus parent
            bindsym $mod+d focus child

            mode "layout" {
              bindsym s layout stacking; mode "default"
              bindsym t layout tabbed; mode "default"
              bindsym w layout toggle split; mode "default"
            }
            bindsym $mod+less mode "layout"

            mode "resize" {
              bindsym Left resize shrink width 10 px or 10 ppt
              bindsym Down resize grow height 10 px or 10 ppt
              bindsym Up resize shrink height 10 px or 10 ppt
              bindsym Right resize grow width 10 px or 10 ppt
              bindsym Return mode "default"
              bindsym Escape mode "default"
            }
            bindsym $mod+r mode "resize"

            mode "run" {
              bindsym t exec --no-startup-id ${pkgs.tmuxp_sessions}/bin/tmuxp_sessions; mode "default"
              bindsym d exec --no-startup-id ${pkgs.dbms}/bin/dbms; workspace $ws_shell; mode "default"
              bindsym a exec --no-startup-id ${pkgs.autorandr_profiles}/bin/autorandr_profiles; mode "default"
              bindsym c exec --no-startup-id ${pkgs.systemd}/bin/systemctl --user restart compton.service; mode "default"
              bindsym r exec --no-startup-id ${pkgs.reposearch}/bin/reposearch; mode "default"
              bindsym p exec --no-startup-id ${pkgs.rofi-pass}/bin/rofi-pass; mode "default"
              bindsym b exec --no-startup-id ${pkgs.bookshelf}/bin/bookshelf; mode "default"
            }
            bindsym $mod+r mode "run"

            mode "network" {
              bindsym Shift-f exec --no-startup-id ${pkgs.sshfsmenu}/bin/sshfsmenu --mode unmount; mode "default"
              bindsym f exec --no-startup-id ${pkgs.sshfsmenu}/bin/sshfsmenu --mode mount; mode "default"
              bindsym i exec --no-startup-id ${pkgs.ifconfless}/bin/ifconfless; mode "default"
              bindsym d exec --no-startup-id ${pkgs.sshmenu}/bin/sshmenu --choices; workspace $ws_shell; mode "default"
              bindsym s exec --no-startup-id ${pkgs.sshmenu}/bin/sshmenu; workspace $ws_shell; mode "default"
              bindsym t exec --no-startup-id ${pkgs.sshmenu}/bin/sshmenu --ignore-tmux; workspace $ws_shell; mode "default"
              bindsym u exec --no-startup-id tmux new-window ${pkgs.networkmanager}/bin/nmtui; workspace $ws_shell; mode "default"
            }
            bindsym $mod+n mode "network"

            mode "services" {
              bindsym n exec --no-startup-id ${pkgs.systemd}/bin/systemctl restart nscd.service; mode "default"
              bindsym ${config.custom.networking.secrets.vpn.mnemoChar} exec --no-startup-id ${config.custom.networking.secrets.vpn.upCommand}; mode "default"
              bindsym Shift-${config.custom.networking.secrets.vpn.mnemoChar} exec --no-startup-id ${config.custom.networking.secrets.vpn.downCommand}; mode "default"
              bindsym ${config.job."14f7646bef".secrets.vpn.mnemoChar} exec --no-startup-id ${
                config.job."14f7646bef".secrets.vpn.upCommand
              }; mode "default"
              bindsym Shift-${config.job."14f7646bef".secrets.vpn.mnemoChar} exec --no-startup-id ${
                config.job."14f7646bef".secrets.vpn.downCommand
              }; mode "default"
            }
            bindsym $mod+s mode "services"

            mode "virt" {
              bindsym t exec --no-startup-id ${pkgs.docker_containers_traits}/bin/docker_containers_traits; workspace $ws_shell; mode "default"
              bindsym s exec --no-startup-id ${pkgs.docker_shell}/bin/docker_shell; workspace $ws_shell; mode "default"
              bindsym i exec --no-startup-id ${pkgs.docker_swarm_services_info}/bin/docker_swarm_services_info; workspace $ws_shell; mode "default"
            }
            bindsym $mod+v mode "virt"

            mode "windows" {
              bindsym e exec --no-startup-id ${pkgs.emacs}/bin/emacsclient -c -a emacs; mode "default"
              bindsym w exec --no-startup-id ${dmenu_select_windows}/bin/dmenu_select_windows; mode "default"
              bindsym s exec --no-startup-id ${pkgs.insert_snippet}/bin/insert_snippet; mode "default"
            }
            bindsym $mod+w mode "windows"

            bindsym XF86ScreenSaver exec --no-startup-id ${config.custom.security.lockScreenCommand}
            bindsym $mod+p exec --no-startup-id ${pkgs.lxqt.pavucontrol-qt}/bin/pavucontrol-qt
            bindsym $mod+Control+j exec --no-startup-id ${pkgs.srvctl}/bin/srvctl
            bindsym $mod+Shift+u exec --no-startup-id ${pkgs.uptime_info}/bin/uptime_info

            bindsym Print exec --no-startup-id ${pkgs.screenshot_active_window}/bin/screenshot_active_window
            bindsym Control+Print exec --no-startup-id ${pkgs.screenshot_full}/bin/screenshot_full
            bindsym $mod+Print exec --no-startup-id ${pkgs.screenshot_region}/bin/screenshot_region

            bindsym XF86MonBrightnessDown exec --no-startup-id ${pkgs.light}/bin/light -U ${
              toString config.custom.video.backlightDelta
            }
            bindsym XF86MonBrightnessUp exec --no-startup-id ${pkgs.light}/bin/light -A ${
              toString config.custom.video.backlightDelta
            }
            bindsym XF86AudioRaiseVolume exec --no-startup-id ${pkgs.playerctl}/bin/playerctl --all-players volume ${
              builtins.toString config.custom.sound.volume.deltaFraction
            }+
            bindsym XF86AudioLowerVolume exec --no-startup-id ${pkgs.playerctl}/bin/playerctl --all-players volume ${
              builtins.toString config.custom.sound.volume.deltaFraction
            }-
            bindsym XF86AudioPrev exec --no-startup-id ${pkgs.playerctl}/bin/playerctl --all-players previous
            bindsym XF86AudioPlay exec --no-startup-id ${pkgs.playerctl}/bin/playerctl --all-players play-pause
            bindsym XF86AudioNext exec --no-startup-id ${pkgs.playerctl}/bin/playerctl --all-players next
            bindsym XF86AudioMute exec --no-startup-id ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle
            bindsym XF86AudioMicMute exec --no-startup-id ${pkgs.pulseaudio}/bin/pactl set-source-mute @DEFAULT_SOURCE@ toggle

            bindsym Control+XF86MonBrightnessDown exec --no-startup-id ${pkgs.light}/bin/light -S 20
            bindsym Control+XF86MonBrightnessUp exec --no-startup-id ${pkgs.light}/bin/light -S 100
            bindsym $mod+XF86AudioNext exec --no-startup-id ${pkgs.playerctl}/bin/playerctl --all-players position ${
              builtins.toString config.custom.content.players.deltaSeconds
            }+
            bindsym $mod+XF86AudioPrev exec --no-startup-id ${pkgs.playerctl}/bin/playerctl --all-players position ${
              builtins.toString config.custom.content.players.deltaSeconds
            }-

            bindsym $mod+slash exec --no-startup-id ${pkgs.search_selection}/bin/search_selection; workspace $ws_web
            bindsym $mod+Control+slash exec --no-startup-id ${pkgs.search_prompt}/bin/search_prompt; workspace $ws_web
            bindsym $mod+j exec --no-startup-id ${pkgs.webjumps}/bin/webjumps; workspace $ws_web

            ${mkWorkspacesI3 config.wmCommon.workspaces.primary}
            ${mkWorkspacesI3 config.wmCommon.workspaces.secondary}
            ${mkWorkspacesI3 config.wmCommon.workspaces.tertiary}

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

            ${lib.concatStringsSep "\n" (lib.forEach cfg.autostart.entries (e: "exec --no-startup-id ${e}"))}

            exec_always --no-startup-id ${pkgs.kbdctl}/bin/kbdctl

            bar {
                tray_output LVDS-1
                mode hide
                modifier $mod
                workspace_buttons yes
                strip_workspace_numbers yes
                font ${config.wmCommon.fonts.statusbar}
                status_command py3status
            }
          '';
          "i3status/config".text = ''
            general {
              colors = true
              color_good = "#BBBBBB"
              color_bad = "#CC1616"
              color_degraded = "#55858E"
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
