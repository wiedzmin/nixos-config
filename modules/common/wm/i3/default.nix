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
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      wm.xmonad.enable = false;
      wm.stumpwm.enable = false;

      environment.pathsToLink = [ "/libexec" ]; # for i3blocks (later)

      services.xserver = {
        windowManager = { i3 = { enable = true; }; };
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
        xdg.configFile."i3/config".text = ''
          # i3 config file (v4)

          set $mod Mod4
          font ${config.wmCommon.fonts.default}

          floating_modifier Mod1
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

          bindsym $mod+s layout stacking
          bindsym $mod+w layout tabbed
          bindsym $mod+e layout toggle split

          bindsym $mod+Shift+space floating toggle

          bindsym $mod+t focus mode_toggle

          bindsym $mod+a focus parent
          bindsym $mod+d focus child

          set $ws1 1: 
          set $ws2 2: 
          set $ws3 3: 
          set $ws4 4: 
          set $ws5 5: 
          set $ws6 6: 

          bindsym $mod+1 workspace $ws1
          bindsym $mod+2 workspace $ws2
          bindsym $mod+3 workspace $ws3
          bindsym $mod+4 workspace $ws4
          bindsym $mod+5 workspace $ws5
          bindsym $mod+6 workspace $ws6
          bindsym $mod+7 workspace 7
          bindsym $mod+8 workspace 8
          bindsym $mod+9 workspace 9
          bindsym $mod+0 workspace 10

          bindsym $mod+Shift+1 move container to workspace $ws1
          bindsym $mod+Shift+2 move container to workspace $ws2
          bindsym $mod+Shift+3 move container to workspace $ws3
          bindsym $mod+Shift+4 move container to workspace $ws4
          bindsym $mod+Shift+5 move container to workspace $ws5
          bindsym $mod+Shift+6 move container to workspace $ws6
          bindsym $mod+Shift+7 move container to workspace 7
          bindsym $mod+Shift+8 move container to workspace 8
          bindsym $mod+Shift+9 move container to workspace 9
          bindsym $mod+Shift+0 move container to workspace 10
          #workspace 1 output HDMI1
          #workspace 2 output eDP1
          bindsym $mod+q exec "i3-msg reload"
          bindsym $mod+Shift+q restart

          # bindsym $mod+Shift+g exec "paswitch alsa_output.usb-Logitech_Logitech_USB_Headset-00.analog-stereo"
          # bindsym $mod+Shift+m exec "paswitch alsa_output.pci-0000_00_1f.3.analog-stereo"

          mode "resize" {
                  bindsym Left resize shrink width 10 px or 10 ppt
                  bindsym Down resize grow height 10 px or 10 ppt
                  bindsym Up resize shrink height 10 px or 10 ppt
                  bindsym Right resize grow width 10 px or 10 ppt
                  bindsym Return mode "default"
                  bindsym Escape mode "default"
          }
          bindsym $mod+r mode "resize"
          hide_edge_borders smart

          # assign [class="Emacs"] $ws1
          # assign [class="Google-chrome"] $ws2
          # assign [instance="Navigator"] $ws2
          # assign [instance="main-terminal"] $ws3
          # assign [class="Slack"] $ws4
          # assign [class="TelegramDesktop"] $ws5
          # assign [class="Evolution"] $ws6

          # autostart apps
          # exec --no-startup-id emacs
          # exec --no-startup-id qutebrowser -P default --class qb-default
          # exec --no-startup-id alacritty urxvt -name "main-terminal"
          # exec --no-startup-id slack
          # exec --no-startup-id telegram-desktop
          exec --no-startup-id ${pkgs.kbdctl}/bin/kbdctl

          bar {
              strip_workspace_numbers yes
              font ${config.wmCommon.fonts.statusbar}
              status_command i3status
          }
        '';
      };
    })
  ];
}
