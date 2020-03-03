{ config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let cfg = config.custom.system;
in {
  options = {
    custom.system = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable various system tools.";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        # FIXME: use ideas from https://github.com/mitchweaver/bin/blob/5bad2e16006d82aeeb448f7185ce665934a9c242/util/pad
        srvctl = writePythonScriptWithPythonPackages "srvctl" [
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.libtmux
          pkgs.python3Packages.notify2
          pkgs.python3Packages.redis
          pkgs.python3Packages.xlib
        ] (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./srvctl.py; })));
        uptime_info = pkgs.writeScriptBin "uptime_info" (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./uptime_info.sh; })));
      };
      home-manager.users."${config.attributes.mainUser.name}" = {
        services.udiskie = {
          enable = true;
          automount = true;
          notify = true;
          tray = "never";
        };
        programs.htop = {
          enable = true;
          fields = [ "USER" "PRIORITY" "NICE" "M_SIZE" "STATE" "PERCENT_CPU" "PERCENT_MEM" "TIME" "COMM" ];
          meters.left = [ "AllCPUs" "Memory" ];
          colorScheme = 0;
          detailedCpuTime = true;
        };
        services.dunst = { # TODO: consider extracting options
          enable = true;
          settings = {
            global = {
              alignment = "left";
              always_run_script = "true";
              bounce_freq = 0;
              browser = "${pkgs.firefox-unwrapped}/bin/firefox -new-tab";
              dmenu = "${pkgs.dmenu}/bin/dmenu -p dunst:";
              ellipsize = "middle";
              follow = "keyboard";
              force_xinerama = "false";
              format = "<span foreground='#F3F4F5'><b>%s %p</b></span>\\n%b";
              frame_color = "#232323";
              frame_width = 3;
              geometry = "0x5-15+15";
              hide_duplicates_count = "false";
              history_length = 20;
              horizontal_padding = 10;
              icon_position = "left";
              idle_threshold = 120;
              ignore_newline = "no";
              indicate_hidden = "yes";
              line_height = 0;
              markup = "full";
              max_icon_size = 32;
              monitor = 0;
              notification_height = 0;
              padding = 10;
              separator_color = "frame";
              separator_height = 2;
              show_age_threshold = 60;
              show_indicators = "yes";
              shrink = "no";
              sort = "yes";
              stack_duplicates = "true";
              startup_notification = "false";
              sticky_history = "yes";
              transparency = 0;
              verbosity = "mesg";
              word_wrap = "yes";
            };
            shortcuts = {
              close = "ctrl+space";
              close_all = "ctrl+shift+space";
              history = "ctrl+grave";
              context = "ctrl+shift+period";
            };
            urgency_low = {
              background = "#232323";
              foreground = "#A8A8A8";
              timeout = 3;
            };
            urgency_normal = {
              background = "#285577";
              foreground = "#ffffff";
              timeout = 5;
            };
            urgency_critical = {
              background = "#D64E4E";
              foreground = "#F0E0E0";
              frame_color = "#D64E4E";
              timeout = 7;
            };
          };
        };
      };
      environment.systemPackages = with pkgs; [ srvctl ];
    })
    (mkIf (cfg.enable && cfg.xmonad.enable) {
      wm.xmonad.keybindings = {
        "M-C-j" = ''spawn "${pkgs.srvctl}/bin/srvctl"'';
        "M-S-u" = ''spawn "${pkgs.uptime_info}/bin/uptime_info"'';
      };
    })
  ];
}
