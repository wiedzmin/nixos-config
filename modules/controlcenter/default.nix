{ config, inputs, lib, pkgs, ... }:
with import ../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.controlcenter;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  prefix = config.wmCommon.prefix;
  dmenu_runapps = mkShellScriptWithDeps "dmenu_runapps"
    (with pkgs; [ coreutils nurpkgs.dmenu-ng haskellPackages.yeganesh j4-dmenu-desktop ]) ''
      j4-dmenu-desktop --display-binary --dmenu="(cat ; (stest -flx $(echo $PATH | tr : ' ') | sort -u)) | \
        yeganesh -- -i -l 15 -fn '${config.wmCommon.fonts.dmenu}'"
    '';
in {
  options = {
    controlcenter = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable various tools for system maintainence/monitoring/etc.";
      };
      networking.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable networking toolset";
      };
      gmrun.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable gmrun.
        '';
      };
      gmrun.historySize = mkOption {
        type = types.int;
        default = 1024;
        description = ''
          History length.
        '';
      };
      gmrun.terminalApps = mkOption {
        type = types.listOf types.str;
        default = [ "info" "lynx" "man" "mc" "ssh" "vi" "vim" ];
        description = ''
          List of apps to always run in terminal.
        '';
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        # FIXME: use ideas from https://github.com/mitchweaver/bin/blob/5bad2e16006d82aeeb448f7185ce665934a9c242/util/pad
        srvctl = mkPythonScriptWithDeps "srvctl" (with pkgs; [
          nurpkgs.pyfzf
          nurpkgs.pystdlib
          python3Packages.libtmux
          python3Packages.redis
          python3Packages.xlib
        ]) (readSubstituted ../subst.nix ./scripts/srvctl.py);
        uptime_info = mkPythonScriptWithDeps "uptime_info" (with pkgs; [ dunst gnused procps ])
          (readSubstituted ../subst.nix ./scripts/uptime_info.sh);
      };
      home-manager.users.${user} = {
        home.packages = with pkgs; [ dmenu_runapps j4-dmenu-desktop ];
        services.udiskie = {
          enable = true;
          automount = true;
          notify = true;
          tray = "auto";
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
              dmenu = "${nurpkgs.dmenu-ng}/bin/dmenu -p dunst:";
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
            urgency_low = { timeout = 3; };
            urgency_normal = { timeout = 5; };
            urgency_critical = { timeout = 7; };
          };
        };
        home.activation.srvctl = {
          after = [ "linkGeneration" ];
          before = [ ];
          data = "DISPLAY=:0 ${pkgs.srvctl}/bin/srvctl --invalidate-cache";
        };

      };
      environment.systemPackages = with pkgs; [ srvctl ];
    })
    (mkIf (cfg.enable && cfg.networking.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        # FIXME: bugs
        ifconfless = mkPythonScriptWithDeps "ifconfless" (with pkgs; [ nettools nurpkgs.pystdlib xsel yad ])
          (readSubstituted ../subst.nix ./scripts/ifconfless.py);
      };
    })
    (mkIf (cfg.enable && cfg.gmrun.enable) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ gmrun ];
        home.file = { ".gmrunrc".text = readSubstituted ../subst.nix ./assets/gmrunrc; };
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ "j" ];
          cmd = "${pkgs.srvctl}/bin/srvctl";
          mode = "services";
        }
        {
          key = [ prefix "Shift" "u" ];
          cmd = "${pkgs.uptime_info}/bin/uptime_info";
          mode = "root";
        }
        {
          key = [ "XF86Launch1" ];
          cmd = "${dmenu_runapps}/bin/dmenu_runapps";
          mode = "root";
        }
        {
          key = [ prefix "Shift" "p" ];
          cmd = "${dmenu_runapps}/bin/dmenu_runapps";
          mode = "root";
        }
      ] ++ optionals (cfg.networking.enable) [{
        key = [ "i" ];
        cmd = "${pkgs.ifconfless}/bin/ifconfless";
        mode = "network";
      }];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = { home.packages = with pkgs; [ srvctl uptime_info ]; };
    })
  ];
}
