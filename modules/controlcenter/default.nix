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
  notify-emacs-messages = mkShellScriptWithDeps "notify-emacs-messages" # TODO: integrate into notifications
    (with pkgs; [ emacs ]) ''
      APPNAME="$1"
      SUMMARY="$2"
      BODY="$3"
      ICON="$4"
      URGENCY="$5"
      emacsclient -n --eval "(message \"${APPNAME}/${SUMMARY}: $BODY\")"
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
      notifications.backend = mkOption {
        type = types.enum [ "dunst" "lnc" ];
        default = "dunst";
        description = "System notifications backend to use";
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
        # FIXME: bugs
        ifconfless = mkPythonScriptWithDeps "ifconfless" (with pkgs; [ nettools nurpkgs.pystdlib xsel yad ])
          (readSubstituted ../subst.nix ./scripts/ifconfless.py);
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
        home.activation.srvctl = {
          after = [ "linkGeneration" ];
          before = [ ];
          data = "DISPLAY=:0 ${pkgs.srvctl}/bin/srvctl --invalidate-cache";
        };
      };
      environment.systemPackages = with pkgs; [ srvctl ];
    })
    (mkIf (cfg.enable && cfg.notifications.backend == "dunst") {
      home-manager.users.${user} = {
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
        # NOTE: looks like service restart should be added to autorandr hook
        # (or something like this)
        home.activation.stop_lnc = {
          after = [ "linkGeneration" ];
          before = [ ];
          data = "${config.systemd.package}/bin/systemctl --user stop linux_notification_center.service || exit 0";
        };
      };
    })
    (mkIf (cfg.enable && cfg.notifications.backend == "lnc") {
      # NOTE: see https://github.com/phuhl/linux_notification_center for client tricks
      # TODO: https://github.com/Mesabloo/nix-config/blob/57d97b983803005778f265ac117ed7aacb03cd0d/modules/services/deadd.nix
      home-manager.users.${user} = {
        home.packages = with pkgs; [ deadd-notification-center ];
        # TODO: review https://github.com/phuhl/linux_notification_center/blob/62c8e42d3cd8e913320d20a5c18d17725d2ec72d/style.css
        xdg.configFile."deadd/deadd.css".text = "";
        xdg.configFile."deadd/deadd.conf".text = generators.toINI { } {
          notification-center = {
            hideOnMouseLeave = true;
            marginTop = 0;
            marginBottom = 0;
            marginRight = 0;
            width = 500;
            monitor = 0;
            followMouse = true;
            newFirst = true;
            useActionIcons = true;
            ignoreTransient = false;
            useMarkup = true;
            parseHtmlEntities = true;
            configSendNotiClosedDbusMessage = false;
            guessIconFromAppname = true;
          };
          notification-center-notification-popup = {
            notiDefaultTimeout = 10000;
            distanceTop = 50;
            distanceRight = 50;
            distanceBetween = 20;
            width = 500;
            monitor = 1;
            followMouse = true;
            iconSize = 20;
            maxImageSize = 100;
            imageMarginTop = 15;
            imageMarginBottom = 15;
            imageMarginLeft = 15;
            imageMarginRight = 0;
            shortenBody = 5;
            dismissButton = "mouse1";
            defaultActionButton = "mouse3";
          };
          buttons = {
            buttonsPerRow = 5;
            buttonHeight = 60;
            buttonMargin = 2;
          };
        };
        home.activation.stop_dunst = {
          after = [ "linkGeneration" ];
          before = [ ];
          data = "${config.systemd.package}/bin/systemctl --user stop dunst.service || exit 0";
        };
        # TODO: add keybinding for programmatic removal of all currently visible notifications (like in dunst)
      };
      systemd.user.services."linux_notification_center" = {
        description = "Deadd Notification Center";
        path = with pkgs; [ at-spi2-core glibc ];
        serviceConfig = {
          Environment = [ "DISPLAY=:0" "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus" ];
          PIDFile = "/run/notification-center.pid";
          Restart = "always";
          RestartSec = 10;
          ExecStart = "${pkgs.deadd-notification-center}/bin/deadd-notification-center";
          StandardOutput = "journal";
          StandardError = "journal";
        };
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
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
      home-manager.users.${user} = { home.packages = with pkgs; [ srvctl uptime_info ifconfless ]; };
    })
  ];
}
