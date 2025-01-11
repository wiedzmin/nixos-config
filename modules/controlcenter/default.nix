{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.controlcenter;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  inherit (config.wmCommon) prefix;
  yaml = pkgs.formats.yaml { };
  lncThemeModule = types.submodule {
    options = {
      foregroundNormal = mkOption {
        type = types.str;
        description = "Normal status text color";
        default = "#322";
      };
      foregroundCritical = mkOption {
        type = types.str;
        description = "Critical status text color";
        default = "#000";
      };
      backgroundNormal = mkOption {
        type = types.str;
        description = "Normal status background";
        default = "rgba(255, 255, 255, 0.5)";
      };
      backgroundCritical = mkOption {
        type = types.str;
        description = "Critical status background";
        default = "rgba(255, 0, 0, 0.5)";
      };
      foregroundNormalNC = mkOption {
        type = types.str;
        description = "Normal status text color for notification center";
        default = "#322";
      };
      foregroundCriticalNC = mkOption {
        type = types.str;
        description = "Critical status text color for notification center";
        default = "#000";
      };
      backgroundNormalNC = mkOption {
        type = types.str;
        description = "Normal status background for notification center";
        default = "rgba(255, 255, 255, 0.4)";
      };
      backgroundCriticalNC = mkOption {
        type = types.str;
        description = "Critical status background for notification center";
        default = "rgba(155, 0, 20, 0.5)";
      };
    };
  };
in
{
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
      lnc.theme = mkOption {
        type = lncThemeModule;
        description = "LNC theming CSS settings";
      };
      launcher = mkOption {
        type = types.enum [ "rofi" "gmrun" ];
        default = "rofi";
        description = "Launcher program to use";
      };
      commandsDebugLogRoot = mkOption {
        type = types.str;
        default = homePrefix user "wm-logs";
        description = "Path to store WM commands debug logs under";
      };
      substitutions = mkOption {
        type = types.attrs;
        visible = false; # NOTE: really?
        internal = true;
        default = {
          "lists.gnu.org" = "mail.gnu.org";
        };
        description = "Common system-wide text substitutions";
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
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: {
        ifconfless = mkPythonScriptWithDeps pkgs "ifconfless" (with pkgs; [ nettools nurpkgs.pystdlib xsel yad ])
          (builtins.readFile ./scripts/ifconfless.py);
      };
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ btop fastfetch inotify-info sysz ];
        services.udiskie = {
          enable = true;
          automount = true;
          notify = true;
          tray = "auto";
        };
        programs.htop = {
          enable = true;
          package = pkgs.htop-vim;
          settings = {
            color_scheme = 0;
            detailed_cpu_time = true;
          };
        };
        home.extraActivationPath = with pkgs; [ xkb-switch systemd ]; # NOTE: for `uncacheServices` below when using local binaries
        home.activation = {
          uncacheServices = {
            after = [ "linkGeneration" ];
            before = [ ];
            data = ''${nurpkgs.toolbox}/bin/services --invalidate-cache'';
          };
          ensureDebugLogsRoot = {
            after = [ "linkGeneration" ];
            before = [ ];
            data = "mkdir -p ${cfg.commandsDebugLogRoot}";
          };
        };
        programs.rofi = {
          enable = true;
          cycle = true;
          location = "top";
          xoffset = 0;
          yoffset = 0;
          extraConfig = {
            monitor = "-4";
            matching = "normal";
            tokenize = true;
            disable-history = false;
            threads = 0;
            window-format = "{w}   {c}   {t}";
          };
        };
      };
      services.udisks2.enable = true;
    })
    (mkIf (cfg.enable && cfg.notifications.backend == "dunst") {
      home-manager.users."${user}" = {
        services.dunst = {
          # TODO: consider extracting options
          enable = true;
          settings = {
            global = {
              alignment = "left";
              always_run_script = "true";
              bounce_freq = 0;
              browser = appCmdFull config.attributes.browser.default.traits;
              dmenu = "${nurpkgs.dmenu-ng}/bin/dmenu -p dunst:";
              ellipsize = "middle";
              follow = "keyboard";
              force_xinerama = "false";
              format = "<span foreground='#F3F4F5'><b>%s %p</b></span>\\n%b";
              frame_color = "#232323";
              frame_width = 3;
              geometry = "0x5-15+15";
              hide_duplicates_count = "false";
              history_length = 50;
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
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ deadd-notification-center ];
        # FIXME: parameterize font sizes and style/weight
        xdg.configFile."deadd/deadd.css".text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./assets/deadd.css ];
        xdg.configFile."deadd/deadd.yml".source = yaml.generate "deadd.yml" {
          notification-center = {
            margin-top = 0;
            margin-right = 0;
            margin-bottom = 0;
            width = 650;
            monitor = 0;
            follow-mouse = true;
            hide-on-mouse-leave = true;
            new-first = true;
            ignore-transient = false;
            buttons = {
              buttons-per-row = 5;
              button-height = 60;
              button-margin = 2;
            };
          };
          notification = {
            use-markup = true;
            parse-html-entities = true;
            dbus = {
              send-noti-closed = false;
            };
            app-icon = {
              # guess-icon-from-name = true;
              icon-size = 15;
            };
            image = {
              size = 25;
              # margin-top = 15;
              # margin-bottom = 15;
              # margin-left = 15;
              # margin-right = 0;
            };
            popup = {
              default-timeout = 10000;
              margin-top = 50;
              margin-right = 50;
              margin-between = 20;
              max-lines-in-body = 3;
              hide-body-if-empty = false;
              monitor = 0;
              follow-mouse = true;
              click-behavior = {
                dismiss = "mouse1";
                default-action = "mouse3";
              };
            };
          };
        };
        home.activation.stop_dunst = {
          after = [ "linkGeneration" ];
          before = [ ];
          data = "${config.systemd.package}/bin/systemctl --user stop dunst.service || exit 0";
        };
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
      wmCommon.keybindings.entries = [
        {
          key = [ "Control" "Shift" "Space" ];
          cmd = ''kill -s USR1 $(pidof deadd-notification-center)'';
          mode = "root";
        }
      ];
    })
    (mkIf (cfg.enable && cfg.launcher == "gmrun") {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ gmrun ];
        home.file = { ".gmrunrc".text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./assets/gmrunrc ]; };
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.entries = [
        {
          key = [ "j" ];
          cmd = ''${nurpkgs.toolbox}/bin/services --flat'';
          mode = "services";
        }
        {
          key = [ "Shift" "j" ];
          cmd = ''${nurpkgs.toolbox}/bin/services --dump-show-cmd --flat'';
          mode = "services";
        }
        {
          key = [ prefix "Shift" "u" ];
          cmd = "${pkgs.citron}/bin/citron uptime";
          mode = "root";
        }
      ] ++ optionals (cfg.launcher == "rofi") [
        {
          key = [ "XF86Launch1" ];
          cmd = ''"${pkgs.rofi}/bin/rofi -modi combi -show combi -combi-modi run,drun"'';
          mode = "root";
        }
        {
          key = [ prefix "Shift" "p" ];
          cmd = ''"${pkgs.rofi}/bin/rofi -modi combi -show combi -combi-modi run,drun"'';
          mode = "root";
        }
      ] ++ optionals cfg.networking.enable [{
        key = [ "i" ];
        cmd = "${pkgs.ifconfless}/bin/ifconfless";
        mode = "network";
      }] ++ optionals (cfg.launcher == "gmrun") [{
        key = [ prefix "Shift" "p" ];
        cmd = "${pkgs.gmrun}/bin/gmrun";
        mode = "root";
      }] ++ optionals (cfg.notifications.backend == "dunst") [
        {
          key = [ "Control" "space" ];
          cmd = "${pkgs.dunst}/bin/dunstctl close";
          mode = "root";
        }
        {
          key = [ "Control" "Shift" "space" ];
          cmd = "${pkgs.dunst}/bin/dunstctl close-all";
          mode = "root";
        }
        {
          key = [ "Control" "grave" ];
          cmd = "${pkgs.dunst}/bin/dunstctl history-pop";
          mode = "root";
        }
        {
          key = [ "Control" "Shift" "equal" ];
          cmd = "${pkgs.dunst}/bin/dunstctl context";
          mode = "root";
        }
      ];
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      completion.expansions.espanso.matches = {
        controlcenter = {
          matches = [
            {
              trigger = ":sctlf";
              replace = "systemctl --user --state=failed";
            }
            {
              trigger = ":sctrf";
              replace = "systemctl --user reset-failed";
            }
            {
              trigger = ":fatr";
              replace = "nix run nixpkgs.fatrace -c fatrace";
            }
          ];
        };
      };
    })
    (mkIf (cfg.enable && config.attributes.debug.exposeScripts) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ ifconfless ]; };
    })
  ];
}
