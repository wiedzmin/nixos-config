{ config, lib, pkgs, ... }:
with import ../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.custom.system;
  srvctl = writePythonScriptWithPythonPackages "srvctl" [
    pkgs.python3Packages.dmenu-python
    pkgs.python3Packages.libtmux
    pkgs.python3Packages.notify2
    pkgs.python3Packages.redis
    pkgs.python3Packages.xlib
  ] ''
    import os
    import subprocess
    import sys

    from notify2 import URGENCY_NORMAL, URGENCY_CRITICAL
    import dmenu
    import libtmux
    import notify2
    import redis

    services = []

    operations = [
        "stop",
        "restart",
        "journal",
        "status",
    ]

    ${config.custom.dev.pythonLib}

    notify2.init("srvctl")
    r = redis.Redis(host='localhost', port=6379, db=0)

    services = r.lrange("system/services", 0, -1)

    if not services:
        system_units_task = subprocess.Popen("systemctl list-unit-files", shell=True, stdout=subprocess.PIPE)
        services.extend(["{0} [system]".format(unit.split()[0].split(".")[0])
                         for unit in system_units_task.stdout.read().decode().split("\n")[1:-3]
                         if unit.split()[0].endswith("service")])
        assert system_units_task.wait() == 0

        user_units_task = subprocess.Popen("systemctl --user list-unit-files", shell=True, stdout=subprocess.PIPE)
        services.extend(["{0} [user]".format(unit.split()[0].split(".")[0])
                         for unit in user_units_task.stdout.read().decode().split("\n")[1:-3]
                         if unit.split()[0].endswith("service")])
        assert system_units_task.wait() == 0

        r.lpush("system/services", *services)

    service = dmenu.show(sorted(list(dict.fromkeys([service.decode() for service in services]))), prompt='service', lines=20)
    if not service:
        sys.exit(1)
    operation = dmenu.show(operations, prompt='> ', lines=5)
    if not operation:
        sys.exit(1)
    if operation == "stop":
        os.system("systemctl {0}stop {1}".format("--user " if "user" in service else "", service.split()[0]))
        n = notify2.Notification("[pkgsctl]", "Stopped {0}".format(service))
        n.set_urgency(URGENCY_CRITICAL)
        n.set_timeout(5000)
        n.show()
    elif operation == "restart":
        os.system("systemctl {0}restart {1}".format("--user " if "user" in service else "", service.split()[0]))
        n = notify2.Notification("[pkgsctl]", "Restarted {0}".format(service))
        n.set_urgency(URGENCY_NORMAL)
        n.set_timeout(5000)
        n.show()
    elif operation == "status":
        tmux_server = libtmux.Server()
        tmux_session = tmux_server.find_where({ "session_name": "${config.attributes.tmux.defaultSession}" })
        status_window = tmux_session.new_window(attach=True, window_name="status for {0}".format(service),
                                                     window_shell="sh -c 'systemctl {0} status {1}; read'".format("--user " if "user" in service else "",
                                                                                                    service.split()[0]))
        switch_desktop(1)
    else:
        tmux_server = libtmux.Server()
        tmux_session = tmux_server.find_where({ "session_name": "${config.attributes.tmux.defaultSession}" })
        journal_window = tmux_session.new_window(attach=True, window_name="journal for {0}".format(service),
                                                      window_shell="sh -c 'journalctl {0}-u {1}; read'".format("--user " if "user" in service else "",
                                                                                                 service.split()[0]))
        switch_desktop(1)
  '';
  uptime_info = pkgs.writeScriptBin "uptime_info" ''
    ${pkgs.dunst}/bin/dunstify -t 7000 "Uptime: $(${pkgs.procps}/bin/w | \
    ${pkgs.gnused}/bin/sed -r '1 s/.*up *(.*),.*user.*/\1/g;q')"
  '';
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
      home-manager.users."${config.attributes.mainUser.name}" = {
        services.udiskie = {
          enable = true;
          automount = true;
          notify = true;
          tray = "never";
        };
        programs.htop = {
          enable = true;
          fields = [
            "USER"
            "PRIORITY"
            "NICE"
            "M_SIZE"
            "STATE"
            "PERCENT_CPU"
            "PERCENT_MEM"
            "TIME"
            "COMM"
          ];
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
        # without it we may not be able to see new or unsee removed services
        home.activation.removeServicesFromRedis = {
          after = ["linkGeneration"];
          before = [];
          data = "${pkgs.redis}/bin/redis-cli del system/services";
        };
      };
      environment.systemPackages = with pkgs; [
        srvctl
      ];
    })
    (mkIf cfg.xmonad.enable {
      wm.xmonad.keybindings = {
        "M-C-j" = ''spawn "${srvctl}/bin/srvctl"'';
        "M-S-u" = ''spawn "${uptime_info}/bin/uptime_info"'';
      };
    })
  ];
}
