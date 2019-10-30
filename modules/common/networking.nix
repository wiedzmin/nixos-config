{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.networking;
  ssh_custom_user = pkgs.writeScriptBin "ssh_custom_user" ''
    #! /usr/bin/env nix-shell
    #! nix-shell -i python3 -p python3 python3Packages.dmenu-python
    import os
    import sys

    import dmenu


    users = [
        "root",
        "alex3rd", # FIXME: parameterize back again
        "octocat"
    ]

    hosts = []
    with open("/etc/hosts", "r") as hostsfile:
        hosts = sorted(list(dict.fromkeys([entry.split()[1] for entry in hostsfile.read().split("\n") if len(entry)])))

    user = dmenu.show(sorted(users), prompt='user', lines=5)
    if not user:
        sys.exit(1)
    host = dmenu.show(sorted(hosts), prompt='host', lines=20)
    if not host:
        sys.exit(1)
    os.system('tmux new-window "et {0}@{1}"'.format(user, host))
  '';
  jnettop_hosts = pkgs.writeShellScriptBin "jnettop_hosts" ''
    main() {
        HOST=$( cat /etc/hosts | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.dmenu}/bin/dmenu -i -p "Host" -l 15)
        if [ -n "$HOST" ]; then
            enforce_job_vpn_up || exit 1
            ${pkgs.tmux}/bin/tmux new-window "${pkgs.eternal-terminal}/bin/et \
            $HOST -c 'jnettop'"
        fi
    }

    main

    exit 0
  '';
in {
  options = {
    custom.networking = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable networking support";
      };
      tools.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable network monitoring/debug tools";
      };
      clients.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable various network clients, mostly for development";
      };
      remoteControlling.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable clients to control remote machines.";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.tools.enable) {
      programs = {
        mtr.enable = true;
        wireshark = {
          enable = true;
          package = pkgs.wireshark-qt;
        };
        wavemon.enable = true;
      };

      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nload
          speedtest-cli
          nethogs
          jnettop
          bmon
          ngrep
          netsniff-ng
          pcapfix
          tcpreplay
          vnstat # TODO: also review corrsponding service
          hss
        ];
      };

      users.extraUsers."${config.attributes.mainUser.name}".extraGroups = [ "wireshark" ];
    })
    (mkIf (cfg.enable && cfg.clients.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          # curlie
          # davfs2
          # vegeta # TODO: package
          eternal-terminal
          gcalcli
          hasmail
          hpWorking.http-prompt
          httplab
          rclone
          socat
          tcpdump
          websocat
          weighttp
          wuzz
        ];
        programs.ssh = {
          enable = true;
          forwardAgent = true;
          userKnownHostsFile = "~/.ssh/known_hosts";
          controlMaster = "auto";
          controlPath = "~/.ssh/sockets/%r@%h:%p";
          controlPersist = "4h";
          serverAliveInterval = 30;
          matchBlocks = {
            "*" = {
              identityFile = toString (pkgs.writeTextFile {
                name = "id_rsa";
                text = config.secrets.network.ssh.privateKey;
              });
              compression = true;
            };
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.remoteControlling.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          anydesk
          teamviewer
        ];
      };
    })
    (mkIf (cfg.enable && cfg.xmonad.enable) {
      wm.xmonad.keybindings = {
        "M-S-d" = ''spawn "${ssh_custom_user}/bin/ssh_custom_user" >> showWSOnProperScreen "shell"'';
        "M-S-s" = ''spawn "${pkgs.rofi}/bin/rofi -show ssh" >> showWSOnProperScreen "shell"'';
        "M-M1-w" = ''spawn "${pkgs.wpa_supplicant_gui}/bin/wpa_gui"'';
        "M-M1-S-w" = ''spawn "tmux new-window ${pkgs.wpa_supplicant}/bin/wpa_cli" >> showWSOnProperScreen "shell"'';
        "M-s n <Up>" = ''spawn "${pkgs.systemd}/bin/systemctl restart nscd.service"'';
      };
      # https://github.com/menski/sshmenu/blob/master/sshmenu
    })
  ];
}
