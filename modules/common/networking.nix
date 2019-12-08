let
  deps = import ../../nix/sources.nix;
  hpPinned = import deps.nixpkgs-pinned-httpprompt { config.allowUnfree = true; };
in
{ config, lib, pkgs, ... }:
with import ../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.custom.networking;
  # TODO: (re)write dmenu-based custom scripts for pass based on https://github.com/carnager/rofi-pass/blob/master/rofi-pass
  sshmenu = writePythonScriptWithPythonPackages "sshmenu" [
    pkgs.python3Packages.dmenu-python
    pkgs.python3Packages.libtmux
    pkgs.python3Packages.redis
  ] ''
    import argparse
    import json
    import subprocess
    import sys

    import dmenu
    from libtmux import Server
    from libtmux.exc import LibTmuxException
    import redis


    parser = argparse.ArgumentParser(description="Execute command over SSH.")
    parser.add_argument("--choices", dest="show_choices", action="store_true",
                       default=False, help="show predefined command choices")

    args = parser.parse_args()

    r = redis.Redis(host='localhost', port=6379, db=0)

    extra_hosts_data = json.loads(r.get("job/extra_hosts"))
    extra_hosts = []
    for host in extra_hosts_data.values():
        extra_hosts.extend(host)

    host = dmenu.show(extra_hosts, prompt="ssh to",
                      case_insensitive=True, lines=10)

    if host:
        cmd = "${pkgs.openssh}/bin/ssh {0}".format(host)
        if args.show_choices:
            command_choices = json.loads(r.get("job/command_choices"))
            choice = dmenu.show(command_choices, prompt="execute", case_insensitive=True, lines=5)
            if choice:
               cmd += " -t '{0}'".format(choice)
            else:
               sys.exit(1)

        tmux_server = Server()
        try:
            tmux_session = tmux_server.find_where({ "session_name": "${config.attributes.tmux.defaultSession}" })
            ssh_window = tmux_session.new_window(attach=True, window_name=host,
                                                 window_shell=cmd)
        except LibTmuxException:
            pparams = ["${config.attributes.defaultCommands.terminal}", "-e"]
            pparams.extend(cmd.split())
            subprocess.Popen(pparams)
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
      extraHosts = mkOption {
        type = types.attrs;
        description = "Extra hosts.";
        default = {};
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      networking.extraHosts = ''
        127.0.0.1   ${config.networking.hostName}
        ${builtins.concatStringsSep "\n"
          (lib.mapAttrsToList (ip: hosts: ip + "    " + (builtins.concatStringsSep " " hosts))
            cfg.extraHosts)};
      '';
    })
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
      systemd.services.dhcpcd.serviceConfig.Type = lib.mkForce "simple"; # NOTE: forking is not acceptable for dhcpcd.
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          # curlie
          # davfs2
          # vegeta # TODO: package
          gcalcli
          hasmail
          hpPinned.http-prompt
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
          serverAliveInterval = 10;
          matchBlocks = {
            "localhost" = {
              extraOptions = {
                Compression = "no";
                ControlMaster = "no";
              };
            };
            "* !localhost" = {
              extraOptions = {
                ControlMaster = "auto";
                ControlPersist = "2h";
              };
            };
            "*" = {
              identityFile = toString (pkgs.writeTextFile {
                name = "id_rsa";
                text = config.secrets.identity.ssh.privateKey;
              });
              compression = true;
              extraOptions = {
                TCPKeepAlive = "yes";
                ServerAliveCountMax = "10";
              };
            };
            "github" = {
              hostname = "github.com";
              user = "git";
              serverAliveInterval = 60;
              identityFile = toString (pkgs.writeTextFile {
                name = "github_id_rsa";
                text = config.secrets.dev.github.ssh.privateKey;
              });
              extraOptions = {
                ControlMaster = "auto";
                ControlPersist = "yes";
                preferredAuthentications = "publickey";
              };
            };
            "bitbucket" = {
              hostname = "bitbucket.org";
              user = "git";
              serverAliveInterval = 60;
              identityFile = toString (pkgs.writeTextFile {
                name = "bitbucket_id_rsa";
                text = config.secrets.dev.bitbucket.ssh.privateKey;
              });
              identitiesOnly = true;
              extraOptions = {
                ControlMaster = "auto";
                ControlPersist = "yes";
                preferredAuthentications = "publickey";
              };
            };
          };
          extraConfig = ''
            AddKeysToAgent yes
          '';
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
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          wpa_supplicant_gui
        ];
      };
      wm.xmonad.keybindings = {
        "M-S-s" = ''spawn "${sshmenu}/bin/sshmenu" >> showWSOnProperScreen "shell"'';
        "M-S-d" = ''spawn "${sshmenu}/bin/sshmenu --choices" >> showWSOnProperScreen "shell"'';
        "M-M1-w" = ''spawn "${pkgs.wpa_supplicant_gui}/bin/wpa_gui"'';
        "M-M1-S-w" = ''spawn "tmux new-window ${pkgs.wpa_supplicant}/bin/wpa_cli" >> showWSOnProperScreen "shell"'';
        "M-s n <Up>" = ''spawn "${pkgs.systemd}/bin/systemctl restart nscd.service"'';
      };
    })
  ];
}
