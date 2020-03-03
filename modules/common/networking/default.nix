let
  deps = import ../../../nix/sources.nix;
  nixpkgs-pinned-05_12_19 = import deps.nixpkgs-pinned-05_12_19 { config.allowUnfree = true; };
in { config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let
  # TODO: (re)write dmenu-based custom scripts for pass based on https://github.com/carnager/rofi-pass/blob/master/rofi-pass
  cfg = config.custom.networking;
in {
  options = {
    custom.networking = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable networking support";
      };
      bluetooth.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable Bluetooth support";
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
      messengers.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to install messengers.";
      };
      scripts.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable custom scripts.";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
      extraHosts = mkOption {
        type = types.attrs;
        description = "Extra hosts.";
        default = { };
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set job/extra_hosts ${lib.strings.escapeNixString (builtins.toJSON cfg.extraHosts)}
        ${pkgs.redis}/bin/redis-cli set job/command_choices ${
          lib.strings.escapeNixString (builtins.toJSON config.attributes.dev.remoteCommands)
        }
      '';
      networking.extraHosts = ''
        127.0.0.1   ${config.networking.hostName}
        ${
          builtins.concatStringsSep "\n"
          (lib.mapAttrsToList (ip: hosts: ip + "    " + (builtins.concatStringsSep " " hosts)) cfg.extraHosts)
        }
      '';
      nixpkgs.config.packageOverrides = _: rec {
        wifi-status = pkgs.writeScriptBin "wifi-status" (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./wifi-status.sh; })));
        sshmenu = writePythonScriptWithPythonPackages "sshmenu" [
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.libtmux
          pkgs.python3Packages.redis
        ] (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./sshmenu.py; })));
      };
    })
    (mkIf (cfg.bluetooth.enable) {
      hardware = {
        bluetooth = {
          enable = true;
          powerOnBoot = false;
        };
      };
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
          nixpkgs-pinned-05_12_19.http-prompt
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
                text = config.identity.secrets.ssh.privateKey;
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
                text = config.custom.dev.secrets.github.ssh.privateKey;
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
                text = config.custom.dev.secrets.bitbucket.ssh.privateKey;
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
        home.file = { ".ssh/id_rsa.pub".text = config.identity.secrets.ssh.publicKey; };
      };
    })
    (mkIf (cfg.enable && cfg.remoteControlling.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ anydesk teamviewer ]; };
    })
    (mkIf (cfg.enable && cfg.messengers.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ skype slack tdesktop zoom-us ];
      };
    })
    (mkIf (cfg.enable && cfg.scripts.enable) { environment.systemPackages = with pkgs; [ wifi-status ]; })
    (mkIf (cfg.enable && cfg.xmonad.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ wpa_supplicant_gui ]; };
      wm.xmonad.keybindings = {
        "M-S-s" = ''spawn "${pkgs.sshmenu}/bin/sshmenu" >> showWSOnProperScreen "shell"'';
        "M-S-d" = ''spawn "${pkgs.sshmenu}/bin/sshmenu --choices" >> showWSOnProperScreen "shell"'';
        "M-M1-w" = ''spawn "${pkgs.wpa_supplicant_gui}/bin/wpa_gui"'';
        "M-M1-S-w" = ''spawn "tmux new-window ${pkgs.wpa_supplicant}/bin/wpa_cli" >> showWSOnProperScreen "shell"'';
        "M-s n <Up>" = ''spawn "${pkgs.systemd}/bin/systemctl restart nscd.service"'';
      };
    })
  ];
}
