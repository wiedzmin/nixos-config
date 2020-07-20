let
  deps = import ../../../nix/sources.nix;
  nixpkgs-pinned-16_04_20 = import deps.nixpkgs-pinned-16_04_20 { config.allowUnfree = true; };
in { config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let cfg = config.custom.networking;
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
      sshfs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable SSHFS infra";
      };
      sshfs.entries = mkOption {
        type = types.attrs;
        default = { };
        description = "SSHFS mappings";
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
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
      extraHosts.enable = mkOption {
        type = types.bool;
        description = "Whether to enable extra hosts.";
        default = false;
      };
      extraHosts.entries = mkOption {
        type = types.attrs;
        description = "Extra hosts metadata.";
        default = { };
      };
      vpnMeta = mkOption {
        type = types.attrs;
        description = "VPN metadata.";
        default = { };
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set net/command_choices ${
          lib.strings.escapeNixString (builtins.toJSON config.custom.dev.remote.commands)
        }
      '';
      programs.nm-applet.enable = config.wm.i3.enable;

      nixpkgs.config.packageOverrides = _: rec {
        wifi-status = mkShellScriptWithDeps "wifi-status" (with pkgs; [ gawk wirelesstools ]) (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./wifi-status.sh; })));
        vpnctl = mkPythonScriptWithDeps "vpnctl" (with pkgs; [ pystdlib python3Packages.redis ]) (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./vpnctl.py; })));
        ifconfless = mkPythonScriptWithDeps "ifconfless" (with pkgs; [ nettools pystdlib xsel yad ]) (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./ifconfless.py; })));
        sshmenu = mkPythonScriptWithDeps "sshmenu"
          (with pkgs; [ openssh pystdlib python3Packages.libtmux python3Packages.redis vpnctl ]) (builtins.readFile
            (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./sshmenu.py; })));
        sshfsmenu = mkPythonScriptWithDeps "sshfsmenu" (with pkgs; [ pystdlib python3Packages.redis sshfs-fuse ])
          (builtins.readFile
            (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./sshfsmenu.py; })));
      };
      services.openssh = {
        enable = true;
        startWhenNeeded = true;
        authorizedKeysFiles = [ (secretsPrefix "identity/id_rsa.mobile.pub") ];
      };
    })
    (mkIf (cfg.enable && cfg.extraHosts.enable) {
      networking.extraHosts = ''
        127.0.0.1   ${config.networking.hostName}
        ${renderHosts cfg.extraHosts.entries}
      '';

      home-manager.users."${config.attributes.mainUser.name}".programs.ssh.matchBlocks = lib.mapAttrs' (_: meta:
        lib.nameValuePair (builtins.head meta.hostnames) {
          hostname = "${builtins.head meta.hostnames}";
          user = "${meta.user}";
        }) cfg.extraHosts.entries;

      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set net/extra_hosts ${
          lib.strings.escapeNixString (builtins.toJSON (lib.mapAttrs (_: meta: builtins.head meta.hostnames)
            (filterAttrs (_: v: (!builtins.hasAttr "ssh" v) || ((builtins.hasAttr "ssh" v) && v.ssh == true))
              cfg.extraHosts.entries)))
        }
        ${pkgs.redis}/bin/redis-cli set tmux/extra_hosts ${
          lib.strings.escapeNixString (builtins.toJSON (lib.mapAttrs' (_: meta:
            lib.nameValuePair (builtins.head meta.hostnames)
            (if lib.hasAttrByPath [ "tmux" ] meta then meta.tmux else config.custom.shell.tmux.defaultSession))
            cfg.extraHosts.entries))
        }
        ${pkgs.redis}/bin/redis-cli set net/vpn_meta ${lib.strings.escapeNixString (builtins.toJSON cfg.vpnMeta)}
        ${pkgs.redis}/bin/redis-cli set net/hosts_vpn ${
          lib.strings.escapeNixString (builtins.toJSON (lib.mapAttrs' (_: meta:
            lib.nameValuePair (builtins.head meta.hostnames)
            (if lib.hasAttrByPath [ "vpn" ] meta then meta.vpn else "")) cfg.extraHosts.entries))
        }
      '';
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

      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ jnettop ]; };

      users.extraUsers."${config.attributes.mainUser.name}".extraGroups = [ "wireshark" ];
    })
    (mkIf (cfg.enable && cfg.clients.enable) {
      systemd.services.dhcpcd.serviceConfig.Type = lib.mkForce "simple"; # NOTE: forking is not acceptable for dhcpcd.
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          # curlie
          # davfs2
          gcalcli
          hasmail
          nixpkgs-pinned-16_04_20.http-prompt
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
                name = "ssh_private_key";
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
                name = "ssh_private_key";
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
                name = "ssh_private_key";
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
    (mkIf (cfg.enable && cfg.sshfs.enable) {
      custom.navigation.workspaceRoots = { sshfs = "workspace/ssh"; };
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ sshfsmenu ]; };
      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set net/sshfs_map ${lib.strings.escapeNixString (builtins.toJSON cfg.sshfs.entries)}
      '';
      wmCommon.keys = [
        {
          key = [ "Shift" "f" ];
          cmd = "${pkgs.sshfsmenu}/bin/sshfsmenu --mode unmount";
          mode = "network";
        }
        {
          key = [ "f" ];
          cmd = "${pkgs.sshfsmenu}/bin/sshfsmenu --mode mount";
          mode = "network";
        }
      ];
    })
    (mkIf (cfg.enable && cfg.remoteControlling.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ anydesk teamviewer ]; };
    })
    (mkIf (cfg.enable && cfg.messengers.enable) {
      services.quassel.enable = true;
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ skype slack tdesktop quasselClient ];
      };
      custom.xinput.xkeysnail.rc = ''
        define_keymap(re.compile("TelegramDesktop"), {
            K("C-x"): {
                K("C-c"): K("C-q"),
            },
            K("C-s"): K("Esc"),
            K("C-t"): [K("Shift-Left"), K("C-x"), K("Left"), K("C-v"), K("Right")],
        }, "Telegram")

        define_keymap(re.compile("Slack"), {
            K("C-y"): K("C-v"),
        }, "Slack")
      '';
    })
    (mkIf (cfg.enable && cfg.scripts.enable) { environment.systemPackages = with pkgs; [ wifi-status ]; })
    (mkIf (cfg.enable && cfg.wm.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ wpa_supplicant_gui ]; };
      wmCommon.keys = [
        {
          key = [ "u" ];
          cmd = "tmux new-window ${pkgs.networkmanager}/bin/nmtui";
          desktop = "shell";
          mode = "network";
        }
        {
          key = [ "i" ];
          cmd = "${pkgs.ifconfless}/bin/ifconfless";
          mode = "network";
        }
        {
          key = [ "v" ];
          cmd = "${pkgs.vpnctl}/bin/vpnctl --status";
          mode = "network";
        }
        {
          key = [ "n" ];
          cmd = "${pkgs.systemd}/bin/systemctl restart nscd.service";
          mode = "service";
        }
        {
          key = [ "d" ];
          cmd = "${pkgs.sshmenu}/bin/sshmenu --choices";
          desktop = "shell";
          mode = "network";
        }
        {
          key = [ "s" ];
          cmd = "${pkgs.sshmenu}/bin/sshmenu";
          desktop = "shell";
          mode = "network";
        }
        {
          key = [ "t" ];
          cmd = "${pkgs.sshmenu}/bin/sshmenu --ignore-tmux";
          desktop = "shell";
          mode = "network";
        }
        # { key = [ "w" ]; # FIXME: make conditional
        #   cmd = "tmux new-window ${pkgs.wpa_supplicant}/bin/wpa_cli"; desktop = "shell"; mode = "network"; }
      ];
    })
  ];
}
