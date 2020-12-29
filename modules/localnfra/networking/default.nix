{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.custom.networking;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
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
      nixpkgs.config.packageOverrides = _: rec {
        wifi-status = mkShellScriptWithDeps "wifi-status" (with pkgs; [ gawk wirelesstools ])
          (readSubstituted ../../subst.nix ./scripts/wifi-status.sh);
        vpnctl = mkPythonScriptWithDeps "vpnctl" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis ])
          (readSubstituted ../../subst.nix ./scripts/vpnctl.py);
        ifconfless = mkPythonScriptWithDeps "ifconfless" (with pkgs; [ nettools nurpkgs.pystdlib xsel yad ])
          (readSubstituted ../../subst.nix ./scripts/ifconfless.py);
        sshmenu = mkPythonScriptWithDeps "sshmenu"
          (with pkgs; [ openssh nurpkgs.pystdlib python3Packages.libtmux python3Packages.redis vpnctl ])
          (readSubstituted ../../subst.nix ./scripts/sshmenu.py);
      };
      home-manager.users.${user} = {
        home.packages = with pkgs; [ vpnctl ]; # NOTE: for shell usage
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

      home-manager.users.${user}.programs.ssh.matchBlocks = mapAttrs' (hostname: meta:
        lib.nameValuePair hostname ({
          hostname = hostname;
          user = "${meta.user}";
          port = if (builtins.hasAttr "port" meta) then meta.port else null;
        })) (filterAttrs (_: meta: !((hasAttr "forge" meta) && meta.forge)) cfg.extraHosts.entries);

      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set net/extra_hosts ${
          lib.strings.escapeNixString (builtins.toJSON
            (filterAttrs (_: v: (!builtins.hasAttr "ssh" v) || ((builtins.hasAttr "ssh" v) && v.ssh == true))
              cfg.extraHosts.entries))
        }
        ${pkgs.redis}/bin/redis-cli set net/vpn_meta ${lib.strings.escapeNixString (builtins.toJSON cfg.vpnMeta)}
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

      home-manager.users."${user}" = { home.packages = with pkgs; [ jnettop ]; };

      users.extraUsers."${user}".extraGroups = [ "wireshark" ];
    })
    (mkIf (cfg.enable && cfg.clients.enable) {
      systemd.services.dhcpcd.serviceConfig.Type = lib.mkForce "simple"; # NOTE: forking is not acceptable for dhcpcd.
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ davfs2 gcalcli ];
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
          };
          extraConfig = ''
            AddKeysToAgent yes
          '';
        };
        home.file = { ".ssh/id_rsa.pub".text = config.identity.secrets.ssh.publicKey; };
      };
    })
    (mkIf (cfg.enable && cfg.messengers.enable) {
      services.quassel.enable = true;
      home-manager.users."${user}" = { home.packages = with pkgs; [ skype tdesktop quasselClient ]; };
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
      home-manager.users."${user}" = { home.packages = with pkgs; [ wpa_supplicant_gui ]; };
      wmCommon.keys = [
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
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ ifconfless sshmenu vpnctl wifi-status ]; };
    })
  ];
}
