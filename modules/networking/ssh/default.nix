{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ext.networking.ssh;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  sshModule = types.submodule {
    options = {
      private = mkOption {
        type = types.str;
        default = "";
        description = "SSH private key.";
      };
      public = mkOption {
        type = types.str;
        default = "";
        description = "SSH public key.";
      };
    };
  };

in {
  options = {
    ext.networking.ssh = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable SSH functionality";
      };
      keypair = mkOption {
        type = sshModule;
        description = "SSH keypair.";
      };
      authorizedKeys = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Authorized keys paths list";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      assertions = [{
        assertion = config.ext.networking.vpn.enable;
        message = "networking/ssh: must enable vpn functionality.";
      }];

      nixpkgs.config.packageOverrides = _: rec {
        sshmenu = mkPythonScriptWithDeps "sshmenu"
          (with pkgs; [ openssh nurpkgs.pystdlib python3Packages.libtmux python3Packages.redis vpnctl ])
          (readSubstituted ../../subst.nix ./scripts/sshmenu.py);
      };
      services.openssh = {
        enable = true;
        startWhenNeeded = true;
        authorizedKeysFiles = cfg.authorizedKeys;
      };
      programs.ssh.askPassword = "${pkgs.lxqt.lxqt-openssh-askpass}/bin/lxqt-openssh-askpass";
      home-manager.users."${user}" = {
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
                text = cfg.keypair.private;
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
        home.file = { ".ssh/id_rsa.pub".text = cfg.keypair.public; };
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
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
      ];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ sshmenu ]; };
    })
  ];
}
