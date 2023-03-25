{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
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
  yaml = pkgs.formats.yaml { };
in
{
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
    (mkIf cfg.enable {
      assertions = [{
        assertion = config.ext.networking.vpn.enable;
        message = "networking/ssh: must enable vpn functionality.";
      }];

      nixpkgs.config.packageOverrides = _: rec {
        sshmenu = mkPythonScriptWithDeps pkgs "sshmenu"
          (with pkgs; [ openssh nurpkgs.pystdlib python3Packages.libtmux python3Packages.redis nurpkgs.toolbox ])
          (builtins.readFile ./scripts/sshmenu.py);
      };
      services.openssh = {
        enable = true;
        allowSFTP = true;
        settings.X11Forwarding = false;
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
        home.packages = with pkgs; [ ssh-key-confirmer ];
        # TODO: automate, refer to https://github.com/benjojo/ssh-key-confirmer
        # TODO: also add nix/redis-level registry of used keypairs and marry them to extraHosts infrastructure
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.common = [
        {
          key = [ "d" ];
          cmd = "${pkgs.sshmenu}/bin/sshmenu --choices --term-command '${
            lib.concatStringsSep " " config.attributes.vt.default.cmd}'";
          mode = "network";
        }
        {
          key = [ "s" ];
          cmd = "${pkgs.sshmenu}/bin/sshmenu --term-command '${
            lib.concatStringsSep " " config.attributes.vt.default.cmd}'";
          mode = "network";
        }
        {
          key = [ "t" ];
          cmd = "${pkgs.sshmenu}/bin/sshmenu --ignore-tmux --term-command '${
            lib.concatStringsSep " " config.attributes.vt.default.cmd}'";
          mode = "network";
        }
      ];
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile."espanso/match/ssh.yml".source = yaml.generate "espanso-ssh.yml" {
          matches = [
            {
              trigger = ":ssf";
              replace = "ssh -L {{port}}:{{forwardfrom}}:{{port}} {{remoteuser}}@{{forwardto}}";
              vars = [
                {
                  name = "port";
                  type = "form";
                  params = { layout = "Forward port [[value]]"; };
                }
                {
                  name = "forwardfrom";
                  type = "form";
                  params = { layout = "Forward from IP [[value]]"; };
                }
                {
                  name = "forwardto";
                  type = "form";
                  params = { layout = "Forward to IP [[value]]"; };
                }
                {
                  name = "remoteuser";
                  type = "choice";
                  params = {
                    values = [ "root" user ];
                  };
                }
              ];
            }
          ];
        };
      };
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ sshmenu ]; };
    })
  ];
}
