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
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = config.ext.networking.vpn.enable;
        message = "networking/ssh: must enable vpn functionality.";
      }];

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
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      completion.expansions.espanso.matches = {
        ssh = {
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
  ];
}
