{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

# TODO: implement profiles (refer to l2vpn module)
let
  cfg = config.custom.dev.ansible;
  user = config.attributes.mainUser.name;
in {
  options = {
    custom.dev.ansible = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable ansible";
      };
      inventory = mkOption {
        type = types.attrs;
        default = { };
        description = "Inventory entries";
      };
      remoteUser = mkOption {
        type = types.str;
        default = "root";
        description = "RTFM";
      };
      internalPollInterval = mkOption {
        type = types.str;
        default = "0.001"; # consider converting to float if available
        description = "RTFM";
      };
      forks = mkOption {
        type = types.int;
        default = 10;
        description = "RTFM";
      };
      ssh.arguments = mkOption {
        type = types.attrs;
        default = {
          ControlMaster = "auto";
          ControlPersist = "60s";
          PreferredAuthentications = "publickey";
        };
        description = "SSH client arguments";
      };
      ssh.pipelining = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to SSH pipelining";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      home-manager.users.${user} = {
        home.file = {
          "ansible.cfg".text = lib.generators.toINI { } {
            defaults = {
              inventory = "inventory.conf"; # TODO: add assertion (non-empty + semantically correct)
              remote_user = cfg.remoteUser;
              internal_poll_interval = cfg.internalPollInterval;
              forks = builtins.toString cfg.forks;
            };
            ssh_connection = {
              ssh_args = "-o ${lib.concatStringsSep " -o " (lib.mapAttrsToList (k: v: k + "=" + v) cfg.ssh.arguments)}";
              pipelining = if cfg.ssh.pipelining then "true" else "false";
            };
          };
          "inventory.conf".text = ''
            # TODO: review documentation with regard to inventory format(s)
          '';
        };
      };
    })
  ];
}
