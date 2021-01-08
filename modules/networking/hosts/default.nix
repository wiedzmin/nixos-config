{ config, inputs, lib, pkgs, ... }:
with lib;

let
  cfg = config.ext.networking.hosts;
  user = config.attributes.mainUser.name;
  renderHosts = metadata:
    builtins.concatStringsSep "\n"
    (lib.mapAttrsToList (ip: meta: ip + "   " + (builtins.concatStringsSep " " (lib.forEach meta (x: x.host))))
      (lib.groupBy (x: x.ip) (lib.flatten (lib.mapAttrsToList (host: meta:
        (lib.forEach meta.ips (ip: {
          "ip" = ip;
          "host" = host;
        }))) metadata))));
in {
  options = {
    ext.networking.hosts = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable enriched metadata for extra network hosts";
      };
      entries = mkOption {
        type = types.attrs;
        description = "Extra hosts metadata.";
        default = { };
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = config.workstation.systemtraits.enable;
        message = "networking/hosts: must enable systemtraits maintainence.";
      }];

      systemd.services.dhcpcd.serviceConfig.Type = mkForce "simple"; # NOTE: forking is not acceptable for dhcpcd.

      networking.extraHosts = ''
        127.0.0.1   ${config.networking.hostName}
        ${renderHosts cfg.entries}
      '';

      home-manager.users.${user}.programs.ssh.matchBlocks = mapAttrs' (hostname: meta:
        nameValuePair hostname ({
          hostname = hostname;
          user = "${meta.user}";
          port = if (builtins.hasAttr "port" meta) then meta.port else null;
        })) (filterAttrs (_: meta: !((hasAttr "forge" meta) && meta.forge)) cfg.entries);

      workstation.systemtraits.instructions = ''
        ${pkgs.redis}/bin/redis-cli set net/extra_hosts ${
          strings.escapeNixString (builtins.toJSON
            (filterAttrs (_: v: (!builtins.hasAttr "ssh" v) || ((builtins.hasAttr "ssh" v) && v.ssh == true))
              cfg.entries))
        }
      '';
    })
  ];
}
