{ config, inputs, lib, pkgs, ... }:
with lib;

let
  cfg = config.ext.networking.core;
  user = config.attributes.mainUser.name;
in {
  options = {
    ext.networking.core = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable core networking customizations";
      };
      hostname = mkOption {
        type = types.str;
        default = config.attributes.machine.name;
        description = "Hostname";
      };
      hostId = mkOption {
        type = types.str;
        default = "";
        description = "Host ID";
      };
      nameservers = mkOption {
        type = types.listOf types.str;
        default = [
          "77.88.8.1"
          "77.88.8.8"
          "8.8.8.8"
        ];
        description = "DNS servers";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      boot.kernel.sysctl = {
        "net.ipv4.ip_default_ttl" = 65;
        "net.ipv4.tcp_sack" = 0;
      };

      networking = {
        hostName = cfg.hostname;
        hostId = cfg.hostId;
        enableIPv6 = false;
        firewall.enable = false;
        usePredictableInterfaceNames = lib.mkForce false; # TODO: consider extracting option
        resolvconf = {
          enable = true;
          dnsExtensionMechanism = false;
        };
        dhcpcd.denyInterfaces = [ "br*" ];
        nameservers = cfg.nameservers;
        networkmanager = {
          enable = true;
          unmanaged = [ "br0" "lo" ];
        };
      };
      home-manager.users.${user} = {
        xdg.configFile."espanso/user/networking.yml".text = ''
          name: networking
          parent: default

          matches:
            - trigger: ":ip"
              replace: "{{output}}"
              vars:
                - name: output
                  type: shell
                  params:
                    cmd: "curl 'https://api.ipify.org'"
        '';
      };
    })
  ];
}
