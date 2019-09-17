{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.docker-devdns;
  containerName = "docker_dns";
in {
  options = {
    services.docker-devdns = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable dev DNS for Docker.
        '';
      };
      restartPolicy = mkOption {
        type = types.str;
        default = "unless-stopped";
        description = ''
          Service restarting policy. See respective Docker docs for reference.
        '';
      };
      dnsPort = mkOption {
        type = types.str;
        default = "5380";
        description = ''
          Port to bind service on.
        '';
      };
      autoStart = mkOption {
        default = true;
        type = types.bool;
        description = "Whether service should be started automatically.";
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services."docker-devdns" = {
      description = "Dev DNS for docker containers";
      wantedBy = optional cfg.autoStart [ "multi-user.target" ];
      after = [ "docker.service" "docker.socket" ];
      requires = [ "docker.service" "docker.socket" ];
      script = ''
        ${pkgs.docker}/bin/docker run \
                                  --restart=${cfg.restartPolicy} \
                                  -p ${cfg.dnsPort}:${cfg.dnsPort} \
                                  --name=${containerName} \
                                  -v /var/run/docker.sock:/var/run/docker.sock \
                                  -v /etc/resolv.conf:/etc/resolv.conf \
                                  defreitas/dns-proxy-server
      '';
      preStop = "${pkgs.docker}/bin/docker stop ${containerName}";
      reload = "${pkgs.docker}/bin/docker restart ${containerName}";
      serviceConfig = {
        ExecStartPre = "-${pkgs.docker}/bin/docker rm -f ${containerName}";
        ExecStopPost = "-${pkgs.docker}/bin/docker rm -f ${containerName}";
        TimeoutStartSec = 0;
        TimeoutStopSec = 120;
        Restart = "always";
      };
    };
  };
}
