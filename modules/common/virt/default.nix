{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.custom.virtualization;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users.${user};
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  prefix = config.wmCommon.prefix;
  vdi2qcow2 = pkgs.writeShellScriptBin "vdi2qcow2" ''
    ${pkgs.qemu}/bin/qemu-img convert -f vdi -O qcow2 $1 "''${1%.*}.qcow2"
  '';
  configHome = hm.xdg.configHome;
in {
  options = {
    custom.virtualization = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable virtualization";
      };
      docker.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Docker";
      };
      docker.aux.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Docker auxillary packages";
      };
      docker.defaultContainerShell = mkOption {
        type = types.str;
        default = "/bin/bash";
        description = "Default shell to execute within container with `exec -it`";
      };
      docker.storageDriver = mkOption {
        type = types.str;
        default = "overlay2";
        description = "Docker storage driver";
      };
      docker.devdns.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Docker internal DNS service";
      };
      docker.devdns.restartPolicy = mkOption {
        type = types.str;
        default = "unless-stopped";
        description = "DNS service restarting policy. See respective Docker docs for reference.";
      };
      docker.devdns.port = mkOption {
        type = types.str;
        default = "5380";
        description = "Port to bind DNS service on.";
      };
      docker.devdns.containerName = mkOption {
        type = types.str;
        default = "docker_dns";
        description = "Containet name for docker DNS service.";
      };
      docker.devdns.autoStart = mkOption {
        default = false;
        type = types.bool;
        description = "Whether DNS service should be started automatically.";
      };
      docker.stacks.psCustomFormat = mkOption {
        type = types.str;
        default = "{{.Name}}   {{.Image}}   {{.Node}} {{.DesiredState}}   {{.CurrentState}}";
        description = "Custom columns formatting for `docker stack ps` output.";
      };
      docker.stacks.useCustomFormat = mkOption {
        type = types.bool;
        default = false;
        description = "Wheteher to use custom formatting for `docker stack ps` output.";
      };
      docker.stacks.showOnlyRunning = mkOption {
        type = types.bool;
        default = true;
        description = "Show only running services in stack.";
      };
      docker.swarm.meta = mkOption {
        type = types.attrs;
        default = { };
        description = "Swarm --> entrypoint host mappings.";
      };
      libvirt.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Libvirt";
      };
      virtualbox.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable VirtualBox";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
      staging.packages = mkOption {
        type = types.listOf types.package;
        default = [ ];
        description = "List of staging packages.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      environment.systemPackages = with pkgs; [
        nfs-utils # for vagrant
        inputs.nixpkgs-16_04_20.legacyPackages.x86_64-linux.vagrant
      ];

      boot.kernel.sysctl = {
        "net.ipv4.ip_forward" = 1; # for VMs forwarding
      };
    })
    (mkIf (cfg.enable && cfg.virtualbox.enable) {
      virtualisation.virtualbox.host.enable = true;
      users.users.${user}.extraGroups = [ "vboxusers" ];
    })
    (mkIf (cfg.enable && cfg.docker.enable && cfg.docker.aux.enable) {
      environment.systemPackages = with pkgs; [ docker-slim nsjail skopeo ];
    })

    (mkIf (cfg.enable && cfg.docker.enable) {
      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set virt/swarm_meta ${
          lib.strings.escapeNixString (builtins.toJSON cfg.docker.swarm.meta)
        }
        ${pkgs.redis}/bin/redis-cli expire virt/swarm_meta 604800
      '';
      nixpkgs.config.packageOverrides = _: rec {
        dlint = mkShellScriptWithDeps "dlint" (with pkgs; [ docker ]) (readSubstituted ../subst.nix ./scripts/dlint.sh);
        hadolintd = mkShellScriptWithDeps "hadolintd" (with pkgs; [ docker ])
          (readSubstituted ../subst.nix ./scripts/hadolintd.sh);
        docker_containers_traits = mkPythonScriptWithDeps "docker_containers_traits"
          (with pkgs; [ docker nurpkgs.pystdlib python3Packages.redis xsel yad ])
          (readSubstituted ../subst.nix ./scripts/docker_containers_traits.py);
        discover_containerized_services =
          mkPythonScriptWithDeps "discover_containerized_services" (with pkgs; [ docker nurpkgs.pystdlib ])
            (readSubstituted ../subst.nix ./scripts/discover_containerized_services.py);
        docker_shell =
          mkPythonScriptWithDeps "docker_shell" (with pkgs; [ nurpkgs.pystdlib python3Packages.libtmux python3Packages.redis ])
            (readSubstituted ../subst.nix ./scripts/docker_shell.py);
        docker_swarm_services_info = mkPythonScriptWithDeps "docker_swarm_services_info"
          (with pkgs; [ docker nurpkgs.pystdlib python3Packages.libtmux python3Packages.redis vpnctl yad ])
          (readSubstituted ../subst.nix ./scripts/docker_swarm_services_info.py);
      };

      virtualisation.docker = {
        enable = true;
        storageDriver = cfg.docker.storageDriver;
      };

      users.users.${user}.extraGroups = [ "docker" ];

      home-manager.users.${user} = {
        xdg.configFile."hadolint.yaml".text = builtins.toJSON {
          ignored = [ "DL3007" ];
          trustedRegistries = [ "docker.io" ];
        };
      };

      environment.variables.DOCKER_CONFIG = "${configHome}/docker";

      environment.systemPackages = with pkgs;
        [
          inputs.nixpkgs-16_04_20.legacyPackages.x86_64-linux.arion
          ctop
          discover_containerized_services
          dive
          dlint
          docker_compose
          docker_containers_traits
          docker_shell
          docker_swarm_services_info
          hadolintd
          libcgroup
          vdi2qcow2
        ] ++ lib.optionals (config.attributes.debug.enable) [ ];
    })

    (mkIf (cfg.enable && cfg.docker.enable && cfg.docker.devdns.enable) {
      systemd.services."docker-devdns" = {
        description = "Dev DNS for docker containers";
        wantedBy = [ (optionalString cfg.docker.devdns.autoStart "multi-user.target") ];
        after = [ "docker.service" "docker.socket" ];
        requires = [ "docker.service" "docker.socket" ];
        script = ''
          ${pkgs.docker}/bin/docker run \
                                    --restart=${cfg.docker.devdns.restartPolicy} \
                                    -p ${cfg.docker.devdns.port}:${cfg.docker.devdns.port} \
                                    --name=${cfg.docker.devdns.containerName} \
                                    -v /var/run/docker.sock:/var/run/docker.sock \
                                    -v /etc/resolv.conf:/etc/resolv.conf \
                                    defreitas/dns-proxy-server
        '';
        preStop = "${pkgs.docker}/bin/docker stop ${cfg.docker.devdns.containerName}";
        reload = "${pkgs.docker}/bin/docker restart ${cfg.docker.devdns.containerName}";
        serviceConfig = {
          ExecStartPre = "-${pkgs.docker}/bin/docker rm -f ${cfg.docker.devdns.containerName}";
          ExecStopPost = "-${pkgs.docker}/bin/docker rm -f ${cfg.docker.devdns.containerName}";
          TimeoutStartSec = 0;
          TimeoutStopSec = 120;
          Restart = "always";
        };
      };
    })

    (mkIf (cfg.enable && cfg.libvirt.enable) {
      virtualisation.libvirtd = { enable = true; };
      virtualisation.kvmgt.enable = true;

      users.users.${user}.extraGroups = [ "libvirtd" ];
      environment.sessionVariables.LIBVIRT_DEFAULT_URI = [ "qemu:///system" ];

      networking.nat.internalInterfaces = [ "virbr0" ];
      services.dnsmasq.extraConfig = ''
        except-interface=virbr0 # ignore virbr0 as libvirtd listens here
      '';

      boot.kernelParams = [ "kvm.allow_unsafe_assigned_interrupts=1" "kvm.ignore_msrs=1" "kvm-intel.nested=1" ];
      boot.kernelModules = [ "kvm-intel" ];
      boot.extraModprobeConfig = ''
        options kvm-intel nested=1
      '';

      # required for usb redirection to work
      security.wrappers.spice-client-glib-usb-acl-helper.source =
        "${pkgs.spice_gtk}/bin/spice-client-glib-usb-acl-helper";

      environment.systemPackages = with pkgs;
        [
          kvm
          libvirt # for `vagrant plugin install vagrant-libvirt`
          qemu-utils
          spice
          spice-gtk
          virtmanager
          virtviewer
        ] ++ lib.optionals (cfg.staging.packages != [ ]) cfg.staging.packages;
      wmCommon.wsMapping.rules = [{
        class = "Virt-manager";
        desktop = "tools";
      }];
    })
    (mkIf (cfg.docker.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ "t" ];
          cmd = "${pkgs.docker_containers_traits}/bin/docker_containers_traits";
          desktop = "shell";
          mode = "virt";
        }
        {
          key = [ "s" ];
          cmd = "${pkgs.docker_shell}/bin/docker_shell";
          desktop = "shell";
          mode = "virt";
        }
        {
          key = [ "i" ];
          cmd = "${pkgs.docker_swarm_services_info}/bin/docker_swarm_services_info";
          desktop = "shell";
          mode = "virt";
        }
      ];
      wmCommon.modeBindings.virt = [ prefix "d" ];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [
          discover_containerized_services
          dlint
          docker_containers_traits
          docker_shell
          docker_swarm_services_info
          hadolintd
          vdi2qcow2
        ];
      };
    })
  ];
}
