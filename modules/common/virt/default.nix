let
  deps = import ../../../nix/sources.nix;
  nixpkgs-pinned-05_12_19 = import deps.nixpkgs-pinned-05_12_19 { config.allowUnfree = true; };
in { config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.custom.virtualization;
  vdi2qcow2 = pkgs.writeShellScriptBin "vdi2qcow2" ''
    ${pkgs.qemu}/bin/qemu-img convert -f vdi -O qcow2 $1 "''${1%.*}.qcow2"
  '';
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
        # tigervnc
        nixpkgs-pinned-05_12_19.vagrant
      ];

      boot.kernel.sysctl = {
        "net.ipv4.ip_forward" = 1; # for VMs forwarding
      };
    })
    (mkIf (cfg.enable && cfg.virtualbox.enable) {
      virtualisation.virtualbox.host.enable = true;
      users.users."${config.attributes.mainUser.name}".extraGroups = [ "vboxusers" ];
    })
    (mkIf (cfg.enable && cfg.docker.enable && cfg.docker.aux.enable) {
      environment.systemPackages = with pkgs; [
        # docker-slim # TODO: make package https://github.com/docker-slim/docker-slim
        clair # https://werner-dijkerman.nl/2019/01/28/scanning-docker-images-with-coreos-clair/
        nsjail
        skopeo
      ];
    })

    (mkIf (cfg.enable && cfg.docker.enable) {
      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set virt/swarm_meta ${
          lib.strings.escapeNixString (builtins.toJSON cfg.docker.swarm.meta)
        }
      '';
      nixpkgs.config.packageOverrides = _: rec {
        dlint = writeShellScriptBinWithDeps "dlint" [ pkgs.docker ] (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./dlint.sh; })));
        hadolintd = writeShellScriptBinWithDeps "hadolintd" [ pkgs.docker ] (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./hadolintd.sh; })));
        docker_containers_traits = writePythonScriptWithPythonPackages "docker_containers_traits" [
          pkgs.docker
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.redis
          pkgs.xsel
          pkgs.yad
        ] (builtins.readFile (pkgs.substituteAll
          ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./docker_containers_traits.py; })));
        discover_containerized_services = writePythonScriptWithPythonPackages "discover_containerized_services" [
          pkgs.docker
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.notify2
        ] (builtins.readFile (pkgs.substituteAll
          ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./discover_containerized_services.py; })));
        docker_shell = writePythonScriptWithPythonPackages "docker_shell" [
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.libtmux
          pkgs.python3Packages.redis
        ] (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./docker_shell.py; })));
        docker_swarm_services_info = writePythonScriptWithPythonPackages "docker_swarm_services_info" [
          pkgs.docker
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.libtmux
          pkgs.python3Packages.notify2
          pkgs.python3Packages.redis
          pkgs.vpnctl
          pkgs.yad
        ] (builtins.readFile (pkgs.substituteAll
          ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./docker_swarm_services_info.py; })));
      };

      virtualisation.docker = {
        enable = true;
        storageDriver = cfg.docker.storageDriver;
      };

      users.users."${config.attributes.mainUser.name}".extraGroups = [ "docker" ];

      home-manager.users."${config.attributes.mainUser.name}" = {
        xdg.configFile."hadolint.yaml".text = builtins.toJSON {
          ignored = [ "DL3007" ];
          trustedRegistries = [ "docker.io" ];
        };
      };

      environment.systemPackages = with pkgs;
        [
          nixpkgs-pinned-05_12_19.arion
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

      users.users."${config.attributes.mainUser.name}".extraGroups = [ "libvirtd" ];

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
    })
    (mkIf (cfg.docker.enable && cfg.wm.enable) {
      wmCommon.keys = {
        "M-d t" = {
          cmd = "${pkgs.docker_containers_traits}/bin/docker_containers_traits";
          desktop = "shell";
        };
        "M-d s" = {
          cmd = "${pkgs.docker_shell}/bin/docker_shell";
          desktop = "shell";
        };
        "M-d i" = {
          cmd = "${pkgs.docker_swarm_services_info}/bin/docker_swarm_services_info";
          desktop = "shell";
        };
      };
    })
  ];
}
