{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.ext.virtualization.core;
  user = config.attributes.mainUser.name;
  vdi2qcow2 = pkgs.writeShellScriptBin "vdi2qcow2" ''
    ${pkgs.qemu}/bin/qemu-img convert -f vdi -O qcow2 $1 "''${1%.*}.qcow2"
  '';
in
{
  options = {
    ext.virtualization.core = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Libvirt";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      environment.systemPackages = with pkgs; [
        libvirt
        qemu-utils
        qemu_kvm
        quickemu # see https://github.com/quickemu-project/quickemu for details
        spice
        spice-gtk
        vdi2qcow2
        virt-manager
        virt-viewer
      ];

      boot = {
        kernel.sysctl = {
          "net.ipv4.ip_forward" = 1; # for VMs forwarding
        };
        kernelParams = [ "kvm.allow_unsafe_assigned_interrupts=1" "kvm.ignore_msrs=1" "kvm-intel.nested=1" ];
        kernelModules = [ "kvm-intel" ];
        extraModprobeConfig = ''
          options kvm-intel nested=1
        '';
      };

      virtualisation = {
        libvirtd.enable = true;
        kvmgt.enable = true;
        spiceUSBRedirection.enable = true;
      };

      networking = {
        nat.internalInterfaces = [ "virbr0" ];
        dhcpcd.denyInterfaces = [ "virbr*" ];
      };

      services.dnsmasq.settings = ''
        except-interface=virbr0 # ignore virbr0 as libvirtd listens here
      '';

      users.users."${user}".extraGroups = [ "libvirtd" ];
      shell.core.variables = [{ LIBVIRT_DEFAULT_URI = "qemu:///system"; global = true; }];

      wmCommon.wsMapping.rules = [{
        class = "Virt-manager";
        desktop = "tools"; # [ref:desktop_tools]
      }];
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      completion.expansions.espanso.matches = {
        libvirt = {
          matches = [
            {
              trigger = ":vdq";
              replace = "qemu-img convert -f vdi -O qcow2 {{image_basename.value}}.vdi {{image_basename.value}}.qcow2";
              vars = [
                {
                  name = "image_basename";
                  type = "form";
                  params = { layout = "image base name: {{value}}"; };
                }
              ];
            }
          ];
        } // optionalAttrs (config.shell.tmux.enable) {
          filter_title = "\".*${config.shell.tmux.defaultSession}.*${config.attributes.machine.name}.*\"";
        };
      };
    })
    (mkIf (cfg.enable && config.attributes.debug.exposeScripts) {
      home-manager.users."${user}" = {
        home.packages = [ vdi2qcow2 ];
      };
    })
  ];
}
