{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.ext.virtualization.libvirt;
  user = config.attributes.mainUser.name;
  vdi2qcow2 = pkgs.writeShellScriptBin "vdi2qcow2" ''
    ${pkgs.qemu}/bin/qemu-img convert -f vdi -O qcow2 $1 "''${1%.*}.qcow2"
  '';
in
{
  options = {
    ext.virtualization.libvirt = {
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
        kvm
        libvirt # for `vagrant plugin install vagrant-libvirt`
        nfs-utils # for vagrant
        qemu-utils
        spice
        spice-gtk
        vagrant
        vdi2qcow2
        virtmanager
        virtviewer
      ];

      boot.kernel.sysctl = {
        "net.ipv4.ip_forward" = 1; # for VMs forwarding
      };

      virtualisation.libvirtd = { enable = true; };
      virtualisation.kvmgt.enable = true;

      users.users."${user}".extraGroups = [ "libvirtd" ];
      shell.core.variables = [{ LIBVIRT_DEFAULT_URI = "qemu:///system"; global = true; }];

      networking.nat.internalInterfaces = [ "virbr0" ];
      services.dnsmasq.extraConfig = ''
        except-interface=virbr0 # ignore virbr0 as libvirtd listens here
      '';

      networking.dhcpcd.denyInterfaces = [ "virbr*" ];

      boot.kernelParams = [ "kvm.allow_unsafe_assigned_interrupts=1" "kvm.ignore_msrs=1" "kvm-intel.nested=1" ];
      boot.kernelModules = [ "kvm-intel" ];
      boot.extraModprobeConfig = ''
        options kvm-intel nested=1
      '';

      virtualisation.spiceUSBRedirection.enable = true;

      wmCommon.wsMapping.rules = [{
        class = "Virt-manager";
        desktop = "tools";
      }];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          vdi2qcow2
        ];
      };
    })
  ];
}
