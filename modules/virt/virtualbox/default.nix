{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ext.virtualization.virtualbox;
  user = config.attributes.mainUser.name;
  kernelVersionCap = "5.4";
  stable = import inputs.stable {
    config = config.nixpkgs.config // { allowUnfree = true; };
    localSystem = { system = "x86_64-linux"; };
  };
in
{
  options = {
    ext.virtualization.virtualbox = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable VirtualBox";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = builtins.compareVersions config.boot.kernelPackages.kernel.version kernelVersionCap >= 0;
        message = ''
          ext/virtualization/virtualbox: currently VB fails to build or work with kernels > ${kernelVersionCap}
          See https://www.virtualbox.org/ticket/19845 for some details.
        '';
      }];

      virtualisation.virtualbox.host = {
        enable = true;
        enableExtensionPack = true;
        package = stable.virtualbox;
      };
      users.users."${user}".extraGroups = [ "vboxusers" ];

      networking.networkmanager.unmanaged = [ "interface-name:vb-*" "interface-name:vbox*" "interface-name:ve-*" ];

      wmCommon.wsMapping.rules = [{
        class = "VirtualBox Manager";
        desktop = "tools";
      }];
    })
  ];
}
