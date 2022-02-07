{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.appearance.gtk;
  user = config.attributes.mainUser.name;
in
{
  options = {
    appearance.gtk = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable GTK theming.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      programs.dconf.enable = true;
      services.dbus.packages = with pkgs; [ gnome3.dconf ];
      home-manager.users."${user}" = { gtk.enable = true; };
    })
  ];
}
