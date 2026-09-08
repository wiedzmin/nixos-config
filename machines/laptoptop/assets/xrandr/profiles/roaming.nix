{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

let
  profileName = config.workstation.randr.defaults.autorandr.roaming;
  user = config.attributes.mainUser.name;
  layout = config.heads.layouts.roaming;
  heads = {
    "primary" = config.hardware.heads.internal-x270-SL10M37887;
  };
in
{
  nixpkgs.config.packageOverrides = _: {
    "rescreen-${profileName}-i3" = pkgs.writeShellApplication {
      name = "rescreen-${profileName}-i3";
      runtimeInputs = with pkgs; [ i3 ];
      text = mkRescreenScriptI3 config.wmCommon.workspaces heads;
    };
  };
  home-manager.users."${user}" = {
    home.packages = [ pkgs."rescreen-${profileName}-i3" ];
    programs.autorandr.profiles = mkAutorandrProfile profileName heads layout config.wm.i3.enable;
  };
  wmCommon.statusbar.outputs = [ heads."primary".output ];
}
