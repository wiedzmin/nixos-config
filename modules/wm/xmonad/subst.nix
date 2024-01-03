{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

let
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  dwmLayoutFont = lib.optionalString (config.wmCommon.fonts.default != "") ''fontName = "${config.wmCommon.fonts.default}"'';

  htopBinary = "${pkgs.htop}/bin/htop";
  iotopBinary = "${pkgs.iotop}/bin/iotop";
  gotopBinary = "${pkgs.gotop}/bin/gotop";
  bcBinary = "${pkgs.bc}/bin/bc";
  xmobarBinary = "${pkgs.xmobar}/bin/xmobar";
  redisTuiBinary = "${nurpkgs.redis-tui}/bin/redis-tui";

  xmonadKeys = bindKeysXmonad config.wmCommon.keybindings.entries 18;

  xmobarFontDef = config.wm.xmonad.xmobar.font;
  xmonadPrimaryWorkspaces = mkWorkspacesXmonad config.wmCommon.workspaces "primary" 20;
  xmonadSecondaryWorkspaces = mkWorkspacesXmonad config.wmCommon.workspaces "secondary" 22;
  xmonadTertiaryWorkspaces = mkWorkspacesXmonad config.wmCommon.workspaces "tertiary" 20;
}
