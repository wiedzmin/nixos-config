{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  xmobarMaybeFont = ""; # FIXME: reveal correct value (was lost)
  xmonadPrimaryWorkspaces = mkWorkspacesXmonad config.wmCommon.workspaces.primary 20;
  xmonadSecondaryWorkspaces = mkWorkspacesXmonad config.wmCommon.workspaces.secondary 22;
  xmonadTertiaryWorkspaces = mkWorkspacesXmonad config.wmCommon.workspaces.tertiary 20;
}
