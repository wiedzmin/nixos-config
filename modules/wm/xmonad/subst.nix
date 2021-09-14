{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };

rec {
  xmobarMaybeFont = ""; # FIXME: reveal correct value (was lost)
  xmonadPrimaryWorkspaces = mkWorkspacesXmonad config.wmCommon.workspaces.primary 20;
  xmonadSecondaryWorkspaces = mkWorkspacesXmonad config.wmCommon.workspaces.secondary 22;
  xmonadTertiaryWorkspaces = mkWorkspacesXmonad config.wmCommon.workspaces.tertiary 20;
}
