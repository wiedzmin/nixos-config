{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

rec {
  globalWorkspaceRoot = config.navigation.bookmarks.workspaces.globalRoot;
}
