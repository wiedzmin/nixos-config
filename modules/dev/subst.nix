{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

let
  user = config.attributes.mainUser.name;
in
rec {
  globalWorkspaceRoot = config.navigation.bookmarks.workspaces.globalRoot;
}
