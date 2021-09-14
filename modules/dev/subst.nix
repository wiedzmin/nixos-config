{ config, inputs, lib, pkgs, ... }:
with import ../util.nix { inherit config inputs lib pkgs; };

let
  user = config.attributes.mainUser.name;
in
rec {
  globalWorkspaceRoot = homePrefix user config.navigation.bookmarks.workspaces.globalRoot;
}
