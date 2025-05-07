{ config, lib, ... }:
with config.ide.emacs;

{
  projectsRootMarkersEmacs = builtins.concatStringsSep " " (lib.forEach config.dev.navigation.projects.markers.root (marker: ''"${marker}"''));
  projectsSearchPaths = with config.navigation.bookmarks.workspaces;
    builtins.concatStringsSep " " (lib.forEach [ globalRoot ] (root: ''"${root}"''));
}
