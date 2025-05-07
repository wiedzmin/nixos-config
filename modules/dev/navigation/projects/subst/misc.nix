{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  todoMarkers = concatStringListsQuoted " " config.dev.navigation.projects.markers.todo;
}
