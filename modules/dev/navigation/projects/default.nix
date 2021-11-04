{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.navigation.projects;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  options = {
    dev.navigation.projects = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable projects navigation.";
      };
      bookmarks.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable bookmarked project accessibility.";
      };
      fuzzySearch.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable global repositories fuzzy search.";
      };
      fuzzySearch.root = mkOption {
        type = types.str;
        default = config.navigation.bookmarks.workspaces.globalRoot;
        description = "Search root.";
      };
      fuzzySearch.depth = mkOption {
        type = types.int;
        default = 4;
        description = "Search depth.";
      };
      rootMarkers = mkOption {
        type = types.listOf types.str;
        default = [ ".projectile" ];
        description = "Filenames that could be used to denote project root";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.wm.enable {
      wmCommon.keys = lib.optionals cfg.fuzzySearch.enable [{
        key = [ "r" ];
        cmd = with config.dev.navigation.projects.fuzzySearch;
          "${nurpkgs.toolbox}/bin/projects search --root ${root} --depth ${builtins.toString depth}";
        mode = "dev";
      }] ++ lib.optionals (cfg.bookmarks.enable && config.navigation.bookmarks.enable) [{
        key = [ "p" ];
        cmd = "${nurpkgs.toolbox}/bin/projects open";
        mode = "dev";
      }];
    })
  ];
}
