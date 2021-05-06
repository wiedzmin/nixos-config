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
        default = homePrefix config.navigation.bookmarks.workspaces.globalRoot;
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
    (mkIf (cfg.bookmarks.enable && config.navigation.bookmarks.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        open-project = mkPythonScriptWithDeps "open-project" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis ])
          (builtins.readFile ./scripts/open-project.py);
      };
    })
    (mkIf cfg.fuzzySearch.enable {
      nixpkgs.config.packageOverrides = _: rec {
        reposearch =
          mkPythonScriptWithDeps "reposearch" (with pkgs; [ fd python3Packages.libtmux xsel emacs nurpkgs.pystdlib ])
            (builtins.readFile ./scripts/reposearch.py);
      };
    })
    (mkIf (cfg.wm.enable) {
      wmCommon.keys = lib.optionals (cfg.fuzzySearch.enable) [{
        key = [ "r" ];
        cmd = with config.dev.navigation.projects.fuzzySearch;
          "${pkgs.reposearch}/bin/reposearch --root ${root} --depth ${builtins.toString depth}";
        mode = "dev";
      }] ++ lib.optionals (cfg.bookmarks.enable && config.navigation.bookmarks.enable) [{
        key = [ "p" ];
        cmd = "${pkgs.open-project}/bin/open-project";
        mode = "dev";
      }];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = { home.packages = with pkgs; [ open-project reposearch ]; };
    })
  ];
}
