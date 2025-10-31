{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

# FIXME: consider extracting functionality more semantically or relocate module itself

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
      markers.root = mkOption {
        type = types.listOf types.str;
        default = [ ] ++ lib.optionals (config.ide.emacs.navigation.projects.backend == "projectile") [ ".projectile" ];
        description = "Filenames that could be used to denote project root";
      };
      markers.todo = mkOption {
        type = types.listOf types.str;
        default = [ "todo.org" "project.org" "agenda.org" "TODO.org" ];
        description = "Filenames that could be used to denote project TODOs";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable projects infra for Emacs.";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ onefetch inputs.devenv-src.packages.${pkgs.system}.devenv ];
        programs.git = {
          aliases = {
            psum = "!which onefetch && onefetch";
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.org-project-capture
      ];
      ide.emacs.core.customPackages = {
        "projects-misc" = { text = readSubstituted config inputs pkgs [ ./subst/misc.nix ] [ ./elisp/custom/misc.el ]; };
      };
      ide.emacs.core.config = builtins.readFile ./elisp/projects.el;
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.entries = lib.optionals cfg.fuzzySearch.enable [
        {
          key = [ "r" ];
          cmd = with config.dev.navigation.projects.fuzzySearch;
            "${nurpkgs.toolbox}/bin/projects search --root ${root} --depth ${builtins.toString depth}";
          mode = "dev";
        }
        (goLocalDebugKeybinding config.dev.golang.goPath config.attributes.debug.useLocalGoBinaries {
          key = [ "Shift" "s" ];
          cmd = with config.dev.navigation.projects.fuzzySearch;
            "go#projects search --shell --root ${root} --depth ${builtins.toString depth}";
          mode = "dev";
        })
        (goLocalDebugKeybinding config.dev.golang.goPath config.attributes.debug.useLocalGoBinaries {
          key = [ "Shift" "c" ];
          cmd = with config.dev.navigation.projects.fuzzySearch;
            "go#projects search --copy-local --root ${root} --depth ${builtins.toString depth}";
          mode = "dev";
        })
      ] ++ lib.optionals (cfg.bookmarks.enable && config.navigation.bookmarks.enable) [
        (goLocalDebugKeybinding config.dev.golang.goPath config.attributes.debug.useLocalGoBinaries {
          key = [ "p" ];
          cmd = "go#projects open";
          mode = "dev";
        })
        (goLocalDebugKeybinding config.dev.golang.goPath config.attributes.debug.useLocalGoBinaries {
          key = [ "s" ];
          cmd = "go#projects open --shell";
          mode = "dev";
        })
        (goLocalDebugKeybinding config.dev.golang.goPath config.attributes.debug.useLocalGoBinaries {
          key = [ "c" ];
          cmd = "go#projects open --copy-local";
          mode = "dev";
        })
      ] ++ lib.optionals (cfg.bookmarks.enable && config.navigation.bookmarks.enable && config.pim.orgmode.enable) [
        (goLocalDebugKeybinding config.dev.golang.goPath config.attributes.debug.useLocalGoBinaries {
          key = [ "a" ];
          cmd = "go#projects open --path ${config.pim.orgmode.org-roam.rootDir}/agenda.org";
          mode = "dev";
        })
      ];
    })
    (mkIf (cfg.enable && cfg.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "cachix/devenv/options" = {
          desc = "cachix/devenv options";
          url = "https://devenv.sh/reference/options/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "projectile/docs" = {
          desc = "Projectile docs";
          url = "https://docs.projectile.mx/projectile/index.html";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "cachix/devenv/releases" = {
          desc = "cachix/devenv releases";
          url = "https://github.com/cachix/devenv/releases";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
      };
    })
  ];
}
