{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.python;
  user = config.attributes.mainUser.name;
in
{
  options = {
    dev.python = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Python dev infrastructure.";
      };
      lineLengthThreshold = mkOption {
        type = types.int;
        default = 120;
        description = "Maximum source line length.";
      };
      indentWidth = mkOption {
        type = types.int;
        default = 4;
        description = "Source indentation level width.";
      };
      excludes = mkOption {
        type = types.listOf types.str;
        default = [ ".git" "__pycache__" ];
        description = "Filesystem entries to exclude.";
      };
      ignoredErrors = mkOption {
        type = types.listOf types.str;
        default = [ "C901" "E124" "E128" "E201" "E203" "E211" "E251" "W503" "W504" ];
        description = "Error codes to ignore.";
      };
      pylsExtraSourcePaths = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "List of paths to search for Python packages, which will be fed to Python Language Server.";
      };
      rootMarkers.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to use additional project root' marker files";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs Python setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      dev.projectenv.templates.entries = {
        "python.project" = configPrefix config.navigation.bookmarks.workspaces.roots "modules/dev/python/templates/project";
      };
      dev.projectenv.templates.settings.common = {
        "pythonColumnLimit" = cfg.lineLengthThreshold;
        "pythonExcludes" = lib.concatStringsSep "," cfg.excludes;
        "pythonIgnores" = lib.concatStringsSep ", " cfg.ignoredErrors;
        "pythonEnableBlack" = false;
      };
      dev.editorconfig.rules = {
        "*.py" = {
          charset = "utf-8";
          indent_style = "space";
          indent_size = "4";
        };
      };
      dev.git.core.gitignore = ''
        .mypy_cache/*
      '';
      dev.misc.timeTracking.extensions.dev = { "py" = "coding:python"; };
    })
    (mkIf (cfg.enable && cfg.rootMarkers.enable) {
      dev.navigation.projects.rootMarkers =
        [ "Pipfile" "manage.py" "poetry.lock" "requirements.txt" "requirements.pip" "setup.py" "tox.ini" ];
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      # TODO: also play with the `pylsp` (https://github.com/python-lsp/python-lsp-server)
      ide.emacs.core.extraPackages = epkgs: [ epkgs.pip-requirements epkgs.flycheck-prospector epkgs.lsp-python-ms ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./emacs/python.el ];
    })
  ];
}
