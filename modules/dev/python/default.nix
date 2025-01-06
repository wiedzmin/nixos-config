{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
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
      emacs.orgmode.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs org-mode helper packages for org-babel and such";
      };
      bookmarks.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable Python-related bookmarks";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
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
      completion.tabnine.config = { language.python = { command = "python-lsp-server"; }; };
    })
    (mkIf (cfg.enable && cfg.rootMarkers.enable) {
      dev.navigation.projects.rootMarkers =
        [ "Pipfile" "manage.py" "poetry.lock" "requirements.txt" "requirements.pip" "setup.py" "tox.ini" ];
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      # TODO: consider reorganizing some settings below under other module's options, if appropriate (including creating new ones)
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ python3Packages.python-lsp-server ];
      };
      ide.emacs.core.extraPackages = epkgs: [ epkgs.pip-requirements epkgs.flycheck-prospector ];
      ide.emacs.core.config = lib.optionalString (!config.ide.emacs.core.treesitter.enable) (readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/python-mode.el ]) +
        lib.optionalString (config.ide.emacs.core.treesitter.enable) (readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/python-ts-mode.el ]) +
        (builtins.readFile ./elisp/common.el) +
        lib.optionalString cfg.emacs.orgmode.enable ''
          (use-package ob-python
            :commands (org-babel-execute:python))
        '';
      ide.emacs.core.treesitter.grammars = {
        python = "https://github.com/tree-sitter/tree-sitter-python";
      };
      ide.emacs.core.treesitter.modeRemappings = {
        python-mode = "python-ts-mode";
      };
    })
    (mkIf (cfg.enable && cfg.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "goopy" = {
          desc = "python + ";
          tags = [ "dev" ];
          remote = {
            url = "https://www.google.ru/";
            searchSuffix = "?q=python+";
          };
        };
        "pypi" = {
          desc = "PyPI";
          tags = [ "dev" "python" ];
          remote = {
            url = "https://pypi.org";
            searchSuffix = "/search/?q=";
          };
        };
      };
    })
  ];
}
