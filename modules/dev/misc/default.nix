{ config, lib, inputs, pkgs, ... }:
with pkgs.unstable.commonutils;
with config.navigation.bookmarks.workspaces;
with lib;

# <[debug linux]> - <consult-ripgrep "/home/alex3rd/workspace/repos/github.com/NixOS/nixpkgs/" "debug linux description">

let
  cfg = config.dev.misc;
  user = config.attributes.mainUser.name;
  nixpkgs-idafree-pinned = import inputs.nixpkgs-idafree-pinned {
    config = config.nixpkgs.config // {
      allowUnfree = true;
      permittedInsecurePackages = config.ext.nix.core.permittedInsecurePackages;
    };
    localSystem = { system = "x86_64-linux"; };
  };
  nixpkgs-last-unbroken = import inputs.nixpkgs-last-unbroken {
    config = config.nixpkgs.config // {
      allowUnfree = true;
      permittedInsecurePackages = config.ext.nix.core.permittedInsecurePackages;
    };
    localSystem = { system = "x86_64-linux"; };
  };
in
{
  options = {
    dev.misc = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable misc development setup.";
      };
      comby.excludes = mkOption {
        type = types.listOf types.str;
        default = [ ".envrc" ".gitattributes" ".pre-commit-config.yaml" "configuration.nix" "shell.nix" ];
        description = ''
          Patterns to ignore when running Comby refactoring.

          Comby plays badly with symlinks to /nix/store, so here they are.
        '';
      };
      timeTracking.extensions.dev = mkOption {
        type = types.attrsOf types.str;
        default = { };
        description = "File extensions to be considered dev-related";
      };
      timeTracking.sites.regexps = mkOption {
        type = types.attrsOf types.str;
        default = {
          "habr" = "site:habr";
          "pypi" = "site:pypi";
          "stackoverflow" = "site:stackoverflow";
        };
        description = "URL regexps to be considered dev-related";
      };
      patching.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable patching helper tools.";
      };
      diagrams.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable diagrams scheming tools";
      };
      diagrams.plantuml.server.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable PlantUML server";
      };
      diagrams.plantuml.server.port = mkOption {
        type = types.int;
        default = 5678;
        description = "Default port for PlantUML server to listen";
      };
      diagrams.pikchr.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable Pikchr";
      };
      networking.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable network investigation tools.";
      };
      tools.xserver.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable xserver info/debug tools.";
      };
      tools.misc.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable xserver info/debug tools.";
      };
      shebangedBinaries = mkOption {
        type = types.listOf types.str;
        default = [ "bash" "python" ];
        description = ''
          Patterns to ignore when running Comby refactoring.

          Comby plays badly with symlinks to /nix/store, so here they are.
        '';
      };
      just.chooserCmd = mkOption {
        type = types.str;
        default = "rofi -dmenu";
        description = "Chooser command to use for `just` recipes selection";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable development infra for Emacs.";
      };
      emacs.lsp.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable LSP functionality inside Emacs";
      };
      emacs.lsp.impl = mkOption {
        type = types.enum [ "lsp-mode" "eglot" ];
        default = "lsp-mode";
        description = ''
          Which client implementation to use.

          Note that Eglot was recently added to Emacs as default LSP option.
        '';
      };
      emacs.lsp.packageName = mkOption {
        type = types.str;
        readOnly = true;
        default = if cfg.emacs.lsp.impl == "lsp-mode" then "lsp-mode" else "eglot";
        description = "Emacs package name of LSP client";
      };
      emacs.lsp.startFunction = mkOption {
        type = types.str;
        readOnly = true;
        default = if cfg.emacs.lsp.impl == "lsp-mode" then "lsp-deferred" else "eglot-ensure";
        description = "Elisp function to use in major mode hooks for LSP client start";
      };
      emacs.orgmode.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs org-mode helper packages for org-babel and such";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      shell.core.variables = [{ JUST_CHOOSER = cfg.just.chooserCmd; }];
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ code-maat nixpkgs-last-unbroken.comby dfmt fastmod nixpkgs-idafree-pinned.ida-free just lnav tagref xh ];
      };
      pim.timetracking.rules =
        mkArbttProgramMapTitleRule (with config.attributes.browser; [ default.traits.wmClass fallback.traits.wmClass ])
          cfg.timeTracking.extensions.dev + "\n" + mkArbttEmacsMapTitleRule cfg.timeTracking.extensions.dev;
      dev.editorconfig.rules = {
        "Makefile" = {
          indent_style = "tab";
          indent_size = "4";
        };
        "*.{json,yml,yaml}" = {
          indent_style = "space";
          indent_size = "2";
        };
        "*.md" = { trim_trailing_whitespace = false; };
      };
    })
    (mkIf (cfg.enable && cfg.patching.enable) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ diffoscope icdiff patchutils wiggle xmldiff ]; };
    })
    (mkIf (cfg.enable && cfg.diagrams.enable) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ dot-language-server pikchr plantuml ]; };
      services.plantuml-server = {
        inherit (cfg.diagrams.plantuml.server) enable;
        listenPort = cfg.diagrams.plantuml.server.port;
      };
      navigation.bookmarks.entries = {
        plantuml-server = {
          desc = "PlantUML server instance";
          url = "http://localhost:${builtins.toString cfg.diagrams.plantuml.server.port}/plantuml";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
      };
    })
    (mkIf (cfg.enable && cfg.networking.enable) {
      programs = {
        mtr.enable = true;
        wireshark = {
          enable = true;
          package = pkgs.wireshark-qt;
        };
      };
      home-manager.users."${user}" = { home.packages = with pkgs; [ jnettop ]; };
      users.extraUsers."${user}".extraGroups = [ "wireshark" ];
    })
    (mkIf (cfg.enable && cfg.tools.xserver.enable) {
      environment.systemPackages = with pkgs; [ xorg.xev xorg.xprop xorg.xkbcomp drm_info xtruss ];
    })
    (mkIf (cfg.enable && cfg.tools.misc.enable) {
      environment.systemPackages = with pkgs; [
        # D-Bus debug tools
        d-spy
        # bustle # recently broken
        yaml-language-server
        nodejs
      ];
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs."0x0"
        epkgs.comby
        epkgs.elmacro
        epkgs.fic-mode
        epkgs.hl-prog-extra
        epkgs.jinja2-mode
        epkgs.jq-format
        epkgs.justl
        epkgs.webpaste
        epkgs.yaml-pro
        epkgs.yaml-mode
      ] ++ lib.optionals (cfg.diagrams.enable) [
        epkgs.blockdiag-mode
        epkgs.graphviz-dot-mode
        epkgs.pikchr-mode
        epkgs.plantuml-mode
      ] ++ lib.optionals (cfg.diagrams.enable && cfg.emacs.orgmode.enable) [
        epkgs.ob-blockdiag
      ] ++ lib.optionals (!config.ide.emacs.core.treesitter.enable) [
        epkgs.just-mode
        epkgs.lua-mode
      ];
      ide.emacs.core.config = lib.optionalString (!config.ide.emacs.core.treesitter.enable) (readSubstituted config inputs pkgs [ ./subst/non-ts.nix ] [ ./elisp/non-ts.el ]) +
        lib.optionalString (config.ide.emacs.core.treesitter.enable) (readSubstituted config inputs pkgs [ ./subst/ts.nix ] [ ./elisp/ts.el ]) +
        (readSubstituted config inputs pkgs [ ./subst/misc.nix ] [ ./elisp/misc.el ]) +
        lib.optionalString cfg.emacs.orgmode.enable ''
          (use-package ob-restclient
            :after ob restclient
            :commands (org-babel-execute:restclient))
        '' +
        lib.optionalString cfg.diagrams.enable (readSubstituted config inputs pkgs [ ./subst/diagrams.nix ] [ ./elisp/diagrams.el ]) +
        lib.optionalString (cfg.emacs.orgmode.enable && cfg.diagrams.enable) (readSubstituted config inputs pkgs [ ./subst/orgmode-diagrams.nix ] [ ./elisp/orgmode-diagrams.el ]);
      ide.emacs.core.treesitter.grammars = {
        just = "https://github.com/IndianBoy42/tree-sitter-just";
      };
      ide.emacs.core.treesitter.modeRemappings = {
        just-mode = "just-ts-mode";
      };
      ide.emacs.core.customKeymaps = {
        "custom-webpaste-map" = "C-c [";
      };
      ide.emacs.completion.tempel.snippets = ''
        prog-mode

        (fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME: ")
        (todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO: ")
        (bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG: ")
        (hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK: ")
        (note (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "NOTE: ")
        (fixmep (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME(${user}): ")
        (todop (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO(${user}): ")
        (bugp (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG(${user}): ")
        (hackp (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK(${user}): ")
        (notep (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "NOTE(${user}): ")
      '';
      ide.emacs.core.treesitter.grammars = {
        json = "https://github.com/tree-sitter/tree-sitter-json";
        make = "https://github.com/alemuller/tree-sitter-make";
      };
      ide.emacs.core.treesitter.modeRemappings = {
        json-mode = "json-ts-mode";
        js-json-mode = "json-ts-mode";
      };
    })
    # TODO: consider making some kind of frontend for common functionality, e.g. renaming, etc. for both impls
    (mkIf (cfg.enable && cfg.emacs.enable && cfg.emacs.lsp.enable && cfg.emacs.lsp.impl == "lsp-mode") {
      assertions = [{
        assertion = config.ide.emacs.core.enable;
        message = "dev/misc/emacs/lsp: core configuration must be enabled.";
      }];

      ide.emacs.core.extraPackages = epkgs: [
        epkgs.lsp-mode
        epkgs.lsp-ui
      ] ++ optionals (config.ide.emacs.navigation.collections.backend == "consult") [
        epkgs.consult-lsp
      ];
      ide.emacs.core.config = builtins.readFile ./elisp/lsp-mode.el +
        (optionalString (config.ide.emacs.navigation.collections.backend == "consult") (builtins.readFile ./elisp/consult.el));
      home-manager.users."${user}" = {
        home.activation.ensureLspSessionDir = {
          # lsp-deferred fails otherwise
          after = [ ];
          before = [ "linkGeneration" ];
          data = "mkdir -p ${config.ide.emacs.core.dataDir}/lsp";
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable && cfg.emacs.lsp.enable && cfg.emacs.lsp.impl == "eglot") {
      assertions = [{
        assertion = config.ide.emacs.core.enable;
        message = "dev/misc/emacs/lsp: core configuration must be enabled.";
      }];

      ide.emacs.core.extraPackages = epkgs: [
        epkgs.eglot # NOTE: do not depend on bundled version
        epkgs.eglot-tempel
        epkgs.sideline-eglot
      ] ++ optionals (config.ide.emacs.navigation.collections.backend == "consult") [
        epkgs.consult-eglot
        epkgs.consult-eglot-embark
      ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst/eglot.nix ] [ ./elisp/eglot.el ] +
        (optionalString (config.ide.emacs.navigation.collections.backend == "consult") (readSubstituted config inputs pkgs [ ./subst/consult-eglot.nix ] [ ./elisp/consult-eglot.el ]));
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      completion.expansions.espanso.matches = {
        dev_misc = {
          matches = [
            {
              trigger = ":sheb";
              replace = "#!/usr/bin/env {{binary.value}}";
              vars = [
                {
                  name = "binary";
                  type = "choice";
                  params = { values = cfg.shebangedBinaries; };
                }
              ];
            }
            {
              # FIXME: investigate/debug relative paths usage (see below)
              trigger = ":sjar";
              replace = "jar tf {{jarfilename.value}}";
              vars = [
                {
                  name = "files";
                  type = "shell";
                  params = { cmd = "fd -e jar"; };
                }
                {
                  name = "jarfilename";
                  type = "choice";
                  params = { values = "{{files}}"; };
                }
              ];
            }
            {
              trigger = ":gma";
              replace = "git log --all --numstat --date=short --pretty=format:'--%h--%ad--%aN' --no-renames > maat.log";
            }
            {
              trigger = ":gdeb";
              replace = "goBinPrefix config.dev.golang.goPath \"$|$\"";
            }
            {
              trigger = ":codo";
              replace = "```$|$```"; # NOTE: one-liner, for cases with interpreted newlines
            }
            {
              trigger = ":codm";
              replace = ''
                ```
                $|$
                ```'';
            }
          ];
        };
      };
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          # NOTE: expansions deps
          temurin-bin # error: adoptopenjdk has been removed as the upstream project is deprecated. Consider using `temurin-bin`
          git
        ];
      };
    })
    (mkIf (cfg.enable && config.navigation.bookmarks.enable) {
      navigation.bookmarks.entries = {
        linux_kernel_sources_2_6 = {
          path = "${config.navigation.bookmarks.workspaces.globalRoot}/git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux";
        };
      };
    })
  ];
}
