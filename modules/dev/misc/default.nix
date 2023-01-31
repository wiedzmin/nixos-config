{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with config.navigation.bookmarks.workspaces;
with lib;

let
  cfg = config.dev.misc;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
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
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      shell.core.variables = [{ JUST_CHOOSER = cfg.just.chooserCmd; }];
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ dfmt go-task just lnav comby tagref xh nurpkgs.code-maat ];
      };
      pim.timetracking.rules =
        mkArbttProgramMapTitleRule (with config.attributes.browser; [ default.windowClass fallback.windowClass ])
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
      dev.projectenv.templates.settings.common = {
        gitUsername = config.attributes.mainUser.fullName;
        gitEmail = config.attributes.mainUser.email;
        gpgSigningKey = config.attributes.mainUser.gpgKeyID;
        bugReferenceBugRegexp = "\\\\#\\\\(?2:[0-9]+\\\\)\\\\>";
        inputsUnstableRev = inputs.unstable.rev;
      };
      dev.projectenv.templates.entries = {
        "ansible" = configPrefix roots "modules/dev/dbms/misc/templates/ansible";
        "reveng" = configPrefix roots "modules/dev/dbms/misc/templates/reveng";
      };
    })
    (mkIf (cfg.enable && cfg.patching.enable) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ diffoscope icdiff patchutils wiggle xmldiff ]; };
    })
    (mkIf (cfg.enable && cfg.diagrams.enable) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ pikchr plantuml ]; };
      services.plantuml-server = {
        inherit (cfg.diagrams.plantuml.server) enable;
        listenPort = cfg.diagrams.plantuml.server.port;
      };
      navigation.bookmarks.entries = {
        plantuml-server = {
          desc = "PlantUML server instance";
          remote.url = "http://localhost:${cfg.diagrams.plantuml.server.port}/";
        };
      };
    })
    (mkIf (cfg.enable && cfg.diagrams.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.pikchr-mode
      ];
      ide.emacs.core.config = ''
        (use-package pikchr-mode
          :commands (org-babel-default-header-args:pikchr
                     org-babel-execute:pikchr
                     org-babel-prep-session:pikchr))
      '';
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
        dfeet
        bustle
      ];
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs."0x0"
        epkgs.blockdiag-mode
        epkgs.comby
        epkgs.elmacro
        epkgs.fic-mode
        epkgs.hl-prog-extra
        epkgs.jinja2-mode
        epkgs.just-mode
        epkgs.justl
        epkgs.leetcode
        epkgs.lua-mode
        epkgs.plantuml-mode
        epkgs.webpaste
        epkgs.yaml-pro
        epkgs.yaml-mode
        epkgs.groovy-mode
      ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/misc.el ];
      ide.emacs.core.customKeymaps = {
        "custom-webpaste-map" = "C-c [";
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable && cfg.emacs.lsp.enable) {
      assertions = [{
        assertion = config.ide.emacs.core.enable;
        message = "dev/misc/emacs/lsp: core configuration must be enabled.";
      }];

      ide.emacs.core.extraPackages = epkgs: [
        epkgs.consult-lsp
        epkgs.lsp-mode
        epkgs.lsp-ui
      ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/lsp.el ];
      ide.emacs.core.customKeymaps = {
        "custom-lsp-treemacs-map" = "C-c t";
      };
      home-manager.users."${user}" = {
        home.activation.ensureLspSessionDir = {
          # lsp-deferred fails otherwise
          after = [ ];
          before = [ "linkGeneration" ];
          data = "mkdir -p ${config.ide.emacs.core.dataDir}/lsp";
        };
      };
    })
    # FIXME: make tmux session templating optional everywhere (see below) !!!!
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile."espanso/user/dev_misc.yml".text = ''
          name: dev_misc
          parent: default
          filter_title: ".*${config.shell.tmux.defaultSession}.*${config.attributes.machine.name}.*"

          matches:
            - trigger: ":gma"
              replace: "git log --all --numstat --date=short --pretty=format:'--%h--%ad--%aN' --no-renames > maat.log"
        '';
      };
    })
  ];
}
