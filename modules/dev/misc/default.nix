{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.misc;
  user = config.attributes.mainUser.name;
  stable = import inputs.stable ({
    config = config.nixpkgs.config // { allowUnfree = true; };
    localSystem = { system = "x86_64-linux"; };
  });
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
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable development infra for Emacs.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      shell.core.variables = [{ JUST_CHOOSER = "rofi -dmenu"; }];
      home-manager.users.${user} = {
        home.packages = with pkgs; [ dfmt go-task just lnav comby plantuml tagref xh ];
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
        "ansible" = configPrefix "modules/dev/dbms/misc/templates/ansible" ;
        "reveng" = configPrefix "modules/dev/dbms/misc/templates/reveng";
      };
    })
    (mkIf (cfg.enable && cfg.patching.enable) {
      home-manager.users.${user} = { home.packages = with pkgs; [ stable.diffoscope icdiff patchutils wiggle xmldiff ]; };
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
      environment.systemPackages = with pkgs; [ xlibs.xev xlibs.xprop xorg.xkbcomp drm_info xtruss ];
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
        epkgs.blockdiag-mode
        epkgs.comby
        epkgs.elmacro
        epkgs.fic-mode
        epkgs.jinja2-mode
        epkgs.just-mode
        epkgs.leetcode
        epkgs.lsp-mode
        epkgs.lsp-ui
        epkgs.lua-mode
        epkgs.plantuml-mode
        epkgs.webpaste
        epkgs.yaml-mode
        epkgs.groovy-mode
      ];
      ide.emacs.core.config = readSubstitutedList ../../subst.nix [ ./emacs/misc.el ./emacs/lsp.el ];
      ide.emacs.core.customKeymaps = {
        "custom-lsp-treemacs-map" = "C-c t";
        "custom-webpaste-map" = "C-c [";
      };
      home-manager.users.${user} = {
        home.activation.ensureLspSessionDir = {
          # lsp-deferred fails otherwise
          after = [ ];
          before = [ "linkGeneration" ];
          data = "mkdir -p ${config.ide.emacs.core.dataDir}/lsp";
        };
      };
    })
  ];
}
