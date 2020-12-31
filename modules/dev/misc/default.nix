{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.misc;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in {
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
      timeTracking.sites.regexps = mkOption { # TODO: consider splitting by respective submodules
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
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable development infra for Emacs.";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ nurpkgs.comby tagref go-task ];
      };
      custom.pim.timeTracking.rules = with config.attributes.browser; ''
        -- TODO: parameterize IDE (probably, not only emacs)
        ${
          concatStringsSep ''
            ,
          '' (lib.mapAttrsToList (re: tag: ''current window ($program == [${
              concatStringListsQuoted ", " [ default.windowClass fallback.windowClass ]
            }] && $title =~ /${re}/) ==> tag ${tag}'' )
            cfg.timeTracking.extensions.dev)
        },
        ${
          concatStringsSep ''
            ,
          '' (lib.mapAttrsToList (ext: tag: "current window ($title =~ /^emacs - [^ ]+\\.${ext} .*$/) ==> tag ${tag}")
            cfg.timeTracking.extensions.dev)
        },
      '';
      dev.editorconfig.rules = {
        "Makefile" = {
          indent_style = "tab";
          indent_size = "4";
        };
        "*.{json,yml,yaml}" = {
          indent_style = "space";
          indent_size = "2";
        };
        "*.md" = {
          trim_trailing_whitespace = false;
        };
      };
    })
    (mkIf (cfg.enable && cfg.patching.enable) {
      home-manager.users.${user} = { home.packages = with pkgs; [ diffoscope icdiff patchutils wiggle ]; };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs:
        [
          epkgs.comby
          epkgs.elmacro
          epkgs.fic-mode
          epkgs.jinja2-mode
          epkgs.lsp-mode
          epkgs.lsp-ui
          epkgs.webpaste
          epkgs.yaml-mode
        ];
      ide.emacs.core.config = readSubstituted ../../subst.nix ./emacs/misc.el;
      home-manager.users.${user} = {
        home.activation.ensureLspSessionDir = { # lsp-deferred fails otherwise
          after = [ ];
          before = [ "linkGeneration" ];
          data = "mkdir -p ${config.ide.emacs.core.dataDir}/lsp";
        };
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable && config.custom.virtualization.docker.enable) {
      wmCommon.keys = [
        {
          key = [ "Control" "d" ];
          cmd = "${pkgs.systemd}/bin/systemctl restart docker-devdns.service";
          mode = "dev";
        }
        {
          key = [ "Control" "Shift" "d" ];
          cmd = "${pkgs.systemd}/bin/systemctl stop docker-devdns.service";
          mode = "dev";
        }
      ];
    })
  ];
}
