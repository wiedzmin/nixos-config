{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.golang;
  user = config.attributes.mainUser.name;
in
{
  options = {
    dev.golang = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Golang dev infrastructure.";
      };
      goPath = mkOption {
        type = types.str;
        default = "";
        description = "Path to be used as $GOPATH root.";
      };
      privateModules = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Glob patterns of Go modules to consider private (e.g. GOPRIVATE contents).";
      };
      misc.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable miscelanneous tools.";
      };
      rootMarkers.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to use additional project root' marker files";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs Golang setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      assertions = [{
        assertion = (cfg.enable && cfg.goPath != "");
        message = "dev/golang: cannot proceed without valid $GOPATH value.";
      }];

      shell.core.variables = [({
        emacs = true;
        GOPATH = cfg.goPath;
      } // lib.optionalAttrs (cfg.privateModules != [ ]) {
        GOPRIVATE = builtins.concatStringsSep "," cfg.privateModules;
      })];
      home-manager.users.${user} = {
        home.packages = with pkgs; [ delve gopls go gomacro ];
        home.sessionPath = [ "${cfg.goPath}/bin" ];
      };
      dev.editorconfig.rules = {
        "*.go" = {
          charset = "utf-8";
          indent_style = "tab";
          indent_size = "4";
        };
      };
      dev.misc.timeTracking.extensions.dev = { "go" = "coding:go"; };

      shell.prompts.starship.modulesConfiguration = { golang = { format = "[🐹 $version](bold cyan) "; }; };

      dev.projectenv.templates.entries = {
        # TODO: consider unbind lp-repl from golang since it is more of common functionality
        "golang.lp-repl" = "${homePrefix (wsRoot "github")}/wiedzmin/nixos-config/modules/dev/golang/templates/lp-repl";
        "golang.nix" = "${homePrefix (wsRoot "github")}/wiedzmin/nixos-config/modules/dev/golang/templates/go2nix";
        "golang.project" = "${homePrefix (wsRoot "github")}/wiedzmin/nixos-config/modules/dev/golang/templates/project";
      };

      dev.projectenv.templates.settings.common = { "golangEnableModules" = true; };
    })
    (mkIf (cfg.enable && cfg.misc.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        go-install-wrapper = mkShellScriptWithDeps "go-install-wrapper" (with pkgs; [ ]) ''
          go install ./...
        '';
      };
      home-manager.users.${user} = { home.packages = with pkgs; [ go-install-wrapper gore goimports impl ]; };
    })
    (mkIf (cfg.enable && cfg.rootMarkers.enable) { dev.navigation.projects.rootMarkers = [ "go.mod" ]; })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.flycheck-golangci-lint epkgs.go-mode epkgs.go-tag epkgs.gotest ];
      ide.emacs.core.customKeymaps = { "custom-gotag-map" = "C-c `"; };
      ide.emacs.core.config = readSubstituted ../../subst.nix ./emacs/golang.el;
    })
  ];
}
