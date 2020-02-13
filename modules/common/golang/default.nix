{ config, lib, pkgs, ... }:
with lib;

let cfg = config.custom.dev.golang;
in {
  options = {
    custom.dev.golang = {
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
        default = [];
        description = "Glob patterns of Go modules to consider private (e.g. GOPRIVATE contents).";
      };
      packaging.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable packaging toolset.";
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
      assertions = [
        {
          assertion = (cfg.goPath != "" && builtins.pathExists cfg.goPath);
          message = "dev/golang: cannot proceed without valid $GOPATH value.";
        }
      ];

      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          gometalinter
          gomodifytags
          gotools # for gopls
          dep
          go
        ];
        programs.zsh.sessionVariables = {
          GOPATH = cfg.goPath;
        } // lib.optionalAttrs (cfg.privateModules != []) {
          GOPRIVATE = builtins.concatStringsSep "," cfg.privateModules;
        };
      };
    })
    (mkIf (cfg.enable && cfg.packaging.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          dep2nix
          go2nix
          vgo2nix
        ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.emacs.extraPackages = epkgs: [
          epkgs.flycheck-gometalinter
          epkgs.go-guru
          epkgs.go-mode
          epkgs.go-tag
          epkgs.gotest
        ];
      };
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./golang.el; }));
    })
  ];
}
