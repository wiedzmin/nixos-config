{ config, lib, pkgs, ... }:
with lib;

let cfg = config.packaging;
in {
  options = {
    packaging = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable packaging infra.";
      };
      nix.helpers.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Nix helper tools.";
      };
      nix.srcfmt.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Nix source formatting tools.";
      };
      nix.importers.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable tools to convert package definitions to Nix ones.";
      };
      nix.search.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Nix searching helper tools.";
      };
      misc.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable misc packaging tools.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.nix.helpers.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nix-prefetch
          nix-prefetch-github
          nix-prefetch-scripts
          nixos-generators
        ];
      };
    })
    (mkIf (cfg.enable && cfg.nix.srcfmt.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nixfmt
        ];
      };
    })
    (mkIf (cfg.enable && cfg.nix.importers.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nodePackages.node2nix
          pypi2nix
        ];
      };
    })
    (mkIf (cfg.enable && cfg.nix.search.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          # custom.gen-nix-du
          nix-index # TODO: maybe make easier shell alias
        ];
      };
    })
    (mkIf (cfg.enable && cfg.misc.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          cachix
          dotnet-sdk # for building some binary releases
          nix-zsh-completions
          nix-review # https://github.com/Mic92/nix-review
        ];
      };
    })
  ];
}
